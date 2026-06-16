#lang racket/base

;; sandbox/executor-server.rkt — mTLS TCP executor node server
;; STABILITY: evolving
;;
;; W2 (v0.99.12): Network-facing TLS server that accepts mTLS connections
;; from orchestrator clients and dispatches JSON-RPC tool calls to the
;; worker tool registry.
;;
;; Architecture:
;;   ┌──────────────┐   mTLS   ┌───────────────┐
;;   │ Orchestrator │ ───────→ │ Executor      │ ← this server
;;   │ (W1 client)  │ ←─────── │ (W2 server)   │
;;   └──────────────┘          └───────────────┘
;;
;; Per-connection thread:
;;   1. ssl-accept (mTLS handshake with client cert verification)
;;   2. Read newline-delimited JSON requests
;;   3. Validate capability token in arguments
;;   4. Parse as ipc-request
;;   5. Dispatch via process-ipc-request (shared with stdio worker)
;;   6. Write ipc-response as JSON line
;;
;; Security:
;;   - Mandatory mTLS: ssl-set-verify! #t rejects connections without client cert
;;   - Capability token validation on every request
;;   - Request size limit: IPC-MAX-REQUEST-BYTES (1 MB)
;;   - Graceful shutdown: stop accepting, drain active connections

(require racket/contract
         racket/match
         racket/string
         racket/tcp
         openssl
         (only-in json string->jsexpr jsexpr->string)
         "ipc-protocol.rkt"
         "worker-dispatch.rkt"
         "../util/security/capability-tokens.rkt")

;; ── Logging ─────────────────────────────────────────────────────

(define-logger executor-server)

;; ── Defaults ────────────────────────────────────────────────────

(define DEFAULT-MAX-CONNECTIONS 50)
(define DEFAULT-MAX-REQUEST-BYTES IPC-MAX-REQUEST-BYTES)

;; ── Executor Server Struct ──────────────────────────────────────

(struct executor-server
        (listener ; ssl-listener?
         accept-thread ; (or/c thread? #f)
         capability-secret ; string? — shared secret for token validation
         active-conns ; (boxof exact-nonnegative-integer?)
         shutdown? ; (boxof boolean)
         custodian) ; custodian?
  #:transparent)

;; ── Capability Token Validation ─────────────────────────────────

;; Validate the capability token embedded in the request arguments.
;; Returns (values valid? error-message).
(define (validate-request-capability request secret)
  (define arguments (ipc-request-arguments request))
  (define required-cap (ipc-request-capability request))
  (define token (hash-ref arguments 'capability-token #f))
  (cond
    [(not token) (values #f "missing capability token")]
    [(not (string? token)) (values #f "invalid capability token format")]
    [else
     (define claims (validate-capability-token/claims token secret))
     (cond
       [(not claims) (values #f "invalid or expired capability token")]
       [(not (eq? (capability-token-claims-capability claims) required-cap))
        (values #f "capability mismatch: token does not match request capability")]
       [else (values #t #f)])]))

;; ── Per-Connection Handler ──────────────────────────────────────

(define (handle-connection in out secret server-shutdown? conn-counter max-conns)
  ;; Increment active connection count
  (set-box! conn-counter (add1 (unbox conn-counter)))
  (log-executor-server-info "connection opened (~a active)" (unbox conn-counter))
  (with-handlers ([exn:fail? (lambda (e)
                               (log-executor-server-warning "connection error: ~a" (exn-message e)))])
    (let loop ()
      (unless (unbox server-shutdown?)
        (define line (read-line in 'any))
        (cond
          [(eof-object? line) (log-executor-server-info "client disconnected")]
          [else
           (define trimmed (string-trim line))
           (unless (string=? trimmed "")
             ;; Check request size
             (define req-size (string-length trimmed))
             (define response
               (if (> req-size DEFAULT-MAX-REQUEST-BYTES)
                   (make-error-response #f
                                        (format "request too large (~a bytes, max ~a)"
                                                req-size
                                                DEFAULT-MAX-REQUEST-BYTES))
                   ;; Parse and dispatch
                   (process-secure-request trimmed secret)))
             ;; Send response
             (define json-str (serialize-response response))
             (display json-str out)
             (newline out)
             (flush-output out))
           (loop)]))))
  ;; Decrement active connection count
  (set-box! conn-counter (sub1 (unbox conn-counter)))
  (with-handlers ([exn:fail? void])
    (close-input-port in)
    (close-output-port out)))

;; Process a single request line with capability token validation
(define (process-secure-request line secret)
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-response #f (format "server error: ~a" (exn-message e))))])
    (define req-data
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (string->jsexpr line)))
    (define request (and req-data (jsexpr->ipc-request req-data)))
    (cond
      [(not request) (make-error-response #f "malformed request")]
      [else
       (define-values (valid? err-msg) (validate-request-capability request secret))
       (if valid?
           ;; F-10 fix: Strip capability token from arguments before dispatch
           ;; so it's not passed to the worker tool.
           (let ([cleaned-request (struct-copy ipc-request
                                               request
                                               [arguments
                                                (hash-remove (ipc-request-arguments request)
                                                             'capability-token)])])
             (process-ipc-request cleaned-request))
           (make-error-response (ipc-request-request-id request) err-msg))])))

;; ── Accept Loop ─────────────────────────────────────────────────

(define (start-accept-loop! server)
  (define listener (executor-server-listener server))
  (define secret (executor-server-capability-secret server))
  (define shutdown-box (executor-server-shutdown? server))
  (define conn-counter (executor-server-active-conns server))
  (define max-conns DEFAULT-MAX-CONNECTIONS)
  (thread
   (lambda ()
     (let loop ()
       (unless (unbox shutdown-box)
         (with-handlers ([exn:fail? (lambda (e)
                                      (unless (unbox shutdown-box)
                                        (log-executor-server-warning "accept error: ~a"
                                                                     (exn-message e))))])
           ;; Check connection limit
           (when (< (unbox conn-counter) max-conns)
             (define-values (in out) (ssl-accept listener))
             (thread (lambda ()
                       (handle-connection in out secret shutdown-box conn-counter max-conns)))))
         (loop))))))

;; ── Server Lifecycle ────────────────────────────────────────────

;; Start an executor server on the given port with mTLS.
;; The ssl-server-context must have ssl-set-verify! #t (mandatory client cert).
(define (start-executor-server! #:port port
                                #:ssl-context ssl-context
                                #:capability-secret capability-secret
                                #:max-connections [max-connections DEFAULT-MAX-CONNECTIONS])
  (define server-custodian (make-custodian))
  (define server-listener
    (parameterize ([current-custodian server-custodian])
      ;; Pass the pre-configured ssl-server-context (already has certs,
      ;; key, CA, and ssl-set-verify! #t from make-server-ssl-context)
      (ssl-listen port 5 #t #f ssl-context)))
  (define server
    (executor-server server-listener #f capability-secret (box 0) (box #f) server-custodian))
  (define accept-thread (start-accept-loop! server))
  (define server-with-thread (struct-copy executor-server server [accept-thread accept-thread]))
  (log-executor-server-info "executor server started on port ~a" port)
  server-with-thread)

;; Gracefully shutdown: stop accepting, wait for active connections to drain
(define (shutdown-executor-server! server [drain-timeout-ms 5000])
  (set-box! (executor-server-shutdown? server) #t)
  ;; Close listener to stop accepting new connections
  (with-handlers ([exn:fail? void])
    (ssl-close (executor-server-listener server)))
  ;; Wait for active connections to drain (up to timeout)
  (define deadline (+ (current-inexact-milliseconds) drain-timeout-ms))
  (let loop ()
    (when (and (> (unbox (executor-server-active-conns server)) 0)
               (< (current-inexact-milliseconds) deadline))
      (sleep 0.1)
      (loop)))
  ;; Force shutdown: kill custodian
  (custodian-shutdown-all (executor-server-custodian server))
  (log-executor-server-info "executor server stopped"))

;; Check if server is still accepting connections
(define (executor-server-running? server)
  (and (not (unbox (executor-server-shutdown? server)))
       (executor-server-accept-thread server)
       (thread-running? (executor-server-accept-thread server))))

;; ── Provides ────────────────────────────────────────────────────

(provide executor-server
         executor-server?
         executor-server-listener
         executor-server-accept-thread
         executor-server-capability-secret
         executor-server-active-conns
         executor-server-shutdown?
         executor-server-custodian)

(provide (contract-out [start-executor-server!
                        (->* (#:port exact-positive-integer?
                                     #:ssl-context ssl-server-context?
                                     #:capability-secret string?)
                             (#:max-connections exact-positive-integer?)
                             executor-server?)]
                       [shutdown-executor-server!
                        (->* (executor-server?) (exact-positive-integer?) void?)]
                       [executor-server-running? (-> executor-server? boolean?)]))

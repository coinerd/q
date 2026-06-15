#lang racket/base

;; agent/distributed/remote-ipc.rkt — Async TLS client for remote executor IPC
;; STABILITY: evolving
;;
;; W1 (v0.99.12): TCP+TLS client that mirrors gateway-ipc.rkt's async
;; request/response pattern but operates over a TLS socket instead of stdio.
;;
;; Architecture:
;;   ┌──────────────┐   TLS   ┌──────────────┐
;;   │ Orchestrator │ ──────→ │ Executor     │
;;   │ (this client)│ ←────── │ (W2 server)  │
;;   └──────────────┘         └──────────────┘
;;
;; Key design decisions:
;;   - Reuses ipc-protocol.rkt structs (ipc-request, ipc-response) — zero translation
;;   - Separate drain thread reads newline-delimited JSON responses
;;   - Per-request async-channel for response delivery
;;   - Connection drop → all pending requests notified with 'error
;;   - Timeout → async-channel sync/timeout returns #f → timeout response
;;   - Module-level request-id semaphore (like gateway-ipc C1)

(require racket/contract
         racket/match
         racket/string
         racket/port
         racket/async-channel
         openssl
         json
         "../../sandbox/ipc-protocol.rkt")

;; ── Logging ─────────────────────────────────────────────────────

(define-logger remote-ipc)

;; ── Module-level request-id lock (mirrors gateway-ipc C1) ───────

(define request-id-lock (make-semaphore 1))
(define request-counter (box 0))

(define (generate-remote-request-id)
  (call-with-semaphore request-id-lock
                       (lambda ()
                         (set-box! request-counter (add1 (unbox request-counter)))
                         (format "rmt-~a-~a"
                                 (exact->inexact (round (current-inexact-milliseconds)))
                                 (unbox request-counter)))))

;; ── Error Type ──────────────────────────────────────────────────

(struct exn:fail:remote-connection exn:fail () #:transparent)

;; ── Remote Connection Struct ────────────────────────────────────

;; NOTE: Field named alive-flag (not alive?) so the auto-generated accessor
;; is remote-connection-alive-flag, not remote-connection-alive?.
;; The function remote-connection-alive? is defined separately below.

(struct remote-connection
        (host ; string?
         port ; exact-positive-integer?
         ssl-context ; ssl-client-context?
         in-port ; (or/c input-port? #f)
         out-port ; (or/c output-port? #f)
         drain-thread ; (or/c thread? #f)
         pending-requests ; (boxof (hash/c string? async-channel?))
         lock ; semaphore? — serializes access to pending
         write-lock ; semaphore? — serializes writes to out-port
         custodian ; custodian?
         alive-flag ; (boxof boolean)
         started-ms) ; exact-nonnegative-integer?
  #:transparent)

;; ── Response Wrapper ────────────────────────────────────────────

(struct remote-response-packet (id response) #:transparent)

;; ── Pending Request Management ──────────────────────────────────

(define (register-pending! conn req-id resp-ch)
  (call-with-semaphore (remote-connection-lock conn)
                       (lambda ()
                         (define current (unbox (remote-connection-pending-requests conn)))
                         (hash-set! current req-id resp-ch))))

(define (unregister-pending! conn req-id)
  (call-with-semaphore (remote-connection-lock conn)
                       (lambda ()
                         (define current (unbox (remote-connection-pending-requests conn)))
                         (hash-remove! current req-id))))

(define (get-pending-channel conn req-id)
  (call-with-semaphore (remote-connection-lock conn)
                       (lambda ()
                         (define current (unbox (remote-connection-pending-requests conn)))
                         (hash-ref current req-id #f))))

;; Connection drop → notify all pending requests with error
(define (clear-all-pending! conn reason)
  (call-with-semaphore
   (remote-connection-lock conn)
   (lambda ()
     (define current (unbox (remote-connection-pending-requests conn)))
     (for ([(id ch) (in-hash current)])
       (async-channel-put ch (remote-response-packet id (cons 'connection-error reason))))
     (hash-clear! current))))

;; ── Drain Thread ────────────────────────────────────────────────

(define (start-drain-thread! conn)
  (thread
   (lambda ()
     (define port (remote-connection-in-port conn))
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-remote-ipc-warning "drain thread crashed: ~a" (exn-message e))
                                  (clear-all-pending! conn 'drain-crash))])
       (let loop ()
         (define line (read-line port 'any))
         (cond
           [(eof-object? line)
            (log-remote-ipc-warning "remote connection EOF — connection dropped")
            (set-box! (remote-connection-alive-flag conn) #f)
            (clear-all-pending! conn 'connection-drop)]
           [else
            (define trimmed (string-trim line))
            (unless (string=? trimmed "")
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-remote-ipc-warning "failed to parse response: ~a"
                                                                   (exn-message e)))])
                (define jsexpr (with-input-from-string trimmed read-json))
                (define resp (and jsexpr (jsexpr->ipc-response jsexpr)))
                (when (and resp (ipc-response? resp))
                  (define ch (get-pending-channel conn (ipc-response-request-id resp)))
                  (when ch
                    (async-channel-put ch
                                       (remote-response-packet (ipc-response-request-id resp)
                                                               resp))))))
            (loop)]))))))

;; ── Connection Lifecycle ────────────────────────────────────────

(define (start-remote-connection! host port ssl-context [connect-timeout-ms 10000])
  (define conn-custodian (make-custodian))
  ;; Run ssl-connect in a thread, use a channel to apply timeout.
  (define connect-ch (make-channel))
  (define connect-custodian (make-custodian conn-custodian))
  (parameterize ([current-custodian connect-custodian])
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e)
                                           (channel-put connect-ch
                                                        (cons 'error
                                                              (format "connection failed: ~a"
                                                                      (exn-message e)))))])
                (define-values (tcp-in tcp-out) (ssl-connect host port ssl-context))
                (channel-put connect-ch (cons 'ok (cons tcp-in tcp-out)))))))
  (define result (sync/timeout (/ connect-timeout-ms 1000.0) connect-ch))
  (cond
    [(not result)
     (custodian-shutdown-all connect-custodian)
     (raise (exn:fail:remote-connection "connection timeout" (current-continuation-marks)))]
    [(eq? (car result) 'error)
     (raise (exn:fail:remote-connection (cdr result) (current-continuation-marks)))]
    [else
     (define in-port (cadr result))
     (define out-port (cddr result))
     (define conn
       (remote-connection host
                          port
                          ssl-context
                          in-port
                          out-port
                          #f
                          (box (make-hash))
                          (make-semaphore 1)
                          (make-semaphore 1)
                          conn-custodian
                          (box #t)
                          (current-inexact-milliseconds)))
     (define dt
       (parameterize ([current-custodian conn-custodian])
         (start-drain-thread! conn)))
     (define conn-with-thread (struct-copy remote-connection conn [drain-thread dt]))
     (log-remote-ipc-info "remote connection established to ~a:~a" host port)
     conn-with-thread]))

;; ── Request/Response ────────────────────────────────────────────

(define (send-remote-request! conn req [timeout-ms IPC-DEFAULT-TIMEOUT-MS])
  (unless (remote-connection-alive? conn)
    (raise (exn:fail:remote-connection "remote connection not alive" (current-continuation-marks))))
  (when (ipc-request-too-large? req)
    (raise (exn:fail:remote-connection "request too large" (current-continuation-marks))))
  (define req-id (ipc-request-request-id req))
  (define resp-ch (make-async-channel))
  (register-pending! conn req-id resp-ch)
  (define jsexpr (ipc-request->jsexpr req))
  (define json-str (jsexpr->string jsexpr))
  (define out (remote-connection-out-port conn))
  (call-with-semaphore (remote-connection-write-lock conn)
                       (lambda ()
                         (display json-str out)
                         (newline out)
                         (flush-output out)))
  (define result (sync/timeout (/ timeout-ms 1000.0) resp-ch))
  (unregister-pending! conn req-id)
  (cond
    [(not result) (make-timeout-response req-id)]
    [(remote-response-packet? result)
     (define resp (remote-response-packet-response result))
     (if (and (pair? resp) (eq? (car resp) 'connection-error))
         (make-error-response req-id (format "connection error: ~a" (cdr resp)))
         resp)]
    [else (make-error-response req-id "unexpected response format")]))

;; ── Connection Status / Shutdown ────────────────────────────────

(define (remote-connection-alive? conn)
  (and (remote-connection-in-port conn) (unbox (remote-connection-alive-flag conn))))

(define (close-remote-connection! conn)
  (set-box! (remote-connection-alive-flag conn) #f)
  (clear-all-pending! conn 'shutdown)
  (with-handlers ([exn:fail? void])
    (close-input-port (remote-connection-in-port conn))
    (close-output-port (remote-connection-out-port conn)))
  (custodian-shutdown-all (remote-connection-custodian conn))
  (log-remote-ipc-info "remote connection closed"))

;; ── Provides ────────────────────────────────────────────────────

(provide remote-connection
         remote-connection?
         remote-connection-host
         remote-connection-port
         remote-connection-ssl-context
         remote-connection-in-port
         remote-connection-out-port
         remote-connection-drain-thread
         remote-connection-pending-requests
         remote-connection-lock
         remote-connection-write-lock
         remote-connection-custodian
         remote-connection-alive-flag
         remote-connection-started-ms)

(provide remote-response-packet
         remote-response-packet?
         remote-response-packet-id
         remote-response-packet-response)

(provide exn:fail:remote-connection
         exn:fail:remote-connection?)

(provide (contract-out
          [generate-remote-request-id (-> string?)]
          [start-remote-connection!
           (->* (string? exact-positive-integer? ssl-client-context?)
                (exact-positive-integer?)
                remote-connection?)]
          [send-remote-request!
           (->* (remote-connection? ipc-request?) (exact-positive-integer?) ipc-response?)]
          [remote-connection-alive? (-> remote-connection? boolean?)]
          [close-remote-connection! (-> remote-connection? void?)]))

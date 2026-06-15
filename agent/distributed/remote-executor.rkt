#lang racket/base

;; agent/distributed/remote-executor.rkt — High-level remote executor client
;; STABILITY: evolving
;;
;; W1 (v0.99.12): Wraps remote-ipc.rkt with executor-specific logic:
;;   - Capability token injection (signs a fresh token per request)
;;   - mas-envelope → ipc-request translation
;;   - Connection retry with exponential backoff
;;   - Timeout handling from envelope metadata

(require racket/contract
         racket/match
         racket/string
         openssl
         "../../sandbox/ipc-protocol.rkt"
         "../../util/security/capability-tokens.rkt"
         "remote-ipc.rkt")

;; ── Logging ─────────────────────────────────────────────────────

(define-logger remote-executor)

;; ── Defaults ────────────────────────────────────────────────────

(define DEFAULT-CONNECT-TIMEOUT-MS 10000)
(define DEFAULT-REQUEST-TIMEOUT-MS 120000)
(define MAX-RETRIES 3)
(define RETRY-BASE-DELAY-MS 500)

;; ── Remote Executor Struct ──────────────────────────────────────

(struct remote-executor
        (connection ; remote-connection?
         capability-secret ; string? — HMAC secret for capability tokens
         agent-id ; string? — identity for capability token claims
         max-retries) ; exact-nonnegative-integer?
  #:transparent)

;; ── Lifecycle ───────────────────────────────────────────────────

;; Connect to a remote executor node and wrap it with executor-specific logic.
(define (start-remote-executor! #:host host
                                #:port port
                                #:ssl-context ssl-context
                                #:capability-secret capability-secret
                                #:agent-id agent-id
                                #:max-retries [max-retries MAX-RETRIES]
                                #:connect-timeout-ms [connect-timeout-ms DEFAULT-CONNECT-TIMEOUT-MS])
  (define conn (start-remote-connection! host port ssl-context connect-timeout-ms))
  (remote-executor conn capability-secret agent-id max-retries))

;; ── Execution ───────────────────────────────────────────────────

;; Execute a tool call via the remote executor.
;; Parameters:
;;   executor — remote-executor?
;;   tool-name — string naming the tool to execute
;;   arguments — hash of arguments to pass to the tool
;;   capability — symbol naming the required capability
;;   timeout-ms — per-request timeout (default 120s)
(define (execute-via-remote executor
                            tool-name
                            arguments
                            capability
                            [timeout-ms DEFAULT-REQUEST-TIMEOUT-MS])
  (define conn (remote-executor-connection executor))
  (define req-id (generate-remote-request-id))
  ;; Sign a fresh capability token for this request
  (define cap-token
    (sign-capability-token capability
                           (remote-executor-agent-id executor)
                           (remote-executor-capability-secret executor)))
  ;; Build ipc-request with the capability token embedded in arguments
  ;; The capability field carries the symbol; the token is in arguments
  ;; so the server can verify authenticity.
  (define enriched-args (hash-set arguments 'capability-token cap-token))
  (define req
    (ipc-request req-id
                 tool-name
                 enriched-args
                 timeout-ms
                 #f ; working-dir: remote node decides
                 capability
                 IPC-SCHEMA-VERSION))
  (with-retry executor req timeout-ms 0))

;; ── Retry Logic ─────────────────────────────────────────────────

(define (with-retry executor req timeout-ms attempt)
  (define conn (remote-executor-connection executor))
  (define max-retries (remote-executor-max-retries executor))
  (define result
    (with-handlers ([exn:fail:remote-connection?
                     (lambda (e)
                       (log-remote-executor-warning "remote execution error (attempt ~a): ~a"
                                                    (add1 attempt)
                                                    (exn-message e))
                       ;; Return an error response for retry logic
                       (make-error-response (ipc-request-request-id req) (exn-message e)))])
      (send-remote-request! conn req timeout-ms)))
  (cond
    ;; Success or non-retryable error
    [(eq? (ipc-response-status result) 'ok) result]
    [(>= attempt max-retries)
     (log-remote-executor-warning "max retries (~a) exhausted" max-retries)
     result]
    ;; Retryable: timeout or connection error
    [(or (eq? (ipc-response-status result) 'timeout)
         (and (eq? (ipc-response-status result) 'error)
              (string-contains? (or (ipc-response-error-message result) "") "connection error")))
     (define delay (* RETRY-BASE-DELAY-MS (expt 2 attempt)))
     (log-remote-executor-info "retrying in ~ams (attempt ~a)" delay (add1 attempt))
     (sleep (/ delay 1000.0))
     ;; Check if connection is still alive; if not, we can't retry
     (unless (remote-connection-alive? conn)
       (log-remote-executor-warning "connection dead, cannot retry")
       (return (make-error-response (ipc-request-request-id req)
                                    "connection lost and cannot be re-established")))
     (with-retry executor req timeout-ms (add1 attempt))]
    [else result]))

;; Helper: return value without implicit begin issues
(define (return v)
  v)

;; ── Shutdown ────────────────────────────────────────────────────

(define (shutdown-remote-executor! executor)
  (close-remote-connection! (remote-executor-connection executor))
  (log-remote-executor-info "remote executor shut down"))

;; ── Status ──────────────────────────────────────────────────────

(define (remote-executor-alive? executor)
  (remote-connection-alive? (remote-executor-connection executor)))

;; ── Provides ────────────────────────────────────────────────────

(provide DEFAULT-CONNECT-TIMEOUT-MS
         DEFAULT-REQUEST-TIMEOUT-MS
         MAX-RETRIES)

(provide remote-executor
         remote-executor?
         remote-executor-connection
         remote-executor-capability-secret
         remote-executor-agent-id
         remote-executor-max-retries)

(provide (contract-out
          [start-remote-executor!
           (->* (#:host string?
                        #:port exact-positive-integer?
                        #:ssl-context ssl-client-context?
                        #:capability-secret string?
                        #:agent-id string?)
                (#:max-retries exact-nonnegative-integer?
                               #:connect-timeout-ms exact-positive-integer?)
                remote-executor?)]
          [execute-via-remote
           (->* (remote-executor? string? hash? symbol?) (exact-positive-integer?) ipc-response?)]
          [shutdown-remote-executor! (-> remote-executor? void?)]
          [remote-executor-alive? (-> remote-executor? boolean?)]))

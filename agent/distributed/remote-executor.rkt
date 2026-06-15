#lang racket/base

;; agent/distributed/remote-executor.rkt — High-level remote executor client
;; STABILITY: evolving
;;
;; W1 (v0.99.12): Wraps remote-ipc.rkt with executor-specific logic:
;;   - Capability token injection (signs a fresh token per request)
;;   - mas-envelope → ipc-request translation
;;   - Connection retry with exponential backoff
;;   - Timeout handling from envelope metadata
;;
;; W4 (v0.99.12): Connection resilience + circuit breaker:
;;   - Automatic reconnection with exponential backoff (100ms → 5s)
;;   - Circuit breaker: N failures → open → cooldown → half-open → closed
;;   - Periodic health check (background thread, configurable interval)
;;   - Fail-fast when circuit is open (no wasted connection attempts)

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

;; W4: Circuit breaker defaults
(define DEFAULT-CIRCUIT-FAILURE-THRESHOLD 5)
(define DEFAULT-CIRCUIT-COOLDOWN-MS 30000)
(define DEFAULT-HEALTH-CHECK-INTERVAL-MS 30000)

;; W4: Reconnection defaults
(define RECONNECT-BASE-DELAY-MS 100)
(define RECONNECT-MAX-DELAY-MS 5000)
(define RECONNECT-MAX-ATTEMPTS 3)

;; ── Circuit Breaker State ───────────────────────────────────────

;; Circuit breaker tracks consecutive failures and implements the
;; three-state pattern: closed → open → half-open → closed/open.
;; State is mutable (boxed) and protected by a semaphore.

(struct circuit-breaker
        (failure-count-box ; (boxof exact-nonnegative-integer?)
         state-box ; (boxof (or/c 'closed 'open 'half-open))
         opened-ms-box ; (boxof (or/c number? #f))
         lock ; semaphore?
         threshold ; exact-positive-integer? (immutable)
         cooldown-ms ; exact-positive-integer? (immutable)
         )
  #:transparent)

(define (make-circuit-breaker threshold cooldown-ms)
  (circuit-breaker (box 0) (box 'closed) (box #f) (make-semaphore 1) threshold cooldown-ms))

;; Returns 'allow or 'reject.
;; If open and cooldown elapsed → transition to half-open, return 'allow.
;; If open and cooldown not elapsed → return 'reject.
;; If half-open → return 'allow (one trial request).
;; If closed → return 'allow.
(define (circuit-check! cb)
  (call-with-semaphore
   (circuit-breaker-lock cb)
   (lambda ()
     (define state (unbox (circuit-breaker-state-box cb)))
     (case state
       [(closed) 'allow]
       [(half-open) 'allow]
       [(open)
        (define opened-ms (unbox (circuit-breaker-opened-ms-box cb)))
        (define elapsed (- (current-inexact-milliseconds) (or opened-ms 0)))
        (if (>= elapsed (circuit-breaker-cooldown-ms cb))
            (begin
              (set-box! (circuit-breaker-state-box cb) 'half-open)
              (log-remote-executor-info "circuit breaker: open → half-open (cooldown elapsed)")
              'allow)
            'reject)]
       [else 'allow]))))

;; Reset failure count and close circuit (success path).
(define (circuit-record-success! cb)
  (call-with-semaphore (circuit-breaker-lock cb)
                       (lambda ()
                         (when (not (eq? (unbox (circuit-breaker-state-box cb)) 'closed))
                           (log-remote-executor-info "circuit breaker: → closed (success)"))
                         (set-box! (circuit-breaker-failure-count-box cb) 0)
                         (set-box! (circuit-breaker-state-box cb) 'closed)
                         (set-box! (circuit-breaker-opened-ms-box cb) #f))))

;; Increment failure count, open circuit if threshold reached.
(define (circuit-record-failure! cb)
  (call-with-semaphore
   (circuit-breaker-lock cb)
   (lambda ()
     (define new-count (add1 (unbox (circuit-breaker-failure-count-box cb))))
     (set-box! (circuit-breaker-failure-count-box cb) new-count)
     (when (and (eq? (unbox (circuit-breaker-state-box cb)) 'half-open) (>= new-count 1))
       ;; Half-open failure → re-open immediately
       (set-box! (circuit-breaker-state-box cb) 'open)
       (set-box! (circuit-breaker-opened-ms-box cb) (current-inexact-milliseconds))
       (log-remote-executor-warning "circuit breaker: half-open → open (trial request failed)"))
     (when (and (eq? (unbox (circuit-breaker-state-box cb)) 'closed)
                (>= new-count (circuit-breaker-threshold cb)))
       (set-box! (circuit-breaker-state-box cb) 'open)
       (set-box! (circuit-breaker-opened-ms-box cb) (current-inexact-milliseconds))
       (log-remote-executor-warning "circuit breaker: closed → open (~a consecutive failures)"
                                    new-count)))))

;; Read-only state query for testing.
(define (circuit-state cb)
  (unbox (circuit-breaker-state-box cb)))

(define (circuit-failure-count cb)
  (unbox (circuit-breaker-failure-count-box cb)))

;; ── Remote Executor Struct ──────────────────────────────────────

;; W4: Restructured to hold connection params (for reconnection) and
;; circuit breaker state. The connection is swappable via connection-box.

(struct remote-executor
        (host ; string?
         port ; exact-positive-integer?
         ssl-context ; ssl-client-context?
         capability-secret ; string? — HMAC secret for capability tokens
         agent-id ; string? — identity for capability token claims
         max-retries ; exact-nonnegative-integer?
         connect-timeout-ms ; exact-positive-integer?
         connection-box ; (boxof (or/c remote-connection? #f))
         circuit ; circuit-breaker?
         health-thread-box ; (boxof (or/c thread? #f))
         health-custodian-box ; (boxof (or/c custodian? #f))
         )
  #:transparent)

;; Accessor helper: get current connection (or #f).
(define (get-connection executor)
  (unbox (remote-executor-connection-box executor)))

;; Backward compat accessor.
(define (remote-executor-connection executor)
  (get-connection executor))

;; ── Lifecycle ───────────────────────────────────────────────────

;; Connect to a remote executor node and wrap it with executor-specific logic.
(define (start-remote-executor!
         #:host host
         #:port port
         #:ssl-context ssl-context
         #:capability-secret capability-secret
         #:agent-id agent-id
         #:max-retries [max-retries MAX-RETRIES]
         #:connect-timeout-ms [connect-timeout-ms DEFAULT-CONNECT-TIMEOUT-MS]
         #:circuit-failure-threshold [circuit-failure-threshold DEFAULT-CIRCUIT-FAILURE-THRESHOLD]
         #:circuit-cooldown-ms [circuit-cooldown-ms DEFAULT-CIRCUIT-COOLDOWN-MS]
         #:health-check-interval-ms [health-check-interval-ms DEFAULT-HEALTH-CHECK-INTERVAL-MS]
         #:health-check-enabled [health-check-enabled #t])
  (define conn (start-remote-connection! host port ssl-context connect-timeout-ms))
  (define cb (make-circuit-breaker circuit-failure-threshold circuit-cooldown-ms))
  (define exec
    (remote-executor host
                     port
                     ssl-context
                     capability-secret
                     agent-id
                     max-retries
                     connect-timeout-ms
                     (box conn)
                     cb
                     (box #f)
                     (box #f)))
  (when health-check-enabled
    (start-health-check-thread! exec health-check-interval-ms))
  exec)

;; ── Health Check Thread ─────────────────────────────────────────

;; Background thread that periodically checks connection health.
;; If connection is dead, attempts reconnection.
;; If connection is alive, sends a lightweight ping.
(define (do-health-check! executor)
  (define conn (get-connection executor))
  (cond
    [(not (and conn (remote-connection-alive? conn)))
     (log-remote-executor-info "health check: connection dead, attempting reconnect")
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-remote-executor-warning "health check: reconnect failed: ~a"
                                                               (exn-message e))
                                  (circuit-record-failure! (remote-executor-circuit executor)))])
       (reconnect-executor! executor))]
    [else
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-remote-executor-warning "health check: ping failed: ~a"
                                                               (exn-message e))
                                  (circuit-record-failure! (remote-executor-circuit executor)))])
       (define req-id (generate-remote-request-id))
       (define cap-token
         (sign-capability-token 'health-check
                                (remote-executor-agent-id executor)
                                (remote-executor-capability-secret executor)))
       (define req
         (ipc-request req-id
                      "__health__"
                      (hasheq 'capability-token cap-token)
                      5000
                      #f
                      'health-check
                      IPC-SCHEMA-VERSION))
       (define resp (send-remote-request! conn req 5000))
       (when (memq (ipc-response-status resp) '(timeout error))
         (log-remote-executor-warning "health check: server returned ~a"
                                      (ipc-response-status resp))))]))

(define (start-health-check-thread! executor interval-ms)
  (define hc-custodian (make-custodian))
  (set-box! (remote-executor-health-custodian-box executor) hc-custodian)
  (define t
    (parameterize ([current-custodian hc-custodian])
      (thread (lambda ()
                (let loop ()
                  (sleep (/ interval-ms 1000.0))
                  (do-health-check! executor)
                  (loop))))))
  (set-box! (remote-executor-health-thread-box executor) t))

;; ── Reconnection ────────────────────────────────────────────────

;; Attempt to re-establish a dead connection with exponential backoff.
;; Returns #t on success, #f on failure.
(define (reconnect-executor! executor)
  (define host (remote-executor-host executor))
  (define port (remote-executor-port executor))
  (define ctx (remote-executor-ssl-context executor))
  (define timeout (remote-executor-connect-timeout-ms executor))
  ;; Close old connection if any
  (define old-conn (get-connection executor))
  (when old-conn
    (with-handlers ([exn:fail? void])
      (close-remote-connection! old-conn)))
  ;; Reconnect with exponential backoff
  (let loop ([attempt 0])
    (cond
      [(>= attempt RECONNECT-MAX-ATTEMPTS)
       (log-remote-executor-warning "reconnect: max attempts (~a) exhausted" RECONNECT-MAX-ATTEMPTS)
       #f]
      [else
       (when (> attempt 0)
         (define delay
           (min (* RECONNECT-BASE-DELAY-MS (expt 2 (sub1 attempt))) RECONNECT-MAX-DELAY-MS))
         (log-remote-executor-info "reconnect: backing off ~ams before attempt ~a"
                                   delay
                                   (add1 attempt))
         (sleep (/ delay 1000.0)))
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-remote-executor-warning "reconnect attempt ~a failed: ~a"
                                                                 (add1 attempt)
                                                                 (exn-message e))
                                    (loop (add1 attempt)))])
         (define new-conn (start-remote-connection! host port ctx timeout))
         (set-box! (remote-executor-connection-box executor) new-conn)
         (log-remote-executor-info "reconnect: established new connection to ~a:~a" host port)
         #t)])))

;; ── Execution ───────────────────────────────────────────────────

;; Execute a tool call via the remote executor.
;; Checks circuit breaker state before executing.
;; Attempts reconnection if connection is dead.
;; Records success/failure for circuit breaker.
(define (execute-via-remote executor
                            tool-name
                            arguments
                            capability
                            [timeout-ms DEFAULT-REQUEST-TIMEOUT-MS])
  (define cb (remote-executor-circuit executor))
  ;; Check circuit breaker
  (define circuit-verdict (circuit-check! cb))
  (cond
    [(eq? circuit-verdict 'reject)
     (log-remote-executor-warning "circuit breaker open — fast-failing request")
     (make-error-response #f "circuit breaker is open — too many consecutive failures")]
    [else
     ;; Circuit allows request — execute with retry + reconnect
     (define result (execute-with-resilience executor tool-name arguments capability timeout-ms))
     ;; Record outcome
     (if (eq? (ipc-response-status result) 'ok)
         (circuit-record-success! cb)
         (circuit-record-failure! cb))
     result]))

;; Execute with reconnection + retry logic.
(define (execute-with-resilience executor tool-name arguments capability timeout-ms)
  (define conn (get-connection executor))
  ;; If connection is dead, try to reconnect first
  (unless (and conn (remote-connection-alive? conn))
    (log-remote-executor-info "connection dead, attempting reconnect before request")
    (define reconnected? (reconnect-executor! executor))
    (unless reconnected?
      (return (make-error-response #f "connection lost and reconnection failed"))))
  (define req-id (generate-remote-request-id))
  (define cap-token
    (sign-capability-token capability
                           (remote-executor-agent-id executor)
                           (remote-executor-capability-secret executor)))
  (define enriched-args (hash-set arguments 'capability-token cap-token))
  (define req
    (ipc-request req-id tool-name enriched-args timeout-ms #f capability IPC-SCHEMA-VERSION))
  (with-retry executor req timeout-ms 0))

;; ── Retry Logic ─────────────────────────────────────────────────

(define (with-retry executor req timeout-ms attempt)
  (define conn (get-connection executor))
  (define max-retries (remote-executor-max-retries executor))
  (define result
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-remote-executor-warning "remote execution error (attempt ~a): ~a"
                                                    (add1 attempt)
                                                    (exn-message e))
                       ;; Mark connection as dead on network errors
                       (when (remote-connection-alive? conn)
                         (set-box! (remote-connection-alive-flag conn) #f))
                       (make-error-response (ipc-request-request-id req)
                                            (format "connection error: ~a" (exn-message e))))])
      (send-remote-request! conn req timeout-ms)))
  (cond
    ;; Success or non-retryable error
    [(eq? (ipc-response-status result) 'ok) result]
    [(>= attempt max-retries)
     (log-remote-executor-warning "max retries (~a) exhausted" max-retries)
     result]
    ;; Retryable: timeout or connection error → try reconnect + retry
    [(or (eq? (ipc-response-status result) 'timeout)
         (and (eq? (ipc-response-status result) 'error)
              (string-contains? (or (ipc-response-error-message result) "") "connection error")))
     (define delay (* RETRY-BASE-DELAY-MS (expt 2 attempt)))
     (log-remote-executor-info "retrying in ~ams (attempt ~a)" delay (add1 attempt))
     (sleep (/ delay 1000.0))
     ;; Try to reconnect if connection is dead
     (unless (remote-connection-alive? conn)
       (log-remote-executor-info "connection dead during retry, attempting reconnect")
       (define reconnected? (reconnect-executor! executor))
       (unless reconnected?
         (log-remote-executor-warning "reconnect failed during retry")
         (return (make-error-response (ipc-request-request-id req)
                                      "connection lost and cannot be re-established"))))
     (with-retry executor req timeout-ms (add1 attempt))]
    [else result]))

;; Helper: return value without implicit begin issues
(define (return v)
  v)

;; ── Shutdown ────────────────────────────────────────────────────

(define (shutdown-remote-executor! executor)
  ;; Stop health check thread
  (define hc-custodian (unbox (remote-executor-health-custodian-box executor)))
  (when hc-custodian
    (custodian-shutdown-all hc-custodian))
  ;; Close connection
  (define conn (get-connection executor))
  (when conn
    (close-remote-connection! conn))
  (log-remote-executor-info "remote executor shut down"))

;; ── Status ──────────────────────────────────────────────────────

;; Connection alive AND circuit not in 'open state.
(define (remote-executor-alive? executor)
  (define conn (get-connection executor))
  (and conn
       (remote-connection-alive? conn)
       (not (eq? (circuit-state (remote-executor-circuit executor)) 'open))))

;; ── Circuit Breaker Access (for testing) ────────────────────────

(define (remote-executor-circuit-state executor)
  (circuit-state (remote-executor-circuit executor)))

(define (remote-executor-circuit-failure-count executor)
  (circuit-failure-count (remote-executor-circuit executor)))

;; ── Provides ────────────────────────────────────────────────────

(provide DEFAULT-CONNECT-TIMEOUT-MS
         DEFAULT-REQUEST-TIMEOUT-MS
         MAX-RETRIES
         DEFAULT-CIRCUIT-FAILURE-THRESHOLD
         DEFAULT-CIRCUIT-COOLDOWN-MS
         DEFAULT-HEALTH-CHECK-INTERVAL-MS
         RECONNECT-BASE-DELAY-MS
         RECONNECT-MAX-DELAY-MS
         RECONNECT-MAX-ATTEMPTS)

(provide circuit-breaker
         circuit-breaker?
         make-circuit-breaker)

(provide remote-executor
         remote-executor?
         remote-executor-host
         remote-executor-port
         remote-executor-ssl-context
         remote-executor-capability-secret
         remote-executor-agent-id
         remote-executor-max-retries
         remote-executor-connect-timeout-ms
         remote-executor-connection-box
         remote-executor-circuit
         remote-executor-health-thread-box
         remote-executor-health-custodian-box)

(provide (contract-out
          [circuit-check! (-> circuit-breaker? (or/c 'allow 'reject))]
          [circuit-record-success! (-> circuit-breaker? void?)]
          [circuit-record-failure! (-> circuit-breaker? void?)]
          [circuit-state (-> circuit-breaker? (or/c 'closed 'open 'half-open))]
          [circuit-failure-count (-> circuit-breaker? exact-nonnegative-integer?)]
          [start-remote-executor!
           (->* (#:host string?
                        #:port exact-positive-integer?
                        #:ssl-context ssl-client-context?
                        #:capability-secret string?
                        #:agent-id string?)
                (#:max-retries exact-nonnegative-integer?
                               #:connect-timeout-ms exact-positive-integer?
                               #:circuit-failure-threshold exact-positive-integer?
                               #:circuit-cooldown-ms exact-positive-integer?
                               #:health-check-interval-ms exact-positive-integer?
                               #:health-check-enabled boolean?)
                remote-executor?)]
          [execute-via-remote
           (->* (remote-executor? string? hash? symbol?) (exact-positive-integer?) ipc-response?)]
          [reconnect-executor! (-> remote-executor? boolean?)]
          [shutdown-remote-executor! (-> remote-executor? void?)]
          [remote-executor-alive? (-> remote-executor? boolean?)]
          [remote-executor-connection (-> remote-executor? (or/c remote-connection? #f))]
          [remote-executor-circuit-state (-> remote-executor? (or/c 'closed 'open 'half-open))]
          [remote-executor-circuit-failure-count (-> remote-executor? exact-nonnegative-integer?)]))

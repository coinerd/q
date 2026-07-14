#lang racket/base

;; tui/approval-channel.rkt — HITL approval synchronization
;; STABILITY: evolving
;;
;; v0.99.25 §5.3: Channel-based blocking mechanism for HITL approval.
;; The tool execution thread (session) blocks on channel-get while
;; the TUI main thread collects user input and puts the result.
;;
;; Uses async-channel: put is non-blocking, get blocks with timeout.
;;
;; Thread-safety:
;;   - Uses a module-level box, NOT a parameter. Racket parameters are
;;     NOT inherited by child threads. The session thread spawned by
;;     handle-user-submit! wouldn't see a parameterized value.
;;   - Boxes ARE shared across threads (heap-allocated mutable cells).
;;
;; Deadlock safety:
;;   - Session thread runs in (thread ...) spawned by submit-handler.rkt
;;   - TUI main loop uses sync/timeout 0 — never blocks
;;   - async-channel-get has a timeout (default 120s) to prevent permanent hang

(require racket/contract
         racket/async-channel)

(provide (contract-out [approval-channel? (-> any/c boolean?)]
                       [make-approval-channel
                        (->* () (#:timeout-ms exact-nonnegative-integer?) approval-channel?)]
                       [approval-channel-ch (-> approval-channel? async-channel?)]
                       [approval-channel-timeout-ms (-> approval-channel? exact-nonnegative-integer?)]
                       [set-approval-channel! (-> (or/c approval-channel? #f) void?)]
                       [clear-approval-channel! (-> void?)]
                       [current-approval-channel (-> (or/c approval-channel? #f))]
                       [approval-await-result (->* () () boolean?)])
         approval-put!
         DEFAULT-APPROVAL-TIMEOUT-MS
         ;; v0.99.50 W2 (TMUX-04): Correlated exactly-once approval delivery
         register-approval-request!
         approval-await-for-id
         approval-put-for-id!
         clear-pending-approvals!
         pending-approval-count)

;; ============================================================
;; Approval channel struct
;; ============================================================

;; The async-channel carries boolean? values: #t = approved, #f = denied.
(struct approval-channel (ch timeout-ms) #:transparent)

;; ============================================================
;; Module-level box (thread-safe shared state)
;; ============================================================

;; The box is set once during TUI init and read by spawn-subagent.
;; When #f (non-interactive mode: CLI, JSON, RPC), approval is permissive.
(define current-approval-channel-box (box #f))

;; Default timeout: 120 seconds (2 minutes).
(define DEFAULT-APPROVAL-TIMEOUT-MS 120000)

;; ============================================================
;; Public API
;; ============================================================

(define (make-approval-channel #:timeout-ms [timeout-ms DEFAULT-APPROVAL-TIMEOUT-MS])
  (approval-channel (make-async-channel) timeout-ms))

;; Set the shared approval channel (called by TUI init).
(define (set-approval-channel! ch)
  (set-box! current-approval-channel-box ch)
  (void))

;; Clear the shared approval channel (called on TUI teardown).
;; v0.99.50 W2: Also clears the pending-approvals registry.
(define (clear-approval-channel!)
  (set-box! current-approval-channel-box #f)
  (clear-pending-approvals!)
  (void))

;; Read the current shared approval channel.
;; Returns #f in non-interactive mode.
(define (current-approval-channel)
  (unbox current-approval-channel-box))

;; Await approval result from the TUI.
;; Blocks the calling thread (session) until the TUI puts a result.
;; Returns #t (approved) or #f (denied/timeout).
;; When no channel is set (non-interactive mode), returns #t (permissive).
(define (approval-await-result)
  (define ch (current-approval-channel))
  (cond
    [(not ch) #t] ; Non-interactive: permissive
    [else
     (define timeout-secs (/ (approval-channel-timeout-ms ch) 1000.0))
     (define ach (approval-channel-ch ch))
     (define result (sync/timeout timeout-secs ach))
     ;; sync/timeout returns #f on timeout, or the channel value.
     ;; Channel values are #t (approved) or #f (denied).
     ;; On timeout, return #f (deny). On denial, also #f. Both safe.
     (if result result #f)]))

;; Put an approval result onto the channel (called by TUI key handler).
;; Non-blocking: async-channel-put never blocks.
;; Returns void. If no channel is set, this is a no-op.
(define (approval-put! result)
  (define ch (current-approval-channel))
  (when ch
    (async-channel-put (approval-channel-ch ch) result))
  (void))

;; ============================================================
;; v0.99.50 W2 (TMUX-04): Correlated exactly-once approval delivery
;; ============================================================

;; A pending approval request with a dedicated response channel.
;; Each request gets its own async-channel, so responses can't cross.
(struct pending-approval (id ch created-at) #:transparent)

;; Registry: hash from request-id (symbol) → pending-approval
(define pending-approvals-box (box (hash)))

;; Semaphore for thread-safe registry access
(define registry-sem (make-semaphore 1))

;; Register a new approval request.
;; Returns a unique request-id (symbol) that must be included in the
;; response to correlate it back to this request.
;; Each request gets its own dedicated async-channel.
(define (register-approval-request!)
  (define req-id (gensym 'approval))
  (define pa (pending-approval req-id (make-async-channel) (current-inexact-milliseconds)))
  (call-with-semaphore
   registry-sem
   (lambda () (set-box! pending-approvals-box (hash-set (unbox pending-approvals-box) req-id pa))))
  req-id)

;; Await an approval result for a specific request-id.
;; Blocks until the response arrives or timeout.
;; Returns (values approved? delivered?) where delivered? is #f on timeout
;; or if the request was not found / already consumed.
;; Removes the request from the registry on completion or timeout.
(define (approval-await-for-id req-id [timeout-ms #f])
  (define pa
    (call-with-semaphore registry-sem (lambda () (hash-ref (unbox pending-approvals-box) req-id #f))))
  (cond
    [(not pa) (values #f #f)]
    [else
     (define ach (pending-approval-ch pa))
     (define effective-timeout
       (or timeout-ms
           (let ([ch (current-approval-channel)])
             (if ch
                 (approval-channel-timeout-ms ch)
                 DEFAULT-APPROVAL-TIMEOUT-MS))))
     (define result (sync/timeout (/ effective-timeout 1000.0) ach))
     (call-with-semaphore
      registry-sem
      (lambda () (set-box! pending-approvals-box (hash-remove (unbox pending-approvals-box) req-id))))
     (if result
         (values (car result) #t)
         (values #f #f))]))

;; Put an approval result for a specific request-id (exactly-once).
;; Atomically removes the request from the registry before delivering,
;; so a second put for the same ID is a no-op (returns #f).
;; Returns #t if delivered, #f if the request-id was not found
;; (already answered, timed out, or never registered).
(define (approval-put-for-id! req-id approved?)
  (define pa
    (call-with-semaphore registry-sem
                         (lambda ()
                           (define h (unbox pending-approvals-box))
                           (define found (hash-ref h req-id #f))
                           (when found
                             (set-box! pending-approvals-box (hash-remove h req-id)))
                           found)))
  (cond
    [(not pa) #f]
    [else
     (async-channel-put (pending-approval-ch pa) (list approved?))
     #t]))

;; Clear all pending approval requests (called on TUI teardown).
(define (clear-pending-approvals!)
  (call-with-semaphore registry-sem (lambda () (set-box! pending-approvals-box (hash)))))

;; Count of pending approval requests (for testing/introspection).
(define (pending-approval-count)
  (call-with-semaphore registry-sem (lambda () (hash-count (unbox pending-approvals-box)))))

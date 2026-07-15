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
         racket/async-channel
         (only-in file/sha1 bytes->hex-string)
         (only-in racket/random crypto-random-bytes))

(provide (contract-out [approval-channel? (-> any/c boolean?)]
                       [make-approval-channel
                        (->* () (#:timeout-ms exact-nonnegative-integer?) approval-channel?)]
                       [approval-channel-ch (-> approval-channel? async-channel?)]
                       [approval-channel-timeout-ms (-> approval-channel? exact-nonnegative-integer?)]
                       [set-approval-channel! (-> (or/c approval-channel? #f) void?)]
                       [clear-approval-channel! (-> void?)]
                       [set-headless-approval-mode! (-> void?)]
                       [current-approval-channel (-> (or/c approval-channel? #f))]
                       [headless-approval-mode? (-> boolean?)]
                       [approval-await-result (->* () () boolean?)])
         approval-put!
         DEFAULT-APPROVAL-TIMEOUT-MS
         register-approval-request!
         register-approval-request-for-channel!
         approval-request-pending?
         approval-await-for-id
         approval-put-for-id!
         cancel-approval-request!
         clear-pending-approvals!
         pending-approval-count)

;; The decision channel carries booleans.  The semaphore-peek event is a
;; broadcast cancellation event: one post wakes every legacy waiter without
;; allowing one waiter to consume teardown notification intended for others.
(struct approval-channel (ch timeout-ms cancellation) #:transparent)

(define current-approval-channel-box (box #f))
;; A process begins in explicit non-interactive mode.  Once an interactive
;; generation has existed, teardown changes this to 'closed so surviving work
;; cannot silently fall back to permissive headless approval.
(define approval-lifecycle-mode-box (box 'headless))

(define DEFAULT-APPROVAL-TIMEOUT-MS 120000)

(define (make-approval-channel #:timeout-ms [timeout-ms DEFAULT-APPROVAL-TIMEOUT-MS])
  (approval-channel (make-async-channel) timeout-ms (make-semaphore 0)))

;; Caller holds registry-sem.  Cancel both correlated and legacy waiters from
;; the prior generation before publishing the replacement mode/channel.
(define (replace-approval-channel-locked! ch mode)
  (cancel-pending-locked!)
  (define old (unbox current-approval-channel-box))
  (when old
    (semaphore-post (approval-channel-cancellation old)))
  (set-box! current-approval-channel-box ch)
  (set-box! approval-lifecycle-mode-box mode)
  (set-box! approval-generation-box (add1 (unbox approval-generation-box))))

(define (set-approval-channel! ch)
  (call-with-semaphore registry-sem
                       (lambda () (replace-approval-channel-locked! ch (if ch 'interactive 'closed))))
  (void))

(define (clear-approval-channel!)
  (set-approval-channel! #f))

;; Explicit process-mode transition for CLI/JSON/RPC startup and test isolation.
;; This is intentionally distinct from teardown: clear always remains closed.
(define (set-headless-approval-mode!)
  (call-with-semaphore registry-sem (lambda () (replace-approval-channel-locked! #f 'headless)))
  (void))

(define (current-approval-channel)
  (call-with-semaphore registry-sem (lambda () (unbox current-approval-channel-box))))

(define (headless-approval-mode?)
  (call-with-semaphore registry-sem (lambda () (eq? (unbox approval-lifecycle-mode-box) 'headless))))

(define (approval-await-result)
  (define-values (ch mode)
    (call-with-semaphore
     registry-sem
     (lambda () (values (unbox current-approval-channel-box) (unbox approval-lifecycle-mode-box)))))
  (cond
    [(not ch) (eq? mode 'headless)]
    [else
     (define outcome
       (sync/timeout (/ (approval-channel-timeout-ms ch) 1000.0)
                     (handle-evt (approval-channel-ch ch)
                                 (lambda (decision) (cons 'decision decision)))
                     (handle-evt (semaphore-peek-evt (approval-channel-cancellation ch))
                                 (lambda (_) '(cancelled)))))
     (and (pair? outcome) (eq? (car outcome) 'decision) (eq? (cdr outcome) #t))]))

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

;; The registry, channel generation, and every pending -> terminal transition
;; are protected by this one semaphore. Terminal states are 'delivered,
;; 'timed-out, or 'cancelled. The wake channel carries only a notification;
;; the authoritative result is read from the locked state.
(struct pending-approval (id generation wake state approved? waiter?) #:mutable #:transparent)

(define pending-approvals-box (box (hash)))
(define approval-generation-box (box 0))
(define registry-sem (make-semaphore 1))

;; Caller must hold registry-sem. Transition all live requests to cancelled,
;; empty the registry, and wake their waiters while the replacement is atomic.
(define (cancel-pending-locked!)
  (define pending (unbox pending-approvals-box))
  (set-box! pending-approvals-box (hash))
  (for ([pa (in-hash-values pending)])
    (when (eq? (pending-approval-state pa) 'pending)
      (set-pending-approval-state! pa 'cancelled)
      (async-channel-put (pending-approval-wake pa) 'terminal))))

;; Caller must hold registry-sem. IDs are ordinary strings so event payloads
;; can be serialized as JSON without symbol coercion.
(define (register-approval-request-locked!)
  (define generation (unbox approval-generation-box))
  (define req-id (format "approval-~a-~a" generation (bytes->hex-string (crypto-random-bytes 16))))
  (define pa (pending-approval req-id generation (make-async-channel) 'pending #f #f))
  (set-box! pending-approvals-box (hash-set (unbox pending-approvals-box) req-id pa))
  req-id)

;; Compatibility API for legacy callers and tests that do not bind registration
;; to an observed channel.
(define (register-approval-request!)
  (call-with-semaphore registry-sem register-approval-request-locked!))

;; Atomically register only when EXPECTED-CH is still the exact active channel.
;; This closes the observation -> registration race during TUI replacement.
(define (register-approval-request-for-channel! expected-ch)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (if (and (approval-channel? expected-ch)
                                  (eq? expected-ch (unbox current-approval-channel-box)))
                             (register-approval-request-locked!)
                             #f))))

;; Caller must hold registry-sem. Normally the death monitor eagerly removes an
;; abandoned waiter; this sweep also makes introspection deterministic if the
;; owner and monitor become runnable at the same time.
(define (remove-abandoned-waiters-locked!)
  (define retained
    (for/fold ([retained (unbox pending-approvals-box)])
              ([(req-id pa) (in-hash (unbox pending-approvals-box))])
      (define waiter (pending-approval-waiter? pa))
      (if (and (thread? waiter) (thread-dead? waiter) (eq? (pending-approval-state pa) 'pending))
          (begin
            (set-pending-approval-state! pa 'cancelled)
            (hash-remove retained req-id))
          retained)))
  (set-box! pending-approvals-box retained))

;; True only for a live request in the active channel generation. Terminal and
;; stale records are never valid TUI requests.
(define (approval-request-pending? req-id)
  (and (string? req-id)
       (call-with-semaphore registry-sem
                            (lambda ()
                              (remove-abandoned-waiters-locked!)
                              (let ([pa (hash-ref (unbox pending-approvals-box) req-id #f)])
                                (and pa
                                     (= (pending-approval-generation pa)
                                        (unbox approval-generation-box))
                                     (eq? (pending-approval-state pa) 'pending)))))))

;; Remove exactly the requested retained record. EXPECTED-PA prevents an
;; abandoned waiter from touching any other record (however unlikely an ID
;; collision may be). The caller must hold registry-sem.
(define (cancel-approval-request-locked! req-id [expected-pa #f])
  (define pending (unbox pending-approvals-box))
  (define pa (hash-ref pending req-id #f))
  (cond
    [(or (not pa) (and expected-pa (not (eq? pa expected-pa)))) #f]
    [else
     (set-box! pending-approvals-box (hash-remove pending req-id))
     (when (eq? (pending-approval-state pa) 'pending)
       (set-pending-approval-state! pa 'cancelled)
       (async-channel-put (pending-approval-wake pa) 'terminal))
     #t]))

;; Explicit exactly-once cleanup. It also wakes a live waiter; a delivered
;; result that has not yet been claimed is discarded by explicit cancellation.
(define (cancel-approval-request! req-id)
  (and (string? req-id)
       (parameterize-break
        #f
        (call-with-semaphore registry-sem (lambda () (cancel-approval-request-locked! req-id))))))

;; Await an approval result for a specific request-id. A timeout does not decide
;; outside the lock: after sync/timeout, it competes with response/cancellation
;; for the single pending -> terminal transition.
(define (approval-await-for-id req-id [timeout-ms #f])
  (cond
    [(or (not (string? req-id)) (and timeout-ms (not (exact-nonnegative-integer? timeout-ms))))
     (values #f #f)]
    [else
     ;; Once this call claims waiter ownership, dynamic-wind handles normal
     ;; escapes/breaks/exceptions and a death monitor handles kill-thread.
     (define claimed-pa #f)
     (define waiter-finished (make-semaphore 0))
     (dynamic-wind
      void
      (lambda ()
        (define-values (mode value detail)
          (call-with-semaphore
           registry-sem
           (lambda ()
             (define pending (unbox pending-approvals-box))
             (define pa (hash-ref pending req-id #f))
             (define ch (unbox current-approval-channel-box))
             (cond
               [(not pa) (values 'done #f #f)]
               [(eq? (pending-approval-state pa) 'delivered)
                (set-box! pending-approvals-box (hash-remove pending req-id))
                (values 'done (pending-approval-approved? pa) #t)]
               [(or (not (eq? (pending-approval-state pa) 'pending)) (pending-approval-waiter? pa))
                (values 'done #f #f)]
               [else
                (define owner (current-thread))
                ;; Assign first so an interruption at any later instruction is
                ;; covered by the post thunk.
                (set! claimed-pa pa)
                (set-pending-approval-waiter?! pa owner)
                ;; kill-thread does not unwind the target continuation. A small
                ;; monitor therefore performs the same exact-record cleanup if
                ;; the owning thread dies without running the post thunk.
                (thread (lambda ()
                          (define abandoned?
                            (sync (handle-evt (thread-dead-evt owner) (lambda (_) #t))
                                  (handle-evt waiter-finished (lambda (_) #f))))
                          (when abandoned?
                            (parameterize-break #f
                                                (call-with-semaphore
                                                 registry-sem
                                                 (lambda ()
                                                   (cancel-approval-request-locked! req-id pa)))))))
                (values 'wait
                        pa
                        (or timeout-ms
                            (and ch (approval-channel-timeout-ms ch))
                            DEFAULT-APPROVAL-TIMEOUT-MS))]))))
        (cond
          [(eq? mode 'done) (values value detail)]
          [else
           (define pa value)
           (define effective-timeout detail)
           (sync/timeout (/ effective-timeout 1000.0) (pending-approval-wake pa))
           (call-with-semaphore registry-sem
                                (lambda ()
                                  (define pending (unbox pending-approvals-box))
                                  (when (eq? (hash-ref pending req-id #f) pa)
                                    (set-box! pending-approvals-box (hash-remove pending req-id)))
                                  (case (pending-approval-state pa)
                                    [(delivered) (values (pending-approval-approved? pa) #t)]
                                    [(cancelled timed-out) (values #f #f)]
                                    [else
                                     (set-pending-approval-state! pa 'timed-out)
                                     (values #f #f)])))]))
      (lambda ()
        (when claimed-pa
          (parameterize-break #f
                              (call-with-semaphore
                               registry-sem
                               (lambda () (cancel-approval-request-locked! req-id claimed-pa)))))
        (semaphore-post waiter-finished)))]))

;; Put an approval result for a specific request-id (exactly once). Malformed
;; correlated input is rejected rather than treated as a legacy approval.
(define (approval-put-for-id! req-id approved?)
  (cond
    [(or (not (string? req-id)) (not (boolean? approved?))) #f]
    [else
     (call-with-semaphore registry-sem
                          (lambda ()
                            (remove-abandoned-waiters-locked!)
                            (let* ([pending (unbox pending-approvals-box)]
                                   [pa (hash-ref pending req-id #f)])
                              (cond
                                [(or (not pa)
                                     (not (= (pending-approval-generation pa)
                                             (unbox approval-generation-box)))
                                     (not (eq? (pending-approval-state pa) 'pending)))
                                 #f]
                                [else
                                 (set-pending-approval-approved?! pa approved?)
                                 (set-pending-approval-state! pa 'delivered)
                                 ;; A registered request may be answered just before its owner enters
                                 ;; await. Keep that terminal result until await claims it; an already
                                 ;; waiting owner holds PA directly, so its registry entry can go now.
                                 (when (pending-approval-waiter? pa)
                                   (set-box! pending-approvals-box (hash-remove pending req-id)))
                                 (async-channel-put (pending-approval-wake pa) 'terminal)
                                 #t]))))]))

;; Cancel and wake all pending requests without changing the active generation.
(define (clear-pending-approvals!)
  (call-with-semaphore registry-sem cancel-pending-locked!)
  (void))

;; Count every retained registry record, including a delivered response waiting
;; to be claimed. This makes retained terminal records visible as potential leaks.
(define (pending-approval-count)
  (call-with-semaphore registry-sem
                       (lambda ()
                         (remove-abandoned-waiters-locked!)
                         (hash-count (unbox pending-approvals-box)))))

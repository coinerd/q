#lang racket/base

;; @speed fast
;; @suite security

;; tests/test-approval-correlation.rkt
;; v0.99.50 W2 (TMUX-04): Correlated exactly-once HITL approval delivery.
;;
;; Verifies:
;; 1. register → await → put-for-id delivers correct result
;; 2. duplicate put-for-id is exactly-once (second returns #f)
;; 3. mismatched IDs don't deliver to wrong request
;; 4. timeout cleans up registry (no leak)
;; 5. clear-pending-approvals cleans up all pending
;; 6. concurrent requests get unique IDs and channels
;; 7. request-spawn-approval emits request-id in event payload

(require rackunit
         rackunit/text-ui
         json
         racket/list
         "../tui/approval-channel.rkt"
         "../tui/state-types.rkt"
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  tui-ctx-ui-state-box
                  handle-approval-overlay-key)
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt")

(define suite
  (test-suite "Correlated Exactly-Once Approval Delivery (v0.99.50 W2)"

    ;; ── Setup: ensure clean state ──
    (test-case "starts with empty registry"
      (clear-pending-approvals!)
      (check-equal? (pending-approval-count) 0))

    ;; ── 1. Basic register → await → put-for-id ──

    (test-case "request IDs survive JSON round trip and still correlate"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-true (string? req-id))
      (define wire (jsexpr->string (hasheq 'request-id req-id)))
      (define recovered-id (hash-ref (string->jsexpr wire) 'request-id))
      (check-equal? recovered-id req-id)
      (check-true (approval-put-for-id! recovered-id #t))
      (define-values (approved? delivered?) (approval-await-for-id req-id 10))
      (check-true approved?)
      (check-true delivered?)
      (check-equal? (pending-approval-count) 0))

    (test-case "register then put-for-id delivers result"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-equal? (pending-approval-count) 1)
      (define result-box (box #f))
      (define t
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id))
                  (set-box! result-box (list approved? delivered?)))))
      (sleep 0.05)
      (check-true (approval-put-for-id! req-id #t))
      (thread-wait t)
      (check-equal? (unbox result-box) (list #t #t))
      (check-equal? (pending-approval-count) 0))

    (test-case "delivered-before-await remains counted until exactly-once claim"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-true (approval-request-pending? req-id))
      (check-true (approval-put-for-id! req-id #t))
      (check-false (approval-request-pending? req-id))
      (check-equal? (pending-approval-count) 1 "retained terminal result must be visible")
      (define-values (approved? delivered?) (approval-await-for-id req-id 10))
      (check-true approved?)
      (check-true delivered?)
      (check-equal? (pending-approval-count) 0)
      (define-values (again-approved? again-delivered?) (approval-await-for-id req-id 10))
      (check-false again-approved?)
      (check-false again-delivered?))

    (test-case "register then put-for-id delivers denial"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (define result-box (box #f))
      (define t
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id))
                  (set-box! result-box (list approved? delivered?)))))
      (sleep 0.05)
      (check-true (approval-put-for-id! req-id #f))
      (thread-wait t)
      (check-equal? (unbox result-box) (list #f #t)))

    ;; ── 2. Exactly-once: duplicate put returns #f ──

    (test-case "duplicate put-for-id is exactly-once"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      ;; First put transitions the retained record to delivered.
      (check-true (approval-put-for-id! req-id #t))
      ;; Second put sees a terminal record — exactly-once enforcement
      (check-false (approval-put-for-id! req-id #t))
      (check-false (approval-put-for-id! req-id #f))
      (check-equal? (pending-approval-count) 1)
      (check-true (cancel-approval-request! req-id)))

    ;; ── 3. Mismatched IDs don't cross ──

    (test-case "mismatched ID does not deliver to wrong request"
      (clear-pending-approvals!)
      (define req-a (register-approval-request!))
      (define req-b (register-approval-request!))
      (check-not-equal? req-a req-b)
      (check-equal? (pending-approval-count) 2)
      ;; Put for B should not deliver to A. B remains retained for its owner.
      (check-true (approval-put-for-id! req-b #t))
      (check-true (approval-request-pending? req-a))
      (check-false (approval-request-pending? req-b))
      (check-equal? (pending-approval-count) 2)
      ;; Now put for A; both terminal records remain visible until claimed.
      (check-true (approval-put-for-id! req-a #f))
      (check-equal? (pending-approval-count) 2)
      (define-values (_a a-delivered?) (approval-await-for-id req-a 10))
      (define-values (_b b-delivered?) (approval-await-for-id req-b 10))
      (check-true a-delivered?)
      (check-true b-delivered?)
      (check-equal? (pending-approval-count) 0))

    ;; ── 4. Timeout cleans up registry ──

    (test-case "timeout removes request from registry"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-equal? (pending-approval-count) 1)
      (define-values (approved? delivered?) (approval-await-for-id req-id 50))
      (check-false approved?)
      (check-false delivered?)
      (check-equal? (pending-approval-count) 0))

    ;; ── 5. clear-pending-approvals cleans up ──

    (test-case "clear-pending-approvals removes all pending"
      (clear-pending-approvals!)
      (register-approval-request!)
      (register-approval-request!)
      (register-approval-request!)
      (check-equal? (pending-approval-count) 3)
      (clear-pending-approvals!)
      (check-equal? (pending-approval-count) 0))

    ;; ── 6. Unique IDs for concurrent requests ──

    (test-case "concurrent requests get unique IDs"
      (clear-pending-approvals!)
      (define output (make-channel))
      (define workers
        (for/list ([_ (in-range 10)])
          (thread (lambda () (channel-put output (register-approval-request!))))))
      (define ids
        (for/list ([_ (in-range 10)])
          (channel-get output)))
      (for-each thread-wait workers)
      (check-equal? (length ids) 10)
      (check-equal? (length (remove-duplicates ids)) 10)
      (check-equal? (pending-approval-count) 10)
      (clear-pending-approvals!))

    ;; ── 7. await for unknown ID returns #f #f ──

    (test-case "await for unknown ID returns not-delivered"
      (clear-pending-approvals!)
      (define-values (approved? delivered?) (approval-await-for-id "nonexistent-id" 10))
      (check-false approved?)
      (check-false delivered?))

    (test-case "correlated API rejects malformed IDs and non-boolean decisions"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-false (approval-put-for-id! (string->symbol req-id) #t))
      (check-false (approval-put-for-id! req-id 'yes))
      (check-equal? (pending-approval-count) 1)
      (define-values (approved? delivered?) (approval-await-for-id 42 10))
      (check-false approved?)
      (check-false delivered?)
      (check-true (approval-put-for-id! req-id #f))
      (check-equal? (pending-approval-count) 1)
      (check-true (cancel-approval-request! req-id))
      (check-false (cancel-approval-request! req-id))
      (check-equal? (pending-approval-count) 0))

    (test-case "explicit cancellation is exactly once and invalidates the request"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (check-true (approval-request-pending? req-id))
      (check-true (cancel-approval-request! req-id))
      (check-false (cancel-approval-request! req-id))
      (check-false (approval-request-pending? req-id))
      (check-false (approval-put-for-id! req-id #t))
      (check-equal? (pending-approval-count) 0))

    (test-case "barrier-synchronized timeout and response have one terminal winner"
      (clear-pending-approvals!)
      (for ([_ (in-range 80)])
        (define req-id (register-approval-request!))
        (define gate (make-semaphore 0))
        (define ready (make-channel))
        (define waiter-result (box 'waiting))
        (define put-result (box 'waiting))
        (define waiter
          (thread (lambda ()
                    (channel-put ready 'waiter)
                    (sync (semaphore-peek-evt gate))
                    (define-values (approved? delivered?) (approval-await-for-id req-id 1))
                    (set-box! waiter-result (list approved? delivered?)))))
        (define responder
          (thread (lambda ()
                    (channel-put ready 'responder)
                    (sync (semaphore-peek-evt gate))
                    (set-box! put-result (approval-put-for-id! req-id #t)))))
        (channel-get ready)
        (channel-get ready)
        (semaphore-post gate)
        (thread-wait waiter)
        (thread-wait responder)
        (if (unbox put-result)
            (check-equal? (unbox waiter-result) '(#t #t))
            (check-equal? (unbox waiter-result) '(#f #f)))
        (check-equal? (pending-approval-count) 0)))

    ;; ── 8. clear-approval-channel! also clears pending ──

    (test-case "clear-approval-channel! also clears pending approvals"
      (clear-pending-approvals!)
      (register-approval-request!)
      (register-approval-request!)
      (check-equal? (pending-approval-count) 2)
      (clear-approval-channel!)
      (check-equal? (pending-approval-count) 0))

    ;; ── 9. Cross-thread: full correlated lifecycle ──

    (test-case "full correlated lifecycle across threads"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (define result-box (box #f))
      ;; Session thread blocks on correlated await
      (define t
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id))
                  (set-box! result-box approved?))))
      ;; Simulate TUI key handler delivering correlated response
      (sleep 0.05)
      (check-true (approval-put-for-id! req-id #t))
      (thread-wait t)
      (check-equal? (unbox result-box) #t)
      ;; Registry is now empty
      (check-equal? (pending-approval-count) 0))

    ;; ── 10. request-spawn-approval emits request-id in event ──

    (test-case "request-spawn-approval emits request-id when channel is set"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 100))
      (define events-received '())
      (define mock-publisher
        (lambda (event-type payload)
          (set! events-received (cons (cons event-type payload) events-received))))
      (define mock-ctx (make-exec-context #:event-publisher mock-publisher))
      ;; Spawn a thread to deliver the approval (will timeout otherwise)
      (thread (lambda ()
                (sleep 0.05)
                ;; Find the request-id from the event and deliver
                (when (pair? events-received)
                  (define payload (cdar events-received))
                  (define req-id (hash-ref payload 'request-id #f))
                  (when req-id
                    (approval-put-for-id! req-id #t)))))
      (define approved (request-spawn-approval '(shell-exec) "dangerous task" mock-ctx))
      (check-true approved "should be approved via correlated delivery")
      (check-true (pair? events-received) "should have emitted an event")
      (define payload (cdar events-received))
      (check-true (hash-has-key? payload 'request-id) "event must contain request-id")
      (clear-approval-channel!))

    ;; ── 11. Stale approval cannot auto-approve new request ──

    (test-case "stale approval does not leak to new request"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 100))
      ;; First request registers and times out
      (define req-1 (register-approval-request!))
      (define-values (a1 d1) (approval-await-for-id req-1 50))
      (check-false d1 "first request timed out")
      (check-equal? (pending-approval-count) 0)
      ;; Stale approval for req-1 arrives (late response)
      (check-false (approval-put-for-id! req-1 #t) "stale approval must be rejected")
      ;; New request gets a fresh ID
      (define req-2 (register-approval-request!))
      (check-not-equal? req-1 req-2)
      ;; The stale #t is NOT sitting on req-2's channel
      (define-values (a2 d2) (approval-await-for-id req-2 50))
      (check-false d2 "new request must not receive stale approval")
      (clear-approval-channel!))

    (test-case "publisher failure cancels the registered approval request"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 5000))
      (define ctx
        (make-exec-context #:event-publisher (lambda (_type _payload)
                                               (error 'publisher "simulated failure"))))
      (check-exn #rx"simulated failure"
                 (lambda () (request-spawn-approval '(shell-exec) "blocked" ctx)))
      (check-equal? (pending-approval-count) 0)
      (clear-approval-channel!))

    ;; ── 12. Key handler delivers correlated response ──

    (test-case "key handler delivers via approval-put-for-id! when request-id present"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 5000))
      (define req-id (register-approval-request!))
      ;; Build a tui-ctx with an approval overlay that has request-id in extra
      (define ctx (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))
      (set-box!
       (tui-ctx-ui-state-box ctx)
       (struct-copy
        ui-state
        (initial-ui-state)
        [active-overlay
         (overlay-state
          'approval-prompt
          '()
          ""
          'top-left
          #f
          #f
          0
          (hasheq 'capabilities '(shell-exec) 'task-preview "correlated test" 'request-id req-id))]))
      ;; Session thread blocks on correlated await
      (define result-box (box #f))
      (define t
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id))
                  (set-box! result-box approved?))))
      (sleep 0.05)
      ;; User presses 'y' — key handler uses correlated delivery
      (define handler-result (handle-approval-overlay-key ctx #\y))
      (check-equal? handler-result 'handled)
      (thread-wait t)
      (check-equal? (unbox result-box) #t)
      (check-equal? (pending-approval-count) 0)
      (clear-approval-channel!))

    ;; ── 13. Missing correlation fails closed ──

    (test-case "key handler never uses legacy channel when request-id is missing"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 5000))
      ;; Build a tui-ctx with an approval overlay WITHOUT request-id in extra
      (define ctx (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))
      (set-box! (tui-ctx-ui-state-box ctx)
                (struct-copy ui-state
                             (initial-ui-state)
                             [active-overlay
                              (overlay-state
                               'approval-prompt
                               '()
                               ""
                               'top-left
                               #f
                               #f
                               0
                               (hasheq 'capabilities '(shell-exec) 'task-preview "legacy test"))]))
      ;; A positive decision would be unambiguously observable if legacy
      ;; fallback remained.  It must instead be consumed with no delivery.
      (define handler-result (handle-approval-overlay-key ctx #\y))
      (check-equal? handler-result 'handled)
      (check-not-false (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
      (define ch (current-approval-channel))
      (check-not-false ch "channel should be set")
      (check-false (sync/timeout 0.1 (approval-channel-ch ch)))
      (clear-approval-channel!))))

(run-tests suite)

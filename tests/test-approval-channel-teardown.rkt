#lang racket

;; @speed fast
;; @suite security

;; tests/test-approval-channel-teardown.rkt
;; Verify exception-safe channel cleanup, including the shared lifecycle extent
;; used by both full and simple TUI entry points.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  approval-put!
                  approval-await-result
                  register-approval-request!
                  register-approval-request-for-channel!
                  approval-request-pending?
                  approval-await-for-id
                  approval-put-for-id!
                  cancel-approval-request!
                  clear-pending-approvals!
                  pending-approval-count)
         (only-in "../tui/tui-init.rkt" call-with-tui-approval-channel))

(define suite
  (test-suite "Approval Channel Teardown (v0.99.26 W0 E-2)"

    (test-case "channel is cleared after explicit clear call"
      ;; Simulate TUI init
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      ;; Simulate TUI teardown
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "TUI lifecycle clears after normal completion"
      (call-with-tui-approval-channel (lambda () (check-not-false (current-approval-channel))))
      (check-false (current-approval-channel)))

    (test-case "TUI lifecycle clears after setup or loop exception"
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (call-with-tui-approval-channel (lambda ()
                                          (check-not-false (current-approval-channel))
                                          (raise (exn:fail "simulated setup/loop crash"
                                                           (current-continuation-marks))))))
      (check-false (current-approval-channel)))

    (test-case "channel is cleared even after simulated exception"
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "simulated crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      (check-false (current-approval-channel)))

    (test-case "channel works correctly after re-init following exception"
      ;; First session crashes
      (set-approval-channel! (make-approval-channel))
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      (check-false (current-approval-channel))
      ;; Second session starts fresh
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      ;; Approval works in new session
      (define result-box (box #f))
      (define t (thread (lambda () (set-box! result-box (approval-await-result)))))
      (sleep 0.05)
      (approval-put! #t)
      (thread-wait t)
      (check-equal? (unbox result-box) #t)
      (clear-approval-channel!))

    (test-case "teardown cancels pending requests and wakes waiters immediately"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define req-id (register-approval-request!))
      (define result-box (box 'waiting))
      (define waiter
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id))
                  (set-box! result-box (list approved? delivered?)))))
      (sleep 0.02)
      (clear-approval-channel!)
      (check-not-false (sync/timeout 0.25 waiter) "teardown must wake the waiter")
      (check-equal? (unbox result-box) '(#f #f))
      (check-false (approval-put-for-id! req-id #t))
      (check-equal? (pending-approval-count) 0))

    (test-case "teardown wakes every correlated waiter in the generation"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define ids
        (for/list ([_ (in-range 5)])
          (register-approval-request!)))
      (define results
        (for/list ([_ (in-range 5)])
          (box 'waiting)))
      (define waiters
        (for/list ([req-id (in-list ids)]
                   [result (in-list results)])
          (thread (lambda ()
                    (define-values (approved? delivered?) (approval-await-for-id req-id))
                    (set-box! result (list approved? delivered?))))))
      (sleep 0.02)
      (clear-approval-channel!)
      (for ([waiter (in-list waiters)]
            [result (in-list results)])
        (check-not-false (sync/timeout 0.25 waiter))
        (check-equal? (unbox result) '(#f #f)))
      (check-equal? (pending-approval-count) 0))

    (test-case "channel replacement atomically cancels the old generation"
      (clear-pending-approvals!)
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define old-id (register-approval-request!))
      (define old-result (box 'waiting))
      (define old-waiter
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id old-id))
                  (set-box! old-result (list approved? delivered?)))))
      (sleep 0.02)
      (set-approval-channel! (make-approval-channel #:timeout-ms 1000))
      (check-not-false (sync/timeout 0.25 old-waiter) "replacement must wake old waiter")
      (check-equal? (unbox old-result) '(#f #f))
      (check-false (approval-put-for-id! old-id #t) "old-generation ID must be stale")
      (check-equal? (pending-approval-count) 0)
      (define new-id (register-approval-request!))
      (check-not-equal? old-id new-id)
      (check-true (approval-put-for-id! new-id #t))
      (check-equal? (pending-approval-count) 1)
      (define-values (approved? delivered?) (approval-await-for-id new-id 10))
      (check-true approved?)
      (check-true delivered?)
      (check-equal? (pending-approval-count) 0)
      (clear-approval-channel!))

    (test-case "registration fails atomically when observed channel was replaced"
      (clear-pending-approvals!)
      (define observed (make-approval-channel))
      (set-approval-channel! observed)
      (define replacement (make-approval-channel))
      (set-approval-channel! replacement)
      (check-false (register-approval-request-for-channel! observed))
      (check-equal? (pending-approval-count) 0)
      (define current-id (register-approval-request-for-channel! replacement))
      (check-true (string? current-id))
      (check-true (approval-request-pending? current-id))
      (check-true (cancel-approval-request! current-id))
      (clear-approval-channel!))

    (test-case "pending query rejects stale generations and terminal records"
      (clear-pending-approvals!)
      (define old-channel (make-approval-channel))
      (set-approval-channel! old-channel)
      (define old-id (register-approval-request-for-channel! old-channel))
      (check-true (approval-request-pending? old-id))
      (set-approval-channel! (make-approval-channel))
      (check-false (approval-request-pending? old-id))
      (define current-id (register-approval-request-for-channel! (current-approval-channel)))
      (check-true (approval-put-for-id! current-id #t))
      (check-false (approval-request-pending? current-id))
      (check-true (cancel-approval-request! current-id))
      (clear-approval-channel!))

    (test-case "clear-pending wakes waiters and leaves registry empty"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (define result-box (box 'waiting))
      (define waiter
        (thread (lambda ()
                  (define-values (approved? delivered?) (approval-await-for-id req-id 60000))
                  (set-box! result-box (list approved? delivered?)))))
      (sleep 0.02)
      (clear-pending-approvals!)
      (check-not-false (sync/timeout 0.25 waiter))
      (check-equal? (unbox result-box) '(#f #f))
      (check-equal? (pending-approval-count) 0))

    (test-case "killing an abandoned waiter cleans its registry entry"
      (clear-pending-approvals!)
      (define req-id (register-approval-request!))
      (define entering (make-semaphore 0))
      (define waiter
        (thread (lambda ()
                  (semaphore-post entering)
                  (approval-await-for-id req-id 60000))))
      (semaphore-wait entering)
      (sleep 0.02)
      (kill-thread waiter)
      (thread-wait waiter)
      (check-equal? (pending-approval-count) 0)
      (check-false (approval-request-pending? req-id))
      (check-false (approval-put-for-id! req-id #t)))

    (test-case "teardown broadcasts cancellation to every legacy waiter"
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define result-boxes
        (for/list ([_ (in-range 4)])
          (box 'waiting)))
      (define waiters
        (for/list ([result-box (in-list result-boxes)])
          (thread (lambda () (set-box! result-box (approval-await-result))))))
      (sleep 0.02)
      (clear-approval-channel!)
      (for ([waiter (in-list waiters)]
            [result-box (in-list result-boxes)])
        (check-not-false (sync/timeout 0.25 waiter) "every legacy waiter must wake on teardown")
        (check-false (unbox result-box))))

    (test-case "crashed interactive generation fails closed after teardown"
      (set-approval-channel! (make-approval-channel))
      (with-handlers ([exn:fail? (lambda (_) (void))])
        (dynamic-wind void
                      (lambda () (raise (exn:fail "crash" (current-continuation-marks))))
                      (lambda () (clear-approval-channel!))))
      (check-false (current-approval-channel))
      (check-false (approval-await-result)))))

(run-tests suite)

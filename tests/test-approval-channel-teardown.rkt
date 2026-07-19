#lang racket/base

;; @speed fast
;; @suite security

;; Exception-safe and atomic teardown for digest-bound approval requests.

(require rackunit
         rackunit/text-ui
         "../runtime/approval/broker.rkt"
         (only-in "../tui/tui-init.rkt" call-with-tui-approval-channel))

(define digest-a (make-string 64 #\a))
(define view-a (hasheq 'task-preview "teardown test" 'capabilities '(shell-exec)))

(define (register-current!)
  (register-approval-request-for-channel! (current-approval-channel) digest-a view-a))

(define suite
  (test-suite "Approval Channel Teardown"

    (test-case "explicit teardown clears the channel"
      (set-approval-channel! (make-approval-channel))
      (check-not-false (current-approval-channel))
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "TUI lifecycle clears after normal completion"
      (call-with-tui-approval-channel (lambda () (check-not-false (current-approval-channel))))
      (check-false (current-approval-channel)))

    (test-case "TUI lifecycle clears after an exception"
      (check-exn #rx"simulated crash"
                 (lambda ()
                   (call-with-tui-approval-channel (lambda ()
                                                     (check-not-false (current-approval-channel))
                                                     (error 'test "simulated crash")))))
      (check-false (current-approval-channel)))

    (test-case "teardown atomically cancels a pending request and wakes its waiter"
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define id (register-current!))
      (define result (box 'waiting))
      (define waiter
        (thread (lambda ()
                  (define-values (outcome grant) (approval-await-grant id digest-a))
                  (set-box! result (list outcome grant)))))
      (sleep 0.02)
      (clear-approval-channel!)
      (check-not-false (sync/timeout 0.25 waiter) "teardown must wake the waiter")
      (check-equal? (unbox result) '(cancelled #f))
      (check-false (approval-decide! id digest-a #t))
      (check-equal? (pending-approval-count) 0))

    (test-case "teardown wakes every request in the active generation"
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define ids
        (for/list ([_ (in-range 5)])
          (register-current!)))
      (define results
        (for/list ([_ (in-range 5)])
          (box 'waiting)))
      (define waiters
        (for/list ([id (in-list ids)]
                   [result (in-list results)])
          (thread (lambda ()
                    (define-values (outcome grant) (approval-await-grant id digest-a))
                    (set-box! result (list outcome grant))))))
      (sleep 0.02)
      (clear-approval-channel!)
      (for ([waiter (in-list waiters)]
            [result (in-list results)])
        (check-not-false (sync/timeout 0.25 waiter))
        (check-equal? (unbox result) '(cancelled #f)))
      (check-equal? (pending-approval-count) 0))

    (test-case "channel replacement atomically invalidates the old generation"
      (define old-channel (make-approval-channel #:timeout-ms 60000))
      (set-approval-channel! old-channel)
      (define old-id (register-current!))
      (define old-result (box 'waiting))
      (define old-waiter
        (thread (lambda ()
                  (define-values (outcome grant) (approval-await-grant old-id digest-a))
                  (set-box! old-result (list outcome grant)))))
      (sleep 0.02)
      (define replacement (make-approval-channel))
      (set-approval-channel! replacement)
      (check-not-false (sync/timeout 0.25 old-waiter))
      (check-equal? (unbox old-result) '(cancelled #f))
      (check-false (approval-decide! old-id digest-a #t))
      (check-false (register-approval-request-for-channel! old-channel digest-a view-a))
      (define new-id (register-approval-request-for-channel! replacement digest-a view-a))
      (check-true (string? new-id))
      (check-not-equal? old-id new-id)
      (check-true (cancel-approval-request! new-id))
      (clear-approval-channel!))

    (test-case "teardown racing an approval cannot leave a stale grant"
      (for ([_ (in-range 50)])
        (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
        (define id (register-current!))
        (define gate (make-semaphore 0))
        (define decider
          (thread (lambda ()
                    (sync (semaphore-peek-evt gate))
                    (approval-decide! id digest-a #t))))
        (define clearer
          (thread (lambda ()
                    (sync (semaphore-peek-evt gate))
                    (clear-approval-channel!))))
        (semaphore-post gate)
        (thread-wait decider)
        (thread-wait clearer)
        (define-values (outcome grant) (approval-await-grant id digest-a 1))
        (check-equal? outcome 'cancelled)
        (check-false grant)
        (check-equal? (pending-approval-count) 0)))

    (test-case "clear-pending atomically wakes waiters and empties the registry"
      (set-approval-channel! (make-approval-channel #:timeout-ms 60000))
      (define id (register-current!))
      (define result (box 'waiting))
      (define waiter
        (thread (lambda ()
                  (define-values (outcome grant) (approval-await-grant id digest-a))
                  (set-box! result (list outcome grant)))))
      (sleep 0.02)
      (clear-pending-approvals!)
      (check-not-false (sync/timeout 0.25 waiter))
      (check-equal? (unbox result) '(cancelled #f))
      (check-equal? (pending-approval-count) 0)
      (clear-approval-channel!))))

(exit (run-tests suite))

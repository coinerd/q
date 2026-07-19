#lang racket/base

;; @speed fast
;; @suite security

;; End-to-end broker lifecycle tests using digest-bound, one-use grants.

(require rackunit
         rackunit/text-ui
         "../runtime/approval/broker.rkt")

(define digest-a (make-string 64 #\a))
(define digest-b (make-string 64 #\b))
(define view-a (hasheq 'task-preview "integration task" 'capabilities '(shell-exec)))

(define (consume-grant grant digest)
  (call-with-approval-grant grant digest (lambda () #t)))

(define (with-channel thunk #:timeout-ms [timeout-ms 500])
  (define channel (make-approval-channel #:timeout-ms timeout-ms))
  (dynamic-wind (lambda () (set-approval-channel! channel))
                (lambda () (thunk channel))
                clear-approval-channel!))

(define suite
  (test-suite "HITL Approval Integration"

    (test-case "full approval flow yields and consumes one exact grant"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define result (box 'waiting))
                      (define owner
                        (thread (lambda ()
                                  (define-values (outcome grant) (approval-await-grant id digest-a))
                                  (set-box! result
                                            (list outcome
                                                  (and grant (consume-grant grant digest-a))
                                                  (and grant (consume-grant grant digest-a)))))))
                      (sleep 0.02)
                      (check-true (approval-decide! id digest-a #t))
                      (thread-wait owner)
                      (check-equal? (unbox result) '(approved #t #f)))))

    (test-case "full denial flow returns no authority"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (check-true (approval-decide! id digest-a #f))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'denied)
                      (check-false grant))))

    (test-case "digest mismatch cannot decide, await, or consume authority"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (check-false (approval-decide! id digest-b #t))
                      (define-values (wrong-outcome wrong-grant) (approval-await-grant id digest-b 1))
                      (check-equal? wrong-outcome 'cancelled)
                      (check-false wrong-grant)
                      (check-true (approval-decide! id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-false (consume-grant grant digest-b))
                      (check-true (consume-grant grant digest-a)))))

    (test-case "timeout fails closed and rejects late decisions"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define-values (outcome grant) (approval-await-grant id digest-a))
                      (check-equal? outcome 'timed-out)
                      (check-false grant)
                      (check-false (approval-decide! id digest-a #t)))
                    #:timeout-ms 10))

    (test-case "teardown wakes blocked execution without a grant"
      (define channel (make-approval-channel #:timeout-ms 60000))
      (set-approval-channel! channel)
      (define id (register-approval-request-for-channel! channel digest-a view-a))
      (define result (box 'waiting))
      (define owner
        (thread (lambda ()
                  (define-values (outcome grant) (approval-await-grant id digest-a))
                  (set-box! result (list outcome grant)))))
      (sleep 0.02)
      (clear-approval-channel!)
      (check-not-false (sync/timeout 0.25 owner))
      (check-equal? (unbox result) '(cancelled #f))
      (check-false (current-approval-channel)))

    (test-case "re-init starts a fresh generation with no stale approval"
      (define first (make-approval-channel))
      (set-approval-channel! first)
      (define stale-id (register-approval-request-for-channel! first digest-a view-a))
      (clear-approval-channel!)
      (define second (make-approval-channel))
      (set-approval-channel! second)
      (check-false (approval-decide! stale-id digest-a #t))
      (define fresh-id (register-approval-request-for-channel! second digest-a view-a))
      (check-not-equal? stale-id fresh-id)
      (check-true (approval-decide! fresh-id digest-a #t))
      (define-values (outcome grant) (approval-await-grant fresh-id digest-a 20))
      (check-equal? outcome 'approved)
      (check-true (consume-grant grant digest-a))
      (clear-approval-channel!))))

(exit (run-tests suite))

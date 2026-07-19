#lang racket/base

;; @speed fast
;; @suite security

;; Safe, digest-bound approval broker channel tests.

(require rackunit
         rackunit/text-ui
         "../runtime/approval/broker.rkt")

(define digest-a (make-string 64 #\a))
(define digest-b (make-string 64 #\b))
(define view-a (hasheq 'task-preview "dangerous task" 'capabilities '(shell-exec)))

(define (consume-grant grant digest)
  (call-with-approval-grant grant digest (lambda () #t)))

(define (with-channel thunk #:timeout-ms [timeout-ms 200])
  (define channel (make-approval-channel #:timeout-ms timeout-ms))
  (dynamic-wind (lambda () (set-approval-channel! channel))
                (lambda () (thunk channel))
                clear-approval-channel!))

(define suite
  (test-suite "Digest-bound Approval Channel"

    (test-case "make, set, and clear approval channel"
      (clear-approval-channel!)
      (define channel (make-approval-channel))
      (check-true (approval-channel? channel))
      (set-approval-channel! channel)
      (check-eq? (current-approval-channel) channel)
      (clear-approval-channel!)
      (check-false (current-approval-channel)))

    (test-case "registration requires the active channel, a digest, and a view"
      (clear-approval-channel!)
      (define inactive (make-approval-channel))
      (check-false (register-approval-request-for-channel! inactive digest-a view-a))
      (with-channel
       (lambda (channel)
         (check-false (register-approval-request-for-channel! channel "short" view-a))
         (check-exn exn:fail:contract?
                    (lambda () (register-approval-request-for-channel! channel digest-a 'not-a-view)))
         (define id (register-approval-request-for-channel! channel digest-a view-a))
         (check-true (string? id))
         (check-true (approval-request-pending? id digest-a))
         (define registered-view (approval-request-view id digest-a))
         (check-equal? (hash-ref registered-view 'task-preview) "dangerous task")
         (check-equal? (hash-ref registered-view 'capabilities) '(shell-exec)))))

    (test-case "decision and await are bound to the exact digest"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (check-false (approval-decide! id digest-b #t))
                      (check-false (approval-request-view id digest-b))
                      (check-true (approval-request-pending? id digest-a))
                      (check-true (approval-decide! id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-true (approval-grant? grant))
                      (check-false (consume-grant grant digest-b))
                      (check-true (consume-grant grant digest-a))
                      (check-false (consume-grant grant digest-a)))))

    (test-case "denial has no grant and a decision cannot be replayed"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (check-true (approval-decide! id digest-a #f))
                      (check-false (approval-decide! id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'denied)
                      (check-false grant))))

    (test-case "await times out closed and removes the request"
      (with-channel (lambda (channel)
                      (define id (register-approval-request-for-channel! channel digest-a view-a))
                      (define-values (outcome grant) (approval-await-grant id digest-a 10))
                      (check-equal? outcome 'timed-out)
                      (check-false grant)
                      (check-equal? (pending-approval-count) 0))
                    #:timeout-ms 10))))

(exit (run-tests suite))

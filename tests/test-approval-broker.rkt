#lang racket/base

;; @speed fast
;; @suite security

(require rackunit
         rackunit/text-ui
         "../runtime/approval/broker.rkt")

(define digest-a (make-string 64 #\a))
(define digest-b (make-string 64 #\b))
(define view-a (hasheq 'task-preview "safe preview" 'capabilities '(shell-exec)))

(define (consume-grant grant digest)
  (call-with-approval-grant grant digest (lambda () #t)))

(define (with-channel thunk)
  (define ch (make-approval-channel #:timeout-ms 200))
  (dynamic-wind (lambda () (set-approval-channel! ch))
                (lambda () (thunk ch))
                clear-approval-channel!))

(define suite
  (test-suite "digest-bound approval broker"
    (test-case "registration owns immutable view and commitment"
      (with-channel (lambda (ch)
                      (define id (register-approval-request-for-channel! ch digest-a view-a))
                      (check-true (string? id))
                      (check-equal? (hash-ref (approval-request-view id digest-a) 'task-preview)
                                    "safe preview")
                      (check-false (approval-request-view id digest-b))
                      (check-true (approval-request-pending? id digest-a))
                      (check-false (approval-request-pending? id digest-b)))))
    (test-case "wrong digest decision cannot grant"
      (with-channel (lambda (ch)
                      (define id (register-approval-request-for-channel! ch digest-a view-a))
                      (check-false (approval-decide! id digest-b #t))
                      (check-true (approval-request-pending? id digest-a))
                      (check-true (cancel-approval-request! id)))))
    (test-case "matching decision returns opaque one-use exact-digest grant"
      (with-channel (lambda (ch)
                      (define id (register-approval-request-for-channel! ch digest-a view-a))
                      (check-true (approval-decide! id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'approved)
                      (check-true (approval-grant? grant))
                      (check-false (consume-grant grant digest-b))
                      (check-true (consume-grant grant digest-a))
                      (check-false (consume-grant grant digest-a)))))
    (test-case "denial produces no grant and replay is rejected"
      (with-channel (lambda (ch)
                      (define id (register-approval-request-for-channel! ch digest-a view-a))
                      (check-true (approval-decide! id digest-a #f))
                      (check-false (approval-decide! id digest-a #t))
                      (define-values (outcome grant) (approval-await-grant id digest-a 20))
                      (check-equal? outcome 'denied)
                      (check-false grant))))
    (test-case "clear racing decision has one atomic winner and never stale approval"
      (for ([_ (in-range 100)])
        (with-channel (lambda (ch)
                        (define id (register-approval-request-for-channel! ch digest-a view-a))
                        (define gate (make-semaphore 0))
                        (define decider
                          (thread (lambda ()
                                    (sync (semaphore-peek-evt gate))
                                    (approval-decide! id digest-a #t))))
                        (define clearer
                          (thread (lambda ()
                                    (sync (semaphore-peek-evt gate))
                                    (clear-pending-approvals!))))
                        (semaphore-post gate)
                        (thread-wait decider)
                        (thread-wait clearer)
                        (define-values (outcome grant) (approval-await-grant id digest-a 1))
                        (check-equal? outcome 'cancelled)
                        (check-false grant)
                        (check-false (approval-request-pending? id digest-a))))))
    (test-case "registration rejects malformed digest and mutable presentation"
      (with-channel (lambda (ch)
                      (check-false (register-approval-request-for-channel! ch "short" view-a))
                      (check-false (register-approval-request-for-channel!
                                    ch
                                    digest-a
                                    (hasheq 'task-preview "x" 'mutable (box "forged"))))
                      (define mutable-view (make-hasheq '((task-preview . "x"))))
                      (define id (register-approval-request-for-channel! ch digest-a mutable-view))
                      (hash-set! mutable-view 'task-preview "forged")
                      (check-equal? (hash-ref (approval-request-view id digest-a) 'task-preview)
                                    "x"))))
    (test-case "frontend teardown revokes an approved but unconsumed grant"
      (define channel (make-approval-channel #:timeout-ms 200))
      (set-approval-channel! channel)
      (define id (register-approval-request-for-channel! channel digest-a view-a))
      (check-true (approval-decide! id digest-a #t))
      (define-values (outcome grant) (approval-await-grant id digest-a 20))
      (check-equal? outcome 'approved)
      (clear-approval-channel!)
      (check-false (consume-grant grant digest-a)))))

(exit (run-tests suite))

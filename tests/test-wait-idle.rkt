#lang racket

;; tests/test-wait-idle.rkt — FEAT-78: waitForIdle in SDK

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../agent/wait-idle.rkt")

(define wait-idle-tests
  (test-suite "waitForIdle"

    (test-case "wait-for-idle! returns idle on iteration.completed"
      (define bus (make-event-bus))
      (define sid "test-session-1")
      ;; Publish completion event in a thread
      (thread (lambda ()
                (sleep 0.05)
                (publish! bus
                          (make-event "iteration.completed"
                                      (inexact->exact (truncate (/ (current-inexact-milliseconds)
                                                                   1000)))
                                      sid
                                      #f
                                      (hasheq)))))
      (define result (wait-for-idle! bus sid))
      (check-equal? result 'idle))

    (test-case "wait-for-idle! returns timeout when no event"
      (define bus (make-event-bus))
      (define sid "test-session-2")
      (define result (wait-for-idle! bus sid 100))
      (check-equal? result 'timeout))

    (test-case "wait-for-idle! ignores events from other sessions"
      (define bus (make-event-bus))
      (define sid "correct-session")
      (thread
       (lambda ()
         (sleep 0.05)
         ;; Publish for wrong session
         (publish! bus
                   (make-event "iteration.completed"
                               (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                               "wrong-session"
                               #f
                               (hasheq)))
         (sleep 0.05)
         ;; Then publish for correct session
         (publish! bus
                   (make-event "iteration.completed"
                               (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000)))
                               sid
                               #f
                               (hasheq)))))
      (define result (wait-for-idle! bus sid 2000))
      (check-equal? result 'idle))

    (test-case "wait-for-idle! responds to iteration.ended too"
      (define bus (make-event-bus))
      (define sid "test-session-3")
      (thread (lambda ()
                (sleep 0.05)
                (publish! bus
                          (make-event "iteration.ended"
                                      (inexact->exact (truncate (/ (current-inexact-milliseconds)
                                                                   1000)))
                                      sid
                                      #f
                                      (hasheq)))))
      (define result (wait-for-idle! bus sid))
      (check-equal? result 'idle))))

(run-tests wait-idle-tests)

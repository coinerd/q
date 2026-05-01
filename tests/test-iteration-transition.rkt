#lang racket

;; tests/test-iteration-transition.rkt — tests for iteration/transition-logic

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/transition-logic.rkt")

(define transition-tests
  (test-suite "Iteration Transition Logic"

    (test-case "check-cancellation: no token, no shutdown"
      (define-values (stop? reason) (check-cancellation #f #f #f))
      (check-false stop?)
      (check-false reason))

    (test-case "check-cancellation: force shutdown"
      (define-values (stop? reason)
        (check-cancellation #f #f (lambda () #t)))
      (check-true stop?)
      (check-equal? reason 'force-shutdown))

    (test-case "check-cancellation: graceful shutdown"
      (define-values (stop? reason)
        (check-cancellation #f (lambda () #t) #f))
      (check-true stop?)
      (check-equal? reason 'graceful-shutdown))

    (test-case "check-soft-limit: below limit"
      (check-false (check-soft-limit 5 10 16)))

    (test-case "check-soft-limit: at soft limit"
      (check-true (check-soft-limit 9 10 16)))

    (test-case "check-hard-limit: below limit"
      (check-false (check-hard-limit 5 16)))

    (test-case "check-hard-limit: at limit"
      (check-true (check-hard-limit 15 16)))

    (test-case "termination-decision: completed stops"
      (check-equal? (termination-decision 0 10 16 'completed) 'stop))

    (test-case "termination-decision: tool-calls at hard limit"
      (check-equal? (termination-decision 15 10 16 'tool-calls-pending) 'hard-stop))

    (test-case "termination-decision: tool-calls at soft limit"
      (check-equal? (termination-decision 9 10 16 'tool-calls-pending) 'warn))

    (test-case "termination-decision: tool-calls below limits"
      (check-equal? (termination-decision 5 10 16 'tool-calls-pending) 'continue))))

(module+ main
  (run-tests transition-tests))
(module+ test
  (run-tests transition-tests))

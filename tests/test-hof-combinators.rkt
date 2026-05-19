#lang racket

;; BOUNDARY: integration

;; tests/test-hof-combinators.rkt — tests for HOF combinators

(require rackunit
         "../extensions/combinators.rkt")

(test-case "with-timeout combinator exists"
  (check-not-exn (lambda () with-timeout)))

(test-case "with-error-policy combinator exists"
  (check-not-exn (lambda () with-error-policy)))

(test-case "with-hook-validation combinator exists"
  (check-not-exn (lambda () with-hook-validation)))

(test-case "with-timeout basic usage"
  (check-equal? (with-timeout 1000 (lambda () 'done)) 'done))

(test-case "with-error-policy basic usage"
  (check-equal? (with-error-policy #t (lambda () 'ok) (lambda () 'errored)) 'ok))

;; End of tests

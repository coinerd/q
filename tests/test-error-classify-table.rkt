#lang racket

;; tests/test-error-classify-table.rkt — R-23: Error classification data table

(require rackunit
         rackunit/text-ui
         "../runtime/auto-retry.rkt")

(define suite
  (test-suite "Error classification table tests (R-23)"

    (test-case "classify-error-from-table recognizes rate-limit"
      (check-equal? (classify-error-from-table "got 429 too many requests") 'rate-limit))

    (test-case "classify-error-from-table recognizes timeout"
      (check-equal? (classify-error-from-table "connection timed out") 'timeout))

    (test-case "classify-error-from-table recognizes auth errors"
      (check-equal? (classify-error-from-table "401 unauthorized") 'auth))

    (test-case "classify-error-from-table recognizes context overflow"
      (check-equal? (classify-error-from-table "context_length exceeded") 'context-overflow))

    (test-case "classify-error-from-table returns #f for unknown"
      (check-false (classify-error-from-table "unknown error happened")))

    (test-case "ERROR-CLASSIFICATION-TABLE has expected categories"
      (define categories (map car ERROR-CLASSIFICATION-TABLE))
      (check-not-false (member 'rate-limit categories))
      (check-not-false (member 'timeout categories))
      (check-not-false (member 'auth categories))
      (check-not-false (member 'context-overflow categories))
      (check-not-false (member 'max-iterations categories)))))

(run-tests suite 'verbose)

#lang racket/base

;; Regression coverage for v0.99.30 W2 zero-parsed false-green elimination.

(require rackunit
         rackunit/text-ui
         "../scripts/run-tests.rkt")

(define zero-parsed-elimination-tests
  (test-suite "run-tests zero-parsed elimination"

    (test-case "known helper/debug/obsolete files are explicitly excluded"
      (clear-metadata-cache!)
      (define files (collect-test-files 'all))
      (for ([path (in-list '("tests/test-context-assembly-config.rkt"
                             "tests/test-cursor-debug.rkt"
                             "tests/tui/test-error-scenarios.rkt"))])
        (check-true (hash-ref (get-file-metadata path) 'not-test? #f) path)
        (check-false (member path files) path)
        (check-equal? (classify-exclusion-reason path) 'metadata-not-test path)))

    (test-case "fixed suites produce parsed rackunit counts"
      (for ([path (in-list '("tests/test-event-ordering.rkt" "tests/test-vision-helpers.rkt"))]
            [expected (in-list '(3 6))])
        (define result (run-single-file path #:timeout 120000))
        (check-equal? (test-file-result-exit-code result) 0 path)
        (check-equal? (test-file-result-total result) expected path)))))

(run-tests zero-parsed-elimination-tests)

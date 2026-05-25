#lang racket

;; tests/test-strict-runner.rkt — Strict summary mode tests (v0.54.5 W0)

(require rackunit
         rackunit/text-ui
         "../scripts/run-tests.rkt")

(define strict-suite
  (test-suite "strict runner mode tests"

    (test-case "test-file-result struct has expected fields"
      (define r (make-test-file-result "test.rkt" 0 #"" #"" 100 5 0 5))
      (check-equal? (test-file-result-path r) "test.rkt")
      (check-equal? (test-file-result-exit-code r) 0)
      (check-equal? (test-file-result-passed r) 5)
      (check-equal? (test-file-result-failed r) 0)
      (check-equal? (test-file-result-total r) 5))

    (test-case "normalize-counts preserves good results"
      (define-values (passed failed total) (normalize-counts 0 10 0 10))
      (check-equal? passed 10)
      (check-equal? failed 0)
      (check-equal? total 10))

    (test-case "normalize-counts preserves parsed failures"
      (define-values (passed failed total) (normalize-counts 0 8 2 10))
      (check-equal? passed 8)
      (check-equal? failed 2)
      (check-equal? total 10))

    (test-case "summary-exit-code: 0 when no failures"
      (check-equal? (summary-exit-code 0 0) 0))

    (test-case "summary-exit-code: 1 for failures"
      (check-equal? (summary-exit-code 1 0) 1))

    (test-case "summary-exit-code: 2 for timeouts"
      (check-equal? (summary-exit-code 0 1) 2))

    (test-case "summary-exit-code: 3 for both"
      (check-equal? (summary-exit-code 1 1) 3))

    (test-case "extract-failure-lines: empty for passing output"
      (define output #"3 success(es) 0 failure(s) 0 error(s) 3 test(s) run\n")
      (check-equal? (extract-failure-lines output) '()))

    (test-case "parse-raco-output: standard rackunit format"
      (define-values (passed failed total)
        (parse-raco-output #"some output\n5 success(es) 0 failure(s) 0 error(s) 5 test(s) run\n"))
      (check-equal? passed 5)
      (check-equal? failed 0)
      (check-equal? total 5))))

(run-tests strict-suite)

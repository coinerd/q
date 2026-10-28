#lang racket/base

;; Regression coverage for v0.99.30 W2 zero-parsed false-green elimination.

;; @speed slow
;; @suite default
;; @boundary unit
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
        (check-equal? (test-file-result-total result) expected path)))

    (test-case "an alias module reports the canonical pins it executes"
      ;; tests/test-gsd-planning-preflight-kernel.rkt is a historical alias: it
      ;; runs the canonical plan-validation pins from its own (module+ test) body.
      ;; While it was a bare `require`, plain `racket` executed the pins silently
      ;; and the runner classified the file ZERO_PARSED (strict exit 4) even
      ;; though every assertion passed. The alias must keep reporting parsed
      ;; tests, so a regression to the silent shape fails here.
      (define result
        (run-single-file "tests/test-gsd-planning-preflight-kernel.rkt" #:timeout 120000))
      (check-equal? (test-file-result-exit-code result) 0)
      (check-equal? (test-file-result-total result) 20)
      (check-eq? (classify-test-result result) 'PASS))))

(run-tests zero-parsed-elimination-tests)

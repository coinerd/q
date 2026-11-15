#lang racket
;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/run-tests/runner.rkt
(require rackunit
         racket/file
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/reporting.rkt"
         "../scripts/run-tests/classify.rkt")

(module+ test
  (test-case "copying the skipped subtype preserves classification"
    (define r (make-skipped-result "skip" 'ci '("network")))
    (define copied (struct-copy profile-skipped-result r [exit-code #:parent test-file-result 5]))
    (check-equal? (classify-test-result copied) 'SKIPPED_BY_PROFILE))
  (test-case "real RackUnit failure and error layouts override a passing summary"
    (for ([kind '("FAILURE" "ERROR")])
      (define output
        (format (string-append "--------------------\ncase > suite\n~a\n"
                               "name:       check-equal?\nlocation:   test.rkt:17:20\n"
                               "actual:     1\nexpected:   7\n"
                               "--------------------\n"
                               "1 success(es) 0 failure(s) 0 error(s) 1 test(s) run\n")
                kind))
      (define-values (_ failed __) (parse-raco-output (string->bytes/utf-8 output)))
      (check-true (positive? failed)))))

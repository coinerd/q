#lang racket
;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/run-tests/reporting.rkt

(require rackunit
         "../scripts/run-tests/reporting.rkt")

(module+ test
  (test-case "standalone failure cannot be hidden by later successful summaries"
    (define-values (passed failed total)
      (parse-raco-output
       #"--------------------\ncase\nFAILURE\nname: check-equal?\n--------------------\n2 success(es) 0 failure(s) 0 error(s) 2 test(s) run\n"))
    (check-true (positive? failed))
    (check-equal? total (+ passed failed))))

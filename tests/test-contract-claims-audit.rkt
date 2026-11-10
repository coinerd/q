#lang racket
;; @speed fast
;; @suite default
;; @boundary unit
;; @timeout 120
(require rackunit
         "helpers/contract-claims-audit-fixtures.rkt")
(provide evidence-suite)
(define evidence-suite
  (test-suite "contract-claims audit content contract"
    (test-case "audit exists"
      (check-true (audit-exists?)))
    (test-case "audit names DONE-only"
      (check-true (audit-contains? "DONE-only")))
    (test-case "audit names idempotent"
      (check-true (audit-contains? "idempotent")))
    (test-case "audit names default-OFF"
      (check-true (audit-contains? "default-OFF")))
    (test-case "audit carries verdicts"
      (check-true (or (audit-contains? "correct-as-shipped")
                      (audit-contains? "corrected-in-this-PR"))))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))

#lang racket
;; @speed fast
;; @suite default
;; @boundary unit
;; @timeout 120
(require rackunit
         "helpers/planning-reconciliation-fixtures.rkt")
(provide evidence-suite)
(define evidence-suite
  (test-suite "planning reconciliation report content contract"
    (test-case "report exists"
      (check-true (report-exists?)))
    (test-case "report names membership-hardening"
      (check-true (report-contains? "membership-hardening")))
    (test-case "report names W1-worktree-isolation-autonomy"
      (check-true (report-contains? "W1-worktree-isolation-autonomy")))
    (test-case "report names W2-canary-seven-link-delivery"
      (check-true (report-contains? "W2-canary-seven-link-delivery")))
    (test-case "report names execution-refactor"
      (check-true (report-contains? "execution-refactor")))
    (test-case "report names 9867"
      (check-true (report-contains? "9867")))
    (test-case "report names 9871"
      (check-true (report-contains? "9871")))
    (test-case "report names 9873"
      (check-true (report-contains? "9873")))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))

#lang racket

;; tests/test-gsd-plan-diff.rkt — tests for plan comparison and wave summary

(require rackunit
         rackunit/text-ui
         "../extensions/gsd-planning/plan-diff.rkt")

(define plan-diff-tests
  (test-suite "GSD Plan Diff"

    (test-case "plan-wave-summary: empty plan"
      (define plan (gsd-plan '() "" '() '()))
      (check-equal? (plan-wave-summary plan) "Waves: 0/0 completed"))

    (test-case "plan-wave-summary: with waves"
      (define waves
        (list (gsd-wave 0 "Setup" 'pending "" '() '() "" '())
              (gsd-wave 1 "Implement" 'completed "" '() '() "" '())
              (gsd-wave 2 "Test" 'completed "" '() '() "" '())))
      (define plan (gsd-plan waves "" '() '()))
      (check-equal? (plan-wave-summary plan) "Waves: 2/3 completed"))))

(require (only-in "../extensions/gsd/plan-types.rkt"
                  gsd-plan gsd-wave))

(module+ main
  (run-tests plan-diff-tests))
(module+ test
  (run-tests plan-diff-tests))

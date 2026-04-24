#lang racket

(require rackunit
         (except-in "../scripts/benchmark/report.rkt" compare-reports)
         "../scripts/benchmark/scorer.rkt"
         "../scripts/benchmark/compare.rkt")


;; Helper: build a benchmark-report from a list of (cons task-name score)
(define (make-report-with-tasks task-scores)
  (generate-report
   (for/list ([ts (in-list task-scores)])
     (score-result (car ts) (cdr ts) 'PASS (cdr ts) (cdr ts) (cdr ts) (cdr ts) (cdr ts) #f))))

(test-case "REGRESSION-THRESHOLD is 10"
  (check-equal? REGRESSION-THRESHOLD 10))

(test-case "IMPROVEMENT-THRESHOLD is 10"
  (check-equal? IMPROVEMENT-THRESHOLD 10))

(test-case "compare-reports: no changes"
  (define old-r (make-report-with-tasks '(("task-a" . 80.0) ("task-b" . 90.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 80.0) ("task-b" . 90.0))))
  (define comp (compare-reports old-r new-r))
  (check-equal? (comparison-result-regressions comp) '())
  (check-equal? (comparison-result-improvements comp) '()))

(test-case "compare-reports: detects regression"
  (define old-r (make-report-with-tasks '(("task-a" . 85.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 70.0))))
  (define comp (compare-reports old-r new-r))
  (check-equal? (length (comparison-result-regressions comp)) 1)
  (check-equal? (car (car (comparison-result-regressions comp))) "task-a"))

(test-case "compare-reports: detects improvement"
  (define old-r (make-report-with-tasks '(("task-a" . 70.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 85.0))))
  (define comp (compare-reports old-r new-r))
  (check-equal? (length (comparison-result-improvements comp)) 1)
  (check-equal? (car (car (comparison-result-improvements comp))) "task-a"))

(test-case "compare-reports: small delta ignored"
  (define old-r (make-report-with-tasks '(("task-a" . 80.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 75.0))))
  (define comp (compare-reports old-r new-r))
  (check-equal? (comparison-result-regressions comp) '())
  (check-equal? (comparison-result-improvements comp) '()))

(test-case "format-comparison-human: regression output"
  (define old-r (make-report-with-tasks '(("task-a" . 85.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 60.0))))
  (define comp (compare-reports old-r new-r))
  (define output (format-comparison-human comp))
  (check-true (string-contains? output "REGRESSION") "output should contain REGRESSION"))

(test-case "format-comparison-human: improvement output"
  (define old-r (make-report-with-tasks '(("task-a" . 60.0))))
  (define new-r (make-report-with-tasks '(("task-a" . 85.0))))
  (define comp (compare-reports old-r new-r))
  (define output (format-comparison-human comp))
  (check-true (string-contains? output "IMPROVEMENT") "output should contain IMPROVEMENT"))

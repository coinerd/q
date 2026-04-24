#lang racket

;; tests/test-benchmark-report.rkt — Tests for benchmark report generator

(require rackunit
         racket/format
         json
         (only-in "../scripts/benchmark/scorer.rkt"
                  score-result
                  score-result?
                  score-result-task-name
                  score-result-total-score
                  score-result-verdict
                  score-result-correctness
                  score-result-tool-discipline
                  score-result-efficiency
                  score-result-skill-compliance
                  score-result-no-regressions
                  score-result-details)
         (only-in "../scripts/benchmark/report.rkt"
                  benchmark-report?
                  benchmark-report-version
                  benchmark-report-results
                  benchmark-report-summary
                  comparison-result?
                  comparison-result-regressions
                  comparison-result-improvements
                  generate-report
                  report->jsexpr
                  format-report-human
                  format-report-markdown
                  compare-reports
                  comparison->jsexpr))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-score
         #:name [name "test-task"]
         #:total [total 85]
         #:verdict [verdict 'PASS]
         #:correctness [corr 90]
         #:tool-discipline [td 80]
         #:efficiency [eff 85]
         #:skill-compliance [sc 90]
         #:no-regressions [nr 95]
         #:details [details #f])
  (score-result name total verdict corr td eff sc nr details))

;; ============================================================
;; generate-report
;; ============================================================

(test-case "generate-report creates benchmark-report"
  (define scores (list (make-test-score #:name "task-a" #:total 90 #:verdict 'PASS)
                       (make-test-score #:name "task-b" #:total 45 #:verdict 'PARTIAL)))
  (define report (generate-report scores))
  (check-true (benchmark-report? report))
  (check-equal? (length (benchmark-report-results report)) 2)
  (check-true (string? (benchmark-report-version report))))

(test-case "generate-report summary has correct counts"
  (define scores (list (make-test-score #:total 90 #:verdict 'PASS)
                       (make-test-score #:total 45 #:verdict 'PARTIAL)
                       (make-test-score #:total 30 #:verdict 'FAIL)))
  (define report (generate-report scores))
  (define summary (benchmark-report-summary report))
  (check-equal? (hash-ref summary 'total-tasks) 3)
  (check-equal? (hash-ref summary 'pass-count) 1)
  (check-equal? (hash-ref summary 'partial-count) 1)
  (check-equal? (hash-ref summary 'fail-count) 1))

(test-case "generate-report avg-score"
  (define scores (list (make-test-score #:total 80)
                       (make-test-score #:total 60)))
  (define report (generate-report scores))
  (define summary (benchmark-report-summary report))
  ;; avg-score is stored as formatted string
  (check-true (equal? (hash-ref summary 'avg-score) "70")))

(test-case "generate-report with empty list"
  (define report (generate-report '()))
  (check-true (benchmark-report? report))
  (check-equal? (length (benchmark-report-results report)) 0)
  (define summary (benchmark-report-summary report))
  (check-equal? (hash-ref summary 'total-tasks) 0))

;; ============================================================
;; report->jsexpr
;; ============================================================

(test-case "report->jsexpr produces valid JSON"
  (define scores (list (make-test-score)))
  (define report (generate-report scores))
  (define js (report->jsexpr report))
  (check-true (hash? js))
  (check-true (hash-has-key? js 'version))
  (check-true (hash-has-key? js 'results))
  ;; Should be serializable
  (define json-str (jsexpr->string js))
  (check-true (string? json-str)))

;; ============================================================
;; format-report-human
;; ============================================================

(test-case "format-report-human produces readable output"
  (define scores (list (make-test-score #:name "task-1" #:total 90 #:verdict 'PASS)
                       (make-test-score #:name "task-2" #:total 30 #:verdict 'FAIL)))
  (define report (generate-report scores))
  (define output (format-report-human report))
  (check-true (string? output))
  (check-true (string-contains? output "task-1"))
  (check-true (string-contains? output "task-2")))

;; ============================================================
;; format-report-markdown
;; ============================================================

(test-case "format-report-markdown produces markdown"
  (define scores (list (make-test-score #:name "task-1")))
  (define report (generate-report scores))
  (define output (format-report-markdown report))
  (check-true (string? output))
  (check-true (string-contains? output "task-1")))

;; ============================================================
;; compare-reports
;; ============================================================

(test-case "compare-reports detects regressions"
  (define old-scores (list (make-test-score #:name "task-1" #:total 90)))
  (define new-scores (list (make-test-score #:name "task-1" #:total 70)))
  (define old-report (generate-report old-scores))
  (define new-report (generate-report new-scores))
  (define cmp (compare-reports old-report new-report))
  (check-true (comparison-result? cmp))
  ;; 20-point drop → regression
  (check-true (>= (length (comparison-result-regressions cmp)) 1)))

(test-case "compare-reports detects improvements"
  (define old-scores (list (make-test-score #:name "task-1" #:total 50)))
  (define new-scores (list (make-test-score #:name "task-1" #:total 80)))
  (define old-report (generate-report old-scores))
  (define new-report (generate-report new-scores))
  (define cmp (compare-reports old-report new-report))
  ;; 30-point gain → improvement
  (check-true (>= (length (comparison-result-improvements cmp)) 1)))

(test-case "compare-reports: stable scores are neutral"
  (define old-scores (list (make-test-score #:name "task-1" #:total 75)))
  (define new-scores (list (make-test-score #:name "task-1" #:total 78)))
  (define old-report (generate-report old-scores))
  (define new-report (generate-report new-scores))
  (define cmp (compare-reports old-report new-report))
  ;; 3-point change is within ±10 threshold
  (check-equal? (length (comparison-result-regressions cmp)) 0)
  (check-equal? (length (comparison-result-improvements cmp)) 0))

;; ============================================================
;; comparison->jsexpr
;; ============================================================

(test-case "comparison->jsexpr is serializable"
  (define old-scores (list (make-test-score #:name "t" #:total 50)))
  (define new-scores (list (make-test-score #:name "t" #:total 80)))
  (define cmp (compare-reports (generate-report old-scores)
                               (generate-report new-scores)))
  (define js (comparison->jsexpr cmp))
  (check-true (hash? js))
  (define json-str (jsexpr->string js))
  (check-true (string? json-str)))

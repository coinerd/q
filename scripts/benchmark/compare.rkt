#lang racket

;; scripts/benchmark/compare.rkt — Compare two benchmark reports
;;
;; Compares a new benchmark report against a baseline, highlighting
;; regressions (score drop > REGRESSION-THRESHOLD) and improvements
;; (score gain > IMPROVEMENT-THRESHOLD).

(require racket/format
         racket/string
         (only-in "report.rkt"
                  benchmark-report
                  benchmark-report-results
                  benchmark-report-version
                  benchmark-report-timestamp
                  comparison-result
                  comparison-result-old-report
                  comparison-result-new-report
                  comparison-result-regressions
                  comparison-result-improvements)
         (only-in "scorer.rkt" score-result score-result-task-name score-result-total-score)
         (only-in "baseline.rkt" load-baseline))

(provide compare-reports
         compare-to-baseline
         format-comparison-human
         REGRESSION-THRESHOLD
         IMPROVEMENT-THRESHOLD)

;; ============================================================
;; Constants
;; ============================================================

(define REGRESSION-THRESHOLD 10)
(define IMPROVEMENT-THRESHOLD 10)

;; ============================================================
;; Comparison
;; ============================================================

;; compare-reports : benchmark-report? benchmark-report? -> comparison-result?
;; Compare task-by-task by matching score-result-task-name.
;; Regressions: score drop > REGRESSION-THRESHOLD.
;; Improvements: score gain > IMPROVEMENT-THRESHOLD.
(define (compare-reports old-report new-report)
  (define old-map
    (for/hasheq ([r (in-list (benchmark-report-results old-report))])
      (values (score-result-task-name r) r)))
  (define new-map
    (for/hasheq ([r (in-list (benchmark-report-results new-report))])
      (values (score-result-task-name r) r)))

  (define all-old-names (hash-keys old-map))
  (define all-new-names (hash-keys new-map))

  ;; Tasks present in both — check for regressions and improvements
  (define common-names (filter (lambda (n) (hash-has-key? old-map n)) all-new-names))

  (define regressions
    (for/list ([name (in-list common-names)]
               #:when (> (- (score-result-total-score (hash-ref old-map name))
                            (score-result-total-score (hash-ref new-map name)))
                         REGRESSION-THRESHOLD))
      (list name
            (score-result-total-score (hash-ref old-map name))
            (score-result-total-score (hash-ref new-map name)))))

  (define improvements
    (for/list ([name (in-list common-names)]
               #:when (> (- (score-result-total-score (hash-ref new-map name))
                            (score-result-total-score (hash-ref old-map name)))
                         IMPROVEMENT-THRESHOLD))
      (list name
            (score-result-total-score (hash-ref old-map name))
            (score-result-total-score (hash-ref new-map name)))))

  ;; New and removed tasks
  (define new-tasks (filter (lambda (n) (not (hash-has-key? old-map n))) all-new-names))
  (define removed-tasks (filter (lambda (n) (not (hash-has-key? new-map n))) all-old-names))

  (comparison-result old-report new-report regressions improvements new-tasks removed-tasks))

;; ============================================================
;; Compare to baseline
;; ============================================================

;; compare-to-baseline : benchmark-report? path? -> comparison-result?
;; Load baseline from path and compare against the given report.
(define (compare-to-baseline report baseline-path)
  (define baseline (load-baseline baseline-path))
  (compare-reports baseline report))

;; ============================================================
;; Human-readable formatting
;; ============================================================

;; format-comparison-human : comparison-result? -> string?
;; Format comparison as human-readable text.
(define (format-comparison-human comp-result)
  (define old-ver (benchmark-report-version (comparison-result-old-report comp-result)))
  (define new-ver (benchmark-report-version (comparison-result-new-report comp-result)))
  (define regressions (comparison-result-regressions comp-result))
  (define improvements (comparison-result-improvements comp-result))

  (define n-unchanged
    (- (length (benchmark-report-results (comparison-result-old-report comp-result)))
       (length regressions)
       (length improvements)))

  (define lines
    (append (list (format "Comparison: v~a → v~a" old-ver new-ver) "")
            (for/list ([r (in-list regressions)])
              (define delta (- (caddr r) (cadr r))) ;; new - old (negative for regressions)
              (format "REGRESSION: ~a ~a → ~a (Δ~a)" (car r) (cadr r) (caddr r) delta))
            (for/list ([r (in-list improvements)])
              (define delta (- (caddr r) (cadr r))) ;; new - old (positive for improvements)
              (format "IMPROVEMENT: ~a ~a → ~a (+~a)" (car r) (cadr r) (caddr r) delta))
            (list ""
                  (format "~a regressions, ~a improvements, ~a unchanged"
                          (length regressions)
                          (length improvements)
                          (max 0 n-unchanged)))))

  (string-join lines "\n"))

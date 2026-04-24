#lang racket

;; scripts/benchmark/baseline.rkt — Baseline capture and loading for benchmark reports
;;
;; Persists benchmark-report structs as JSON files for later comparison.
;; Baselines are stored as <version>.json in a configurable directory.

(require racket/file
         racket/path
         json
         "report.rkt")

(provide capture-baseline
         load-baseline
         list-baselines
         baseline-path)

;; ============================================================
;; Path helpers
;; ============================================================

;; baseline-path : string? path? -> path?
;; Returns the file path for a baseline with the given version string.
(define (baseline-path version-string dir)
  (build-path dir (string-append version-string ".json")))

;; ============================================================
;; Capture
;; ============================================================

;; capture-baseline : benchmark-report? string? path? -> path?
;; Serialize report to JSON and save to (baseline-path version-string dir).
;; Creates dir if it doesn't exist. Returns the saved path.
(define (capture-baseline report version-string dir)
  (make-directory* dir)
  (define dest (baseline-path version-string dir))
  (define summary (benchmark-report-summary report))
  (define json-data
    (hasheq 'version
            (benchmark-report-version report)
            'timestamp
            (benchmark-report-timestamp report)
            'results
            (for/list ([r (in-list (benchmark-report-results report))])
              (hasheq 'task_name
                      (score-result-task-name r)
                      'total_score
                      (score-result-total-score r)
                      'verdict
                      (symbol->string (score-result-verdict r))
                      'correctness
                      (score-result-correctness r)
                      'tool_discipline
                      (score-result-tool-discipline r)
                      'efficiency
                      (score-result-efficiency r)
                      'skill_compliance
                      (score-result-skill-compliance r)
                      'no_regressions
                      (score-result-no-regressions r)))
            'summary
            (hasheq 'avg-score
                    (hash-ref summary 'avg-score "0")
                    'pass-count
                    (hash-ref summary 'pass-count 0)
                    'partial-count
                    (hash-ref summary 'partial-count 0)
                    'fail-count
                    (hash-ref summary 'fail-count 0)
                    'total-tasks
                    (hash-ref summary 'total-tasks 0))))
  (call-with-output-file dest (lambda (out) (write-json json-data out)) #:exists 'replace)
  dest)

;; ============================================================
;; Loading
;; ============================================================

;; load-baseline : path? -> benchmark-report?
;; Read JSON from path and reconstruct a benchmark-report struct.
(define (load-baseline path)
  (define data (call-with-input-file path read-json))
  (define results
    (for/list ([r (in-list (hash-ref data 'results '()))])
      (score-result (hash-ref r 'task_name)
                    (hash-ref r 'total_score 0)
                    (string->symbol (hash-ref r 'verdict "FAIL"))
                    (hash-ref r 'correctness 0)
                    (hash-ref r 'tool_discipline 0)
                    (hash-ref r 'efficiency 0)
                    (hash-ref r 'skill_compliance 0)
                    (hash-ref r 'no_regressions 0)
                    (hash))))
  (benchmark-report (hash-ref data 'version "")
                    (hash-ref data 'timestamp "")
                    results
                    (hash-ref data 'summary (hasheq))))

;; ============================================================
;; Listing
;; ============================================================

;; list-baselines : path? -> (listof path?)
;; List all *.json files in dir, sorted alphabetically.
;; Returns empty list if dir doesn't exist.
(define (list-baselines dir)
  (if (directory-exists? dir)
      (sort (filter (lambda (p) (equal? (path-get-extension p) #".json")) (directory-list dir))
            (lambda (a b) (string<? (path->string a) (path->string b))))
      '()))

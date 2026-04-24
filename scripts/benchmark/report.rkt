#lang racket/base

;; scripts/benchmark/report.rkt — Benchmark report generation, formatting, and comparison
;;
;; Defines score-result and benchmark-report structs, generates summary
;; statistics, and formats reports as human-readable tables or markdown.
;; Supports comparing two reports to highlight regressions and improvements.

(require racket/format
         racket/list
         racket/string
         json
         (only-in "../../util/version.rkt" q-version)
         "scorer.rkt")

(provide (all-from-out "scorer.rkt")
         (struct-out benchmark-report)
         (struct-out comparison-result)
         generate-report
         report->jsexpr
         format-report-human
         format-report-markdown
         compare-reports
         comparison->jsexpr)

;; ============================================================
;; Structs
;; ============================================================

(struct benchmark-report
        (version ; string?
         timestamp ; string? — ISO-ish from current-seconds
         results ; (listof score-result?)
         summary) ; hash? — avg-score, pass-count, partial-count, fail-count, total-tasks
  #:transparent)

(struct comparison-result
        (old-report ; benchmark-report?
         new-report ; benchmark-report?
         regressions ; (listof (list/c string? real? real?)) — task-name, old-score, new-score
         improvements ; (listof (list/c string? real? real?)) — task-name, old-score, new-score
         new-tasks ; (listof string?) — task names in new but not old
         removed-tasks) ; (listof string?) — task names in old but not new
  #:transparent)

;; ============================================================
;; Report generation
;; ============================================================

;; generate-report : (listof score-result?) -> benchmark-report?
(define (generate-report results)
  (define pass-count (length (filter (lambda (r) (eq? (score-result-verdict r) 'PASS)) results)))
  (define partial-count
    (length (filter (lambda (r) (eq? (score-result-verdict r) 'PARTIAL)) results)))
  (define fail-count (length (filter (lambda (r) (eq? (score-result-verdict r) 'FAIL)) results)))
  (define total-tasks (length results))
  (define avg-score
    (if (zero? total-tasks)
        0.0
        (/ (apply + (map score-result-total-score results)) total-tasks)))
  (define summary
    (hasheq 'avg-score
            (~r avg-score #:precision 1)
            'pass-count
            pass-count
            'partial-count
            partial-count
            'fail-count
            fail-count
            'total-tasks
            total-tasks))
  (benchmark-report q-version (timestamp-string) results summary))

;; timestamp-string : -> string?
(define (timestamp-string)
  (define secs (current-seconds))
  (define date (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year date)
          (~a (date-month date) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-day date) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-hour date) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-minute date) #:width 2 #:align 'right #:pad-string "0")
          (~a (date-second date) #:width 2 #:align 'right #:pad-string "0")))

;; ============================================================
;; JSON serialization
;; ============================================================

;; score-result->jsexpr : score-result? -> jsexpr?
(define (score-result->jsexpr sr)
  (hasheq 'task-name
          (score-result-task-name sr)
          'total-score
          (score-result-total-score sr)
          'verdict
          (symbol->string (score-result-verdict sr))
          'correctness
          (score-result-correctness sr)
          'tool-discipline
          (score-result-tool-discipline sr)
          'efficiency
          (score-result-efficiency sr)
          'skill-compliance
          (score-result-skill-compliance sr)
          'no-regressions
          (score-result-no-regressions sr)
          'details
          (score-result-details sr)))

;; report->jsexpr : benchmark-report? -> jsexpr?
(define (report->jsexpr report)
  (hasheq 'version
          (benchmark-report-version report)
          'timestamp
          (benchmark-report-timestamp report)
          'results
          (map score-result->jsexpr (benchmark-report-results report))
          'summary
          (benchmark-report-summary report)))

;; ============================================================
;; Human-readable formatting
;; ============================================================

;; format-report-human : benchmark-report? -> string?
(define (format-report-human report)
  (define results (benchmark-report-results report))
  (define summary (benchmark-report-summary report))
  (define lines
    (append (list (format "Benchmark Report — ~a (q v~a)"
                          (benchmark-report-timestamp report)
                          (benchmark-report-version report))
                  "")
            (format-result-table results)
            (list ""
                  (format "Summary: ~a tasks | Avg: ~a | Pass: ~a | Partial: ~a | Fail: ~a"
                          (hash-ref summary 'total-tasks)
                          (hash-ref summary 'avg-score)
                          (hash-ref summary 'pass-count)
                          (hash-ref summary 'partial-count)
                          (hash-ref summary 'fail-count)))))
  (string-join lines "\n"))

;; format-result-table : (listof score-result?) -> (listof string?)
(define (format-result-table results)
  (if (null? results)
      (list "  (no results)")
      (append
       (list
        "  Task                                    Score  Verdict  Corr  Tool  Effi  Skill NoReg"
        "  --------------------------------------- -----  -------  ----  ----  ----  ----- -----")
       (for/list ([r (in-list results)])
         (format "  ~a  ~a   ~a  ~a  ~a  ~a  ~a  ~a"
                 (~a (score-result-task-name r) #:width 39 #:align 'left)
                 (~r (score-result-total-score r) #:precision 0 #:min-width 5)
                 (~a (symbol->string (score-result-verdict r)) #:width 7 #:align 'left)
                 (~r (score-result-correctness r) #:precision 0 #:min-width 4)
                 (~r (score-result-tool-discipline r) #:precision 0 #:min-width 4)
                 (~r (score-result-efficiency r) #:precision 0 #:min-width 4)
                 (~r (score-result-skill-compliance r) #:precision 0 #:min-width 5)
                 (~r (score-result-no-regressions r) #:precision 0 #:min-width 5))))))

;; ============================================================
;; Markdown formatting
;; ============================================================

;; format-report-markdown : benchmark-report? -> string?
(define (format-report-markdown report)
  (define results (benchmark-report-results report))
  (define summary (benchmark-report-summary report))
  (define lines
    (append
     (list
      (format "## Benchmark Report — ~a" (benchmark-report-timestamp report))
      (format "**Version:** q v~a" (benchmark-report-version report))
      ""
      "### Results"
      ""
      "| Task | Score | Verdict | Correctness | Tool Discipline | Efficiency | Skill Compliance | No Regressions |"
      "|------|-------|---------|-------------|-----------------|------------|------------------|----------------|")
     (for/list ([r (in-list results)])
       (format "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |"
               (score-result-task-name r)
               (~r (score-result-total-score r) #:precision 1)
               (symbol->string (score-result-verdict r))
               (~r (score-result-correctness r) #:precision 1)
               (~r (score-result-tool-discipline r) #:precision 1)
               (~r (score-result-efficiency r) #:precision 1)
               (~r (score-result-skill-compliance r) #:precision 1)
               (~r (score-result-no-regressions r) #:precision 1)))
     (list ""
           "### Summary"
           ""
           (format "- **Total tasks:** ~a" (hash-ref summary 'total-tasks))
           (format "- **Average score:** ~a" (hash-ref summary 'avg-score))
           (format "- **Pass:** ~a | **Partial:** ~a | **Fail:** ~a"
                   (hash-ref summary 'pass-count)
                   (hash-ref summary 'partial-count)
                   (hash-ref summary 'fail-count)))))
  (string-join lines "\n"))

;; ============================================================
;; Report comparison
;; ============================================================

;; compare-reports : benchmark-report? benchmark-report? -> comparison-result?
;; Highlights regressions (score drop > 10) and improvements (score gain > 10).
(define (compare-reports old-report new-report)
  (define old-map
    (for/hasheq ([r (in-list (benchmark-report-results old-report))])
      (values (string->symbol (score-result-task-name r)) r)))
  (define new-map
    (for/hasheq ([r (in-list (benchmark-report-results new-report))])
      (values (string->symbol (score-result-task-name r)) r)))

  (define old-keys (hash-keys old-map))
  (define new-keys (hash-keys new-map))

  ;; Tasks present in both
  (define regressions
    (for*/list ([k (in-list new-keys)]
                #:when (hash-has-key? old-map k)
                [old-r (in-value (hash-ref old-map k))]
                [new-r (in-value (hash-ref new-map k))]
                #:when (> (- (score-result-total-score old-r) (score-result-total-score new-r)) 10))
      (list (score-result-task-name new-r)
            (score-result-total-score old-r)
            (score-result-total-score new-r))))

  (define improvements
    (for*/list ([k (in-list new-keys)]
                #:when (hash-has-key? old-map k)
                [old-r (in-value (hash-ref old-map k))]
                [new-r (in-value (hash-ref new-map k))]
                #:when (> (- (score-result-total-score new-r) (score-result-total-score old-r)) 10))
      (list (score-result-task-name new-r)
            (score-result-total-score old-r)
            (score-result-total-score new-r))))

  ;; New / removed tasks
  (define new-tasks
    (for/list ([k (in-list new-keys)]
               #:unless (hash-has-key? old-map k))
      (symbol->string k)))
  (define removed-tasks
    (for/list ([k (in-list old-keys)]
               #:unless (hash-has-key? new-map k))
      (symbol->string k)))

  (comparison-result old-report new-report regressions improvements new-tasks removed-tasks))

;; comparison->jsexpr : comparison-result? -> jsexpr?
(define (comparison->jsexpr comp)
  (hasheq 'old-timestamp
          (benchmark-report-timestamp (comparison-result-old-report comp))
          'new-timestamp
          (benchmark-report-timestamp (comparison-result-new-report comp))
          'regressions
          (for/list ([r (in-list (comparison-result-regressions comp))])
            (hasheq 'task-name (car r) 'old-score (cadr r) 'new-score (caddr r)))
          'improvements
          (for/list ([r (in-list (comparison-result-improvements comp))])
            (hasheq 'task-name (car r) 'old-score (cadr r) 'new-score (caddr r)))
          'new-tasks
          (comparison-result-new-tasks comp)
          'removed-tasks
          (comparison-result-removed-tasks comp)))

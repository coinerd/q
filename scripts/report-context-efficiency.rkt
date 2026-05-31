#lang racket/base

;; scripts/report-context-efficiency.rkt — Context efficiency report
;; v0.76.2 W1: Simple text report for token savings and conclusion coverage.

(require racket/format
         racket/string
         (only-in "../runtime/context-assembly/token-metrics.rkt"
                  context-metrics?
                  context-metrics-task-state
                  context-metrics-before-tokens
                  context-metrics-after-tokens
                  context-metrics-savings-tokens
                  context-metrics-savings-pct
                  context-metrics-category-breakdown
                  context-metrics-timestamp
                  compute-conclusion-coverage))

;; ── Helpers ──

(define (pct-str v)
  (~r v #:precision 1))

(define (pretty-breakdown breakdown)
  (format "    tier-a: ~a tokens\n    tier-b: ~a tokens\n    tier-c: ~a tokens\n    total:  ~a tokens"
          (hash-ref breakdown 'tier-a 0)
          (hash-ref breakdown 'tier-b 0)
          (hash-ref breakdown 'tier-c 0)
          (hash-ref breakdown 'total 0)))

;; ── Main report ──

(define (print-report metrics-list conclusion-coverage)
  (printf "═══════════════════════════════════════════════════════════\n")
  (printf "   Context Efficiency Report\n")
  (printf "═══════════════════════════════════════════════════════════\n\n")

  (printf "Conclusion coverage (last N turns): ~a%\n\n" (pct-str (* 100.0 conclusion-coverage)))

  (printf "Assembly metrics:\n")
  (for ([m (in-list metrics-list)]
        [i (in-naturals 1)])
    (printf "\n  Assembly ~a (~a):\n" i (context-metrics-task-state m))
    (printf "    Before:  ~a tokens\n" (context-metrics-before-tokens m))
    (printf "    After:   ~a tokens\n" (context-metrics-after-tokens m))
    (printf "    Savings: ~a tokens (~a%)\n"
            (context-metrics-savings-tokens m)
            (pct-str (context-metrics-savings-pct m)))
    (printf "    Breakdown:\n~a\n" (pretty-breakdown (context-metrics-category-breakdown m)))
    (printf "    Time:    ~a\n" (context-metrics-timestamp m)))

  (printf "\n═══════════════════════════════════════════════════════════\n")
  (when (< conclusion-coverage 0.30)
    (printf "WARNING: Conclusion coverage is low (< 30%%).\n")
    (printf "Consider using record_conclusion more often.\n"))
  (printf "Report complete.\n"))

;; ── CLI ──

(module+ main
  (define args (current-command-line-arguments))
  (cond
    [(>= (vector-length args) 1)
     (printf "Session ID: ~a\n" (vector-ref args 0))
     (printf "(Report from stored metrics not yet implemented.)\n")]
    [else
     (printf "Usage: racket scripts/report-context-efficiency.rkt <session-id>\n")
     (printf "\nExample:\n")
     (printf "  racket scripts/report-context-efficiency.rkt my-session\n")]))

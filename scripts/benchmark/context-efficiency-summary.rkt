#lang racket/base

;; scripts/benchmark/context-efficiency-summary.rkt — Aggregate context efficiency summary
;; v0.76.6 M7 W0: Runs A/B benchmark and produces aggregate statistics.
;;
;; Usage: racket scripts/benchmark/context-efficiency-summary.rkt

(require racket/format
         racket/list
         racket/match
         "context-efficiency.rkt")

;; ── Aggregate statistics ──

(define (summarize-results results)
  (define savings-list (map ab-result-savings-pct results))
  (define n (length savings-list))
  (define mean (/ (apply + savings-list) n))
  (define sorted (sort savings-list <))
  (define median
    (if (even? n)
        (/ (+ (list-ref sorted (sub1 (/ n 2))) (list-ref sorted (/ n 2))) 2.0)
        (list-ref sorted (quotient n 2))))
  (define min-s (first sorted))
  (define max-s (last sorted))
  ;; 95% confidence interval (simple: ±1.96 * stddev / sqrt(n))
  (define variance (/ (for/sum ([s savings-list]) (expt (- s mean) 2)) n))
  (define stddev (sqrt variance))
  (define ci (* 1.96 (/ stddev (sqrt n))))
  (hasheq 'mean mean
          'median median
          'min min-s
          'max max-s
          'ci ci
          'n n
          'stddev stddev
          'pass-2of3 (>= (count (λ (r) (> (ab-result-savings-pct r) 0)) results)
                         (ceiling (* 2/3 n)))))

;; ── Main ──

(module+ main
  (printf "══════════════════════════════════════════════════════════════\n")
  (printf "   Context Efficiency Aggregate Summary v0.76.6\n")
  (printf "══════════════════════════════════════════════════════════════\n\n")

  ;; Run all A/B scenarios
  (define scenarios
    (list (list 'exploration "Short exploration" 5 '())
          (list 'exploration
                "Exploration w/ conclusions"
                15
                (for/list ([i (in-range 5)])
                  (make-conclusion (format "Found fact ~a about the module" i))))
          (list 'planning
                "Planning"
                10
                (for/list ([i (in-range 3)])
                  (make-conclusion (format "Plan step ~a identified" i))))
          (list 'implementation
                "Implementation"
                20
                (for/list ([i (in-range 8)])
                  (make-conclusion (format "Implementation detail ~a" i))))
          (list 'verification
                "Verification"
                15
                (for/list ([i (in-range 4)])
                  (make-conclusion (format "Verified: check ~a passed" i))))
          (list 'debugging
                "Debugging"
                25
                (for/list ([i (in-range 6)])
                  (make-conclusion (format "Bug clue ~a: stack trace line" i))))))

  (define results
    (for/list ([sc (in-list scenarios)])
      (match-define (list state label msg-count conclusions) sc)
      (define msgs (generate-context-messages msg-count))
      (run-ab-comparison msgs state conclusions label)))

  (define summary (summarize-results results))

  (printf "Scenarios: ~a\n" (hash-ref summary 'n))
  (printf "Mean savings:   ~a%\n" (~r (hash-ref summary 'mean) #:precision 1))
  (printf "Median savings: ~a%\n" (~r (hash-ref summary 'median) #:precision 1))
  (printf "Min savings:    ~a%\n" (~r (hash-ref summary 'min) #:precision 1))
  (printf "Max savings:    ~a%\n" (~r (hash-ref summary 'max) #:precision 1))
  (printf "Stddev:         ~a%\n" (~r (hash-ref summary 'stddev) #:precision 1))
  (printf "95% CI:         ±~a%\n" (~r (hash-ref summary 'ci) #:precision 1))
  (printf "\n2/3 tasks with savings: ~a\n"
          (if (hash-ref summary 'pass-2of3) "PASS ✓" "FAIL ✗"))

  ;; Per-scenario breakdown
  (printf "\nPer-scenario:\n")
  (for ([r (in-list results)])
    (printf "  ~a: ~a% savings\n"
            (~a (ab-result-label r) #:min-width 28)
            (~r (ab-result-savings-pct r) #:precision 1)))

  (printf "\n══════════════════════════════════════════════════════════════\n")
  (printf "Summary complete.\n"))

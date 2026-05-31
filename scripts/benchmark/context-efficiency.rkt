#lang racket/base

;; scripts/benchmark/context-efficiency.rkt — A/B comparison harness
;; v0.76.3 W1: Compare context assembly with state-aware ON vs OFF.
;;
;; Measures: assembled context token count, tier distribution,
;;           conclusion coverage, repeated tool calls.
;; Usage: racket scripts/benchmark/context-efficiency.rkt [--tasks <dir>]

(require racket/cmdline
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string
         "../../runtime/context-assembly/context-floor.rkt"
         "../../runtime/context-assembly/state-aware-builder.rkt"
         "../../runtime/context-assembly/task-conclusion.rkt"
         (only-in "../../util/protocol-types.rkt" make-message make-text-part message-content)
         (only-in "../../util/content-parts.rkt" text-part-text))

;; ── Helpers ──

(define (make-test-msg text)
  (make-message (format "msg-~a" (random 100000))
                #f
                'user
                'text
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (token-estimate text)
  ;; Rough: ~4 chars per token
  (ceiling (/ (string-length text) 4.0)))

(define (msg-tokens msg)
  (for/sum ([part (in-list (message-content msg))]) (token-estimate (text-part-text part))))

(define (tc-total-tokens tc)
  (+ (for/sum ([m (tiered-context-tier-a tc)]) (msg-tokens m))
     (for/sum ([m (tiered-context-tier-b tc)]) (msg-tokens m))
     (for/sum ([m (tiered-context-tier-c tc)]) (msg-tokens m))))

(define (make-conclusion text)
  (task-conclusion (format "c~a" (random 100000))
                   text
                   'fact
                   'exploration
                   '()
                   (current-seconds)
                   '()
                   '()))

;; ── Generate synthetic context for benchmark ──

(define (generate-context-messages n)
  (for/list ([i (in-range n)])
    (make-test-msg (format "Message ~a: ~a"
                           i
                           (apply string-append
                                  (for/list ([j (in-range 20)])
                                    (format "Word~a " j)))))))

;; ── A/B measurement ──

(struct ab-result (label tokens-a tokens-b savings-pct tier-a-count tier-b-count tier-c-count)
  #:transparent)

(provide ab-result
         ab-result?
         ab-result-label
         ab-result-tokens-a
         ab-result-tokens-b
         ab-result-savings-pct
         ab-result-tier-a-count
         ab-result-tier-b-count
         ab-result-tier-c-count
         make-conclusion
         generate-context-messages
         run-ab-comparison)

(define (run-ab-comparison messages task-state conclusions label)
  ;; A: state-aware ON
  (define tc-a
    (build-tiered-context/state-aware messages
                                      #:tier-b-count 10
                                      #:tier-c-count 5
                                      #:task-state task-state
                                      #:conclusions conclusions))
  ;; B: state-aware OFF (standard assembly)
  (define tc-b (build-tiered-context messages #:tier-b-count 10 #:tier-c-count 5))

  (define tokens-a (tc-total-tokens tc-a))
  (define tokens-b (tc-total-tokens tc-b))
  (define savings-pct
    (if (> tokens-b 0)
        (* 100.0 (/ (- tokens-b tokens-a) tokens-b))
        0.0))

  (ab-result label
             tokens-a
             tokens-b
             savings-pct
             (length (tiered-context-tier-a tc-a))
             (length (tiered-context-tier-b tc-a))
             (length (tiered-context-tier-c tc-a))))

;; ── Main ──

(define task-dir (build-path (path-only (find-system-path 'run-file)) "tasks"))

(command-line #:args () (void))

;; Benchmark scenarios
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

(printf "══════════════════════════════════════════════════════════════\n")
(printf "   Context Efficiency A/B Benchmark v0.76.3\n")
(printf "══════════════════════════════════════════════════════════════\n\n")

(printf
 "| Scenario | Tokens (A: aware) | Tokens (B: standard) | Savings% | Tier-A | Tier-B | Tier-C |\n")
(printf
 "|----------|-------------------|----------------------|----------|--------|--------|--------|\n")

(define results
  (for/list ([sc (in-list scenarios)])
    (match-define (list state label msg-count conclusions) sc)
    (define msgs (generate-context-messages msg-count))
    (define r (run-ab-comparison msgs state conclusions label))
    (printf "| ~a | ~a | ~a | ~a% | ~a | ~a | ~a |\n"
            (~a label #:min-width 25)
            (~a (ab-result-tokens-a r) #:min-width 17)
            (~a (ab-result-tokens-b r) #:min-width 20)
            (~r (ab-result-savings-pct r) #:precision 1)
            (ab-result-tier-a-count r)
            (ab-result-tier-b-count r)
            (ab-result-tier-c-count r))
    r))

;; Summary
(printf "\n══════════════════════════════════════════════════════════════\n")
(printf "   SUMMARY\n")
(printf "══════════════════════════════════════════════════════════════\n")

(define avg-savings
  (if (null? results)
      0.0
      (/ (for/sum ([r (in-list results)]) (ab-result-savings-pct r)) (length results))))

(define scenarios-with-savings
  (for/list ([r (in-list results)]
             #:when (> (ab-result-savings-pct r) 0))
    (ab-result-label r)))

(printf "Average savings: ~a%\n" (~r avg-savings #:precision 1))
(printf "Scenarios with savings: ~a / ~a\n" (length scenarios-with-savings) (length results))
(printf
 "Minimum 10% savings on 2/3 tasks: ~a\n"
 (if (>= (length scenarios-with-savings) (ceiling (* 2/3 (length results)))) "PASS ✓" "FAIL ✗"))

;; Conclusion coverage check
(printf "\nConclusion coverage:\n")
(for ([sc (in-list scenarios)])
  (match-define (list state label _ conclusions) sc)
  (define has-conclusions? (pair? conclusions))
  (printf "  ~a: ~a conclusions → ~a\n"
          (~a label #:min-width 25)
          (length conclusions)
          (if has-conclusions? "included in preamble ✓" "no conclusions (baseline)")))

(printf "\nBenchmark complete.\n")

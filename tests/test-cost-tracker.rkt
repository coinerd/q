#lang racket

;; tests/test-cost-tracker.rkt — tests for util/cost-tracker.rkt

(require rackunit
         rackunit/text-ui
         "../util/cost-tracker.rkt")

;; ============================================================
;; calculate-cost with known models
;; ============================================================

(test-case "calculate-cost: gpt-4o pricing"
  ;; $2.50/1M input, $10/1M output
  (define cost (calculate-cost 1000000 1000000 "gpt-4o"))
  (check-= cost 12.50 0.001))

(test-case "calculate-cost: gpt-4o-mini pricing"
  ;; $0.15/1M input, $0.60/1M output
  (define cost (calculate-cost 1000000 1000000 "gpt-4o-mini"))
  (check-= cost 0.75 0.001))

(test-case "calculate-cost: claude-3.5-sonnet pricing"
  ;; $3/1M input, $15/1M output
  (define cost (calculate-cost 1000000 1000000 "claude-3.5-sonnet"))
  (check-= cost 18.0 0.001))

(test-case "calculate-cost: small token counts"
  ;; gpt-4o: 1000 input, 500 output
  ;; input:  1000 * 2.50 / 1M = 0.0025
  ;; output:  500 * 10.0 / 1M = 0.005
  ;; total = 0.0075
  (define cost (calculate-cost 1000 500 "gpt-4o"))
  (check-= cost 0.0075 0.0001))

;; ============================================================
;; calculate-cost with unknown model (default pricing)
;; ============================================================

(test-case "calculate-cost: unknown model uses default pricing"
  ;; Default: $3/1M input, $15/1M output
  (define cost (calculate-cost 1000000 1000000 "some-unknown-model"))
  (check-= cost 18.0 0.001))

(test-case "calculate-cost: #f model uses default pricing"
  (define cost (calculate-cost 1000000 1000000 #f))
  (check-= cost 18.0 0.001))

;; ============================================================
;; format-cost
;; ============================================================

(test-case "format-cost: zero"
  (check-equal? (format-cost 0.0) "$0.00"))

(test-case "format-cost: small amount"
  (check-equal? (format-cost 1.23) "$1.23"))

(test-case "format-cost: large amount"
  (check-equal? (format-cost 100.0) "$100.00"))

(test-case "format-cost: fractional cents round"
  (check-equal? (format-cost 0.0075) "$0.01"))

(test-case "format-cost: exact integer"
  (check-equal? (format-cost 5) "$5.00"))

;; ============================================================
;; cost-tracker accumulates tokens across updates
;; ============================================================

(test-case "cost-tracker: accumulates tokens"
  (define ct (make-cost-tracker "gpt-4o"))
  (cost-tracker-update! ct 1000 500)
  (cost-tracker-update! ct 2000 1000)
  (cost-tracker-update! ct 500 250)
  ;; Total: 3500 input, 1750 output
  (check-equal? (cost-tracker-input-tokens-total ct) 3500)
  (check-equal? (cost-tracker-output-tokens-total ct) 1750)
  ;; Expected cost: 3500*2.50/1M + 1750*10.0/1M = 0.00875 + 0.0175 = 0.02625
  (check-= (cost-tracker-total ct) 0.02625 0.0001))

(test-case "cost-tracker: zero initially"
  (define ct (make-cost-tracker))
  (check-equal? (cost-tracker-input-tokens-total ct) 0)
  (check-equal? (cost-tracker-output-tokens-total ct) 0)
  (check-= (cost-tracker-total ct) 0.0 0.0001))

(test-case "cost-tracker: reset clears tokens"
  (define ct (make-cost-tracker "gpt-4o"))
  (cost-tracker-update! ct 1000 500)
  (cost-tracker-reset! ct)
  (check-equal? (cost-tracker-input-tokens-total ct) 0)
  (check-equal? (cost-tracker-output-tokens-total ct) 0))

;; ============================================================
;; Thread safety: concurrent updates don't lose data
;; ============================================================

(test-case "cost-tracker: concurrent updates are safe"
  (define ct (make-cost-tracker "gpt-4o"))
  (define N 1000)
  ;; Spawn threads that each add 1 input + 1 output token
  (define threads
    (for/list ([_ (in-range N)])
      (thread (lambda () (cost-tracker-update! ct 1 1)))))
  ;; Wait for all threads
  (for-each thread-wait threads)
  ;; Must have exactly N*1 input and N*1 output
  (check-equal? (cost-tracker-input-tokens-total ct) N)
  (check-equal? (cost-tracker-output-tokens-total ct) N))

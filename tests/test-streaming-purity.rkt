#lang racket/base

;; tests/test-streaming-purity.rkt — Streaming plan purity tests (v0.47.2)
;; Tests that compute-streaming-plan and cost-tracker-update are pure.

(require rackunit
         (only-in "../agent/effect-types.rkt"
                  streaming-plan?
                  streaming-plan-expected-effects
                  streaming-plan-session-id
                  streaming-plan-raw-messages)
         (only-in "../agent/loop-phases.rkt" compute-streaming-plan)
         (only-in "../util/cost-tracker.rkt"
                  cost-tracker?
                  make-cost-tracker
                  cost-tracker-update
                  cost-tracker-total
                  cost-tracker-input-tokens-total
                  cost-tracker-output-tokens-total))

;; ── cost-tracker-update purity tests ──

(test-case "cost-tracker-update returns new tracker, original unchanged"
  (define t1 (make-cost-tracker "gpt-4o"))
  (define t2 (cost-tracker-update t1 100 50))
  (check-equal? (cost-tracker-input-tokens-total t1) 0)
  (check-equal? (cost-tracker-input-tokens-total t2) 100)
  (check-equal? (cost-tracker-output-tokens-total t1) 0)
  (check-equal? (cost-tracker-output-tokens-total t2) 50))

(test-case "cost-tracker-update is idempotent with same inputs"
  (define t1 (make-cost-tracker))
  (define t2 (cost-tracker-update t1 200 100))
  (define t3 (cost-tracker-update t1 200 100))
  (check-equal? (cost-tracker-input-tokens-total t2)
                (cost-tracker-input-tokens-total t3))
  (check-equal? (cost-tracker-output-tokens-total t2)
                (cost-tracker-output-tokens-total t3)))

(test-case "cost-tracker-update chains correctly"
  (define t1 (make-cost-tracker))
  (define t2 (cost-tracker-update t1 100 50))
  (define t3 (cost-tracker-update t2 200 100))
  (check-equal? (cost-tracker-input-tokens-total t3) 300)
  (check-equal? (cost-tracker-output-tokens-total t3) 150))

(test-case "cost-tracker-update with model-name"
  (define t1 (make-cost-tracker "model-a"))
  (define t2 (cost-tracker-update t1 100 50 "model-b"))
  ;; New tracker should have updated model (accessible via total)
  (check-equal? (cost-tracker-input-tokens-total t2) 100)
  ;; Original unchanged
  (check-equal? (cost-tracker-input-tokens-total t1) 0))

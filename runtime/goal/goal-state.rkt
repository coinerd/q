#lang racket/base

;; q/runtime/goal-state.rkt — Facade: re-exports from goal-types + goal-codec
;;
;; Backward-compatible facade. All identifiers previously provided
;; by this module are still available from the same import path.

(require "goal-types.rkt"
         "goal-codec.rkt")

;; Explicit re-exports from goal-types.rkt — do NOT use all-from-out (ADR-0028)
(provide goal-state
         goal-state?
         goal-check
         goal-check?
         evaluation-result
         evaluation-result?
         check-result
         check-result?
         ;; Constructors with contracts
         make-goal-state
         make-goal-check
         make-evaluation-result
         make-check-result
         ;; Accessors
         goal-state-id
         goal-state-goal-text
         goal-state-status
         goal-state-turns-used
         goal-state-max-turns
         goal-state-evaluator-model
         goal-state-evaluator-mode
         goal-state-checks
         goal-state-evaluations
         goal-state-last-evaluation
         goal-state-started-at
         goal-state-updated-at
         goal-state-meta
         goal-check-command
         goal-check-expected-exit
         goal-check-label
         check-result-label
         check-result-exit-code
         check-result-stdout
         check-result-stderr
         check-result-timed-out?
         check-result-elapsed-ms
         evaluation-result-achieved?
         evaluation-result-reason
         evaluation-result-check-results
         evaluation-result-model-used
         evaluation-result-token-cost
         ;; Predicates
         goal-status?
         evaluator-mode?
         ;; Constants
         DEFAULT-GOAL-MAX-TURNS
         DEFAULT-EVALUATOR-MODE
         NO-PROGRESS-THRESHOLD
         ;; Helpers
         string-truncate
         ;; I4 (v0.72.7)
         goal-state-total-token-cost

         ;; Explicit re-exports from goal-codec.rkt — do NOT use all-from-out (ADR-0028)
         goal-state->hash
         hash->goal-state
         goal-check->hash
         hash->goal-check
         evaluation-result->hash
         hash->evaluation-result
         check-result->hash
         hash->check-result)

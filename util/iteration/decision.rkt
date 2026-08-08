#lang racket/base

;; util/iteration/decision.rkt — shared iteration decision protocol types
;;
;; SHARED PROTOCOL: Iteration decision types used by both Agent and Runtime.
;;
;; v0.99.86: Types extracted from runtime/iteration/decision.rkt.
;; Pure decision functions (decide-next-action, compute-step-result) moved
;; to agent/iteration/decision.rkt because they depend on Agent-layer
;; counter types.
;;
;; Provides:
;;   iteration-ctx     — struct capturing pure subset of loop state
;;   step-result       — struct describing what the loop should do next
;;   step-action?      — contract for step action symbols
;;   known-termination-reasons — list of valid termination reason symbols
;;   current-max-consecutive-tool-calls — circuit breaker parameter

(require racket/contract)

;; ============================================================
;; Structs
;; ============================================================

(struct iteration-ctx
        (iteration consecutive-tool-count explore-count max-iterations max-iterations-hard)
  #:transparent)

;; v0.99.78 FIX (tool-call circling): consecutive-tool circuit breaker.
;; A model that emits only tool-calls (no text, no terminating condition) for
;; many consecutive iterations is circling (observed: 262 consecutive tool-only
;; turns in a GSD /go execution, burning LLM calls until max-iterations).
;; Default 30 consecutive tool-only iterations before the loop stops.
(define current-max-consecutive-tool-calls (make-parameter 30))

(struct step-result
        (action ; symbol: 'continue | 'stop | 'stop-hard-limit | 'stop-soft-limit
         termination ; symbol?: termination reason (symbol for stop actions, #f for continue)
         new-counters ; loop-counters? — updated counters after this step
         metadata) ; hash? — metadata for result construction
  #:transparent)

(define step-action? (or/c 'continue 'stop 'stop-hard-limit 'stop-soft-limit))

(provide iteration-ctx
         iteration-ctx?
         iteration-ctx-iteration
         iteration-ctx-consecutive-tool-count
         iteration-ctx-explore-count
         iteration-ctx-max-iterations
         iteration-ctx-max-iterations-hard
         (contract-out (struct step-result
                               ([action (or/c 'continue 'stop 'stop-hard-limit 'stop-soft-limit)]
                                [termination (or/c symbol? #f)]
                                [new-counters any/c]
                                [metadata hash?])))
         known-termination-reasons
         step-action?
         current-max-consecutive-tool-calls)

;; ============================================================
;; Data
;; ============================================================

(define (known-termination-reasons)
  '(completed cancelled
              tool-calls-pending
              error
              force-shutdown
              shutdown
              max-iterations-exceeded
              hook-blocked
              empty-response))

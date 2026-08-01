#lang racket/base

;; runtime/iteration/decision.rkt — pure decision functions for iteration loop
;;
;; Extracted from runtime/iteration.rkt (v0.34.6 W0a — A-01 decomposition).
;;
;; Provides:
;;   iteration-ctx     — struct capturing pure subset of loop state
;;   step-result       — struct describing what the loop should do next
;;   decide-next-action — pure decision function
;;;;   (removed in v0.34.7 — inlined into compute-step-result)
;;   compute-step-result — pure step computation
;;   known-termination-reasons — list of valid termination reason symbols

(require racket/contract
         racket/match
         (only-in "../../agent/iteration/counters.rkt" compute-next-counters)
         (only-in "../../util/loop-result.rkt" loop-result-termination-reason loop-result-messages)
         (only-in "../../agent/iteration/loop-state.rkt"
                  loop-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count))

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
         decide-next-action
         compute-step-result
         known-termination-reasons
         step-action?
         current-max-consecutive-tool-calls)

;; ============================================================
;; Pure functions
;; ============================================================

(define (known-termination-reasons)
  '(completed cancelled
              tool-calls-pending
              error
              force-shutdown
              shutdown
              max-iterations-exceeded
              hook-blocked))

(define (decide-next-action ctx result)
  (define term (loop-result-termination-reason result))
  (match term
    [(or 'completed 'cancelled 'force-shutdown 'shutdown) 'stop]
    ['hook-blocked 'stop]
    ['max-iterations-exceeded 'stop]
    ['error 'stop]
    ['tool-calls-pending
     (define next-iter (add1 (iteration-ctx-iteration ctx)))
     (define consecutive (iteration-ctx-consecutive-tool-count ctx))
     (cond
       ;; v0.99.78 FIX: consecutive-tool circuit breaker — stop the loop before
       ;; it burns max-iterations LLM calls on a read/explore circle.
       [(>= consecutive (current-max-consecutive-tool-calls)) 'stop]
       [(>= next-iter (iteration-ctx-max-iterations-hard ctx)) 'stop-hard-limit]
       [(>= next-iter (iteration-ctx-max-iterations ctx)) 'stop-soft-limit]
       [else 'continue])]
    [_ 'stop]))

(define (compute-step-result ctx result counters)
  (define action (decide-next-action ctx result))
  ;; Inline termination — both branches returned (loop-result-termination-reason result)
  (define termination (loop-result-termination-reason result))
  (define new-msgs (loop-result-messages result))
  (define new-counters (compute-next-counters counters new-msgs))
  ;; v0.99.78 FIX: mark circuit-breaker stops distinctly so callers/TUI can
  ;; surface the reason instead of a bare stop with a tool-calls-pending reason.
  (define tool-loop-limit?
    (and (eq? action 'stop)
         (eq? termination 'tool-calls-pending)
         (>= (iteration-ctx-consecutive-tool-count ctx) (current-max-consecutive-tool-calls))))
  (define metadata
    (cond
      [tool-loop-limit? (hasheq 'toolLoopLimit #t)]
      [(eq? action 'stop-hard-limit) (hasheq 'maxIterationsReached #t)]
      [else (hasheq)]))
  (step-result action termination new-counters metadata))

#lang racket/base

;; agent/iteration/decision.rkt — pure iteration decision functions
;;
;; AGENT DECISION LOGIC: decide-next-action and compute-step-result.
;;
;; v0.99.86: Extracted from runtime/iteration/decision.rkt.
;; These functions depend on Agent-layer counter types (compute-next-counters,
;; loop-counters accessors) and therefore belong in the Agent layer.
;; The types they operate on (iteration-ctx, step-result) live in
;; util/iteration/decision.rkt as shared protocol.
;;
;; decide-next-action  — pure: given iteration-ctx + loop-result, returns action symbol
;; compute-step-result — pure: given iteration-ctx + loop-result + counters, returns step-result

(require racket/match
         (only-in "../../util/iteration/decision.rkt"
                  iteration-ctx
                  iteration-ctx-iteration
                  iteration-ctx-consecutive-tool-count
                  iteration-ctx-max-iterations
                  iteration-ctx-max-iterations-hard
                  step-result
                  current-max-consecutive-tool-calls)
         (only-in "../../util/loop-result.rkt" loop-result-termination-reason loop-result-messages)
         (only-in "counters.rkt" compute-next-counters)
         (only-in "loop-state.rkt"
                  loop-counters
                  loop-counters-iteration
                  loop-counters-consecutive-tool-count
                  loop-counters-explore-count))

(provide decide-next-action
         compute-step-result)

;; ============================================================
;; Pure functions
;; ============================================================

(define (decide-next-action ctx result)
  (define term (loop-result-termination-reason result))
  (match term
    [(or 'completed 'cancelled 'force-shutdown 'shutdown) 'stop]
    ['hook-blocked 'stop]
    ['max-iterations-exceeded 'stop]
    ['error 'stop]
    ['empty-response 'continue]
    ['tool-calls-pending
     (define next-iter (add1 (iteration-ctx-iteration ctx)))
     (define consecutive (iteration-ctx-consecutive-tool-count ctx))
     (cond
       [(>= next-iter (iteration-ctx-max-iterations-hard ctx)) 'stop-hard-limit]
       ;; v0.99.78 FIX: consecutive-tool circuit breaker — stop the loop before
       ;; it burns max-iterations LLM calls on a read/explore circle. Fires ONLY
       ;; while within the soft iteration budget: once the loop has passed the
       ;; soft limit, the soft→hard escalation governs and the loop reports
       ;; max-iterations-exceeded (preserving max-iterations=0/1 test semantics).
       [(and (<= next-iter (iteration-ctx-max-iterations ctx))
             (>= consecutive (current-max-consecutive-tool-calls)))
        'stop]
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
    (match action
      ['stop
       (if tool-loop-limit?
           (hasheq 'toolLoopLimit #t)
           (hasheq))]
      ['stop-hard-limit (hasheq 'maxIterationsReached #t)]
      [_ (hasheq)]))
  ;; v0.99.83 W2: Mark empty-response in metadata so step-interpreter can
  ;; inject a nudge message and recurse instead of silently completing.
  (define final-metadata
    (if (eq? termination 'empty-response)
        (hash-set metadata 'emptyResponse #t)
        metadata))
  (step-result action termination new-counters final-metadata))

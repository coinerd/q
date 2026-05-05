#lang racket/base

;; runtime/iteration/loop-state.rkt — DI resolvers for iteration loop
;;
;; v0.29.5 W2: Removed parameter indirection and lazy-require.
;; DI resolvers now directly reference concrete implementations.
;; Callers pass these explicitly via keyword arguments.

(require (only-in "../../runtime/compactor.rkt" compact-history)
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../../extensions/message-inject.rkt" injection-event-topic)
         racket/set)

(provide resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic
         loop-infra
         loop-infra?
         loop-counters
         loop-counters?
         make-initial-counters
         loop-infra-ctx
         loop-infra-ext-reg
         loop-infra-reg
         loop-infra-bus
         loop-infra-session-id
         loop-infra-log-path
         loop-infra-token
         loop-counters-iteration
         loop-counters-consecutive-tool-count
         loop-counters-seen-paths
         loop-counters-intent-retry-count
         loop-counters-consecutive-error-count
         loop-counters-recent-tool-names
         loop-counters-explore-count
         loop-counters-implement-count
         loop-counters-stall-retry-count)

;; Direct DI resolvers — no parameter fallback, no lazy-require.
(define (resolve-compact-proc)
  compact-history)
(define (resolve-estimate-tokens)
  estimate-context-tokens)
(define (resolve-inject-topic)
  injection-event-topic)

;; ── Loop state structs (v0.29.16 W0) ──────────────────────────
;;
;; Captures iteration-loop state that was previously threaded as
;; 26 positional parameters through dispatch-loop-action.
;; See AUDIT-v0.29.15 W4 for the 26-param code smell analysis.

;; Infrastructure that doesn't change across iterations.
(struct loop-infra (ctx ext-reg reg bus session-id log-path token) #:transparent)

;; Counters and accumulators that evolve across iterations.
(struct loop-counters
        (iteration consecutive-tool-count
                   seen-paths
                   intent-retry-count
                   consecutive-error-count
                   recent-tool-names
                   explore-count
                   implement-count
                   stall-retry-count)
  #:transparent)

;; Constructor helper: create initial counters from defaults.
(define (make-initial-counters)
  (loop-counters 0 0 (set) 0 0 '() 0 0 0))

#lang racket/base

;; runtime/iteration/loop-state.rkt — DI resolvers for iteration loop
;;
;; v0.29.5 W2: Removed parameter indirection and lazy-require.
;; DI resolvers now directly reference concrete implementations.
;; Callers pass these explicitly via keyword arguments.

(require (only-in "../../runtime/compactor.rkt" compact-history)
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../../extensions/message-inject.rkt" injection-event-topic))

(provide resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic)

;; Direct DI resolvers — no parameter fallback, no lazy-require.
(define (resolve-compact-proc)
  compact-history)
(define (resolve-estimate-tokens)
  estimate-context-tokens)
(define (resolve-inject-topic)
  injection-event-topic)

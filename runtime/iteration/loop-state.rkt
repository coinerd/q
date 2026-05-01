#lang racket/base

;; runtime/iteration/loop-state.rkt — DI parameters and loop state definitions
;;
;; Injectable parameters for iteration.rkt testability.
;; Thread-local: set by make-agent-session in the calling thread.

(require racket/lazy-require
         (only-in "../../extensions/message-inject.rkt" injection-event-topic))

(lazy-require ["../../runtime/compactor.rkt" (compact-history)]
              ["../../llm/token-budget.rkt" (estimate-context-tokens)])

(provide current-compact-proc
         current-estimate-tokens
         current-inject-topic
         resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic)

;; DI parameters — decouple iteration from concrete imports
(define current-compact-proc (make-parameter #f))
(define current-estimate-tokens (make-parameter #f))
(define current-inject-topic (make-parameter #f))

;; Resolve DI parameter to concrete impl if not explicitly set.
(define (resolve-compact-proc)
  (or (current-compact-proc) compact-history))

(define (resolve-estimate-tokens)
  (or (current-estimate-tokens) estimate-context-tokens))

(define (resolve-inject-topic)
  (or (current-inject-topic) injection-event-topic))

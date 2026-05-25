#lang racket/base

;; runtime/session-context.rkt — context building helpers
;;
;; ARCH-05 partial (v0.22.0): Extracted from agent-session.rkt.
;; Pure functions for context message analysis.

(require racket/contract
         (only-in "../util/protocol-types.rkt" message-kind message-meta-safe))

;; extract-path-settings — walk context messages to find latest
;;   model-change and thinking-level-change entries.
;;   Returns a hash with 'model and 'thinking-level keys.
(define (extract-path-settings messages)
  (for/fold ([settings (hasheq)]) ([msg (in-list messages)])
    (define kind (message-kind msg))
    (cond
      [(and (eq? kind 'model-change) (hash? (message-meta-safe msg)))
       (hash-set settings 'model (hash-ref (message-meta-safe msg) 'model #f))]
      [(and (eq? kind 'thinking-level-change) (hash? (message-meta-safe msg)))
       (hash-set settings 'thinking-level (hash-ref (message-meta-safe msg) 'level #f))]
      [else settings])))

(provide (contract-out [extract-path-settings (-> list? any/c)]))

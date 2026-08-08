#lang racket/base

;; util/token-estimate.rkt — pure token estimation for messages
;;
;; Extracted from runtime/context/context-policy.rkt so that all layers
;; can use token estimation without importing a Runtime context module.
;;
;; The estimate-text-tokens function lives in llm/token-budget.rkt and
;; provides a rough character-count/4 heuristic that is provider-independent.

(require racket/contract
         racket/string
         (only-in "../llm/token-budget.rkt" estimate-text-tokens)
         (only-in "content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-result-part?
                  tool-result-part-content)
         (only-in "content/content-helpers.rkt" tool-result-content->string)
         (only-in "message/message.rkt" message-content))

(provide (contract-out [estimate-message-tokens (-> any/c exact-nonnegative-integer?)]))

;; Estimate token count for a single message struct.
;; Extracts text from all text-parts AND tool-result-part content.
(define (estimate-message-tokens msg)
  (define text-tokens
    (estimate-text-tokens (string-join (for/list ([part (in-list (message-content msg))]
                                                  #:when (text-part? part))
                                         (text-part-text part))
                                       " ")))
  (define tool-result-tokens
    (for/sum ([part (in-list (message-content msg))] #:when (tool-result-part? part))
             (estimate-text-tokens (tool-result-content->string (tool-result-part-content part)))))
  (+ text-tokens tool-result-tokens))

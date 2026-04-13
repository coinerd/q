#lang racket/base

;; llm/token-budget.rkt — token budget estimation and thresholds
;;
;; Provides heuristic-based token estimation and budget helpers.
;; The estimation uses a chars/4 heuristic which is reasonable for
;; English text with modern tokenizers.

(require racket/contract
         racket/string)

(provide
 estimate-context-tokens
 estimate-turn-tokens
 should-compact?
 remaining-budget

 ;; Constants (for reuse in SDK and agent-session defaults)
 DEFAULT-TOKEN-BUDGET-THRESHOLD)

;; ============================================================
;; estimate-context-tokens
;; ============================================================

;; Heuristic: ~4 characters per token.
;; Extracts text from message content (either a string or list of
;; content parts with 'text keys).
(define CHARS-PER-TOKEN 4)

;; Extract text from a single message hash
(define (extract-message-text msg)
  (define content (hash-ref msg 'content ""))
  (cond
    [(string? content) content]
    [(list? content)
     ;; Content parts: extract text from parts with 'text key
     (string-append*
      (for/list ([part (in-list content)]
                 #:when (hash? part))
        (hash-ref part 'text "")))]
    [else ""]))

(define (estimate-context-tokens messages)
  (define total-chars
    (for/sum ([msg (in-list messages)])
      (string-length (extract-message-text msg))))
  (quotient total-chars CHARS-PER-TOKEN))

;; ============================================================
;; estimate-turn-tokens
;; ============================================================

;; Estimate token count from messages plus response text as fallback.
;; This is used by the agent loop when the provider returns no usage data.
;; Uses the same chars/4 heuristic as estimate-context-tokens but also
;; accounts for the response text length.
(define (estimate-turn-tokens messages response-text)
  (define total-chars
    (+ (for/sum ([msg (in-list messages)])
         (string-length (extract-message-text msg)))
       (string-length (or response-text ""))))
  (quotient total-chars CHARS-PER-TOKEN))

;; ============================================================
;; should-compact?
;; ============================================================

;; Default token budget threshold (100k tokens).
;; Used by the SDK and agent-session as the default value
;; when no explicit threshold is configured.
(define DEFAULT-TOKEN-BUDGET-THRESHOLD 100000)

;; Compaction threshold is 80% of the budget.
(define COMPACT-RATIO 0.8)

(define (should-compact? current-tokens budget-threshold)
  (define threshold (* budget-threshold COMPACT-RATIO))
  (>= current-tokens threshold))

;; ============================================================
;; remaining-budget
;; ============================================================

(define (remaining-budget current-tokens budget-threshold)
  (- budget-threshold current-tokens))

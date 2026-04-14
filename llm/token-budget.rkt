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
 estimate-text-tokens
 ;; Constants (for reuse in SDK and agent-session defaults)
 DEFAULT-TOKEN-BUDGET-THRESHOLD
 DEFAULT-SAFETY-MARGIN-PCT)

;; ============================================================
;; estimate-context-tokens
;; ============================================================

;; Heuristic: ~4 characters per token.
;; Extracts text from message content (either a string or list of
;; content parts with 'text keys).
(define CHARS-PER-TOKEN 4)

;; CJK text uses ~2 chars/token (more compact in token space).
;; Code uses ~3 chars/token (operators, identifiers).
;; ASCII text uses ~4 chars/token.
(define CJK-CHARS-PER-TOKEN 2)
(define CODE-CHARS-PER-TOKEN 3)

;; Detect if a string contains significant CJK content.
;; Returns 'cjk, 'code, or 'text.
(define (classify-text text)
  (define len (string-length text))
  (if (= len 0)
      'text
      (let loop ([i 0] [cjk-count 0] [code-count 0])
        (cond
          [(>= i len)
           (cond
             [(> cjk-count (quotient len 4)) 'cjk]
             [(> code-count (quotient len 3)) 'code]
             [else 'text])]
          [else
           (define c (string-ref text i))
           (define cp (char->integer c))
           (define cjk-char?
             (or (and (>= cp #x4E00) (<= cp #x9FFF))
                 (and (>= cp #x3400) (<= cp #x4DBF))
                 (and (>= cp #xAC00) (<= cp #xD7AF))))
           (define code-char?
             (or (char=? c #\{) (char=? c #\})
                 (char=? c #\() (char=? c #\))
                 (char=? c #\[) (char=? c #\])
                 (char=? c #\;) (char=? c #\=)))
           (loop (add1 i)
                 (+ cjk-count (if cjk-char? 1 0))
                 (+ code-count (if code-char? 1 0)))]))))

;; Estimate tokens for a single text string using content-aware heuristics.
;; Returns at least 1 for any non-empty string.
(define (estimate-text-tokens text)
  (if (= (string-length text) 0)
      0
      (max 1
          (let ([ratio (case (classify-text text)
                         [(cjk) CJK-CHARS-PER-TOKEN]
                         [(code) CODE-CHARS-PER-TOKEN]
                         [else CHARS-PER-TOKEN])])
            (quotient (string-length text) ratio)))))

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
  (for/sum ([msg (in-list messages)])
    (estimate-text-tokens (extract-message-text msg))))

;; ============================================================
;; estimate-turn-tokens
;; ============================================================

;; Estimate token count from messages plus response text as fallback.
;; This is used by the agent loop when the provider returns no usage data.
;; Uses content-aware heuristics for better accuracy.
(define (estimate-turn-tokens messages response-text)
  (define msg-tokens
    (for/sum ([msg (in-list messages)])
      (estimate-text-tokens (extract-message-text msg))))
  (define resp-tokens (estimate-text-tokens (or response-text "")))
  (+ msg-tokens resp-tokens))

;; Safety margin percentage added to estimates to prevent overflow.
;; Default: 10% safety margin.
(define DEFAULT-SAFETY-MARGIN-PCT 0.10)

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

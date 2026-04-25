#lang racket/base

;; llm/token-budget.rkt — token budget estimation and thresholds
;;
;; Provides heuristic-based token estimation and budget helpers.
;; The estimation uses a chars/4 heuristic which is reasonable for
;; English text with modern tokenizers.

(require racket/contract
         racket/string
         (only-in "../util/protocol-types.rkt" message? message-content text-part? text-part-text))

(provide estimate-context-tokens
         estimate-turn-tokens
         should-compact?
         remaining-budget
         estimate-text-tokens
         ;; Constants (for reuse in SDK and agent-session defaults)
         DEFAULT-TOKEN-BUDGET-THRESHOLD
         DEFAULT-SAFETY-MARGIN-PCT
         ;; Context usage API (#1154)
         (struct-out context-usage)
         get-context-usage
         context-usage-near-threshold?)

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
      (let loop ([i 0]
                 [cjk-count 0]
                 [code-count 0])
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
             (or (char=? c #\{)
                 (char=? c #\})
                 (char=? c #\()
                 (char=? c #\))
                 (char=? c #\[)
                 (char=? c #\])
                 (char=? c #\;)
                 (char=? c #\=)))
           (loop (add1 i) (+ cjk-count (if cjk-char? 1 0)) (+ code-count (if code-char? 1 0)))]))))

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

;; Extract text from a single message (hash or message struct)
(define (extract-message-text msg)
  (cond
    [(message? msg)
     ;; message struct: content is (listof content-part)
     (define parts (message-content msg))
     (string-append* (for/list ([part (in-list parts)]
                                #:when (text-part? part))
                       (text-part-text part)))]
    [(hash? msg)
     (define content (hash-ref msg 'content ""))
     (cond
       [(string? content) content]
       [(list? content)
        (string-append* (for/list ([part (in-list content)]
                                   #:when (hash? part))
                          (hash-ref part 'text "")))]
       [else ""])]
    [else ""]))

(define (estimate-context-tokens messages)
  (for/sum ([msg (in-list messages)]) (estimate-text-tokens (extract-message-text msg))))

;; ============================================================
;; estimate-turn-tokens
;; ============================================================

;; Estimate token count from messages plus response text as fallback.
;; This is used by the agent loop when the provider returns no usage data.
;; Uses content-aware heuristics for better accuracy.
(define (estimate-turn-tokens messages response-text)
  (define msg-tokens
    (for/sum ([msg (in-list messages)]) (estimate-text-tokens (extract-message-text msg))))
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
  ;; Apply safety margin so compaction triggers before actual overflow (#450)
  (define effective-budget (* budget-threshold (- 1 DEFAULT-SAFETY-MARGIN-PCT)))
  (define threshold (* effective-budget COMPACT-RATIO))
  (>= current-tokens threshold))

;; ============================================================
;; remaining-budget
;; ============================================================

(define (remaining-budget current-tokens budget-threshold)
  ;; Account for safety margin in reported remaining budget (#450)
  (define effective-budget (* budget-threshold (- 1 DEFAULT-SAFETY-MARGIN-PCT)))
  (- effective-budget current-tokens))

;; ============================================================
;; Context usage API (#1154)
;; ============================================================

;; Struct representing context window usage statistics.
;; total-tokens: estimated tokens currently used
;; max-tokens: the configured token budget threshold
;; usage-percent: percentage of budget used (0.0–100.0+)
;; compaction-threshold: percentage at which compaction triggers
(struct context-usage (total-tokens max-tokens usage-percent compaction-threshold) #:transparent)

;; get-context-usage : integer? integer? -> context-usage?
;;
;; Computes context usage statistics from token counts and budget.
;; total-tokens: current token count
;; max-tokens: the token budget threshold
;; Returns a context-usage struct with computed percentage and threshold.
(define (get-context-usage total-tokens max-tokens)
  (define pct
    (if (> max-tokens 0)
        (* 100.0 (/ total-tokens max-tokens))
        0.0))
  ;; Compaction triggers at effective-budget * COMPACT-RATIO, expressed as a percentage
  ;; of the nominal max-tokens. effective-budget = max-tokens * (1 - safety-margin).
  (define threshold-pct (* 100.0 (- 1 DEFAULT-SAFETY-MARGIN-PCT) COMPACT-RATIO))
  (context-usage total-tokens max-tokens pct threshold-pct))

;; context-usage-near-threshold? : context-usage? [real?] -> boolean?
;;
;; Returns #t if usage is within `margin` percentage points of the
;; compaction threshold. Useful for proactive UI warnings.
(define (context-usage-near-threshold? usage [margin 5.0])
  (> (context-usage-usage-percent usage) (- (context-usage-compaction-threshold usage) margin)))

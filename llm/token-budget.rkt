#lang racket/base
;; STABILITY: public

;; llm/token-budget.rkt — token budget estimation and thresholds
;;
;; Provides heuristic-based token estimation and budget helpers.
;; The estimation uses a chars/4 heuristic which is reasonable for
;; English text with modern tokenizers.

(require racket/contract
         racket/string
         racket/port
         (only-in "../util/content/content-parts.rkt"
                  text-part?
                  tool-call-part?
                  tool-result-part?
                  image-part?)
         (only-in "../util/message/message.rkt" message? message-content message-role)
         (only-in "../util/content/content-parts.rkt"
                  text-part-text
                  tool-call-part-name
                  tool-call-part-arguments
                  tool-result-part-content
                  tool-result-part-is-error?
                  image-part-data
                  image-part-mime-type))

(provide DEFAULT-TOKEN-BUDGET-THRESHOLD
         DEFAULT-SAFETY-MARGIN-PCT
         PER-MESSAGE-OVERHEAD-TOKENS
         TOOL-CALL-OVERHEAD-TOKENS
         TOOL-RESULT-OVERHEAD-TOKENS
         IMAGE-OVERHEAD-TOKENS
         TOOL-SCHEMA-BASE-TOKENS
         context-usage
         context-usage?
         context-usage-total-tokens
         context-usage-max-tokens
         context-usage-usage-percent
         context-usage-compaction-threshold
         (contract-out
          [estimate-context-tokens (-> (listof (or/c message? hash?)) exact-nonnegative-integer?)]
          [estimate-turn-tokens
           (-> (listof (or/c message? hash?)) string? exact-nonnegative-integer?)]
          [should-compact? (-> exact-nonnegative-integer? exact-nonnegative-integer? boolean?)]
          [remaining-budget (-> exact-nonnegative-integer? exact-nonnegative-integer? exact-integer?)]
          [estimate-text-tokens (-> string? exact-nonnegative-integer?)]
          [estimate-tool-definition-tokens (-> string? string? exact-nonnegative-integer?)]
          [estimate-content-part-tokens (-> (or/c string? hash? list?) exact-nonnegative-integer?)]
          [get-context-usage
           (-> exact-nonnegative-integer? exact-nonnegative-integer? context-usage?)]
          [context-usage-near-threshold? (->* (context-usage?) (real?) boolean?)]
          [estimate-message-overhead
           (->* () (#:role (or/c string? symbol? #f)) exact-nonnegative-integer?)]
          [estimate-non-text-tokens (-> (or/c message? hash?) exact-nonnegative-integer?)]))

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

;; ============================================================
;; Per-message and structural overhead constants
;; ============================================================

;; Per-message role/delimiter overhead in tokens.
;; Each message in the provider API adds role markers (~4 chars),
;; newlines, and formatting (~12 chars total ~= 3 tokens) plus
;; a message-boundary structural token.
(define PER-MESSAGE-OVERHEAD-TOKENS 4)

;; Tool-call overhead: the JSON wrapper around a tool invocation
;; includes {"type":"tool-call","id":...,"name":...,"arguments":{...}}
;; The structural wrapping adds ~60 chars / 4 ~= 15 tokens.
(define TOOL-CALL-OVERHEAD-TOKENS 15)

;; Tool-result overhead: {"type":"tool-result","toolCallId":...,"content":...,"isError":...}
;; adds ~50 chars / 4 ~= 12 tokens of structural wrapping.
(define TOOL-RESULT-OVERHEAD-TOKENS 12)

;; Image overhead: {"type":"image","mimeType":...,"data":"...","detail":...}
;; The data field is base64 encoded. Each image adds ~50 tokens of
;; structural overhead + the mime-type/detail fields.
(define IMAGE-OVERHEAD-TOKENS 50)

;; Tool definition/schema: each tool defined in the provider schema
;; adds its name, description, and JSON schema parameters.
;; Base overhead for a tool definition without parameters: ~15 tokens.
(define TOOL-SCHEMA-BASE-TOKENS 15)

;; Per-parameter overhead in a tool schema (name, type, description): ~4 tokens.
(define TOOL-SCHEMA-PER-PARAM-TOKENS 4)

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

;; ============================================================
;; Per-message role overhead
;; ============================================================

;; Estimate the overhead tokens for a message's role/delimiter.
;; Different roles may have slightly different overhead (user messages
;; have different delimiters than assistant/tool messages).
(define (estimate-message-overhead #:role [role #f])
  (cond
    [(not role) PER-MESSAGE-OVERHEAD-TOKENS]
    ;; Tool-role messages have additional tool-call/tool-result overhead
    ;; that is estimated separately; just the role delimiter here.
    [(eq? role 'tool) PER-MESSAGE-OVERHEAD-TOKENS]
    ;; Assistant messages may include thinking/scratchpad wrappers.
    [(eq? role 'assistant) (+ PER-MESSAGE-OVERHEAD-TOKENS 1)]
    [else PER-MESSAGE-OVERHEAD-TOKENS]))

;; ============================================================
;; Non-text content part token estimation
;; ============================================================

;; Estimate tokens for a single content part (not text).
;; Returns 0 for text parts (handled separately in extract-message-text).
;; For hashes/maps without type info, estimate from serialized string length.
(define (estimate-content-part-tokens part)
  (cond
    [(text-part? part) 0] ; handled by extract-message-text
    [(tool-call-part? part)
     (define name (tool-call-part-name part))
     (define args (tool-call-part-arguments part))
     (define name-tokens
       (if (string? name)
           (estimate-text-tokens name)
           0))
     (define args-tokens
       (cond
         [(hash? args) (estimate-json-hash-tokens args)]
         [(string? args) (estimate-text-tokens args)]
         [else 0]))
     (+ TOOL-CALL-OVERHEAD-TOKENS name-tokens args-tokens)]
    [(tool-result-part? part)
     (define content (tool-result-part-content part))
     (define content-tokens
       (cond
         [(string? content) (estimate-text-tokens content)]
         [(hash? content) (estimate-json-hash-tokens content)]
         [(list? content) (for/sum ([item (in-list content)]) (estimate-content-part-tokens item))]
         [else 0]))
     (+ TOOL-RESULT-OVERHEAD-TOKENS content-tokens)]
    [(image-part? part)
     ;; Images have a data URL/base64 payload plus mime-type/detail.
     (define data (image-part-data part))
     (define mime (image-part-mime-type part))
     (define data-tokens
       (if (string? data)
           ;; Base64 data: ~4 chars per 3 bytes, ~1 token per 4 chars
           ;; Typical image: 100k-500k base64 chars = 25k-125k tokens
           (quotient (string-length data) 4)
           0))
     (+ IMAGE-OVERHEAD-TOKENS data-tokens)]
    [(hash? part)
     ;; Hash-based content part (from JSON deserialization)
     (define type (hash-ref part "type" ""))
     (case type
       [("tool-call")
        (define name (hash-ref part "name" ""))
        (define args (hash-ref part "arguments" ""))
        (define name-tokens
          (if (string? name)
              (estimate-text-tokens name)
              0))
        (define args-tokens
          (cond
            [(hash? args) (estimate-json-hash-tokens args)]
            [(string? args) (estimate-text-tokens args)]
            [else 0]))
        (+ TOOL-CALL-OVERHEAD-TOKENS name-tokens args-tokens)]
       [("tool-result")
        (define content (hash-ref part "content" ""))
        (define content-tokens
          (cond
            [(string? content) (estimate-text-tokens content)]
            [(hash? content) (estimate-json-hash-tokens content)]
            [(list? content) (for/sum ([item (in-list content)]) (estimate-content-part-tokens item))]
            [else 0]))
        (+ TOOL-RESULT-OVERHEAD-TOKENS content-tokens)]
       [("image")
        (define data (hash-ref part "data" ""))
        (define data-tokens
          (if (string? data)
              (quotient (string-length data) 4)
              0))
        (+ IMAGE-OVERHEAD-TOKENS data-tokens)]
       [else 0])]
    [(list? part) (for/sum ([item (in-list part)]) (estimate-content-part-tokens item))]
    [else 0]))

;; Estimate tokens for a JSON-compatible hash value.
;; Converts to JSON string and estimates from length.
(define (estimate-json-hash-tokens h)
  (define json-str (with-output-to-string (lambda () (write-json h))))
  (estimate-text-tokens json-str))

;; Simple JSON writer for hash -> string estimation.
;; Not as fast as a proper JSON library, but sufficient for estimation.
(define (write-json val)
  (cond
    [(hash? val)
     (display "{")
     (define keys (hash-keys val))
     (for ([i (in-naturals)]
           [k (in-list keys)])
       (unless (zero? i)
         (display ", "))
       (write-json-string (cond
                            [(symbol? k) (symbol->string k)]
                            [(string? k) k]
                            [else (format "~a" k)]))
       (display ": ")
       (write-json (hash-ref val k)))
     (display "}")]
    [(list? val)
     (display "[")
     (for ([i (in-naturals)]
           [v (in-list val)])
       (unless (zero? i)
         (display ", "))
       (write-json v))
     (display "]")]
    [(string? val) (write-json-string val)]
    [(boolean? val) (display (if val "true" "false"))]
    [(number? val) (display val)]
    [(void? val) (display "null")]
    [else (display val)]))

(define (write-json-string s)
  (display "\"")
  (for ([c (in-string s)])
    (case c
      [(#\") (display "\\\"")]
      [(#\\) (display "\\\\")]
      [(#\newline) (display "\\n")]
      [(#\tab) (display "\\t")]
      [else (write-char c)]))
  (display "\""))

;; ============================================================
;; Estimate non-text tokens in a message
;; ============================================================

;; Count tokens from non-text content parts (tool-call, tool-result, image)
;; plus the per-message structural overhead.
(define (estimate-non-text-tokens msg)
  (define parts
    (cond
      [(message? msg) (message-content msg)]
      [(hash? msg) (hash-ref msg "content" '())]
      [else '()]))
  (define non-text-tokens
    (for/sum ([part
               (in-list (if (list? parts)
                            parts
                            '()))])
             (estimate-content-part-tokens part)))
  non-text-tokens)

;; ============================================================
;; Tool definition/schema token estimation
;; ============================================================

;; Estimate tokens for a single tool definition.
;; name: tool name string
;; schema: tool schema as a hash with 'properties and optional 'description
(define (estimate-tool-definition-tokens name schema-json)
  (define name-tokens (estimate-text-tokens name))
  (define schema-tokens (estimate-text-tokens (or schema-json "")))
  (+ TOOL-SCHEMA-BASE-TOKENS name-tokens schema-tokens))

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
  (for/sum ([msg (in-list messages)])
           (define text-tokens (estimate-text-tokens (extract-message-text msg)))
           (define non-text (estimate-non-text-tokens msg))
           (define overhead
             (estimate-message-overhead #:role (cond
                                                 [(message? msg) (message-role msg)]
                                                 [(hash? msg) (hash-ref msg 'role #f)]
                                                 [else #f])))
           (+ text-tokens non-text overhead)))

;; ============================================================
;; estimate-turn-tokens
;; ============================================================

;; Estimate token count from messages plus response text as fallback.
;; This is used by the agent loop when the provider returns no usage data.
;; Uses content-aware heuristics for better accuracy.
(define (estimate-turn-tokens messages response-text)
  (define msg-tokens (estimate-context-tokens messages))
  (define resp-tokens (estimate-text-tokens (or response-text "")))
  (+ msg-tokens resp-tokens))

;; Safety margin percentage added to estimates to prevent overflow.
;; Default: 10% safety margin.
(define DEFAULT-SAFETY-MARGIN-PCT 1/10)

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

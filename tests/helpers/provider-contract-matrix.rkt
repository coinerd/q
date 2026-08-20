#lang racket/base

;; @speed fast
;; @suite default

;; tests/helpers/provider-contract-matrix.rkt — Provider Contract Golden Matrix
;;
;; v0.99.91 W0 (#9237): the cross-provider contract oracle.
;;
;; This module is TEST-LEVEL DATA ONLY — it is not a production abstraction and
;; imports no production parser logic. It declares, per contract scenario, what
;; each provider adapter is EXPECTED to produce when its real parser is driven
;; with a representative fixture. Consumers (test-provider-contract-golden-matrix)
;; drive the real provider parsers and compare against this matrix.
;;
;; Capability discipline (Path B): where a provider genuinely lacks a
;; normalization (e.g. Anthropic/Gemini reasoning deltas), the matrix marks the
;; scenario UNSUPPORTED for that provider instead of forcing artificial
;; equality. No shared production abstraction is introduced.

(provide provider-contract-names
         provider-contract-scenarios
         provider-contract-expected
         matrix-cell-supported?
         matrix-cell-note
         matrix-cell-expected
         matrix-cell-for
         check-matrix-complete!
         typed-unsupported-capabilities
         unsupported-capability-provider
         unsupported-capability-scenario
         unsupported-capability-kind
         unsupported-capability-observable
         unsupported-capability-rationale
         typed-unsupported-capability-for
         check-typed-unsupported-complete!
         provider-specific-contracts
         provider-specific-contract-provider
         provider-specific-contract-name
         provider-specific-contract-expected
         provider-specific-contract-note
         check-provider-specific-contracts-complete!)

(require racket/hash
         racket/list)

;; ---------------------------------------------------------------------------
;; Matrix shape
;; ---------------------------------------------------------------------------

(define provider-contract-names '(anthropic gemini openai-compatible azure-openai))

(define provider-contract-scenarios
  '(text-nonstream reasoning-delta
                   usage-nonstream
                   usage-stream-done
                   tool-call-nonstream
                   tool-delta-stream
                   sse-framing
                   timeout-exn
                   malformed-sse
                   error-envelope))

;; A matrix cell: (supported? expected note)
;;   supported?  — #t if the provider normalizes this scenario
;;   expected    — the normalized observable the real parser must produce
;;   note        — capability explanation (why supported/unsupported)
(struct matrix-cell (supported? expected note) #:transparent)

;; matrix-cell-for : scenario provider -> matrix-cell
(define (matrix-cell-for scenario provider)
  (define by-provider (hash-ref provider-contract-expected scenario #f))
  (and by-provider (hash-ref by-provider provider #f)))

;; ---------------------------------------------------------------------------
;; The golden matrix — scenario -> provider -> matrix-cell
;; ---------------------------------------------------------------------------

(define provider-contract-expected
  (hash
   ;; 1. Non-streaming text response -> canonical content text block
   'text-nonstream
   (hash 'anthropic
         (matrix-cell #t '(1 "text") "content blocks: {type text text}")
         'gemini
         (matrix-cell #t '(1 "text") "candidates[].content.parts[].text")
         'openai-compatible
         (matrix-cell #t '(1 "text") "choices[].message.content string")
         'azure-openai
         (matrix-cell #t '(1 "text") "same wire format as openai-compatible"))
   ;; 2. Streaming reasoning delta -> delta-thinking
   ;;    OpenAI/DeepSeek-style reasoning_content is normalized to
   ;;    stream-chunk delta-thinking. v1.00.05 W0: Anthropic thinking_delta is
   ;;    now normalized too (kimi/Anthropic extended thinking). Gemini thought
   ;;    parts still have NO delta-thinking mapping -> unsupported.
   'reasoning-delta
   (hash
    'anthropic
    (matrix-cell #t
                 "Let me think..."
                 "thinking_delta -> delta-thinking (v1.00.05 W0: kimi/Anthropic extended thinking)")
    'gemini
    (matrix-cell #f #f "thought part has no delta-thinking mapping; parsed as plain text chunk")
    'openai-compatible
    (matrix-cell #t "reasoning..." "delta.reasoning_content -> delta-thinking")
    'azure-openai
    (matrix-cell #t "reasoning..." "reuses normalize-openai-chunk -> delta-thinking"))
   ;; 3. Non-streaming usage -> canonical keys (prompt/completion/total)
   'usage-nonstream
   (hash
    'anthropic
    (matrix-cell #t '(10 5 15) "input_tokens/output_tokens -> canonical (sum as total)")
    'gemini
    (matrix-cell #t '(7 3 10) "usageMetadata promptTokenCount/candidatesTokenCount/totalTokenCount")
    'openai-compatible
    (matrix-cell #t '(4 2 6) "usage hash already canonical")
    'azure-openai
    (matrix-cell #t '(4 2 6) "openai parse path; canonical keys"))
   ;; 4. Streaming done chunk usage — REAL ASYMMETRY, documented not equalized.
   ;;    Anthropic message_delta only carries output_tokens -> the done chunk's
   ;;    usage hash has ONLY completion_tokens. Gemini's done chunk carries the
   ;;    full canonical usage. OpenAI/Azure pass the usage hash through.
   'usage-stream-done
   (hash 'anthropic
         (matrix-cell #t
                      '(completion_tokens)
                      "message_delta usage = output_tokens only -> completion_tokens")
         'gemini
         (matrix-cell #t
                      '(completion_tokens prompt_tokens total_tokens)
                      "finishReason event usage = full canonical keys (sorted)")
         'openai-compatible
         (matrix-cell #t
                      '(completion_tokens prompt_tokens total_tokens)
                      "final chunk usage hash passed through (stream_options include_usage)")
         'azure-openai
         (matrix-cell #t
                      '(completion_tokens prompt_tokens total_tokens)
                      "openai chunk path; usage passed through"))
   ;; 5. Non-streaming tool call -> canonical tool-call content block
   'tool-call-nonstream
   (hash
    'anthropic
    (matrix-cell #t '("tool-call" "read_file") "tool_use block -> {type tool-call id name arguments}")
    'gemini
    (matrix-cell #t
                 '("tool-call" "read_file")
                 "functionCall part -> {type tool-call id name arguments}")
    'openai-compatible
    (matrix-cell #t '("tool-call" "read_file") "message.tool_calls -> tool-call content block")
    'azure-openai
    (matrix-cell #t '("tool-call" "read_file") "openai parse path"))
   ;; 6. Streaming tool-call delta -> delta-tool-call hash
   ;;    Anthropic requires content_block_start + input_json_delta pair.
   ;;    Gemini emits the full functionCall args in ONE delta (no partial
   ;;    accumulation); OpenAI/Azure accumulate arguments fragments.
   'tool-delta-stream
   (hash
    'anthropic
    (matrix-cell #t 'hash "content_block_start (index/id/name) + input_json_delta -> delta-tool-call")
    'gemini
    (matrix-cell #t 'hash "functionCall part -> single delta-tool-call with full args")
    'openai-compatible
    (matrix-cell #t 'hash "delta.tool_calls[0] -> delta-tool-call (partial args)")
    'azure-openai
    (matrix-cell #t 'hash "normalize-openai-chunk tool_calls path"))
   ;; 7. SSE framing: comments/empty lines/[DONE] filtered, data parsed
   'sse-framing
   (hash 'anthropic
         (matrix-cell #t 2 "shared parse-sse-lines: 2 data events, comments/empty/[DONE] filtered")
         'gemini
         (matrix-cell #t 2 "shared parse-sse-lines")
         'openai-compatible
         (matrix-cell #t 2 "shared parse-sse-lines")
         'azure-openai
         (matrix-cell #t 2 "shared parse-sse-lines"))
   ;; 8. Timeout -> exn:fail:network:timeout raised by call-with-request-timeout
   'timeout-exn
   (hash 'anthropic
         (matrix-cell #t #t "shared call-with-request-timeout raises exn:fail:network:timeout")
         'gemini
         (matrix-cell #t #t "shared call-with-request-timeout")
         'openai-compatible
         (matrix-cell #t #t "shared call-with-request-timeout")
         'azure-openai
         (matrix-cell #t #t "shared call-with-request-timeout"))
   ;; 9. Malformed SSE data line -> skipped (parse-sse-line returns #f)
   'malformed-sse
   (hash 'anthropic
         (matrix-cell #t #f "parse-sse-line returns #f on malformed JSON")
         'gemini
         (matrix-cell #t #f "shared parse-sse-line")
         'openai-compatible
         (matrix-cell #t #f "shared parse-sse-line")
         'azure-openai
         (matrix-cell #t #f "shared parse-sse-line"))
   ;; 10. HTTP error envelope -> provider-error with classified category
   'error-envelope
   (hash 'anthropic
         (matrix-cell #t 'bad-request "check-provider-status! 400 -> provider-error category")
         'gemini
         (matrix-cell #t 'bad-request "check-provider-status! 400 -> category bad-request")
         'openai-compatible
         (matrix-cell #t 'bad-request "check-provider-status! 400 -> category bad-request")
         'azure-openai
         (matrix-cell #t 'bad-request "bespoke check-azure-status! still raises provider-error"))))

;; ---------------------------------------------------------------------------
;; Completeness gate: every scenario x provider has an explicit cell, and
;; every matrix entry is reachable from the declared scenario/provider lists
;; (no orphan entries that would silently drop out of the contract).
;; Returns a list of problems: (missing-scenario provider) or (orphan key).
;; ---------------------------------------------------------------------------

(define (check-matrix-complete!)
  (define gaps '())
  (for ([scenario (in-list provider-contract-scenarios)])
    (for ([provider (in-list provider-contract-names)])
      (unless (matrix-cell-for scenario provider)
        (set! gaps (cons (list 'missing scenario provider) gaps)))))
  ;; Orphan detection: keys in the matrix not declared in the lists.
  (for ([scenario (in-hash-keys provider-contract-expected)])
    (unless (member scenario provider-contract-scenarios)
      (set! gaps (cons (list 'orphan-scenario scenario) gaps)))
    (define by-provider (hash-ref provider-contract-expected scenario))
    (for ([provider (in-hash-keys by-provider)])
      (unless (member provider provider-contract-names)
        (set! gaps (cons (list 'orphan-provider scenario provider) gaps)))))
  (reverse gaps))

;; ---------------------------------------------------------------------------
;; W1-B typed unsupported capabilities
;; ---------------------------------------------------------------------------

;; Machine-readable test contract for a genuinely unsupported normalization.
;; `kind` classifies the adapter behavior; `observable` pins the REAL parser
;; result so unsupported never means untested or silently skipped.
(struct unsupported-capability (provider scenario kind observable rationale) #:transparent)

(define typed-unsupported-capabilities
  (list (unsupported-capability 'gemini
                                'reasoning-delta
                                'mapped-to-text-not-thinking
                                #f
                                "thought parts emit delta-text but no delta-thinking")))

;; v1.00.05 W0 removed the anthropic reasoning-delta record: kimi/Anthropic
;; thinking_delta is now normalized to delta-thinking (supported).

(define (typed-unsupported-capability-for scenario provider)
  (for/first ([record (in-list typed-unsupported-capabilities)]
              #:when (and (eq? scenario (unsupported-capability-scenario record))
                          (eq? provider (unsupported-capability-provider record))))
    record))

;; Bijection gate: unsupported W0 cells and typed records must match exactly.
(define (check-typed-unsupported-complete!)
  (define unsupported-cells
    (for*/list ([scenario (in-list provider-contract-scenarios)]
                [provider (in-list provider-contract-names)]
                #:when (let ([cell (matrix-cell-for scenario provider)])
                         (and cell (not (matrix-cell-supported? cell)))))
      (list scenario provider)))
  (define record-cells
    (for/list ([record (in-list typed-unsupported-capabilities)])
      (list (unsupported-capability-scenario record) (unsupported-capability-provider record))))
  (append (for/list ([cell (in-list unsupported-cells)]
                     #:unless (member cell record-cells))
            (cons 'missing-typed-record cell))
          (for/list ([cell (in-list record-cells)]
                     #:unless (member cell unsupported-cells))
            (cons 'orphan-typed-record cell))
          (for/list ([cell (in-list record-cells)]
                     #:when (> (count (lambda (candidate) (equal? candidate cell)) record-cells) 1))
            (cons 'duplicate-typed-record cell))))

;; ---------------------------------------------------------------------------
;; W1-B provider-specific contract inventory
;; ---------------------------------------------------------------------------

(struct provider-specific-contract (provider name expected note) #:transparent)

(define provider-specific-contracts
  (list
   (provider-specific-contract 'anthropic
                               'anthropic-message-start-zero
                               '()
                               "message_start emits no prompt-usage chunk when input_tokens is zero")
   (provider-specific-contract 'gemini
                               'gemini-usage-total-fallback
                               '(7 3 10)
                               "missing totalTokenCount falls back to prompt plus candidates tokens")
   (provider-specific-contract 'openai-compatible
                               'openai-malformed-tool-arguments
                               "{broken"
                               "malformed JSON tool arguments remain the original string")
   (provider-specific-contract
    'azure-openai
    'azure-model-injection
    "deployment-test"
    "Azure wrapper injects the configured deployment when wire model is absent")))

(define (check-provider-specific-contracts-complete!)
  (define names (map provider-specific-contract-name provider-specific-contracts))
  (define providers (map provider-specific-contract-provider provider-specific-contracts))
  (append (for/list ([provider (in-list provider-contract-names)]
                     #:unless (member provider providers))
            (list 'missing-provider provider))
          (for/list ([provider (in-list provider-contract-names)]
                     #:when (> (count (lambda (candidate) (eq? candidate provider)) providers) 1))
            (list 'duplicate-provider provider))
          (for/list ([provider (in-list providers)]
                     #:unless (member provider provider-contract-names))
            (list 'orphan-provider provider))
          (for/list ([name (in-list names)]
                     #:when (> (count (lambda (candidate) (eq? candidate name)) names) 1))
            (list 'duplicate-case name))
          (for/list ([contract (in-list provider-specific-contracts)]
                     #:unless (positive? (string-length (provider-specific-contract-note contract))))
            (list 'missing-note (provider-specific-contract-name contract)))))

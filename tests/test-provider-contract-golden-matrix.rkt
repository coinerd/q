#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-provider-contract-golden-matrix.rkt — Provider Contract Golden Matrix
;;
;; v0.99.91 W0 (#9237): drives the REAL provider parsers with representative
;; fixtures and asserts the normalized observable equals the golden matrix
;; expectation — for every scenario x provider. No artificial equality:
;; scenarios a provider genuinely cannot normalize (Anthropic/Gemini reasoning
;; deltas) are asserted to yield the unsupported observable, not a fabricated
;; match.

(require rackunit
         racket/match
         racket/port
         racket/string
         json
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/azure-openai.rkt"
         "../llm/stream.rkt"
         "../llm/model.rkt"
         "../llm/provider-errors.rkt"
         "../llm/http-helpers.rkt"
         "helpers/provider-contract-matrix.rkt")

;; ============================================================
;; Fixtures (per-provider wire shapes)
;; ============================================================

;; --- text-nonstream ---
(define text-fixture
  (hash
   'anthropic
   (hasheq 'model
           "claude-test"
           'id
           "msg_1"
           'stop_reason
           "end_turn"
           'usage
           (hasheq 'input_tokens 10 'output_tokens 5)
           'content
           (list (hasheq 'type "text" 'text "hi")))
   'gemini
   (hasheq 'modelVersion
           "gemini-test"
           'usageMetadata
           (hasheq 'promptTokenCount 7 'candidatesTokenCount 3 'totalTokenCount 10)
           'candidates
           (list (hasheq 'content (hasheq 'parts (list (hasheq 'text "hi"))) 'finishReason "STOP")))
   'openai-compatible
   (hasheq 'model
           "gpt-test"
           'id
           "cmpl_1"
           'usage
           (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6)
           'choices
           (list (hasheq 'message (hasheq 'content "hi" 'role "assistant") 'finish_reason "stop")))
   'azure-openai
   (hasheq 'model
           "azure-test"
           'id
           "cmpl_2"
           'usage
           (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6)
           'choices
           (list (hasheq 'message (hasheq 'content "hi" 'role "assistant") 'finish_reason "stop")))))

;; --- reasoning-delta ---
;; OpenAI/Azure: delta.reasoning_content string.
;; Anthropic: thinking_delta (no normalization -> no chunk).
;; Gemini: part with 'thought (no mapping -> no chunk).
(define reasoning-fixture
  (hash 'anthropic
        (hasheq 'type
                "content_block_delta"
                'index
                0
                'delta
                (hasheq 'type "thinking_delta" 'thinking "Let me think..."))
        'gemini
        (hasheq 'candidates
                (list (hasheq 'content
                              (hasheq 'parts (list (hasheq 'thought #t 'text "Let me think..."))))))
        'openai-compatible
        (hasheq 'choices
                (list (hasheq 'delta (hasheq 'reasoning_content "reasoning...") 'finish_reason #f)))
        'azure-openai
        (hasheq 'choices
                (list (hasheq 'delta (hasheq 'reasoning_content "reasoning...") 'finish_reason #f)))))

;; --- usage-stream-done ---
;; Anthropic: message_delta with output_tokens only.
;; Gemini: event with finishReason + usageMetadata full.
;; OpenAI/Azure: final chunk with full usage hash.
(define usage-done-fixture
  (hash 'anthropic
        (hasheq 'type
                "message_delta"
                'delta
                (hasheq 'stop_reason "end_turn")
                'usage
                (hasheq 'output_tokens 5))
        'gemini
        (hasheq 'candidates
                (list (hasheq 'content (hasheq 'parts '()) 'finishReason "STOP"))
                'usageMetadata
                (hasheq 'promptTokenCount 7 'candidatesTokenCount 3 'totalTokenCount 10))
        'openai-compatible
        (hasheq 'choices
                (list (hasheq 'delta (hasheq 'content #f) 'finish_reason "stop"))
                'usage
                (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6))
        'azure-openai
        (hasheq 'choices
                (list (hasheq 'delta (hasheq 'content #f) 'finish_reason "stop"))
                'usage
                (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6))))

;; --- tool-call-nonstream ---
(define tool-fixture
  (hash
   'anthropic
   (hasheq
    'model
    "claude-test"
    'id
    "msg_1"
    'stop_reason
    "tool_use"
    'usage
    (hasheq 'input_tokens 10 'output_tokens 5)
    'content
    (list (hasheq 'type "tool_use" 'id "toolu_1" 'name "read_file" 'input (hasheq 'path "foo.rkt"))))
   'gemini
   (hasheq
    'modelVersion
    "gemini-test"
    'usageMetadata
    (hasheq 'promptTokenCount 7 'candidatesTokenCount 3 'totalTokenCount 10)
    'candidates
    (list
     (hasheq
      'content
      (hasheq 'parts
              (list (hasheq 'functionCall
                            (hasheq 'id "fc_1" 'name "read_file" 'args (hasheq 'path "foo.rkt")))))
      'finishReason
      "STOP")))
   'openai-compatible
   (hasheq
    'model
    "gpt-test"
    'id
    "cmpl_1"
    'usage
    (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6)
    'choices
    (list (hasheq
           'message
           (hasheq 'content
                   #f
                   'tool_calls
                   (list (hasheq 'id
                                 "call_1"
                                 'type
                                 "function"
                                 'function
                                 (hasheq 'name "read_file" 'arguments "{\"path\":\"foo.rkt\"}"))))
           'finish_reason
           "tool_calls")))
   'azure-openai
   (hasheq
    'model
    "azure-test"
    'id
    "cmpl_2"
    'usage
    (hasheq 'prompt_tokens 4 'completion_tokens 2 'total_tokens 6)
    'choices
    (list (hasheq
           'message
           (hasheq 'content
                   #f
                   'tool_calls
                   (list (hasheq 'id
                                 "call_2"
                                 'type
                                 "function"
                                 'function
                                 (hasheq 'name "read_file" 'arguments "{\"path\":\"foo.rkt\"}"))))
           'finish_reason
           "tool_calls")))))

;; --- tool-delta-stream ---
;; Anthropic: content_block_start (index 0, id, name) + input_json_delta.
;; Gemini: functionCall part (full args in one delta).
;; OpenAI/Azure: delta.tool_calls[0].
(define tool-delta-fixture
  (hash
   'anthropic
   (list (hasheq 'type
                 "content_block_start"
                 'index
                 0
                 'content_block
                 (hasheq 'type "tool_use" 'id "toolu_1" 'name "read_file"))
         (hasheq 'type
                 "content_block_delta"
                 'index
                 0
                 'delta
                 (hasheq 'type "input_json_delta" 'partial_json "{\"path\":\"foo.rkt\"}")))
   'gemini
   (hasheq
    'candidates
    (list (hasheq
           'content
           (hasheq
            'parts
            (list (hasheq 'functionCall
                          (hasheq 'id "fc_1" 'name "read_file" 'args (hasheq 'path "foo.rkt"))))))))
   'openai-compatible
   (hasheq
    'choices
    (list (hasheq 'delta
                  (hasheq 'tool_calls
                          (list (hasheq 'index
                                        0
                                        'id
                                        "call_1"
                                        'type
                                        "function"
                                        'function
                                        (hasheq 'name "read_file" 'arguments "{\"path\":\"foo")))
                          'finish_reason
                          #f))))
   'azure-openai
   (hasheq
    'choices
    (list (hasheq 'delta
                  (hasheq 'tool_calls
                          (list (hasheq 'index
                                        0
                                        'id
                                        "call_2"
                                        'type
                                        "function"
                                        'function
                                        (hasheq 'name "read_file" 'arguments "{\"path\":\"foo")))
                          'finish_reason
                          #f))))))

;; --- sse-framing ---
(define sse-framing-text
  ": keep-alive\n\nid: 1\ndata: {\"type\":\"ping\"}\n\nid: 2\ndata: {\"type\":\"text\",\"text\":\"hi\"}\n\ndata: [DONE]\n")

;; --- timeout-exn ---
;; Shared call-with-request-timeout with an immediate 0 timeout.

;; --- malformed-sse ---
(define malformed-line "data: {not valid json")

;; --- error-envelope ---
(define error-body (string->bytes/utf-8 "{\"error\":{\"message\":\"boom\"}}"))
(define status-400 (string->bytes/utf-8 "HTTP/1.1 400 Bad Request"))

;; ============================================================
;; Probe helpers — drive the REAL parser per provider
;; ============================================================

(define (parse-nonstream provider fixture)
  (match provider
    ['anthropic (anthropic-parse-response fixture)]
    ['gemini (gemini-parse-response fixture)]
    ['openai-compatible (openai-parse-response fixture)]
    ['azure-openai (openai-parse-response-from-jsexpr fixture "azure-test")]))

(define (parse-stream-event provider fixture)
  (match provider
    ['anthropic (anthropic-parse-single-event fixture (box #f) (box #f) (box 0))]
    ['gemini (gemini-parse-single-event fixture)]
    ['openai-compatible (list (normalize-openai-chunk fixture))]
    ['azure-openai (list (normalize-openai-chunk fixture))]))

;; ============================================================
;; Scenario probes -> normalized observable
;; ============================================================

(define (probe-text-nonstream provider)
  (define resp (parse-nonstream provider (hash-ref text-fixture provider)))
  (define content (model-response-content resp))
  (list (length content) (hash-ref (car content) 'type)))

(define (probe-reasoning-delta provider)
  (define chunks (parse-stream-event provider (hash-ref reasoning-fixture provider)))
  (if (null? chunks)
      'no-chunks
      (stream-chunk-delta-thinking (car chunks))))

(define (probe-usage-nonstream provider)
  (define resp (parse-nonstream provider (hash-ref text-fixture provider)))
  (define u (model-response-usage resp))
  (list (hash-ref u 'prompt_tokens) (hash-ref u 'completion_tokens) (hash-ref u 'total_tokens)))

(define (probe-usage-stream-done provider)
  (define chunks (parse-stream-event provider (hash-ref usage-done-fixture provider)))
  (define done-chunk (findf (lambda (ch) (stream-chunk-done? ch)) chunks))
  (define u (and done-chunk (stream-chunk-usage done-chunk)))
  (if u
      (sort (hash-keys u) string<? #:key symbol->string)
      'no-done-chunk))

(define (probe-tool-call-nonstream provider)
  (define resp (parse-nonstream provider (hash-ref tool-fixture provider)))
  (define content (model-response-content resp))
  (define tc (findf (lambda (blk) (equal? (hash-ref blk 'type #f) "tool-call")) content))
  (list (hash-ref tc 'type) (hash-ref tc 'name)))

(define (probe-tool-delta-stream provider)
  (define chunks
    (if (eq? provider 'anthropic)
        (let ([events (hash-ref tool-delta-fixture provider)])
          (define tid (box #f))
          (define tname (box #f))
          (define tidx (box 0))
          (apply append
                 (for/list ([ev (in-list events)])
                   (anthropic-parse-single-event ev tid tname tidx))))
        (parse-stream-event provider (hash-ref tool-delta-fixture provider))))
  (define with-tool (findf (lambda (ch) (stream-chunk-delta-tool-call ch)) chunks))
  (if with-tool 'hash 'no-tool-delta))

(define (probe-sse-framing provider)
  ;; shared parse-sse-lines — count data events in a 5-line stream
  (length (parse-sse-lines sse-framing-text)))

(define (probe-timeout-exn provider)
  (define raised?
    (with-handlers ([exn:fail:network:timeout? (lambda (e) #t)]
                    [exn? (lambda (e) #f)])
      (call-with-request-timeout (lambda () (sleep 10)) #:timeout 0.05)
      #f))
  raised?)

(define (probe-malformed-sse provider)
  (parse-sse-line malformed-line))

(define (probe-error-envelope provider)
  (define raised
    (with-handlers ([provider-error? (lambda (e) (provider-error-category e))]
                    [exn? (lambda (e) 'other-exn)])
      (if (eq? provider 'azure-openai)
          (check-azure-status! status-400 error-body)
          (check-provider-status! "Probe" status-400 error-body))
      'no-error-raised))
  raised)

;; scenario -> probe procedure
(define probe-table
  (hash 'text-nonstream
        probe-text-nonstream
        'reasoning-delta
        probe-reasoning-delta
        'usage-nonstream
        probe-usage-nonstream
        'usage-stream-done
        probe-usage-stream-done
        'tool-call-nonstream
        probe-tool-call-nonstream
        'tool-delta-stream
        probe-tool-delta-stream
        'sse-framing
        probe-sse-framing
        'timeout-exn
        probe-timeout-exn
        'malformed-sse
        probe-malformed-sse
        'error-envelope
        probe-error-envelope))

;; ============================================================
;; Tests
;; ============================================================

;; G1: the matrix is complete — every scenario has an explicit cell for every
;; provider (no accidental gap that would silently skip a provider).
(test-case "G1: golden matrix complete — every scenario x provider explicit"
  (define gaps (check-matrix-complete!))
  (check-equal? gaps '() (format "matrix gaps: ~a" gaps)))

;; G2: every cell's expectation matches the REAL parser output.
(test-case "G2: real parser output matches golden matrix per scenario x provider"
  (for ([scenario (in-list provider-contract-scenarios)])
    (for ([provider (in-list provider-contract-names)])
      (define cell (matrix-cell-for scenario provider))
      (check-not-false cell (format "~a/~a missing cell" scenario provider))
      (when cell
        (define probe (hash-ref probe-table scenario))
        (define actual (probe provider))
        (define expected (matrix-cell-expected cell))
        (check-equal? actual expected (format "~a/~a observable" scenario provider))))))

;; G3: unsupported cells are genuinely unsupported — the parser must NOT
;; fabricate the normalized capability. This is the anti-artificial-equality
;; gate: Anthropic/Gemini reasoning deltas stay unsupported.
(test-case "G3: no artificial equality — unsupported scenarios stay unsupported"
  (define unsupported-cells
    (for*/list ([scenario (in-list provider-contract-scenarios)]
                [provider (in-list provider-contract-names)]
                #:when (let ([c (matrix-cell-for scenario provider)])
                         (and c (not (matrix-cell-supported? c)))))
      (list scenario provider)))
  ;; Currently exactly: reasoning-delta for anthropic + gemini.
  (check-equal? (map car unsupported-cells)
                (list 'reasoning-delta 'reasoning-delta)
                "expected unsupported set")
  (check-equal? (map cadr unsupported-cells)
                (list 'anthropic 'gemini)
                "expected unsupported providers")
  ;; And each unsupported cell carries an explanatory note (no silent gap).
  (for ([pair (in-list unsupported-cells)])
    (define cell (matrix-cell-for (car pair) (cadr pair)))
    (check-true (positive? (string-length (matrix-cell-note cell)))
                (format "~a/~a must carry a capability note" (car pair) (cadr pair)))))

;; G4: capabilities are explicit and documented per provider (matrix is
;; self-describing) — every supported cell carries a non-empty note.
(test-case "G4: every matrix cell carries an explicit capability note"
  (for ([scenario (in-list provider-contract-scenarios)])
    (for ([provider (in-list provider-contract-names)])
      (define cell (matrix-cell-for scenario provider))
      (check-true (positive? (string-length (matrix-cell-note cell)))
                  (format "~a/~a missing note" scenario provider)))))

;; G5: the known streaming-usage asymmetry is REAL — Anthropic's done chunk
;; usage has only completion_tokens while Gemini/OpenAI/Azure carry full keys.
;; Asserting the asymmetry explicitly (not equalizing it) is the contract.
(test-case "G5: documented streaming-usage asymmetry holds"
  (check-equal? (probe-usage-stream-done 'anthropic) '(completion_tokens))
  (check-equal? (probe-usage-stream-done 'gemini) '(completion_tokens prompt_tokens total_tokens))
  (check-equal? (probe-usage-stream-done 'openai-compatible)
                '(completion_tokens prompt_tokens total_tokens))
  (check-equal? (probe-usage-stream-done 'azure-openai)
                '(completion_tokens prompt_tokens total_tokens)))

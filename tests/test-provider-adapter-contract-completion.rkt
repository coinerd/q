#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; v0.99.91 W1-B (#9238): provider-specific adapter contracts and typed
;; unsupported capabilities. All probes drive existing REAL provider parsers;
;; this file introduces no shared production abstraction.

(require rackunit
         racket/list
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/azure-openai.rkt"
         "../llm/model.rkt"
         "../llm/stream.rkt"
         "helpers/provider-contract-matrix.rkt")

;; ---------------------------------------------------------------------------
;; Provider-specific REAL-parser probes
;; ---------------------------------------------------------------------------

(define (probe-anthropic-message-start-zero)
  (anthropic-parse-single-event
   (hasheq 'type "message_start" 'message (hasheq 'usage (hasheq 'input_tokens 0 'output_tokens 0)))
   (box #f)
   (box #f)
   (box 0)))

(define (probe-gemini-usage-total-fallback)
  (define response
    (gemini-parse-response
     (hasheq
      'modelVersion
      "gemini-test"
      'usageMetadata
      (hasheq 'promptTokenCount 7 'candidatesTokenCount 3)
      'candidates
      (list (hasheq 'content (hasheq 'parts (list (hasheq 'text "ok"))) 'finishReason "STOP")))))
  (define usage (model-response-usage response))
  (list (hash-ref usage 'prompt_tokens)
        (hash-ref usage 'completion_tokens)
        (hash-ref usage 'total_tokens)))

(define (probe-openai-malformed-tool-arguments)
  (define response
    (openai-parse-response
     (hasheq 'model
             "gpt-test"
             'choices
             (list (hasheq 'message
                           (hasheq 'content
                                   #f
                                   'tool_calls
                                   (list (hasheq 'id
                                                 "call_bad"
                                                 'type
                                                 "function"
                                                 'function
                                                 (hasheq 'name "read_file" 'arguments "{broken"))))
                           'finish_reason
                           "tool_calls")))))
  (define tool-block
    (findf (lambda (block) (equal? (hash-ref block 'type #f) "tool-call"))
           (model-response-content response)))
  (hash-ref tool-block 'arguments))

(define (probe-azure-model-injection)
  (define response
    (openai-parse-response-from-jsexpr
     (hasheq 'id
             "azure-response-1"
             'choices
             (list (hasheq 'message (hasheq 'content "ok") 'finish_reason "stop")))
     "deployment-test"))
  (model-response-model response))

(define provider-specific-probes
  (hash 'anthropic-message-start-zero
        probe-anthropic-message-start-zero
        'gemini-usage-total-fallback
        probe-gemini-usage-total-fallback
        'openai-malformed-tool-arguments
        probe-openai-malformed-tool-arguments
        'azure-model-injection
        probe-azure-model-injection))

;; ---------------------------------------------------------------------------
;; Contract gates
;; ---------------------------------------------------------------------------

(test-case "W1-B1: every unsupported W0 cell has one typed capability record"
  (check-equal? (check-typed-unsupported-complete!) '())
  (check-equal? (length typed-unsupported-capabilities) 2)
  (for ([record (in-list typed-unsupported-capabilities)])
    (check-true (symbol? (unsupported-capability-provider record)))
    (check-true (symbol? (unsupported-capability-scenario record)))
    (check-true (symbol? (unsupported-capability-kind record)))
    ;; The exact observable may legitimately be #f (Gemini delta-thinking).
    ;; W1-B2 pins each provider-specific value below.
    (check-true (positive? (string-length (unsupported-capability-rationale record))))))

(test-case "W1-B2: typed records remain source-grounded, not artificially equal"
  (define anthropic (typed-unsupported-capability-for 'reasoning-delta 'anthropic))
  (define gemini (typed-unsupported-capability-for 'reasoning-delta 'gemini))
  (check-equal? (unsupported-capability-kind anthropic) 'unmapped-event)
  (check-equal? (unsupported-capability-observable anthropic) 'no-chunks)
  (check-equal? (unsupported-capability-kind gemini) 'mapped-to-text-not-thinking)
  (check-false (unsupported-capability-observable gemini)))

(test-case "W1-B3: provider-specific contract inventory is complete"
  (check-equal? (check-provider-specific-contracts-complete!) '())
  (check-equal? (sort (remove-duplicates (map provider-specific-contract-provider
                                              provider-specific-contracts))
                      symbol<?)
                '(anthropic azure-openai gemini openai-compatible))
  ;; Reviewer MINOR-2: contract data and executable probes are a bijection;
  ;; neither stale probes nor unprobed expectations can silently survive.
  (check-equal? (sort (hash-keys provider-specific-probes) symbol<?)
                (sort (map provider-specific-contract-name provider-specific-contracts) symbol<?)))

(test-case "W1-B4: every provider-specific expectation matches its REAL parser"
  (for ([contract (in-list provider-specific-contracts)])
    (define probe (hash-ref provider-specific-probes (provider-specific-contract-name contract)))
    (check-equal? (probe)
                  (provider-specific-contract-expected contract)
                  (format "~a/~a observable"
                          (provider-specific-contract-provider contract)
                          (provider-specific-contract-name contract)))))

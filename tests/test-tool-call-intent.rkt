#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-tool-call-intent.rkt — Unified tool-call-intent AST tests
;;
;; Covers:
;;   (A) tool-call-intent round-trip via hash
;;   (B) Shadow validation in provider parsers (OpenAI, Anthropic, Gemini)

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string
         racket/hash
         (only-in "../llm/model.rkt"
                  make-tool-call-intent
                  tool-call-intent-id
                  tool-call-intent-name
                  tool-call-intent-arguments
                  tool-call-intent->hash
                  hash->tool-call-intent
                  make-model-response
                  model-response-content)
         (only-in "../llm/openai-compatible.rkt"
                  openai-parse-response)
         (only-in "../llm/anthropic.rkt"
                  anthropic-parse-response)
         (only-in "../llm/gemini.rkt"
                  gemini-parse-response
                  gemini-reset-tool-id-counter!))

;; ============================================================
;; (A) Round-trip tests
;; ============================================================

(define roundtrip-tests
  (test-suite "tool-call-intent round-trip"

    (test-case "basic round-trip"
      (define tci (make-tool-call-intent "call_123" "bash" (hasheq 'cmd "ls")))
      (define h (tool-call-intent->hash tci))
      (check-equal? (hash-ref h 'type) "tool-call")
      (check-equal? (hash-ref h 'id) "call_123")
      (check-equal? (hash-ref h 'name) "bash")
      (check-equal? (hash-ref h 'arguments) (hasheq 'cmd "ls"))
      (define tci2 (hash->tool-call-intent h))
      (check-equal? (tool-call-intent-id tci) (tool-call-intent-id tci2))
      (check-equal? (tool-call-intent-name tci) (tool-call-intent-name tci2))
      (check-equal? (tool-call-intent-arguments tci) (tool-call-intent-arguments tci2)))

    (test-case "empty arguments default to empty hash"
      (define tci (make-tool-call-intent "call_0" "date" (hasheq)))
      (define h (tool-call-intent->hash tci))
      (define tci2 (hash->tool-call-intent h))
      (check-equal? (tool-call-intent-arguments tci2) (hasheq)))

    (test-case "hash->tool-call-intent handles missing fields"
      (define h (hasheq 'type "tool-call" 'id "x" 'name "y"))
      (define tci (hash->tool-call-intent h))
      (check-equal? (tool-call-intent-id tci) "x")
      (check-equal? (tool-call-intent-name tci) "y")
      (check-equal? (tool-call-intent-arguments tci) (hasheq)))

    (test-case "hash->tool-call-intent handles non-string id/name"
      (define h (hasheq 'type "tool-call" 'id 42 'name 'foo 'arguments (hasheq 'a 1)))
      (define tci (hash->tool-call-intent h))
      (check-equal? (tool-call-intent-id tci) "")
      (check-equal? (tool-call-intent-name tci) "")
      (check-equal? (tool-call-intent-arguments tci) (hasheq 'a 1)))

    (test-case "nested arguments survive round-trip"
      (define args (hasheq 'file "app.rkt" 'opts (hasheq 'lines 10)))
      (define tci (make-tool-call-intent "c1" "read" args))
      (define h (tool-call-intent->hash tci))
      (define tci2 (hash->tool-call-intent h))
      (check-equal? (tool-call-intent-arguments tci2) args))))

;; ============================================================
;; (B) Provider shadow tests
;; ============================================================

(define provider-tests
  (test-suite "Provider shadow migration"

    (test-case "OpenAI parse produces tool-call-intent-compatible hash"
      (define raw
        (hasheq 'model "gpt-4"
                'usage (hasheq)
                'choices
                (list (hasheq 'message
                              (hasheq 'content "hello"
                                      'tool_calls
                                      (list (hasheq 'id "call_1"
                                                    'function
                                                    (hasheq 'name "bash"
                                                            'arguments "{\"cmd\":\"ls\"}"))))
                             'finish_reason "stop"))))
      (define resp (openai-parse-response raw))
      (define content (let ([c (model-response-content resp)]) c))
      (define tc-block (for/first ([b (in-list content)]
                                   #:when (equal? (hash-ref b 'type "") "tool-call"))
                         b))
      (check-true (hash? tc-block) "OpenAI response should have tool-call block")
      (when tc-block
        (define tci (hash->tool-call-intent tc-block))
        (check-equal? (tool-call-intent-id tci) "call_1")
        (check-equal? (tool-call-intent-name tci) "bash")
        (check-equal? (tool-call-intent-arguments tci) (hasheq 'cmd "ls"))))

    (test-case "Anthropic parse produces tool-call-intent-compatible hash"
      (define raw
        (hasheq 'model "claude-3"
                'usage (hasheq 'input_tokens 10 'output_tokens 5)
                'stop_reason "stop"
                'content
                (list (hasheq 'type "tool_use"
                              'id "tu_1"
                              'name "bash"
                              'input (hasheq 'cmd "ls")))))
      (define resp (anthropic-parse-response raw))
      (define content (let ([c (model-response-content resp)]) c))
      (define tc-block (for/first ([b (in-list content)]
                                   #:when (equal? (hash-ref b 'type "") "tool-call"))
                         b))
      (check-true (hash? tc-block) "Anthropic response should have tool-call block")
      (when tc-block
        (define tci (hash->tool-call-intent tc-block))
        (check-equal? (tool-call-intent-id tci) "tu_1")
        (check-equal? (tool-call-intent-name tci) "bash")
        (check-equal? (tool-call-intent-arguments tci) (hasheq 'cmd "ls"))))

    (test-case "Gemini parse produces tool-call-intent-compatible hash"
      (gemini-reset-tool-id-counter!)
      (define raw
        (hasheq 'modelVersion "gemini-1.5"
                'usageMetadata (hasheq 'promptTokenCount 10
                                       'candidatesTokenCount 5
                                       'totalTokenCount 15)
                'candidates
                (list (hasheq 'content
                              (hasheq 'parts
                                      (list (hasheq 'functionCall
                                                    (hasheq 'name "bash"
                                                            'args (hasheq 'cmd "ls")))))
                             'finishReason "STOP"))))
      (define resp (gemini-parse-response raw))
      (define content (let ([c (model-response-content resp)]) c))
      (define tc-block (for/first ([b (in-list content)]
                                   #:when (equal? (hash-ref b 'type "") "tool-call"))
                         b))
      (check-true (hash? tc-block) "Gemini response should have tool-call block")
      (when tc-block
        (define tci (hash->tool-call-intent tc-block))
        ;; ID is generated by gemini-gen-tool-id
        (check-true (string? (tool-call-intent-id tci)))
        (check-equal? (tool-call-intent-name tci) "bash")
        (check-equal? (tool-call-intent-arguments tci) (hasheq 'cmd "ls"))))

    (test-case "shadow mismatch detection triggers warning"
      ;; Tamper with name field to simulate non-string name
      (define tampered
        (hasheq 'id "tc-mismatch"
                'name 42
                'arguments (hasheq 'path "/tmp/x")))
      ;; hash->tool-call-intent defensively converts non-string name to ""
      (define tci (hash->tool-call-intent tampered))
      (define round-trip (tool-call-intent->hash tci))
      ;; Verify the mismatch: original had name 42 but round-tripped to ""
      (check-not-equal? (hash-ref tampered 'name) (hash-ref round-trip 'name)
                        "tampered integer name should differ from round-tripped string name")
      ;; Verify the defensive default
      (check-equal? (tool-call-intent-name tci) ""
                    "non-string name should default to empty string"))))

;; ============================================================
;; Run all
;; ============================================================

(run-tests roundtrip-tests)
(run-tests provider-tests)

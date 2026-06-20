#lang racket/base

;; @speed fast
;; @suite fast

;; W8 v0.99.35: Tests for anthropic-helpers.rkt
;; Pure functions extracted from llm/anthropic.rkt.
;; Tests cover: content block conversion, tool definition translation,
;; usage normalization — all edge cases.

(require rackunit
         rackunit/text-ui
         "../llm/anthropic-helpers.rkt")

;; ============================================================
;; openai-block->anthropic tests
;; ============================================================

(define-test-suite
 openai-block->anthropic-tests
 (test-case "text block passes through unchanged"
   (define block (hasheq 'type "text" 'text "hello"))
   (check-equal? (openai-block->anthropic block) block))
 (test-case "text block with default type when missing"
   (define block (hasheq 'text "hello"))
   (check-equal? (openai-block->anthropic block) block))
 (test-case "image_url block converts to Anthropic image format"
   (define block (hasheq 'type "image_url" 'image_url (hasheq 'url "data:image/png;base64,iVBOR")))
   (define result (openai-block->anthropic block))
   (check-equal? (hash-ref result 'type) "image")
   (define source (hash-ref result 'source))
   (check-equal? (hash-ref source 'type) "base64")
   (check-equal? (hash-ref source 'media_type) "image/png")
   (check-equal? (hash-ref source 'data) "iVBOR"))
 (test-case "image_url with jpeg data url"
   (define block (hasheq 'type "image_url" 'image_url (hasheq 'url "data:image/jpeg;base64,/9j/")))
   (define result (openai-block->anthropic block))
   (check-equal? (hash-ref result 'type) "image")
   (check-equal? (hash-ref (hash-ref result 'source) 'media_type) "image/jpeg")
   (check-equal? (hash-ref (hash-ref result 'source) 'data) "/9j/"))
 (test-case "unknown block type passes through unchanged"
   (define block (hasheq 'type "custom" 'foo "bar"))
   (check-equal? (openai-block->anthropic block) block))
 (test-case "image_url with missing url defaults to empty"
   (define block (hasheq 'type "image_url"))
   (define result (openai-block->anthropic block))
   (check-equal? (hash-ref result 'type) "image")))

;; ============================================================
;; anthropic-translate-tool tests
;; ============================================================

(define-test-suite
 anthropic-translate-tool-tests
 (test-case "standard function tool"
   (define tool
     (hasheq
      'type
      "function"
      'function
      (hasheq 'name "get_weather" 'description "Get weather" 'parameters (hasheq 'type "object"))))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'name) "get_weather")
   (check-equal? (hash-ref result 'description) "Get weather")
   (check-equal? (hash-ref (hash-ref result 'input_schema) 'type) "object"))
 (test-case "tool without type wrapper (direct function hash)"
   (define tool (hasheq 'name "bash" 'description "Run bash" 'parameters (hasheq)))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'name) "bash")
   (check-equal? (hash-ref result 'description) "Run bash"))
 (test-case "missing name defaults to unknown"
   (define tool (hasheq 'function (hasheq 'description "no name")))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'name) "unknown"))
 (test-case "missing description defaults to empty"
   (define tool (hasheq 'function (hasheq 'name "tool1")))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'description) ""))
 (test-case "missing parameters defaults to empty hash"
   (define tool (hasheq 'function (hasheq 'name "tool2" 'description "test")))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'input_schema) (hasheq)))
 (test-case "complex nested parameters preserved"
   (define params
     (hasheq 'type
             "object"
             'properties
             (hasheq 'location (hasheq 'type "string"))
             'required
             (list "location")))
   (define tool (hasheq 'function (hasheq 'name "weather" 'parameters params)))
   (define result (anthropic-translate-tool tool))
   (check-equal? (hash-ref result 'input_schema) params)))

;; ============================================================
;; translate-anthropic-usage tests
;; ============================================================

(define-test-suite translate-anthropic-usage-tests
                   (test-case "standard usage with both token types"
                     (define usage (hasheq 'input_tokens 100 'output_tokens 50))
                     (define result (translate-anthropic-usage usage))
                     (check-equal? (hash-ref result 'prompt_tokens) 100)
                     (check-equal? (hash-ref result 'completion_tokens) 50)
                     (check-equal? (hash-ref result 'total_tokens) 150))
                   (test-case "zero tokens"
                     (define usage (hasheq 'input_tokens 0 'output_tokens 0))
                     (define result (translate-anthropic-usage usage))
                     (check-equal? (hash-ref result 'total_tokens) 0))
                   (test-case "large token counts"
                     (define usage (hasheq 'input_tokens 100000 'output_tokens 50000))
                     (define result (translate-anthropic-usage usage))
                     (check-equal? (hash-ref result 'total_tokens) 150000))
                   (test-case "missing input_tokens defaults to 0"
                     (define usage (hasheq 'output_tokens 30))
                     (define result (translate-anthropic-usage usage))
                     (check-equal? (hash-ref result 'prompt_tokens) 0)
                     (check-equal? (hash-ref result 'completion_tokens) 30)
                     (check-equal? (hash-ref result 'total_tokens) 30))
                   (test-case "missing output_tokens defaults to 0"
                     (define usage (hasheq 'input_tokens 70))
                     (define result (translate-anthropic-usage usage))
                     (check-equal? (hash-ref result 'prompt_tokens) 70)
                     (check-equal? (hash-ref result 'completion_tokens) 0)
                     (check-equal? (hash-ref result 'total_tokens) 70))
                   (test-case "empty usage hash"
                     (define result (translate-anthropic-usage (hasheq)))
                     (check-equal? (hash-ref result 'prompt_tokens) 0)
                     (check-equal? (hash-ref result 'completion_tokens) 0)
                     (check-equal? (hash-ref result 'total_tokens) 0))
                   (test-case "result keys are correct"
                     (define result
                       (translate-anthropic-usage (hasheq 'input_tokens 1 'output_tokens 1)))
                     (check-true (hash-has-key? result 'prompt_tokens))
                     (check-true (hash-has-key? result 'completion_tokens))
                     (check-true (hash-has-key? result 'total_tokens))))

;; ============================================================
;; All tests
;; ============================================================

(define-test-suite all-anthropic-helpers-tests
                   openai-block->anthropic-tests
                   anthropic-translate-tool-tests
                   translate-anthropic-usage-tests)

(module+ test
  (run-tests all-anthropic-helpers-tests))

(module+ main
  (run-tests all-anthropic-helpers-tests))

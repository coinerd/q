#lang racket/base

;; llm/anthropic-helpers.rkt — Pure helper functions for Anthropic provider
;;
;; W8 v0.99.35: Narrow extraction of pure parse/format functions from
;; llm/anthropic.rkt. These functions perform pure hash→hash transformations
;; with no I/O, no state mutation, no side effects, and no logging.
;;
;; STABILITY: evolving

(require racket/match
         (only-in "vision-helpers.rkt" parse-data-url))

(provide openai-block->anthropic
         anthropic-translate-tool
         translate-anthropic-usage)

;; ============================================================
;; Content block conversion (request side)
;; ============================================================

;; Convert OpenAI-format content blocks to Anthropic content blocks.
;; Handles: text (passthrough), image_url → image (base64 decode via parse-data-url).
(define (openai-block->anthropic block)
  (define btype (hash-ref block 'type "text"))
  (cond
    [(equal? btype "text") block]
    [(equal? btype "image_url")
     (define image-url-hash (hash-ref block 'image_url (hasheq)))
     (define url (hash-ref image-url-hash 'url ""))
     (define-values (mime data) (parse-data-url url))
     (hasheq 'type "image" 'source (hasheq 'type "base64" 'media_type mime 'data data))]
    [else block]))

;; ============================================================
;; Tool definition translation (request side)
;; ============================================================

;; Translate a normalized tool definition to Anthropic format.
;; Input: {"type":"function","function":{"name":"...","description":"...","parameters":{}}}
;; Output: {"name":"...","description":"...","input_schema":{...}}
(define (anthropic-translate-tool tool)
  (define fn (hash-ref tool 'function tool))
  (define name (hash-ref fn 'name "unknown"))
  (define description (hash-ref fn 'description ""))
  (define parameters (hash-ref fn 'parameters (hasheq)))
  (hasheq 'name name 'description description 'input_schema parameters))

;; ============================================================
;; Usage normalization (response side)
;; ============================================================

;; Translate Anthropic usage hash to normalized format.
;; input_tokens → prompt_tokens, output_tokens → completion_tokens,
;; total_tokens = input + output.
(define (translate-anthropic-usage usage-raw)
  (define input-tokens (hash-ref usage-raw 'input_tokens 0))
  (define output-tokens (hash-ref usage-raw 'output_tokens 0))
  (hasheq 'prompt_tokens
          input-tokens
          'completion_tokens
          output-tokens
          'total_tokens
          (+ input-tokens output-tokens)))

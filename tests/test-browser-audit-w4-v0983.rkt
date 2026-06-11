#lang racket

;; tests/test-browser-audit-w4-v0983.rkt — Tests for Wave 4 audit fixes (v0.98.3)
;; GAP-V1: Cross-provider vision image serialization

(require rackunit
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt"
         "../llm/model.rkt")

;; ---------------------------------------------------------------------------
;; GAP-V1: Anthropic image block conversion
;; ---------------------------------------------------------------------------

(test-case "GAP-V1: Anthropic converts OpenAI image_url block to Anthropic image block"
  (define req
    (make-model-request
     (list (hasheq 'role
                   "user"
                   'content
                   (list (hasheq 'type "text" 'text "Hello")
                         (hasheq 'type
                                 "image_url"
                                 'image_url
                                 (hasheq 'url "data:image/png;base64,ABC123" 'detail "auto")))))
     #f
     (hasheq 'model "claude-3" 'max-tokens 1024)))
  (define body (anthropic-build-request-body req))
  (define messages (hash-ref body 'messages))
  (check-equal? (length messages) 1)
  (define content (hash-ref (car messages) 'content))
  (check-equal? (length content) 2)
  (define img (cadr content))
  (check-equal? (hash-ref img 'type) "image")
  (define src (hash-ref img 'source))
  (check-equal? (hash-ref src 'type) "base64")
  (check-equal? (hash-ref src 'media_type) "image/png")
  (check-equal? (hash-ref src 'data) "ABC123"))

;; ---------------------------------------------------------------------------
;; GAP-V1: Gemini image block conversion
;; ---------------------------------------------------------------------------

(test-case "GAP-V1: Gemini converts OpenAI image_url block to Gemini inlineData part"
  (define req
    (make-model-request
     (list (hasheq 'role
                   "user"
                   'content
                   (list (hasheq 'type "text" 'text "Hello")
                         (hasheq 'type
                                 "image_url"
                                 'image_url
                                 (hasheq 'url "data:image/jpeg;base64,XYZ789" 'detail "auto")))))
     #f
     (hasheq 'model "gemini-1.5" 'max-tokens 1024)))
  (define body (gemini-build-request-body req))
  (define contents (hash-ref body 'contents))
  (check-equal? (length contents) 1)
  (define parts (hash-ref (car contents) 'parts))
  (check-equal? (length parts) 2)
  (define img (cadr parts))
  (check-true (hash-has-key? img 'inlineData))
  (define inline (hash-ref img 'inlineData))
  (check-equal? (hash-ref inline 'mimeType) "image/jpeg")
  (check-equal? (hash-ref inline 'data) "XYZ789"))

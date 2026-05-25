#lang racket

;;; tests/test-image-format.rkt — Provider image format tests (#5272)

(require rackunit
         rackunit/text-ui
         "../extensions/image-format.rkt")

(define image-format-tests
  (test-suite "image format"

    ;; ============================================================
    ;; OpenAI format
    ;; ============================================================

    (test-case "format-image-openai produces data URI"
      (define result (format-image-openai "abc123" "image/png"))
      (check-equal? (hash-ref result 'type) "image_url")
      (define url-obj (hash-ref result 'image_url))
      (check-true (hash-has-key? url-obj 'url))
      (check-true (string-contains? (hash-ref url-obj 'url) "data:image/png;base64,abc123")))

    (test-case "format-image-openai handles jpeg"
      (define result (format-image-openai "xyz" "image/jpeg"))
      (define url (hash-ref (hash-ref result 'image_url) 'url))
      (check-true (string-contains? url "data:image/jpeg;base64,xyz")))

    ;; ============================================================
    ;; Anthropic format
    ;; ============================================================

    (test-case "format-image-anthropic produces source block"
      (define result (format-image-anthropic "abc123" "image/png"))
      (check-equal? (hash-ref result 'type) "image")
      (define source (hash-ref result 'source))
      (check-equal? (hash-ref source 'type) "base64")
      (check-equal? (hash-ref source 'media_type) "image/png")
      (check-equal? (hash-ref source 'data) "abc123"))

    (test-case "format-image-anthropic handles jpeg"
      (define result (format-image-anthropic "xyz" "image/jpeg"))
      (check-equal? (hash-ref (hash-ref result 'source) 'media_type) "image/jpeg"))

    ;; ============================================================
    ;; Gemini format
    ;; ============================================================

    (test-case "format-image-gemini produces inlineData block"
      (define result (format-image-gemini "abc123" "image/png"))
      (check-equal? (hash-ref result 'type) "inline_data")
      (define inline-data (hash-ref result 'inlineData))
      (check-equal? (hash-ref inline-data 'mimeType) "image/png")
      (check-equal? (hash-ref inline-data 'data) "abc123"))

    (test-case "format-image-gemini handles webp"
      (define result (format-image-gemini "xyz" "image/webp"))
      (check-equal? (hash-ref (hash-ref result 'inlineData) 'mimeType) "image/webp"))

    ;; ============================================================
    ;; Dispatch
    ;; ============================================================

    (test-case "format-image-for-provider openai"
      (define result (format-image-for-provider 'openai "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "image_url"))

    (test-case "format-image-for-provider anthropic"
      (define result (format-image-for-provider 'anthropic "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "image"))

    (test-case "format-image-for-provider claude alias"
      (define result (format-image-for-provider 'claude "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "image"))

    (test-case "format-image-for-provider gemini"
      (define result (format-image-for-provider 'gemini "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "inline_data"))

    (test-case "format-image-for-provider google alias"
      (define result (format-image-for-provider 'google "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "inline_data"))

    (test-case "format-image-for-provider azure uses openai format"
      (define result (format-image-for-provider 'azure "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "image_url"))

    (test-case "format-image-for-provider unknown defaults to openai"
      (define result (format-image-for-provider 'unknown-provider "abc" "image/png"))
      (check-equal? (hash-ref result 'type) "image_url"))))

(module+ main
  (run-tests image-format-tests))

#lang racket/base

;; extensions/image-format.rkt — Provider-specific image format adapters (#5269, #5270)
;;
;; Converts base64-encoded image data into the correct format for each
;; LLM provider's API:
;;   - OpenAI:  image_url with data URI
;;   - Anthropic: source block with type "base64" and media_type
;;   - Gemini: inlineData with mimeType

(require racket/contract
         racket/string
         "image-input.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Provider format adapters
;; ═══════════════════════════════════════════════════════════════════

;; Format an image for OpenAI's chat completions API.
;; Returns a hash with type=image_url containing a data URI.
(define (format-image-openai base64-data media-type)
  (hasheq 'type
          "image_url"
          'image_url
          (hasheq 'url (format "data:~a;base64,~a" media-type base64-data))))

;; Format an image for Anthropic's messages API.
;; Returns a source block with type "base64".
(define (format-image-anthropic base64-data media-type)
  (hasheq 'type "image" 'source (hasheq 'type "base64" 'media_type media-type 'data base64-data)))

;; Format an image for Gemini's generateContent API.
;; Returns an inlineData block with mimeType.
(define (format-image-gemini base64-data media-type)
  (hasheq 'type "inline_data" 'inlineData (hasheq 'mimeType media-type 'data base64-data)))

;; ═══════════════════════════════════════════════════════════════════
;; Dispatch
;; ═══════════════════════════════════════════════════════════════════

;; Format an image for a specific provider.
;; provider-sym is one of: 'openai, 'anthropic, 'gemini
;; Returns a hash suitable for inclusion in the provider's message format.
(define (format-image-for-provider provider-sym base64-data media-type)
  (case provider-sym
    [(openai openai-compatible azure) (format-image-openai base64-data media-type)]
    [(anthropic claude) (format-image-anthropic base64-data media-type)]
    [(gemini google) (format-image-gemini base64-data media-type)]
    [else (format-image-openai base64-data media-type)]))

;; Build a complete multi-modal content block for a provider.
;; Returns a list of content parts suitable for the messages API.
(define (build-multimodal-content provider-sym text image-path)
  (define b64 (image->base64 image-path))
  (define mt (extension->media-type image-path))
  (list (hasheq 'type "text" 'text text) (format-image-for-provider provider-sym b64 mt)))

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

(provide (contract-out [format-image-openai (-> string? string? hash?)]
                       [format-image-anthropic (-> string? string? hash?)]
                       [format-image-gemini (-> string? string? hash?)]
                       [format-image-for-provider (-> symbol? string? string? hash?)]
                       [build-multimodal-content (-> symbol? string? path-string? (listof hash?))]))

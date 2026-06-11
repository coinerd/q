#lang racket/base
;; STABILITY: public

;; util/content-parts.rkt — Content part structs for messages
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Text, tool-call, and tool-result content parts with JSON serialization.

(require racket/contract)

(provide text-part
         text-part?
         text-part-text
         tool-call-part
         tool-call-part?
         tool-call-part-id
         tool-call-part-name
         tool-call-part-arguments
         tool-result-part
         tool-result-part?
         tool-result-part-tool-call-id
         tool-result-part-content
         tool-result-part-is-error?
         image-part
         image-part?
         image-part-mime-type
         image-part-data
         image-part-detail
         ;; Re-export parent struct accessors
         content-part?
         content-part-type
         (contract-out [make-text-part (-> string? text-part?)]
                       [make-tool-call-part
                        (-> (or/c string? #f) (or/c string? #f) (or/c string? hash?) tool-call-part?)]
                       [make-tool-result-part (-> string? (or/c string? hash? list?) boolean? tool-result-part?)]
                       [make-image-part (->* (string? string?) ((or/c string? #f)) image-part?)]
                       [content-part->jsexpr (-> content-part? hash?)]
                       [jsexpr->content-part (-> hash? content-part?)]))

;; Base struct — not exported directly; use type-specific constructors.
(struct content-part (type) #:transparent)

;; Text content part
(struct text-part content-part (text) #:transparent)

;; Tool-call content part
(struct tool-call-part content-part (id name arguments) #:transparent)

;; Tool-result content part
(struct tool-result-part content-part (tool-call-id content is-error?) #:transparent)

;; Image content part (v0.98.1: multimodal pipeline)
(struct image-part content-part (mime-type data detail) #:transparent)

;; Convenience constructors
(define (make-text-part text)
  (text-part "text" text))

(define (make-tool-call-part id name arguments)
  (tool-call-part "tool-call" id name arguments))

(define (make-tool-result-part tool-call-id content is-error?)
  (tool-result-part "tool-result" tool-call-id content is-error?))

(define (make-image-part mime-type data [detail #f])
  (image-part "image" mime-type data detail))

;; Serialization
(define (content-part->jsexpr cp)
  (cond
    [(text-part? cp) (hasheq 'type "text" 'text (text-part-text cp))]
    [(tool-call-part? cp)
     (hasheq 'type
             "tool-call"
             'id
             (tool-call-part-id cp)
             'name
             (tool-call-part-name cp)
             'arguments
             (tool-call-part-arguments cp))]
    [(tool-result-part? cp)
     (hasheq 'type
             "tool-result"
             'toolCallId
             (tool-result-part-tool-call-id cp)
             'content
             (tool-result-part-content cp)
             'isError
             (tool-result-part-is-error? cp))]
    [(image-part? cp)
     (hasheq 'type "image"
             'mimeType (image-part-mime-type cp)
             'data (image-part-data cp)
             'detail (image-part-detail cp))]
    [else (raise-arguments-error 'content-part->jsexpr "unknown content part type" "type" cp)]))

;; Deserialization
(define (jsexpr->content-part h)
  (define tp (hash-ref h 'type))
  (case tp
    [("text") (make-text-part (hash-ref h 'text))]
    [("tool-call") (make-tool-call-part (hash-ref h 'id) (hash-ref h 'name) (hash-ref h 'arguments))]
    [("tool-result")
     (make-tool-result-part (hash-ref h 'toolCallId) (hash-ref h 'content) (hash-ref h 'isError))]
    [("image")
     (make-image-part (hash-ref h 'mimeType) (hash-ref h 'data) (hash-ref h 'detail #f))]
    [else (raise-arguments-error 'jsexpr->content-part "unknown content part type" "type" tp)]))

;; Get the type tag from a content part (struct accessor from content-part)

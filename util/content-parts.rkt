#lang racket/base

;; util/content-parts.rkt — Content part structs for messages
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Text, tool-call, and tool-result content parts with JSON serialization.

(require racket/contract)

(provide (contract-out
          [text-part (-> string? string? text-part?)]
          [text-part? (-> any/c boolean?)]
          [text-part-text (-> text-part? string?)]
          [text-part-type (-> text-part? string?)]
          [tool-call-part
           (-> string? (or/c string? #f) (or/c string? #f) (or/c string? hash?) tool-call-part?)]
          [tool-call-part? (-> any/c boolean?)]
          [tool-call-part-id (-> tool-call-part? (or/c string? #f))]
          [tool-call-part-name (-> tool-call-part? (or/c string? #f))]
          [tool-call-part-arguments (-> tool-call-part? (or/c string? hash?))]
          [tool-call-part-type (-> tool-call-part? string?)]
          [tool-result-part (-> string? string? (or/c string? bytes?) any/c tool-result-part?)]
          [tool-result-part? (-> any/c boolean?)]
          [tool-result-part-tool-call-id (-> tool-result-part? string?)]
          [tool-result-part-content (-> tool-result-part? (or/c string? bytes?))]
          [tool-result-part-is-error? (-> tool-result-part? any/c)]
          [tool-result-part-type (-> tool-result-part? string?)]
          [content-part? (-> any/c boolean?)]
          [content-part-type (-> content-part? string?)]
          [make-text-part (-> string? text-part?)]
          [make-tool-call-part
           (-> (or/c string? #f) (or/c string? #f) (or/c string? hash?) tool-call-part?)]
          [make-tool-result-part (-> string? (or/c string? bytes?) any/c tool-result-part?)]
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

;; Wrapper accessors for inherited fields (struct-out creates these automatically,
;; but with explicit exports we must define them ourselves).
(define (text-part-type cp)
  (content-part-type cp))
(define (tool-call-part-type cp)
  (content-part-type cp))
(define (tool-result-part-type cp)
  (content-part-type cp))

;; Convenience constructors
(define (make-text-part text)
  (text-part "text" text))

(define (make-tool-call-part id name arguments)
  (tool-call-part "tool-call" id name arguments))

(define (make-tool-result-part tool-call-id content is-error?)
  (tool-result-part "tool-result" tool-call-id content is-error?))

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
    [else (raise-arguments-error 'content-part->jsexpr "unknown content part type" "type" cp)]))

;; Deserialization
(define (jsexpr->content-part h)
  (define tp (hash-ref h 'type))
  (case tp
    [("text") (make-text-part (hash-ref h 'text))]
    [("tool-call") (make-tool-call-part (hash-ref h 'id) (hash-ref h 'name) (hash-ref h 'arguments))]
    [("tool-result")
     (make-tool-result-part (hash-ref h 'toolCallId) (hash-ref h 'content) (hash-ref h 'isError))]
    [else (raise-arguments-error 'jsexpr->content-part "unknown content part type" "type" tp)]))

;; Get the type tag from a content part (struct accessor from content-part)

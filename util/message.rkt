#lang racket/base

;; util/message.rkt — Message struct and serialization
;;
;; Extracted from protocol-types.rkt (ARCH-05 split).
;; Message struct with JSON serialization/deserialization.

(require racket/contract
         "content-parts.rkt")

(provide (struct-out message)
         make-message
         message->jsexpr
         jsexpr->message)

;; ============================================================
;; Message struct
;; ============================================================

(struct message (id parent-id role kind content timestamp meta) #:transparent)

(define (make-message id parent-id role kind content timestamp meta)
  (message id parent-id role kind content timestamp meta))

;; Symbol -> string for JSON
(define (symbol->string* s)
  (if (symbol? s)
      (symbol->string s)
      s))

;; String -> symbol from JSON
(define (string->symbol* s)
  (if (string? s)
      (string->symbol s)
      s))

;; Serialize message to jsexpr (hash)
(define (message->jsexpr msg)
  (hasheq 'id
          (message-id msg)
          'parentId
          (message-parent-id msg)
          'role
          (symbol->string* (message-role msg))
          'kind
          (symbol->string* (message-kind msg))
          'content
          (map content-part->jsexpr (message-content msg))
          'timestamp
          (message-timestamp msg)
          'meta
          (message-meta msg)))

;; Deserialize jsexpr (hash) to message
(define (jsexpr->message h)
  (make-message (hash-ref h 'id)
                (hash-ref h 'parentId)
                (string->symbol* (hash-ref h 'role))
                (string->symbol* (hash-ref h 'kind))
                (map jsexpr->content-part (hash-ref h 'content))
                (hash-ref h 'timestamp)
                (hash-ref h 'meta)))

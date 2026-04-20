#lang racket/base

;; llm/model.rkt — normalized model request/response types
;;
;; Provides canonical structs for LLM interactions:
;;   - model-request: messages + tools + settings
;;   - model-response: content parts + usage + model + stop-reason
;;   - stream-chunk: delta-text/delta-tool-call/usage/done?
;;
;; All structs are transparent for debugging. JSON helpers provided
;; for serialization/deserialization.

(require racket/contract)

;; model-request
(provide (struct-out model-request)
         make-model-request
         model-request->jsexpr
         jsexpr->model-request

         ;; model-response
         (struct-out model-response)
         make-model-response
         model-response->jsexpr
         jsexpr->model-response

         ;; stream-chunk
         (struct-out stream-chunk)
         make-stream-chunk)

;; ============================================================
;; model-request
;; ============================================================

(struct model-request (messages tools settings) #:transparent)

(define (make-model-request messages tools settings)
  (model-request messages tools settings))

;; Serialize to jsexpr
(define (model-request->jsexpr req)
  (define h (hasheq 'messages (model-request-messages req) 'settings (model-request-settings req)))
  (if (model-request-tools req)
      (hash-set h 'tools (model-request-tools req))
      h))

;; Deserialize from jsexpr
(define (jsexpr->model-request h)
  (make-model-request (hash-ref h 'messages) (hash-ref h 'tools #f) (hash-ref h 'settings)))

;; ============================================================
;; model-response
;; ============================================================

(struct model-response (content usage model stop-reason) #:transparent)

(define (make-model-response content usage model stop-reason)
  (model-response content usage model stop-reason))

;; Serialize to jsexpr
(define (model-response->jsexpr resp)
  (hasheq 'content
          (model-response-content resp)
          'usage
          (model-response-usage resp)
          'model
          (model-response-model resp)
          'stopReason
          (symbol->string (model-response-stop-reason resp))))

;; Deserialize from jsexpr
(define (jsexpr->model-response h)
  (make-model-response (hash-ref h 'content)
                       (hash-ref h 'usage)
                       (hash-ref h 'model)
                       (string->symbol (hash-ref h 'stopReason))))

;; ============================================================
;; stream-chunk
;; ============================================================

(struct stream-chunk (delta-text delta-tool-call delta-thinking usage done?) #:transparent)

;; Convenience constructor with optional #:delta-thinking keyword.
;; v0.12.3 Wave 2.3: Added keyword arg so providers can pass thinking data
;; without needing the raw 6-field stream-chunk constructor.
(define (make-stream-chunk delta-text
                           delta-tool-call
                           usage
                           done?
                           #:delta-thinking [delta-thinking #f])
  (stream-chunk delta-text delta-tool-call delta-thinking usage done?))

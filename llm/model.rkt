#lang racket/base
;; STABILITY: public

;; llm/model.rkt — normalized model request/response types
;;
;; Provides canonical structs for LLM interactions:
;;   - model-request: messages + tools + settings
;;   - model-response: content parts + usage + model + stop-reason
;;   - stream-chunk: delta-text/delta-tool-call/usage/done?
;;   - tool-call-intent: unified AST for provider tool-call data
;;
;; MIGRATION NOTE (v0.85.1, C2-07):
;; Reverted from #lang typed/racket to #lang racket/base with explicit contracts.
;; Previous TR annotations used `Any` for most fields — zero compile-time safety.
;; Plain contracts provide better runtime guarantees and faster compilation.

(require racket/contract
         racket/match)

;; ============================================================
;; Contracts
;; ============================================================

(define message-list/c (listof any/c))
(define tool-list/c (or/c (listof any/c) #f))
(define settings/c (or/c hash? #f))
(define usage/c (or/c hash? #f))
(define stop-reason/c (or/c symbol? #f))
(define finish-reason/c (or/c string? symbol? #f))

;; ============================================================
;; model-request
;; ============================================================

(struct model-request (messages tools settings) #:transparent)

(define (make-model-request messages tools settings)
  (model-request messages tools settings))

(define (model-request->jsexpr req)
  (define h
    (hasheq 'messages
            (model-request-messages req)
            'settings
            (or (model-request-settings req) (hasheq))))
  (if (model-request-tools req)
      (hash-set h 'tools (model-request-tools req))
      h))

(define (jsexpr->model-request h)
  (define msgs (hash-ref h 'messages))
  (unless (list? msgs)
    (error 'jsexpr->model-request "messages must be a list, got: ~a" msgs))
  (define tools (hash-ref h 'tools #f))
  (unless (or (list? tools) (not tools))
    (error 'jsexpr->model-request "tools must be a list or #f, got: ~a" tools))
  (define settings (hash-ref h 'settings #f))
  (unless (or (hash? settings) (not settings))
    (error 'jsexpr->model-request "settings must be a hash or #f, got: ~a" settings))
  (make-model-request msgs tools settings))

;; ============================================================
;; model-response
;; ============================================================

(struct model-response (content usage model stop-reason) #:transparent)

(define (make-model-response content usage model stop-reason)
  (model-response content usage model stop-reason))

(define (model-response->jsexpr resp)
  (hasheq 'content
          (model-response-content resp)
          'usage
          (or (model-response-usage resp) (hasheq))
          'model
          (model-response-model resp)
          'stopReason
          (let ([sr (model-response-stop-reason resp)])
            (if sr
                (symbol->string sr)
                ""))))

(define (jsexpr->model-response h)
  (define content (hash-ref h 'content))
  (unless (list? content)
    (error 'jsexpr->model-response "content must be a list, got: ~a" content))
  (define model-val (hash-ref h 'model))
  (unless (string? model-val)
    (error 'jsexpr->model-response "model must be a string, got: ~a" model-val))
  (make-model-response content
                       (let ([u (hash-ref h 'usage #f)])
                         (if (hash? u)
                             u
                             (hasheq)))
                       model-val
                       (let ([sr (hash-ref h 'stopReason #f)])
                         (if sr
                             (string->symbol sr)
                             #f))))

;; ============================================================
;; stream-chunk
;; ============================================================

(struct stream-chunk (delta-text delta-tool-call delta-thinking usage done? finish-reason)
  #:transparent)

(define (make-stream-chunk delta-text
                           delta-tool-call
                           usage
                           done?
                           #:delta-thinking [delta-thinking #f]
                           #:finish-reason [finish-reason #f])
  (stream-chunk delta-text delta-tool-call delta-thinking usage done? finish-reason))

;; ============================================================
;; tool-call-intent — unified AST for provider tool-call data
;; ============================================================

(struct tool-call-intent (id name arguments) #:transparent)

(define (make-tool-call-intent id name arguments)
  (tool-call-intent id name arguments))

(define (tool-call-intent->hash tci)
  (hasheq 'type
          "tool-call"
          'id
          (tool-call-intent-id tci)
          'name
          (tool-call-intent-name tci)
          'arguments
          (tool-call-intent-arguments tci)))

(define (hash->tool-call-intent h)
  (define id0 (hash-ref h 'id #f))
  (define name0 (hash-ref h 'name #f))
  (define args0 (hash-ref h 'arguments #f))
  (tool-call-intent (if (string? id0) id0 "")
                    (if (string? name0) name0 "")
                    (if (hash? args0)
                        args0
                        (hasheq))))

;; ============================================================
;; Provide with contracts
;; ============================================================

;; Struct types (predicates + accessors)
(provide (struct-out model-request)
         (struct-out model-response)
         (struct-out stream-chunk)
         (struct-out tool-call-intent)

         ;; Constructors with contracts
         (contract-out [make-model-request (-> message-list/c tool-list/c settings/c model-request?)]
                       [model-request->jsexpr (-> model-request? hash?)]
                       [jsexpr->model-request (-> hash? model-request?)]
                       [make-model-response
                        (-> message-list/c usage/c string? stop-reason/c model-response?)]
                       [model-response->jsexpr (-> model-response? hash?)]
                       [jsexpr->model-response (-> hash? model-response?)]
                       [make-stream-chunk
                        (->* ((or/c string? #f) (or/c hash? #f) (or/c hash? #f) boolean?)
                             (#:delta-thinking (or/c string? #f) #:finish-reason finish-reason/c)
                             stream-chunk?)]
                       [make-tool-call-intent (-> string? string? hash? tool-call-intent?)]
                       [tool-call-intent->hash (-> tool-call-intent? hash?)]
                       [hash->tool-call-intent (-> hash? tool-call-intent?)]))

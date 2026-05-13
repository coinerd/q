#lang typed/racket

;; llm/model.rkt — normalized model request/response types
;;
;; Provides canonical structs for LLM interactions:
;;   - model-request: messages + tools + settings
;;   - model-response: content parts + usage + model + stop-reason
;;   - stream-chunk: delta-text/delta-tool-call/usage/done?
;;
;; TR BOUNDARY:
;; Migrated to #lang typed/racket in v0.29.3 W1.
;; Untyped consumers receive auto-generated contracts from TR boundary
;; system. Struct constructors enforce field types at call sites.

(provide (struct-out model-request)
         make-model-request
         model-request->jsexpr
         jsexpr->model-request

         (struct-out model-response)
         make-model-response
         model-response->jsexpr
         jsexpr->model-response

         (struct-out stream-chunk)
         make-stream-chunk)

;; ============================================================
;; model-request
;; ============================================================

(struct model-request
        ([messages : (Listof Any)] [tools : (Option (Listof Any))]
                                   [settings : (Option (HashTable Symbol Any))])
  #:transparent)

(: make-model-request
   ((Listof Any) (Option (Listof Any)) (Option (HashTable Symbol Any)) -> model-request))
(define (make-model-request messages tools settings)
  (model-request messages tools settings))

(: model-request->jsexpr (model-request -> (HashTable Symbol Any)))
(define (model-request->jsexpr req)
  (define h
    :
    (HashTable Symbol Any)
    (hasheq 'messages
            (model-request-messages req)
            'settings
            (or (model-request-settings req) (hasheq))))
  (if (model-request-tools req)
      (hash-set h 'tools (model-request-tools req))
      h))

(: jsexpr->model-request ((HashTable Symbol Any) -> model-request))
(define (jsexpr->model-request h)
  (define msgs (hash-ref h 'messages))
  (unless (list? msgs)
    (error 'jsexpr->model-request "messages must be a list, got: ~a" msgs))
  (define tools (hash-ref h 'tools #f))
  (unless (or (list? tools) (not tools))
    (error 'jsexpr->model-request "tools must be a list or #f, got: ~a" tools))
  (define settings (hash-ref h 'settings))
  (unless (hash? settings)
    (error 'jsexpr->model-request "settings must be a hash, got: ~a" settings))
  (make-model-request (cast msgs (Listof Any))
                      (cast tools (U (Listof Any) #f))
                      (cast settings (HashTable Symbol Any))))

;; ============================================================
;; model-response
;; ============================================================

(struct model-response
        ([content : (Listof Any)] [usage : (Option (HashTable Symbol Any))]
                                  [model : String]
                                  [stop-reason : (U Symbol #f)])
  #:transparent)

(: make-model-response
   ((Listof Any) (Option (HashTable Symbol Any)) String (U Symbol #f) -> model-response))
(define (make-model-response content usage model stop-reason)
  (model-response content usage model stop-reason))

(: model-response->jsexpr (model-response -> (HashTable Symbol Any)))
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

(: jsexpr->model-response ((HashTable Symbol Any) -> model-response))
(define (jsexpr->model-response h)
  (define content (hash-ref h 'content))
  (unless (list? content)
    (error 'jsexpr->model-response "content must be a list, got: ~a" content))
  (define model (hash-ref h 'model))
  (unless (string? model)
    (error 'jsexpr->model-response "model must be a string, got: ~a" model))
  (make-model-response (cast content (Listof Any))
                       (let ([u (hash-ref h 'usage #f)])
                         (if (hash? u) (cast u (HashTable Symbol Any)) (hasheq)))
                       (cast model String)
                       (let ([sr (hash-ref h 'stopReason #f)])
                         (if sr
                             (string->symbol (cast sr String))
                             #f))))

;; ============================================================
;; stream-chunk
;; ============================================================

(struct stream-chunk
        ([delta-text : (Option String)] [delta-tool-call : (Option (HashTable Symbol Any))]
                                        [delta-thinking : (Option String)]
                                        [usage : (Option (HashTable Symbol Any))]
                                        [done? : Boolean]
                                        [finish-reason : (Option (U String Symbol))])
  #:transparent)

(define make-stream-chunk
  (lambda ([delta-text : (Option String) #f]
           [delta-tool-call : (Option (HashTable Symbol Any)) #f]
           [usage : (Option (HashTable Symbol Any)) #f]
           [done? : Boolean #f]
           #:delta-thinking [delta-thinking : (Option String) #f]
           #:finish-reason [finish-reason : (Option (U String Symbol)) #f])
    (stream-chunk delta-text delta-tool-call delta-thinking usage done? finish-reason)))

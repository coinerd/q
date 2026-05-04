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
                                   [settings : (HashTable Symbol Any)])
  #:transparent)

(: make-model-request ((Listof Any) (Option (Listof Any)) (HashTable Symbol Any) -> model-request))
(define (make-model-request messages tools settings)
  (model-request messages tools settings))

(: model-request->jsexpr (model-request -> (HashTable Symbol Any)))
(define (model-request->jsexpr req)
  (define h
    :
    (HashTable Symbol Any)
    (hasheq 'messages (model-request-messages req) 'settings (model-request-settings req)))
  (if (model-request-tools req)
      (hash-set h 'tools (model-request-tools req))
      h))

(: jsexpr->model-request ((HashTable Symbol Any) -> model-request))
(define (jsexpr->model-request h)
  (make-model-request (cast (hash-ref h 'messages) (Listof Any))
                      (cast (hash-ref h 'tools #f) (U (Listof Any) #f))
                      (cast (hash-ref h 'settings) (HashTable Symbol Any))))

;; ============================================================
;; model-response
;; ============================================================

(struct model-response
        ([content : (Listof Any)] [usage : (HashTable Symbol Any)]
                                  [model : String]
                                  [stop-reason : (U Symbol #f)])
  #:transparent)

(: make-model-response ((Listof Any) (HashTable Symbol Any) String (U Symbol #f) -> model-response))
(define (make-model-response content usage model stop-reason)
  (model-response content usage model stop-reason))

(: model-response->jsexpr (model-response -> (HashTable Symbol Any)))
(define (model-response->jsexpr resp)
  (hasheq 'content
          (model-response-content resp)
          'usage
          (model-response-usage resp)
          'model
          (model-response-model resp)
          'stopReason
          (let ([sr (model-response-stop-reason resp)])
            (if sr
                (symbol->string sr)
                ""))))

(: jsexpr->model-response ((HashTable Symbol Any) -> model-response))
(define (jsexpr->model-response h)
  (make-model-response (cast (hash-ref h 'content) (Listof Any))
                       (cast (hash-ref h 'usage) (HashTable Symbol Any))
                       (cast (hash-ref h 'model) String)
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

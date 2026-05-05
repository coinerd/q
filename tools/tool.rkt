#lang racket/base
;; tools/tool.rkt — Tool contract, execution context, and registry facade
;; STABILITY: stable
;; v0.30.8: Decomposed into submodules (exec-context, registry, schema-helpers)
;;          W1 verified: all 44 consumers unaffected via re-export.
;;          This file re-exports everything for backward compatibility.

(require racket/contract
         racket/hash
         racket/set
         (only-in racket/string string-trim string-contains? string-join)
         json
         (only-in racket/base make-semaphore call-with-semaphore)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         (only-in "../util/errors.rkt" with-logged-catch)
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../runtime/settings.rkt" q-settings?)
         (only-in "../tools/permission-gate.rkt" permission-config?)
         (only-in "../util/protocol-types.rkt"
                  tool-call
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments
                  make-tool-call
                  tool-result
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?)
         ;; Submodule imports
         "tool-struct.rkt"
         "exec-context.rkt"
         "registry.rkt"
         "schema-helpers.rkt")

(provide (all-from-out "tool-struct.rkt")
         (contract-out [make-tool
                        (->* (string? string? hash? procedure?)
                             (#:prompt-snippet (or/c string? #f)
                                               #:render-call (or/c procedure? #f)
                                               #:render-result (or/c procedure? #f)
                                               #:prompt-guidelines (or/c string? #f))
                             tool?)]
                       [validate-tool-args (-> tool? hash? any/c)])
         merge-tool-lists
         validate-tool-schema
         format-tool-schema-hint

         ;; ── Tool result (re-exported from agent/types.rkt) ──
         tool-result?
         (contract-out [make-tool-result (-> any/c any/c any/c tool-result?)]
                       [make-error-result (-> string? tool-result?)]
                       [make-success-result (->* (any/c) (any/c) tool-result?)])
         tool-result-content
         tool-result-details
         tool-result-is-error?
         tool-result->jsexpr
         jsexpr->tool-result

         ;; ── Execution context (from exec-context.rkt) ──
         (contract-out [make-exec-context
                        (->* ()
                             (#:working-directory (or/c path-string? path? #f)
                                                  #:cancellation-token (or/c cancellation-token? #f)
                                                  #:event-publisher (or/c procedure? #f symbol?)
                                                  #:runtime-settings (or/c hash? q-settings? #f)
                                                  #:call-id string?
                                                  #:session-metadata (or/c hash? #f)
                                                  #:progress-callback (or/c procedure? #f)
                                                  #:permission-config (or/c permission-config? #f))
                             exec-context?)])
         exec-context?
         exec-context-working-directory
         exec-context-cancellation-token
         exec-context-event-publisher
         exec-context-runtime-settings
         exec-context-call-id
         exec-context-session-metadata
         exec-context-progress-callback
         exec-context-permission-config
         exec-context-bytes-written
         emit-progress!
         ;; ── Tool-call struct (re-exported from agent/types.rkt) ──
         tool-call
         tool-call?
         tool-call-id
         tool-call-name
         tool-call-arguments
         make-tool-call

         ;; ── Tool-call argument validation ──
         (all-from-out "../util/json-helpers.rkt")
         json-serializable?
         validate-tool-result

         ;; ── Tool registry (from registry.rkt) ──
         make-tool-registry
         tool-registry?
         (contract-out [register-tool! (-> tool-registry? tool? void?)]
                       [unregister-tool! (-> tool-registry? string? void?)]
                       [lookup-tool (-> tool-registry? (or/c string? #f) (or/c tool? #f))]
                       [list-tools (-> tool-registry? (listof tool?))]
                       [tool->jsexpr (-> tool? hash?)])
         set-active-tools!
         tool-active?
         list-active-tools
         list-active-tools-jsexpr
         list-tools-jsexpr
         tool-names)

;; ============================================================
;; Progress emission
;; ============================================================

(define (emit-progress! ctx percentage message)
  (define cb (exec-context-progress-callback ctx))
  exec-context-permission-config
  (when cb
    (cb percentage message)))

;; ============================================================
;; Tool constructor
;; ============================================================

(define (make-tool name
                   description
                   schema
                   execute
                   #:prompt-snippet [prompt-snippet #f]
                   #:render-call [render-call #f]
                   #:render-result [render-result #f]
                   #:prompt-guidelines [prompt-guidelines #f]
                   #:dangerous? [dangerous? #f])
  (unless (string? name)
    (raise-argument-error 'make-tool "string?" name))
  (unless (string? description)
    (raise-argument-error 'make-tool "string?" description))
  (unless (hash? schema)
    (raise-argument-error 'make-tool "hash?" schema))
  (unless (procedure? execute)
    (raise-argument-error 'make-tool "procedure?" execute))
  (tool name
        description
        schema
        execute
        prompt-snippet
        prompt-guidelines
        render-call
        render-result
        dangerous?))

;; ============================================================
;; JSON-serializability validation
;; ============================================================

(define (json-serializable? v)
  (with-logged-catch #f
                     (lambda ()
                       (jsexpr->string v)
                       #t)))

;; ============================================================
;; Tool result helpers (struct imported from agent/types.rkt)
;; ============================================================

(define (make-tool-result content details is-error)
  (tool-result content details is-error))

(define (tool-result->jsexpr tr)
  (hasheq 'content
          (tool-result-content tr)
          'details
          (tool-result-details tr)
          'isError
          (tool-result-is-error? tr)))

(define (jsexpr->tool-result h)
  (make-tool-result (hash-ref h 'content '())
                    (hash-ref h 'details (hasheq))
                    (hash-ref h 'isError #f)))

(define (make-error-result message)
  (make-tool-result (list (hasheq 'type "text" 'text message)) (hasheq) #t))

(define (make-success-result content [details (hasheq)])
  (unless (json-serializable? content)
    (raise-argument-error 'make-success-result "JSON-serializable content" content))
  (make-tool-result content details #f))

;; ============================================================
;; Tool result validation
;; ============================================================

(define (validate-tool-result v)
  (and (tool-result? v)
       (json-serializable? (tool-result-content v))
       (json-serializable? (tool-result-details v))
       v))

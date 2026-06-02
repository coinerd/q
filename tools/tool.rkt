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
         (only-in "../util/tool-types.rkt"
                  tool-call
                  tool-call?
                  tool-call-id
                  tool-call-name
                  tool-call-arguments
                  make-tool-call
                  tool-result
                  tool-result?
                  tool-result-content
                  tool-result-details)
         (only-in "../util/tool-types.rkt" tool-result-is-error?)
         ;; Submodule imports
         "tool-struct.rkt"
         "exec-context.rkt"
         "registry.rkt"
         "schema-helpers.rkt")

(provide tool?
         tool-name
         tool-description
         tool-schema
         tool-prompt-snippet
         tool-prompt-guidelines
         tool-dangerous?
         ;; Tool execution accessor (re-exported from tool-struct.rkt)
         ;; NOTE: tool-execute is for test/internal use only.
         ;;       Production code should use tools/scheduler.rkt for invocation.
         tool-execute
         tool-render-call
         tool-render-result
         tool-timeout-seconds
         (contract-out [make-tool
                        (->* (string? string? hash? procedure?)
                             (#:prompt-snippet (or/c string? #f)
                                               #:render-call (or/c procedure? #f)
                                               #:render-result (or/c procedure? #f)
                                               #:prompt-guidelines (or/c string? #f)
                                               #:dangerous? boolean?
                                               #:timeout-seconds (or/c exact-nonnegative-integer? #f))
                             tool?)]
                       [validate-tool-args (-> tool? hash? boolean?)])
         merge-tool-lists
         validate-tool-schema
         format-tool-schema-hint

         ;; ── Tool result (re-exported from agent/types.rkt) ──
         tool-result?
         (contract-out
          [make-tool-result (-> (or/c string? hash? list?) (or/c hash? #f) boolean? tool-result?)]
          [make-error-result (-> string? tool-result?)]
          [make-success-result (->* ((or/c string? hash? list?)) ((or/c hash? #f)) tool-result?)])
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
                              ;; event-publisher : (or/c #f (-> string? hash? any))
                              ;; Callback for scheduler lifecycle events. Receives event-type (string)
                              ;; and payload (hash). Called by execute-tool-plan for batch events.
                              #:event-publisher (or/c procedure? #f)
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
         ensure-hash-args
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
         tool-registry-tools
         list-active-tools
         list-active-tools-jsexpr
         list-tools-jsexpr
         tool-names)

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
                   #:dangerous? [dangerous? #f]
                   #:timeout-seconds [timeout-seconds #f])
  (unless (string? name)
    (raise-argument-error 'make-tool "string?" name))
  (unless (string? description)
    (raise-argument-error 'make-tool "string?" description))
  (unless (hash? schema)
    (raise-argument-error 'make-tool "hash?" schema))
  (unless (procedure? execute)
    (raise-argument-error 'make-tool "procedure?" execute))
  ;; W-16: Arity check -- validate handler accepts 1 or 2 args (args [exec-ctx])
  (unless (or (procedure-arity-includes? execute 1) (procedure-arity-includes? execute 2))
    (raise-arguments-error 'make-tool
                           (format "tool '~a' handler does not accept 1 or 2 args (args [exec-ctx])"
                                   name)))
  (tool name
        description
        schema
        execute
        prompt-snippet
        prompt-guidelines
        render-call
        render-result
        dangerous?
        timeout-seconds))

;; ============================================================
;; Progress emission
;; ============================================================

(define (emit-progress! ctx percentage message)
  (define cb (exec-context-progress-callback ctx))
  (when cb
    (cb percentage message)))

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

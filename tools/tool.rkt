#lang racket/base

(require racket/contract
         racket/hash
         racket/set
         (only-in racket/string string-trim)
         json
         ;; 3.2: Thread-safe registry access
         (only-in racket/base make-semaphore call-with-semaphore)
         (only-in "../util/json-helpers.rkt" ensure-hash-args)
         ;; ARCH-01: tool-call and tool-result structs from util/protocol-types.rkt
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
                  tool-result-is-error?))

;;; tools/tool.rkt — canonical tool contract, execution context,
;;;                    and tool registry.
;;;
;;; Exports:
;;;   - tool struct & helpers (make-tool, tool?, accessors, validate-tool-args)
;;;   - tool-result struct & helpers (re-exported from agent/types.rkt,
;;;                                   plus make-error-result, make-success-result, serialization)
;;;   - exec-context struct (execution context for tool invocations)
;;;   - tool-registry (register, lookup, list, unregister)
;;;
;;; ARCH-04 NOTE: At 373 lines this module is still cohesive. If tool contracts
;;; and execution contexts grow independently, consider splitting into
;;; tools/tool-contract.rkt (tool struct, validation) and
;;; tools/tool-registry.rkt (registry, lookup). Current size is manageable.

;; ── Tool struct ──
(provide (struct-out tool)
         tool?
         (contract-out [make-tool
                        (->* (string? string? hash? procedure?)
                             (#:prompt-snippet (or/c string? #f)
                                               #:render-call (or/c procedure? #f)
                                               #:render-result (or/c procedure? #f)
                                               #:prompt-guidelines (or/c string? #f))
                             tool?)]
                       [validate-tool-args (-> tool? hash? any/c)])
         tool-name
         tool-description
         tool-schema
         tool-execute
         tool-prompt-snippet
         tool-prompt-guidelines
         tool-dangerous?
         tool->jsexpr
         merge-tool-lists
         validate-tool-schema

         ;; ── Tool result (re-exported from agent/types.rkt) ──
         tool-result?
         (contract-out [make-tool-result (-> any/c any/c any/c tool-result?)]
                       [make-error-result (-> string? tool-result?)]
                       [make-success-result (->* (any/c) (any/c) tool-result?)])
         tool-result-content
         tool-result-details
         tool-result-is-error?
         tool-result->jsexpr ; reserved for SDK consumers
         jsexpr->tool-result ; reserved for SDK consumers

         ;; ── Execution context ──
         make-exec-context
         exec-context?
         exec-context-working-directory
         exec-context-cancellation-token
         exec-context-event-publisher
         exec-context-runtime-settings
         exec-context-call-id
         exec-context-session-metadata
         exec-context-progress-callback
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

         ;; ── Tool registry ──
         make-tool-registry
         tool-registry?
         register-tool!
         unregister-tool! ; reserved for SDK consumers
         set-active-tools!
         tool-active?
         list-active-tools
         list-active-tools-jsexpr
         lookup-tool
         list-tools
         list-tools-jsexpr
         tool-names)

;; ============================================================
;; Progress emission
;; ============================================================

;; emit-progress! calls the progress-callback if available.
;; Tools should call this to report incremental progress.
(define (emit-progress! ctx percentage message)
  (define cb (exec-context-progress-callback ctx))
  (when cb
    (cb percentage message)))

;; ============================================================
;; Tool struct
;; ============================================================

(struct tool
        (name description
              schema
              execute
              prompt-snippet
              prompt-guidelines
              render-call
              render-result
              dangerous?)
  #:transparent)

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
;; Tool schema validation (#672)
;; ============================================================

;; Validates that a tool schema conforms to OpenAI function-calling spec.
;; Returns #t if valid, raises exn:fail if not.
(define (validate-tool-schema schema)
  (unless (hash? schema)
    (raise-argument-error 'validate-tool-schema "hash?" schema))
  (unless (equal? (hash-ref schema 'type #f) "object")
    (raise (exn:fail (format "validate-tool-schema: 'type must be \"object\", got ~v"
                             (hash-ref schema 'type #f))
                     (current-continuation-marks))))
  (define props (hash-ref schema 'properties #f))
  (when props
    (unless (hash? props)
      (raise (exn:fail (format "validate-tool-schema: 'properties must be a hash, got ~v" props)
                       (current-continuation-marks)))))
  (define required (hash-ref schema 'required #f))
  (when required
    (unless (list? required)
      (raise (exn:fail (format "validate-tool-schema: 'required must be a list, got ~v" required)
                       (current-continuation-marks)))))
  #t)

;; ============================================================
;; Merge extension tools into LLM tool list (#673)
;; ============================================================

;; Merges a list of extension-provided tool jsexprs into the base tool list.
;; Extension tools override built-in tools with the same name.
(define (merge-tool-lists base-tools ext-tools)
  (define base-hash
    (for/hash ([t (in-list base-tools)])
      (values (hash-ref (hash-ref t 'function) 'name) t)))
  (define ext-hash
    (for/hash ([t (in-list ext-tools)])
      (values (hash-ref (hash-ref t 'function) 'name) t)))
  ;; Extension tools override base tools with same name
  (define merged
    (for/fold ([h base-hash]) ([(k v) (in-hash ext-hash)])
      (hash-set h k v)))
  ;; Preserve base order, append new extension tools at end
  (define base-names
    (for/list ([t (in-list base-tools)])
      (hash-ref (hash-ref t 'function) 'name)))
  (define ext-only-names (filter (lambda (n) (not (member n base-names))) (hash-keys ext-hash)))
  (append (for/list ([n (in-list base-names)])
            (hash-ref merged n))
          (for/list ([n (in-list ext-only-names)])
            (hash-ref merged n))))

;; ============================================================
;; JSON-serializability validation
;; ============================================================

;; Check whether a value is JSON-serializable.
;; Returns #t if jsexpr->string would succeed, #f otherwise.
(define (json-serializable? v)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (jsexpr->string v)
    #t))

;; ============================================================
;; Tool result helpers (struct imported from agent/types.rkt)
;; ============================================================

(define (make-tool-result content details is-error)
  (tool-result content details is-error))

;; --- JSON serialization ---

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

;; --- Convenience constructors ---

(define (make-error-result message)
  (make-tool-result (list (hasheq 'type "text" 'text message)) (hasheq) #t))

(define (make-success-result content [details (hasheq)])
  ;; Validate content is JSON-serializable before accepting
  (unless (json-serializable? content)
    (raise-argument-error 'make-success-result "JSON-serializable content" content))
  (make-tool-result content details #f))

;; ============================================================
;; Execution context
;; ============================================================

(struct exec-context
        (working-directory cancellation-token
                           event-publisher
                           runtime-settings
                           call-id
                           session-metadata
                           progress-callback)
  #:transparent)

(define (make-exec-context #:working-directory [working-directory (current-directory)]
                           #:cancellation-token [cancellation-token #f]
                           #:event-publisher [event-publisher #f]
                           #:runtime-settings [runtime-settings #f]
                           #:call-id [call-id ""]
                           #:session-metadata [session-metadata #f]
                           #:progress-callback [progress-callback #f])
  (exec-context working-directory
                cancellation-token
                event-publisher
                runtime-settings
                call-id
                session-metadata
                progress-callback))

;; ============================================================
;; Tool registry
;; ============================================================

(struct tool-registry (tools-box active-set-box sem) #:transparent)

(define (make-tool-registry)
  (tool-registry (make-hash) (box #f) (make-semaphore 1)))

(define (register-tool! reg t)
  (unless (tool? t)
    (raise-argument-error 'register-tool! "tool?" t))
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda ()
                         (define tbl (tool-registry-tools-box reg))
                         (define n (tool-name t))
                         (when (hash-has-key? tbl n)
                           (error 'register-tool! "tool already registered: ~a" n))
                         (hash-set! tbl n t))))

(define (unregister-tool! reg name)
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda () (hash-remove! (tool-registry-tools-box reg) name))))

(define (lookup-tool reg name)
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda () (hash-ref (tool-registry-tools-box reg) name #f))))

;; tool->jsexpr : tool? -> hash?
;; Serialize a tool struct to the OpenAI normalized format.
;; Output: {"type":"function","function":{"name":"...","description":"...","parameters":{...}}}
(define (tool->jsexpr t)
  (define base-fn
    (hasheq 'name (tool-name t) 'description (tool-description t) 'parameters (tool-schema t)))
  (define fn-with-snippet
    (if (tool-prompt-snippet t)
        (hash-set base-fn 'promptSnippet (tool-prompt-snippet t))
        base-fn))
  (define fn-with-guidelines
    (if (tool-prompt-guidelines t)
        (hash-set fn-with-snippet 'promptGuidelines (tool-prompt-guidelines t))
        fn-with-snippet))
  (hasheq 'type "function" 'function fn-with-guidelines))

;; list-tools-jsexpr : tool-registry? -> (listof hash?)
;; Return all registered tools serialized to the OpenAI normalized JSON format.
;; ── Active tool management ──

;; Set which tools are active. #f means all tools are active.
;; active-names is (or/c #f (listof string?))
(define (set-active-tools! reg active-names)
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda ()
                         (set-box! (tool-registry-active-set-box reg)
                                   (and active-names (list->set active-names))))))

;; Check if a tool is active.
(define (tool-active? reg name)
  (define active-set (unbox (tool-registry-active-set-box reg)))
  (or (not active-set) (set-member? active-set name)))

;; List only active tools.
(define (list-active-tools reg)
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda ()
                         (filter (lambda (t) (tool-active? reg (tool-name t)))
                                 (hash-values (tool-registry-tools-box reg))))))

;; List active tools in jsexpr format.
(define (list-active-tools-jsexpr reg)
  (map tool->jsexpr (list-active-tools reg)))

;; list-tools-jsexpr now respects the active set.
(define (list-tools-jsexpr reg)
  (map tool->jsexpr (list-active-tools reg)))

(define (list-tools reg)
  (call-with-semaphore (tool-registry-sem reg)
                       (lambda () (hash-values (tool-registry-tools-box reg)))))

(define (tool-names reg)
  (call-with-semaphore (tool-registry-sem reg) (lambda () (hash-keys (tool-registry-tools-box reg)))))

;; ============================================================
;; Tool-call argument validation (for post-processing)
;; ============================================================

;; Ensure that tool-call-part-arguments is a hash after processing.
;; This guards against raw strings or other types leaking through.
;; ensure-hash-args imported from util/json-helpers.rkt

;; ============================================================
;; Argument validation
;; ============================================================

(define (validate-tool-args t args)
  (unless (hash? args)
    (raise (exn:fail (format "validate-tool-args: args must be a hash, got ~a" args)
                     (current-continuation-marks))))
  (define schema (tool-schema t))
  ;; Only validate if schema declares required or properties
  (define required (hash-ref schema 'required #f))
  (define properties (hash-ref schema 'properties #f))
  ;; Check required keys
  (when (and required (list? required))
    (for ([key (in-list required)])
      (unless (hash-has-key? args
                             (if (string? key)
                                 (string->symbol key)
                                 key))
        (raise (exn:fail (format "validate-tool-args: missing required argument '~a' for tool '~a'"
                                 key
                                 (tool-name t))
                         (current-continuation-marks))))))
  ;; Check types for present keys
  (when (and properties (hash? properties))
    (for ([(arg-key arg-val) (in-hash args)])
      (define prop-spec
        (or (hash-ref properties arg-key #f) (hash-ref properties (symbol->string arg-key) #f)))
      (when prop-spec
        (define expected-type (hash-ref prop-spec 'type #f))
        (when expected-type
          (unless (type-matches? arg-val expected-type)
            (raise
             (exn:fail
              (format "validate-tool-args: argument '~a' expected type '~a', got ~v for tool '~a'"
                      arg-key
                      expected-type
                      arg-val
                      (tool-name t))
              (current-continuation-marks))))))))
  #t)

;; Basic type checking against JSON Schema type strings
(define (type-matches? v type-str)
  (case type-str
    [("string") (string? v)]
    [("integer") (exact-integer? v)]
    [("number") (real? v)]
    [("boolean") (boolean? v)]
    [("object") (hash? v)]
    [("array") (list? v)]
    [else #t])) ; unknown type spec -> pass

;; ============================================================
;; Tool result validation
;; ============================================================

;; Validate that a value is a proper tool-result with JSON-serializable content.
;; Returns the tool-result if valid, or #f if invalid.
(define (validate-tool-result v)
  (and (tool-result? v)
       (json-serializable? (tool-result-content v))
       (json-serializable? (tool-result-details v))
       v))

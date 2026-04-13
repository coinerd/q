#lang racket/base

(require racket/contract
         racket/hash
         (only-in racket/string string-trim)
         json
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

;; ── Tool struct ──
(provide (struct-out tool)
         tool?
         (contract-out [make-tool (-> string? string? hash? procedure? tool?)]
                       [validate-tool-args (-> tool? hash? any/c)])
         tool-name
         tool-description
         tool-schema
         tool-execute
         tool->jsexpr

         ;; ── Tool result (re-exported from agent/types.rkt) ──
         tool-result?
         (contract-out [make-tool-result (-> any/c any/c any/c tool-result?)]
                       [make-error-result (-> string? tool-result?)]
                       [make-success-result (->* (any/c) (any/c) tool-result?)])
         tool-result-content
         tool-result-details
         tool-result-is-error?
         tool-result->jsexpr          ; reserved for SDK consumers
         jsexpr->tool-result           ; reserved for SDK consumers

         ;; ── Execution context ──
         make-exec-context
         exec-context?
         exec-context-working-directory
         exec-context-cancellation-token
         exec-context-event-publisher
         exec-context-runtime-settings
         exec-context-call-id
         exec-context-session-metadata

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
         unregister-tool!             ; reserved for SDK consumers
         lookup-tool
         list-tools
         list-tools-jsexpr
         tool-names)

;; ============================================================
;; Tool struct
;; ============================================================

(struct tool (name description schema execute) #:transparent)

(define (make-tool name description schema execute)
  (unless (string? name)
    (raise-argument-error 'make-tool "string?" name))
  (unless (string? description)
    (raise-argument-error 'make-tool "string?" description))
  (unless (hash? schema)
    (raise-argument-error 'make-tool "hash?" schema))
  (unless (procedure? execute)
    (raise-argument-error 'make-tool "procedure?" execute))
  (tool name description schema execute))

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
                           session-metadata)
  #:transparent)

(define (make-exec-context #:working-directory [working-directory (current-directory)]
                           #:cancellation-token [cancellation-token #f]
                           #:event-publisher [event-publisher #f]
                           #:runtime-settings [runtime-settings #f]
                           #:call-id [call-id ""]
                           #:session-metadata [session-metadata #f])
  (exec-context working-directory
                cancellation-token
                event-publisher
                runtime-settings
                call-id
                session-metadata))

;; ============================================================
;; Tool registry
;; ============================================================

(struct tool-registry (tools-box) #:transparent)

(define (make-tool-registry)
  (tool-registry (make-hash)))

(define (register-tool! reg t)
  (unless (tool? t)
    (raise-argument-error 'register-tool! "tool?" t))
  (define tbl (tool-registry-tools-box reg))
  (define n (tool-name t))
  (when (hash-has-key? tbl n)
    (error 'register-tool! "tool already registered: ~a" n))
  (hash-set! tbl n t))

(define (unregister-tool! reg name)
  (define tbl (tool-registry-tools-box reg))
  (hash-remove! tbl name))

(define (lookup-tool reg name)
  (hash-ref (tool-registry-tools-box reg) name #f))

;; tool->jsexpr : tool? -> hash?
;; Serialize a tool struct to the OpenAI normalized format.
;; Output: {"type":"function","function":{"name":"...","description":"...","parameters":{...}}}
(define (tool->jsexpr t)
  (hasheq 'type
          "function"
          'function
          (hasheq 'name (tool-name t) 'description (tool-description t) 'parameters (tool-schema t))))

;; list-tools-jsexpr : tool-registry? -> (listof hash?)
;; Return all registered tools serialized to the OpenAI normalized JSON format.
(define (list-tools-jsexpr reg)
  (map tool->jsexpr (hash-values (tool-registry-tools-box reg))))

(define (list-tools reg)
  (hash-values (tool-registry-tools-box reg)))

(define (tool-names reg)
  (hash-keys (tool-registry-tools-box reg)))

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

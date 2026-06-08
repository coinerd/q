#lang racket/base
;; tools/builtins/memory-tools-shared.rkt — shared helpers for memory tool handlers
;;
;; Extracted from memory-tools.rkt (v0.96.2 F4).
;; Contains query helpers, event emitters, and utility functions
;; shared across store/query/manage handlers.

(require racket/path
         racket/string
         (only-in "../tool.rkt" make-error-result make-success-result)
         (only-in "../../runtime/memory/types.rkt"
                  memory-item
                  memory-item-id
                  memory-item-content
                  memory-item-type
                  memory-item-scope
                  memory-item-metadata
                  memory-item-validity
                  memory-item-created-at
                  memory-item-updated-at
                  memory-query
                  memory-result-ok?
                  memory-result-value
                  memory-result-error
                  memory-result-metadata
                  memory-type?)
         (only-in "../../runtime/memory/protocol.rkt"
                  memory-backend?
                  memory-backend-name
                  gen:store-memory!
                  gen:retrieve-memory
                  gen:update-memory!
                  gen:delete-memory!
                  gen:manage-memory!)
         (only-in "../../runtime/memory/policy.rkt"
                  default-memory-policy
                  effective-memory-scope
                  policy-allows-store?
                  policy-allows-retrieve?
                  policy-allows-delete?
                  policy-allows-scope?
                  memory-persistent-write-allowed?
                  redacted-memory-snippet)
         (only-in "../../runtime/memory/backends/helpers.rkt" current-iso-8601)
         (only-in "../../runtime/memory/service.rkt" current-memory-backend current-memory-policy)
         (only-in "../exec-context.rkt"
                  exec-context?
                  exec-context-working-directory
                  exec-context-session-metadata
                  exec-context-event-publisher))

(define (make-memory-id)
  (format "mem_~a_~a"
          (current-seconds)
          (modulo (inexact->exact (round (* 1000 (current-inexact-milliseconds)))) 1000000)))

;; F32: format-iso-now/pad2 removed — using current-iso-8601 from helpers.rkt
(define (format-iso-now)
  (current-iso-8601))

(define (memory-item-summary item)
  (hasheq
   'id
   (memory-item-id item)
   'type
   (memory-item-type item)
   'scope
   (memory-item-scope item)
   'content
   (substring (memory-item-content item) 0 (min 200 (string-length (memory-item-content item))))
   'tags
   (hash-ref (memory-item-metadata item) 'tags '())))

(define (format-item-line item)
  (format
   "  [~a] ~a/~a — \"~a\"~a"
   (memory-item-id item)
   (memory-item-type item)
   (memory-item-scope item)
   (substring (memory-item-content item) 0 (min 120 (string-length (memory-item-content item))))
   (let ([ts (hash-ref (memory-item-metadata item) 'tags '())])
     (if (pair? ts)
         (format " [tags: ~a]" (string-join (map (lambda (t) (format "~a" t)) ts) ", "))
         ""))))

(define (format-items-text items label)
  (if (null? items)
      (format "No ~a found." label)
      (format "~a ~a:\n~a" (length items) label (string-join (map format-item-line items) "\n"))))

(define (pathish->string v)
  (cond
    [(path? v) (path->string (simplify-path v))]
    [(string? v) (path->string (simplify-path (path->complete-path v)))]
    [else #f]))

(define (tool-project-root args exec-ctx)
  (or (and (exec-context? exec-ctx) (pathish->string (exec-context-working-directory exec-ctx)))
      (pathish->string (hash-ref args 'project-root #f))))

(define (tool-session-id args exec-ctx)
  (or (and (exec-context? exec-ctx)
           (let ([metadata (exec-context-session-metadata exec-ctx)])
             (and (hash? metadata) (hash-ref metadata 'session-id #f))))
      (let ([sid (hash-ref args 'session-id #f)]) (and (string? sid) sid))))

(define (parse-symbol-arg args key default)
  (define v (hash-ref args key default))
  (cond
    [(string? v) (string->symbol v)]
    [else v]))

(define (query-project-root-for-scope scope project-root)
  (if (eq? scope 'project) project-root #f))

(define (query-session-id-for-scope scope session-id)
  (if (eq? scope 'session) session-id #f))

(define (validate-scope policy scope)
  (cond
    [(not (memq scope '(session project user)))
     (format "Invalid scope: ~a. Must be session, project, or user." scope)]
    [(not (policy-allows-scope? policy scope))
     (format "Memory scope ~a is disabled by policy." scope)]
    [else #f]))

(define (validate-scope-context scope project-root session-id)
  (cond
    [(and (eq? scope 'project) (not project-root)) "Project-scoped memory requires a project root."]
    [(and (eq? scope 'session) (not session-id)) "Session-scoped memory requires a session id."]
    [else #f]))

(define (make-scoped-query text scope project-root session-id types tags limit include-expired?)
  (memory-query text
                scope
                (query-project-root-for-scope scope project-root)
                (query-session-id-for-scope scope session-id)
                types
                tags
                limit
                include-expired?))

(define (make-visible-query args
                            exec-ctx
                            #:default-limit [default-limit 5]
                            #:include-expired? [include-expired? #f])
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (define scope (effective-memory-scope (parse-symbol-arg args 'scope #f) project-root))
  (define raw-types (hash-ref args 'types #f))
  (define types
    (and raw-types
         (map (lambda (t)
                (if (string? t)
                    (string->symbol t)
                    t))
              raw-types)))
  (define tags (hash-ref args 'tags #f))
  (define limit (hash-ref args 'limit default-limit))
  (define query-text (hash-ref args 'query #f))
  (values
   scope
   project-root
   session-id
   types
   tags
   limit
   (make-scoped-query query-text scope project-root session-id types tags limit include-expired?)))

(define (backend-name backend)
  (if (memory-backend? backend)
      (memory-backend-name backend)
      "none"))

;; F29: Tools emit plain-hash events via the session event bus (not typed-event
;; structs). Typed events are used by auto-extraction for its separate callback.
;; This is by design — the session event bus expects jsexpr-safe plain hashes.
(define (publish-memory-event! exec-ctx event)
  (when (and (exec-context? exec-ctx) (exec-context-event-publisher exec-ctx))
    (with-handlers ([exn:fail? (lambda (e)
                                 ;; F31: Log instead of silently swallowing
                                 (fprintf (current-error-port)
                                          "memory event emission failed: ~a\n"
                                          (exn-message e)))])
      ((exec-context-event-publisher exec-ctx) (hash-ref event 'type "memory.unknown") event))))

(define (event-hash type fields)
  (hash-set fields 'type type))

(define (emit-backend-unavailable! exec-ctx backend action)
  (publish-memory-event! exec-ctx
                         (event-hash "memory.backend.unavailable"
                                     (hasheq 'backend (backend-name backend) 'action action))))

(define (emit-policy-blocked! exec-ctx action reason source content)
  (publish-memory-event! exec-ctx
                         (event-hash "memory.policy.blocked"
                                     (hasheq 'action
                                             action
                                             'reason
                                             reason
                                             'source
                                             source
                                             'redacted-snippet
                                             (redacted-memory-snippet content)))))

(define (emit-store-requested! exec-ctx id type-sym scope source)
  (publish-memory-event!
   exec-ctx
   (event-hash "memory.item.store.requested"
               (hasheq 'candidate-id id 'type type-sym 'scope scope 'source source))))

(define (emit-stored! exec-ctx item)
  (publish-memory-event! exec-ctx
                         (event-hash "memory.item.stored"
                                     (hasheq 'id
                                             (memory-item-id item)
                                             'type
                                             (memory-item-type item)
                                             'scope
                                             (memory-item-scope item)
                                             'source
                                             (hash-ref (memory-item-metadata item) 'source 'tool)
                                             'redacted-snippet
                                             (redacted-memory-snippet (memory-item-content item))))))

;; F25: Plain-hash events use lisp-case keys ('query-snippet, 'latency-ms,
;; 'result-count) for jsexpr compatibility. These map to the typed-event
;; struct field names and are consistent with SPEC §6.
(define (emit-retrieval! exec-ctx scope query-text limit count result)
  (publish-memory-event!
   exec-ctx
   (event-hash "memory.retrieval.performed"
               (hasheq 'scope
                       scope
                       'query-snippet
                       (redacted-memory-snippet (or query-text ""))
                       'limit
                       limit
                       'result-count
                       count
                       'latency-ms
                       (hash-ref (memory-result-metadata result) 'latency-ms 0)))))

(define (emit-deleted! exec-ctx id scope backend)
  (publish-memory-event! exec-ctx
                         (event-hash "memory.item.deleted"
                                     (hasheq 'id id 'scope scope 'backend (backend-name backend)))))

(define (memory-error-message result)
  (hash-ref (memory-result-error result) 'message "unknown error"))

(provide make-memory-id
         format-iso-now
         memory-item-summary
         format-item-line
         format-items-text
         pathish->string
         tool-project-root
         tool-session-id
         parse-symbol-arg
         query-project-root-for-scope
         query-session-id-for-scope
         validate-scope
         validate-scope-context
         make-scoped-query
         make-visible-query
         backend-name
         publish-memory-event!
         event-hash
         emit-backend-unavailable!
         emit-policy-blocked!
         emit-store-requested!
         emit-stored!
         emit-retrieval!
         emit-deleted!
         memory-error-message
         ;; Re-exports from dependencies
         current-memory-backend
         current-memory-policy
         effective-memory-scope
         memory-backend?
         memory-backend-name
         memory-item
         memory-item-id
         memory-item-content
         memory-item-type
         memory-item-scope
         memory-item-metadata
         memory-item-validity
         memory-item-created-at
         memory-item-updated-at
         memory-query
         memory-result-ok?
         memory-result-value
         memory-result-error
         memory-result-metadata
         memory-type?
         gen:store-memory!
         gen:retrieve-memory
         gen:update-memory!
         gen:delete-memory!
         gen:manage-memory!
         default-memory-policy
         policy-allows-store?
         policy-allows-retrieve?
         policy-allows-delete?
         policy-allows-scope?
         memory-persistent-write-allowed?
         redacted-memory-snippet
         current-iso-8601
         make-error-result
         make-success-result
         exec-context?
         exec-context-working-directory
         exec-context-session-metadata
         exec-context-event-publisher)
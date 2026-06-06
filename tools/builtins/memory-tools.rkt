#lang racket/base
;; tools/builtins/memory-tools.rkt — explicit modular memory tools

(require racket/path
         racket/string
         "../tool.rkt"
         "../define-tool.rkt"
         (only-in "../../runtime/memory/types.rkt"
                  memory-item
                  memory-item-id
                  memory-item-content
                  memory-item-type
                  memory-item-scope
                  memory-item-metadata
                  memory-item-updated-at
                  memory-query
                  memory-result-ok?
                  memory-result-value
                  memory-result-error
                  memory-result-metadata)
         (only-in "../../runtime/memory/protocol.rkt"
                  memory-backend?
                  memory-backend-name
                  gen:store-memory!
                  gen:retrieve-memory
                  gen:delete-memory!)
         (only-in "../../runtime/memory/policy.rkt"
                  default-memory-policy
                  effective-memory-scope
                  policy-allows-store?
                  policy-allows-retrieve?
                  policy-allows-delete?
                  policy-allows-scope?
                  memory-persistent-write-allowed?
                  redacted-memory-snippet))

(define current-memory-backend (make-parameter #f))
(define current-memory-policy (make-parameter default-memory-policy))

(define (make-memory-id)
  (format "mem_~a_~a"
          (current-seconds)
          (modulo (inexact->exact (round (* 1000 (current-inexact-milliseconds)))) 1000000)))

(define (format-iso-now)
  (define d (seconds->date (current-seconds) #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (format "~a" n)))

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
  (define types (hash-ref args 'types #f))
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

(define (publish-memory-event! exec-ctx event)
  (when (and (exec-context? exec-ctx) (exec-context-event-publisher exec-ctx))
    (with-handlers ([exn:fail? (lambda (_) (void))])
      ((exec-context-event-publisher exec-ctx) event))))

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
                       'count
                       count
                       'latency-ms
                       (hash-ref (memory-result-metadata result) 'latency-ms 0)))))

(define (emit-deleted! exec-ctx id scope backend)
  (publish-memory-event! exec-ctx
                         (event-hash "memory.item.deleted"
                                     (hasheq 'id id 'scope scope 'backend (backend-name backend)))))

(define (memory-error-message result)
  (hash-ref (memory-result-error result) 'message "unknown error"))

(define (store-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define content (hash-ref args 'content #f))
  (define type-sym (parse-symbol-arg args 'type 'semantic))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (define scope-sym (effective-memory-scope (parse-symbol-arg args 'scope #f) project-root))
  (define tags (hash-ref args 'tags '()))
  (define sensitivity (parse-symbol-arg args 'sensitivity 'public))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'store)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(not (memory-persistent-write-allowed?))
     (emit-policy-blocked! exec-ctx 'store 'safe-mode 'tool content)
     (make-error-result "Memory store blocked by safe mode.")]
    [(not (and content (string? content) (> (string-length content) 0)))
     (make-error-result "content is required and must be a non-empty string")]
    [(not (memq type-sym '(episodic semantic procedural)))
     (make-error-result (format "Invalid type: ~a. Must be episodic, semantic, or procedural."
                                type-sym))]
    [(validate-scope policy scope-sym)
     =>
     make-error-result]
    [(validate-scope-context scope-sym project-root session-id)
     =>
     make-error-result]
    [(not (memq sensitivity
                '(public internal
                         sensitive
                         secret)))
     (make-error-result
      (format "Invalid sensitivity: ~a. Must be public, internal, sensitive, or secret."
              sensitivity))]
    [else
     (define id (make-memory-id))
     (define now (format-iso-now))
     (emit-store-requested! exec-ctx id type-sym scope-sym 'tool)
     ;; Extract origin-message-id from exec-context if available (P2-2)
     (define origin-message-id
       (and (exec-context? exec-ctx)
            (let ([md (exec-context-session-metadata exec-ctx)])
              (and (hash? md) (hash-ref md 'message-id #f)))))
     (define item
       (memory-item id
                    type-sym
                    scope-sym
                    content
                    (hasheq 'tags
                            (if (list? tags)
                                tags
                                '())
                            'source
                            'tool
                            'project-root
                            project-root
                            'session-id
                            session-id
                            'origin-message-id
                            (or origin-message-id #f))
                    (hasheq 'sensitivity sensitivity 'confidence 1.0 'expires-at #f 'supersedes '())
                    now
                    now))
     (cond
       [(not (policy-allows-store? policy item))
        (emit-policy-blocked! exec-ctx 'store 'store-policy 'tool content)
        (make-error-result "Memory store blocked by policy.")]
       [else
        (define result (gen:store-memory! backend item))
        (if (memory-result-ok? result)
            (begin
              (emit-stored! exec-ctx item)
              (make-success-result (list (hasheq 'type
                                                 "text"
                                                 'text
                                                 (format "Memory stored: [~a/~a] ~a"
                                                         type-sym
                                                         scope-sym
                                                         (redacted-memory-snippet content))))
                                   (hasheq 'memory-id id)))
            (make-error-result (format "Failed to store memory: ~a"
                                       (memory-error-message result))))])]))

(define (search-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define query-text (hash-ref args 'query ""))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'search)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [else
     (define-values (scope project-root session-id types tags limit _query)
       (make-visible-query (hash-set args 'query query-text) exec-ctx #:default-limit 5))
     (cond
       [(validate-scope policy scope)
        =>
        make-error-result]
       [(validate-scope-context scope project-root session-id)
        =>
        make-error-result]
       [(not (policy-allows-retrieve? policy limit))
        (make-error-result (format "Requested limit ~a exceeds policy maximum" limit))]
       [else
        (define q (make-scoped-query query-text scope project-root session-id types tags limit #f))
        (define result (gen:retrieve-memory backend q))
        (if (memory-result-ok? result)
            (let ([items (memory-result-value result)])
              (emit-retrieval! exec-ctx scope query-text limit (length items) result)
              (make-success-result
               (list (hasheq 'type "text" 'text (format "Found ~a memories" (length items))))
               (hasheq 'items (map memory-item-summary items))))
            (make-error-result (format "Failed to search memory: ~a"
                                       (memory-error-message result))))])]))

(define (delete-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define id (hash-ref args 'id #f))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'delete)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(not id) (make-error-result "id is required")]
    [(not (policy-allows-delete? policy)) (make-error-result "Memory delete is disabled by policy")]
    [else
     (define-values (scope project-root session-id _types _tags _limit _query)
       (make-visible-query args exec-ctx #:default-limit 20 #:include-expired? #t))
     (cond
       [(validate-scope policy scope)
        =>
        make-error-result]
       [(validate-scope-context scope project-root session-id)
        =>
        make-error-result]
       [(not (policy-allows-retrieve? policy 20))
        (make-error-result "Delete preflight exceeds retrieve policy maximum.")]
       [else
        (define preflight-query (make-scoped-query #f scope project-root session-id #f #f 20 #t))
        (define preflight (gen:retrieve-memory backend preflight-query))
        (define visible-item
          (and (memory-result-ok? preflight)
               (for/first ([item (in-list (memory-result-value preflight))]
                           #:when (equal? id (memory-item-id item)))
                 item)))
        (cond
          [(not (memory-result-ok? preflight))
           (make-error-result (format "Failed to verify memory scope before delete: ~a"
                                      (memory-error-message preflight)))]
          [(not visible-item)
           (make-error-result "Memory not found in current scope/project/session.")]
          [else
           (define result (gen:delete-memory! backend id (memory-item-scope visible-item)))
           (if (memory-result-ok? result)
               (begin
                 (emit-deleted! exec-ctx id (memory-item-scope visible-item) backend)
                 (make-success-result
                  (list (hasheq 'type "text" 'text (format "Memory deleted: ~a" id)))))
               (make-error-result (format "Failed to delete memory: ~a"
                                          (memory-error-message result))))])])]))

(define (list-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'list)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [else
     (define-values (scope project-root session-id types _tags limit _query)
       (make-visible-query args exec-ctx #:default-limit 5))
     (cond
       [(validate-scope policy scope)
        =>
        make-error-result]
       [(validate-scope-context scope project-root session-id)
        =>
        make-error-result]
       [(not (policy-allows-retrieve? policy limit))
        (make-error-result (format "Requested limit ~a exceeds policy maximum" limit))]
       [else
        (define q (make-scoped-query #f scope project-root session-id types #f limit #f))
        (define result (gen:retrieve-memory backend q))
        (if (memory-result-ok? result)
            (let ([items (memory-result-value result)])
              (emit-retrieval! exec-ctx scope #f limit (length items) result)
              (define summaries
                (for/list ([item (in-list items)])
                  (hasheq 'id
                          (memory-item-id item)
                          'type
                          (memory-item-type item)
                          'scope
                          (memory-item-scope item)
                          'updated-at
                          (memory-item-updated-at item)
                          'snippet
                          (redacted-memory-snippet (memory-item-content item)))))
              (make-success-result
               (list (hasheq 'type "text" 'text (format "~a memory items found" (length items))))
               (hasheq 'items summaries)))
            (make-error-result (format "Failed to list memory: ~a"
                                       (memory-error-message result))))])]))

(define (clear-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define scope (parse-symbol-arg args 'scope #f))
  (define confirm (hash-ref args 'confirm #f))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'clear)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(not scope)
     (make-error-result "scope is required for clear_memory. Specify session, project, or user.")]
    [(validate-scope policy scope)
     =>
     make-error-result]
    [(validate-scope-context scope project-root session-id)
     =>
     make-error-result]
    [(not (equal? confirm #t))
     (make-error-result
      (format "clear_memory requires confirm=true. This will delete ALL memories in ~a scope."
              scope))]
    [(not (policy-allows-delete? policy)) (make-error-result "Memory delete is disabled by policy")]
    [(not (policy-allows-retrieve? policy 20))
     (make-error-result "clear_memory preflight exceeds retrieve policy maximum.")]
    [else
     ;; Loop until no items remain (P2-9)
     (define (clear-loop total-deleted)
       (define q (make-scoped-query #f scope project-root session-id #f #f 20 #t))
       (define result (gen:retrieve-memory backend q))
       (if (and (memory-result-ok? result) (> (length (memory-result-value result)) 0))
           (let* ([items (memory-result-value result)]
                  [ids (for/list ([item (in-list items)])
                         (gen:delete-memory! backend (memory-item-id item) scope)
                         (memory-item-id item))])
             (clear-loop (+ total-deleted (length ids))))
           total-deleted))
     (define total (clear-loop 0))
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Cleared ~a memory items from ~a scope" total scope)))
      (hasheq 'scope scope 'deleted-count total))]))

(define-tool
 store-memory
 #:description "Store a piece of information in memory for later retrieval."
 #:required ("content")
 #:properties
 [(content "string" "The information to store")
  (type "string" "Memory type: episodic, semantic, or procedural (default: semantic)")
  (scope
   "string"
   "Visibility: session, project, or user (default: project when project root exists, else session; user disabled by default)")
  (tags "array" "List of string tags for categorization")
  (sensitivity "string" "Sensitivity: public, internal, sensitive, secret (default: public)")]
 store-memory-handler)

(define-tool
 search-memory
 #:description
 "Search stored memories by query, scope, type, or tags. Defaults to project scope when project root exists, else session."
 #:required ("query")
 #:properties
 [(query "string" "Search query text")
  (scope "string" "Filter by scope: session, project, or user (user disabled by default)")
  (types "array" "Filter by memory types")
  (tags "array" "Filter by tags")
  (limit "integer" "Maximum results (default: 5, max: 20)")]
 search-memory-handler)

(define-tool delete-memory
             #:description "Delete a stored memory item by ID after scoped visibility preflight."
             #:required ("id")
             #:properties
             [(id "string" "The memory item ID to delete")
              (scope "string" "Scope of the item (defaults safely; user disabled by default)")]
             delete-memory-handler)

(define-tool
 list-memory
 #:description
 "List stored memories, optionally filtered by scope and type. Defaults to project scope when project root exists, else session."
 #:required ()
 #:properties
 [(scope "string" "Filter by scope: session, project, or user (user disabled by default)")
  (types "array" "Filter by memory types")
  (limit "integer" "Maximum results (default: 5, max: 20)")]
 list-memory-handler)

(define-tool
 clear-memory
 #:description
 "Delete ALL memories in a given scope. Destructive — requires confirm=true. Use list_memory first to review."
 #:required ("scope" "confirm")
 #:properties [(scope "string" "Scope to clear: session, project, or user (user disabled by default)")
               (confirm "boolean" "Must be true to confirm destructive operation")]
 clear-memory-handler)

(provide store-memory
         search-memory
         delete-memory
         list-memory
         clear-memory
         current-memory-backend
         current-memory-policy)

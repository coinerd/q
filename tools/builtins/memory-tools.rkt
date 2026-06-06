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
                  memory-item-validity
                  memory-item-created-at
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
         (only-in "../../runtime/memory/service.rkt" current-memory-backend current-memory-policy))

;; F1: current-memory-backend and current-memory-policy now live in
;; runtime/memory/service.rkt (the runtime-owned service boundary).
;; Tools import from service, not the other way around.

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
  ;; F4: Emit store.requested BEFORE any validation (SPEC §6).
  ;; F3: Generate single ID and reuse for both event and store.
  ;; F23: store.requested is emitted even when backend is #f (disabled) for
  ;; complete audit tracing. The subsequent backend-unavailable event explains
  ;; why the store did not proceed.
  (define store-candidate-id (make-memory-id))
  (emit-store-requested! exec-ctx store-candidate-id type-sym scope-sym 'tool)
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
    [(eq? sensitivity 'secret)
     ;; F4/F33: Reject secret sensitivity early before constructing item
     (make-error-result "Secret sensitivity is not allowed for memory storage.")]
    [(not (memq sensitivity
                '(public internal
                         sensitive)))
     (make-error-result (format "Invalid sensitivity: ~a. Must be public, internal, or sensitive."
                                sensitivity))]
    [else
     (define id store-candidate-id)
     (define now (format-iso-now))
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
                          'created-at ; F30: include both timestamps
                          (memory-item-created-at item)
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
     ;; Iterative loop until no items remain (P2-9), emit deleted events (F3)
     ;; M13-F5: Converted from recursion to explicit iterative loop
     (define total
       (let loop ([total-deleted 0])
         (define q (make-scoped-query #f scope project-root session-id #f #f 20 #t))
         (define result (gen:retrieve-memory backend q))
         (if (and (memory-result-ok? result) (> (length (memory-result-value result)) 0))
             (let* ([items (memory-result-value result)]
                    [batch-count (for/sum ([item (in-list items)])
                                          (gen:delete-memory! backend (memory-item-id item) scope)
                                          (emit-deleted! exec-ctx (memory-item-id item) scope backend)
                                          1)])
               (loop (+ total-deleted batch-count)))
             total-deleted)))
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Cleared ~a memory items from ~a scope" total scope)))
      (hasheq 'scope scope 'deleted-count total))]))

;; ---------------------------------------------------------------------------
;; M13-F5: update_memory tool
;; ---------------------------------------------------------------------------

(define (update-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define id (hash-ref args 'id #f))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (define scope (effective-memory-scope (parse-symbol-arg args 'scope #f) project-root))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'update)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(not id) (make-error-result "id is required")]
    [(not (memory-persistent-write-allowed?))
     (emit-policy-blocked! exec-ctx 'update 'safe-mode 'tool (hash-ref args 'content #f))
     (make-error-result "Memory update blocked by safe mode.")]
    [(validate-scope policy scope)
     =>
     make-error-result]
    [(validate-scope-context scope project-root session-id)
     =>
     make-error-result]
    [else
     ;; Scoped preflight: verify item exists and is visible in current scope
     (define preflight-query (make-scoped-query #f scope project-root session-id #f #f 20 #t))
     (define preflight (gen:retrieve-memory backend preflight-query))
     (define visible-item
       (and (memory-result-ok? preflight)
            (for/first ([item (in-list (memory-result-value preflight))]
                        #:when (equal? id (memory-item-id item)))
              item)))
     (cond
       [(not (memory-result-ok? preflight))
        (make-error-result (format "Failed to verify memory scope before update: ~a"
                                   (memory-error-message preflight)))]
       [(not visible-item) (make-error-result "Memory not found in current scope/project/session.")]
       [else
        ;; Build patch from provided args
        (define content (hash-ref args 'content #f))
        (define type-sym (parse-symbol-arg args 'type (memory-item-type visible-item)))
        (define tags (hash-ref args 'tags #f))
        (define sensitivity (parse-symbol-arg args 'sensitivity #f))
        (define supersedes (hash-ref args 'supersedes #f))
        (when (and sensitivity (eq? sensitivity 'secret))
          (make-error-result "Secret sensitivity is not allowed for memory storage."))
        (define patch
          (for/hash ([(k v) (in-hash
                             (hasheq 'content
                                     content
                                     'type
                                     (and content type-sym)
                                     'tags
                                     tags
                                     'validity
                                     (and (or sensitivity supersedes)
                                          (let ([base (memory-item-validity visible-item)])
                                            (cond
                                              [(and sensitivity supersedes)
                                               (hasheq 'sensitivity
                                                       sensitivity
                                                       'supersedes
                                                       (if (list? supersedes)
                                                           supersedes
                                                           '()))]
                                              [sensitivity (hash-set base 'sensitivity sensitivity)]
                                              [else
                                               (hash-set base
                                                         'supersedes
                                                         (if (list? supersedes)
                                                             supersedes
                                                             '()))])))
                                     'updated-at
                                     (format-iso-now)))]
                     #:when v)
            (values k v)))
        ;; Revalidate through policy
        (define updated-item
          (memory-item id
                       (if (hash-has-key? patch 'type)
                           (hash-ref patch 'type)
                           (memory-item-type visible-item))
                       (memory-item-scope visible-item)
                       (if (hash-has-key? patch 'content)
                           (hash-ref patch 'content)
                           (memory-item-content visible-item))
                       (if (hash-has-key? patch 'tags)
                           (hash-set (memory-item-metadata visible-item) 'tags (hash-ref patch 'tags))
                           (memory-item-metadata visible-item))
                       (if (hash-has-key? patch 'validity)
                           (hash-ref patch 'validity)
                           (memory-item-validity visible-item))
                       (memory-item-created-at visible-item)
                       (format-iso-now)))
        (cond
          [(not (policy-allows-store? policy updated-item))
           (emit-policy-blocked! exec-ctx 'update 'store-policy 'tool content)
           (make-error-result "Memory update blocked by policy.")]
          [else
           (define result (gen:update-memory! backend id patch))
           (if (memory-result-ok? result)
               (begin
                 (publish-memory-event! exec-ctx
                                        (event-hash "memory.item.updated"
                                                    (hasheq 'id
                                                            id
                                                            'scope
                                                            (memory-item-scope visible-item)
                                                            'fields-updated
                                                            (hash-keys patch))))
                 (make-success-result
                  (list (hasheq 'type "text" 'text (format "Memory updated: ~a" id)))
                  (hasheq 'memory-id id)))
               (make-error-result (format "Failed to update memory: ~a"
                                          (memory-error-message result))))])])]))

;; ---------------------------------------------------------------------------
;; M13-F5: cleanup_expired_memory management tool
;; ---------------------------------------------------------------------------

(define (cleanup-expired-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define scope (parse-symbol-arg args 'scope #f))
  (define dry-run (hash-ref args 'dry-run #f))
  (define confirm (hash-ref args 'confirm #f))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'cleanup)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(validate-scope policy scope)
     =>
     make-error-result]
    [(validate-scope-context scope project-root session-id)
     =>
     make-error-result]
    [else
     ;; Retrieve all items including expired
     (define q (make-scoped-query #f scope project-root session-id #f #f 100 #t))
     (define result (gen:retrieve-memory backend q))
     (cond
       [(not (memory-result-ok? result))
        (make-error-result (format "Failed to retrieve memories for cleanup: ~a"
                                   (memory-error-message result)))]
       [else
        (define all-items (memory-result-value result))
        (define expired-items
          (filter (lambda (item)
                    (define expires-at (hash-ref (memory-item-validity item) 'expires-at #f))
                    (and expires-at (string? expires-at) (string<? expires-at (format-iso-now))))
                  all-items))
        (cond
          [dry-run
           ;; Inspect only: report what would be deleted
           (make-success-result
            (list (hasheq
                   'type
                   "text"
                   'text
                   (format "Cleanup dry-run: ~a expired items found in ~a scope (out of ~a total)"
                           (length expired-items)
                           scope
                           (length all-items))))
            (hasheq 'scope
                    scope
                    'expired-count
                    (length expired-items)
                    'total-count
                    (length all-items)
                    'expired-ids
                    (map memory-item-id expired-items)))]
          [(not confirm)
           (make-error-result
            (format "cleanup_expired_memory requires confirm=true. ~a expired items would be deleted."
                    (length expired-items)))]
          [else
           ;; Delete expired items iteratively
           (define deleted-count
             (for/sum
              ([item (in-list expired-items)])
              (define r (gen:delete-memory! backend (memory-item-id item) (memory-item-scope item)))
              (when (memory-result-ok? r)
                (emit-deleted! exec-ctx (memory-item-id item) (memory-item-scope item) backend))
              (if (memory-result-ok? r) 1 0)))
           (make-success-result
            (list (hasheq
                   'type
                   "text"
                   'text
                   (format "Cleaned up ~a expired memory items from ~a scope" deleted-count scope)))
            (hasheq 'scope scope 'deleted-count deleted-count))])])]))

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
  (sensitivity "string" "Sensitivity: public, internal, or sensitive (default: public)")]
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

(define-tool
 update-memory
 #:description
 "Update an existing memory item by ID. Supports updating content, type, tags, sensitivity, and supersedes."
 #:required ("id")
 #:properties [(id "string" "The memory item ID to update")
               (content "string" "New content for the memory item")
               (type "string" "New type: episodic, semantic, or procedural")
               (tags "array" "New list of string tags")
               (sensitivity "string" "New sensitivity: public, internal, or sensitive")
               (supersedes "array" "List of memory IDs this item supersedes")
               (scope "string" "Scope context for visibility check")]
 update-memory-handler)

(define-tool
 cleanup-expired-memory
 #:description
 "Clean up expired memory items. Use dry-run=true first to inspect, then confirm=true to delete."
 #:required ("scope")
 #:properties [(scope "string" "Scope to clean: session, project, or user")
               (dry-run "boolean" "If true, only report what would be deleted (no deletion)")
               (confirm "boolean" "Must be true to confirm deletion of expired items")]
 cleanup-expired-handler)

(provide store-memory
         search-memory
         delete-memory
         list-memory
         clear-memory
         update-memory
         cleanup-expired-memory)

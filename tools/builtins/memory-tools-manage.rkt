#lang racket/base
;; tools/builtins/memory-tools-manage.rkt — manage memory handlers (delete, clear, consolidate, cleanup)
;;
;; Part of memory-tools decomposition (v0.96.2 F4).
;; Imports shared helpers from memory-tools-shared.rkt.

(require racket/string
         "memory-tools-shared.rkt")

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

(define (consolidate-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define ids (hash-ref args 'ids #f))
  (define merged-content (hash-ref args 'merged-content #f))
  (define scope (parse-symbol-arg args 'scope 'session))
  (define target-type (parse-symbol-arg args 'target-type #f))
  (define target-scope (parse-symbol-arg args 'target-scope #f))
  (define keep-originals? (hash-ref args 'keep-originals? #t))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'consolidate)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [(not (and ids (list? ids) (>= (length ids) 2)))
     (make-error-result "ids must be a list of at least 2 memory item IDs.")]
    [(not (and merged-content (string? merged-content) (> (string-length merged-content) 0)))
     (make-error-result "merged-content must be a non-empty string.")]
    [(validate-scope policy scope)
     =>
     make-error-result]
    [else
     (define effective-scope (or target-scope scope))
     (define effective-type (or target-type 'semantic))
     ;; v0.95.18 F6: Validate target type before storing
     (cond
       [(and target-type (not (memory-type? effective-type)))
        (make-error-result
         (format "Invalid target-type '~a'. Must be one of: semantic, episodic, procedural"
                 effective-type))]
       [else
        ;; Retrieve all items in scope to verify IDs exist
        (define q (make-scoped-query #f scope project-root session-id #f #f 100 #f))
        (define result (gen:retrieve-memory backend q))
        (cond
          [(not (memory-result-ok? result))
           (make-error-result (format "Failed to retrieve memories: ~a"
                                      (memory-error-message result)))]
          [else
           (define all-items (memory-result-value result))
           (define id-set (list->set ids))
           (define found-items
             (filter (lambda (item) (set-member? id-set (memory-item-id item))) all-items))
           (cond
             [(< (length found-items) (length ids))
              (define found-ids (map memory-item-id found-items))
              (define missing (filter (lambda (id) (not (member id found-ids))) ids))
              (make-error-result (format "IDs not found in scope: ~a" missing))]
             [else
              ;; Create merged item with supersedes metadata
              (define merged-id (make-memory-id))
              (define now-ts (format-iso-now))
              (define merged-item
                (memory-item merged-id
                             effective-type
                             effective-scope
                             merged-content
                             (hash 'source
                                   'consolidation
                                   'session-id
                                   (or session-id "unknown")
                                   'project-root
                                   (or project-root ".")
                                   'tags
                                   '()
                                   'origin-tool-call-id
                                   "consolidate-memory")
                             (hash 'sensitivity 'public 'confidence 0.7 'supersedes ids)
                             now-ts
                             now-ts))
              (define store-result (gen:store-memory! backend merged-item))
              (cond
                [(not (memory-result-ok? store-result))
                 (make-error-result (format "Failed to store merged item: ~a"
                                            (memory-error-message store-result)))]
                [else
                 ;; v0.95.18 F6: Supersede originals with lineage metadata
                 ;; instead of silent deletion
                 (define source-updates
                   (if keep-originals?
                       '()
                       (for/list ([item (in-list found-items)])
                         (define update-validity
                           (hash-set (memory-item-validity item) 'superseded-by merged-id))
                         (define update-patch (hasheq 'validity update-validity 'updated-at now-ts))
                         (define update-res
                           (gen:update-memory! backend (memory-item-id item) update-patch))
                         (hash 'id
                               (memory-item-id item)
                               'update-ok?
                               (memory-result-ok? update-res)
                               'superseded-by
                               merged-id))))
                 (define all-updates-ok?
                   (or keep-originals?
                       (andmap (lambda (u) (hash-ref u 'update-ok? #f)) source-updates)))
                 (define result-text
                   (if keep-originals?
                       (format "Consolidated ~a items into ~a (originals kept)"
                               (length ids)
                               merged-id)
                       (if all-updates-ok?
                           (format "Consolidated ~a items into ~a (originals superseded)"
                                   (length ids)
                                   merged-id)
                           (format
                            "Consolidated ~a items into ~a (partial: some supersessions failed)"
                            (length ids)
                            merged-id))))
                 (make-success-result result-text
                                      (hasheq 'merged-id
                                              merged-id
                                              'supersedes
                                              ids
                                              'keep-originals?
                                              keep-originals?
                                              'source-updates
                                              source-updates))])])])])]))

(define (list->set lst)
  (for/hash ([v (in-list lst)])
    (values v #t)))

(define (set-member? st v)
  (hash-has-key? st v))

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

(provide delete-memory-handler
         clear-memory-handler
         consolidate-memory-handler
         cleanup-expired-handler)

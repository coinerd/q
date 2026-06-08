#lang racket/base
;; tools/builtins/memory-tools-store.rkt — store and update memory handlers
;;
;; Part of memory-tools decomposition (v0.96.2 F4).
;; Imports shared helpers from memory-tools-shared.rkt.

(require racket/string
         "memory-tools-shared.rkt")

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
        (define content (hash-ref args 'content #f))
        (define type-sym (parse-symbol-arg args 'type (memory-item-type visible-item)))
        (define tags (hash-ref args 'tags #f))
        (define sensitivity (parse-symbol-arg args 'sensitivity #f))
        (define supersedes (hash-ref args 'supersedes #f))
        (if (and sensitivity (eq? sensitivity 'secret))
            (make-error-result "Secret sensitivity is not allowed for memory storage.")
            (let* ([base-validity (memory-item-validity visible-item)]
                   [with-sensitivity (if sensitivity
                                         (hash-set base-validity 'sensitivity sensitivity)
                                         base-validity)]
                   [new-validity (and (or sensitivity supersedes)
                                      (if supersedes
                                          (hash-set with-sensitivity
                                                    'supersedes
                                                    (if (list? supersedes)
                                                        supersedes
                                                        '()))
                                          with-sensitivity))]
                   [patch (for/hash ([(k v) (in-hash (hasheq 'content
                                                             content
                                                             'type
                                                             (and content type-sym)
                                                             'tags
                                                             tags
                                                             'validity
                                                             new-validity
                                                             'updated-at
                                                             (format-iso-now)))]
                                     #:when v)
                            (values k v))]
                   [updated-item (memory-item id
                                              (if (hash-has-key? patch 'type)
                                                  (hash-ref patch 'type)
                                                  (memory-item-type visible-item))
                                              (memory-item-scope visible-item)
                                              (if (hash-has-key? patch 'content)
                                                  (hash-ref patch 'content)
                                                  (memory-item-content visible-item))
                                              (if (hash-has-key? patch 'tags)
                                                  (hash-set (memory-item-metadata visible-item)
                                                            'tags
                                                            (hash-ref patch 'tags))
                                                  (memory-item-metadata visible-item))
                                              (if (hash-has-key? patch 'validity)
                                                  (hash-ref patch 'validity)
                                                  (memory-item-validity visible-item))
                                              (memory-item-created-at visible-item)
                                              (format-iso-now))])
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
                                                (memory-error-message result))))])))])]))

;; ---------------------------------------------------------------------------
;; M14: consolidate_memory tool — merge multiple items into one
;; ---------------------------------------------------------------------------

(provide store-memory-handler
         update-memory-handler)

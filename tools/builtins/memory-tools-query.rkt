#lang racket/base
;; tools/builtins/memory-tools-query.rkt — search and list memory handlers
;;
;; Part of memory-tools decomposition (v0.96.2 F4).
;; Imports shared helpers from memory-tools-shared.rkt.

(require racket/list
         racket/string
         "memory-tools-shared.rkt")

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
               (list (hasheq 'type "text" 'text (format-items-text items "memories")))
               (hasheq 'items (map memory-item-summary items))))
            (make-error-result (format "Failed to search memory: ~a"
                                       (memory-error-message result))))])]))

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
               (list (hasheq 'type "text" 'text (format-items-text items "memory items")))
               (hasheq 'items summaries)))
            (make-error-result (format "Failed to list memory: ~a"
                                       (memory-error-message result))))])]))

;; ---------------------------------------------------------------------------
;; W2 v0.99.59: memory-inventory — cross-scope item counts
;; ---------------------------------------------------------------------------

(define (memory-inventory-handler args [exec-ctx #f])
  ;; Returns count of memory items per scope, across all accessible scopes.
  ;; This is a lightweight read-only operation — queries each scope with limit 1
  ;; to get rough counts without loading full item data.
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define project-root (tool-project-root args exec-ctx))
  (define session-id (tool-session-id args exec-ctx))
  (cond
    [(not backend)
     (emit-backend-unavailable! exec-ctx backend 'inventory)
     (make-error-result "Memory not available. Enable memory in session config.")]
    [else
     ;; Query each scope to get approximate counts
     (define scopes-to-query
       (filter (lambda (s)
                 (and (not (validate-scope policy s))
                      (not (validate-scope-context s project-root session-id))))
               '(session project user)))
     (define counts
       (for/list ([scope (in-list scopes-to-query)])
         (define q (make-scoped-query #f scope project-root session-id #f #f 1000 #f))
         ;; v0.99.59 W2: Use gen:retrieve-memory with a high limit to get all items,
         ;; then count them. The limit is a soft cap — backend returns up to that many.
         (define result (gen:retrieve-memory backend q))
         (define count
           (if (memory-result-ok? result)
               (length (memory-result-value result))
               0))
         (list scope count)))
     (define total (apply + (map second counts)))
     (define summary
       (for/list ([entry (in-list counts)])
         (format "~a: ~a" (car entry) (cadr entry))))
     (make-success-result (list (hasheq 'type
                                        "text"
                                        'text
                                        (format "Memory Inventory: ~a total items across ~a scopes:~a"
                                                total
                                                (length counts)
                                                (string-append "
" (string-join summary "
")))))
                          (hasheq 'total
                                  total
                                  'scopes
                                  (for/hash ([entry (in-list counts)])
                                    (values (car entry) (cadr entry)))))]))

(provide search-memory-handler
         list-memory-handler
         memory-inventory-handler)

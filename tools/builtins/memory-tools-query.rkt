#lang racket/base
;; tools/builtins/memory-tools-query.rkt — search and list memory handlers
;;
;; Part of memory-tools decomposition (v0.96.2 F4).
;; Imports shared helpers from memory-tools-shared.rkt.

(require racket/string
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

(provide search-memory-handler
         list-memory-handler)

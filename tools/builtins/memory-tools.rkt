#lang racket/base
;; tools/builtins/memory-tools.rkt — store_memory, search_memory, delete_memory tools
;;
;; Layer: tools (built-in)
;; Purpose: Explicit memory tools for the agent. Gated by memory policy.
;;   - store_memory: stores a memory item if policy allows
;;   - search_memory: retrieves matching memory items
;;   - delete_memory: removes a memory item by ID
;;   - All return error if memory not configured (current-memory-backend = #f)

(require racket/string
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
                  valid-memory-item?)
         (only-in "../../runtime/memory/protocol.rkt"
                  memory-backend?
                  gen:store-memory!
                  gen:retrieve-memory
                  gen:delete-memory!
                  gen:memory-available?)
         (only-in "../../runtime/memory/policy.rkt"
                  default-memory-policy
                  policy-allows-store?
                  policy-allows-retrieve?
                  policy-allows-delete?))

;; ---------------------------------------------------------------------------
;; Runtime parameter — set by session when memory is enabled
;; ---------------------------------------------------------------------------

(define current-memory-backend (make-parameter #f))
(define current-memory-policy (make-parameter default-memory-policy))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-memory-id)
  (format "mem_~a_~a"
          (current-seconds)
          (modulo (inexact->exact (round (* 1000 (current-inexact-milliseconds)))) 1000000)))

(define (format-iso-now)
  (define ts (current-seconds))
  (define d (seconds->date ts #f))
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

;; ---------------------------------------------------------------------------
;; store_memory handler
;; ---------------------------------------------------------------------------

(define (store-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define content (hash-ref args 'content #f))
  (define type-sym
    (if (string? (hash-ref args 'type #f))
        (string->symbol (hash-ref args 'type))
        (hash-ref args 'type 'semantic)))
  (define scope-sym
    (if (string? (hash-ref args 'scope #f))
        (string->symbol (hash-ref args 'scope))
        (hash-ref args 'scope 'session)))
  (define tags (hash-ref args 'tags '()))
  (define sensitivity
    (if (string? (hash-ref args 'sensitivity #f))
        (string->symbol (hash-ref args 'sensitivity))
        (hash-ref args 'sensitivity 'public)))

  (cond
    [(not backend) (make-error-result "Memory not available. Enable memory in session config.")]
    [(not (and content (string? content) (> (string-length content) 0)))
     (make-error-result "content is required and must be a non-empty string")]
    [(not (memq type-sym '(episodic semantic procedural)))
     (make-error-result (format "Invalid type: ~a. Must be episodic, semantic, or procedural."
                                type-sym))]
    [(not (memq scope-sym '(session project user)))
     (make-error-result (format "Invalid scope: ~a. Must be session, project, or user." scope-sym))]
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
                            'tool)
                    (hasheq 'sensitivity sensitivity 'confidence 1.0)
                    now
                    now))
     (cond
       [(not (policy-allows-store? policy item))
        (make-error-result "Memory store blocked by policy.")]
       [else
        (define result (gen:store-memory! backend item))
        (if (memory-result-ok? result)
            (make-success-result
             (list (hasheq 'type
                           "text"
                           'text
                           (format "Memory stored: [~a/~a] ~a"
                                   type-sym
                                   scope-sym
                                   (substring content 0 (min 80 (string-length content))))))
             (hasheq 'memory-id id))
            (make-error-result
             (format "Failed to store memory: ~a"
                     (hash-ref (memory-result-error result) 'message "unknown error"))))])]))

;; ---------------------------------------------------------------------------
;; search_memory handler
;; ---------------------------------------------------------------------------

(define (search-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define query-text (hash-ref args 'query ""))
  (define scope
    (let ([s (hash-ref args 'scope #f)])
      (if (string? s)
          (string->symbol s)
          s)))
  (define types (hash-ref args 'types #f))
  (define tags (hash-ref args 'tags #f))
  (define limit (hash-ref args 'limit 10))

  (cond
    [(not backend) (make-error-result "Memory not available. Enable memory in session config.")]
    [(not (policy-allows-retrieve? policy limit))
     (make-error-result (format "Requested limit ~a exceeds policy maximum" limit))]
    [else
     (define q (memory-query query-text scope #f #f types tags limit #f))
     (define result (gen:retrieve-memory backend q))
     (if (memory-result-ok? result)
         (let ([items (memory-result-value result)])
           (make-success-result
            (list (hasheq 'type "text" 'text (format "Found ~a memories" (length items))))
            (hasheq 'items (map memory-item-summary items))))
         (make-error-result
          (format "Failed to search memory: ~a"
                  (hash-ref (memory-result-error result) 'message "unknown error"))))]))

;; ---------------------------------------------------------------------------
;; delete_memory handler
;; ---------------------------------------------------------------------------

(define (delete-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define id (hash-ref args 'id #f))
  (define scope
    (let ([s (hash-ref args 'scope #f)])
      (if (string? s)
          (string->symbol s)
          s)))

  (cond
    [(not backend) (make-error-result "Memory not available. Enable memory in session config.")]
    [(not id) (make-error-result "id is required")]
    [(not (policy-allows-delete? policy)) (make-error-result "Memory delete is disabled by policy")]
    [else
     (define result (gen:delete-memory! backend id scope))
     (if (memory-result-ok? result)
         (make-success-result (list (hasheq 'type "text" 'text (format "Memory deleted: ~a" id))))
         (make-error-result
          (format "Failed to delete memory: ~a"
                  (hash-ref (memory-result-error result) 'message "unknown error"))))]))

;; ---------------------------------------------------------------------------
;; Tool definitions via define-tool macro
;; ---------------------------------------------------------------------------

(define-tool
 store-memory
 #:description
 "Store a piece of information in memory for later retrieval. Use to remember facts, decisions, patterns, or observations that may be useful in future turns or sessions."
 #:required ("content")
 #:properties
 [(content "string" "The information to store")
  (type "string" "Memory type: episodic, semantic, or procedural (default: semantic)")
  (scope "string" "Visibility: session, project, or user (default: session)")
  (tags "array" "List of string tags for categorization")
  (sensitivity "string" "Sensitivity: public, internal, sensitive, secret (default: public)")]
 store-memory-handler)

(define-tool
 search-memory
 #:description
 "Search stored memories by query, scope, type, or tags. Returns matching items ranked by recency."
 #:required ()
 #:properties [(query "string" "Search query text")
               (scope "string" "Filter by scope: session, project, or user")
               (types "array" "Filter by memory types")
               (tags "array" "Filter by tags")
               (limit "integer" "Maximum results (default: 10, max: 20)")]
 search-memory-handler)

(define-tool delete-memory
             #:description "Delete a stored memory item by ID. Requires matching scope for safety."
             #:required ("id")
             #:properties [(id "string" "The memory item ID to delete")
                           (scope "string" "Scope of the item (for safety check)")]
             delete-memory-handler)

;; ---------------------------------------------------------------------------
;; list_memory handler
;; ---------------------------------------------------------------------------

(define (list-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define scope
    (let ([s (hash-ref args 'scope #f)])
      (if (string? s)
          (string->symbol s)
          s)))
  (define types (hash-ref args 'types #f))
  (define limit (hash-ref args 'limit 50))

  (cond
    [(not backend) (make-error-result "Memory not available. Enable memory in session config.")]
    [else
     (define q (memory-query #f scope #f #f types #f limit #f))
     (define result (gen:retrieve-memory backend q))
     (if (memory-result-ok? result)
         (let ([items (memory-result-value result)])
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
                       (let ([c (memory-item-content item)])
                         (substring c 0 (min 80 (string-length c)))))))
           (make-success-result
            (list (hasheq 'type "text" 'text (format "~a memory items found" (length items))))
            (hasheq 'items summaries)))
         (make-error-result
          (format "Failed to list memory: ~a"
                  (hash-ref (memory-result-error result) 'message "unknown error"))))]))

;; ---------------------------------------------------------------------------
;; clear_memory handler
;; ---------------------------------------------------------------------------

(define (clear-memory-handler args [exec-ctx #f])
  (define backend (current-memory-backend))
  (define policy (current-memory-policy))
  (define scope
    (let ([s (hash-ref args 'scope #f)])
      (if (string? s)
          (string->symbol s)
          s)))
  (define confirm (hash-ref args 'confirm #f))

  (cond
    [(not backend) (make-error-result "Memory not available. Enable memory in session config.")]
    [(not scope)
     (make-error-result "scope is required for clear_memory. Specify session, project, or user.")]
    [(not (memq scope '(session project user)))
     (make-error-result (format "Invalid scope: ~a. Must be session, project, or user." scope))]
    [(not (equal? confirm #t))
     (make-error-result
      (format "clear_memory requires confirm=true. This will delete ALL memories in ~a scope."
              scope))]
    [(not (policy-allows-delete? policy)) (make-error-result "Memory delete is disabled by policy")]
    [else
     ;; Retrieve all items in scope, then delete each
     (define q (memory-query #f scope #f #f #f #f 1000 #f))
     (define result (gen:retrieve-memory backend q))
     (if (memory-result-ok? result)
         (let ([items (memory-result-value result)])
           (define deleted-ids
             (for/list ([item (in-list items)])
               (gen:delete-memory! backend (memory-item-id item) scope)
               (memory-item-id item)))
           (make-success-result (list (hasheq 'type
                                              "text"
                                              'text
                                              (format "Cleared ~a memory items from ~a scope"
                                                      (length deleted-ids)
                                                      scope)))
                                (hasheq 'scope scope 'deleted-count (length deleted-ids))))
         (make-error-result
          (format "Failed to retrieve items for clearing: ~a"
                  (hash-ref (memory-result-error result) 'message "unknown error"))))]))

;; ---------------------------------------------------------------------------
;; Tool definitions
;; ---------------------------------------------------------------------------

(define-tool
 list-memory
 #:description
 "List all stored memories, optionally filtered by scope and type. Returns id, type, scope, timestamp, and content snippet for each item."
 #:required ()
 #:properties [(scope "string" "Filter by scope: session, project, or user")
               (types "array" "Filter by memory types")
               (limit "integer" "Maximum results (default: 50)")]
 list-memory-handler)

(define-tool
 clear-memory
 #:description
 "Delete ALL memories in a given scope. Destructive — requires confirm=true. Use list_memory first to review."
 #:required ("scope" "confirm")
 #:properties [(scope "string" "Scope to clear: session, project, or user")
               (confirm "boolean" "Must be true to confirm destructive operation")]
 clear-memory-handler)

(provide store-memory
         search-memory
         delete-memory
         list-memory
         clear-memory
         current-memory-backend
         current-memory-policy)

#lang racket/base
;; runtime/memory/backends/memory-hash.rkt — Deterministic in-memory hash backend
;;
;; Ephemeral hash-table backend for fast tests and non-persistent sessions.
;; Deterministic ordering: sort by (updated-at, id) descending.

(require racket/list
         racket/string
         "../types.rkt"
         "../protocol.rkt"
         (only-in "helpers.rkt"
                  scope-match?
                  type-match?
                  tag-match?
                  text-match?
                  expired?
                  sort-items
                  take-at-most
                  current-iso-8601))

(provide make-memory-hash-backend
         memory-hash-backend-items)

;; ---------------------------------------------------------------------------
;; Backend constructor
;; ---------------------------------------------------------------------------

;; Returns a new memory-backend using a fresh hash table.
;; Thread safety: NOT thread-safe. Callers must serialize access.
(define (make-memory-hash-backend)
  (define store (make-hash)) ; id -> memory-item

  ;; Scope/type/tag/expiry/text filters imported from helpers.rkt (F28)
  ;; F7: text-match? added to filter chain

  ;; store!
  (define (store! item)
    (cond
      [(not (valid-memory-item? item))
       (memory-result #f #f (make-memory-error 'invalid-item "Invalid memory item") (hasheq))]
      [(hash-has-key? store (memory-item-id item))
       (define existing (hash-ref store (memory-item-id item)))
       (if (equal? (memory-item-content existing) (memory-item-content item))
           ;; Idempotent: same content — return success (P2-4)
           (memory-result #t (memory-item-id item) #f (hasheq 'backend 'memory-hash 'idempotent #t))
           ;; Different content for same id: error
           (memory-result #f
                          #f
                          (make-memory-error 'duplicate "Item already exists with different content")
                          (hasheq)))]
      [else
       (hash-set! store (memory-item-id item) item)
       (memory-result #t (memory-item-id item) #f (hasheq 'backend 'memory-hash))]))

  ;; retrieve
  (define (retrieve query)
    (define all-items (hash-values store))
    (define filtered
      (filter (lambda (item)
                (and (scope-match? item query)
                     (type-match? item query)
                     (tag-match? item query)
                     (text-match? item query) ; F7
                     (or (memory-query-include-expired? query) (not (expired? item)))))
              all-items))
    (define sorted (sort-items filtered))
    (define limit (memory-query-limit query))
    (define result (take-at-most sorted limit))
    (memory-result #t
                   result
                   #f
                   (hasheq 'count (length result) 'total (length filtered) 'backend 'memory-hash)))

  ;; update!
  (define (update! id patch)
    (define existing (hash-ref store id #f))
    (cond
      [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
      [else
       ;; Strip immutable fields from patch (P3-3)
       (define safe-patch
         (for/hasheq ([(k v) (in-hash patch)]
                      #:when (not (memq k '(id created-at))))
           (values k v)))
       (define new-content (hash-ref safe-patch 'content (memory-item-content existing)))
       (define new-type (hash-ref safe-patch 'type (memory-item-type existing)))
       (define new-scope (hash-ref safe-patch 'scope (memory-item-scope existing)))
       (define new-validity (hash-ref safe-patch 'validity (memory-item-validity existing)))
       (define new-meta
         (cond
           [(hash-ref safe-patch 'metadata #f)
            =>
            values] ; F9: full replacement
           [else
            (let ([new-tags (hash-ref safe-patch 'tags #f)])
              (if new-tags
                  (hash-set (memory-item-metadata existing) 'tags new-tags)
                  (memory-item-metadata existing)))]))
       (define updated
         (struct-copy memory-item
                      existing
                      [content new-content]
                      [type new-type]
                      [scope new-scope]
                      [metadata new-meta]
                      [validity new-validity]
                      [updated-at (hash-ref safe-patch 'updated-at (current-iso-8601))]))
       (hash-set! store id updated)
       (memory-result #t id #f (hasheq 'backend 'memory-hash))]))

  ;; delete!
  (define (delete! id scope)
    (define existing (hash-ref store id #f))
    (cond
      [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
      [(and scope (not (eq? scope (memory-item-scope existing))))
       (memory-result #f #f (make-memory-error 'scope-mismatch "Scope mismatch") (hasheq))]
      [else
       (hash-remove! store id)
       (memory-result #t id #f (hasheq 'backend 'memory-hash))]))

  ;; list
  (define (list-items query)
    (retrieve query))

  ;; available?
  (define (available?)
    #t)

  ;; manage! — no-op for in-memory backend
  (define (manage! policy)
    (memory-result #t #f #f (hasheq 'backend 'memory-hash)))

  (memory-backend "memory-hash" store! retrieve update! delete! list-items available? manage!))

;; ---------------------------------------------------------------------------
;; Test helper: inspect backend contents
;; ---------------------------------------------------------------------------

(define (memory-hash-backend-items backend)
  ;; Access the internal store via closures — for testing only.
  ;; We use the list operation with a permissive query.
  (define r (gen:list-memory backend (memory-query "" #f #f #f #f #f 10000 #t)))
  (if (memory-result-ok? r)
      (memory-result-value r)
      '()))

;; Helpers imported from helpers.rkt (F28)

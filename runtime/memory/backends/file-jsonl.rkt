#lang racket/base
;; runtime/memory/backends/file-jsonl.rkt — Project-local JSONL memory backend
;;
;; Durable append-only JSONL storage for memory items.
;; Each store/update/delete appends a record. Loading reconstructs the
;; latest view by id (last write wins, delete = tombstone).
;;
;; Safety:
;;   - Project-local by default (path under configured memory-root)
;;   - Path normalization prevents escaping memory root
;;   - Corrupt lines skipped with warning, not fatal
;;   - Append-only writes (no in-place mutation)
;;   - File created with restrictive permissions where possible

(require json
         racket/contract
         racket/file
         racket/list
         racket/match
         racket/path
         racket/string
         racket/system
         "../../../util/json/jsonl.rkt"
         "../types.rkt"
         "../protocol.rkt")

(provide make-file-jsonl-backend
         file-jsonl-backend-path
         safe-memory-path)

;; ---------------------------------------------------------------------------
;; Memory item <-> jsexpr conversion
;; ---------------------------------------------------------------------------

(define (memory-item->jsexpr item)
  (hasheq 'id
          (memory-item-id item)
          'type
          (symbol->string (memory-item-type item))
          'scope
          (symbol->string (memory-item-scope item))
          'content
          (memory-item-content item)
          'metadata
          (memory-item-metadata item)
          'validity
          (memory-item-validity item)
          'created-at
          (memory-item-created-at item)
          'updated-at
          (memory-item-updated-at item)))

(define (jsexpr->memory-item h)
  (memory-item (hash-ref h 'id "")
               (string->symbol (hash-ref h 'type "semantic"))
               (string->symbol (hash-ref h 'scope "session"))
               (hash-ref h 'content "")
               (hash-ref h 'metadata (hasheq))
               (hash-ref h 'validity (hasheq))
               (hash-ref h 'created-at "")
               (hash-ref h 'updated-at "")))

;; ---------------------------------------------------------------------------
;; Event record types
;; ---------------------------------------------------------------------------

;; Each line is: { "__event": "stored"|"updated"|"deleted", "item": {...} }
;; Or for delete: { "__event": "deleted", "id": "...", "scope": "..." }

(define (make-store-record item)
  (hasheq '__event "stored" 'item (memory-item->jsexpr item)))

(define (make-update-record item)
  (hasheq '__event "updated" 'item (memory-item->jsexpr item)))

(define (make-delete-record id scope)
  (hasheq '__event
          "deleted"
          'id
          id
          'scope
          (if scope
              (symbol->string scope)
              #f)))

;; ---------------------------------------------------------------------------
;; Path safety
;; ---------------------------------------------------------------------------

;; Ensure a path stays within the memory root directory.
;; Returns the normalized path or raises an error.
(define (safe-memory-path memory-root rel-path)
  (define root (normalize-path (path->complete-path memory-root)))
  (define target (normalize-path (path->complete-path (build-path memory-root rel-path))))
  (unless (path-within? root target)
    (error 'file-jsonl-backend "path escapes memory root: ~a" target))
  target)

(define (path-within? root path)
  (define root-parts (explode-path root))
  (define path-parts (explode-path path))
  (define root-len (length root-parts))
  (and (>= (length path-parts) root-len)
       (for/and ([a (in-list root-parts)]
                 [b (in-list path-parts)])
         (equal? a b))))

(define (restrict-path-permissions! path mode)
  ;; Best-effort Unix permission hardening. Windows/non-Unix failures are ignored
  ;; because Racket 8.10 does not expose a portable setter in this environment.
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (eq? (system-type 'os) 'unix)
      (system* "/bin/chmod" (number->string mode 8) (path->string path)))))

;; ---------------------------------------------------------------------------
;; Loading: reconstruct latest view
;; ---------------------------------------------------------------------------

;; Read JSONL file and build current view (id -> item or 'deleted).
;; Corrupt lines are skipped with logging.
(define (load-current-view jsonl-path)
  (cond
    [(not (file-exists? jsonl-path)) (hash)]
    [else
     (define-values (entries _skipped) (jsonl-read-all-valid-with-count jsonl-path))
     (for/fold ([view (hash)]) ([entry (in-list entries)])
       (define evt (hash-ref entry '__event #f))
       (cond
         [(equal? evt "stored")
          (define item (jsexpr->memory-item (hash-ref entry 'item (hash))))
          (hash-set view (memory-item-id item) item)]
         [(equal? evt "updated")
          (define item (jsexpr->memory-item (hash-ref entry 'item (hash))))
          (hash-set view (memory-item-id item) item)]
         [(equal? evt "deleted")
          (define id (hash-ref entry 'id #f))
          (if id
              (hash-remove view id)
              view)]
         [else view]))]))

;; ---------------------------------------------------------------------------
;; Backend constructor
;; ---------------------------------------------------------------------------

;; Create a file-jsonl backend rooted at `memory-root`.
;; The JSONL file is at `memory-root/memory.jsonl`.
;; memory-root is created if it doesn't exist.
(define (make-file-jsonl-backend memory-root)
  ;; Ensure directory exists before normalizing target path.
  (unless (directory-exists? memory-root)
    (make-directory* memory-root))
  (restrict-path-permissions! memory-root #o700)
  (define jsonl-path (safe-memory-path memory-root "memory.jsonl"))

  ;; In-memory cache, lazily loaded
  (define cache #f)
  (define cache-dirty? #f)

  (define (get-view)
    (cond
      [(and cache (not cache-dirty?)) cache]
      [else
       (set! cache (load-current-view jsonl-path))
       (set! cache-dirty? #f)
       cache]))

  (define (invalidate-cache!)
    (set! cache-dirty? #t))

  ;; store!
  (define (store! item)
    (cond
      [(not (valid-memory-item? item))
       (memory-result #f #f (make-memory-error 'invalid-item "Invalid memory item") (hasheq))]
      [(hash-ref (get-view) (memory-item-id item) #f)
       (memory-result #f #f (make-memory-error 'duplicate "Item already exists") (hasheq))]
      [else
       (jsonl-append! jsonl-path (make-store-record item))
       (restrict-path-permissions! jsonl-path #o600)
       (invalidate-cache!)
       (memory-result #t (memory-item-id item) #f (hasheq 'backend 'file-jsonl))]))

  ;; retrieve
  (define (retrieve query)
    (define view (get-view))
    (define all-items (hash-values view))
    (define filtered
      (filter (lambda (item)
                (and (scope-match? item query)
                     (type-match? item query)
                     (tag-match? item query)
                     (or (memory-query-include-expired? query) (not (expired? item)))))
              all-items))
    (define sorted (sort-items filtered))
    (define limit (memory-query-limit query))
    (define result (take-at-most sorted limit))
    (memory-result #t
                   result
                   #f
                   (hasheq 'count (length result) 'total (length filtered) 'backend 'file-jsonl)))

  ;; update!
  (define (update! id patch)
    (define view (get-view))
    (define existing (hash-ref view id #f))
    (cond
      [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
      [else
       ;; Strip immutable fields from patch (P3-3)
       (define safe-patch
         (for/hasheq ([(k v) (in-hash patch)]
                      #:when (not (memq k '(id created-at))))
           (values k v)))
       (define new-content (hash-ref safe-patch 'content (memory-item-content existing)))
       (define new-meta
         (let ([new-tags (hash-ref safe-patch 'tags #f)])
           (if new-tags
               (hash-set (memory-item-metadata existing) 'tags new-tags)
               (memory-item-metadata existing))))
       (define new-validity (hash-ref safe-patch 'validity (memory-item-validity existing)))
       (define updated
         (struct-copy memory-item
                      existing
                      [content new-content]
                      [metadata new-meta]
                      [validity new-validity]
                      [updated-at (hash-ref safe-patch 'updated-at (current-iso-8601))]))
       (jsonl-append! jsonl-path (make-update-record updated))
       (restrict-path-permissions! jsonl-path #o600)
       (invalidate-cache!)
       (memory-result #t id #f (hasheq 'backend 'file-jsonl))]))

  ;; delete!
  (define (delete! id scope)
    (define view (get-view))
    (define existing (hash-ref view id #f))
    (cond
      [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
      [(and scope (not (eq? scope (memory-item-scope existing))))
       (memory-result #f #f (make-memory-error 'scope-mismatch "Scope mismatch") (hasheq))]
      [else
       (jsonl-append! jsonl-path (make-delete-record id scope))
       (restrict-path-permissions! jsonl-path #o600)
       (invalidate-cache!)
       (memory-result #t id #f (hasheq 'backend 'file-jsonl))]))

  ;; list
  (define (list-items query)
    (retrieve query))

  ;; available?
  (define (available?)
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (or (not (file-exists? jsonl-path)) (file-exists? jsonl-path))))

  ;; manage! — compact/compaction placeholder
  (define (manage! policy)
    (memory-result #t #f #f (hasheq 'backend 'file-jsonl)))

  (memory-backend "file-jsonl" store! retrieve update! delete! list-items available? manage!))

;; ---------------------------------------------------------------------------
;; Test helper
;; ---------------------------------------------------------------------------

(define (file-jsonl-backend-path backend)
  (memory-backend-name backend))

;; ---------------------------------------------------------------------------
;; Shared filter/sort helpers (same logic as memory-hash backend)
;; ---------------------------------------------------------------------------

(define (scope-match? item query)
  (define q-scope (memory-query-scope query))
  (define q-project (memory-query-project-root query))
  (define q-session (memory-query-session-id query))
  (and (or (not q-scope) (eq? q-scope (memory-item-scope item)))
       (or (not q-project) (equal? q-project (hash-ref (memory-item-metadata item) 'project-root #f)))
       (or (not q-session) (equal? q-session (hash-ref (memory-item-metadata item) 'session-id #f)))))

(define (type-match? item query)
  (define q-types (memory-query-types query))
  (or (not q-types) (memq (memory-item-type item) q-types)))

(define (tag-match? item query)
  (define q-tags (memory-query-tags query))
  (or (not q-tags)
      (let ([item-tags (hash-ref (memory-item-metadata item) 'tags '())])
        (for/and ([t (in-list q-tags)])
          (member t item-tags)))))

(define (expired? item)
  (define expires (hash-ref (memory-item-validity item) 'expires-at #f))
  (and expires (string<? expires (current-iso-8601))))

(define (sort-items items)
  (sort items
        (lambda (a b)
          (define ta (memory-item-updated-at a))
          (define tb (memory-item-updated-at b))
          (cond
            [(string>? ta tb) #t]
            [(string<? ta tb) #f]
            [else (string>? (memory-item-id a) (memory-item-id b))]))))

(define (take-at-most lst n)
  (if (and n (< n (length lst)))
      (take lst n)
      lst))

(define (current-iso-8601)
  (define ts (current-seconds))
  (define d (seconds->date ts #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (pad2 v)
  (define s (format "~a" v))
  (if (< (string-length s) 2)
      (string-append "0" s)
      s))

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
         "../protocol.rkt"
         (only-in "helpers.rkt" scope-match? type-match? tag-match? expired? current-iso-8601 pad2)
         "../search.rkt"
         (only-in "../management.rkt" analyze-items))

(provide make-file-jsonl-backend
         file-jsonl-backend-path
         safe-memory-path)

;; ---------------------------------------------------------------------------
;; Memory item <-> jsexpr conversion
;; ---------------------------------------------------------------------------

;; Convert symbol values to strings for JSON serialization (F2/F31)
;; JSON cannot represent symbols as values.
(define (hash-symbols->strings h)
  (for/hasheq ([(k val) (in-hash h)])
    (values k
            (cond
              [(symbol? val) (symbol->string val)]
              [(list? val)
               (map (lambda (v)
                      (if (symbol? v)
                          (symbol->string v)
                          v))
                    val)]
              [else val]))))

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
          (hash-symbols->strings (memory-item-metadata item))
          'validity
          (hash-symbols->strings (memory-item-validity item))
          'created-at
          (memory-item-created-at item)
          'updated-at
          (memory-item-updated-at item)))

;; Convert string keys back to symbols after JSON round-trip (F2/F31)
;; JSON serialization converts hasheq symbol keys to strings.
(define (normalize-string-keys h)
  (for/hasheq ([(k v) (in-hash h)])
    (values (if (symbol? k)
                k
                (string->symbol k))
            v)))

;; Convert sensitivity from string to symbol after JSON round-trip (F2/F31)
(define (normalize-validity v)
  (define h (normalize-string-keys v))
  (define sens (hash-ref h 'sensitivity 'public))
  (hash-set h
            'sensitivity
            (if (symbol? sens)
                sens
                (string->symbol sens))))

(define (jsexpr->memory-item h)
  (memory-item (hash-ref h 'id "")
               (string->symbol (hash-ref h 'type "semantic"))
               (string->symbol (hash-ref h 'scope "session"))
               (hash-ref h 'content "")
               (normalize-string-keys (hash-ref h 'metadata (hasheq)))
               (normalize-validity (hash-ref h 'validity (hasheq)))
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

;; M13-F6: Apply a list of JSONL entries to a view hash (id -> item).
;; Used by both full load and incremental load.
(define (apply-entries-to-view view entries)
  (for/fold ([v view]) ([entry (in-list entries)])
    (let ([evt (hash-ref entry '__event #f)])
      (cond
        [(equal? evt "stored")
         (let ([item (jsexpr->memory-item (hash-ref entry 'item (hash)))])
           (hash-set v (memory-item-id item) item))]
        [(equal? evt "updated")
         (let ([item (jsexpr->memory-item (hash-ref entry 'item (hash)))])
           (hash-set v (memory-item-id item) item))]
        [(equal? evt "deleted")
         (let ([id (hash-ref entry 'id #f)])
           (if id
               (hash-remove v id)
               v))]
        [else v]))))

;; M13-F6: Read entries from a JSONL file starting at byte offset.
;; Returns list of parsed entries (or '() on error/missing file).
;; If offset lands at a line boundary, reads all lines from there.
;; If offset lands mid-line, skips the partial line.
(define (read-entries-from-offset path offset)
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (in)
                              (cond
                                [(= offset 0)
                                 ;; Reading from start — no partial line issue
                                 (for/list ([line (in-lines in)]
                                            #:when (non-empty-string? (string-trim line)))
                                   (with-handlers ([exn:fail? (lambda (_) #f)])
                                     (string->jsexpr line)))]
                                [else
                                 ;; Check if offset is at a line boundary
                                 (file-position in (max 0 (sub1 offset)))
                                 (define prev-byte (read-byte in))
                                 (unless (or (= prev-byte (char->integer #\newline))
                                             (= prev-byte (char->integer #\return)))
                                   ;; Mid-line — skip partial line
                                   (read-line in))
                                 (for/list ([line (in-lines in)]
                                            #:when (non-empty-string? (string-trim line)))
                                   (with-handlers ([exn:fail? (lambda (_) #f)])
                                     (string->jsexpr line)))]))
                            #:mode 'text)))

;; M13-F6: Incremental load — only process appended lines since last cache.
(define (incremental-load-view path old-view old-size)
  (cond
    [(not (file-exists? path)) (hash)]
    [(and old-view (> old-size 0))
     (let ([current-size (file-size path)])
       (cond
         [(< current-size old-size) (load-current-view path)]
         [(= current-size old-size) old-view]
         [else
          (let ([new-entries (filter values (read-entries-from-offset path old-size))])
            (apply-entries-to-view old-view new-entries))]))]
    [else (load-current-view path)]))

;; ---------------------------------------------------------------------------
;; Backend constructor
;; ---------------------------------------------------------------------------

;; Create a file-jsonl backend rooted at `memory-root`.
;; The JSONL file is at `memory-root/memory.jsonl`.
;; memory-root is created if it doesn't exist.
(define (make-file-jsonl-backend memory-root)
  ;; Lazy directory creation: defer until first write (P3-6)
  (define dir-initialized? #f)
  (define (ensure-dir!)
    (unless dir-initialized?
      (unless (directory-exists? memory-root)
        (make-directory* memory-root))
      (restrict-path-permissions! memory-root #o700)
      (set! dir-initialized? #t)))
  (define jsonl-path (build-path memory-root "memory.jsonl"))

  ;; In-memory cache with incremental processing (M13-F6)
  (define cache #f)
  (define cache-dirty? #f)
  (define cached-file-size 0)

  (define (get-view)
    (define current-size
      (if (file-exists? jsonl-path)
          (file-size jsonl-path)
          0))
    (cond
      [(and cache (not cache-dirty?) (= current-size cached-file-size)) cache]
      [else
       (set! cache
             (if (file-exists? jsonl-path)
                 (incremental-load-view jsonl-path cache cached-file-size)
                 (hash)))
       (set! cached-file-size current-size)
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
       (define existing (hash-ref (get-view) (memory-item-id item)))
       (if (equal? (memory-item-content existing) (memory-item-content item))
           ;; Idempotent: same content — return success (P2-4)
           (memory-result #t (memory-item-id item) #f (hasheq 'backend 'file-jsonl 'idempotent #t))
           ;; Different content for same id: error
           (memory-result #f
                          #f
                          (make-memory-error 'duplicate "Item already exists with different content")
                          (hasheq)))]
      [(for/first ([existing (in-list (hash-values (get-view)))]
                   #:when (content-duplicate? existing item))
         existing)
       =>
       (lambda (existing)
         (memory-result
          #t
          (memory-item-id existing)
          #f
          (hasheq 'backend 'file-jsonl 'idempotent #t 'duplicate-of (memory-item-id existing))))]
      [else
       (ensure-dir!)
       (define safe-path (safe-memory-path memory-root "memory.jsonl"))
       (jsonl-append! safe-path (make-store-record item))
       (restrict-path-permissions! safe-path #o600)
       (invalidate-cache!)
       (memory-result #t (memory-item-id item) #f (hasheq 'backend 'file-jsonl))]))

  ;; retrieve — uses search.rkt for ranking + dedup (M13-F3, M13-F4)
  (define (retrieve query)
    (define view (get-view))
    (define all-items (hash-values view))
    ;; Scope/type/tag/expiry filters still done at backend level
    (define filtered
      (filter (lambda (item)
                (and (scope-match? item query)
                     (type-match? item query)
                     (tag-match? item query)
                     (or (memory-query-include-expired? query) (not (expired? item)))))
              all-items))
    ;; Text filtering: keep items with positive relevance score
    (define query-text (memory-query-text query))
    (define text-filtered
      (if (or (not query-text) (string=? query-text ""))
          filtered
          (filter (lambda (item) (> (relevance-score item query-text) 0)) filtered)))
    ;; Post-retrieve: dedup, supersedes, ranking, limit
    (define result (post-retrieve-process text-filtered query #:all-items filtered))
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
       (define new-type (hash-ref safe-patch 'type (memory-item-type existing)))
       (define new-scope (hash-ref safe-patch 'scope (memory-item-scope existing)))
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
       (define new-validity (hash-ref safe-patch 'validity (memory-item-validity existing)))
       (define updated
         (struct-copy memory-item
                      existing
                      [content new-content]
                      [type new-type]
                      [scope new-scope]
                      [metadata new-meta]
                      [validity new-validity]
                      [updated-at (hash-ref safe-patch 'updated-at (current-iso-8601))]))
       (cond
         [(not (valid-memory-item? updated))
          (memory-result #f
                         #f
                         (make-memory-error 'invalid-item "Invalid updated memory item")
                         (hasheq))]
         [else
          (ensure-dir!)
          (define safe-path (safe-memory-path memory-root "memory.jsonl"))
          (jsonl-append! safe-path (make-update-record updated))
          (restrict-path-permissions! safe-path #o600)
          (invalidate-cache!)
          (memory-result #t id #f (hasheq 'backend 'file-jsonl))])]))

  ;; delete!
  (define (delete! id scope)
    (define view (get-view))
    (define existing (hash-ref view id #f))
    (cond
      [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
      [(and scope (not (eq? scope (memory-item-scope existing))))
       (memory-result #f #f (make-memory-error 'scope-mismatch "Scope mismatch") (hasheq))]
      [else
       (ensure-dir!)
       (define safe-path (safe-memory-path memory-root "memory.jsonl"))
       (jsonl-append! safe-path (make-delete-record id scope))
       (restrict-path-permissions! safe-path #o600)
       (invalidate-cache!)
       (memory-result #t id #f (hasheq 'backend 'file-jsonl))]))

  ;; list — raw scoped view for management/export (no text search, dedup, or supersedes filtering).
  (define (list-items query)
    (define view (get-view))
    (define filtered
      (filter (lambda (item)
                (and (scope-match? item query)
                     (type-match? item query)
                     (tag-match? item query)
                     (or (memory-query-include-expired? query) (not (expired? item)))))
              (hash-values view)))
    (define sorted
      (sort filtered
            (lambda (a b)
              (define ta (memory-item-updated-at a))
              (define tb (memory-item-updated-at b))
              (cond
                [(string>? ta tb) #t]
                [(string<? ta tb) #f]
                [else (string>? (memory-item-id a) (memory-item-id b))]))))
    (define limit (memory-query-limit query))
    (define result
      (if (and limit (< limit (length sorted)))
          (take sorted limit)
          sorted))
    (memory-result #t
                   result
                   #f
                   (hasheq 'count (length result) 'total (length filtered) 'backend 'file-jsonl)))

  ;; available? (F8): always #t — directory is lazily created on first write.
  ;; A missing directory/JSONL file is a valid state.
  (define (available?)
    #t)

  ;; manage! — v0.95.16 W6: Use management.rkt for deterministic analysis
  (define (manage! policy)
    (define view (get-view))
    (define current-items (hash-values view))
    (define report (analyze-items current-items policy))
    (define compacted-count (length (hash-ref report 'removed-ids '())))
    (cond
      ;; Report only — no file mutations
      [(hash-ref report 'dry-run? #f) (memory-result #t #f #f (hash-set report 'backend 'file-jsonl))]
      [else
       ;; Rewrite file with only kept items
       (define kept-ids (hash-ref report 'kept-ids '()))
       (define keepers (filter (lambda (i) (member (memory-item-id i) kept-ids)) current-items))
       (ensure-dir!)
       (define safe-path (safe-memory-path memory-root "memory.jsonl"))
       (call-with-output-file safe-path
                              (lambda (out)
                                (for ([item (in-list keepers)])
                                  (write-json (make-store-record item) out)
                                  (newline out)))
                              #:exists 'replace
                              #:mode 'text)
       (restrict-path-permissions! safe-path #o600)
       (set! cache
             (for/hash ([item (in-list keepers)])
               (values (memory-item-id item) item)))
       (set! cached-file-size
             (if (file-exists? safe-path)
                 (file-size safe-path)
                 0))
       (set! cache-dirty? #f)
       (memory-result #t #f #f (hasheq 'backend 'file-jsonl 'compacted-count compacted-count))]))

  (define the-backend
    (memory-backend "file-jsonl" store! retrieve update! delete! list-items available? manage!))
  (hash-set! backend-paths the-backend (path->string jsonl-path))
  the-backend)

;; ---------------------------------------------------------------------------
;; Test helper
;; ---------------------------------------------------------------------------

;; Backend path registry — weak hash from backend -> actual jsonl-path (P3-4)
(define backend-paths (make-weak-hasheq))

(define (file-jsonl-backend-path backend)
  (hash-ref backend-paths backend (memory-backend-name backend)))

;; Helpers imported from helpers.rkt (F28)

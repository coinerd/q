#lang racket/base

;; runtime/memory/backends/file-jsonl-ops.rkt — Extracted operations for file-jsonl backend
;; STABILITY: internal
;;
;; v0.96.6 (F8): Extracted from file-jsonl.rkt to keep the backend constructor ≤50 LOC.
;; Contains all helper functions, record constructors, loading, and backend operations.

(require json
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
         (only-in "../search.rkt" content-duplicate? relevance-score post-retrieve-process)
         (only-in "../management.rkt" analyze-items))

(provide make-store-record
         make-update-record
         make-delete-record
         safe-memory-path
         restrict-path-permissions!
         incremental-load-view
         fjl-store!
         fjl-retrieve
         fjl-update!
         fjl-delete!
         fjl-list-items
         fjl-manage!)

;; ---------------------------------------------------------------------------
;; Memory item <-> jsexpr conversion
;; ---------------------------------------------------------------------------

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

(define (normalize-string-keys h)
  (for/hasheq ([(k v) (in-hash h)])
    (values (if (symbol? k)
                k
                (string->symbol k))
            v)))

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
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (when (eq? (system-type 'os) 'unix)
      (system* "/bin/chmod" (number->string mode 8) (path->string path)))))

;; ---------------------------------------------------------------------------
;; Loading: reconstruct latest view
;; ---------------------------------------------------------------------------

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

(define (read-entries-from-offset path offset)
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (in)
                              (cond
                                [(= offset 0)
                                 (for/list ([line (in-lines in)]
                                            #:when (non-empty-string? (string-trim line)))
                                   (with-handlers ([exn:fail? (lambda (_) #f)])
                                     (string->jsexpr line)))]
                                [else
                                 (file-position in (max 0 (sub1 offset)))
                                 (define prev-byte (read-byte in))
                                 (unless (or (= prev-byte (char->integer #\newline))
                                             (= prev-byte (char->integer #\return)))
                                   (read-line in))
                                 (for/list ([line (in-lines in)]
                                            #:when (non-empty-string? (string-trim line)))
                                   (with-handlers ([exn:fail? (lambda (_) #f)])
                                     (string->jsexpr line)))]))
                            #:mode 'text)))

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
;; JSONL append helper
;; ---------------------------------------------------------------------------

(define (jsonl-append! path record)
  (call-with-output-file path
                         (lambda (out)
                           (write-json record out)
                           (newline out))
                         #:exists 'append
                         #:mode 'text))

;; ---------------------------------------------------------------------------
;; Extracted backend operations
;; ---------------------------------------------------------------------------

(define (fjl-store! get-view ensure-dir! invalidate-cache! memory-root item)
  (cond
    [(not (valid-memory-item? item))
     (memory-result #f #f (make-memory-error 'invalid-item "Invalid memory item") (hasheq))]
    [(hash-ref (get-view) (memory-item-id item) #f)
     (define existing (hash-ref (get-view) (memory-item-id item)))
     (if (equal? (memory-item-content existing) (memory-item-content item))
         (memory-result #t (memory-item-id item) #f (hasheq 'backend 'file-jsonl 'idempotent #t))
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

(define (fjl-retrieve get-view query)
  (define view (get-view))
  (define all-items (hash-values view))
  (define filtered
    (filter (lambda (item)
              (and (scope-match? item query)
                   (type-match? item query)
                   (tag-match? item query)
                   (or (memory-query-include-expired? query) (not (expired? item)))))
            all-items))
  (define query-text (memory-query-text query))
  (define text-filtered
    (if (or (not query-text) (string=? query-text ""))
        filtered
        (filter (lambda (item) (> (relevance-score item query-text) 0)) filtered)))
  (define result (post-retrieve-process text-filtered query #:all-items filtered))
  (memory-result #t
                 result
                 #f
                 (hasheq 'count (length result) 'total (length filtered) 'backend 'file-jsonl)))

(define (fjl-update! get-view ensure-dir! invalidate-cache! memory-root id patch)
  (define view (get-view))
  (define existing (hash-ref view id #f))
  (cond
    [(not existing) (memory-result #f #f (make-memory-error 'not-found "Item not found") (hasheq))]
    [else
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
          values]
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

(define (fjl-delete! get-view ensure-dir! invalidate-cache! memory-root id scope)
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

(define (fjl-list-items get-view query)
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

(define (fjl-manage! get-view ensure-dir! set-cache! set-cache-size! memory-root policy)
  (define view (get-view))
  (define current-items (hash-values view))
  (define report (analyze-items current-items policy))
  (define compacted-count (length (hash-ref report 'removed-ids '())))
  (cond
    [(hash-ref report 'dry-run? #f) (memory-result #t #f #f (hash-set report 'backend 'file-jsonl))]
    [else
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
     (set-cache! (for/hash ([item (in-list keepers)])
                   (values (memory-item-id item) item)))
     (set-cache-size! (if (file-exists? safe-path)
                          (file-size safe-path)
                          0))
     (memory-result #t #f #f (hasheq 'backend 'file-jsonl 'compacted-count compacted-count))]))

#lang racket/base

;; runtime/session-index/bookmarks.rkt — bookmark domain operations
;;
;; Extracted from mutations.rkt in v0.99.58 W2-1 (P1-BK).
;; Bookmarks are a self-contained domain: in-memory mutations, queries,
;; and JSON persistence. No dependencies on append/update/branch logic.

(require racket/contract
         racket/string
         racket/file
         racket/port
         racket/path
         json
         "../../util/error/error-helpers.rkt"
         "../../util/json/json-helpers.rkt"
         "../../util/message/message-helpers.rkt"
         "schema.rkt")

(provide (contract-out
          [add-bookmark!
           (-> session-index? (or/c string? #f) string? (values session-index? (or/c string? #f)))]
          [remove-bookmark! (-> session-index? (or/c string? #f) (values session-index? boolean?))]
          [list-bookmarks (-> session-index? (listof bookmark?))]
          [find-bookmark-by-label (-> session-index? string? (or/c bookmark? #f))]
          [get-bookmark (-> session-index? (or/c string? #f) (or/c bookmark? #f))]
          [load-bookmarks (-> path-string? (listof bookmark?))]
          [save-bookmarks! (-> path-string? session-index? void?)]
          [bookmarks-path (-> path-string? path?)]
          [bookmark->jsexpr (-> bookmark? hash?)]
          [jsexpr->bookmark (-> hash? bookmark?)]))

;; ============================================================
;; ID generation
;; ============================================================

;; Counter for generating unique bookmark IDs
;; M11 (v0.97.15): Replaced bare set! with boxed counter for thread safety
(define bm-counter (box 0))
(define bm-counter-mutex (make-semaphore 1))

(define (generate-bookmark-id)
  (semaphore-wait bm-counter-mutex)
  (set-box! bm-counter (add1 (unbox bm-counter)))
  (define id (format "bm-~a-~a" (current-milliseconds) (unbox bm-counter)))
  (semaphore-post bm-counter-mutex)
  id)

;; Local path-only helper (from util/path-helpers.rkt)
(define (path-only p)
  (define-values (dir _base _must-be-dir?) (split-path p))
  (if (eq? dir 'relative) #f dir))

;; ============================================================
;; Bookmarks
;; ============================================================

;; R-6: Returns (values session-index? (or/c string? #f))
(define (add-bookmark! idx entry-id label)
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define old-bookmarks (session-index-bookmarks idx))
                         (define existing (find-bookmark-by-label idx label))
                         (define bookmarks-after-remove
                           (if existing
                               (hash-remove old-bookmarks (bookmark-id existing))
                               old-bookmarks))
                         (define id (generate-bookmark-id))
                         (define ts (current-seconds))
                         (define bm (make-bookmark id entry-id label ts))
                         (define new-bookmarks (hash-set bookmarks-after-remove id bm))
                         (values (session-index (session-index-by-id idx)
                                                (session-index-children idx)
                                                (session-index-entry-order idx)
                                                new-bookmarks
                                                (session-index-active-leaf-id idx)
                                                (session-index-bookmark-sem idx))
                                 id))))

;; R-6: Returns (values session-index? boolean?)
(define (remove-bookmark! idx bookmark-id)
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define bookmarks (session-index-bookmarks idx))
                         (if (hash-has-key? bookmarks bookmark-id)
                             (values (session-index (session-index-by-id idx)
                                                    (session-index-children idx)
                                                    (session-index-entry-order idx)
                                                    (hash-remove bookmarks bookmark-id)
                                                    (session-index-active-leaf-id idx)
                                                    (session-index-bookmark-sem idx))
                                     #t)
                             (values idx #f)))))

(define (list-bookmarks idx)
  (hash-values (session-index-bookmarks idx)))

(define (find-bookmark-by-label idx label)
  (define bookmarks (session-index-bookmarks idx))
  (for/first ([bm (in-hash-values bookmarks)]
              #:when (equal? (bookmark-label bm) label))
    bm))

(define (get-bookmark idx bookmark-id)
  (hash-ref (session-index-bookmarks idx) bookmark-id #f))

;; ============================================================
;; Bookmark persistence
;; ============================================================

(define (bookmarks-path session-path)
  (define dir (path-only session-path))
  (define base (file-name-from-path session-path))
  (define base-str (path->string base))
  (define name
    (if (string-suffix? base-str ".jsonl")
        (substring base-str 0 (- (string-length base-str) 6))
        base-str))
  (build-path dir (format "~a-bookmarks.json" name)))

(define (bookmark->jsexpr bm)
  (hasheq 'id
          (bookmark-id bm)
          'entryId
          (bookmark-entry-id bm)
          'label
          (bookmark-label bm)
          'timestamp
          (bookmark-timestamp bm)))

(define (jsexpr->bookmark h)
  (make-bookmark (hash-ref h 'id) (hash-ref h 'entryId) (hash-ref h 'label) (hash-ref h 'timestamp)))

(define (save-bookmarks! session-path idx)
  (define bpath (bookmarks-path session-path))
  (define bookmarks (list-bookmarks idx))
  (define data (map bookmark->jsexpr bookmarks))
  (ensure-parent-dirs! bpath)
  (define temp-path (format "~a.tmp" bpath))
  (write-json-file temp-path data)
  (when (file-exists? bpath)
    (delete-file bpath))
  (rename-file-or-directory temp-path bpath))

(define (load-bookmarks session-path)
  (define bpath (bookmarks-path session-path))
  (cond
    [(not (file-exists? bpath)) '()]
    [else
     (with-safe-fallback '()
                         (define data
                           (call-with-input-file bpath (lambda (in) (read-json in)) #:mode 'text))
                         (if (list? data)
                             (map jsexpr->bookmark data)
                             '()))]))

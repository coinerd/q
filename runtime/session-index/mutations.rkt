#lang racket/base

;; runtime/session-index/mutations.rkt — append, update, branch, bookmark, persist operations
;;
;; Mutation operations on session-index: building, persisting, branching, bookmarking.

(require "../../util/error-helpers.rkt")
(require racket/contract
         racket/string
         racket/file
         racket/port
         racket/path
         racket/list
         json
         "../../util/json-helpers.rkt"
         (only-in "../../util/protocol-types.rkt"
                  message-id
                  message-parent-id
                  message-kind
                  message-role
                  make-message
                  make-text-part
                  message->jsexpr
                  jsexpr->message
                  message)
         (only-in "../../util/message-helpers.rkt" ensure-parent-dirs!)
         (only-in "../../util/jsonl.rkt" jsonl-read-all-valid)
         (only-in "../session-store.rkt" load-session-log)
         "schema.rkt"
         "query.rkt")

(provide (contract-out [build-index! (-> path-string? path-string? any/c)]
                       [load-index (-> string? any/c)]
                       [save-index! (-> string? any/c any/c)]
                       [switch-leaf! (-> any/c any/c any/c)]
                       [mark-active-leaf! (-> any/c any/c any/c)]
                       [navigate-to-entry! (-> any/c any/c any/c)]
                       [navigate-to-leaf! (-> any/c any/c any/c)]
                       [navigate-next-leaf! (-> any/c any/c)]
                       [navigate-prev-leaf! (-> any/c any/c)]
                       [branch! (-> any/c any/c any/c)]
                       [branch-with-summary! (-> any/c any/c string? any/c)]
                       [reset-leaf! (-> any/c void?)]
                       [append-to-leaf! (-> any/c any/c any/c)]
                       [add-bookmark! (-> any/c any/c string? any/c)]
                       [remove-bookmark! (-> any/c any/c boolean?)]
                       [list-bookmarks (-> any/c (listof any/c))]
                       [find-bookmark-by-label (-> any/c string? any/c)]
                       [get-bookmark (-> any/c any/c any/c)]
                       [load-bookmarks (-> string? (listof any/c))]
                       [save-bookmarks! (-> string? any/c void?)]
                       [load-index-with-bookmarks (-> string? string? any/c)]
                       [bookmarks-path (-> string? string?)]))

;; Counter for generating unique bookmark IDs
(define bm-counter 0)
(define bm-counter-mutex (make-semaphore 1))

;; Local path-only helper (from util/path-helpers.rkt)
(define (path-only p)
  (define-values (dir _base _must-be-dir?) (split-path p))
  (if (eq? dir 'relative) #f dir))

(define (generate-bookmark-id)
  (semaphore-wait bm-counter-mutex)
  (set! bm-counter (add1 bm-counter))
  (define id (format "bm-~a-~a" (current-milliseconds) bm-counter))
  (semaphore-post bm-counter-mutex)
  id)

;; ============================================================
;; build-index!
;; ============================================================

(define (build-index! session-path index-path)
  (define messages (load-session-log session-path))
  (define by-id (make-hash))
  (define children (make-hash))
  (for ([msg (in-list messages)])
    (hash-set! by-id (message-id msg) msg)
    (hash-set! children (message-id msg) '()))
  ;; Infer missing parentIds from log order
  (define fixed-messages
    (for/list ([msg (in-list messages)]
               [i (in-naturals)])
      (cond
        [(and (not (message-parent-id msg)) (> i 0) (not (eq? (message-kind msg) 'session-info)))
         (define prev-msg (list-ref messages (sub1 i)))
         (struct-copy message msg [parent-id (message-id prev-msg)])]
        [else msg])))
  (for ([msg (in-list fixed-messages)])
    (hash-set! by-id (message-id msg) msg))
  (for ([msg (in-list fixed-messages)])
    (define pid (message-parent-id msg))
    (when (and pid (hash-has-key? children pid))
      (hash-update! children pid (lambda (lst) (append lst (list msg))))))
  (define idx
    (session-index by-id
                   children
                   (list->vector fixed-messages)
                   (make-hash)
                   (box #f)
                   (make-semaphore 1)))
  (save-index! index-path idx)
  idx)

;; ============================================================
;; Persistence
;; ============================================================

(define (save-index! path idx)
  (define entries (vector->list (session-index-entry-order idx)))
  (define header (hasheq 'type "index-header" 'count (length entries)))
  (define lines (cons header (map message->jsexpr entries)))
  (ensure-parent-dirs! path)
  (define blob
    (with-output-to-string (lambda ()
                             (for ([entry (in-list lines)])
                               (write-json entry)
                               (newline)))))
  (call-with-output-file path (lambda (out) (display blob out)) #:mode 'text #:exists 'truncate))

(define (load-index path)
  (if (not (file-exists? path))
      (make-empty-index)
      (let ()
        (define raw (jsonl-read-all-valid path))
        (cond
          [(null? raw)
           (fprintf (current-error-port)
                    "WARNING: Session index corrupted and could not be loaded.\n")
           (make-empty-index)]
          [else
           (define entries (map jsexpr->message (cdr raw)))
           (define by-id (make-hash))
           (define children (make-hash))
           (for ([msg (in-list entries)])
             (hash-set! by-id (message-id msg) msg)
             (hash-set! children (message-id msg) '()))
           (for ([msg (in-list entries)])
             (define pid (message-parent-id msg))
             (when (and pid (hash-has-key? children pid))
               (hash-update! children pid (lambda (lst) (append lst (list msg))))))
           (session-index by-id
                          children
                          (list->vector entries)
                          (make-hash)
                          (box #f)
                          (make-semaphore 1))]))))

;; ============================================================
;; Active leaf
;; ============================================================

(define (switch-leaf! idx entry-id)
  (if (lookup-entry idx entry-id)
      (begin
        (set-box! (session-index-active-leaf-id idx) entry-id)
        entry-id)
      #f))

(define (mark-active-leaf! idx entry-id)
  (if (lookup-entry idx entry-id)
      (begin
        (set-box! (session-index-active-leaf-id idx) entry-id)
        entry-id)
      #f))

;; ============================================================
;; Navigation
;; ============================================================

(define (navigate-to-entry! idx entry-id)
  (define entry (lookup-entry idx entry-id))
  (if (not entry)
      #f
      (let* ([branch (or (get-branch idx entry-id) '())]
             [children (hash-ref (session-index-children idx) entry-id '())]
             [is-leaf (null? children)])
        (set-box! (session-index-active-leaf-id idx) entry-id)
        (navigate-result entry branch children is-leaf))))

(define (navigate-to-leaf! idx entry-id)
  (define result (navigate-to-entry! idx entry-id))
  (if (and result (navigate-result-leaf? result)) result #f))

(define (navigate-next-leaf! idx)
  (define leaves (leaf-nodes idx))
  (if (null? leaves)
      #f
      (let* ([current-id (unbox (session-index-active-leaf-id idx))]
             [leaf-ids (map message-id leaves)]
             [current-pos (if current-id
                              (or (for/first ([id (in-list leaf-ids)]
                                              [i (in-naturals)]
                                              #:when (equal? id current-id))
                                    i)
                                  -1)
                              -1)]
             [next-pos (modulo (add1 current-pos) (length leaves))]
             [next-entry (list-ref leaves next-pos)])
        (navigate-to-entry! idx (message-id next-entry)))))

(define (navigate-prev-leaf! idx)
  (define leaves (leaf-nodes idx))
  (if (null? leaves)
      #f
      (let* ([current-id (unbox (session-index-active-leaf-id idx))]
             [leaf-ids (map message-id leaves)]
             [n (length leaves)]
             [current-pos (if current-id
                              (or (for/first ([id (in-list leaf-ids)]
                                              [i (in-naturals)]
                                              #:when (equal? id current-id))
                                    i)
                                  0)
                              0)]
             [prev-pos (modulo (sub1 current-pos) n)]
             [prev-entry (list-ref leaves prev-pos)])
        (navigate-to-entry! idx (message-id prev-entry)))))

;; ============================================================
;; Branch operations
;; ============================================================

(define (branch! idx entry-id)
  (define entry (lookup-entry idx entry-id))
  (cond
    [(not entry) #f]
    [else
     (set-box! (session-index-active-leaf-id idx) entry-id)
     entry]))

(define (branch-with-summary! idx entry-id summary-text)
  (define entry (branch! idx entry-id))
  (cond
    [(not entry) #f]
    [else
     (define summary-msg
       (make-message (format "branch-summary-~a" (current-inexact-milliseconds))
                     entry-id
                     'system
                     'branch-summary
                     (list (make-text-part summary-text))
                     (current-seconds)
                     (hasheq 'type "branch-summary" 'from-id entry-id)))
     (append-to-leaf! idx summary-msg)
     summary-msg]))

(define (reset-leaf! idx)
  (set-box! (session-index-active-leaf-id idx) #f)
  (void))

(define (append-to-leaf! idx entry)
  (define parent (active-leaf idx))
  (define parent-id
    (if parent
        (message-id parent)
        #f))
  (define fixed-entry
    (if (and parent-id (not (message-parent-id entry)))
        (struct-copy message entry [parent-id parent-id])
        entry))
  (hash-set! (session-index-by-id idx) (message-id fixed-entry) fixed-entry)
  (when parent-id
    (hash-update! (session-index-children idx)
                  parent-id
                  (lambda (lst) (append lst (list fixed-entry)))))
  (hash-set! (session-index-children idx) (message-id fixed-entry) '())
  (set-box! (session-index-active-leaf-id idx) (message-id fixed-entry))
  fixed-entry)

;; ============================================================
;; Bookmarks
;; ============================================================

(define (add-bookmark! idx entry-id label)
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define bookmarks (session-index-bookmarks idx))
                         (define existing (find-bookmark-by-label idx label))
                         (when existing
                           (hash-remove! bookmarks (bookmark-id existing)))
                         (define id (generate-bookmark-id))
                         (define ts (current-seconds))
                         (define bm (make-bookmark id entry-id label ts))
                         (hash-set! bookmarks id bm)
                         id)))

(define (remove-bookmark! idx bookmark-id)
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define bookmarks (session-index-bookmarks idx))
                         (if (hash-has-key? bookmarks bookmark-id)
                             (begin
                               (hash-remove! bookmarks bookmark-id)
                               #t)
                             #f))))

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

(define (load-index-with-bookmarks session-path index-path)
  (define idx (load-index index-path))
  (define bookmarks (load-bookmarks session-path))
  (define bm-hash (make-hash))
  (for ([bm (in-list bookmarks)])
    (hash-set! bm-hash (bookmark-id bm) bm))
  (session-index (session-index-by-id idx)
                 (session-index-children idx)
                 (session-index-entry-order idx)
                 bm-hash
                 (box #f)
                 (session-index-bookmark-sem idx)))

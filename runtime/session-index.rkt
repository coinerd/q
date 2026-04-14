#lang racket/base

;;; runtime/session-index.rkt — sidecar derived indexes for session logs
;;;
;;; Provides:
;;;   session-index?         — predicate
;;;   session-index-by-id    — accessor: id -> message hash table
;;;   build-index!           — build/refresh index from JSONL log, save sidecar
;;;   load-index             — load a previously saved index from disk
;;;   lookup-entry           — find entry by id
;;;   children-of            — get children of a node
;;;   leaf-nodes             — get all leaf nodes (no children)
;;;   resolve-active-leaf    — get the current active leaf
;;;
;;; The index is derived data, never canonical truth.
;;; It must always be rebuildable from the JSONL log.

(require racket/contract
         racket/string
         racket/hash
         racket/file
         racket/port
         racket/path
         racket/list
         json
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt"
         "../runtime/session-store.rkt")

(provide session-index?
         session-index-by-id
         session-index-entry-order
         session-index-bookmarks
         build-index!
         load-index
         lookup-entry
         children-of
         leaf-nodes
         resolve-active-leaf
         ;; Active Leaf API
         active-leaf
         switch-leaf!
         mark-active-leaf!
         ;; Tree operations (#496)
         get-branch
         branch!
         reset-leaf!
         append-to-leaf!
         leaf-depth
         ;; Bookmark API
         bookmark?
         bookmark-id
         bookmark-entry-id
         bookmark-label
         bookmark-timestamp
         make-bookmark
         add-bookmark!
         remove-bookmark!
         list-bookmarks
         find-bookmark-by-label
         get-bookmark
         load-bookmarks
         save-bookmarks!)

;; ============================================================
;; Index struct
;; ============================================================

;; A bookmark holds user-defined labels for quick navigation
(struct bookmark (id entry-id label timestamp) #:transparent)

;; A session-index holds:
;;   by-id          — hash: id-string -> message
;;   children        — hash: id-string -> (listof message)
;;   entry-order     — vector of messages in append order
;;   bookmarks       — hash: bookmark-id -> bookmark
;;   active-leaf-id  — (box (or/c #f string?)) — id of explicitly marked active leaf
;;   bookmark-sem    — semaphore for thread-safe bookmark mutations (#115)

;; ============================================================
;; build-index!
;; ============================================================

(define (build-index! session-path index-path)
  ;; Build the sidecar index from the JSONL log at session-path.
  ;; Writes the index to index-path.
  ;; Returns the session-index struct.
  (define messages (load-session-log session-path))
  (define by-id (make-hash))
  (define children (make-hash))
  ;; Populate by-id
  (for ([msg (in-list messages)])
    (hash-set! by-id (message-id msg) msg)
    (hash-set! children (message-id msg) '()))
  ;; Populate children: for each entry, add it to its parent's children list
  (for ([msg (in-list messages)])
    (define pid (message-parent-id msg))
    (when (and pid (hash-has-key? children pid))
      (hash-update! children pid (lambda (lst) (append lst (list msg))))))
  (define idx
    (session-index by-id children (list->vector messages) (make-hash) (box #f) (make-semaphore 1)))
  ;; Persist index to disk
  (save-index! index-path idx)
  idx)

;; ============================================================
;; Index persistence
;; ============================================================

(define (save-index! path idx)
  ;; Save the index to disk as a JSONL file.
  ;; Format: one line per entry, with a header line for metadata.
  ;; First line: {"type":"index-header","count":N}
  ;; Subsequent lines: serialized message entries
  (define entries (vector->list (session-index-entry-order idx)))
  (define header (hasheq 'type "index-header" 'count (length entries)))
  (define lines (cons header (map message->jsexpr entries)))
  (ensure-parent-dirs! path)
  ;; Write atomically: build blob then overwrite
  (define blob
    (with-output-to-string (lambda ()
                             (for ([entry (in-list lines)])
                               (write-json entry)
                               (newline)))))
  (call-with-output-file path (lambda (out) (display blob out)) #:mode 'text #:exists 'truncate))

;; ============================================================
;; load-index
;; ============================================================

(define (load-index path)
  ;; Load a previously saved index from disk.
  ;; Returns an empty index if file does not exist.
  ;; Warns on stderr if the index is corrupted.
  (if (not (file-exists? path))
      (session-index (make-hash) (make-hash) (vector) (make-hash) (box #f) (make-semaphore 1))
      (let ()
        (define raw (jsonl-read-all-valid path))
        (cond
          [(null? raw)
           (fprintf
            (current-error-port)
            "WARNING: Session index corrupted and could not be loaded. Starting with empty index. Previous session list may be unavailable.\n")
           (session-index (make-hash) (make-hash) (vector) (make-hash) (box #f) (make-semaphore 1))]
          [else
           ;; First entry should be the header
           (define header (car raw))
           (define entries (map jsexpr->message (cdr raw)))
           ;; Rebuild index in memory
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
;; Query operations
;; ============================================================

(define (lookup-entry idx id)
  ;; Find entry by id. Returns message or #f.
  (hash-ref (session-index-by-id idx) id #f))

(define (children-of idx id)
  ;; Get children of a node. Returns list of messages.
  ;; Returns '() if id not found or has no children.
  (hash-ref (session-index-children idx) id '()))

(define (leaf-nodes idx)
  ;; Get all leaf nodes (nodes with no children).
  ;; Returns list of messages in append order.
  (define children (session-index-children idx))
  (for/list ([msg (in-vector (session-index-entry-order idx))]
             #:when (null? (hash-ref children (message-id msg) '())))
    msg))

(define (resolve-active-leaf idx)
  ;; Get the current active leaf.
  ;; Default: the last leaf in append order.
  ;; Returns #f if no entries exist.
  (define leaves (leaf-nodes idx))
  (if (null? leaves)
      #f
      (last leaves)))

;; ============================================================
;; Active Leaf API
;; ============================================================

(define (active-leaf idx)
  ;; Return the active leaf entry.
  ;; If an explicit active leaf id is set, return that entry.
  ;; Otherwise fall back to the last entry in append order.
  ;; Returns #f if no entries exist or if the marked id is not found.
  (define marked-id (unbox (session-index-active-leaf-id idx)))
  (cond
    [marked-id (lookup-entry idx marked-id)]
    [else
     ;; Default: last entry in append order
     (define order (session-index-entry-order idx))
     (if (= (vector-length order) 0)
         #f
         (vector-ref order (sub1 (vector-length order))))]))

(define (switch-leaf! idx entry-id)
  ;; Set the active leaf to the given entry id.
  ;; Returns the entry-id on success, #f if the id is not found in the index.
  (if (lookup-entry idx entry-id)
      (begin
        (set-box! (session-index-active-leaf-id idx) entry-id)
        entry-id)
      #f))

(define (mark-active-leaf! idx entry-id)
  ;; Mark an entry as the active leaf in the index.
  ;; Same semantics as switch-leaf! — explicit alias.
  ;; Returns the entry-id on success, #f if the id is not found.
  (if (lookup-entry idx entry-id)
      (begin
        (set-box! (session-index-active-leaf-id idx) entry-id)
        entry-id)
      #f))

;; ============================================================
;; Tree operations (#496)
;; ============================================================

(define (get-branch idx entry-id)
  ;; Walk entry→root, returning the path as a list of messages.
  ;; The root is first, the given entry is last.
  ;; Returns #f if entry-id is not found.
  (define (walk-up id acc)
    (define entry (lookup-entry idx id))
    (cond
      [(not entry) #f]
      [else
       (define new-acc (cons entry acc))
       (define pid (message-parent-id entry))
       (if pid
           (walk-up pid new-acc)
           new-acc)]))
  (walk-up entry-id '()))

(define (branch! idx entry-id)
  ;; Move active leaf to the given entry-id.
  ;; Returns the entry on success, #f if not found.
  (define entry (lookup-entry idx entry-id))
  (cond
    [(not entry) #f]
    [else
     (set-box! (session-index-active-leaf-id idx) entry-id)
     entry]))

(define (reset-leaf! idx)
  ;; Set active leaf to #f (use latest entry as default).
  (set-box! (session-index-active-leaf-id idx) #f)
  (void))

(define (append-to-leaf! idx entry)
  ;; Create child of active leaf, advance leaf pointer.
  ;; Adds entry to the index and updates parent-child map.
  ;; Returns the entry on success.
  (define parent (active-leaf idx))
  (define parent-id (if parent (message-id parent) #f))
  ;; Update entry's parent-id to match
  (define fixed-entry
    (if (and parent-id (not (message-parent-id entry)))
        (struct-copy message entry [parent-id parent-id])
        entry))
  ;; Add to by-id
  (hash-set! (session-index-by-id idx) (message-id fixed-entry) fixed-entry)
  ;; Add to children of parent
  (when parent-id
    (hash-update! (session-index-children idx)
                  parent-id
                  (lambda (lst) (append lst (list fixed-entry)))))
  ;; Initialize own children list
  (hash-set! (session-index-children idx) (message-id fixed-entry) '())
  ;; Append to entry-order vector — use vector->list, append, list->vector
  ;; through the by-id and children hashes (which ARE mutable)
  ;; Note: entry-order is immutable, so we create a new vector.
  ;; For session-index, callers should rebuild the index after bulk operations.
  ;; For single appends, the entry-order vector is best-effort (in-memory only).
  (set-box! (session-index-active-leaf-id idx) (message-id fixed-entry))
  fixed-entry)

(define (leaf-depth idx entry-id)
  ;; Depth from root to given entry (root = 0).
  ;; Returns #f if entry-id is not found.
  (define path (get-branch idx entry-id))
  (if path
      (sub1 (length path))
      #f))

;; ============================================================
;; Bookmark operations
;; ============================================================

;; Counter for generating unique bookmark IDs within the same millisecond
(define bm-counter 0)
(define bm-counter-mutex (make-semaphore 1))

;; Per-index semaphore for bookmark mutations (#115)
(struct session-index (by-id children entry-order bookmarks active-leaf-id bookmark-sem)
  #:transparent)

(define (make-bookmark id entry-id label timestamp)
  (bookmark id entry-id label timestamp))

(define (generate-bookmark-id)
  ;; Generate unique bookmark ID using timestamp + counter
  (semaphore-wait bm-counter-mutex)
  (set! bm-counter (add1 bm-counter))
  (define id (format "bm-~a-~a" (current-milliseconds) bm-counter))
  (semaphore-post bm-counter-mutex)
  id)

(define (add-bookmark! idx entry-id label)
  ;; Add a new bookmark for the given entry-id with the given label.
  ;; Returns the bookmark id.
  ;; Thread-safe: mutations protected by per-index semaphore (#115).
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define bookmarks (session-index-bookmarks idx))
                         ;; Remove existing bookmark with same label if present
                         (define existing (find-bookmark-by-label idx label))
                         (when existing
                           (hash-remove! bookmarks (bookmark-id existing)))
                         ;; Create new bookmark
                         (define id (generate-bookmark-id))
                         (define ts (current-seconds))
                         (define bm (make-bookmark id entry-id label ts))
                         (hash-set! bookmarks id bm)
                         id)))

(define (remove-bookmark! idx bookmark-id)
  ;; Remove bookmark by id. Returns #t if found and removed, #f otherwise.
  ;; Thread-safe: mutations protected by per-index semaphore (#115).
  (call-with-semaphore (session-index-bookmark-sem idx)
                       (lambda ()
                         (define bookmarks (session-index-bookmarks idx))
                         (if (hash-has-key? bookmarks bookmark-id)
                             (begin
                               (hash-remove! bookmarks bookmark-id)
                               #t)
                             #f))))

(define (list-bookmarks idx)
  ;; Returns list of all bookmarks in arbitrary order.
  (hash-values (session-index-bookmarks idx)))

(define (find-bookmark-by-label idx label)
  ;; Find bookmark by exact label match. Returns bookmark or #f.
  (define bookmarks (session-index-bookmarks idx))
  (for/first ([bm (in-hash-values bookmarks)]
              #:when (equal? (bookmark-label bm) label))
    bm))

(define (get-bookmark idx bookmark-id)
  ;; Get bookmark by id. Returns bookmark or #f.
  (hash-ref (session-index-bookmarks idx) bookmark-id #f))

;; ============================================================
;; Bookmark persistence (sidecar file)
;; ============================================================

(define (bookmarks-path session-path)
  ;; Derive bookmarks sidecar path from session path.
  ;; session.jsonl -> session-bookmarks.json
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
  ;; Save bookmarks to sidecar file.
  ;; Writes atomically using a temp file then rename.
  (define bpath (bookmarks-path session-path))
  (define bookmarks (list-bookmarks idx))
  (define data (map bookmark->jsexpr bookmarks))
  (ensure-parent-dirs! bpath)
  ;; Atomic write: write to temp, then rename
  (define temp-path (format "~a.tmp" bpath))
  (call-with-output-file temp-path
                         (lambda (out) (write-json data out))
                         #:mode 'text
                         #:exists 'truncate)
  (when (file-exists? bpath)
    (delete-file bpath))
  (rename-file-or-directory temp-path bpath))

(define (load-bookmarks session-path)
  ;; Load bookmarks from sidecar file.
  ;; Returns list of bookmarks (empty if file doesn't exist or is invalid).
  (define bpath (bookmarks-path session-path))
  (cond
    [(not (file-exists? bpath)) '()]
    [else
     (with-handlers ([exn:fail? (lambda (e) '())])
       (define data (call-with-input-file bpath (lambda (in) (read-json in)) #:mode 'text))
       (if (list? data)
           (map jsexpr->bookmark data)
           '()))]))

(define (load-index-with-bookmarks session-path index-path)
  ;; Load index and populate bookmarks from sidecar.
  (define idx (load-index index-path))
  ;; Rebuild bookmarks hash from loaded list
  (define bookmarks (load-bookmarks session-path))
  (define bm-hash (make-hash))
  (for ([bm (in-list bookmarks)])
    (hash-set! bm-hash (bookmark-id bm) bm))
  ;; Create new index with bookmarks
  (session-index (session-index-by-id idx)
                 (session-index-children idx)
                 (session-index-entry-order idx)
                 bm-hash
                 (box #f)
                 (session-index-bookmark-sem idx)))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (ensure-parent-dirs! path)
  (define dir (let ([p (path-only path)]) (if p p #f)))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir)))

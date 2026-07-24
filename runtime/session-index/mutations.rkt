#lang racket/base

;; runtime/session-index/mutations.rkt — append, update, branch, bookmark, persist operations
;;
;; R-6: Session index is now fully immutable — hash-set/hash-update/hash-remove
;; return new hashes; no more mutable hash round-trip.

(require "../../util/error/error-helpers.rkt")
(require racket/contract
         racket/string
         racket/file
         racket/port
         racket/path
         racket/list
         racket/vector
         json
         "../../util/json/json-helpers.rkt"
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/message/message.rkt"
                  message-id
                  message-parent-id
                  message-kind
                  message-role
                  make-message
                  message
                  message?)
         (only-in "../../util/message/message.rkt" message->jsexpr jsexpr->message)
         (only-in "../../util/message/message-helpers.rkt" ensure-parent-dirs!)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/json/jsonl.rkt" jsonl-read-all-valid)
         (only-in "../session/session-store.rkt" load-session-log)
         "schema.rkt"
         "query.rkt"
         "bookmarks.rkt")

(provide (contract-out
          [build-index! (-> path-string? path-string? session-index?)]
          [load-index (-> path-string? session-index?)]
          [save-index! (-> path-string? session-index? void?)]
          [switch-leaf! (-> session-index? (or/c string? #f) (or/c string? #f))]
          [mark-active-leaf! (-> session-index? (or/c string? #f) (or/c string? #f))]
          [navigate-to-entry! (-> session-index? (or/c string? #f) (or/c navigate-result? #f))]
          [navigate-to-leaf! (-> session-index? (or/c string? #f) (or/c navigate-result? #f))]
          [navigate-next-leaf! (-> session-index? (or/c navigate-result? #f))]
          [navigate-prev-leaf! (-> session-index? (or/c navigate-result? #f))]
          [branch! (-> session-index? (or/c string? #f) (or/c message? #f))]
          [branch-with-summary!
           (-> session-index? (or/c string? #f) string? (values session-index? (or/c message? #f)))]
          [reset-leaf! (-> session-index? void?)]
          [append-to-leaf! (-> session-index? message? (values session-index? message?))]))
(provide load-index-with-bookmarks
         (all-from-out "bookmarks.rkt"))

;; ============================================================
;; build-index!
;; ============================================================

(define (build-index! session-path index-path)
  (define messages (load-session-log session-path))
  ;; R-6: Use immutable hashes throughout — no mutable hash round-trip.
  (define-values (by-id-0 children-0)
    (for/fold ([bid (hash)]
               [chd (hash)])
              ([msg (in-list messages)])
      (values (hash-set bid (message-id msg) msg) (hash-set chd (message-id msg) '()))))
  ;; Infer missing parentIds from log order
  (define fixed-messages
    (for/list ([msg (in-list messages)]
               [i (in-naturals)])
      (cond
        [(and (not (message-parent-id msg)) (> i 0) (not (eq? (message-kind msg) 'session-info)))
         (define prev-msg (list-ref messages (sub1 i)))
         (struct-copy message msg [parent-id (message-id prev-msg)])]
        [else msg])))
  (define by-id-1
    (for/fold ([bid by-id-0]) ([msg (in-list fixed-messages)])
      (hash-set bid (message-id msg) msg)))
  (define children-final
    (for/fold ([chd children-0]) ([msg (in-list fixed-messages)])
      (define pid (message-parent-id msg))
      (if (and pid (hash-has-key? chd pid))
          (hash-update chd pid (lambda (lst) (append lst (list msg))))
          chd)))
  ;; R-6: Use immutable hashes directly in session-index struct
  (define idx
    (session-index by-id-1
                   children-final
                   (list->vector fixed-messages)
                   (hash)
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
           ;; R-6: Use immutable hashes throughout — no mutable hash round-trip.
           (define-values (by-id children-0)
             (for/fold ([bid (hash)]
                        [chd (hash)])
                       ([msg (in-list entries)])
               (values (hash-set bid (message-id msg) msg) (hash-set chd (message-id msg) '()))))
           (define children
             (for/fold ([chd children-0]) ([msg (in-list entries)])
               (define pid (message-parent-id msg))
               (if (and pid (hash-has-key? chd pid))
                   (hash-update chd pid (lambda (lst) (append lst (list msg))))
                   chd)))
           ;; R-6: Use immutable hashes directly — no mutable conversion
           (session-index by-id
                          children
                          (list->vector entries)
                          (hash)
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
    [(not entry) (values idx #f)]
    [else
     (define summary-msg
       (make-message (string-append "bs-" (generate-id))
                     entry-id
                     'system
                     'branch-summary
                     (list (make-text-part summary-text))
                     (current-seconds)
                     (hasheq 'type "branch-summary" 'from-id entry-id)))
     ;; R-6: append-to-leaf! now returns (values idx entry); capture updated index
     (define-values (new-idx _summary-msg) (append-to-leaf! idx summary-msg))
     (values new-idx summary-msg)]))

(define (reset-leaf! idx)
  (set-box! (session-index-active-leaf-id idx) #f)
  (void))

;; R-6: Returns (values session-index? message?) — immutable hash operations
(define (append-to-leaf! idx entry)
  (define parent (active-leaf idx))
  ;; Honor an explicit parent; otherwise attach to the active leaf. Children
  ;; and by-id must describe the same topology.
  (define parent-id (or (message-parent-id entry) (and parent (message-id parent))))
  (define fixed-entry
    (if (and parent-id (not (message-parent-id entry)))
        (struct-copy message entry [parent-id parent-id])
        entry))
  ;; R-6: Use immutable hash-set/hash-update instead of hash-set!/hash-update!
  (define new-by-id (hash-set (session-index-by-id idx) (message-id fixed-entry) fixed-entry))
  (define new-children
    (if parent-id
        (hash-update (session-index-children idx)
                     parent-id
                     (lambda (lst) (append lst (list fixed-entry))))
        (session-index-children idx)))
  (define new-children2 (hash-set new-children (message-id fixed-entry) '()))
  (set-box! (session-index-active-leaf-id idx) (message-id fixed-entry))
  ;; Return updated index with immutable hashes, along with the entry
  (values (session-index new-by-id
                         new-children2
                         (vector-append (session-index-entry-order idx) (vector fixed-entry))
                         (session-index-bookmarks idx)
                         (session-index-active-leaf-id idx)
                         (session-index-bookmark-sem idx))
          fixed-entry))

;; R-6: Use immutable hashes for bookmarks in the index
(define (load-index-with-bookmarks session-path index-path)
  (define idx (load-index index-path))
  (define bookmarks (load-bookmarks session-path))
  (define bm-hash
    (for/fold ([h (hash)]) ([bm (in-list bookmarks)])
      (hash-set h (bookmark-id bm) bm)))
  (session-index (session-index-by-id idx)
                 (session-index-children idx)
                 (session-index-entry-order idx)
                 bm-hash
                 (box #f)
                 (session-index-bookmark-sem idx)))

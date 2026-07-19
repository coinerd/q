#lang racket/base

;; runtime/session/session-repository.rkt
;; Session-owned opaque capability repository.
;;
;; Owns the atomic no-follow filesystem boundary (session-filesystem.rkt) on
;; behalf of a session root. The repository is the only owner of directory
;; capabilities; consumers receive capabilities by reference but never re-resolve
;; pathnames. This closes the check-to-use race (finding F-02/A-03): a capability
;; is acquired once via a held descriptor and every subsequent operation is
;; relative to that descriptor.
;;
;; W2 scope: foundation only. No production session-store cutover (that is W3).

(require racket/match
         racket/set
         racket/string
         racket/port
         json
         "session-filesystem.rkt"
         (only-in "session-store-integrity.rkt" compute-event-hash GENESIS-HASH)
         (only-in "../../util/message/message.rkt" message->jsexpr message?)
         (only-in "../session-store/versioning.rkt" make-version-header-message))

;; Repository lifecycle
(provide make-session-repository ; root-path-string -> session-repository?
         session-repository?
         session-repository-root ; repository -> path-string
         close-session-repository! ; repository -> void (closes the held root cap)
         ;; Session directory access (capabilities owned by the repository)
         repository-open-session-dir ; repository session-id -> session-dir-cap (NOFOLLOW)
         repository-create-session-dir ; repository session-id -> session-dir-cap
         repository-session-dir-exists? ; repository session-id -> boolean
         ;; Convenience: read/append/atomic-replace/unlink/list/fork via the repository
         repository-read-artifact
         repository-append-artifact
         repository-atomic-replace-artifact
         repository-unlink-artifact
         repository-list-artifacts
         repository-fork-session
         ;; Backend descriptor (passthrough)
         repository-backend-supported?
         ;; W3: integrity-preserving no-follow event operations
         repository-artifact-exists?
         repository-ensure-session!
         repository-load-events
         repository-read-last-hash
         repository-append-event!
         repository-write-version-header!
         repository-write-pending-marker!
         repository-remove-pending-marker!
         repository-has-pending-marker?
         repository-fork-event-log!)

;; ---------------------------------------------------------------------------
;; Repository
;; ---------------------------------------------------------------------------

;; A session-repository owns the root directory capability and tracks which
;; session directory capabilities it has handed out (for fail-closed teardown).
(struct session-repository
        (root-path ; the canonical root path (diagnostics only — never re-opened)
         [root-cap #:mutable] ; held opaque capability for the root directory
         open-caps ; (set) tracked session dir capabilities (diagnostics)
         )
  #:transparent)

(define (make-session-repository root-path)
  (define cap (open-session-root root-path))
  (session-repository root-path cap (mutable-set)))

(define (session-repository-root repo)
  (session-repository-root-path repo))

(define (repository-backend-supported?)
  (filesystem-backend-supported?))

(define (close-session-repository! repo)
  (define cap (session-repository-root-cap repo))
  (when cap
    (close-session-dir-cap cap)
    (set-session-repository-root-cap! repo #f)))

;; Acquire a session directory capability. The capability is tracked by the
;; repository. The returned capability is opaque; the caller must not re-resolve
;; it to a pathname.
(define (repository-open-session-dir repo session-id)
  (define dir-cap (open-session-dir (session-repository-root-cap repo) session-id))
  (set-add! (session-repository-open-caps repo) dir-cap)
  dir-cap)

(define (repository-create-session-dir repo session-id)
  (define dir-cap (create-session-dir (session-repository-root-cap repo) session-id))
  (set-add! (session-repository-open-caps repo) dir-cap)
  dir-cap)

(define (repository-session-dir-exists? repo session-id)
  (session-dir-exists? (session-repository-root-cap repo) session-id))

(define (repository-read-artifact repo session-id name)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (read-artifact dir-cap name))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-append-artifact repo session-id name data)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (append-artifact dir-cap name data))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-atomic-replace-artifact repo session-id name data)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (atomic-replace-artifact dir-cap name data))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-unlink-artifact repo session-id name)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (unlink-artifact dir-cap name))
                (lambda () (close-session-dir-cap dir-cap))))

(define (repository-list-artifacts repo session-id)
  (define dir-cap (repository-open-session-dir repo session-id))
  (dynamic-wind void
                (lambda () (list-artifacts dir-cap))
                (lambda () (close-session-dir-cap dir-cap))))

;; Fork a session into a destination session under the same repository root.
;; Source and destination are held independently; both no-follow.
(define (repository-fork-session repo src-session-id dst-session-id)
  (fork-session-dirs (session-repository-root-cap repo)
                     src-session-id
                     (session-repository-root-cap repo)
                     dst-session-id))

;; ============================================================
;; W3: integrity-preserving no-follow event operations
;;
;; These faithfully reproduce the session-store append protocol (pending
;; marker -> read-last-hash -> compute-event-hash -> JSONL append -> remove
;; marker) but every filesystem effect is relative to a held session-dir
;; capability with O_NOFOLLOW/AT_SYMLINK_NOFOLLOW. No pathname re-resolution.
;; The pure hash functions are reused from session-store-integrity.rkt so the
;; on-disk hash chain is byte-compatible with the path-based store.
;; ============================================================

(define session-log-name "session.jsonl")
(define session-pending-name "session.jsonl.pending")

(define (jsexpr->jsonl-bytes entry)
  (define out (open-output-string))
  (write-json entry out)
  (newline out)
  (string->bytes/utf-8 (get-output-string out)))

(define (parse-jsonl-bytes bs)
  (for/list ([line (in-lines (open-input-bytes bs))]
             #:when (non-empty-string? (string-trim line)))
    (read-json (open-input-string line))))

(define (repository-artifact-exists? repo session-id name)
  (and (repository-session-dir-exists? repo session-id)
       (let ([dir-cap (repository-open-session-dir repo session-id)])
         (dynamic-wind void
                       (lambda () (and (artifact-stat dir-cap name) #t))
                       (lambda () (close-session-dir-cap dir-cap))))))

(define (repository-ensure-session! repo session-id)
  (unless (repository-session-dir-exists? repo session-id)
    (repository-create-session-dir repo session-id)))

;; Read session.jsonl bytes, or #f if the session dir or log is absent.
(define (repository-read-log-bytes repo session-id)
  (cond
    [(not (repository-session-dir-exists? repo session-id)) #f]
    [else
     (define dir-cap (repository-open-session-dir repo session-id))
     (dynamic-wind void
                   (lambda ()
                     (with-handlers ([exn:fail? (lambda (_) #f)])
                       (read-artifact dir-cap session-log-name)))
                   (lambda () (close-session-dir-cap dir-cap)))]))

(define (repository-load-events repo session-id)
  (define bs (repository-read-log-bytes repo session-id))
  (if (not bs)
      '()
      (parse-jsonl-bytes bs)))

(define (repository-read-last-hash repo session-id)
  (define bs (repository-read-log-bytes repo session-id))
  (cond
    [(not bs) GENESIS-HASH]
    [(zero? (bytes-length bs)) GENESIS-HASH]
    [else
     (define entries (parse-jsonl-bytes bs))
     (if (null? entries)
         GENESIS-HASH
         (let ([last (car (reverse entries))])
           (if (hash? last)
               (hash-ref last 'hash GENESIS-HASH)
               GENESIS-HASH)))]))

(define (repository-write-pending-marker! repo session-id entry-count)
  (repository-ensure-session! repo session-id)
  ;; Defense-in-depth (W3): a pre-existing pending marker that is a symlink is
  ;; evidence of tampering. Fail closed rather than silently replacing an
  ;; attacker's link, even though the no-follow atomic replace would neutralize
  ;; it. The stat is no-follow (fstatat AT_SYMLINK_NOFOLLOW).
  (let ([dir-cap (repository-open-session-dir repo session-id)])
    (dynamic-wind void
                  (lambda ()
                    (define existing (artifact-stat dir-cap session-pending-name))
                    (when (stat-link? existing)
                      (error 'repository-write-pending-marker!
                             "tamper detected: pending marker is a symlink for session ~a"
                             session-id)))
                  (lambda () (close-session-dir-cap dir-cap))))
  (repository-atomic-replace-artifact repo
                                      session-id
                                      session-pending-name
                                      (string->bytes/utf-8 (number->string entry-count))))

(define (repository-remove-pending-marker! repo session-id)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (repository-unlink-artifact repo session-id session-pending-name)))

(define (repository-has-pending-marker? repo session-id)
  (repository-artifact-exists? repo session-id session-pending-name))

;; Atomically install the version header iff the log is empty/absent. Mirrors
;; write-session-version-header!: truncate-to-header only when no content.
(define (repository-write-version-header! repo session-id)
  (repository-ensure-session! repo session-id)
  (define bs (repository-read-log-bytes repo session-id))
  (when (or (not bs) (zero? (bytes-length bs)))
    (define header-msg (make-version-header-message))
    (repository-atomic-replace-artifact repo
                                        session-id
                                        session-log-name
                                        (jsexpr->jsonl-bytes (message->jsexpr header-msg)))))

;; Full hash-chained append, no-follow end to end.
(define (repository-append-event! repo session-id msg)
  (repository-ensure-session! repo session-id)
  (repository-write-pending-marker! repo session-id 1)
  (dynamic-wind
   void
   (lambda ()
     (define prev-hash (repository-read-last-hash repo session-id))
     (define entry (message->jsexpr msg))
     (define h (compute-event-hash entry prev-hash))
     (define chained (hash-set* entry 'prev_hash prev-hash 'hash h))
     (repository-append-artifact repo session-id session-log-name (jsexpr->jsonl-bytes chained)))
   (lambda () (repository-remove-pending-marker! repo session-id))))

;; Fork: load source events, walk the ancestor path of maybe-entry-id (via
;; 'id/'parentId fields), prepend a version header, and append all to the
;; destination under the same held root — no-follow throughout. Like the
;; path-based fork-session!, this is a plain append that preserves the source
;; chain (no re-chaining).
(define (repository-fork-event-log! repo src-session-id dst-session-id maybe-entry-id)
  (repository-ensure-session! repo dst-session-id)
  (define src-events (repository-load-events repo src-session-id))
  (define by-id
    (for/hash ([e (in-list src-events)]
               #:when (hash? e))
      (values (hash-ref e 'id #f) e)))
  (define (walk-up id acc)
    (define e (and id (hash-ref by-id id #f)))
    (cond
      [(not e) acc]
      [else
       (define new-acc (cons e acc))
       (walk-up (hash-ref e 'parentId #f) new-acc)]))
  (define path-entries (walk-up maybe-entry-id '()))
  (define header-msg (make-version-header-message #:extra (hasheq 'parentSession src-session-id)))
  (define all-bytes
    (apply bytes-append
           (jsexpr->jsonl-bytes (message->jsexpr header-msg))
           (map (lambda (e) (jsexpr->jsonl-bytes e)) path-entries)))
  (repository-atomic-replace-artifact repo dst-session-id session-log-name all-bytes)
  (length path-entries))

#lang racket/base
;; STABILITY: evolving

;;; runtime/session-store.rkt — append-only JSONL session storage (facade)
;;;
;;; v0.22.9 W2: Decomposed into:
;;;   session-store-integrity.rkt — hash chain, verification, repair, write-ahead markers
;;;   session-store-tree.rkt      — tree store operations
;;; This module retains: append, load/replay, versioning, forking, import,
;;; naming, in-memory session manager, custom entry helpers.
;;; Re-exports all symbols from extracted modules for backward compatibility.

(require racket/contract
         racket/file
         racket/string
         racket/port
         json
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt"
         (only-in "../util/message-helpers.rkt" ensure-parent-dirs!)
         ;; Extracted modules (v0.22.9 W2)
         "session-store-integrity.rkt"
         "session-store-tree.rkt")

(provide (contract-out [append-entry! (path-string? message? . -> . void?)]
                       [append-entries! (path-string? (listof message?) . -> . void?)]
                       [load-session-log (->* (path-string?) ((or/c #f string?)) (listof message?))]
                       [replay-session (path-string? . -> . (listof message?))]
                       [has-pending-marker? (path-string? . -> . boolean?)]
                       [pending-marker-path (path-string? . -> . path?)])
         ;; Re-exported from session-store-integrity.rkt
         GENESIS-HASH
         compute-event-hash
         verify-hash-chain
         verify-session-integrity
         repair-session-log!
         ;; Re-exported from session-store-tree.rkt
         append-tree-entry!
         load-tree
         get-tree-branch
         get-children
         resolve-active-branch
         tree-info
         ;; Session versioning (#499)
         CURRENT-SESSION-VERSION
         write-session-version-header!
         ensure-session-version-header!
         migrate-session-log!
         ;; Session forking (#500)
         fork-session!
         ;; Session import (#1113)
         import-session!
         ;; Session naming
         write-session-name!
         ;; In-memory session manager (GC-18)
         in-memory-session-manager?
         make-in-memory-session-manager
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!
         ;; Custom entry helpers (#1147)
         append-custom-entry!
         load-custom-entries)

;; ── Append operations ──

(define (append-entry! path msg)
  (write-pending-marker! path 1)
  (dynamic-wind void
                (lambda ()
                  (define prev-hash (read-last-hash path))
                  (define entry (message->jsexpr msg))
                  (define h (compute-event-hash entry prev-hash))
                  (define chained-entry (hash-set* entry 'prev_hash prev-hash 'hash h))
                  (jsonl-append! path chained-entry))
                (lambda () (remove-pending-marker! path))))

(define (append-entries! path msgs)
  (when (null? msgs)
    (void))
  (unless (null? msgs)
    (write-pending-marker! path (length msgs))
    (dynamic-wind void
                  (lambda ()
                    (define prev-hash (read-last-hash path))
                    (define prev-box (box prev-hash))
                    (define jsexprs
                      (for/list ([msg (in-list msgs)])
                        (define entry (message->jsexpr msg))
                        (define prev (unbox prev-box))
                        (define h (compute-event-hash entry prev))
                        (set-box! prev-box h)
                        (hash-set* entry 'prev_hash prev 'hash h)))
                    (jsonl-append-entries! path jsexprs))
                  (lambda () (remove-pending-marker! path)))))

;; ── Load / replay ──

(define (load-session-log path [session-id #f])
  (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count path))
  (when (> corrupted-count 0)
    (fprintf (current-error-port)
             "WARNING: Skipped ~a corrupted lines in ~a. Run 'q sessions repair' to fix.\n"
             corrupted-count
             (or session-id path)))
  (map jsexpr->message raw))

(define (replay-session path)
  (load-session-log path))

;; ============================================================
;; Session versioning (#499)
;; ============================================================

(define CURRENT-SESSION-VERSION 2)

(define (write-session-version-header! log-path)
  (ensure-parent-dirs! log-path)
  (cond
    [(and (file-exists? log-path) (> (file-size log-path) 0)) (void)]
    [else
     (define header-msg
       (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                     #f
                     'system
                     'session-info
                     '()
                     (current-seconds)
                     (hasheq 'version CURRENT-SESSION-VERSION)))
     (call-with-output-file log-path
                            (lambda (out)
                              (write-json (message->jsexpr header-msg) out)
                              (newline out))
                            #:mode 'text
                            #:exists 'truncate)]))

(define (read-first-log-entry log-path)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "session-store: ~a" (exn-message e)))
                               #f)])
    (call-with-input-file log-path
                          (lambda (in)
                            (define line (read-line in))
                            (if (eof-object? line)
                                #f
                                (jsexpr->message (string->jsexpr line))))
                          #:mode 'text)))

(define (ensure-session-version-header! log-path)
  (cond
    [(not (file-exists? log-path))
     (write-session-version-header! log-path)
     CURRENT-SESSION-VERSION]
    [(= (file-size log-path) 0)
     (write-session-version-header! log-path)
     CURRENT-SESSION-VERSION]
    [else
     (define first-entry (read-first-log-entry log-path))
     (cond
       [(not first-entry)
        (write-session-version-header! log-path)
        CURRENT-SESSION-VERSION]
       [(session-info-entry? first-entry) (hash-ref (message-meta first-entry) 'version 1)]
       [else
        (migrate-session-log! log-path 1 CURRENT-SESSION-VERSION)
        CURRENT-SESSION-VERSION])]))

(define (migrate-session-log! log-path from-version to-version)
  (when (< from-version to-version)
    (define entries (load-session-log log-path))
    (define header-msg
      (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                    #f
                    'system
                    'session-info
                    '()
                    (current-seconds)
                    (hasheq 'version to-version)))
    (define all-entries (cons header-msg entries))
    (define bak-path (format "~a.v~a.bak" log-path from-version))
    (when (file-exists? bak-path)
      (delete-file bak-path))
    (rename-file-or-directory log-path bak-path #t)
    (jsonl-append-entries! log-path (map message->jsexpr all-entries))
    (fprintf (current-error-port)
             "Session migrated v~a -> v~a: ~a entries. Backup: ~a\n"
             from-version
             to-version
             (length entries)
             bak-path)))

;; ============================================================
;; Session forking (#500)
;; ============================================================

(define (fork-session! source-path source-entry-id dest-path)
  (define entries (load-session-log source-path))
  (define by-id (make-hash))
  (for ([e (in-list entries)])
    (hash-set! by-id (message-id e) e))
  (define (walk-up id acc)
    (define e (hash-ref by-id id #f))
    (cond
      [(not e) acc]
      [else
       (define new-acc (cons e acc))
       (define pid (message-parent-id e))
       (if pid
           (walk-up pid new-acc)
           new-acc)]))
  (define path-entries (walk-up source-entry-id '()))
  (ensure-parent-dirs! dest-path)
  (define header-msg
    (make-message
     (format "session-version-header-~a" (current-inexact-milliseconds))
     #f
     'system
     'session-info
     '()
     (current-seconds)
     (hasheq 'version CURRENT-SESSION-VERSION 'parentSession (path->string source-path))))
  (define all-entries (cons header-msg path-entries))
  (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
  (length path-entries))

;; ============================================================
;; Session naming
;; ============================================================

(define (write-session-name! log-path name)
  (define name-msg
    (make-message (format "session-name-~a" (current-inexact-milliseconds))
                  #f
                  'system
                  'session-info
                  (list (make-text-part (format "Session renamed: ~a" name)))
                  (current-seconds)
                  (hasheq 'name name)))
  (append-entry! log-path name-msg))

;; ============================================================
;; Session import (#1113)
;; ============================================================

(define (import-session! source-path dest-session-dir)
  (cond
    [(not (file-exists? source-path))
     (error 'import-session! "Source file not found: ~a" source-path)]
    [else
     (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count source-path))
     (when (> corrupted-count 0)
       (fprintf (current-error-port)
                "WARNING: ~a corrupted lines skipped during import from ~a\n"
                corrupted-count
                source-path))
     (when (null? raw)
       (error 'import-session! "No valid entries found in source: ~a" source-path))
     (define new-session-id (format "imported-~a" (current-inexact-milliseconds)))
     (define dest-path (build-path dest-session-dir (format "~a.jsonl" new-session-id)))
     (ensure-parent-dirs! dest-path)
     (define header-msg
       (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
                     #f
                     'system
                     'session-info
                     '()
                     (current-seconds)
                     (hasheq 'version
                             CURRENT-SESSION-VERSION
                             'imported-from
                             (path->string source-path)
                             'imported-at
                             (current-seconds))))
     (define messages (map jsexpr->message raw))
     (define all-entries (cons header-msg messages))
     (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
     new-session-id]))

;; ============================================================
;; In-memory session manager (GC-18)
;; ============================================================

(struct in-memory-session-manager (sessions-box) #:transparent)

(define (make-in-memory-session-manager)
  (in-memory-session-manager (box (hash))))

(define (in-memory-append! mgr session-id msg)
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  (define existing (hash-ref sessions session-id '()))
  (set-box! box (hash-set sessions session-id (append existing (list msg)))))

(define (in-memory-append-entries! mgr session-id msgs)
  (for ([msg (in-list msgs)])
    (in-memory-append! mgr session-id msg)))

(define (in-memory-load mgr session-id)
  (define sessions (unbox (in-memory-session-manager-sessions-box mgr)))
  (hash-ref sessions session-id '()))

(define (in-memory-list-sessions mgr)
  (hash-keys (unbox (in-memory-session-manager-sessions-box mgr))))

(define (in-memory-fork! mgr src-id dest-id [entry-id #f])
  (define entries (in-memory-load mgr src-id))
  (define to-copy
    (if entry-id
        (let loop ([es entries]
                   [acc '()])
          (cond
            [(null? es) (reverse acc)]
            [(equal? (message-id (car es)) entry-id) (reverse (cons (car es) acc))]
            [else (loop (cdr es) (cons (car es) acc))]))
        entries))
  (define box (in-memory-session-manager-sessions-box mgr))
  (define sessions (unbox box))
  (set-box! box (hash-set sessions dest-id to-copy))
  dest-id)

;; ============================================================
;; Custom entry helpers (#1147)
;; ============================================================

(define (append-custom-entry! mgr session-id extension-name key data)
  (define entry (make-custom-entry extension-name key data))
  (in-memory-append! mgr session-id entry))

(define (load-custom-entries mgr session-id extension-name [key #f])
  (define entries (in-memory-load mgr session-id))
  (filter (lambda (e)
            (and (custom-entry? e)
                 (equal? (custom-entry-extension e) extension-name)
                 (or (not key) (equal? (custom-entry-key e) key))))
          entries))

;; ── Session-info helper ──

(define (session-info-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'session-info)))

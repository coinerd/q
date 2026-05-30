#lang racket/base
;; STABILITY: evolving

;;; runtime/session-store.rkt — append-only JSONL session storage (facade)
;;;
;;; v0.22.9 W2: Decomposed into:
;;;   session-store-integrity.rkt — hash chain, verification, repair, write-ahead markers
;;;   session-store-tree.rkt      — tree store operations
;;; v0.72.5 W0: Further decomposed into:
;;;   session-store/versioning.rkt — session versioning constants and functions
;;;   session-store/in-memory.rkt  — in-memory session manager and custom entry helpers
;;; This module retains: append, load/replay, forking, import, naming, sinks.
;;; Re-exports all symbols from extracted modules for backward compatibility.

(require racket/contract
         racket/match
         racket/file
         racket/string
         racket/port
         racket/class
         json
         "../runtime/goal-state.rkt"
         "../util/protocol-types.rkt"
         "../util/jsonl.rkt"
         (only-in "../util/message-helpers.rkt" ensure-parent-dirs!)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../util/errors.rkt" raise-session-error)
         ;; Extracted modules (v0.22.9 W2)
         "session-store-integrity.rkt"
         "session-store-tree.rkt"
         ;; Extracted sub-modules (v0.72.5 W0)
         "session-store/versioning.rkt"
         "session-store/in-memory.rkt")

;; F11: Consumer/Admin tier split:
;;   Consumer tier (always safe): append-entry!, load-session-log,
;;     session-exists?, fork-session!, session naming
;;   Admin tier (integrity tools): verify-hash-chain, repair-session-log!,
;;     import-session!, migrate-session-log!
;;   Submodules provide tiered re-exports; main module re-exports both.
(provide (contract-out [append-entry! (path-string? message? . -> . void?)]
                       [append-entries! (path-string? (listof message?) . -> . void?)]
                       [load-session-log (->* (path-string?) ((or/c #f string?)) (listof message?))]
                       [replay-session (path-string? . -> . (listof message?))]
                       [has-pending-marker? (path-string? . -> . boolean?)]
                       [pending-marker-path (path-string? . -> . path?)])
         ;; Versioning, forking, import, naming — contracted (v0.51.5)
         (contract-out [write-session-version-header! (-> path-string? void?)]
                       [ensure-session-version-header! (-> path-string? exact-nonnegative-integer?)]
                       [migrate-session-log!
                        (-> path-string? exact-nonnegative-integer? exact-nonnegative-integer? void?)]
                       [fork-session!
                        (-> path-string? (or/c string? #f) path-string? exact-nonnegative-integer?)]
                       [import-session! (-> path-string? path-string? string?)]
                       [write-session-name! (-> path-string? string? void?)])
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
         ;; Session versioning constant
         CURRENT-SESSION-VERSION
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
         load-custom-entries
         ;; Goal state persistence (v0.71.0)
         append-goal-state!
         load-latest-goal-state
         ;; R-09/R-10: Session sink interface (v0.39.0)
         session-sink<%>
         file-session-sink%
         in-memory-session-sink%
         ;; v0.70.5: Async session sink (opt-in)
         async-session-sink%)

;; ── R-09/R-10: Session sink interface (v0.39.0) ──
;;
;; A session-sink abstracts the storage backend for session entries.
;; Production code uses file-session-sink%; tests can use in-memory-session-sink%.

(define session-sink<%>
  (interface ()
    ;; Append a single message to the sink.
    sink-append!
    ;; Append multiple messages atomically.
    sink-append-entries!
    ;; Load all messages from the sink.
    sink-load
    ;; Fork the sink at a given entry.
    sink-fork!))

(define file-session-sink%
  (class* object% (session-sink<%>)
    (init-field log-path)
    (super-new)
    (define/public (sink-append! msg) (append-entry! log-path msg))
    (define/public (sink-append-entries! msgs) (append-entries! log-path msgs))
    (define/public (sink-load) (load-session-log log-path))
    (define/public (sink-fork! entry-id dest-path) (fork-session! log-path entry-id dest-path))))

(define in-memory-session-sink%
  (class* object% (session-sink<%>)
    (init-field manager session-id)
    (super-new)
    (define/public (sink-append! msg) (in-memory-append! manager session-id msg))
    (define/public (sink-append-entries! msgs) (in-memory-append-entries! manager session-id msgs))
    (define/public (sink-load) (in-memory-load manager session-id))
    (define/public (sink-fork! entry-id dest-id)
      (in-memory-fork! manager session-id dest-id entry-id))))

;; v0.70.5: Async session sink wrapper — opt-in only, preserves order via single worker thread
(define async-session-sink%
  (class* object% (session-sink<%>)
    (init-field inner-sink [capacity 100] [policy 'block])
    (super-new)
    (define closed-box (box #f))
    (define space-sema (make-semaphore capacity))
    (define flush-ch (make-channel))

    (define worker
      (thread (lambda ()
                (let loop ()
                  (define msg (thread-receive))
                  (cond
                    [(eq? msg 'flush)
                     ;; Flush inner sink if it supports flush (e.g. nested async)
                     (when (object-method-arity-includes? inner-sink 'sink-flush! 0)
                       (send inner-sink sink-flush!))
                     (channel-put flush-ch 'ok)
                     (loop)]
                    [(eq? msg 'stop) (void)]
                    [(pair? msg)
                     ;; msg is a pair: (cons 'entries list) or (cons 'single message)
                     (case (car msg)
                       [(entries)
                        (send inner-sink sink-append-entries! (cdr msg))
                        (semaphore-post space-sema)]
                       [(single)
                        (send inner-sink sink-append! (cdr msg))
                        (semaphore-post space-sema)])
                     (loop)]
                    [else
                     (semaphore-post space-sema)
                     (loop)])))))

    (define (try-send! item)
      (case policy
        [(block)
         (semaphore-wait space-sema)
         (thread-send worker item)]
        [(drop-new)
         (if (semaphore-try-wait? space-sema)
             (thread-send worker item)
             (void))]
        [(drop-old)
         ;; drop-old not fully implemented — falls back to drop-new with warning
         (unless (semaphore-try-wait? space-sema)
           (log-warning "async-session-sink: drop-old not fully implemented, dropping new entry"))
         (when (semaphore-try-wait? space-sema)
           (thread-send worker item))]
        [else
         (semaphore-wait space-sema)
         (thread-send worker item)]))

    (define/public (sink-append! msg)
      (unless (unbox closed-box)
        (try-send! (cons 'single msg))))

    (define/public (sink-append-entries! msgs)
      (unless (unbox closed-box)
        (try-send! (cons 'entries msgs))))

    (define/public (sink-load) (send inner-sink sink-load))

    (define/public (sink-fork! entry-id dest-id) (send inner-sink sink-fork! entry-id dest-id))

    ;; Non-interface methods for flush/close contract
    (define/public (sink-get-worker) worker)
    (define/public (sink-flush!)
      (unless (unbox closed-box)
        (try-send! 'flush)
        (channel-get flush-ch)))

    (define/public (sink-close!)
      (unless (unbox closed-box)
        (set-box! closed-box #t)
        (try-send! 'stop)
        (thread-wait worker)))))

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
    (make-version-header-message #:extra (hasheq 'parentSession (path->string source-path))))
  (define all-entries (cons header-msg path-entries))
  (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
  (length path-entries))

;; ============================================================
;; Session naming
;; ============================================================

(define (write-session-name! log-path name)
  (define name-msg
    (make-message (string-append "sn-" (generate-id))
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
     (raise-session-error 'import-session! "Source file not found" #f source-path)]
    [else
     (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count source-path))
     (when (> corrupted-count 0)
       (fprintf (current-error-port)
                "WARNING: ~a corrupted lines skipped during import from ~a\n"
                corrupted-count
                source-path))
     (when (null? raw)
       (raise-session-error 'import-session! "No valid entries found in source" #f source-path))
     (define new-session-id (string-append "imported-" (generate-id)))
     (define dest-path (build-path dest-session-dir (format "~a.jsonl" new-session-id)))
     (ensure-parent-dirs! dest-path)
     (define header-msg
       (make-version-header-message
        #:extra (hasheq 'imported-from (path->string source-path) 'imported-at (current-seconds))))
     (define messages (map jsexpr->message raw))
     (define all-entries (cons header-msg messages))
     (jsonl-append-entries! dest-path (map message->jsexpr all-entries))
     new-session-id]))

;; ============================================================
;; Goal state persistence (v0.71.0)
;; ============================================================

(define (append-goal-state! path gs)
  ;; Store goal-state as a JSON string in a system message
  ;; so it survives message->jsexpr serialization
  (define gs-json-str (jsexpr->string (goal-state->hash gs)))
  (append-entry! path
                 (make-message (goal-state-id gs)
                               #f
                               'system
                               'goal-state
                               (list (make-text-part gs-json-str))
                               (goal-state-updated-at gs)
                               #f)))

(define (load-latest-goal-state path)
  (define entries (load-session-log path))
  (define goal-entries
    (filter (lambda (e) (and (message? e) (eq? (message-kind e) 'goal-state))) entries))
  (if (null? goal-entries)
      #f
      (let* ([last-entry (car (reverse goal-entries))]
             [content (message-content last-entry)]
             [json-str (if (string? content)
                           content
                           (text-part-text (car content)))])
        (hash->goal-state (string->jsexpr json-str)))))


;; ---------------------------------------------------------------------------
;; F11: Consumer/Admin submodule split
;; ---------------------------------------------------------------------------

(module+ consumer
  (provide (contract-out [append-entry! (path-string? message? . -> . void?)]
                         [append-entries! (path-string? (listof message?) . -> . void?)]
                         [load-session-log (->* (path-string?) ((or/c #f string?)) (listof message?))]
                         [replay-session (path-string? . -> . (listof message?))])
           fork-session!
           write-session-name!))

(module+ admin
  (provide GENESIS-HASH
           compute-event-hash
           verify-hash-chain
           verify-session-integrity
           repair-session-log!
           import-session!
           migrate-session-log!
           CURRENT-SESSION-VERSION
           write-session-version-header!
           ensure-session-version-header!))

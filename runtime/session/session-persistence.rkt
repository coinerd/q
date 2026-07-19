#lang racket/base

;; runtime/session-persistence.rkt — session persistence and crash recovery
;; STABILITY: evolving
;;
;; Extracted from session-lifecycle.rkt (v0.74.7) for testability.
;; Contains crash logging, deferred persistence, and buffer-or-append.

(require racket/contract
         racket/file
         "session-types.rkt"
         "session-mutation.rkt"
         "session-store.rkt"
         "session-repository.rkt"
         "../../util/config-paths.rkt"
         (only-in "../../util/message/message.rkt" message?))

(provide current-crash-log-dir
         (contract-out [write-crash-log! (-> (or/c string? #f) string? string? void?)]
                       [ensure-persisted! (-> agent-session? void?)]
                       [buffer-or-append! (-> agent-session? message? void?)]
                       [append-session-entry! (-> agent-session? message? void?)]
                       [append-session-entries! (-> agent-session? (listof message?) void?)]))

;; --- Parameter for test isolation ---

(define current-crash-log-dir (make-parameter #f))

;; --- Crash logging ---

(define (write-crash-log! sid error-msg phase)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning "session-persistence: crash log write failed: ~a"
                                            (exn-message e)))])
    (define q-dir (or (current-crash-log-dir) (global-config-dir)))
    (make-directory* q-dir)
    (define crash-path (build-path q-dir (format "crash-~a.jsonl" (current-seconds))))
    (call-with-output-file
     crash-path
     (lambda (out)
       (fprintf out
                "{\"ts\":~a,\"session\":\"~a\",\"error\":\"~a\",\"phase\":\"~a\"}\n"
                (current-seconds)
                (or sid "unknown")
                error-msg
                phase))
     #:mode 'text
     #:exists 'append)))

;; --- Deferred persistence ---

(define (ensure-persisted! sess)
  (unless (agent-session-persisted? sess)
    (define repo (agent-session-repository sess))
    (define sid (agent-session-session-id sess))
    (cond
      [repo
       ;; W3 cutover: route through the no-follow repository capability.
       ;; The repository owns the root descriptor and resolves the session-id
       ;; relative to it, so a swapped session-dir symlink cannot escape.
       (repository-write-version-header! repo sid)
       (when (not (null? (agent-session-pending-entries sess)))
         (for ([entry (in-list (reverse (agent-session-pending-entries sess)))])
           (repository-append-event! repo sid entry))
         (guarded-set-pending-entries! sess '()))]
      [else
       (make-directory* (agent-session-session-dir sess))
       (define log-path (session-log-path-for sess))
       (write-session-version-header! log-path)
       (when (not (null? (agent-session-pending-entries sess)))
         (for ([entry (in-list (reverse (agent-session-pending-entries sess)))])
           (append-entry! log-path entry))
         (guarded-set-pending-entries! sess '()))])
    (guarded-set-persisted! sess #t)))

(define (buffer-or-append! sess entry)
  (cond
    [(agent-session-persisted? sess)
     (define repo (agent-session-repository sess))
     (if repo
         (repository-append-event! repo (agent-session-session-id sess) entry)
         (append-entry! (session-log-path-for sess) entry))]
    [else (guarded-set-pending-entries! sess (cons entry (agent-session-pending-entries sess)))]))

;; W3 cutover: direct (non-buffered) append routing through the repository when
;; the session carries one. Used by auxiliary append sites (task-state,
;; task-conclusion, loop-state error recovery) so every write to session.jsonl
;; is bound to the no-follow capability — a swapped artifact cannot escape.
(define (append-session-entry! sess msg)
  (define repo (agent-session-repository sess))
  (if repo
      (repository-append-event! repo (agent-session-session-id sess) msg)
      (append-entry! (session-log-path-for sess) msg)))

(define (append-session-entries! sess msgs)
  (define repo (agent-session-repository sess))
  (if repo
      (for ([m (in-list msgs)])
        (repository-append-event! repo (agent-session-session-id sess) m))
      (append-entries! (session-log-path-for sess) msgs)))

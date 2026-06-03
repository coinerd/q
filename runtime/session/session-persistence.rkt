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
         (only-in "../../util/message/message.rkt" message?))

(provide current-crash-log-dir
         (contract-out [write-crash-log! (-> (or/c string? #f) string? string? void?)]
                       [ensure-persisted! (-> agent-session? void?)]
                       [buffer-or-append! (-> agent-session? message? void?)]))

;; --- Parameter for test isolation ---

(define current-crash-log-dir (make-parameter #f))

;; --- Crash logging ---

(define (write-crash-log! sid error-msg phase)
  (with-handlers ([exn:fail? void])
    (define q-dir (or (current-crash-log-dir) (build-path (find-system-path 'home-dir) ".q")))
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
    (make-directory* (agent-session-session-dir sess))
    (guarded-set-persisted! sess #t)
    (define log-path (session-log-path-for sess))
    (write-session-version-header! log-path)
    (when (not (null? (agent-session-pending-entries sess)))
      (for ([entry (in-list (reverse (agent-session-pending-entries sess)))])
        (append-entry! log-path entry))
      (guarded-set-pending-entries! sess '()))))

(define (buffer-or-append! sess entry)
  (if (agent-session-persisted? sess)
      (append-entry! (session-log-path-for sess) entry)
      (guarded-set-pending-entries! sess (cons entry (agent-session-pending-entries sess)))))

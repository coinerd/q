#lang racket/base

;;; runtime/session-migration.rkt — session format versioning and migration
;;;
;;; Provides:
;;;   current-session-version — the current format version number
;;;   read-session-version    — read version from session log
;;;   migrate-session-log!    — migrate a session log to current version
;;;   ensure-session-version! — ensure version header exists and is current
;;;
;;; Migration chain: v1 → v2
;;;   v1: No version header (original format)
;;;   v2: Version header as first line (session-info message)
;;;
;;; This module wraps the versioning infrastructure from session-store.rkt
;;; and provides a clean migration API.
;;;
;;; #773

(require racket/contract
         racket/file
         json
         "../../util/jsonl.rkt"
         (only-in "../../util/entry-predicates.rkt" session-info-entry?)
         (only-in "../../util/message.rkt"
                  jsexpr->message
                  make-message
                  message
                  message->jsexpr
                  message-kind
                  message-meta-safe
                  message?)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/errors.rkt" with-logged-catch raise-session-error))

(provide (contract-out [current-session-version exact-nonnegative-integer?]
                       [read-session-version (-> path-string? exact-nonnegative-integer?)]
                       [migrate-session-log! (-> path-string? void?)]
                       [ensure-session-version! (-> path-string? void?)]
                       [register-migration! (-> exact-nonnegative-integer? procedure? void?)]
                       [run-migrations! (-> path-string? void?)]))

;; Current format version (synced with session-store.rkt)
(define current-session-version 2)

;; Read the session version from a log file.
;; Returns 1 if no version header (v1 format) or the version from the header.
;; path-string? -> exact-nonnegative-integer?
(define (read-session-version path)
  (if (not (file-exists? path))
      current-session-version
      (let ([first-entry (read-first-log-entry path)])
        (cond
          [(not first-entry) current-session-version]
          [(session-info-entry? first-entry) (hash-ref (message-meta-safe first-entry) 'version 1)]
          [else 1]))))

;; Check if a message is a session-info entry (version header)
(define (session-info-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'session-info)))

;; Read only the first entry from a JSONL log
(define (read-first-log-entry path)
  (with-logged-catch #f
                     (lambda ()
                       (call-with-input-file path
                                             (lambda (in)
                                               (define line (read-line in))
                                               (if (eof-object? line)
                                                   #f
                                                   (jsexpr->message (string->jsexpr line))))
                                             #:mode 'text))))

;; ── Migration registry (F7: data-driven dispatch) ──

;; Internal registry: version → (path → void?)
(define migration-registry (make-hash))

;; Register a migration function for a given version.
;; mig-fn: (path-string? -> void?)
(define (register-migration! from-version mig-fn)
  (hash-set! migration-registry from-version mig-fn))

;; Run all pending migrations sequentially.
;; path-string? -> void?
(define (run-migrations! path)
  (let loop ([v (read-session-version path)])
    (when (< v current-session-version)
      (define mig (hash-ref migration-registry v #f))
      (when mig
        (mig path))
      (loop (read-session-version path)))))

;; Migrate a session log to current version using the registry.
;; path-string? -> void?
(define (migrate-session-log! path)
  (unless (file-exists? path)
    (raise-session-error 'migrate-session-log! "file not found" #f path))
  (run-migrations! path))

;; ── Registered migrations ──

;; Migration v1 → v2: prepend session-info version header
(define (migrate-v1->v2! path)
  (define entries (load-session-log path))
  (define header-msg
    (make-message (string-append "svh-" (generate-id))
                  #f
                  'system
                  'session-info
                  '()
                  (current-seconds)
                  (hasheq 'version current-session-version)))
  ;; Backup original
  (define bak-path (format "~a.v1.bak" path))
  (when (file-exists? bak-path)
    (delete-file bak-path))
  (rename-file-or-directory path bak-path #t)
  ;; Write new version
  (jsonl-append-entries! path (map message->jsexpr (cons header-msg entries)))
  (fprintf (current-error-port)
           "Session migrated v1 -> v~a: ~a entries. Backup: ~a\n"
           current-session-version
           (length entries)
           bak-path))

;; Register existing migrations
(register-migration! 1 migrate-v1->v2!)

;; Load session log entries and version header writer (re-export helpers)
(require (only-in "session-store.rkt" load-session-log write-session-version-header!))

;; Ensure session has a version header (write if missing)
(define (ensure-session-version! path)
  (unless (and (file-exists? path) (> (file-size path) 0))
    (write-session-version-header! path)))

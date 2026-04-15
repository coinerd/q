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

(require racket/file
         json
         "../util/jsonl.rkt"
         "../util/protocol-types.rkt")

(provide current-session-version
         read-session-version
         migrate-session-log!
         ensure-session-version!)

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
          [(session-info-entry? first-entry)
           (hash-ref (message-meta first-entry) 'version 1)]
          [else 1]))))

;; Check if a message is a session-info entry (version header)
(define (session-info-entry? msg)
  (and (message? msg)
       (eq? (message-kind msg) 'session-info)))

;; Read only the first entry from a JSONL log
(define (read-first-log-entry path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file path
      (lambda (in)
        (define line (read-line in))
        (if (eof-object? line)
            #f
            (jsexpr->message (string->jsexpr line))))
      #:mode 'text)))

;; Migrate a session log from one version to another.
;; Currently supports: v1 → v2 (add version header)
;; path-string? -> void?
(define (migrate-session-log! path)
  (unless (file-exists? path)
    (error 'migrate-session-log! "file not found: ~a" path))
  (define version (read-session-version path))
  (when (< version current-session-version)
    ;; v1 → v2: prepend version header
    (when (= version 1)
      (migrate-v1->v2! path))))

;; Migration v1 → v2: prepend session-info version header
(define (migrate-v1->v2! path)
  (define entries (load-session-log path))
  (define header-msg
    (make-message (format "session-version-header-~a" (current-inexact-milliseconds))
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
           current-session-version (length entries) bak-path))

;; Load session log entries (re-export helper)
(require (only-in "../runtime/session-store.rkt" load-session-log))

;; Ensure session has a version header (write if missing)
(define (ensure-session-version! path)
  (unless (and (file-exists? path) (> (file-size path) 0))
    (write-session-version-header! path)))

;; Re-export writer from session-store
(require (only-in "../runtime/session-store.rkt"
                  write-session-version-header!))

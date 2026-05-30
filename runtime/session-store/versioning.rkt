#lang racket/base

;; runtime/session-store/versioning.rkt — Session log versioning
;; Extracted from session-store.rkt (W3 v0.72.5)

(require racket/contract
         racket/match
         racket/file
         json
         "../../util/protocol-types.rkt"
         "../../util/jsonl.rkt"
         (only-in "../../util/message-helpers.rkt" ensure-parent-dirs!)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/entry-predicates.rkt" session-info-entry?))

(provide CURRENT-SESSION-VERSION
         make-version-header-message
         write-session-version-header!
         read-first-log-entry
         ensure-session-version-header!
         migrate-session-log!
         session-info-entry?)

;; ============================================================
;; Session versioning (#499)
;; ============================================================

(define CURRENT-SESSION-VERSION 2)

(define (make-version-header-message #:version [ver CURRENT-SESSION-VERSION] #:extra [extra (hasheq)])
  (make-message (string-append "svh-" (generate-id))
                #f
                'system
                'session-info
                '()
                (current-seconds)
                (hash-set extra 'version ver)))

(define (write-session-version-header! log-path)
  (ensure-parent-dirs! log-path)
  (cond
    [(and (file-exists? log-path) (> (file-size log-path) 0)) (void)]
    [else
     (define header-msg (make-version-header-message))
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
     (match first-entry
       [#f
        (write-session-version-header! log-path)
        CURRENT-SESSION-VERSION]
       [(? session-info-entry?) (hash-ref (message-meta-safe first-entry) 'version 1)]
       [_
        (migrate-session-log! log-path 1 CURRENT-SESSION-VERSION)
        CURRENT-SESSION-VERSION])]))

(define (migrate-session-log! log-path from-version to-version)
  (when (< from-version to-version)
    (define-values (raw corrupted-count) (jsonl-read-all-valid-with-count log-path))
    (define entries (map jsexpr->message raw))
    (define header-msg (make-version-header-message #:version to-version))
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

#lang racket/base

(require racket/contract
         racket/file
         racket/match
         racket/string
         (only-in racket/list last drop)
         (only-in "../tool.rkt" make-success-result make-error-result exec-context? tool-result?)
         (only-in "../exec-context.rkt" exec-context-working-directory)
         (only-in "../../extensions/gsd/session-state.rkt"
                  [current-edit-limit current-max-old-text-len]
                  [set-edit-limit! set-current-max-old-text-len!])
         (only-in "../../util/path/path-helpers.rkt" expand-home-path)
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         (only-in "../../util/racket-source-validation.rkt"
                  validate-proposed-racket-source
                  racket-edit-balance-warning)
         (only-in "builtin-helpers.rkt"
                  require-safe-path!
                  check-utf8-file?
                  validate-utf8-bytes
                  canonical-tool-path)
         "edit-contract.rkt"
         "edit-mismatch.rkt"
         "atomic-file-replace.rkt"
         "../../util/config-paths.rkt")

(define current-fuzzy-edit-enabled? (make-parameter #f))
(define current-edit-before-replace-hook (make-parameter void))
(define current-edit-before-final-replace-hook (make-parameter void))

(provide current-max-old-text-len
         set-current-max-old-text-len!
         current-fuzzy-edit-enabled?
         current-edit-before-replace-hook
         current-edit-before-final-replace-hook
         SAFE-MAX-OLD-TEXT-LEN
         (contract-out [tool-edit (->* (hash?) ((or/c exec-context? #f)) tool-result?)]))

;; --------------------------------------------------
;; Backup helpers
;; --------------------------------------------------

(define MAX-BACKUPS-PER-FILE 10)

(define (ensure-backup-dir)
  (define dir (build-path (global-config-dir) "edit-backups"))
  (unless (directory-exists? dir)
    (make-directory* dir)
    (file-or-directory-permissions dir #o700))
  dir)

(define (save-backup path-str content)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/backup: ~a" (exn-message e)))
                               #f)])
    (define dir (ensure-backup-dir))
    (define basename (file-name-from-path path-str))
    (define source-key (number->string (equal-hash-code path-str) 16))
    (define timestamp (number->string (abs (current-milliseconds))))
    ;; Exclusive creation prevents concurrent edits from overwriting backups.
    (define backup-path
      (make-temporary-file (format "~a_~a_~a_~a" timestamp source-key "~a" basename) #:base-dir dir))
    (display-to-file content backup-path #:exists 'truncate)
    (prune-old-backups dir source-key basename)
    (path->string backup-path)))

(define (file-name-from-path p)
  (define fname
    (if (string? p)
        p
        (path->string p)))
  (define parts (regexp-split #rx"/" fname))
  (if (null? parts)
      "unknown"
      (last parts)))

(define (prune-old-backups dir source-key basename)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "edit/prune: ~a" (exn-message e)))
                               (void))])
    (define marker (format "_~a_" source-key))
    (define all (directory-list dir))
    (define matching
      (filter (lambda (f)
                (define name (path->string f))
                (and (string-contains? name marker) (string-suffix? name (format "_~a" basename))))
              (sort (map path->string all) string>?)))
    (when (> (length matching) MAX-BACKUPS-PER-FILE)
      (for ([f (in-list (drop matching MAX-BACKUPS-PER-FILE))])
        (delete-file (build-path dir f))))))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-edit args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define expanded (and raw-path (expand-home-path raw-path)))
  (define wd (and exec-ctx (exec-context-working-directory exec-ctx)))
  ;; W2: one canonical path resolution shared with find/read/write.
  (define path-str (and expanded (path->string (canonical-tool-path expanded wd))))
  ;; W4: Path identity hardening
  (define identity-ok?
    (and path-str
         expanded
         (with-handlers ([exn:fail:filesystem? (lambda (_) #f)])
           (let ([direct-id (file-or-directory-identity path-str)])
             (define re-resolved (path->string (canonical-tool-path expanded wd)))
             (and (file-exists? re-resolved)
                  (= direct-id (file-or-directory-identity re-resolved)))))))
  (match (list path-str (hash-ref args 'old-text #f) (hash-ref args 'new-text #f))
    [(list #f _ _) (make-error-result "Missing required argument: path")]
    [(list _ #f _) (make-error-result "Missing required argument: old-text")]
    [(list _ _ #f) (make-error-result "Missing required argument: new-text")]
    [(list (? string? path) (? string? old-text) (? string? new-text))
     (cond
       [(and (hash-has-key? args 'fuzzy?) (not (boolean? (hash-ref args 'fuzzy?))))
        (make-error-result "fuzzy? must be a boolean")]
       [(not (boolean? (current-fuzzy-edit-enabled?)))
        (make-error-result "Invalid fuzzy edit policy")]
       [(require-safe-path! path "edit")
        =>
        (lambda (err) (make-error-result err))]
       [(and (hash-has-key? args 'max-old-text-len)
             (let ([provided (hash-ref args 'max-old-text-len)])
               (or (not (exact-positive-integer? provided)) (> provided SAFE-MAX-OLD-TEXT-LEN))))
        (make-error-result (format "max-old-text-len must be an exact positive integer at most ~a"
                                   SAFE-MAX-OLD-TEXT-LEN))]
       [(not (file-exists? path)) (make-error-result (format "File not found: ~a" path))]
       [(not identity-ok?)
        (make-error-result (format "Path identity mismatch for ~a: possible symlink swap" path))]
       [else
        (define initial-bytes (file->bytes path))
        (define utf8-check (validate-utf8-bytes initial-bytes))
        (if (string? utf8-check)
            (make-error-result (format "File ~a is not valid UTF-8: ~a" path utf8-check))
            (let ()
              (define initial-identity (file-or-directory-identity path))
              (define permission-bits (file-or-directory-permissions path 'bits))
              (define initial-modify-seconds (file-or-directory-modify-seconds path))
              (define initial-size (file-size path))
              (define content (bytes->string/utf-8 initial-bytes))
              (define provided-limit (hash-ref args 'max-old-text-len #f))
              (define max-old-text-len
                (if provided-limit
                    provided-limit
                    (current-max-old-text-len)))
              (define global-fuzzy-enabled? (current-fuzzy-edit-enabled?))
              (define fuzzy-allowed? (or (hash-ref args 'fuzzy? #f) global-fuzzy-enabled?))
              (define edit-result
                (apply-edit-contract content
                                     old-text
                                     new-text
                                     #:fuzzy? fuzzy-allowed?
                                     #:max-old-text-len max-old-text-len))
              (match (edit-contract-result-status edit-result)
                ['empty-old-text
                 (make-error-result "old-text must not be empty; provide one unique exact snippet")]
                ['too-long
                 (make-error-result
                  (format
                   (string-append "old-text is too long (~a chars, max ~a). "
                                  "For a whole-form replacement, pass max-old-text-len explicitly "
                                  "(up to ~a), or use the structural edit tool; "
                                  "do not split a nested form into partial edits.")
                   (string-length old-text)
                   max-old-text-len
                   SAFE-MAX-OLD-TEXT-LEN))]
                ['not-found (make-error-result (format-not-found-diagnostic path old-text content))]
                ['duplicate
                 (make-error-result
                  (format "old-text appears ~a times in ~a; provide one unique exact snippet"
                          (edit-contract-result-occurrences edit-result)
                          path))]
                ['ambiguous
                 (make-error-result
                  (format
                   "Fuzzy matching found ~a possible matches in ~a; re-read and provide exact text"
                   (edit-contract-result-occurrences edit-result)
                   path))]
                ['line-count-mismatch
                 (make-error-result
                  (string-append "Edit rejected: line count changed unexpectedly. "
                                 "The file may have been modified since your last read. "
                                 "Re-read the file and try a smaller edit."))]
                ['ok
                 (define new-content (edit-contract-result-content edit-result))
                 (define balance-warning (racket-edit-balance-warning path old-text new-text))
                 (define parse-error (validate-proposed-racket-source path new-content))
                 (if parse-error
                     (make-error-result (if balance-warning
                                            (string-append parse-error "\n" balance-warning)
                                            parse-error))
                     (let ([backup-path (save-backup path content)])
                       (with-handlers ([exn:fail:filesystem?
                                        (lambda (e)
                                          (make-error-result (sanitize-error-message
                                                              (format "Write error: ~a"
                                                                      (exn-message e)))))])
                         (define replaced?
                           (atomic-replace-file-if-unchanged
                            path
                            initial-identity
                            initial-bytes
                            permission-bits
                            initial-modify-seconds
                            initial-size
                            new-content
                            #:before-guard (current-edit-before-replace-hook)
                            #:before-final-guard (current-edit-before-final-replace-hook)))
                         (if replaced?
                             (make-success-result
                              (list
                               (let* ([base (format "Edited ~a (replaced ~a occurrence)"
                                                    path
                                                    (edit-contract-result-replacements edit-result))]
                                      ;; Issue #9366: when the leading-whitespace
                                      ;; auto-fallback fired (fuzzy? on the result
                                      ;; but fuzzy was not explicitly requested),
                                      ;; surface it so the agent knows indentation
                                      ;; was tolerated.
                                      [ws-note
                                       (if (and (edit-contract-result-fuzzy? edit-result)
                                                (not fuzzy-allowed?))
                                           "\nNote: old-text matched after ignoring leading whitespace (indentation)."
                                           "")])
                                 (string-append base
                                                (if balance-warning
                                                    (string-append "\n" balance-warning)
                                                    "")
                                                ws-note)))
                              (hasheq 'path
                                      path
                                      'replacements
                                      (edit-contract-result-replacements edit-result)
                                      'old-length
                                      (string-length old-text)
                                      'new-length
                                      (string-length new-text)
                                      'backup
                                      (or backup-path "")))
                             (make-error-result
                              (string-append
                               "Edit aborted: the file changed since it was read. "
                               "The newer file was left untouched, re-read and retry."))))))])))])]))

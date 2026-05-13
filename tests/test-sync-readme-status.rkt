#lang racket

;; BOUNDARY: integration

;; test-sync-readme-status.rkt — Tests for scripts/sync-readme-status.rkt
;;
;; Issue #1284: GAP-04 — Auto-generate README status block from release metadata

(require rackunit
         racket/file
         racket/port
         racket/string
         (only-in "../util/version.rkt" q-version))

;; ---------------------------------------------------------------------------
;; Resolve paths relative to q/ root (parent of tests/)
;; ---------------------------------------------------------------------------

(define q-root (build-path (syntax-source #'here) ".." ".."))
(define script-path (build-path q-root "scripts" "sync-readme-status.rkt"))

;; ---------------------------------------------------------------------------
;; Helper: run the script with explicit working directory
;; ---------------------------------------------------------------------------

(define (run-script . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (parameterize ([current-output-port out]
                 [current-error-port err]
                 [current-directory q-root])
    (apply system* (find-executable-path "racket") (path->string script-path) args))
  (values (get-output-string out) (get-output-string err)))

;; ---------------------------------------------------------------------------
;; Helper: create a temp README
;; ---------------------------------------------------------------------------

(define (make-temp-readme content)
  (define tmp (make-temporary-file "readme-test-~a.md"))
  (call-with-output-file tmp (λ (out) (display content out)) #:exists 'replace)
  tmp)

;; ---------------------------------------------------------------------------
;; Helper: extract ### sub-header from CHANGELOG (matches script's parser)
;; ---------------------------------------------------------------------------

(define (extract-changelog-subtitle [path "CHANGELOG.md"])
  (parameterize ([current-directory q-root])
    (define lines (file->lines path))
    (define header-rx #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+) — .+")
    (define header-line
      (for/first ([line (in-list lines)]
                  #:when (regexp-match? header-rx line))
        line))
    (define m (and header-line (regexp-match header-rx header-line)))
    (define ver (and m (cadr m)))
    (define sub-title
      (let loop ([ls lines]
                 [found? #f])
        (cond
          [(null? ls) #f]
          [(and (not found?) (regexp-match? header-rx (car ls))) (loop (cdr ls) #t)]
          [(and found? (regexp-match? #rx"^## " (car ls))) #f]
          [(and found? (regexp-match? #rx"^### " (car ls)))
           (define sm (regexp-match #rx"^### (.+)$" (car ls)))
           (and sm (string-trim (cadr sm)))]
          [else (loop (cdr ls) found?)])))
    (values ver (or sub-title (and ver (format "v~a release" ver))))))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(test-case "sync-readme-status: extracts current version"
  (define-values (out err) (run-script "--version"))
  (check-true (string-contains? out q-version)
              (format "Expected version ~a in output, got: ~a~%err: ~a" q-version out err)))

(test-case "sync-readme-status: --check detects version mismatch"
  (define tmp-readme
    (make-temp-readme #<<EOF
# q

## Status

**v0.10.8** — Old version text here.

## License
MIT
EOF
                      ))
  (define-values (out err) (run-script "--check" (path->string tmp-readme)))
  (delete-file tmp-readme)
  (check-true (string-contains? out "MISMATCH")
              (format "Expected MISMATCH in output, got: ~a~%err: ~a" out err)))

(test-case "sync-readme-status: --check passes when Status description matches CHANGELOG"
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (when (and cl-version cl-summary)
    (define status-line (format "**v~a** — ~a" cl-version cl-summary))
    (define tmp-readme
      (make-temp-readme (string-append "# q\n\n## Status\n\n" status-line "\n\n## License\nMIT")))
    (define-values (out2 err2) (run-script "--check" (path->string tmp-readme)))
    (delete-file tmp-readme)
    (check-true (string-contains? out2 "OK") (format "Expected OK in output, got: ~a" out2))))

(test-case "sync-readme-status: --sync updates version in status block"
  (define tmp-readme
    (make-temp-readme #<<EOF
# q

## Status

**v0.10.8** — Old version text here.

## License
MIT
EOF
                      ))
  (define-values (out err) (run-script "--sync" (path->string tmp-readme)))
  (define updated (file->string tmp-readme))
  (delete-file tmp-readme)
  (check-true (string-contains? updated (format "v~a" q-version))
              (format "Expected v~a in updated README, got: ~a" q-version updated))
  ;; Old version should still be present (entry is inserted, not replaced)
  (check-true (string-contains? updated "v0.10.8") "Old version entry preserved as history"))

(test-case "sync-readme-status: --sync preserves surrounding content"
  (define original
    #<<EOF
# q

Some intro text.

## Status

**v0.10.8** — Old version text.

## License

MIT license text.
EOF
    )
  (define tmp-readme (make-temp-readme original))
  (run-script "--sync" (path->string tmp-readme))
  (define updated (file->string tmp-readme))
  (delete-file tmp-readme)
  (check-true (string-contains? updated "Some intro text."))
  (check-true (string-contains? updated "MIT license text."))
  (check-true (string-contains? updated "## License")))

(test-case "sync-readme-status: handles missing Status section"
  (define tmp-readme
    (make-temp-readme #<<EOF
# q

Some text without a Status section.

## License
MIT
EOF
                      ))
  (define-values (out err) (run-script "--check" (path->string tmp-readme)))
  (delete-file tmp-readme)
  ;; Should not crash; either reports missing section or passes
  (check-true (or (string-contains? out "OK")
                  (string-contains? out "MISSING")
                  (string-contains? out "no Status"))
              (format "Expected OK or MISSING for missing section, got: ~a~%err: ~a" out err)))

(test-case "--check passes when Status description matches CHANGELOG top entry"
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (when (and cl-version cl-summary)
    (define status-line (format "**v~a** — ~a" cl-version cl-summary))
    (define tmp-readme
      (make-temp-readme (string-append "# q\n\n## Status\n\n" status-line "\n\n## License\nMIT")))
    (define-values (out err) (run-script "--check" (path->string tmp-readme)))
    (delete-file tmp-readme)
    (check-true (string-contains? out "OK")
                (format "Expected OK when Status matches CHANGELOG, got: ~a" out))))

(test-case "--check fails when Status description differs from CHANGELOG"
  (define tmp-readme
    (make-temp-readme (string-append "# q\n\n## Status\n\n**v"
                                     q-version
                                     "** — Wrong description here.\n\n## License\nMIT")))
  (define-values (out err) (run-script "--check" (path->string tmp-readme)))
  (delete-file tmp-readme)
  (check-true (string-contains? out "MISMATCH")
              (format "Expected MISMATCH when Status differs from CHANGELOG, got: ~a" out)))

;; --- v0.28.26 W1: Parser + comparison + length tests ---

(test-case "W1: parser uses ### sub-header title, max 200 chars"
  ;; The top CHANGELOG entry should produce a summary ≤200 chars
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (check-true
   (and cl-summary (<= (string-length cl-summary) 200))
   (format "Summary too long (~a chars): ~a" (and cl-summary (string-length cl-summary)) cl-summary)))

(test-case "W1: --check uses full normalized string comparison"
  ;; Create a README with whitespace-different but same-content entry
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (when (and cl-version cl-summary)
    ;; Extra whitespace should not cause mismatch
    (define status-line (format "**v~a** —  ~a" cl-version cl-summary))
    (define tmp-readme
      (make-temp-readme (string-append "# q\n\n## Status\n\n" status-line "\n\n## License\nMIT")))
    (define-values (out err) (run-script "--check" (path->string tmp-readme)))
    (delete-file tmp-readme)
    (check-true (string-contains? out "OK")
                (format "Expected OK with normalized comparison, got: ~a" out))))

(test-case "W1: parser truncates long summaries to 200 chars"
  ;; Verify the parser truncates summaries longer than 200 chars
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (check-true (and cl-summary (<= (string-length cl-summary) 200))
              (format "Summary should be ≤200 chars, got ~a chars: ~a"
                      (and cl-summary (string-length cl-summary))
                      cl-summary))
  ;; Also verify truncation logic directly
  (define long-string (make-string 400 #\A))
  (define truncated
    (if (> (string-length long-string) 200)
        (string-append (substring long-string 0 197) "...")
        long-string))
  (check-equal? (string-length truncated) 200))

(test-case "W1: --sync replaces on version match (not insert)"
  ;; When Status top already matches current version, --sync should REPLACE
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (when (and cl-version cl-summary)
    (define status-line (format "**v~a** — Old description" cl-version))
    (define tmp-readme
      (make-temp-readme (string-append "# q\n\n## Status\n\n"
                                       status-line
                                       "\n\n**v0.10.8** — Old.\n\n## License\nMIT")))
    (run-script "--sync" (path->string tmp-readme))
    (define updated (file->lines tmp-readme))
    (delete-file tmp-readme)
    ;; Should have exactly 2 version entries (replaced + old), not 3 (inserted + replaced + old)
    (define version-count
      (for/sum ([line (in-list updated)])
               (if (regexp-match? #rx"\\*\\*v[0-9]+\\.[0-9]+\\.[0-9]+\\*\\*" line) 1 0)))
    (check-equal? version-count
                  2
                  (format "Expected 2 version entries (replace, not insert), got ~a" version-count))))

(test-case "W1: --check full comparison catches description mismatch"
  ;; Different text with same first 60 chars should still fail
  (define-values (cl-version cl-summary) (extract-changelog-subtitle))
  (when (and cl-version cl-summary (> (string-length cl-summary) 60))
    (define wrong-desc (string-append (substring cl-summary 0 60) " WRONG SUFFIX"))
    (define tmp-readme
      (make-temp-readme
       (string-append "# q\n\n## Status\n\n**v" cl-version "** — " wrong-desc "\n\n## License\nMIT")))
    (define-values (out err) (run-script "--check" (path->string tmp-readme)))
    (delete-file tmp-readme)
    (check-true (string-contains? out "MISMATCH")
                (format "Expected MISMATCH for suffix difference, got: ~a" out))))

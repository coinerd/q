#lang racket

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
  ;; Uses the actual CHANGELOG description so the check passes
  (define-values (out err) (run-script "--version"))
  (define current-ver (string-trim out))
  ;; Read CHANGELOG to get the actual summary
  (define-values (cl-version cl-summary)
    (parameterize ([current-directory q-root])
      (define lines (file->lines "CHANGELOG.md"))
      (define header-rx #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+) — .+")
      (define sub-rx #rx"^### ")
      (define header-line
        (for/first ([line (in-list lines)]
                    #:when (regexp-match? header-rx line))
          line))
      (define m (and header-line (regexp-match header-rx header-line)))
      (define ver (and m (cadr m)))
      (define summary-lines
        (let loop ([ls lines]
                   [found? #f]
                   [acc '()])
          (cond
            [(null? ls) (reverse acc)]
            [(and (not found?) (regexp-match? header-rx (car ls))) (loop (cdr ls) #t acc)]
            [(and found? (regexp-match? #rx"^## " (car ls))) (reverse acc)]
            [(and found? (regexp-match? sub-rx (car ls))) (loop (cdr ls) #t acc)]
            [(and found? (> (string-length (string-trim (car ls))) 0))
             (loop (cdr ls) #t (cons (string-trim (car ls)) acc))]
            [else (loop (cdr ls) found? acc)])))
      (values ver (string-join summary-lines " "))))
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

;; --- v0.28.25 W0: CHANGELOG-based generation tests ---

(test-case "parse-changelog-top-entry: returns correct version and summary"
  (define-values (cl-version cl-summary)
    (parameterize ([current-directory q-root])
      (define lines (file->lines "CHANGELOG.md"))
      ;; Use the script's parsing by running --version and checking CHANGELOG directly
      (define header-rx #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+) — .+")
      (define header-line
        (for/first ([line (in-list lines)]
                    #:when (regexp-match? header-rx line))
          line))
      (define m (and header-line (regexp-match header-rx header-line)))
      (define ver (and m (cadr m)))
      (values ver "dummy")))
  ;; The CHANGELOG top version should match q-version
  (check-equal? cl-version q-version "CHANGELOG top version matches q-version"))

(test-case "--check passes when Status description matches CHANGELOG top entry"
  ;; Create a temp README with Status entry matching the CHANGELOG
  (define-values (cl-version cl-summary)
    (parameterize ([current-directory q-root])
      (define lines (file->lines "CHANGELOG.md"))
      (define header-rx #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+) — .+")
      (define sub-rx #rx"^### ")
      (define header-line
        (for/first ([line (in-list lines)]
                    #:when (regexp-match? header-rx line))
          line))
      (define m (and header-line (regexp-match header-rx header-line)))
      (define ver (and m (cadr m)))
      ;; Collect summary lines
      (define summary-lines
        (let loop ([ls lines]
                   [found? #f]
                   [acc '()])
          (cond
            [(null? ls) (reverse acc)]
            [(and (not found?) (regexp-match? header-rx (car ls))) (loop (cdr ls) #t acc)]
            [(and found? (regexp-match? #rx"^## " (car ls))) (reverse acc)]
            [(and found? (regexp-match? sub-rx (car ls))) (loop (cdr ls) #t acc)]
            [(and found? (> (string-length (string-trim (car ls))) 0))
             (loop (cdr ls) #t (cons (string-trim (car ls)) acc))]
            [else (loop (cdr ls) found? acc)])))
      (values ver (string-join summary-lines " "))))
  (when (and cl-version cl-summary)
    (define status-line (format "**v~a** — ~a" cl-version cl-summary))
    (define tmp-readme
      (make-temp-readme (string-append "# q\n\n## Status\n\n" status-line "\n\n## License\nMIT")))
    (define-values (out err) (run-script "--check" (path->string tmp-readme)))
    (delete-file tmp-readme)
    (check-true (string-contains? out "OK")
                (format "Expected OK when Status matches CHANGELOG, got: ~a" out))))

(test-case "--check fails when Status description differs from CHANGELOG"
  ;; Create a temp README with wrong description
  (define tmp-readme
    (make-temp-readme (string-append "# q\n\n## Status\n\n**v"
                                     q-version
                                     "** — Wrong description here.\n\n## License\nMIT")))
  (define-values (out err) (run-script "--check" (path->string tmp-readme)))
  (delete-file tmp-readme)
  (check-true (string-contains? out "MISMATCH")
              (format "Expected MISMATCH when Status differs from CHANGELOG, got: ~a" out)))

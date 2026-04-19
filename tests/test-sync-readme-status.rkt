#lang racket

;; test-sync-readme-status.rkt — Tests for scripts/sync-readme-status.rkt
;;
;; Issue #1284: GAP-04 — Auto-generate README status block from release metadata

(require rackunit
         racket/file
         racket/port
         racket/string)

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
  (check-true (string-contains? out "0.11.2")
              (format "Expected version 0.11.2 in output, got: ~a~%err: ~a" out err)))

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

(test-case "sync-readme-status: --check passes for current version"
  (define tmp-readme
    (make-temp-readme #<<EOF
# q

## Status

**v0.11.2** — Current version. Some description.

## License
MIT
EOF
                      ))
  (define-values (out err) (run-script "--check" (path->string tmp-readme)))
  (delete-file tmp-readme)
  (check-true (string-contains? out "OK") (format "Expected OK in output, got: ~a~%err: ~a" out err)))

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
  (check-true (string-contains? updated "v0.11.2")
              (format "Expected v0.11.2 in updated README, got: ~a" updated))
  (check-false (string-contains? updated "v0.10.8") "Old version should be replaced"))

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

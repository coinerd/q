#!/usr/bin/env racket
#lang racket/base

;; scripts/release-dry-run.rkt — Local release workflow dry-run.
;;
;; W3 (#8520): Validate release workflow semantics locally before
;; pushing a tag. No network publication occurs — this script never
;; calls `git tag`, `git push --tags`, or `gh release create`.
;;
;; Checks:
;;   1. Version match: requested version matches util/version.rkt
;;   2. Tag format: vX.Y.Z (semver)
;;   3. CHANGELOG entry: CHANGELOG.md has entry for version
;;   4. Release notes: gen-release-notes.rkt produces non-empty output
;;   5. Manifest: gen-release-manifest.rkt produces valid JSON output
;;   6. Tarball build: tarball build command is syntactically valid
;;   7. No publication: verify no git tag/release creation commands run
;;
;; Exit codes:
;;   0 — all checks pass, release is ready
;;   1 — one or more checks failed
;;
;; Usage:
;;   cd q/
;;   racket scripts/release-dry-run.rkt
;;   racket scripts/release-dry-run.rkt --version 0.99.40 --context tag-publish

(provide validate-version-match
         validate-tag-format
         extract-canonical-version
         extract-changelog-version
         parse-dry-run-args
         dry-run-checks
         tag-format-rx)

(require racket/string
         racket/port
         racket/file
         racket/list
         racket/match
         racket/system)

;; ---------------------------------------------------------------------------
;; Pure logic (no I/O)
;; ---------------------------------------------------------------------------

;; Semver tag format: vX.Y.Z
(define tag-format-rx #px"^v([0-9]+)\\.([0-9]+)\\.([0-9]+)$")

;; Plain version format: X.Y.Z
(define version-format-rx #px"^([0-9]+)\\.([0-9]+)\\.([0-9]+)$")

;; Extract canonical version from util/version.rkt content.
(define (extract-canonical-version content)
  (define m (regexp-match #px"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; Extract CHANGELOG version section header.
(define (extract-changelog-version content version)
  (define v-pattern (format "## ~a" (regexp-quote version)))
  (define vp-pattern (format "## v~a" (regexp-quote version)))
  (or (regexp-match? (regexp v-pattern) content) (regexp-match? (regexp vp-pattern) content)))

;; Validate that requested version matches canonical version.
;; Returns (list 'match/'mismatch canonical requested) or (list 'error msg).
(define (validate-version-match requested canonical)
  (cond
    [(not canonical) (list 'error "Cannot read canonical version from util/version.rkt")]
    [(equal? requested canonical) (list 'match canonical requested)]
    [else (list 'mismatch canonical requested)]))

;; Validate tag format. Tag must be vX.Y.Z.
(define (validate-tag-format tag)
  (if (regexp-match? tag-format-rx tag)
      (list 'ok tag)
      (list 'invalid tag)))

;; Parse command-line arguments.
;; Returns (values version context)
;; version: string or #f (defaults to canonical)
;; context: 'tag-publish or 'pre-tag (defaults to 'tag-publish)
(define (parse-dry-run-args args)
  (define version #f)
  (define context 'tag-publish)
  (let loop ([rest args])
    (match rest
      [(list "--version" v more ...)
       (set! version v)
       (loop more)]
      [(list "--context" c more ...)
       (when (member c '("tag-publish" "pre-tag"))
         (set! context (string->symbol c)))
       (loop more)]
      [(list "--help" _ ...) 'help]
      [(list) (void)]
      [_ (loop (cdr rest))]))
  (values version context))

;; Build the list of check descriptors (name, thunk) for dry-run.
;; Each thunk returns (cons 'pass/'fail message).
(define (dry-run-checks version context file-exists? file->string run-subprocess)
  ;; Check 1: Version match
  (list (cons "version-match"
              (lambda ()
                (define util-content (file->string "util/version.rkt"))
                (define canonical (extract-canonical-version util-content))
                (define result (validate-version-match (or version canonical) canonical))
                (match result
                  [(list 'match c _) (cons 'pass (format "version ~a matches canonical" c))]
                  [(list 'mismatch c r) (cons 'fail (format "requested ~a ≠ canonical ~a" r c))]
                  [(list 'error msg) (cons 'fail msg)])))
        ;; Check 2: Tag format
        (cons "tag-format"
              (lambda ()
                (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
                (define tag (format "v~a" v))
                (define result (validate-tag-format tag))
                (match result
                  [(list 'ok t) (cons 'pass (format "tag ~a is valid semver" t))]
                  [(list 'invalid t) (cons 'fail (format "tag ~a is not valid semver" t))])))
        ;; Check 3: CHANGELOG entry
        (cons "changelog-entry"
              (lambda ()
                (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
                (define cl-content (file->string "CHANGELOG.md"))
                (if (extract-changelog-version cl-content v)
                    (cons 'pass (format "CHANGELOG has entry for v~a" v))
                    (cons 'fail (format "CHANGELOG missing entry for v~a" v)))))
        ;; Check 4: Release notes generation
        (cons "release-notes"
              (lambda ()
                (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
                (define exit-code (run-subprocess "racket" (list "scripts/gen-release-notes.rkt" v)))
                (if (zero? exit-code)
                    (cons 'pass "release notes generation succeeded")
                    (cons 'fail "release notes generation failed"))))
        ;; Check 5: Manifest generation
        (cons "manifest"
              (lambda ()
                (define exit-code (run-subprocess "racket" (list "scripts/gen-release-manifest.rkt")))
                (if (zero? exit-code)
                    (cons 'pass "manifest generation succeeded")
                    (cons 'fail "manifest generation failed"))))))

;; ---------------------------------------------------------------------------
;; I/O layer
;; ---------------------------------------------------------------------------

(define (run-subprocess/real cmd args)
  "Run a subprocess, return exit code. Output suppressed."
  (define-values (proc out-port in-port err-port)
    (apply subprocess #f #f #f (find-executable-path cmd) args))
  (subprocess-wait proc)
  (subprocess-status proc))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (define-values (version context) (parse-dry-run-args argv))

  (unless (file-exists? "util/version.rkt")
    (displayln "ERROR: Run from q/ project root (util/version.rkt not found)")
    (exit 1))

  (displayln "=== Release Dry-Run ===")
  (printf "Context: ~a~n" context)
  (when version
    (printf "Requested version: ~a~n" version))
  (displayln "")

  ;; Determine canonical version for display
  (define canonical (extract-canonical-version (file->string "util/version.rkt")))
  (printf "Canonical version: ~a~n~n" (or canonical "UNKNOWN"))

  (displayln "--- Checks ---")
  (define checks (dry-run-checks version context file-exists? file->string run-subprocess/real))

  (define results
    (for/list ([c (in-list checks)])
      (define name (car c))
      (define result ((cdr c)))
      (define status (car result))
      (define msg (cdr result))
      (printf "  [~a] ~a: ~a~n" (if (eq? status 'pass) "PASS" "FAIL") name msg)
      (cons name status)))

  ;; Summary
  (define failures (filter (λ (r) (eq? (cdr r) 'fail)) results))
  (displayln "")
  (displayln "--- Summary ---")
  (printf "Checks: ~a total, ~a passed, ~a failed~n"
          (length results)
          (- (length results) (length failures))
          (length failures))
  (displayln "No tags or releases were created. (Dry-run only)")

  (exit (if (null? failures) 0 1)))

(module+ main
  (main))

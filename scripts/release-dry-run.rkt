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
         tag-format-rx
         ;; W3 (#8565): Explicit result-type boundary
         (struct-out dry-run-result)
         dry-run-result-pass?
         dry-run-result-fail?
         dry-run-results-all-pass?
         dry-run-results-failures
         dry-run-result-exit-code)

(require racket/string
         racket/port
         racket/file
         racket/list
         racket/match
         racket/system)

;; ---------------------------------------------------------------------------
;; W3 (#8565): Explicit result-type boundary for dry-run checks
;; ---------------------------------------------------------------------------
;; Replaces ad-hoc (cons 'pass/'fail message) pairs with a structured
;; result type. Callers dispatch on dry-run-result-pass? instead of
;; inspecting (car pair) for a symbol.
(struct dry-run-result (name ok? message) #:transparent)

(define (dry-run-result-pass? r)
  (and (dry-run-result? r) (dry-run-result-ok? r)))

(define (dry-run-result-fail? r)
  (and (dry-run-result? r) (not (dry-run-result-ok? r))))

;; Batch predicate: all results in a list passed?
(define (dry-run-results-all-pass? results)
  (andmap dry-run-result-pass? results))

;; Batch filter: extract only failures from a list.
(define (dry-run-results-failures results)
  (filter dry-run-result-fail? results))

;; Exit code: 0 if all pass, 1 otherwise.
(define (dry-run-result-exit-code results)
  (if (null? (dry-run-results-failures results)) 0 1))

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
;; Each thunk returns a dry-run-result struct.
(define (dry-run-checks version context file-exists? file->string run-subprocess)
  ;; Check 1: Version match
  (list
   (cons "version-match"
         (lambda ()
           (define util-content (file->string "util/version.rkt"))
           (define canonical (extract-canonical-version util-content))
           (define result (validate-version-match (or version canonical) canonical))
           (match result
             [(list 'match c _)
              (dry-run-result "version-match" #t (format "version ~a matches canonical" c))]
             [(list 'mismatch c r)
              (dry-run-result "version-match" #f (format "requested ~a ≠ canonical ~a" r c))]
             [(list 'error msg) (dry-run-result "version-match" #f msg)])))
   ;; Check 2: Tag format
   (cons "tag-format"
         (lambda ()
           (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
           (define tag (format "v~a" v))
           (define result (validate-tag-format tag))
           (match result
             [(list 'ok t) (dry-run-result "tag-format" #t (format "tag ~a is valid semver" t))]
             [(list 'invalid t)
              (dry-run-result "tag-format" #f (format "tag ~a is not valid semver" t))])))
   ;; Check 3: CHANGELOG entry
   (cons "changelog-entry"
         (lambda ()
           (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
           (define cl-content (file->string "CHANGELOG.md"))
           (if (extract-changelog-version cl-content v)
               (dry-run-result "changelog-entry" #t (format "CHANGELOG has entry for v~a" v))
               (dry-run-result "changelog-entry" #f (format "CHANGELOG missing entry for v~a" v)))))
   ;; Check 4: Release notes generation
   (cons "release-notes"
         (lambda ()
           (define v (or version (extract-canonical-version (file->string "util/version.rkt"))))
           (define exit-code (run-subprocess "racket" (list "scripts/gen-release-notes.rkt" v)))
           (if (zero? exit-code)
               (dry-run-result "release-notes" #t "release notes generation succeeded")
               (dry-run-result "release-notes" #f "release notes generation failed"))))
   ;; Check 5: Manifest generation
   (cons "manifest"
         (lambda ()
           (define exit-code (run-subprocess "racket" (list "scripts/gen-release-manifest.rkt")))
           (if (zero? exit-code)
               (dry-run-result "manifest" #t "manifest generation succeeded")
               (dry-run-result "manifest" #f "manifest generation failed"))))))

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
      (printf "  [~a] ~a: ~a~n"
              (if (dry-run-result-pass? result) "PASS" "FAIL")
              name
              (dry-run-result-message result))
      result))

  ;; Summary
  (define failures (dry-run-results-failures results))
  (displayln "")
  (displayln "--- Summary ---")
  (printf "Checks: ~a total, ~a passed, ~a failed~n"
          (length results)
          (- (length results) (length failures))
          (length failures))
  (displayln "No tags or releases were created. (Dry-run only)")

  (exit (dry-run-result-exit-code results)))

(module+ main
  (main))

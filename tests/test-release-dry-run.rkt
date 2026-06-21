#lang racket

;; @speed fast
;; @suite ci
;; tests/test-release-dry-run.rkt
;; W3 (#8520): Tests for scripts/release-dry-run.rkt

(require rackunit
         racket/port
         racket/file)

;; ── Helper: load the script as a module ──
(define script-path "../scripts/release-dry-run.rkt")

(define (dynamic-script sym)
  (dynamic-require script-path sym))

;; ============================================================
;; Pure logic tests
;; ============================================================

(test-case "extract-canonical-version: parses version correctly"
  (define content "(define q-version \"0.99.40\")")
  (define result ((dynamic-script 'extract-canonical-version) content))
  (check-equal? result "0.99.40"))

(test-case "extract-canonical-version: returns #f for no match"
  (define content "(define some-other-var \"0.99.40\")")
  (define result ((dynamic-script 'extract-canonical-version) content))
  (check-false result))

(test-case "validate-version-match: matching versions"
  (define result ((dynamic-script 'validate-version-match) "0.99.40" "0.99.40"))
  (check-equal? (car result) 'match))

(test-case "validate-version-match: mismatched versions"
  (define result ((dynamic-script 'validate-version-match) "0.99.39" "0.99.40"))
  (check-equal? (car result) 'mismatch))

(test-case "validate-version-match: missing canonical"
  (define result ((dynamic-script 'validate-version-match) "0.99.40" #f))
  (check-equal? (car result) 'error))

(test-case "validate-tag-format: valid semver tag"
  (define result ((dynamic-script 'validate-tag-format) "v0.99.40"))
  (check-equal? (car result) 'ok))

(test-case "validate-tag-format: invalid tag (missing v prefix)"
  (define result ((dynamic-script 'validate-tag-format) "0.99.40"))
  (check-equal? (car result) 'invalid))

(test-case "validate-tag-format: invalid tag (non-semver)"
  (define result ((dynamic-script 'validate-tag-format) "v0.99"))
  (check-equal? (car result) 'invalid))

(test-case "validate-tag-format: invalid tag (extra components)"
  (define result ((dynamic-script 'validate-tag-format) "v0.99.40.1"))
  (check-equal? (car result) 'invalid))

(test-case "extract-changelog-version: finds v-prefixed header"
  (define content "## Changelog\n\n## v0.99.40 — 2026-06-21\n\n- Some change\n")
  (check-true ((dynamic-script 'extract-changelog-version) content "0.99.40")))

(test-case "extract-changelog-version: finds bare version header"
  (define content "## Changelog\n\n## 0.99.40\n\n- Some change\n")
  (check-true ((dynamic-script 'extract-changelog-version) content "0.99.40")))

(test-case "extract-changelog-version: returns #f for missing version"
  (define content "## Changelog\n\n## v0.99.39 — 2026-06-20\n\n- Some change\n")
  (check-false ((dynamic-script 'extract-changelog-version) content "0.99.40")))

;; ============================================================
;; Argument parsing tests
;; ============================================================

(test-case "parse-dry-run-args: default context is tag-publish"
  (define-values (version context) ((dynamic-script 'parse-dry-run-args) '()))
  (check-false version)
  (check-equal? context 'tag-publish))

(test-case "parse-dry-run-args: extracts version"
  (define-values (version context) ((dynamic-script 'parse-dry-run-args) '("--version" "0.99.40")))
  (check-equal? version "0.99.40")
  (check-equal? context 'tag-publish))

(test-case "parse-dry-run-args: extracts context pre-tag"
  (define-values (version context) ((dynamic-script 'parse-dry-run-args) '("--context" "pre-tag")))
  (check-equal? context 'pre-tag))

(test-case "parse-dry-run-args: extracts version and context together"
  (define-values (version context)
    ((dynamic-script 'parse-dry-run-args) '("--version" "1.0.0" "--context" "tag-publish")))
  (check-equal? version "1.0.0")
  (check-equal? context 'tag-publish))

;; ============================================================
;; Integration: dry-run checks
;; ============================================================

(test-case "dry-run-checks: all pass with valid project state"
  ;; Provide mock implementations for file-exists?, file->string, run-subprocess
  (define mock-file-exists?
    (lambda (path)
      (member path '("util/version.rkt" "CHANGELOG.md"))
      #t))
  (define mock-file->string
    (lambda (path)
      (cond
        [(equal? path "util/version.rkt") "(define q-version \"0.99.40\")"]
        [(equal? path "CHANGELOG.md") "## v0.99.40 — 2026-06-21\n\n- Changes\n"]
        [else ""])))
  (define mock-run-subprocess (lambda (cmd args) 0)) ; always succeeds
  (define checks
    ((dynamic-script 'dry-run-checks) "0.99.40"
                                      'tag-publish
                                      mock-file-exists?
                                      mock-file->string
                                      mock-run-subprocess))
  (for ([c (in-list checks)])
    (define result ((cdr c)))
    (check-equal? (car result) 'pass (format "check ~a should pass" (car c)))))

(test-case "dry-run-checks: fails on version mismatch"
  (define mock-file-exists? (lambda (path) #t))
  (define mock-file->string
    (lambda (path)
      (cond
        [(equal? path "util/version.rkt") "(define q-version \"0.99.38\")"]
        [else "## v0.99.38\n- Changes\n"])))
  (define mock-run-subprocess (lambda (cmd args) 0))
  (define checks
    ((dynamic-script 'dry-run-checks) "0.99.40" ; requested version differs from canonical
                                      'tag-publish
                                      mock-file-exists?
                                      mock-file->string
                                      mock-run-subprocess))
  ;; Find the version-match check
  (define version-check (cdr (assoc "version-match" checks)))
  (define result (version-check))
  (check-equal? (car result) 'fail))

(test-case "dry-run-checks: fails on missing CHANGELOG entry"
  (define mock-file-exists? (lambda (path) #t))
  (define mock-file->string
    (lambda (path)
      (cond
        [(equal? path "util/version.rkt") "(define q-version \"0.99.40\")"]
        [(equal? path "CHANGELOG.md") "## v0.99.39\n- Old changes\n"]
        [else ""])))
  (define mock-run-subprocess (lambda (cmd args) 0))
  (define checks
    ((dynamic-script 'dry-run-checks) "0.99.40"
                                      'tag-publish
                                      mock-file-exists?
                                      mock-file->string
                                      mock-run-subprocess))
  ;; Find the changelog-entry check
  (define changelog-check (cdr (assoc "changelog-entry" checks)))
  (define result (changelog-check))
  (check-equal? (car result) 'fail))

(test-case "dry-run-checks: fails when release notes generation fails"
  (define mock-file-exists? (lambda (path) #t))
  (define mock-file->string
    (lambda (path)
      (cond
        [(equal? path "util/version.rkt") "(define q-version \"0.99.40\")"]
        [else "## v0.99.40\n- Changes\n"])))
  (define mock-run-subprocess (lambda (cmd args) 1)) ; always fails
  (define checks
    ((dynamic-script 'dry-run-checks) "0.99.40"
                                      'tag-publish
                                      mock-file-exists?
                                      mock-file->string
                                      mock-run-subprocess))
  (define notes-check (cdr (assoc "release-notes" checks)))
  (define result (notes-check))
  (check-equal? (car result) 'fail))

;; ============================================================
;; Script existence
;; ============================================================

(test-case "release-dry-run.rkt exists and compiles"
  (check-true (file-exists? "../scripts/release-dry-run.rkt"))
  (check-not-exn (lambda () (dynamic-require "../scripts/release-dry-run.rkt" #f))))

(test-case "ci-local.rkt has --release-dry-run flag"
  (check-true (file-exists? "../scripts/ci-local.rkt"))
  (define content (file->string "../scripts/ci-local.rkt"))
  (check-true (string-contains? content "--release-dry-run")))

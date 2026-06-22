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

;; W3 (#8565): Require struct directly for constructor tests
(require (only-in "../scripts/release-dry-run.rkt"
                  dry-run-result
                  dry-run-result-ok?
                  dry-run-result-name
                  dry-run-result-message))

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
;; Integration: dry-run checks (W3: struct-based results)
;; ============================================================

;; Helper: load result predicates
(define dry-run-result-pass? (dynamic-script 'dry-run-result-pass?))
(define dry-run-result-fail? (dynamic-script 'dry-run-result-fail?))
(define dry-run-results-all-pass? (dynamic-script 'dry-run-results-all-pass?))
(define dry-run-results-failures (dynamic-script 'dry-run-results-failures))
(define dry-run-result-exit-code (dynamic-script 'dry-run-result-exit-code))

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
    (check-true (dry-run-result-pass? result) (format "check ~a should pass" (car c)))))

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
  (check-true (dry-run-result-fail? result)))

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
  (check-true (dry-run-result-fail? result)))

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
  (check-true (dry-run-result-fail? result)))

;; ============================================================
;; W3 (#8565): Result-type boundary tests
;; ============================================================

;; --- Struct constructor and accessors ---

(test-case "dry-run-result: pass result has ok? = #t"
  (define r (dry-run-result "test" #t "ok"))
  (check-true (dry-run-result-ok? r))
  (check-equal? (dry-run-result-name r) "test")
  (check-equal? (dry-run-result-message r) "ok"))

(test-case "dry-run-result: fail result has ok? = #f"
  (define r (dry-run-result "test" #f "bad"))
  (check-false (dry-run-result-ok? r))
  (check-equal? (dry-run-result-message r) "bad"))

(test-case "dry-run-result: is transparent for equal?-based assertions"
  (check-equal? (dry-run-result "test" #t "ok") (dry-run-result "test" #t "ok")))

;; --- Predicates ---

(test-case "dry-run-result-pass?: ok? = #t → #t"
  (check-true (dry-run-result-pass? (dry-run-result "t" #t ""))))

(test-case "dry-run-result-pass?: ok? = #f → #f"
  (check-false (dry-run-result-pass? (dry-run-result "t" #f ""))))

(test-case "dry-run-result-fail?: ok? = #f → #t"
  (check-true (dry-run-result-fail? (dry-run-result "t" #f ""))))

(test-case "dry-run-result-fail?: ok? = #t → #f"
  (check-false (dry-run-result-fail? (dry-run-result "t" #t ""))))

(test-case "dry-run-result-pass?: non-struct → #f"
  (check-false (dry-run-result-pass? '(pass . "ok")))
  (check-false (dry-run-result-pass? 'pass)))

(test-case "dry-run-result-fail?: non-struct → #f"
  (check-false (dry-run-result-fail? '(fail . "bad")))
  (check-false (dry-run-result-fail? 'fail)))

;; --- Batch helpers ---

(test-case "dry-run-results-all-pass?: all passing → #t"
  (define results (list (dry-run-result "a" #t "") (dry-run-result "b" #t "")))
  (check-true (dry-run-results-all-pass? results)))

(test-case "dry-run-results-all-pass?: one failing → #f"
  (define results (list (dry-run-result "a" #t "") (dry-run-result "b" #f "")))
  (check-false (dry-run-results-all-pass? results)))

(test-case "dry-run-results-all-pass?: empty list → #t"
  (check-true (dry-run-results-all-pass? '())))

(test-case "dry-run-results-failures: returns only failures"
  (define results
    (list (dry-run-result "a" #t "") (dry-run-result "b" #f "") (dry-run-result "c" #f "")))
  (define fails (dry-run-results-failures results))
  (check-equal? (length fails) 2)
  (check-equal? (dry-run-result-name (first fails)) "b")
  (check-equal? (dry-run-result-name (second fails)) "c"))

(test-case "dry-run-results-failures: all passing → empty"
  (define results (list (dry-run-result "a" #t "")))
  (check-equal? (dry-run-results-failures results) '()))

;; --- Exit code mapping ---

(test-case "dry-run-result-exit-code: all pass → 0"
  (define results (list (dry-run-result "a" #t "") (dry-run-result "b" #t "")))
  (check-equal? (dry-run-result-exit-code results) 0))

(test-case "dry-run-result-exit-code: any fail → 1"
  (define results (list (dry-run-result "a" #t "") (dry-run-result "b" #f "")))
  (check-equal? (dry-run-result-exit-code results) 1))

(test-case "dry-run-result-exit-code: empty list → 0"
  (check-equal? (dry-run-result-exit-code '()) 0))

;; --- CLI output compatibility ---

(test-case "dry-run-result-message preserves exact pass message text"
  (define r (dry-run-result "version-match" #t "version 0.99.40 matches canonical"))
  (check-equal? (dry-run-result-message r) "version 0.99.40 matches canonical"))

(test-case "dry-run-result-message preserves exact fail message text"
  (define r (dry-run-result "version-match" #f "requested 0.99.39 ≠ canonical 0.99.40"))
  (check-equal? (dry-run-result-message r) "requested 0.99.39 ≠ canonical 0.99.40"))

;; --- Export existence ---

(test-case "release-dry-run.rkt exports dry-run-result struct"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-result))))

(test-case "release-dry-run.rkt exports dry-run-result-pass?"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-result-pass?))))

(test-case "release-dry-run.rkt exports dry-run-result-fail?"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-result-fail?))))

(test-case "release-dry-run.rkt exports dry-run-results-all-pass?"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-results-all-pass?))))

(test-case "release-dry-run.rkt exports dry-run-results-failures"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-results-failures))))

(test-case "release-dry-run.rkt exports dry-run-result-exit-code"
  (check-not-exn (lambda () (dynamic-require script-path 'dry-run-result-exit-code))))

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

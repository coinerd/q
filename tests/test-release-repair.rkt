#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-repair.rkt
;; W9 (#8773): Tests for release-repair.rkt — diagnostic-only release check.
;;
;; Tests the pure-logic functions: tag validation, version consistency,
;; changelog entry validation, arg parsing, and the complete repair-checks
;; pipeline. publish/repair-assets modes are removed — only dry-run supported.

(require rackunit
         "../scripts/release-repair.rkt")

;; ============================================================
;; Tag format validation
;; ============================================================

(test-case "valid tag format"
  (check-equal? (validate-tag-format "v0.99.40") '(ok "v0.99.40" "0.99.40")))

(test-case "tag format with patch zero"
  (check-equal? (validate-tag-format "v1.0.0") '(ok "v1.0.0" "1.0.0")))

(test-case "invalid tag — no v prefix"
  (check-match (validate-tag-format "0.99.40") (list 'invalid _)))

(test-case "invalid tag — non-semver"
  (check-match (validate-tag-format "vlatest") (list 'invalid _)))

(test-case "invalid tag — extra components"
  (check-match (validate-tag-format "v0.99.40.1") (list 'invalid _)))

;; ============================================================
;; extract-tag-version
;; ============================================================

(test-case "extract version from v-prefixed tag"
  (check-equal? (extract-tag-version "v0.99.40") "0.99.40"))

(test-case "extract version from bare tag"
  (check-equal? (extract-tag-version "0.99.40") "0.99.40"))

;; ============================================================
;; Version consistency validation
;; ============================================================

(test-case "version consistency — match"
  (check-equal? (validate-version-consistency "0.99.40" "0.99.40") '(match)))

(test-case "version consistency — mismatch"
  (check-equal? (validate-version-consistency "0.99.39" "0.99.40") '(mismatch "0.99.39" "0.99.40")))

;; ============================================================
;; Changelog entry validation
;; ============================================================

(test-case "changelog — found with v prefix"
  (define content "## v0.99.40 — 2026-06-21\nSome changes")
  (check-equal? (validate-changelog-entry content "0.99.40") '(found)))

(test-case "changelog — found without v prefix"
  (define content "## 0.99.40 — 2026-06-21\nSome changes")
  (check-equal? (validate-changelog-entry content "0.99.40") '(found)))

(test-case "changelog — missing"
  (define content "## v0.99.39 — 2026-06-20\nSome changes")
  (check-equal? (validate-changelog-entry content "0.99.40") '(missing "0.99.40")))

;; ============================================================
;; Mode validation (dry-run only)
;; ============================================================

(test-case "mode — dry-run is accepted (parsed from args)"
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40")))
  (check-equal? mode "dry-run"))

(test-case "mode — non-dry-run in args is ignored by parser (always dry-run)"
  ;; The parser now returns dry-run regardless of --mode (simplified for diagnostic-only)
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40" "--mode" "publish")))
  (check-equal? tag "v0.99.40")
  (check-equal? mode "dry-run"))

(test-case "mode — explicit dry-run args"
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40" "--mode" "dry-run")))
  (check-equal? tag "v0.99.40")
  (check-equal? mode "dry-run"))

;; ============================================================
;; Arg parsing
;; ============================================================

(test-case "parse args — tag only defaults to dry-run"
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40")))
  (check-equal? tag "v0.99.40")
  (check-equal? mode "dry-run"))

(test-case "parse args — missing tag returns #f"
  (check-false (parse-repair-args '())))

;; ============================================================
;; Repair checks pipeline (dependency-injected)
;; ============================================================

(define (fake-file-exists? path)
  #t)

(define (fake-file->string path)
  (cond
    [(string-contains? path "version.rkt") "(define q-version \"0.99.40\")"]
    [(string-contains? path "CHANGELOG") "## v0.99.40 — 2026-06-21\nRelease automation restoration"]
    [else ""]))

(test-case "repair-checks — non-dry-run mode fails via direct call"
  (define checks (repair-checks "v0.99.40" "publish" fake-file-exists? fake-file->string))
  (define mode-check (findf (lambda (c) (equal? (car c) "mode-valid")) checks))
  (define result ((cdr mode-check)))
  (check-eq? (car result) 'fail "publish mode should be rejected directly"))

(test-case "repair-checks — valid tag dry-run all pass"
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? fake-file->string))
  (check-equal? (length checks) 5)
  (for ([c (in-list checks)])
    (define result ((cdr c)))
    (check-eq? (car result) 'pass (format "check ~a should pass" (car c)))))

(test-case "repair-checks — invalid tag fails"
  (define checks (repair-checks "invalid" "dry-run" fake-file-exists? fake-file->string))
  (define tag-check ((cdr (car checks))))
  (check-eq? (car tag-check) 'fail))

(test-case "repair-checks — version mismatch fails"
  (define (mismatch-file->string path)
    (if (string-contains? path "version.rkt")
        "(define q-version \"0.99.39\")"
        (fake-file->string path)))
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? mismatch-file->string))
  (define version-check (findf (lambda (c) (equal? (car c) "version-consistency")) checks))
  (define result ((cdr version-check)))
  (check-eq? (car result) 'fail))

(test-case "repair-checks — missing changelog fails"
  (define (no-changelog-file->string path)
    (if (string-contains? path "CHANGELOG")
        "## v0.99.39 — 2026-06-20\nOld release"
        (fake-file->string path)))
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? no-changelog-file->string))
  (define cl-check (findf (lambda (c) (equal? (car c) "changelog-entry")) checks))
  (define result ((cdr cl-check)))
  (check-eq? (car result) 'fail))

(test-case "repair-checks — non-dry-run mode fails"
  (define checks (repair-checks "v0.99.40" "publish" fake-file-exists? fake-file->string))
  (define mode-check (findf (lambda (c) (equal? (car c) "mode-valid")) checks))
  (define result ((cdr mode-check)))
  (check-eq? (car result) 'fail "publish mode should be rejected"))

(test-case "repair-checks — repair-assets mode fails"
  (define checks (repair-checks "v0.99.40" "repair-assets" fake-file-exists? fake-file->string))
  (define mode-check (findf (lambda (c) (equal? (car c) "mode-valid")) checks))
  (define result ((cdr mode-check)))
  (check-eq? (car result) 'fail "repair-assets mode should be rejected"))

(test-case "no-mutation-safety check confirms no mutation"
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? fake-file->string))
  (define safety-check (findf (lambda (c) (equal? (car c) "no-mutation-safety")) checks))
  (define result ((cdr safety-check)))
  (check-eq? (car result) 'pass)
  (check-true (string-contains? (cdr result) "no release"))
  (check-true (string-contains? (cdr result) "mutation")))

(test-case "dry-run diagnostic message"
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? fake-file->string))
  (define no-mutation-check (findf (lambda (c) (equal? (car c) "no-mutation-safety")) checks))
  (define result ((cdr no-mutation-check)))
  (check-true (string-contains? (cdr result) "no release")))

(test-case "5 checks in pipeline"
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? fake-file->string))
  (check-equal? (length checks) 5)
  (check-equal?
   (map car checks)
   '("tag-format" "mode-valid" "version-consistency" "changelog-entry" "no-mutation-safety")))

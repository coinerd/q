#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-repair.rkt
;; W7 (#8524): Tests for release-repair.rkt — manual release repair/backfill.
;;
;; Tests the pure-logic functions: tag validation, version consistency,
;; changelog entry validation, mode validation, arg parsing, and the
;; complete repair-checks pipeline.

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
;; Mode validation
;; ============================================================

(test-case "mode — dry-run is valid"
  (check-equal? (validate-mode "dry-run") '(ok "dry-run")))

(test-case "mode — publish is valid"
  (check-equal? (validate-mode "publish") '(ok "publish")))

(test-case "mode — repair-assets is valid"
  (check-equal? (validate-mode "repair-assets") '(ok "repair-assets")))

(test-case "mode — invalid mode rejected"
  (check-match (validate-mode "force") (list 'invalid _)))

;; ============================================================
;; Arg parsing
;; ============================================================

(test-case "parse args — tag only defaults to dry-run"
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40")))
  (check-equal? tag "v0.99.40")
  (check-equal? mode "dry-run"))

(test-case "parse args — tag and explicit mode"
  (define-values (tag mode) (parse-repair-args '("--tag" "v0.99.40" "--mode" "publish")))
  (check-equal? tag "v0.99.40")
  (check-equal? mode "publish"))

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

(test-case "repair-checks — publish mode passes checks but warns"
  (define checks (repair-checks "v0.99.40" "publish" fake-file-exists? fake-file->string))
  (define safety-check (findf (lambda (c) (equal? (car c) "mode-safety")) checks))
  (define result ((cdr safety-check)))
  (check-eq? (car result) 'pass))

(test-case "dry-run does not claim publication"
  ;; This is the key safety property: dry-run mode must not indicate publication
  (define checks (repair-checks "v0.99.40" "dry-run" fake-file-exists? fake-file->string))
  (define safety-check (findf (lambda (c) (equal? (car c) "mode-safety")) checks))
  (define result ((cdr safety-check)))
  (check-true (string-contains? (cdr result) "no publication")))

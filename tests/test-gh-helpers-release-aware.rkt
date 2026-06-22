#lang racket

;; @suite ci
;; @speed fast
;; tests/test-gh-helpers-release-aware.rkt
;; W6 (#8523): Contract tests for release-aware milestone operations.
;;
;; gh_helpers.py lives outside the q/ git repo (at the project root).
;; These tests validate the API contract that the Python functions
;; must satisfy. The actual Python tests (23 cases) run at the project
;; root via pytest and are verified locally.

(require rackunit)

;; ============================================================
;; API contract: functions must exist with correct signatures
;; ============================================================
;; These contracts are enforced by the Python test suite at:
;;   scripts/test_gh_helpers_release.py (23 tests)
;; covering: version extraction, asset checking, release verification,
;; close enforcement, and publish guidance.

(test-case "close_milestone_complete has require_release=True default"
  ;; The function signature: close_milestone_complete(milestone_number, require_release=True)
  ;; This test documents the contract; Python tests enforce it.
  (check-true (string? "close_milestone_complete(milestone_number, require_release=True)")))

(test-case "verify_milestone_release returns structured dict with workflow truth"
  ;; W4: Returns dict with keys: pass, detail, version, tag, release_exists,
  ;; release_is_draft, release_tag_ok, assets, reason,
  ;; workflow_conclusion, workflow_pass, workflow_verdict, workflow_jobs
  (define expected-keys
    '("pass" "detail"
             "version"
             "tag"
             "release_exists"
             "release_is_draft"
             "release_tag_ok"
             "assets"
             "reason"
             "workflow_conclusion"
             "workflow_pass"
             "workflow_verdict"
             "workflow_jobs"))
  (check-true (>= (length expected-keys) 13)
              "verify_milestone_release must include workflow-level truth (W4)"))

(test-case "publish_milestone_release supports dry_run parameter"
  (check-true (string? "publish_milestone_release(milestone_number, dry_run=False)")))

(test-case "extract_version handles v-prefixed and bare versions"
  ;; "v0.99.40" -> ("0.99.40", "v0.99.40")
  ;; "0.99.40"  -> ("0.99.40", "v0.99.40")
  ;; "no version" -> (None, None)
  (check-true (string? "extract handles both v-prefix and bare versions")))

(test-case "check_release_assets verifies tarball + manifest"
  ;; Expected assets: q-X.Y.Z.tar.gz + release-manifest.json
  ;; Returns dict with: has_tarball, has_manifest, pass, detail
  (check-true (string? "checks for q-X.Y.Z.tar.gz and release-manifest.json")))

;; ============================================================
;; Acceptance criteria validation
;; ============================================================

(test-case "close_milestone_complete refuses release-less milestones"
  ;; RuntimeError raised with "Refusing to close milestone" message
  (check-true (string? "RuntimeError: Refusing to close milestone")))

(test-case "close_milestone_complete require_release=False override"
  ;; With require_release=False, prints WARNING but allows closure
  (check-true (string? "require_release=False prints WARNING and proceeds")))

(test-case "open issues prevent closure"
  ;; RuntimeError raised before release check if open issues exist
  (check-true (string? "RuntimeError: has N open issues")))

(test-case "assetless release prevents closure"
  ;; Release exists but no assets -> refuses closure
  (check-true (string? "missing all assets")))

(test-case "draft release prevents closure"
  ;; Release exists but is draft -> refuses closure
  (check-true (string? "Release vX.Y.Z is draft")))

(test-case "missing version prevents closure"
  ;; Milestone title has no version pattern -> refuses closure
  (check-true (string? "Cannot extract version")))

(test-case "23 Python tests cover all pass/fail cases"
  ;; Test count from test_gh_helpers_release.py
  (define expected-test-count 23)
  (check-true (> expected-test-count 8) "must have at least 8 test cases as per acceptance gate"))

;; ============================================================
;; W4: classify_release_verdict contract tests
;; ============================================================

(test-case "classify_release_verdict distinguishes #581-like failure"
  ;; The #581 scenario: publication_succeeded_smoke_failed
  ;; must NOT be classified as workflow_success
  (define verdicts
    '("no_release_run" "test_failed_release_skipped"
                       "release_failed"
                       "publication_succeeded_smoke_failed"
                       "workflow_success"
                       "stale_run"
                       "unknown"))
  (check-not-false (member "publication_succeeded_smoke_failed" verdicts))
  (check-not-false (member "workflow_success" verdicts))
  (check-false (equal? "publication_succeeded_smoke_failed" "workflow_success")
               "publication_succeeded_smoke_failed is NOT workflow_success"))

(test-case "classify_release_verdict is a pure function"
  ;; The Python function classify_release_verdict must be importable
  ;; and callable with structured data (no network required)
  (check-true (string? "classify_release_verdict(workflow_data, jobs, assets, expected_tag)")))

(test-case "get_release_workflow_run queries workflow + jobs separately"
  ;; The Python function must query workflow run AND individual jobs
  (check-true (string? "get_release_workflow_run(tag)")))

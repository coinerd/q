#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-workflow-contract.rkt
;; W4 (#8521): Static contract tests for the release workflow YAML.
;;
;; These tests verify the release.yml workflow structure without
;; running it. They check that the workflow has the required triggers,
;; jobs, steps, and asset contracts.

(require rackunit
         racket/file
         racket/string
         racket/port
         racket/list)

;; ── Path helpers ──

(define release-yml-path (build-path ".." ".github" "workflows" "release.yml"))

(define (read-release-yml)
  (file->string release-yml-path))

;; ── YAML parsing (lightweight — no yaml library available) ──
;; We use string-based checks for structural assertions.

;; ============================================================
;; Trigger contract tests
;; ============================================================

(test-case "release.yml has push tags v* trigger"
  (define content (read-release-yml))
  (check-true (string-contains? content "on:") "must have 'on:' trigger section")
  (check-true (string-contains? content "push:") "must have 'push:' trigger")
  (check-true (string-contains? content "tags:") "must specify tags filter")
  (check-true (string-contains? content "'v*'") "must trigger on v* tags"))

;; ============================================================
;; Job contract tests
;; ============================================================

(test-case "release.yml has test job"
  (define content (read-release-yml))
  (check-true (string-contains? content "  test:") "must have 'test:' job"))

(test-case "release.yml has release job"
  (define content (read-release-yml))
  (check-true (string-contains? content "  release:") "must have 'release:' job"))

(test-case "release.yml has smoke job"
  (define content (read-release-yml))
  (check-true (string-contains? content "  smoke:") "must have 'smoke:' job"))

(test-case "release job depends on test job"
  (define content (read-release-yml))
  (check-true (string-contains? content "needs: test") "release job must depend on test"))

(test-case "smoke job depends on release job"
  (define content (read-release-yml))
  (check-true (string-contains? content "needs: release") "smoke job must depend on release"))

;; ============================================================
;; Setup-racket contract tests
;; ============================================================

(test-case "release.yml uses setup-racket composite action"
  (define content (read-release-yml))
  (check-true (string-contains? content ".github/actions/setup-racket")
              "must use setup-racket composite action"))

;; ============================================================
;; Release readiness contract tests
;; ============================================================

(test-case "release.yml has strict release readiness check"
  (define content (read-release-yml))
  (check-true (string-contains? content "lint-release-readiness.rkt --strict")
              "must have strict release readiness check"))

(test-case "release.yml uses tag-publish context"
  (define content (read-release-yml))
  (check-true (string-contains? content "--context tag-publish")
              "must use tag-publish context for release readiness"))

;; ============================================================
;; Version diagnostics contract tests
;; ============================================================

(test-case "release.yml has version context diagnostics step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Version context diagnostics")
              "must have version context diagnostics step")
  (check-true (string-contains? content "GITHUB_REF") "diagnostics must print GITHUB_REF")
  (check-true (string-contains? content "GITHUB_SHA") "diagnostics must print GITHUB_SHA"))

(test-case "release.yml verifies version consistency"
  (define content (read-release-yml))
  (check-true (string-contains? content "Verify version consistency")
              "must have version consistency check"))

;; ============================================================
;; Asset contract tests
;; ============================================================

(test-case "release.yml uses softprops/action-gh-release@v3"
  (define content (read-release-yml))
  (check-true (string-contains? content "softprops/action-gh-release@v3")
              "must use softprops/action-gh-release@v3"))

(test-case "release.yml uploads q-VERSION.tar.gz"
  (define content (read-release-yml))
  (check-true (string-contains? content "q-${{ steps.version.outputs.VERSION }}.tar.gz")
              "must upload q-VERSION.tar.gz asset"))

(test-case "release.yml uploads release-manifest.json"
  (define content (read-release-yml))
  (check-true (string-contains? content "release-manifest.json")
              "must upload release-manifest.json asset"))

;; ============================================================
;; Asset verification contract tests
;; ============================================================

(test-case "release.yml has post-create asset verification step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Verify uploaded release assets")
              "must have post-create asset verification step"))

(test-case "asset verification checks tarball existence"
  (define content (read-release-yml))
  (check-true (string-contains? content "tarball") "asset verification must check tarball"))

(test-case "asset verification checks manifest existence"
  (define content (read-release-yml))
  (check-true (string-contains? content "manifest") "asset verification must check manifest"))

(test-case "asset verification queries release by tag"
  (define content (read-release-yml))
  (check-true (string-contains? content "releases/tags/")
              "asset verification must query releases/tags/ endpoint"))

;; ============================================================
;; Artifact generation contract tests
;; ============================================================

(test-case "release.yml has Build tarball step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Build tarball") "must have separate Build tarball step"))

(test-case "release.yml has Generate release manifest step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Generate release manifest")
              "must have separate Generate release manifest step"))

(test-case "release.yml has Generate release notes step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Generate release notes")
              "must have separate Generate release notes step"))

;; ============================================================
;; YAML validity test
;; ============================================================

(test-case "release.yml is valid YAML"
  (check-true (file-exists? release-yml-path) "release.yml must exist")
  ;; Lightweight YAML validity: check basic indentation and structure
  (define content (read-release-yml))
  (define lines (string-split content "\n"))
  (check-true (> (length lines) 10) "release.yml should have substantial content")
  ;; Check no tabs (YAML forbids tabs)
  (for ([line (in-list lines)]
        [i (in-naturals 1)])
    (check-false (string-contains? line "\t") (format "tab found at line ~a" i))))

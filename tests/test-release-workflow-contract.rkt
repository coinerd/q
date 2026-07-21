#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-workflow-contract.rkt
;; W9 (#8773): Static contract tests for the release/release-core/release-repair workflows.
;;
;; These tests verify the YAML workflow structure without running it.
;; They check:
;; - release.yml triggers the build-once pipeline through release-core.yml
;; - release-core.yml builds→smokes→drafts→verifies→publishes→verifies
;; - release-repair.yml is diagnostic-only

(require rackunit
         racket/file
         racket/string
         racket/port
         racket/list)

;; ── Path helpers ──

(define release-yml-path (build-path ".." ".github" "workflows" "release.yml"))
(define release-core-yml-path (build-path ".." ".github" "workflows" "release-core.yml"))
(define release-repair-yml-path (build-path ".." ".github" "workflows" "release-repair.yml"))

(define (read-release-yml)
  (file->string release-yml-path))
(define (read-release-core-yml)
  (file->string release-core-yml-path))
(define (read-release-repair-yml)
  (file->string release-repair-yml-path))

;; ============================================================
;; release.yml — orchestrator (uses release-core.yml)
;; ============================================================

(test-case "release.yml has push tags v* trigger"
  (define content (read-release-yml))
  (check-true (string-contains? content "on:") "must have 'on:' trigger section")
  (check-true (string-contains? content "push:") "must have 'push:' trigger")
  (check-true (string-contains? content "tags:") "must specify tags filter")
  (check-true (string-contains? content "'v*'") "must trigger on v* tags"))

(test-case "release.yml has test job"
  (define content (read-release-yml))
  (check-true (string-contains? content "  test:") "must have 'test:' job"))

(test-case "release.yml has prepare job"
  (define content (read-release-yml))
  (check-true (string-contains? content "  prepare:") "must have 'prepare:' job"))

(test-case "release.yml has release-core job (calls reusable workflow)"
  (define content (read-release-yml))
  (check-true (string-contains? content "  release-core:") "must have 'release-core:' job"))

(test-case "release.yml release-core uses release-core.yml"
  (define content (read-release-yml))
  (check-true (string-contains? content "release-core.yml")
              "release-core job must use reusable release-core.yml"))

(test-case "release.yml test job is before prepare and release-core"
  (define content (read-release-yml))
  ;; Find positions of job headers to verify ordering
  (define test-pos (regexp-match-positions #rx"  test:" content))
  (define prepare-pos (regexp-match-positions #rx"  prepare:" content))
  (define release-core-pos (regexp-match-positions #rx"  release-core:" content))
  (when (and test-pos prepare-pos release-core-pos)
    (check-true (< (caar test-pos) (caar prepare-pos)) "test must come before prepare")
    (check-true (< (caar prepare-pos) (caar release-core-pos))
                "prepare must come before release-core")))

(test-case "release.yml prepare job depends on test"
  (define content (read-release-yml))
  (check-true (string-contains? content "needs: test") "prepare job must depend on test"))

(test-case "release.yml release-core depends on prepare"
  (define content (read-release-yml))
  (check-true (string-contains? content "needs: prepare") "release-core must depend on prepare"))

(test-case "release.yml has no separate release/smoke jobs (they are in core)"
  (define content (read-release-yml))
  (check-false (string-contains? content "  smoke:") "smoke job must not be in release.yml")
  (check-false (string-contains? content "  release:") "release job must not be in release.yml"))

(test-case "release.yml has version extraction step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Extract version from tag")
              "must have version extraction step"))

(test-case "release.yml has version diagnosis step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Version context diagnostics")
              "must have version context diagnostics step"))

(test-case "release.yml passes secrets to release-core"
  (define content (read-release-yml))
  (check-true (string-contains? content "secrets: inherit") "must pass secrets to release-core"))

(test-case "release.yml uses setup-racket composite action"
  (define content (read-release-yml))
  (check-true (string-contains? content ".github/actions/setup-racket")
              "must use setup-racket composite action"))

(test-case "release.yml has strict release readiness check"
  (define content (read-release-yml))
  (check-true (string-contains? content "lint-release-readiness.rkt --strict")
              "must have strict release readiness check"))

(test-case "release.yml is valid YAML"
  (check-true (file-exists? release-yml-path) "release.yml must exist")
  (define content (read-release-yml))
  (define lines (string-split content "\n"))
  (check-true (> (length lines) 10) "release.yml should have substantial content")
  (for ([line (in-list lines)]
        [i (in-naturals 1)])
    (check-false (string-contains? line "\t") (format "tab found at line ~a" i))))

;; ============================================================
;; release-core.yml — reusable build→smoke→draft→verify→publish→verify
;; ============================================================

(test-case "release-core.yml has workflow_call trigger"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "workflow_call:") "must have workflow_call trigger"))

(test-case "release-core.yml has build job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  build:") "must have 'build:' job"))

(test-case "release-core.yml has smoke job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  smoke:") "must have 'smoke:' job"))

(test-case "release-core.yml has draft job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  draft:") "must have 'draft:' job"))

(test-case "release-core.yml has verify-draft job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  verify-draft:") "must have 'verify-draft:' job"))

(test-case "release-core.yml has publish job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  publish:") "must have 'publish:' job"))

(test-case "release-core.yml has verify-public job"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "  verify-public:") "must have 'verify-public:' job"))

(test-case "release-core.yml DAG order: build→smoke→draft→verify-draft→publish→verify-public"
  (define content (read-release-core-yml))
  ;; build is first (no needs)
  (check-true (string-contains? content "  build:") "build must be first job")
  ;; smoke needs build
  (check-true (string-contains? content "needs: build") "smoke needs build")
  ;; draft needs smoke
  (check-true (string-contains? content "needs: smoke") "draft needs smoke")
  ;; verify-draft needs draft
  (check-true (string-contains? content "needs: draft") "verify-draft needs draft")
  ;; publish needs verify-draft
  (check-true (string-contains? content "needs: verify-draft") "publish needs verify-draft")
  ;; verify-public needs publish
  (check-true (string-contains? content "needs: publish") "verify-public needs publish"))

(test-case "release-core.yml builds tarball with internal artifact upload"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "Build tarball") "must have Build tarball step")
  (check-true (string-contains? content "actions/upload-artifact@v7")
              "must upload internal build artifact"))

(test-case "release-core.yml has smoke with release-smoke suite"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "release-smoke suite") "smoke must use release-smoke suite"))

(test-case "release-core.yml creates draft release (not public directly)"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "--draft") "must create draft (not public) release"))

(test-case "release-core.yml verifies draft assets before publish"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "Verifying draft release assets") "must verify draft assets"))

(test-case "release-core.yml verifies public assets after publish"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "Verifying public release assets")
              "must verify public assets"))

(test-case "release-core.yml publishes with gh release edit --draft=false"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "--draft=false") "publish must flip draft=false"))

;; ============================================================
;; release-repair.yml — diagnostic-only for existing tags
;; ============================================================

(test-case "release-repair.yml is workflow_dispatch only"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "workflow_dispatch:") "must be workflow_dispatch only")
  (check-false (string-contains? content "push:") "must NOT have push trigger"))

(test-case "release-repair.yml has only dry-run mode"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "dry-run") "must have dry-run mode")
  ;; Check that it does NOT have softprops/action-gh-release (no actual publishing)
  (check-false (string-contains? content "softprops/action-gh-release")
               "must NOT use softprops/action-gh-release")
  ;; Check that it does NOT have publish or repair-assets as mode options
  (check-false (string-contains? content "  options:\n          - publish")
               "must NOT have publish mode option")
  (check-false (string-contains? content "  options:\n          - repair-assets")
               "must NOT have repair-assets mode option"))

(test-case "release-repair.yml has contents: read permission"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "contents: read") "must have read-only permission")
  (check-false (string-contains? content "contents: write") "must NOT have write permission"))

(test-case "release-repair.yml has diagnose job"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "  diagnose:") "must have 'diagnose:' job"))

(test-case "release-repair.yml checks if tag is unpublished"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "IS_UNPUBLISHED") "must check if tag has a release"))

(test-case "release-repair.yml routes unpublished tags to normal pipeline"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "unpublished-tag-detected")
              "must detect and describe unpublished tag path"))

(test-case "release-repair.yml never creates or modifies releases"
  (define content (read-release-repair-yml))
  (check-false (string-contains? content "softprops/action-gh-release")
               "must NOT use softprops/action-gh-release (no release mutation)"))

(test-case "release-repair.yml has release diagnostics step"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "release-repair.rkt")
              "must reference release-repair.rkt script"))

(test-case "release-repair.yml is valid YAML"
  (check-true (file-exists? release-repair-yml-path) "release-repair.yml must exist")
  (define content (read-release-repair-yml))
  (define lines (string-split content "\n"))
  (check-true (> (length lines) 10) "release-repair.yml should have substantial content")
  (for ([line (in-list lines)]
        [i (in-naturals 1)])
    (check-false (string-contains? line "\t") (format "tab found at line ~a" i))))

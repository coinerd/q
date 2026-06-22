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

;; ============================================================
;; W2: Mandatory manifest verification contract tests
;; ============================================================

(test-case "release.yml smoke job verifies manifest as mandatory"
  (define content (read-release-yml))
  (check-true (string-contains? content "Verify manifest (mandatory)")
              "smoke job must have 'Verify manifest (mandatory)' step"))

(test-case "release.yml does not contain optional manifest wording"
  (define content (read-release-yml))
  (check-false (string-contains? content "No release-manifest.json found (optional)")
               "smoke job must NOT treat manifest as optional"))

(test-case "release.yml smoke manifest verification has failing path for missing manifest"
  (define content (read-release-yml))
  ;; Must have explicit error + exit 1 for missing manifest
  (check-true (string-contains? content "ERROR: release-manifest.json")
              "must have ERROR message for missing manifest")
  ;; Must have exit 1 for manifest failure
  (check-true (string-contains? content "exit 1") "must have exit 1 for manifest failures"))

(test-case "release.yml smoke manifest verifies version field"
  (define content (read-release-yml))
  (check-true (string-contains? content "manifest version mismatch")
              "must verify manifest version field"))

(test-case "release.yml smoke manifest verifies tag field"
  (define content (read-release-yml))
  (check-true (string-contains? content "manifest tag mismatch") "must verify manifest tag field"))

(test-case "release.yml smoke manifest verifies tarball asset name"
  (define content (read-release-yml))
  (check-true (string-contains? content "manifest tarball name mismatch")
              "must verify manifest tarball asset name"))

(test-case "release.yml smoke manifest verifies sha256 when available"
  (define content (read-release-yml))
  (check-true (string-contains? content "sha256 mismatch")
              "must verify manifest sha256 when tarball is available"))

(test-case "release.yml smoke manifest uses asset API not direct download URL"
  (define content (read-release-yml))
  ;; Must look up release by tag to find asset URL
  (check-true (string-contains? content "releases/tags/${TAG}")
              "must look up release by tag for manifest asset URL")
  ;; Must find manifest via asset filter, not direct download URL
  (check-false (string-contains? content "releases/download/${TAG}/release-manifest.json")
               "must NOT use unreliable direct download URL for manifest"))

(test-case "release.yml smoke manifest has no '|| true' fallback"
  (define content (read-release-yml))
  ;; The old code had '|| true' on the manifest curl — now it's gone.
  ;; Extract manifest step section using regexp-match-positions
  (define start-pos (regexp-match-positions #rx"Verify manifest .mandatory" content))
  (define end-pos (regexp-match-positions #rx"Run test suite" content))
  (when (and start-pos end-pos)
    (define start-idx (caar start-pos))
    (define end-idx (caar end-pos))
    (when (< start-idx end-idx)
      (define manifest-section (substring content start-idx end-idx))
      (check-false (string-contains? manifest-section "|| true")
                   "manifest download must NOT have '|| true' masking failures"))))

;; ============================================================
;; W3: release-smoke suite in release.yml smoke job
;; ============================================================

(test-case "release.yml smoke uses release-smoke suite"
  (define content (read-release-yml))
  (check-true (string-contains? content "--suite release-smoke")
              "smoke job must use --suite release-smoke instead of default/all"))

(test-case "release.yml smoke does NOT run default/all tests"
  (define content (read-release-yml))
  ;; The old invocation was just '--jobs 4' without --suite
  ;; Verify the smoke step has --suite, not bare '--jobs 4'
  (define smoke-section
    (let* ([start-pos (regexp-match-positions #rx"Run release-smoke test suite" content)]
           [end-pos (regexp-match-positions #rx"Upload smoke log" content)])
      (if (and start-pos end-pos (< (caar start-pos) (caar end-pos)))
          (substring content (caar start-pos) (caar end-pos))
          "")))
  (check-false (and (string-contains? smoke-section "run-tests.rkt")
                    (not (string-contains? smoke-section "--suite")))
               "smoke must NOT run run-tests.rkt without --suite"))

(test-case "release.yml smoke step is blocking (no continue-on-error: true)"
  (define content (read-release-yml))
  ;; The smoke step must not have continue-on-error: true
  (define smoke-section
    (let* ([start-pos (regexp-match-positions #rx"Run release-smoke test suite" content)]
           [end-pos (regexp-match-positions #rx"Upload smoke log" content)])
      (if (and start-pos end-pos (< (caar start-pos) (caar end-pos)))
          (substring content (caar start-pos) (caar end-pos))
          "")))
  (check-false (string-contains? smoke-section "continue-on-error: true")
               "smoke step must NOT be continue-on-error: true"))

(test-case "release.yml smoke uploads log on failure"
  (define content (read-release-yml))
  (check-true (string-contains? content "Upload smoke log on failure")
              "must have step to upload smoke log on failure")
  (check-true (string-contains? content "if: failure()") "log upload must trigger on failure"))

(test-case "release.yml smoke logs selected suite"
  (define content (read-release-yml))
  (check-true (string-contains? content "Running release-smoke suite")
              "smoke step must log which suite is being run"))

(test-case "release.yml smoke has failure summary step"
  (define content (read-release-yml))
  (check-true (string-contains? content "Smoke failure summary")
              "must have smoke failure summary step")
  (check-true (string-contains? content "$GITHUB_STEP_SUMMARY")
              "failure summary must write to GITHUB_STEP_SUMMARY"))

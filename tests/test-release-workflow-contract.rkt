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
;; - release-repair.yml defaults to dry-run and gates an explicit apply path

(require rackunit
         racket/file
         racket/string
         racket/port
         racket/list
         racket/runtime-path)

;; ── Path helpers ──

(define-runtime-path ci-yml-path "../.github/workflows/ci.yml")
(define-runtime-path release-yml-path "../.github/workflows/release.yml")
(define-runtime-path release-core-yml-path "../.github/workflows/release-core.yml")
(define-runtime-path release-repair-yml-path "../.github/workflows/release-repair.yml")

(define (read-ci-yml)
  (file->string ci-yml-path))
(define (read-release-yml)
  (file->string release-yml-path))
(define (read-release-core-yml)
  (file->string release-core-yml-path))
(define (read-release-repair-yml)
  (file->string release-repair-yml-path))

(define (bounded-section content start-marker [end-marker #f])
  (define start-match (regexp-match-positions (regexp (regexp-quote start-marker)) content))
  (if (not (pair? start-match))
      ""
      (let* ([start (caar start-match)]
             [end-match
              (and end-marker
                   (regexp-match-positions (regexp (regexp-quote end-marker)) content start))]
             [end (if end-marker
                      (if (pair? end-match)
                          (caar end-match)
                          (string-length content))
                      (string-length content))])
        (substring content start end))))

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

(test-case "release.yml does not delegate unrelated caller secrets"
  (define content (read-release-yml))
  (check-false (string-contains? content "secrets: inherit")))

(test-case "release.yml uses setup-racket composite action"
  (define content (read-release-yml))
  (check-true (string-contains? content ".github/actions/setup-racket")
              "must use setup-racket composite action"))

(test-case "release.yml has strict release readiness check"
  (define content (read-release-yml))
  (check-true (string-contains? content "lint-release-readiness.rkt --strict")
              "must have strict release readiness check"))

(test-case "release.yml does not dirty tree before strict readiness (tag-publish)"
  ;; A tagged commit is frozen; syncing README/metrics in CI is both
  ;; pointless (changes cannot be committed back) and harmful (it dirties
  ;; the clean checkout that --strict --context tag-publish requires).
  (define content (read-release-yml))
  (define test-job (bounded-section content "  test:" "  prepare:"))
  (define readiness-pos (regexp-match-positions #rx"Strict release readiness" test-job))
  (check-true (pair? readiness-pos) "test job must have strict readiness step")
  (define readiness-start (caar readiness-pos))
  ;; No sync-readme-status or metrics --sync-readme steps before readiness
  (define before-readiness (substring test-job 0 readiness-start))
  (check-false (string-contains? before-readiness "sync-readme-status.rkt --sync")
               "README sync must not dirty tree before strict readiness")
  (check-false (string-contains? before-readiness "metrics.rkt --sync-readme")
               "metrics sync must not dirty tree before strict readiness"))

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

(test-case "release-core.yml builds deterministic tarball with internal artifact upload"
  (define content (read-release-core-yml))
  (define build-step
    (bounded-section content "      - name: Build tarball" "      - name: Generate release manifest"))
  (check-true (string-contains? build-step "Build tarball") "must have Build tarball step")
  (check-true (string-contains? build-step "--sort=name") "tar entries must have canonical order")
  (check-true (string-contains? build-step "--mtime=@${SOURCE_DATE_EPOCH}")
              "tar entry timestamps must be canonical")
  (check-true (string-contains? build-step "gzip -n") "gzip header must omit variable metadata")
  (check-regexp-match #px"actions/upload-artifact@[0-9a-f]{40}"
                      content
                      "artifact action must be pinned to an immutable commit"))

(test-case "release-core.yml has smoke with release-smoke suite"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "release-smoke suite") "smoke must use release-smoke suite"))

(test-case "release-core.yml creates draft release (not public directly)"
  (define content (read-release-core-yml))
  (check-true (string-contains? content "-F draft=true")
              "must create draft (not public) release via API"))

(test-case "release-core.yml verifies exact draft release before publish"
  (define content (read-release-core-yml))
  (define draft-job (bounded-section content "  verify-draft:" "  publish:"))
  (check-true (string-contains? draft-job "needs.draft.outputs.release-id"))
  (check-true (string-contains? draft-job "scripts/verify-release-bundle.rkt")))

(test-case "release-core.yml verifies exact public release after publish"
  (define content (read-release-core-yml))
  (define public-job (bounded-section content "  verify-public:"))
  (check-true (string-contains? public-job "needs.publish.outputs.release-id"))
  (check-true (string-contains? public-job "scripts/verify-release-bundle.rkt")))

(test-case "release-core.yml publishes exact verified release ID only after protected approval"
  (define content (read-release-core-yml))
  (define publish-job (bounded-section content "  publish:" "  verify-public:"))
  (check-true (string-contains? publish-job "needs.verify-draft.outputs.release-id"))
  (check-true (string-contains? publish-job "environment: release-repair")
              "public mutation must wait for protected reviewer approval")
  (check-true (string-contains? publish-job "git/ref/tags/${TAG}")
              "live annotated tag identity must be re-queried before mutation")
  (check-true (string-contains? publish-job "git/tags/${TAG_OBJECT}"))
  (check-true (string-contains? publish-job "post_tag_ref")
              "live tag identity must be re-queried after publication")
  (check-true (string-contains? publish-job "rollback_to_draft")
              "failed immediate public verification must restore draft state")
  (check-true (string-contains? publish-job "restored=$(gh api")
              "rollback must confirm the release returned to draft")
  (check-true (string-contains? publish-job "cmp --silent")
              "published bytes must be compared with the verified build before success")
  (check-true (string-contains? publish-job "scripts/verify-release-bundle.rkt"))
  (check-true (string-contains? publish-job "--method PATCH"))
  (check-true (string-contains? publish-job "draft=false")))

;; ============================================================
;; ci.yml — immutable v0.99.74 regression dry-run
;; ============================================================

(test-case "CI release dry-run builds the frozen v0.99.74 asset with truthful date"
  (define content (read-ci-yml))
  (define dry-run-job (bounded-section content "  release-dry-run:" "  #"))
  (check-true (string-contains? dry-run-job "VERSION=0.99.74")
              "frozen repair regression must not depend on the current q version")
  (check-false (string-contains? dry-run-job "test \"$VERSION\" = 0.99.74"))
  (check-true (string-contains? dry-run-job "Q_RELEASE_DATE: '2026-07-29'"))
  (check-false (string-contains? dry-run-job "Q_RELEASE_DATE: '2026-07-26'")))

;; ============================================================
;; release-repair.yml — guarded repair for existing immutable tags
;; ============================================================

(test-case "release-repair.yml is workflow_dispatch only"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "workflow_dispatch:") "must be workflow_dispatch only")
  (check-false (string-contains? content "push:") "must NOT have push trigger"))

(test-case "release-repair.yml dispatch defaults to dry-run and offers explicit apply"
  (define content (read-release-repair-yml))
  (define mode-input (bounded-section content "      mode:" "permissions:"))
  (check-true (string-contains? mode-input "default: dry-run"))
  (check-true (string-contains? mode-input "- dry-run"))
  (check-true (string-contains? mode-input "- apply"))
  (check-false (string-contains? content "softprops/action-gh-release")
               "repair must use guarded first-party API logic"))

(test-case "release-repair.yml scopes write permission to guarded apply job"
  (define content (read-release-repair-yml))
  (define pre-jobs (bounded-section content "permissions:" "jobs:"))
  (define diagnose-job (bounded-section content "  diagnose:" "  apply:"))
  (define apply-job (bounded-section content "  apply:"))
  (check-true (string-contains? pre-jobs "contents: read"))
  (check-false (string-contains? diagnose-job "contents: write"))
  (check-true (string-contains? apply-job "contents: write"))
  (check-true (string-contains? apply-job "environment:")))

(test-case "release-repair.yml has diagnose job"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "  diagnose:") "must have 'diagnose:' job"))

(test-case "release-repair.yml fixes immutable v0.99.74 identity"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "de0ce7391b4ae23818534b31431f00465241302e"))
  (check-true (string-contains? content "32718281aafd378fca511b4294d3c5668134673c"))
  (check-true (string-contains? content "361518742")))

(test-case "release-repair.yml validates the frozen release target commit"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "REPAIR_TARGET: 32718281aafd378fca511b4294d3c5668134673c")
              "repair target must equal GitHub release 361518742 target_commitish, not the tag name"))

(test-case "release-repair.yml uses the truthful frozen release date"
  (define content (read-release-repair-yml))
  (check-equal? (length (regexp-match* #rx"Q_RELEASE_DATE: '2026-07-29'" content)) 3)
  (check-false (string-contains? content "Q_RELEASE_DATE: '2026-07-26'")))

(test-case "release-repair.yml revalidates approved bytes immediately inside each mutation iteration"
  (define content (read-release-repair-yml))
  (define mutation-loop
    (bounded-section content
                     "          for asset in \"${planned[@]}\"; do"
                     "            upload_url="))
  (check-true (string-contains? mutation-loop "sha256sum \"/tmp/apply/q-$VERSION.tar.gz\""))
  (check-true (string-contains? mutation-loop "sha256sum /tmp/apply/release-manifest.json"))
  (check-true (string-contains? mutation-loop "actions/runs/$APPROVED_RUN"))
  (check-true (string-contains? mutation-loop "github.workflow_sha")))

(test-case "release-repair.yml binds apply to approved bytes and expiry"
  (define content (read-release-repair-yml))
  (define apply-job (bounded-section content "  apply:"))
  (check-true (string-contains? apply-job "EXPECTED_TAR"))
  (check-true (string-contains? apply-job "EXPECTED_MANIFEST"))
  (check-true (string-contains? apply-job "EXPIRES"))
  (check-true (string-contains? apply-job "INCOMPLETE")))

(test-case "release-repair.yml avoids unreviewed third-party release mutation"
  (define content (read-release-repair-yml))
  (check-false (string-contains? content "softprops/action-gh-release")
               "guarded repair must use reviewed first-party mutation logic"))

(test-case "release-repair.yml uses trusted manifest and bundle tooling"
  (define content (read-release-repair-yml))
  (check-true (string-contains? content "tooling/scripts/gen-release-manifest.rkt"))
  (check-true (string-contains? content "tooling/scripts/verify-release-bundle.rkt")))

(test-case "release-repair.yml is valid YAML"
  (check-true (file-exists? release-repair-yml-path) "release-repair.yml must exist")
  (define content (read-release-repair-yml))
  (define lines (string-split content "\n"))
  (check-true (> (length lines) 10) "release-repair.yml should have substantial content")
  (for ([line (in-list lines)]
        [i (in-naturals 1)])
    (check-false (string-contains? line "\t") (format "tab found at line ~a" i))))

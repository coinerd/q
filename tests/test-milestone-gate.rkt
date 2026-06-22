#lang racket

;; @suite ci
;; @speed fast
;; tests/test-milestone-gate.rkt
;; W5 (#8522): Tests for milestone-gate.rkt release truth verification.
;;
;; Tests use mocked release data (jsexpr hashes) to verify the pure
;; validation logic without network calls.

(require rackunit
         racket/file
         racket/string
         racket/port
         racket/system
         json)

;; ── Script loading ──

(define script-path "../scripts/milestone-gate.rkt")

(define (dynamic-script sym)
  (dynamic-require script-path sym))

;; ── Mock data builders ──

(define (make-release-data #:tag-name [tag "v0.99.40"] #:draft [draft #f] #:assets [assets '()])
  (hasheq 'tag_name
          tag
          'draft
          draft
          'prerelease
          #f
          'assets
          assets
          'published_at
          "2026-06-21T00:00:00Z"))

(define (make-asset name)
  (hasheq 'name name 'url (format "https://api.github.com/assets/~a" name) 'size 12345))

(define (complete-release-data)
  (make-release-data #:assets (list (make-asset "q-0.99.40.tar.gz")
                                    (make-asset "release-manifest.json"))))

(define (tarball-only-release-data)
  (make-release-data #:assets (list (make-asset "q-0.99.40.tar.gz"))))

(define (manifest-only-release-data)
  (make-release-data #:assets (list (make-asset "release-manifest.json"))))

(define (assetless-release-data)
  (make-release-data #:assets '()))

;; ============================================================
;; extract-version-from-milestone-title tests
;; ============================================================

(test-case "extract-version: v-prefixed title"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "v0.99.40 — 2026-06-21"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: bare version title"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "0.99.40"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: title with extra text"
  (define result
    ((dynamic-script 'extract-version-from-milestone-title)
     "v0.99.40 GitHub Release Automation Restoration"))
  (check-equal? result "0.99.40"))

(test-case "extract-version: malformed title returns #f"
  (define result ((dynamic-script 'extract-version-from-milestone-title) "no version here"))
  (check-false result))

;; ============================================================
;; validate-release-data: complete release passes
;; ============================================================

(test-case "validate-release-data: complete release passes"
  (define result ((dynamic-script 'validate-release-data) (complete-release-data) "0.99.40"))
  (check-true (car result) "should pass")
  (check-true (string-contains? (cadr result) "verified"))
  ;; Check the release hash structure
  (define release-hash (hash-ref (caddr result) 'release))
  (check-true (hash-ref release-hash 'exists))
  (check-true (hash-ref release-hash 'tarball_asset))
  (check-true (hash-ref release-hash 'manifest_asset))
  (check-true (hash-ref release-hash 'pass)))

;; ============================================================
;; validate-release-data: missing release fails
;; ============================================================

(test-case "validate-release-data: no release fails"
  (define result ((dynamic-script 'validate-release-data) #f "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "No release"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'exists))
  (check-false (hash-ref release-hash 'pass)))

;; ============================================================
;; validate-release-data: assetless release fails
;; ============================================================

(test-case "validate-release-data: assetless release fails"
  (define result ((dynamic-script 'validate-release-data) (assetless-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing all assets"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'tarball_asset))
  (check-false (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: tarball-only release fails
;; ============================================================

(test-case "validate-release-data: tarball-only release fails"
  (define result ((dynamic-script 'validate-release-data) (tarball-only-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing manifest"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-true (hash-ref release-hash 'tarball_asset))
  (check-false (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: manifest-only release fails
;; ============================================================

(test-case "validate-release-data: manifest-only release fails"
  (define result ((dynamic-script 'validate-release-data) (manifest-only-release-data) "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "missing tarball"))
  (define release-hash (hash-ref (caddr result) 'release))
  (check-false (hash-ref release-hash 'tarball_asset))
  (check-true (hash-ref release-hash 'manifest_asset)))

;; ============================================================
;; validate-release-data: draft release fails
;; ============================================================

(test-case "validate-release-data: draft release fails"
  (define data
    (make-release-data #:draft #t
                       #:assets (list (make-asset "q-0.99.40.tar.gz")
                                      (make-asset "release-manifest.json"))))
  (define result ((dynamic-script 'validate-release-data) data "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "draft")))

;; ============================================================
;; validate-release-data: tag mismatch fails
;; ============================================================

(test-case "validate-release-data: tag mismatch fails"
  (define data
    (make-release-data #:tag-name "v0.99.39"
                       #:assets (list (make-asset "q-0.99.39.tar.gz")
                                      (make-asset "release-manifest.json"))))
  (define result ((dynamic-script 'validate-release-data) data "0.99.40"))
  (check-false (car result) "should fail")
  (check-true (string-contains? (cadr result) "mismatch")))

;; ============================================================
;; release-has-asset? helper tests
;; ============================================================

(test-case "release-has-asset?: finds existing asset"
  (define data (complete-release-data))
  (check-true ((dynamic-script 'release-has-asset?) data "q-0.99.40.tar.gz")))

(test-case "release-has-asset?: returns #f for missing asset"
  (define data (complete-release-data))
  (check-false ((dynamic-script 'release-has-asset?) data "nonexistent.zip")))

(test-case "release-has-asset?: handles #f release-data"
  (check-false ((dynamic-script 'release-has-asset?) #f "anything.zip")))

;; ============================================================
;; JSON structure tests
;; ============================================================

(test-case "make-release-check-result: produces valid structure"
  (define result ((dynamic-script 'make-release-check-result) #t "v0.99.40" #t #t #t "all good"))
  (define release-hash (hash-ref result 'release))
  (check-equal? (hash-ref release-hash 'exists) #t)
  (check-equal? (hash-ref release-hash 'tag) "v0.99.40")
  (check-equal? (hash-ref release-hash 'tarball_asset) #t)
  (check-equal? (hash-ref release-hash 'manifest_asset) #t)
  (check-equal? (hash-ref release-hash 'pass) #t)
  (check-equal? (hash-ref release-hash 'detail) "all good"))

;; ============================================================
;; Script existence tests
;; ============================================================

(test-case "milestone-gate.rkt exists and compiles"
  (check-true (file-exists? "../scripts/milestone-gate.rkt"))
  (check-not-exn (lambda () (dynamic-require "../scripts/milestone-gate.rkt" #f))))

;; ============================================================
;; W4: classify-release-verdict tests (in milestone-gate.rkt)
;; ============================================================

(test-case "classify-release-verdict: workflow_success"
  (define result
    ((dynamic-script 'classify-release-verdict)
     (hasheq 'conclusion "success" 'head_branch "v0.99.41" 'run_number 600)
     (hasheq 'test "success" 'release "success" 'smoke "success")
     (hasheq 'release_exists #t 'tarball #t 'manifest #t)
     "v0.99.41"))
  (check-equal? result 'workflow_success))

(test-case "classify-release-verdict: publication_succeeded_smoke_failed"
  (define result
    ((dynamic-script 'classify-release-verdict)
     (hasheq 'conclusion "failure" 'head_branch "v0.99.40" 'run_number 581)
     (hasheq 'test "success" 'release "success" 'smoke "failure")
     (hasheq 'release_exists #t 'tarball #t 'manifest #t)
     "v0.99.40"))
  (check-equal? result 'publication_succeeded_smoke_failed))

(test-case "classify-release-verdict: no_release_run"
  (define result
    ((dynamic-script 'classify-release-verdict) #f (hasheq) (hasheq 'release_exists #f) "v0.99.41"))
  (check-equal? result 'no_release_run))

(test-case "classify-release-verdict: make-workflow-verdict-result structure"
  (define result
    ((dynamic-script 'make-workflow-verdict-result) 12345
                                                    581
                                                    "failure"
                                                    (hasheq 'test "success")
                                                    (hasheq 'release_exists #t)
                                                    'publication_succeeded_smoke_failed
                                                    #f))
  (check-equal? (hash-ref (hash-ref result 'workflow) 'run_number) 581)
  (check-equal? (hash-ref result 'verdict) 'publication_succeeded_smoke_failed))

;; ============================================================
;; W5: classify-ci-verdict tests
;; ============================================================

(test-case "classify-ci-verdict: ci_success"
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 600)
     (make-hash '(("lint" . "success") ("test" . "success")))
     '("lint" "test")))
  (check-equal? result 'ci_success))

(test-case "classify-ci-verdict: ci_in_progress"
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "in_progress" 'conclusion #f 'run_number 601)
     (hash)
     '("lint" "test")))
  (check-equal? result 'ci_in_progress))

(test-case "classify-ci-verdict: ci_failure"
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "failure" 'run_number 602)
     (hash)
     '("lint" "test")))
  (check-equal? result 'ci_failure))

(test-case "classify-ci-verdict: ci_cancelled"
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "cancelled" 'run_number 603)
     (hash)
     '("lint" "test")))
  (check-equal? result 'ci_cancelled))

(test-case "classify-ci-verdict: ci_no_runs"
  (define result ((dynamic-script 'classify-ci-verdict) #f (hash) '("lint" "test")))
  (check-equal? result 'ci_no_runs))

(test-case "classify-ci-verdict: ci_required_job_unexpectedly_skipped"
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 604)
     (make-hash '(("lint" . "success") ("test" . "skipped")))
     '("lint" "test")))
  (check-equal? result 'ci_required_job_unexpectedly_skipped))

(test-case "classify-ci-verdict: allowed job skipped → ci_success"
  ;; release-readiness not in required list → skip is OK
  (define result
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 605)
     (make-hash '(("lint" . "success") ("test" . "success") ("release-readiness" . "skipped")))
     '("lint" "test")))
  (check-equal? result 'ci_success))

(test-case "classify-ci-verdict: all required jobs in ci-required-jobs default"
  (define jobs (dynamic-script 'ci-required-jobs))
  (check-not-false (member "test" jobs))
  (check-not-false (member "security" jobs)))

;; ============================================================
;; W6 (#8568): Lifecycle transition model tests
;; ============================================================

(require (only-in "../scripts/milestone-gate.rkt"
                  milestone-lifecycle-transition-result
                  milestone-lifecycle-transition-result?
                  milestone-lifecycle-transition-result-from
                  milestone-lifecycle-transition-result-to
                  milestone-lifecycle-transition-result-ok?
                  milestone-lifecycle-transition-result-reason))

;; --- milestone-lifecycle-states ---

(test-case "lifecycle-states: has 6 states in order"
  (define states (dynamic-script 'milestone-lifecycle-states))
  (check-equal? states '(planned in_progress release_ready release_published ci_green closed)))

(test-case "lifecycle-states: length is 6"
  (check-equal? (length (dynamic-script 'milestone-lifecycle-states)) 6))

;; --- milestone-lifecycle-next ---

(test-case "lifecycle-next: planned → in_progress"
  (check-equal? ((dynamic-script 'milestone-lifecycle-next) 'planned) 'in_progress))

(test-case "lifecycle-next: in_progress → release_ready"
  (check-equal? ((dynamic-script 'milestone-lifecycle-next) 'in_progress) 'release_ready))

(test-case "lifecycle-next: release_ready → release_published"
  (check-equal? ((dynamic-script 'milestone-lifecycle-next) 'release_ready) 'release_published))

(test-case "lifecycle-next: release_published → ci_green"
  (check-equal? ((dynamic-script 'milestone-lifecycle-next) 'release_published) 'ci_green))

(test-case "lifecycle-next: ci_green → closed"
  (check-equal? ((dynamic-script 'milestone-lifecycle-next) 'ci_green) 'closed))

(test-case "lifecycle-next: closed → #f (no next state)"
  (check-false ((dynamic-script 'milestone-lifecycle-next) 'closed)))

(test-case "lifecycle-next: invalid state returns #f"
  (check-false ((dynamic-script 'milestone-lifecycle-next) 'nonexistent)))

;; --- milestone-valid-transition? ---

(test-case "valid-transition?: planned → in_progress is valid"
  (check-true ((dynamic-script 'milestone-valid-transition?) 'planned 'in_progress)))

(test-case "valid-transition?: in_progress → release_ready is valid"
  (check-true ((dynamic-script 'milestone-valid-transition?) 'in_progress 'release_ready)))

(test-case "valid-transition?: ci_green → closed is valid"
  (check-true ((dynamic-script 'milestone-valid-transition?) 'ci_green 'closed)))

(test-case "valid-transition?: planned → closed is invalid (skipping steps)"
  (check-false ((dynamic-script 'milestone-valid-transition?) 'planned 'closed)))

(test-case "valid-transition?: in_progress → ci_green is invalid (skipping steps)"
  (check-false ((dynamic-script 'milestone-valid-transition?) 'in_progress 'ci_green)))

(test-case "valid-transition?: release_published → release_ready is invalid (backwards)"
  (check-false ((dynamic-script 'milestone-valid-transition?) 'release_published 'release_ready)))

(test-case "valid-transition?: closed → planned is invalid (backwards from terminal)"
  (check-false ((dynamic-script 'milestone-valid-transition?) 'closed 'planned)))

;; --- can-close-milestone? (guard function) ---

(test-case "can-close: success+success → allowed"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_success))
  (check-true (milestone-lifecycle-transition-result-ok? r))
  (check-equal? (milestone-lifecycle-transition-result-from r) 'ci_green)
  (check-equal? (milestone-lifecycle-transition-result-to r) 'closed))

(test-case "can-close: release blocking → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'no_release_run 'ci_success))
  (check-false (milestone-lifecycle-transition-result-ok? r))
  (check-true (string-contains? (milestone-lifecycle-transition-result-reason r) "release")))

(test-case "can-close: release_failed → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'release_failed 'ci_success))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: CI blocking → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_in_progress))
  (check-false (milestone-lifecycle-transition-result-ok? r))
  (check-true (string-contains? (milestone-lifecycle-transition-result-reason r) "CI")))

(test-case "can-close: ci_failure → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_failure))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: ci_cancelled → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_cancelled))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: stale_run → blocked (release not confirmed)"
  (define r ((dynamic-script 'can-close-milestone?) 'stale_run 'ci_success))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: unknown release verdict → blocked"
  (define r ((dynamic-script 'can-close-milestone?) 'unknown 'ci_success))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: ci_required_job_unexpectedly_skipped → blocked"
  (define r
    ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_required_job_unexpectedly_skipped))
  (check-false (milestone-lifecycle-transition-result-ok? r)))

(test-case "can-close: success+success reason is 'all gates passed'"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_success))
  (check-equal? (milestone-lifecycle-transition-result-reason r) "all gates passed"))

(test-case "can-close: result is milestone-lifecycle-transition-result?"
  (define r ((dynamic-script 'can-close-milestone?) 'workflow_success 'ci_success))
  (check-pred milestone-lifecycle-transition-result? r))

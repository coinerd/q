#lang racket

;; @suite ci
;; @speed fast
;; tests/test-release-audit-truth.rkt
;; W4 (#8544): Tests for release workflow verdict / audit-truth tooling.
;;
;; These tests verify the pure verdict classification logic in
;; milestone-gate.rkt, specifically distinguishing:
;;   - Publication succeeded but workflow failed (the #581 scenario)
;;   - Workflow fully succeeded
;;   - Release job failed
;;   - Test failed, release skipped
;;   - No release run exists
;;   - Stale run for wrong tag

(require rackunit
         json)

;; ── Script loading ──

(define script-path "../scripts/milestone-gate.rkt")

(define (dynamic-script sym)
  (dynamic-require script-path sym))

;; ── Mock data builders ──

;; #581-like scenario: test + release succeeded, smoke failed, assets published
(define (make-581-workflow-data)
  (hasheq 'conclusion "failure" 'head_branch "v0.99.40" 'run_number 581))

(define (make-581-jobs)
  (hasheq 'test "success" 'release "success" 'smoke "failure"))

(define (make-581-assets)
  (hasheq 'release_exists #t 'tarball #t 'manifest #t))

;; Full success scenario
(define (make-success-workflow-data)
  (hasheq 'conclusion "success" 'head_branch "v0.99.41" 'run_number 600))

(define (make-success-jobs)
  (hasheq 'test "success" 'release "success" 'smoke "success"))

;; Test failure scenario
(define (make-test-failed-workflow-data)
  (hasheq 'conclusion "failure" 'head_branch "v0.99.41" 'run_number 601))

(define (make-test-failed-jobs)
  (hasheq 'test "failure" 'release #f 'smoke #f))

;; Release failure scenario
(define (make-release-failed-workflow-data)
  (hasheq 'conclusion "failure" 'head_branch "v0.99.41" 'run_number 602))

(define (make-release-failed-jobs)
  (hasheq 'test "success" 'release "failure" 'smoke #f))

;; Stale run scenario
(define (make-stale-workflow-data)
  (hasheq 'conclusion "failure" 'head_branch "v0.99.40" 'run_number 580))

;; ============================================================
;; classify-release-verdict tests
;; ============================================================

(test-case "#581 scenario: publication succeeded, smoke failed"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  (check-equal? verdict 'publication_succeeded_smoke_failed))

(test-case "full workflow success"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-success-workflow-data)
                                                (make-success-jobs)
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-equal? verdict 'workflow_success))

(test-case "test failed, release skipped"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-test-failed-workflow-data)
                                                (make-test-failed-jobs)
                                                (hasheq 'release_exists #f 'tarball #f 'manifest #f)
                                                "v0.99.41"))
  (check-equal? verdict 'test_failed_release_skipped))

(test-case "release job itself failed"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-release-failed-workflow-data)
                                                (make-release-failed-jobs)
                                                (hasheq 'release_exists #f 'tarball #f 'manifest #f)
                                                "v0.99.41"))
  (check-equal? verdict 'release_failed))

(test-case "no release run at all"
  (define verdict
    ((dynamic-script 'classify-release-verdict) #f (hasheq) (hasheq 'release_exists #f) "v0.99.41"))
  (check-equal? verdict 'no_release_run))

(test-case "stale run for wrong tag"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-stale-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-equal? verdict 'stale_run))

(test-case "unknown scenario: workflow success but jobs missing"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-success-workflow-data)
                                                (hasheq) ; no job data
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-equal? verdict 'unknown))

;; ============================================================
;; make-workflow-verdict-result structure tests
;; ============================================================

(test-case "make-workflow-verdict-result produces valid structure"
  (define result
    ((dynamic-script 'make-workflow-verdict-result) 27934598531
                                                    581
                                                    "failure"
                                                    (make-581-jobs)
                                                    (make-581-assets)
                                                    'publication_succeeded_smoke_failed
                                                    #f))
  (define wf-hash (hash-ref result 'workflow))
  (check-equal? (hash-ref wf-hash 'run_id) 27934598531)
  (check-equal? (hash-ref wf-hash 'run_number) 581)
  (check-equal? (hash-ref wf-hash 'conclusion) "failure")
  (check-false (hash-ref wf-hash 'pass))
  (check-equal? (hash-ref result 'verdict) 'publication_succeeded_smoke_failed)
  ;; Jobs should be present
  (define jobs (hash-ref result 'jobs))
  (check-equal? (hash-ref jobs 'test) "success")
  (check-equal? (hash-ref jobs 'release) "success")
  (check-equal? (hash-ref jobs 'smoke) "failure")
  ;; Assets should be present
  (define assets (hash-ref result 'release_assets))
  (check-true (hash-ref assets 'release_exists))
  (check-true (hash-ref assets 'tarball))
  (check-true (hash-ref assets 'manifest)))

(test-case "make-workflow-verdict-result with #f inputs produces defaults"
  (define result ((dynamic-script 'make-workflow-verdict-result) #f #f #f #f #f #f #f))
  (define wf-hash (hash-ref result 'workflow))
  (check-equal? (hash-ref wf-hash 'run_id) 0)
  (check-equal? (hash-ref wf-hash 'conclusion) "unknown")
  (check-false (hash-ref wf-hash 'pass))
  (check-equal? (hash-ref result 'verdict) "unknown"))

;; ============================================================
;; Audit truth: #581 fixture — publication succeeded despite workflow failure
;; ============================================================

(test-case "#581 fixture: workflow-level truth is failure despite release assets"
  ;; This is the core audit-truth test: even though release assets exist,
  ;; the workflow-level conclusion is failure.
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  ;; The verdict must NOT be workflow_success
  (check-false (eq? verdict 'workflow_success)
               "Audit truth: #581 is NOT workflow_success despite published assets")
  ;; The verdict must specifically identify the publication_succeeded_smoke_failed case
  (check-equal? verdict
                'publication_succeeded_smoke_failed
                "Must distinguish publication success from workflow success"))

(test-case "#581 fixture: milestone gate would fail on workflow truth"
  ;; The gate check for Release workflow should fail for #581-like data
  ;; (no --allow-workflow-failure override)
  ;; This is a contract test — the milestone-gate function
  ;; classify-release-verdict returns a non-success verdict for #581.
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  (check-not-false
   (member verdict '(publication_succeeded_smoke_failed release_failed test_failed_release_skipped))
   "Non-success verdict must be a known failure type"))

;; ============================================================
;; Script existence and export tests
;; ============================================================

(test-case "milestone-gate.rkt exports classify-release-verdict"
  (check-not-exn (lambda () (dynamic-require script-path 'classify-release-verdict))))

(test-case "milestone-gate.rkt exports make-workflow-verdict-result"
  (check-not-exn (lambda () (dynamic-require script-path 'make-workflow-verdict-result))))

;; ============================================================
;; W5: classify-ci-verdict tests
;; ============================================================

(define required-jobs '("lint" "test" "security" "smoke" "workflows"))

(test-case "CI success: all required jobs passed"
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 600)
     (make-hash '(("lint" . "success") ("test" . "success")
                                       ("security" . "success")
                                       ("smoke" . "success")
                                       ("workflows" . "success")))
     required-jobs))
  (check-equal? verdict 'ci_success))

(test-case "CI in_progress blocks closure"
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "in_progress" 'conclusion #f 'run_number 601)
     (hash)
     required-jobs))
  (check-equal? verdict 'ci_in_progress))

(test-case "CI failure blocks closure"
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "failure" 'run_number 602)
     (hash)
     required-jobs))
  (check-equal? verdict 'ci_failure))

(test-case "CI cancelled blocks closure"
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "cancelled" 'run_number 603)
     (hash)
     required-jobs))
  (check-equal? verdict 'ci_cancelled))

(test-case "CI no runs found"
  (define verdict ((dynamic-script 'classify-ci-verdict) #f (hash) required-jobs))
  (check-equal? verdict 'ci_no_runs))

(test-case "CI required job unexpectedly skipped fails"
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 604)
     (make-hash '(("lint" . "success") ("test" . "skipped")
                                       ("security" . "success")
                                       ("smoke" . "success")
                                       ("workflows" . "success")))
     required-jobs))
  (check-equal? verdict 'ci_required_job_unexpectedly_skipped))

(test-case "CI allowed job skipped passes (release-readiness not in required)"
  ;; release-readiness is not in required-jobs, so skipping it is OK
  (define verdict
    ((dynamic-script 'classify-ci-verdict)
     (hasheq 'status "completed" 'conclusion "success" 'run_number 605)
     (make-hash '(("lint" . "success") ("test" . "success")
                                       ("security" . "success")
                                       ("smoke" . "success")
                                       ("workflows" . "success")
                                       ("release-readiness" . "skipped")))
     required-jobs))
  (check-equal? verdict 'ci_success))

(test-case "make-ci-check-result structure"
  (define result
    ((dynamic-script 'make-ci-check-result) 12345
                                            600
                                            "completed"
                                            "success"
                                            'ci_success
                                            #t
                                            "All good"))
  (define ci-hash (hash-ref result 'ci))
  (check-equal? (hash-ref ci-hash 'run_number) 600)
  (check-equal? (hash-ref ci-hash 'verdict) 'ci_success)
  (check-true (hash-ref ci-hash 'pass)))

(test-case "ci-required-jobs exports complete current matrix contexts"
  (define jobs (dynamic-script 'ci-required-jobs))
  (check-not-false (member "test (ubuntu-latest, 8.10)" jobs))
  (check-not-false (member "test (ubuntu-latest, 8.11)" jobs))
  (check-not-false (member "test (macos-latest, 8.10)" jobs))
  (check-not-false (member "security" jobs))
  (check-not-false (member "smoke (ubuntu-latest)" jobs))
  (check-not-false (member "smoke (macos-latest)" jobs)))

(test-case "milestone-gate.rkt exports classify-ci-verdict"
  (check-not-exn (lambda () (dynamic-require script-path 'classify-ci-verdict))))

(test-case "milestone-gate.rkt exports make-ci-check-result"
  (check-not-exn (lambda () (dynamic-require script-path 'make-ci-check-result))))

;; ============================================================
;; W2 (#8564): Verdict predicate layer tests
;; ============================================================

;; --- Release verdict predicates ---

(test-case "release-verdict-success?: workflow_success is success"
  (check-true ((dynamic-script 'release-verdict-success?) 'workflow_success)))

(test-case "release-verdict-success?: publication_succeeded_smoke_failed is NOT success"
  (check-false ((dynamic-script 'release-verdict-success?) 'publication_succeeded_smoke_failed)))

(test-case "release-verdict-success?: no_release_run is NOT success"
  (check-false ((dynamic-script 'release-verdict-success?) 'no_release_run)))

(test-case "release-verdict-success?: #f is NOT success"
  (check-false ((dynamic-script 'release-verdict-success?) #f)))

(test-case "release-verdict-blocking?: publication_succeeded_smoke_failed blocks"
  (check-true ((dynamic-script 'release-verdict-blocking?) 'publication_succeeded_smoke_failed)))

(test-case "release-verdict-blocking?: release_failed blocks"
  (check-true ((dynamic-script 'release-verdict-blocking?) 'release_failed)))

(test-case "release-verdict-blocking?: test_failed_release_skipped blocks"
  (check-true ((dynamic-script 'release-verdict-blocking?) 'test_failed_release_skipped)))

(test-case "release-verdict-blocking?: no_release_run blocks"
  (check-true ((dynamic-script 'release-verdict-blocking?) 'no_release_run)))

(test-case "release-verdict-blocking?: unknown blocks"
  (check-true ((dynamic-script 'release-verdict-blocking?) 'unknown)))

(test-case "release-verdict-blocking?: workflow_success does NOT block"
  (check-false ((dynamic-script 'release-verdict-blocking?) 'workflow_success)))

(test-case "release-verdict-blocking?: stale_run does NOT block"
  (check-false ((dynamic-script 'release-verdict-blocking?) 'stale_run)))

(test-case "release-verdict-blocking?: #f does NOT block"
  (check-false ((dynamic-script 'release-verdict-blocking?) #f)))

(test-case "release-verdict-superseded?: stale_run is superseded"
  (check-true ((dynamic-script 'release-verdict-superseded?) 'stale_run)))

(test-case "release-verdict-superseded?: workflow_success is NOT superseded"
  (check-false ((dynamic-script 'release-verdict-superseded?) 'workflow_success)))

(test-case "release-verdict->approval-state: workflow_success → approved"
  (check-equal? ((dynamic-script 'release-verdict->approval-state) 'workflow_success) 'approved))

(test-case "release-verdict->approval-state: stale_run → superseded"
  (check-equal? ((dynamic-script 'release-verdict->approval-state) 'stale_run) 'superseded))

(test-case "release-verdict->approval-state: publication_succeeded_smoke_failed → blocked"
  (check-equal?
   ((dynamic-script 'release-verdict->approval-state) 'publication_succeeded_smoke_failed)
   'blocked))

(test-case "release-verdict->approval-state: release_failed → blocked"
  (check-equal? ((dynamic-script 'release-verdict->approval-state) 'release_failed) 'blocked))

;; --- CI verdict predicates ---

(test-case "ci-verdict-success?: ci_success is success"
  (check-true ((dynamic-script 'ci-verdict-success?) 'ci_success)))

(test-case "ci-verdict-success?: ci_failure is NOT success"
  (check-false ((dynamic-script 'ci-verdict-success?) 'ci_failure)))

(test-case "ci-verdict-blocking?: ci_failure blocks"
  (check-true ((dynamic-script 'ci-verdict-blocking?) 'ci_failure)))

(test-case "ci-verdict-blocking?: ci_in_progress blocks"
  (check-true ((dynamic-script 'ci-verdict-blocking?) 'ci_in_progress)))

(test-case "ci-verdict-blocking?: ci_cancelled blocks"
  (check-true ((dynamic-script 'ci-verdict-blocking?) 'ci_cancelled)))

(test-case "ci-verdict-blocking?: ci_no_runs blocks"
  (check-true ((dynamic-script 'ci-verdict-blocking?) 'ci_no_runs)))

(test-case "ci-verdict-blocking?: ci_success does NOT block"
  (check-false ((dynamic-script 'ci-verdict-blocking?) 'ci_success)))

(test-case "ci-verdict-blocking?: #f does NOT block"
  (check-false ((dynamic-script 'ci-verdict-blocking?) #f)))

(test-case "ci-verdict->approval-state: ci_success → approved"
  (check-equal? ((dynamic-script 'ci-verdict->approval-state) 'ci_success) 'approved))

(test-case "ci-verdict->approval-state: ci_failure → blocked"
  (check-equal? ((dynamic-script 'ci-verdict->approval-state) 'ci_failure) 'blocked))

;; --- GitHub status constant tests ---

(test-case "GITHUB-SUCCESS constant equals \"success\""
  (check-equal? (dynamic-script 'GITHUB-SUCCESS) "success"))

(test-case "GITHUB-FAILURE constant equals \"failure\""
  (check-equal? (dynamic-script 'GITHUB-FAILURE) "failure"))

(test-case "GITHUB-CANCELLED constant equals \"cancelled\""
  (check-equal? (dynamic-script 'GITHUB-CANCELLED) "cancelled"))

(test-case "GITHUB-SKIPPED constant equals \"skipped\""
  (check-equal? (dynamic-script 'GITHUB-SKIPPED) "skipped"))

(test-case "GITHUB-COMPLETED constant equals \"completed\""
  (check-equal? (dynamic-script 'GITHUB-COMPLETED) "completed"))

;; ============================================================
;; W2 (#8564): #581 vs #582 compatibility fixtures through predicates
;; ============================================================

(test-case "#581 compatibility: verdict classified as blocking via predicate"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  (check-true ((dynamic-script 'release-verdict-blocking?) verdict)
              "#581 must be blocking via predicate"))

(test-case "#581 compatibility: verdict NOT classified as success via predicate"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  (check-false ((dynamic-script 'release-verdict-success?) verdict)
               "#581 must NOT be success via predicate"))

(test-case "#581 compatibility: approval-state is blocked"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-581-workflow-data)
                                                (make-581-jobs)
                                                (make-581-assets)
                                                "v0.99.40"))
  (check-equal? ((dynamic-script 'release-verdict->approval-state) verdict) 'blocked))

(test-case "#582 compatibility: verdict classified as success via predicate"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-success-workflow-data)
                                                (make-success-jobs)
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-true ((dynamic-script 'release-verdict-success?) verdict)
              "#582 must be success via predicate"))

(test-case "#582 compatibility: verdict NOT classified as blocking via predicate"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-success-workflow-data)
                                                (make-success-jobs)
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-false ((dynamic-script 'release-verdict-blocking?) verdict)
               "#582 must NOT be blocking via predicate"))

(test-case "#582 compatibility: approval-state is approved"
  (define verdict
    ((dynamic-script 'classify-release-verdict) (make-success-workflow-data)
                                                (make-success-jobs)
                                                (make-581-assets)
                                                "v0.99.41"))
  (check-equal? ((dynamic-script 'release-verdict->approval-state) verdict) 'approved))

;; --- Predicate export tests ---

(test-case "milestone-gate.rkt exports release-verdict-success?"
  (check-not-exn (lambda () (dynamic-require script-path 'release-verdict-success?))))

(test-case "milestone-gate.rkt exports release-verdict-blocking?"
  (check-not-exn (lambda () (dynamic-require script-path 'release-verdict-blocking?))))

(test-case "milestone-gate.rkt exports release-verdict-superseded?"
  (check-not-exn (lambda () (dynamic-require script-path 'release-verdict-superseded?))))

(test-case "milestone-gate.rkt exports release-verdict->approval-state"
  (check-not-exn (lambda () (dynamic-require script-path 'release-verdict->approval-state))))

(test-case "milestone-gate.rkt exports ci-verdict-success?"
  (check-not-exn (lambda () (dynamic-require script-path 'ci-verdict-success?))))

(test-case "milestone-gate.rkt exports ci-verdict-blocking?"
  (check-not-exn (lambda () (dynamic-require script-path 'ci-verdict-blocking?))))

(test-case "milestone-gate.rkt exports ci-verdict->approval-state"
  (check-not-exn (lambda () (dynamic-require script-path 'ci-verdict->approval-state))))

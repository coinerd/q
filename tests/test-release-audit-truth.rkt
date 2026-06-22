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

(test-case "ci-required-jobs default list exported"
  (define jobs (dynamic-script 'ci-required-jobs))
  (check-not-false (member "test" jobs))
  (check-not-false (member "security" jobs))
  (check-not-false (member "smoke" jobs)))

(test-case "milestone-gate.rkt exports classify-ci-verdict"
  (check-not-exn (lambda () (dynamic-require script-path 'classify-ci-verdict))))

(test-case "milestone-gate.rkt exports make-ci-check-result"
  (check-not-exn (lambda () (dynamic-require script-path 'make-ci-check-result))))

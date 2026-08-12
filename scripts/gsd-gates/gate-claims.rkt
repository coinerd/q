#lang racket

;; scripts/gsd-gates/gate-claims.rkt — Claim/verdict classification
;;
;; Extracted from scripts/milestone-gate.rkt (v0.99.42+).
;; Pure functions for classifying release and CI verdicts,
;; and the milestone lifecycle state machine.
;;
;; These functions operate on structured data (hashes), not I/O.

(require racket/list
         racket/match
         racket/string)

;; ---------------------------------------------------------------------------
;; W6 (#8568): Lifecycle transition model
;; ---------------------------------------------------------------------------
(define milestone-lifecycle-states
  '(planned in_progress release_ready release_published ci_green closed))

(struct milestone-lifecycle-transition-result (from to ok? reason) #:transparent)

(define (milestone-lifecycle-next state)
  (define idx (index-of milestone-lifecycle-states state))
  (and idx
       (< (add1 idx) (length milestone-lifecycle-states))
       (list-ref milestone-lifecycle-states (add1 idx))))

(define (milestone-valid-transition? from-state to-state)
  (equal? (milestone-lifecycle-next from-state) to-state))

;; ---------------------------------------------------------------------------
;; W2 (#8564): GitHub Actions status string constants
;; ---------------------------------------------------------------------------

(define GITHUB-SUCCESS "success")
(define GITHUB-FAILURE "failure")
(define GITHUB-CANCELLED "cancelled")
(define GITHUB-SKIPPED "skipped")
(define GITHUB-COMPLETED "completed")
(define GITHUB-IN-PROGRESS "in_progress")

;; ---------------------------------------------------------------------------
;; W2 (#8564): Verdict predicate layer
;; ---------------------------------------------------------------------------

(define release-verdicts
  '(no_release_run test_failed_release_skipped
                   release_failed
                   publication_succeeded_smoke_failed
                   workflow_success
                   stale_run
                   unknown))

(define release-blocking-verdicts
  '(no_release_run test_failed_release_skipped
                   release_failed
                   publication_succeeded_smoke_failed
                   unknown))

(define (release-verdict-success? v)
  (equal? v 'workflow_success))

(define (release-verdict-blocking? v)
  (and v (member v release-blocking-verdicts) #t))

(define (release-verdict-superseded? v)
  (equal? v 'stale_run))

(define (release-verdict->approval-state v)
  (cond
    [(release-verdict-success? v) 'approved]
    [(release-verdict-superseded? v) 'superseded]
    [(release-verdict-blocking? v) 'blocked]
    [else 'unknown]))

(define ci-verdicts
  '(ci_no_runs ci_in_progress
               ci_failure
               ci_cancelled
               ci_required_job_missing
               ci_required_job_unexpectedly_skipped
               ci_required_job_non_success
               ci_success))

(define ci-blocking-verdicts
  '(ci_no_runs ci_in_progress
               ci_failure
               ci_cancelled
               ci_required_job_missing
               ci_required_job_unexpectedly_skipped
               ci_required_job_non_success))

(define (ci-verdict-success? v)
  (equal? v 'ci_success))

(define (ci-verdict-blocking? v)
  (and v (member v ci-blocking-verdicts) #t))

(define (ci-verdict->approval-state v)
  (cond
    [(ci-verdict-success? v) 'approved]
    [(ci-verdict-blocking? v) 'blocked]
    [else 'unknown]))

(define (can-close-milestone? release-verdict ci-verdict)
  (cond
    [(not (release-verdict-success? release-verdict))
     (milestone-lifecycle-transition-result 'ci_green
                                            'closed
                                            #f
                                            (format "release verdict ~a blocks closing"
                                                    release-verdict))]
    [(not (ci-verdict-success? ci-verdict))
     (milestone-lifecycle-transition-result 'ci_green
                                            'closed
                                            #f
                                            (format "CI verdict ~a blocks closing" ci-verdict))]
    [else (milestone-lifecycle-transition-result 'ci_green 'closed #t "all gates passed")]))

;; ---------------------------------------------------------------------------
;; Build a workflow verdict result hash for JSON output.
;; ---------------------------------------------------------------------------
(define (make-workflow-verdict-result run-id run-number conclusion jobs-hash assets-hash verdict pass)
  (hasheq 'workflow
          (hasheq 'run_id
                  (or run-id 0)
                  'run_number
                  (or run-number 0)
                  'conclusion
                  (or conclusion "unknown")
                  'pass
                  pass)
          'jobs
          (or jobs-hash (hasheq))
          'release_assets
          (or assets-hash (hasheq 'release_exists #f 'tarball #f 'manifest #f))
          'verdict
          (or verdict "unknown")))

;; ---------------------------------------------------------------------------
;; Resolve the conclusion of a job group. A reusable-workflow call expands into
;; several API jobs named "<group> / <step>" (e.g. "release-core / publish");
;; the group is success only when every member job is success. A plain single
;; job named exactly <group-name> is matched directly. When the group is absent,
;; fall back to <fallback-key> (legacy job names, e.g. "release").
;; ---------------------------------------------------------------------------
(define (group-status jobs group-name fallback-key)
  (define direct (hash-ref jobs group-name #f))
  (cond
    [direct direct]
    [else
     (define prefix (format "~a / " group-name))
     (define members
       (for/list ([(k v) (in-hash jobs)]
                  #:when (and (symbol? k) (string-prefix? (symbol->string k) prefix)))
         v))
     (cond
       [(null? members) (hash-ref jobs fallback-key #f)]
       [(andmap (lambda (v) (equal? v GITHUB-SUCCESS)) members) GITHUB-SUCCESS]
       [else "failure"])]))

;; ---------------------------------------------------------------------------
;; Classify a release workflow run into a verdict.
;;
;; Job-name resolution matches the current release.yml topology
;; (test -> prepare -> release-core). release-core is a reusable workflow call,
;; so its group status is derived via group-status. Legacy names (release,
;; smoke) are kept as fallbacks so older run records and tests still classify
;; correctly.
;; ---------------------------------------------------------------------------
(define (classify-release-verdict workflow-data jobs assets expected-tag)
  (cond
    [(not workflow-data) 'no_release_run]
    [(let ([branch (hash-ref workflow-data 'head_branch #f)])
       (and branch expected-tag (not (equal? branch expected-tag))))
     'stale_run]
    [else
     (define wf-conclusion (hash-ref workflow-data 'conclusion #f))
     (define test-status (hash-ref jobs 'test #f))
     (define prepare-status (group-status jobs 'prepare 'release))
     (define release-status (group-status jobs 'release-core 'release))
     (define smoke-status (hash-ref jobs 'smoke #f))
     (define release-exists (hash-ref assets 'release_exists #f))
     (cond
       [(and (equal? wf-conclusion GITHUB-SUCCESS))
        (if (and (equal? test-status GITHUB-SUCCESS)
                 (equal? prepare-status GITHUB-SUCCESS)
                 (equal? release-status GITHUB-SUCCESS)
                 (or (not smoke-status) (equal? smoke-status GITHUB-SUCCESS)))
            'workflow_success
            'unknown)]
       [(and (not (equal? test-status GITHUB-SUCCESS)) (not release-status))
        'test_failed_release_skipped]
       [(and release-status (not (equal? release-status GITHUB-SUCCESS))) 'release_failed]
       [(and (equal? test-status GITHUB-SUCCESS)
             (equal? release-status GITHUB-SUCCESS)
             smoke-status
             (not (equal? smoke-status GITHUB-SUCCESS))
             release-exists)
        'publication_succeeded_smoke_failed]
       [else 'unknown])]))

;; ---------------------------------------------------------------------------
;; Default required CI jobs for milestone closure.
;;
;; Source of truth: the `name:` fields in .github/workflows/ci.yml as returned
;; by the actions API (actions/runs/<id>/jobs -> jobs[].name). Matrix jobs use
;; GitHub's generated per-cell names (e.g. "test (0)", "workflows (1)").
;;
;; #9121: removed stale job names (lint-alignment, inter-wave-gate, macos smoke,
;; old matrix labels) and synced to the current sharded CI topology. The
;; fast-suite shards (test (0..2)), platform/cross-version tests, aggregate
;; jobs, gsd-governance, and abstraction-audit are all required.
;;
;; `release-readiness` is intentionally NOT required: it runs on pull_request
;; and is skipped on main pushes, so requiring it would create false
;; CI-green failures during milestone closure.
;; ---------------------------------------------------------------------------
(define ci-required-jobs
  '("lint" "test (0)"
           "test (1)"
           "test (2)"
           "test-aggregate"
           "test-platform"
           "test-cross-version"
           "security"
           "release-dry-run"
           "smoke (ubuntu-latest)"
           "workflows (0)"
           "workflows (1)"
           "workflows-aggregate"
           "gsd-governance"
           "abstraction-audit"))

;; ---------------------------------------------------------------------------
;; Build a CI check result hash for JSON output.
;; ---------------------------------------------------------------------------
(define (make-ci-check-result run-id run-number status conclusion verdict pass detail)
  (hasheq 'ci
          (hasheq 'run_id
                  (or run-id 0)
                  'run_number
                  (or run-number 0)
                  'status
                  (or status "unknown")
                  'conclusion
                  (or conclusion "unknown")
                  'verdict
                  (or verdict 'ci_no_runs)
                  'pass
                  pass
                  'detail
                  (or detail ""))))

;; ---------------------------------------------------------------------------
;; Classify a CI run for milestone closure purposes.
;; ---------------------------------------------------------------------------
(define (classify-ci-verdict ci-run-data jobs required-jobs)
  (cond
    [(not ci-run-data) 'ci_no_runs]
    [(not (equal? (hash-ref ci-run-data 'status #f) GITHUB-COMPLETED)) 'ci_in_progress]
    [(equal? (hash-ref ci-run-data 'conclusion #f) GITHUB-CANCELLED) 'ci_cancelled]
    [(not (equal? (hash-ref ci-run-data 'conclusion #f) GITHUB-SUCCESS)) 'ci_failure]
    [else
     (define missing-required
       (filter (lambda (job-name) (not (hash-has-key? jobs job-name))) required-jobs))
     (define skipped-required
       (for/list ([job-name (in-list required-jobs)]
                  #:when (equal? (hash-ref jobs job-name #f) GITHUB-SKIPPED))
         job-name))
     (define non-success-required
       (for/list ([job-name (in-list required-jobs)]
                  #:when (and (hash-has-key? jobs job-name)
                              (not (equal? (hash-ref jobs job-name #f) GITHUB-SUCCESS))
                              (not (equal? (hash-ref jobs job-name #f) GITHUB-SKIPPED))))
         job-name))
     (cond
       [(pair? missing-required) 'ci_required_job_missing]
       [(pair? skipped-required) 'ci_required_job_unexpectedly_skipped]
       [(pair? non-success-required) 'ci_required_job_non_success]
       [else 'ci_success])]))

;; ---------------------------------------------------------------------------
;; Provide all public bindings.
;; ---------------------------------------------------------------------------
;; W2 (#8564): GitHub status string constants
(provide GITHUB-SUCCESS
         GITHUB-FAILURE
         GITHUB-CANCELLED
         GITHUB-SKIPPED
         GITHUB-COMPLETED
         GITHUB-IN-PROGRESS

         ;; W2 (#8564): Verdict predicate layer
         release-verdict-success?
         release-verdict-blocking?
         release-verdict-superseded?
         release-verdict->approval-state
         ci-verdict-success?
         ci-verdict-blocking?
         ci-verdict->approval-state

         ;; Verdict classifiers
         classify-release-verdict
         make-workflow-verdict-result
         classify-ci-verdict
         make-ci-check-result
         ci-required-jobs)

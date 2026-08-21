#lang racket/base

;; @speed fast
;; @suite fast
;; @boundary integration
;; W8: contract tests for the scheduled/manual full-regression workflow
;; (`.github/workflows/full-regression.yml`). Follows the repo's established
;; workflow-contract test pattern (cf. test-ci-workflow-diagnostics.rkt):
;; explicit assertions against the workflow definition text so drift fails
;; a fast suite instead of being discovered by a nightly incident.

(require rackunit
         racket/file
         racket/string
         racket/path)

(define project-root
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               "..")))

(define wf-path (build-path project-root ".github" "workflows" "full-regression.yml"))
(define wf (file->string wf-path))

;; ---------------------------------------------------------------------------
;; Triggers: scheduled (nightly cron) + manual dispatch with suite/profile
;; override inputs (W8 Action 1).
;; ---------------------------------------------------------------------------

(define-test-suite
 trigger-contract
 (test-case "workflow has a scheduled (cron) trigger"
   (check-true (regexp-match? #px"(?m:^\\s*schedule:)" wf) "must define a schedule trigger"))
 (test-case "nightly cron is present"
   (check-true (regexp-match? #px"cron:\\s*\"" wf) "must contain a cron expression"))
 (test-case "workflow_dispatch trigger present"
   (check-true (regexp-match? #px"(?m:^\\s*workflow_dispatch:)" wf)
               "manual dispatch must be available"))
 (test-case "dispatch exposes suite + profile override inputs"
   (check-true (and (string-contains? wf "suite:") (string-contains? wf "profile:"))
               "dispatch inputs must allow suite/profile overrides"))
 (test-case "dispatch inputs are wired to the runner CLI flags"
   (check-true (and (string-contains? wf "--suite \"$SUITE\"")
                    (string-contains? wf "--profile \"$PROFILE\""))
               "inputs must flow into explicit --suite/--profile flags")))

;; ---------------------------------------------------------------------------
;; Execution: profile-aware runner with sharding; platform variants; the
;; `workflows` suite is executed in addition to the broad suite.
;; ---------------------------------------------------------------------------

(define-test-suite
 execution-contract
 (test-case "runs the profile-aware run-tests.rkt runner"
   (check-true (string-contains? wf "scripts/run-tests.rkt")
               "must invoke the existing profile-aware runner"))
 (test-case "sharding is explicit (shard-index/shard-total matrix)"
   (check-true (and (string-contains? wf "shard-index") (string-contains? wf "shard-total"))
               "per-shard execution must be explicit"))
 (test-case "workflows suite runs in addition to the broad suite"
   (check-true (string-contains? wf "workflows")
               "the workflows suite must be executed by this workflow"))
 (test-case "timeout-minutes is set on jobs (never the 360 default)"
   (check-true (regexp-match? #rx"timeout-minutes:" wf) "job timeouts must be explicit")
   (check-false (regexp-match? #px"timeout-minutes:\\s*360\\b" wf)
                "360 (the default) does not count as an explicit timeout")))

;; ---------------------------------------------------------------------------
;; Evidence: per-shard JSON artifacts (W0 schema) + run summary with
;; pass/fail/timeout/skip, wall clock, and profile.
;; ---------------------------------------------------------------------------

(define-test-suite evidence-contract
                   (test-case "uploads per-shard JSON report artifacts"
                     (check-true (and (string-contains? wf "actions/upload-artifact")
                                      (string-contains? wf "results-shard"))
                                 "shard reports must be uploaded as artifacts"))
                   (test-case "uploads a run summary artifact"
                     (check-true (string-contains? wf "run-summary")
                                 "a summary artifact must be uploaded"))
                   (test-case "all required lanes feed the repository status helper"
                     (check-true (and (string-contains? wf "full-regression-status.rkt")
                                      (string-contains? wf "results-workflows")
                                      (string-contains? wf "results-platform")
                                      (string-contains? wf "needs.workflows-suite.result")
                                      (string-contains? wf "needs.test-platform.result"))
                                 "Linux, workflows, and macOS evidence must all feed L4 status")))

;; ---------------------------------------------------------------------------
;; Timeout semantics: a timed-out shard fails the run with status `timeout`
;; — never `success`. (Enforced by the runner's exit-2 timeout contract and
;; by not masking non-zero exits in the workflow.)
;; ---------------------------------------------------------------------------

(define-test-suite
 timeout-semantics
 (test-case "workflow never converts runner non-zero exits into success"
   (check-false (regexp-match? #px"\\|\\|\\s*true" wf) "exit masking (`|| true`) is prohibited"))
 (test-case "timeout verdict is surfaced (runner exit 2 => status timeout)"
   (check-true (string-contains? wf "timeout") "the workflow must surface the timeout status")))

(module+ test
  (require rackunit/text-ui)
  (exit (run-tests (test-suite "tests/test-ci-workflows.rkt"
                     trigger-contract
                     execution-contract
                     evidence-contract
                     timeout-semantics))))

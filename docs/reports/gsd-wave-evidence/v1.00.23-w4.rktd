#lang reader "rktd-reader.rkt"

;; Wave evidence record for v1.00.23 W4
;; Reproducible 20-PR cohort evidence tooling

((wave . "W4")
 (milestone . "v1.00.23")
 (branch . "campaign/7537743a3dcedebc6ff1e40ed7b601712b6ee26329cedc4e9bae627b4f06f327/w4-delivery")
 (verify-command . "racket tests/test-ci-cohort-report.rkt && racket scripts/run-tests.rkt --suite fast")
 (verify-results
  ((test-ci-cohort-report (tests . 40) (pass . 40) (fail . 0) (result . PASS))
   (fast (files . 1163) (tests . 16848) (pass . 1163) (fail . 0) (result . PASS) (elapsed . "6m11s"))))
 (cohort-size . 20)
 (timing-sample-rule . "exactly one final successful timing-sample per SHA; failed/cancelled/rerun attempts retained as reliability evidence")
 (exclusion-reasons . ("missing-lane-artifact" "incompatible-scheduler" "incompatible-config" "inventory-mismatch" "artifact-corrupt" "artifact-expired" "non-unique-sha"))
 (rejected-cohorts . ("duplicate SHAs" "missing lane artifacts" "incompatible scheduler/config snapshots" "inventory mismatches" "silently truncated cohorts"))
 (percentile-estimator . "linear-interpolation")
 (reported-metrics . ("p50" "p95" "file/inventory digest" "pass/fail/timeout/skip/zero-test counts" "flakes" "parallel-only-failures" "prepared-env outcomes" "queue telemetry" "runner-minute cost"))
 (regeneration . "cohort-report.rkt --manifest <cohort.json> --out-json <stored> --check exits 0 on byte-identical reproduction")
 (external-dependencies . "none")
 (retention-contract . "artifacts/ci-baseline/README.md")
 (delivery-files
  ("q/scripts/run-tests/cohort-report.rkt"
   "q/scripts/run-tests/baseline-report.rkt"
   "q/tests/test-ci-cohort-report.rkt"
   "q/tests/fixtures/ci-cohort/"
   "q/artifacts/ci-baseline/README.md"
   "q/docs/TEST_CONVENTIONS.md"
   "q/docs/operations/test-regression-triage.md"
   "q/docs/reports/test-regression-log.md"
   "q/docs/reports/gsd-wave-evidence/v1.00.23-w4.rktd")))

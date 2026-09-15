#hasheq((branch . "binding/96974d2cd97b-w1")
        (content-digest . "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
        (delivery-head-sha . "b88e446c687dfb18dc44b695071cecfe26f7a3f7")
        (delivery-pr . 9703)
        (fast
         .
         #hasheq((command . "racket scripts/run-tests.rkt --suite fast (CI: ci.yml test shards 0/1/2 + test-aggregate on implementation head b88e446c, run 34999936883)")
                 (detail . "producer: clean fast-suite aggregate (files=1196 pass=1196 fail=0 timeout=0 skip=0)")
                 (result . "passed")))
        (focused-tests
         .
         #hasheq((command . "raco test tests/test-pr-latency-guard.rkt tests/test-ci-cohort-report.rkt")
                 (detail . "179 checks passed, 0 failures (36 + 143)")
                 (result . "passed")))
        (format-compile
         .
         #hasheq((command . "raco fmt -i scripts/ci/pr-latency-guard.rkt scripts/run-tests/cohort-report.rkt tests/test-pr-latency-guard.rkt tests/test-ci-cohort-report.rkt ; raco make scripts/ci/pr-latency-guard.rkt scripts/run-tests/cohort-report.rkt scripts/gsd-wave-gate.rkt")
                 (detail . "raco fmt produced no diff; bytecode compilation clean")
                 (result . "passed")))
        (implementation-sha . "694bc5420efd42b941887fa5990db963a20a2a94")
        (issue . 9687)
        (lint
         .
         #hasheq((command . "racket scripts/check-deps.rkt ; racket scripts/metrics.rkt --lint")
                 (detail . "all external packages declared in info.rkt; all 5 static metrics match README.md")
                 (result . "passed")))
        (merge-sha . "694bc5420efd42b941887fa5990db963a20a2a94")
        (merged-at . "2026-09-15T18:02:31Z")
        (milestone . 895)
        (plan-id
         .
         "96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8")
        (planning-sync . "current")
        (red-first
         .
         #hasheq((command
                  .
                  "racket scripts/ci/pr-latency-guard.rkt --input artifacts/ci-recovery/v1.00.30-w1/raw/canary-red-input.json ; racket scripts/ci/pr-latency-guard.rkt --input artifacts/ci-recovery/v1.00.30-w1/raw/canary-red-missing-job-input.json")
                 (failure
                  .
                  "reproduced on the delivered head: both red canaries print decision fail and exit 1 (stale/duplicate head and absent-required-job inputs are rejected); frozen red verdicts retained in artifacts/ci-recovery/v1.00.30-w1/guard-canaries.json (canary-red-input fail, canary-red-missing-job-input fail); p95-boundary canary is a boundary confirmation (equality passes) per the frozen attestation.")))
        (remaining-items
         .
         (#hasheq((classification . "deferred-noncritical")
                  (owner . "coordinator")
                  (rationale
                   .
                   "Post-bootstrap step (per scripts/required-pr-checks.policy comment and docs/reports/PR-LATENCY-GUARD-v1.00.30.md): enable pr-latency-guard as a required check in branch protection and add the policy entry together with a deliberate update of the frozen dag-checkpoint/CI-contract expectations. Until then the guard runs unenforced and the policy stays at the 13-check baseline."))))
        (required-pr-checks
         .
         ("lint"
          "lint-quality"
          "security"
          "release-dry-run"
          "workflows (0)"
          "workflows (1)"
          "workflows-aggregate"
          "smoke (ubuntu-latest)"
          "test (0)"
          "test (1)"
          "test (2)"
          "test-aggregate"
          "test-platform"))
        (review-artifact
         .
         "docs/reports/gsd-wave-reviews/96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8-w1.rktd")
        (status . "current")
        (wave . "W1")
        (wave-branch . "delivery/v1.00.30-w1-corrected"))
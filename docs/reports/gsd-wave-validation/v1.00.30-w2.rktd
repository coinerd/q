#hasheq((status . "current")
        (milestone . 895)
        (wave . "W2")
        (issue . 9688)
        (branch . "campaign/v1.00.30-w2")
        (implementation-sha . "3f279bc80dc3add9766d1aa335b1830d6d3ecf93")
        (content-digest . "bd2d1ccef3ccec63c43dd3a767f61743def33f3ac8c179f7f56f1f4ef08a8478")
        (red-first
         .
         #hasheq((command . "test-ci-eager-compilation.rkt full-path/purge red fixtures (pre-W2 purge-plus-skipped-compile amplification) + rollback-drill.json prohibited-fallback and future-mtime-purge steps")
                 (failure
                  .
                  "failing-for-the-right-reason: without the global RACKET_PREPARED_ARTIFACT=off opt-in and the single eager boundary, the restore path skips the package-visible compile and the purge-plus-skipped-compile amplification reproduces; per-file compile fallback is forbidden and asserted red")))
        (focused-tests
         .
         #hasheq((command . "raco test tests/test-ci-eager-compilation.rkt tests/test-ci-workflow-diagnostics.rkt tests/test-prepared-env-report.rkt")
                 (detail . "62 checks passed, 0 failures (3 + 38 + 21 focused)")
                 (result . "passed")))
        (format-compile
         .
         #hasheq((command . "raco fmt -i .github/actions/setup-racket/action.yml scripts/ci/prepared-env-report.rkt tests/test-ci-eager-compilation.rkt tests/test-ci-workflow-diagnostics.rkt tests/test-prepared-env-report.rkt tests/test-w9-ci-workflow-verification.rkt ; raco make <touched modules>")
                 (detail . "raco fmt produced no diff; bytecode compilation clean")
                 (result . "passed")))
        (lint
         .
         #hasheq((command . "racket scripts/metrics.rkt --lint ; racket scripts/check-deps.rkt")
                 (detail . "all 5 static metrics match README.md; all external packages declared in info.rkt")
                 (result . "passed")))
        (fast
         .
         #hasheq((command . "racket scripts/run-tests.rkt --suite fast")
                 (detail . "PASS at implementation head 3f279bc8: files=1200 pass=1200 fail=0 timeout=0 skip=0; tests=17849 pass=17849 fail=0; wall-clock-seconds=965.495")
                 (result . "passed")))
        (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.30-w2.rktd")
        (remaining-items
         .
         (#hasheq((item
                   .
                   "coordinator-owned: R0 truthfulness and per-wave guard; R1 cohort (>=3 comparable PR head SHAs, exact final head included; p95 of slowest fast-shard runner wall <= 1736.8 s) evaluated on the implementation PR head; genuine independent APPROVED review; protected squash merge; merge-SHA binding via the campaign binding protocol; enable required pr-latency-guard check in branch protection after bootstrap")
                  (classification . "coordinator-gated")
                  (owner . "coordinator")
                  (rationale
                   .
                   "Local Verify has no authority to waive R0/R1/R2, protection, sample size or release gates; retained remote CI evidence is required before advancing"))))
        (planning-sync . "current"))
#hasheq((status . "current")
        (milestone . 895)
        (wave . "W1")
        (issue . 9687)
        (branch . "campaign/v1.00.30-w1")
        (implementation-sha . "af68b0c3eac01a2040e3bc11c079b9e8f7cc01e5")
        (content-digest
         .
         "55734dd98e8ab36c94ed10208ef1f4ec926d69f92176f1f38e3f13f9bb7d65bf")
        (red-first
         .
         #hasheq((command
                  .
                  "racket scripts/ci/pr-latency-guard.rkt --input artifacts/ci-recovery/v1.00.30-w1/raw/canary-red-input.json ; racket scripts/ci/pr-latency-guard.rkt --input artifacts/ci-recovery/v1.00.30-w1/raw/canary-red-missing-job-input.json")
                 (failure
                  .
                  "both canaries print decision fail and exit 1: stale/wrong-final-head input and missing-required-job input are rejected (reproduced on resume after attempt-1 infra failure; frozen red verdicts retained in guard-canaries.json)")))
        (focused-tests
         .
         #hasheq((result . "passed")
                 (command
                  .
                  "raco test tests/test-pr-latency-guard.rkt tests/test-ci-cohort-report.rkt")
                 (detail . "179 checks passed, 0 failures (74 + 105)")))
        (format-compile
         .
         #hasheq((result . "passed")
                 (command
                  .
                  "raco fmt -i scripts/ci/pr-latency-guard.rkt scripts/run-tests/cohort-report.rkt tests/test-pr-latency-guard.rkt tests/test-ci-cohort-report.rkt ; raco make scripts/ci/pr-latency-guard.rkt scripts/run-tests/cohort-report.rkt scripts/gsd-wave-gate.rkt")
                 (detail . "raco fmt produced no diff; bytecode compilation clean")))
        (lint
         .
         #hasheq((result . "passed")
                 (command
                  .
                  "racket scripts/check-deps.rkt ; racket scripts/metrics.rkt --lint")
                 (detail
                  .
                  "2376 .rkt files scanned, all external deps declared; all 5 static metrics match README.md")))
        (fast
         .
         #hasheq((result . "passed")
                 (command . "racket scripts/run-tests.rkt --suite fast")
                 (detail
                  .
                  "RUN-SUMMARY runner-version=1.00.29 suite=fast profile=local shard=none execution-mode=subprocess file-count=1192 pass=1191 fail=0 timeout=0 skip=1 wall-clock-seconds=929.558 metadata-completeness=explicit:1165/heuristic:0/missing:27 ; VERDICT PASS")))
        (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.30-w1.rktd")
        (remaining-items
         .
         (#hasheq((item
                   .
                   "coordinator post-bootstrap protection: enable required pr-latency-guard check on the protected branch, read back branch rules, protect and squash-merge the wave PR, and bind evidence to the actual merge SHA via the campaign binding protocol")
                  (classification . "noncritical")
                  (owner . "coordinator")
                  (rationale
                   .
                   "Coordinator-owned acceptance gates outside executor authority; W1 bootstrap uses manual evaluation, not a self-approved missing-check exemption."))))
        (planning-sync . "current"))

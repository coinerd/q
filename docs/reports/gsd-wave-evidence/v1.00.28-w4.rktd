;; Wave evidence: v1.00.28 W4 — Production grouped-execution eligibility and parity evidence
;; Record-only companion to gsd-wave-reviews/v1.00.28-w4.rktd
;; and gsd-wave-validation/v1.00.28-w4.rktd.
(
(wave . "v1.00.28-w4")
(ticket . "#9629")
(implementation-sha . "ca94104cecdd7664f937746d40db7306b77234f5")
(delivery . "branch campaign/v1.00.28-w4 from fresh origin/main; squash-merge PR and evidence binding to the merge SHA are owned by the coordinator at merge time")
(base . "origin/main 04637d83 (v1.00.27 release-lane fixes merge)")
(scope . "scripts/run-tests/grouped-eligibility.rkt (classification reader + fail-closed decide-eligibility + parity-matrix predicates + report math), tests/test-grouped-eligibility.rkt (new contract suite), tests/test-runner-work-queue.rkt (extended: fail-closed default + rollback-switch cases), tests/metadata/classification (production cohort sidecar), 5 production test files tagged with isolation metadata, artifacts/test-runtime/v1.00.28-w4/{parity-matrix.json,group-safe-report.json,SHA256SUMS}, docs/reports/GROUPED-PRODUCTION-CHARACTERIZATION-v1.00.28.md, docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w4.rktd (this trio)")
(what-was-done
  (classification . "first production cohort of 14 fast-tier files classified in tests/metadata/classification using the wave's fail-closed criteria: 5 GROUP-SAFE (unit boundary, no declared mutation, parity complete), 9 ISOLATED (4 zero-parsed-output, 1 sandboxed-home declared mutation, 3 declared process/fs mutation in slow integration, 1 runner-gate structural form); the ISOLATED rows carry per-file reasons")
  (parity-matrix . "full characterization matrix for the 5 GROUP-SAFE candidates via runner.rkt primitives: isolated subprocess, grouped in-process, grouped-repeat, grouped-random (shuffled order), grouped-concurrent — 25/25 cells pass with identical parsed test counts, order-independent, repeated-stable, no leaks (cwd/env restored, sentinel probe clean); grouped wall clock 2-4x faster per file than subprocess")
  (report . "group-safe fast files 5/14 (35.7%); group-safe fast work mass 3,032/118,106 ms (2.57%) with measured isolated wall clock standing in for W0 census median_ms — the work-mass share is the decision number and it is small because the slow-integration ISOLATED families dominate cohort mass; both numbers are in checksummed artifacts/test-runtime/v1.00.28-w4/group-safe-report.json and narrated in docs/reports/GROUPED-PRODUCTION-CHARACTERIZATION-v1.00.28.md")
  (fail-closed . "decide-eligibility precedence rollback-switch → unclassified → classified-isolated → parity-missing → parity-failed → grouped; unclassified files, incomplete or failing parity evidence, or the --mode subprocess rollback switch force subprocess execution; default execution mode is unchanged (subprocess) — no activation in this wave")
  (prior-failure-fix . "attempt-1 verification failed on tests/test-milestone-gate.rkt (drift: the generated v1.00.27-w0 tier-ownership matrix did not include the wave's new tests/test-grouped-eligibility.rkt); fixed by regenerating the matrix and refreshing its checksums at ca94104c, keeping the wave content of 1c531ca6 intact"))
(focused-results
  (eligibility-suite . "racket tests/test-grouped-eligibility.rkt → exit 0, all checks pass")
  (work-queue-suite . "racket tests/test-runner-work-queue.rkt → exit 0, 11 checks incl. fail-closed default and rollback-switch")
  (milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (the attempt-1 failure is fixed)")
  (checksums . "sha256sum -c artifacts/test-runtime/v1.00.28-w4/SHA256SUMS → both artifacts OK")
  (matrix . "tmp driver over runner.rkt primitives regenerated parity-matrix.json (25 cells) + group-safe-report.json; self-check of every decide-eligibility expectation passed"))
(security-semantics . "no execute-* check, isolation root, worker-security contract, or scheduler variable modified; wave adds an eligibility decision layer on top of the existing runner modes and leaves the default mode subprocess (fail-closed)")
(result . "production cohort classified with a complete checksummed parity matrix, fail-closed defaults and rollback switch contract-tested, group-safe file (35.7%) and work-mass (2.57%) percentages reported, characterization report written; activation deliberately deferred — defaults stay isolated"))

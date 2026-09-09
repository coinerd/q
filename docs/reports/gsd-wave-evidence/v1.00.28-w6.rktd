;; Wave evidence: v1.00.28 W6 — TDD loop enforcement and developer ergonomics
;; Record-only companion to gsd-wave-reviews/v1.00.28-w6.rktd
;; and gsd-wave-validation/v1.00.28-w6.rktd.
(
(wave . "v1.00.28-w6")
(ticket . "#9594")
(implementation-sha . "2edc3f8e1ea05c7d673cd67c5e9e51d04fce72c4")
(delivery . "branch campaign/v1.00.28-w6 from fresh origin/main; squash-merge PR is owned by the coordinator; this trio is bound to the merge SHA at merge time")
(base . "origin/main 04637d835d33f887a29846a2f3c1855146151985; the branch carries W1-W5 campaign content already checkpointed on this branch's history")
(scope . "docs/TDD-TEST-STRATEGY-PLAN.md (L0/L1 measured-status section replaces 'scoped unknown'; decision ledger rows measured), docs/testing.md (new developer quick-start, L0-L3 one-command table), docs/reports/LOCAL-TDD-LATENCY-v1.00.28.md (new Class E report), artifacts/test-runtime/v1.00.28-w6/{l0,l1}-samples.json + SHA256SUMS (new, retained local samples), README.md (metrics re-sync), docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w6.rktd (this trio); tests/test-run-tests-script.rkt acceptance coverage for the documented command forms landed earlier on this branch")
(what-was-done
  (tdd-first . "the 'do not invent CLI flags' rule is mechanical: every command form documented in the strategy doc must execute against the real CLI or the acceptance test fails; the documented forms (positional file, --changed-base/--changed-head, --impact-dry-run with/without --explain, --suite fast) are covered by tests/test-run-tests-script.rkt (12/12 green)")
  (existing-flags-only . "no new runner flag shipped: L0 = positional single-file; L1 = --changed-base origin/main --changed-head HEAD; L2 = same flags plus --impact-dry-run --explain as a selection-only preview (transitive scope is reached with implemented flags: L1 diff selection union impact-closure closure over the change set); L3 = --suite fast; no alias flag was needed")
  (measured-samples . "retained local samples collected on the W6 branch head: 12 L0 positional runs (p50 1.375 s, p90 2.041 s, max 4.170 s), 10 L1 --changed-base/--changed-head runs (p50 48.276 s, p90 52.711 s, max 57.182 s), 10 L2 selection-only --impact-dry-run --explain walks (p50 7.957 s, p90 7.986 s, max 8.046 s); every sample records elapsed, selected set, and selection/fallback/escalation reason; SHA-256 checksummed")
  (class-e-verdicts . "L0 CONFIRMED (p90 2.041 s <= 5 s, 59% headroom); L1 MISS recorded with attribution, no target edit (selection overhead is ~2 s via --impact-dry-run; the p90 cost is the runtime of the selected file tests/test-run-tests-script.rkt itself); L2 CONFIRMED for the selection-only walk (7.986 s <= 120 s, 93% headroom; full transitive execution remains an L3/broad-window activity by escalation policy)")
  (strategy-doc . "the 'Status of the time budgets' paragraph and the L0/L1/L2 target-decisions rows now cite the measured sample with per-row numbers and caveats (n = 10-12, single host, wall clock includes racket startup, nearest-rank p90); the deferred-block L0-L2 confirmation item is marked done with the report link; no target was revised")
  (workflow-ergonomics . "docs/testing.md quick-start makes the narrow-first loop the default: L0 positional (~2 s) for red-green, L1 impact before push, L2 as a selection preview that escalates execution to L3, broad --suite fast explicitly discouraged during normal iterations")
  (metrics-sync . "adding the wave files changed the test-line/assertion totals, so scripts/metrics.rkt --sync-all was run in the same PR per wave rule 7 (expected 264387/264345-line and 40315/40311-assertion mismatch resolved), and the pre-commit hook now passes"))
(focused-results
  (acceptance . "racket tests/test-run-tests-script.rkt -> 12/12 green, exit 0")
  (checksums . "sha256sum -c artifacts/test-runtime/v1.00.28-w6/SHA256SUMS -> both sample JSONs OK")
  (metrics . "racket scripts/metrics.rkt --sync-all -> README metrics table synced; metrics lint path exercised by the pre-commit hook at commit time")
  (verify-lane . "the declared Verify command is coordinator-owned and runs at return; its components were exercised individually on this branch (acceptance suite green, checksums OK, L0 grep hits, branch != main)"))
(security-semantics . "untouched: no execute-* check, isolation root, worker-security contract, gate semantics, or scheduler variable was modified; the wave adds documentation, measurement artifacts, and test-side acceptance coverage only")
(result . "documented L0-L3 commands all execute against the real runner, retained L0/L1 samples report p50/p90/max with selection and fallback reasons, the strategy doc's L0/L1 sections are measured with an attributed L1 miss and no target edit, the latency report is checksummed and trio-bound, and every declared wave file exists at HEAD"))

;; Wave review: v1.00.28 W6 — TDD loop enforcement and developer ergonomics
;; Record-only companion to gsd-wave-evidence/v1.00.28-w6.rktd
;; and gsd-wave-validation/v1.00.28-w6.rktd.
(
(wave . "v1.00.28-w6")
(reviewed-sha . "2edc3f8e1ea05c7d673cd67c5e9e51d04fce72c4")
(scope . "docs/TDD-TEST-STRATEGY-PLAN.md, docs/testing.md, docs/reports/LOCAL-TDD-LATENCY-v1.00.28.md, artifacts/test-runtime/v1.00.28-w6/{l0,l1}-samples.json + SHA256SUMS, README.md metrics re-sync, docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w6.rktd, plus the test-side acceptance coverage in tests/test-run-tests-script.rkt landed earlier on this branch")
(findings
  (no-invented-flags . "every command string in docs/testing.md and the strategy doc resolves to an implemented CLI surface: positional test file, --changed-base/--changed-head, --impact-dry-run, --explain, --suite fast; the review cross-checked each documented form against tests/test-run-tests-script.rkt acceptance coverage (12/12 green) — a documented flag the runner rejects would be a red test, and none is")
  (l2-honesty . "the L2 row is labeled selection-only in every place it appears (report, strategy ledger, quick-start): the ~8 s p90 measures the --impact-dry-run --explain graph walk, and transitive execution is explicitly escalated to L3/broad windows by policy rather than claimed as a measured <= 120 s execution; this matches the wave's 'name the gap honestly' rule")
  (l1-miss-language . "the L1 p90 52.711 s > 30 s miss is recorded with attribution and no target edit: the same selection completes in ~2 s via --impact-dry-run, so the cost is the selected file's own runtime (tests/test-run-tests-script.rkt), not selection overhead; the miss is stated per-row with numbers in the report, the strategy status paragraph, and the decision ledger")
  (sample-retention . "both sample files retain >= 10 runs per level (12 L0, 10 L1, with the 10 L2 selection walks in the l1 cohort) and each run records elapsed, selected set, and selection/fallback/escalation reason; SHA256SUMS pins both JSONs and verifies at HEAD")
  (measured-not-unknown . "grep confirms the strategy doc no longer describes L0/L1 as unmeasured scoped unknowns: the old 'scoped unknown — not yet measured' rows and the 'unconfirmed aspirations' paragraph are replaced with measured per-row entries; the only remaining 'scoped unknown' mention is the unrelated grouped-execution row, which stays out of this wave's scope")
  (ergonomics . "docs/testing.md recommends the L0-positional red-green loop and explicitly rules out broad --suite fast during normal iterations; the quick-start's empty-selection guidance documents the real exit-3 safety rail instead of inventing behavior")
  (metrics-sync . "README's test-lines/assertion metrics were re-synced after the wave's test-side additions (264345 -> 264387 lines, 40311 -> 40315 assertions observed by the pre-commit hook), satisfying wave rule 7 in the same PR"))
(concerns . "the L1 miss means the <= 30 s direct-impact budget is not met for the current selected-file composition; the honest attribution points at the selected file's runtime, so remediation belongs to the thin-file test lane, not to selection work; n = 10-12 on a single host is a first sample — treat the percentiles as a baseline of record with stated caveats, not as stable SLO evidence")
(verdict . "approved-for-merge"))

;; Wave validation: v1.00.27 W5 — Final C3 cohort and independent verification
;; Record-only companion to gsd-wave-evidence/v1.00.27-w5.rktd.
(
(wave . "v1.00.27-w5")
(validated-sha . "d5f9afbab1de65558a91e92dbab3d19c83cda5d1")
(checks
  (test-ci-cohort-report . "racket tests/test-ci-cohort-report.rkt → 74+9+20+24 success(es) 0 failure(s) 0 error(s), exit 0 (final-claim rows, guard-id duality, unverified-never-pass, exact-quantile interpolation)")
  (regeneration-check . "racket scripts/run-tests/cohort-report.rkt --manifest artifacts/ci-baseline/v1.00.27-c3/cohort.json --out-json artifacts/ci-baseline/v1.00.27-c3/report.json --out-md artifacts/ci-baseline/v1.00.27-c3/report.md --check → CHECK PASS: match, exit 0 (byte-stable regeneration of the committed report)")
  (checksums . "sha256sum -c artifacts/ci-baseline/v1.00.27-c3/SHA256SUMS → cohort.json: OK, report.json: OK, report.md: OK, decision.md: OK, independent-verification.md: OK")
  (branch . "campaign/v1.00.27-w5 checked out; not main")
  (artifact-tally . "cohort.json: 20 unique head SHAs, 21 attempts (21/0/0/0 successes/failures/reruns/cancels), 12 disjoint lane-run-failed exclusions, raw per-attempt timing samples, 24 prepared-env records (24 verified, 0 fallback); report.json/report.md: final-claim gate with all seven §8 rows tool-verdicted; decision.md: 'target not achieved' with observed numbers, no-rollback disposition, named levers; independent-verification.md: second-channel re-derivation, per-row agree on all seven rows, signed; all five digests pinned in SHA256SUMS")
  (verdict-recompute . "independent recomputation from the raw per-SHA samples confirms the stored observed values and verdicts: six rows 'target not achieved' (fast-p50 267.5 s, fast-p95 285.95 s, pr-ci-p50 1043.5 s, pr-ci-p95 1175.65 s, security-runner-p50 685.5 s, workflows-runner-p50 695.5 s — each above its fixed target) and prepared-env-verified-restores 'pass' (100.0% ≥ 95.0%); no threshold revised; no row unverified (all guards provided)")
  (no-security-delta . "no execute-* check, isolation root, worker-security contract, or gate semantics modified; the cohort is read-only measurement of the existing integrated topology"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; this attempt resumed the committed branch work (tooling, cohort ingestion, and report checkpoints already present), verified suite and --check green after a stale-bytecode recompile, wrote and verified SHA256SUMS, committed decision.md and independent-verification.md, and wrote the evidence/review/validation trio so every declared wave file exists at HEAD on the delivery branch")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

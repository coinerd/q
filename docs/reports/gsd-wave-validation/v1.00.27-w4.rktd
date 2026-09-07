;; Wave validation: v1.00.27 W4 — L0/L1/L2 local feedback telemetry with p90 evidence
;; Record-only companion to gsd-wave-evidence/v1.00.27-w4.rktd.
(
(wave . "v1.00.27-w4")
(validated-sha . "633ed9fde5fed1cd1dbd6ac0acbf17cdd876a266")
(checks
  (test-run-tests-overhead-diagnostics . "racket tests/test-run-tests-overhead-diagnostics.rkt → 15 success(es) 0 failure(s) 0 error(s) (10 prior diagnostics + 5 new local-telemetry governance checks)")
  (check-local-p90 . "racket scripts/run-tests/overhead.rkt --check artifacts/tier-ownership/v1.00.27-w4/local-p90.json → local-p90 OK (loops L0/L1/L2 verified, verdicts recomputed), exit 0; required fields, sample counts ≥ 20, all exit codes 0, recomputed p90 L0 1883 ms / L1 4428 ms / L2 234910 ms, recomputed verdicts meet/meet/adjust-per-record")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w4/SHA256SUMS → local-p90.json: OK, slo-evidence-record.md: OK")
  (branch . "campaign/v1.00.27-w4 checked out; not main")
  (artifact-tally . "local-p90.json: 3 loops × 20 samples, linear-interpolated p90 with machine context and per-area grouped/subprocess mode; slo-evidence-record.md: governed L2 ≤240 s adjustment (sample, method, reason, owner, review) with §8 L0/L1 targets untouched; both digests pinned in SHA256SUMS")
  (derived-number-recompute . "independent recomputation from the raw per-sample values confirms the stored p90s and medians (L0 1883 ms, L1 4428 ms, L2 234910 ms; L2 median 230401 ms) and that the SLO verdicts in the record are validator-computed, not hand-written")
  (no-security-delta . "no execute-* check, isolation root, worker-security contract, gate semantics, or grouped/subprocess decision path modified; telemetry subprocesses inherit the existing runner security model unchanged"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; this attempt resumed the committed branch work, refreshed the L2 loop measurement with a final all-pass sample set (a failed-exit warm sample replaced by a fresh identical-protocol run), repaired derived values (L2 p90 234910 ms, median 230401 ms) and the prose/table that cited stale numbers, regenerated SHA256SUMS, and committed the previously missing declared targets (local-p90.json, SHA256SUMS, slo-evidence-record.md, evidence/reviews/validation trio) so every declared wave file exists at HEAD on the delivery branch")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

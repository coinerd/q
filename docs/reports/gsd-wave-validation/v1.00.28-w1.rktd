;; Wave validation: v1.00.28 W1 — Real-time wait elimination across unit-fast and fast
;; Record-only companion to gsd-wave-evidence/v1.00.28-w1.rktd.
(
(wave . "v1.00.28-w1")
(validated-sha . "8b6f3b20ba4bc8b72bef8905032f987f80055ca1")
(checks
  (branch . "campaign/v1.00.28-w1 checked out (not main); base origin/main; prior-attempt implementation checkpoints present, this attempt added the missing artifact set (benchmarks/ manifests, SHA256SUMS) and the evidence/reviews/validation records")
  (checksums . "cd <project-base>/q && sha256sum -c artifacts/test-runtime/v1.00.28-w1/SHA256SUMS → all 8 pinned artifacts OK (wait-audit.json, six benchmark manifests, TEST-WAIT-AUDIT report) at the validated SHA")
  (focused-suites . "racket tests/test-deterministic-clock.rkt → 10/0/0 (includes the wait-audit red/guard lint cases); racket tests/test-auto-retry.rkt → exit 0; racket tests/test-retry-iteration.rkt → exit 0; racket tests/test-agent-session-basic.rkt → 19/0/0")
  (lints . "racket scripts/metrics.rkt --lint → all 5 static metrics match README.md; the wait-audit red/guard lives in the deterministic-clock suite so it re-runs on every future suite invocation")
  (declared-files . "all twelve declared wave files exist at HEAD: tests/helpers/deterministic-clock.rkt, tests/test-deterministic-clock.rkt, tests/test-agent-session-basic.rkt, tests/test-auto-retry.rkt, tests/test-retry-iteration.rkt, artifacts/test-runtime/v1.00.28-w1/wait-audit.json, artifacts/test-runtime/v1.00.28-w1/benchmarks/, artifacts/test-runtime/v1.00.28-w1/SHA256SUMS, docs/reports/TEST-WAIT-AUDIT-v1.00.28.md, and the gsd-wave-{evidence,reviews,validation}/v1.00.28-w1.rktd trio")
  (done-criteria . "zero unjustified real sleeps in unit-fast (audit summary unjustified-unit-fast-sleeps = 0, enforced by the lint case); every retained real-clock test has a named behavioral reason in its audit row; both remediated families have >=10-sample before/after manifests and the purged-residue wave3 family is named INCOMPARABLE without a claimed speedup; the wait-audit report and its evidence are checksummed and bound to the delivery branch"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; this attempt resumed from the committed attempt-branch checkpoints instead of restarting, completed the missing artifact trio (benchmark manifests, SHA256SUMS, reviews/validation records), bound the evidence record to the implementation SHA and the real checksums digest, and re-verified the focused suites at the branch head")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

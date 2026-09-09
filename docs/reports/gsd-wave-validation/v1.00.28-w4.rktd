;; Wave validation: v1.00.28 W4 — Production grouped-execution eligibility and parity evidence
;; Record-only companion to gsd-wave-evidence/v1.00.28-w4.rktd.
(
(wave . "v1.00.28-w4")
(validated-sha . "d6b6dde6b88dd9db51b292c87806b290e60395de")
(checks
  (test-grouped-eligibility . "racket tests/test-grouped-eligibility.rkt → exit 0, 11 success(es) 0 failure(s) 0 error(s): classification parser failure modes, parity-cell passing/completeness separation, fail-closed decisions (unclassified-file, classified-isolated, parity-missing, parity-failed), rollback-switch revocation, file-% vs mass-% report math")
  (test-runner-work-queue . "racket tests/test-runner-work-queue.rkt → exit 0, 11 checks incl. the W4 fail-closed default and rollback-switch (--mode subprocess / config) restoration of isolated execution")
  (test-milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (68 prior checks + drift gate); the attempt-1 failure was matrix drift — tests/test-grouped-eligibility.rkt missing from the generated v1.00.27-w0 tier-ownership matrix — fixed at ca94104c by regenerating the matrix and refreshing its checksums")
  (checksums . "sha256sum -c artifacts/test-runtime/v1.00.28-w4/SHA256SUMS → parity-matrix.json: OK, group-safe-report.json: OK")
  (branch . "campaign/v1.00.28-w4 checked out; not main")
  (artifact-tally . "parity-matrix.json: 25 cells = 5 GROUP-SAFE files × 5 modes, all passing with uniform parsed counts, order-independent, repeated-stable, no leaks; group-safe-report.json: group_safe_files 5/14 (35.71%), group_safe_mass_ms 3032/118106 (2.57%) with the mass stand-in note; tests/metadata/classification: 14 rows, 5 GROUP-SAFE + 9 ISOLATED each with per-file reason")
  (fail-closed-precedence . "every non-grouped outcome of decide-eligibility returns the specific reason token and mode subprocess; complete passing evidence for a GROUP-SAFE file is the ONLY path to grouped, and the rollback switch overrides even that — verified by contract tests at validated-sha")
  (no-activation . "default execution mode remains subprocess; no production file is activated for grouped execution in this wave, so the fast suite's execution plane is byte-for-byte unchanged"))
(prior-failure-addressed . "attempt-1 delivery verification failed solely on tests/test-milestone-gate.rkt (generated tier-ownership matrix lacked the wave's new test file); fixed by the ca94104c checkpoint regenerating artifacts/tier-ownership/v1.00.27-w0/ownership-matrix.json + SHA256SUMS, with the wave content of 1c531ca6 intact; attempt-2 ended in a provider/network infrastructure failure — this attempt resumed the branch, discarded unrelated in-flight wrapper edits that would have invalidated the checksummed parity evidence, committed the remaining declared wave files (classification sidecar, checksummed artifacts, characterization report, evidence trio) and re-ran the focused gates green")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

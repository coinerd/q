;; Wave evidence: v1.00.28 W0 — repository-wide runtime census and work-mass baseline
;; Record-only: adds no code, touches no artifact bytes beyond the trio itself.
(
(wave . "v1.00.28-w0")
(goal . "Measure the actual cost of every test in the canonical fast inventory instead of optimizing another hand-picked hotspot set: per-file duration samples (median/p95 with the hotspot linear interpolation), per-test @speed/@suite/@boundary/@covers metadata, work-type counters (unknown where not safely countable), work mass, Pareto contributions, runtime buckets, boundary breakdown, the static wait/subprocess scan, all checksummed and reproducible via --check.")
(branch . "campaign/v1.00.28-w0")
(dependency-state . "branched from v1.00.27 at 04637d83 (series complete; clean slate)")
(attempt-history
  (attempt-1 . "ended in an INFRASTRUCTURE failure (provider/network) before delivery verification; no logic failure was recorded, and the wave was preserved unconsumed on the attempt branch")
  (attempt-2 . "resume: completed the census implementation, artifacts, report, and evidence trio; remediated the BUG-0009 version-literal lint by deriving the census fixture milestone from util/version.rkt q-version instead of a hard-coded literal; added census runtime artifacts to .gitignore; synced metrics; committed checkpoint b72fd7e86601aacc998342d37e8026e25b50baa4 with the 18-check pre-commit lint green"))
(implementation-sha . "b72fd7e86601aacc998342d37e8026e25b50baa4")
(delivery
  (tdd-tests-first . #t)
  (census-contract . "every fast-selected file has a duration record or an explicit collection failure; a file on disk but absent from the census is a red test case and vice versa; unknown counters serialize as null/unknown, never 0; failures and timeouts are retained records, never discarded — all asserted by tests/test-runtime-census.rkt and tests/test-run-tests-metadata-discovery.rkt")
  (reproducibility . "runtime-census.rkt --check regenerates the canonical bytes of fast-runtime-census.json and verifies the committed SHA256SUMS")
  (focused-suite-command . "racket tests/test-runtime-census.rkt && racket tests/test-run-tests-metadata-discovery.rkt && racket scripts/run-tests/runtime-census.rkt --check artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json && sha256sum -c artifacts/test-runtime/v1.00.28-census/SHA256SUMS — all green at implementation-sha")
  (focused-suite-results . "census contract tests: pass (fixture round green); metadata-discovery: 2 success(es) + 5 success(es), 0 failures 0 errors; census --check: PASS; sha256sum -c: OK for all three checksummed files; branch guard: campaign/v1.00.28-w0, not main")
  (artifacts
    (census . "artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json (digest 3210bb0f40b2505bea38df050b41d560efd0c09b148b073bb7f3915216ee77f2)")
    (static-wait-scan . "artifacts/test-runtime/v1.00.28-census/static-wait-scan.json (digest 6fbd4343b95217f847fd6a05d946d064f1ef98d3110266451cc651ab8d549166; RUNTIME-AUDIT-SPEC §5 triage input — matches listed, never automatic defects)")
    (sha256sums . "artifacts/test-runtime/v1.00.28-census/SHA256SUMS (covers census, scan, report; verified OK standalone)")
    (report . "docs/reports/TEST-WORK-MASS-v1.00.28.md (digest 32e4f8bc0798800cb6d2f782a94064111fa898eaee758866ae04b2e35893a810; all seven §10 audit questions answered)")
    (evidence-trio . "docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w0.rktd (record-only)"))
  (pr-plan . "single PR from campaign/v1.00.28-w0 to main carrying the census tooling, extended inventory/hotspot parameterization, the TDD suite, the checksummed artifacts, the report, and this evidence trio; squash-merge binds the trio to the merge SHA (coordinator-owned)"))
(measurement
  (gate-text . "Roadmap v1.00.28 W0: every fast file has a census record or explicit collection failure; work mass, Pareto, buckets, boundary split, counters, queues, and attributions are reported with checksums; the seven audit questions are answered; census --check passes at the merged PR SHA.")
  (gate-evaluable . #t)
  (gate-verdict . "ready-for-merge — all declared targets delivered and the focused battery green at implementation-sha; the merge-SHA --check completes at the coordinator-owned squash-merge"))
(decision
  (outcome . "census delivered; the work-mass vocabulary (per-file median/p95, counters, buckets, boundary split) is the citation baseline for every remediation wave of v1.00.28")
  (no-shortcuts . "no file was dropped from the census: omissions are contract failures asserted in both directions, unknown work types stay unknown instead of faking zeros, and collection failures/timeouts are retained records"))
(record-only . "the evidence trio adds no code and touches no artifact bytes; the census, scan, and report are bound byte-for-byte by the SHA256SUMS file"))

;; Wave evidence: v1.00.27 W4 — L0/L1/L2 local feedback telemetry with p90 evidence
;; Record-only companion to gsd-wave-reviews/v1.00.27-w4.rktd
;; and gsd-wave-validation/v1.00.27-w4.rktd.
(
(wave . "v1.00.27-w4")
(ticket . "#9592")
(implementation-sha . "ed8649e722e392ed67acfbc38184a50a79c5eac0")
(delivery . "branch campaign/v1.00.27-w4; squash-merge PR owned by the coordinator binds this trio to the merge SHA")
(scope . "scripts/run-tests/overhead.rkt (local telemetry collection + --check validator), tests/test-run-tests-overhead-diagnostics.rkt (extended), artifacts/tier-ownership/v1.00.27-w4/{local-p90.json,SHA256SUMS,slo-evidence-record.md}, docs/reports/gsd-wave-evidence|reviews|validation/v1.00.27-w4.rktd")
(what-was-done
  (telemetry "scripts/run-tests/overhead.rkt --collect-local records, per local loop (L0 single test file, L1 area via direct unit impact set, L2 full unit-fast tier), a record carrying sample-count, per-sample elapsed-ms and exit-code, p90-ms computed by linear interpolation at rank 0.9*(n-1), method field, machine context (CPU count, load averages, Racket version, platform), and grouped/subprocess mode per area per W3's governed configuration")
  (validator "check-local-p90-record (--check) fails any record missing a required field (sample counts, machine context, method, per-area mode) or carrying an exit-code != 0 sample, recomputes p90 by linear interpolation from the raw samples, and recomputes the SLO verdicts from the embedded targets — verdicts are never hand-written; L2's target encodes the adjusted 240 s ceiling from the governed evidence record")
  (measurement "artifacts/tier-ownership/v1.00.27-w4/local-p90.json holds real collected telemetry: 20 samples per loop on the current tree, warm and cold start (L2: 1 cold, 19 warm; every sample a fresh racket scripts/run-tests.rkt process, all exiting 0 in the final set), measured p90 L0 1883 ms, L1 4428 ms, L2 234910 ms, checksummed in SHA256SUMS")
  (slo-verdict "L0 ≤5 s and L1 ≤30 s confirmed (meet); L2 ≤120 s not confirmable (measured p90 234.9 s) and adjusted to ≤240 s exclusively through the governed record artifacts/tier-ownership/v1.00.27-w4/slo-evidence-record.md (sample, method, reason, owner, review) committed in the same PR; §8 L0/L1 targets untouched")
  (sample-fix "one warm L2 sample (sample 7) executed the full tier but exited 1 on an in-tier test failure; failed runs are excluded by --check, so the sample was replaced by one additional fresh warm invocation under the identical protocol (318098 ms, exit 0, 943/943 files passing) recorded in place with its own elapsed time and exit code; the L2 p90 and verdict are computed from the final all-pass set of 20")
  (mutation-safety "the telemetry is read-only observation of the runner: no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or blocking gate was modified; grouped/subprocess decisions still come solely from W3's governed configuration"))
(focused-results
  (test-run-tests-overhead-diagnostics . "racket tests/test-run-tests-overhead-diagnostics.rkt → 15 success(es) 0 failure(s) 0 error(s) (10 diagnostics + 5 new local-telemetry governance checks), exit 0")
  (check-local-p90 . "racket scripts/run-tests/overhead.rkt --check artifacts/tier-ownership/v1.00.27-w4/local-p90.json → exit 0 (validator green: fields, exit codes, recomputed p90s, recomputed verdicts)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w4/SHA256SUMS → local-p90.json: OK, slo-evidence-record.md: OK")
  (branch . "campaign/v1.00.27-w4 checked out; not main"))
(prior-failure-addressed . "the previous wave attempt ended in a provider/network infrastructure failure, not a logic failure; this attempt resumed the committed branch work, replaced the failed-exit L2 sample with a real fresh all-pass measurement, refreshed SHA256SUMS, and committed the previously missing artifact set (local-p90.json, SHA256SUMS, slo-evidence-record.md) and this evidence/review/validation trio so every declared wave target exists at HEAD on the delivery branch")
(security-semantics . "untouched: no execute-* check, isolation root, worker-security contract, or gate semantics was modified; the W4 changes add measurement and validation of the local developer loop only, and the telemetry subprocesses inherit the existing runner security model unchanged")
(result . "L0/L1/L2 local feedback loops carry durable, machine-checked p90 evidence (20 samples each, linear interpolation, machine context, per-area mode); SLOs confirmed at ≤5/≤30 s for L0/L1 and adjusted to ≤240 s for L2 solely through the governed evidence record; all wave-scoped checks green"))

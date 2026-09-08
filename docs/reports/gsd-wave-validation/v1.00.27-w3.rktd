;; Wave validation: v1.00.27 W3 — area-by-area grouped unit-fast expansion with per-area rollback
;; Record-only companion to gsd-wave-evidence/v1.00.27-w3.rktd.
(
(wave . "v1.00.27-w3")
(validated-sha . "57b4720f73607b64459dc035e8bb41dc26fa75f6")
(checks
  (test-run-tests-in-process-mode . "racket tests/test-run-tests-in-process-mode.rkt → 4 success(es) 0 failure(s) 0 error(s) (explicit eligibility, evidence-gated enablement, per-area rollback switch, named fallback reporting)")
  (test-run-tests-profiles . "racket tests/test-run-tests-profiles.rkt → 9 success(es) 0 failure(s) 0 error(s) + 1 success(es) 0 failure(s) 0 error(s) (grouped-expansion artifact governance suite included; JSON emission regression covered)")
  (test-runner-work-queue . "racket tests/test-runner-work-queue.rkt → 11 success(es) 0 failure(s) 0 error(s)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w3/SHA256SUMS → grouped-expansion.json: OK, ci-subprocess.json: OK, ci-grouped.json: OK")
  (branch . "campaign/v1.00.27-w3 checked out; not main")
  (artifact-tally . "grouped-expansion.json: 1 area migrated (ci, exact-match), 0 mismatches, every grouped-enabled area backed by a paired comparison with digests registered in SHA256SUMS; remaining unit-fast areas listed as not-migrated")
  (no-security-delta . "no execute-* check, isolation root, worker-security contract, or gate semantics modified; parallel-writer families remain serial per the W0 matrix"))
(prior-failure-addressed . "attempt-1 failed delivery verification with write-json rejecting a string hash key (\"ci\") emitted by the profiles → JSON grouped-policy export; the exporter now produces legal jsexpr objects (symbol keys, stringified policy values) via grouped-area-config->jsexpr, the profiles suite passes its 9+1 checks, and the declared evidence trio (gsd-wave-evidence, gsd-wave-reviews, gsd-wave-validation records for v1.00.27-w3) exists as committed report files so every declared wave target is present at HEAD")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

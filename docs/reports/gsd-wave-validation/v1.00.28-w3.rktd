;; Wave validation: v1.00.28 W3 — fast-tier architecture review, split business logic from boundary contracts
;; Record-only companion to gsd-wave-evidence/v1.00.28-w3.rktd.
(
(wave . "v1.00.28-w3")
(validated-sha . "8de5440ed3ff754e89da032f3e78c0f8edf77647")
(checks
  (test-cwd-independence . "racket tests/test-cwd-independence.rkt → exit 0 (deterministic unit side plus one real executable/CWD probe canary under the required fast gate)")
  (test-agent-session-basic . "racket tests/test-agent-session-basic.rkt → 19 success(es) 0 failure(s) 0 error(s), exit 0 (in-memory storage for lifecycle behavior; real filesystem persistence retained)")
  (test-golden-flows . "racket tests/test-golden-flows.rkt → exit 0 (deterministic golden behavior; one small real golden path retained)")
  (test-gsd-delivery-verifier . "racket tests/test-gsd-delivery-verifier.rkt → 24 success(es) 0 failure(s) 0 error(s), exit 0 (synthetic Git facts via injectable adapter; minimum real-Git contract set retained)")
  (test-milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (inventory-reconciliation red cases: moved-without-destination and moved-without-named-gate fail closed)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.28-w3/SHA256SUMS → fast-integration-review.json: OK, inventory-reconciliation.json: OK")
  (branch . "campaign/v1.00.28-w3 checked out; not main")
  (artifact-tally . "fast-integration-review.json: 254/254 rows classified with owner and required gates; inventory-reconciliation.json: moves 0, drops 0, in-place reviews 4, before == after per behavior ID; tier-ownership-matrix.json: 972 rows, regenerated, drift check green")
  (no-security-delta . "no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or gate semantics modified; all real-boundary contracts remain bound to their existing required gates"))
(prior-failure-addressed . "attempt-1 ended on an infrastructure failure (provider/network), not a delivery-verification logic failure; the preserved attempt branch was resumed, the remaining declared targets (SHA256SUMS, review report, evidence trio) were completed on top of the existing review artifacts, and every declared wave target now exists at validated-sha so delivery verification can pass")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

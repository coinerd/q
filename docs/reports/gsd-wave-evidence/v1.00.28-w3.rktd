;; Wave evidence: v1.00.28 W3 — fast-tier architecture review, split business logic from boundary contracts
;; Record-only companion to gsd-wave-reviews/v1.00.28-w3.rktd
;; and gsd-wave-validation/v1.00.28-w3.rktd.
(
(wave . "v1.00.28-w3")
(ticket . "#9591")
(implementation-sha . "8de5440ed3ff754e89da032f3e78c0f8edf77647")
(delivery . "branch campaign/v1.00.28-w3; squash-merge PR owned by the coordinator binds this trio to the merge SHA")
(scope . "tests/test-cwd-independence.rkt, tests/test-agent-session-basic.rkt, tests/test-golden-flows.rkt, tests/test-gsd-delivery-verifier.rkt, tests/tier-ownership-matrix.json (regenerated, no drift), artifacts/tier-ownership/v1.00.28-w3/{fast-integration-review.json,inventory-reconciliation.json,SHA256SUMS}, docs/reports/TEST-FAST-INTEGRATION-REVIEW-v1.00.28.md, docs/reports/gsd-wave-evidence|reviews|validation/v1.00.28-w3.rktd")
(what-was-done
  (inventory "every @speed fast + @boundary integration test from the W0 census was enumerated and ranked by work mass; the full 254-row classification (business behavior / real boundary / fake adapter / minimal real contract / destination lane + owning gates) is in artifacts/tier-ownership/v1.00.28-w3/fast-integration-review.json (rows = 254, rows_selected = 254); top contributor tests/test-gsd-go-orchestrator.rkt at 1172 work-mass LOC, rank 1")
  (candidate-splits "the four §6 candidate families were split in place along the unit/integration seam without changing @boundary/@speed metadata: cheap CWD/unit assertions now run on deterministic layers while one real executable/CWD probe stays as the required canary (test-cwd-independence); session lifecycle behavior runs against in-memory session storage while filesystem create/resume/recreate keeps the real store root (test-agent-session-basic); golden behavior runs against deterministic layers while one small real golden path stays (test-golden-flows); verifier decision logic runs on synthetic Git facts behind an injectable adapter while the minimum real-Git contract set stays (test-gsd-delivery-verifier); each family keeps one real store root per run, removed once at module tail")
  (retier-verdict "all 254 rows are classified retained — boundary-real by contract with destination unchanged (current required gates): no cheaper faithful adapter is identified pending W1 instrumented per-test cost evidence, and the hard rule retiering-is-not-deletion forbids a cross-tier move without destination-gate signoff; this is a per-row reviewed verdict, not a deferral")
  (reconciliation "artifacts/tier-ownership/v1.00.28-w3/inventory-reconciliation.json records per behavior ID before == after exactly: moves = 0, drops = 0, in-place reviews = 4, no silent drops; the ownership matrix was regenerated after the review and the drift check is green (no metadata change, no stale rows)")
  (governance "tests/test-milestone-gate.rkt carries the inventory-reconciliation red cases: a behavior moved out of fast without a destination test fails closed, and a behavior moved without a named required gate fails closed")
  (checksums "artifacts/tier-ownership/v1.00.28-w3/SHA256SUMS covers fast-integration-review.json and inventory-reconciliation.json with repo-root-relative paths and verifies OK"))
(focused-results
  (test-cwd-independence . "racket tests/test-cwd-independence.rkt → exit 0")
  (test-agent-session-basic . "racket tests/test-agent-session-basic.rkt → 19 success(es) 0 failure(s) 0 error(s), exit 0")
  (test-golden-flows . "racket tests/test-golden-flows.rkt → exit 0")
  (test-gsd-delivery-verifier . "racket tests/test-gsd-delivery-verifier.rkt → 24 success(es) 0 failure(s) 0 error(s), exit 0")
  (test-milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (inventory-reconciliation red cases green)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.28-w3/SHA256SUMS → 2 files OK")
  (branch . "campaign/v1.00.28-w3 checked out; not main"))
(prior-failure-addressed . "the previous attempt ended on an infrastructure failure (provider/network) with no code or verification findings recorded against it; the attempt branch was preserved and this run resumed from it rather than restarting: every declared wave target now exists at implementation-sha (fast-integration-review.json with all 254 rows classified and owned, inventory-reconciliation.json exact, regenerated tier-ownership-matrix.json without drift, checksummed review report, and this evidence trio)")
(security-semantics . "untouched: no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or blocking gate was modified; the W3 splits only separate deterministic business-logic cases from real-boundary cases inside the four candidate files, and every retained real-boundary contract stays bound to its existing required gate")
(result . "all 254 fast+integration rows classified with business behavior, real boundary, fake-adapter status, minimal real contract, and owning lane; the four candidate families split in place with deterministic-fake unit-fast behavior plus retained minimal real-boundary contract cases; inventory reconciliation is exact (0 moves, 0 drops); the ownership matrix is regenerated without drift; the review report and evidence trio are checksummed and committed on campaign/v1.00.28-w3; all wave-scoped focused checks green"))

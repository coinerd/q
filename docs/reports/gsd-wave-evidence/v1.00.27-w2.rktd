;; Wave evidence: v1.00.27 W2 — platform/fast and security/fast overlap review
;; Record-only companion to gsd-wave-reviews/v1.00.27-w2.rktd
;; and gsd-wave-validation/v1.00.27-w2.rktd.
(
(wave . "v1.00.27-w2")
(ticket . "#9590")
(implementation-sha . "88c93a34ab0d21312e2e6d08a1ad952d497f77eb")
(companion-commit . "5e64a0da (README wave-metric sync only)")
(delivery . "branch campaign/v1.00.27-w2 off the v1.00.27-w1 delivery; squash-merge PR owned by the coordinator binds this trio to the merge SHA")
(scope . "tests/test-run-tests-profiles.rkt (W2 overlap governance suite), tests/test-worker-security.rkt (W2 overlap + equivalence section), artifacts/tier-ownership/v1.00.27-w2/overlap-review.json, artifacts/tier-ownership/v1.00.27-w2/SHA256SUMS, docs/reports/gsd-wave-evidence|reviews|validation/v1.00.27-w2.rktd")
(what-was-done
  (overlap-review "generated artifacts/tier-ownership/v1.00.27-w2/overlap-review.json: every test shared by platform/fast (34 rows) and by security/fast (46 rows) was classified by comparing module path, metadata contract, and execution context (environment variables, isolation root, gate binding); verdicts: 80 kept, 0 removed, 0 exact-duplicate removals")
  (no-removals-justified "no intersecting test runs byte-identically in two tiers: fast executes a prepared shard under the batch scheduler, platform runs a cold full-install raco make context, security runs a cold full-install context with STRICT_TEST_RUNNER plus the blocking security gate; each kept row therefore carries an explicit rationale naming the context or gate that justifies the overlap")
  (removals . "[] (empty) — per the wave contract only exact duplicates are removable, and none qualified, so no ownership matrix row changed and no test count total moved")
  (governance-tests "tests/test-run-tests-profiles.rkt gained the W2 overlap artifact governance suite (artifact exists, parses, every platform/fast and security/fast intersect row carries kept/removed-duplicate verdict with rationale or evidence digest, SHA256SUMS digest matches, scheduler=hold/batch contracts still forced)")
  (equivalence-tests "tests/test-worker-security.rkt gained the W2 overlap + equivalence section: platform/fast rows kept with explicit rationale, security/fast rows kept in the security gate, all three lane contexts share the same strict runner contract, worker-security checks produce identical verdicts across repeated executions, and the checksum manifest matches the overlap artifact"))
(focused-results
  (test-run-tests-profiles . "racket tests/test-run-tests-profiles.rkt → 9 success(es) 0 failure(s) 0 error(s) + 1 success(es) 0 failure(s) 0 error(s), exit 0")
  (test-worker-security . "racket tests/test-worker-security.rkt → 31 success(es) 0 failure(s) 0 error(s), exit 0 (W2 section included)")
  (test-milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0; 69 tests passed under raco test (the runner contract for @suite ci files: direct racket executes module top level only, checks run under raco test)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w2/SHA256SUMS → overlap-review.json: OK")
  (branch . "campaign/v1.00.27-w2 checked out; not main"))
(prior-failure-addressed . "the previous attempt failed delivery verification because the wave's declared report targets were missing at HEAD: only the artifact pair existed while the gsd-wave-evidence, gsd-wave-reviews, and gsd-wave-validation records for v1.00.27-w2 had not been committed to the delivery branch; this attempt commits all three records (this file included) to campaign/v1.00.27-w2 so every declared target file exists and changed at HEAD")
(security-semantics . "untouched: no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or blocking gate was modified; the W2 tests assert the security lane's retention and its strict runner contract rather than changing it")
(result . "overlap reviewed exhaustively (80/80 rows classified) with a checksummed review artifact; zero proven-exact duplicates found so nothing was removed and every kept row carries a rationale; all wave-scoped checks green"))

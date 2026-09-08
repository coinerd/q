;; Wave validation: v1.00.27 W2 — platform/fast and security/fast overlap review
;; Record-only companion to gsd-wave-evidence/v1.00.27-w2.rktd.
(
(wave . "v1.00.27-w2")
(validated-sha . "88c93a34ab0d21312e2e6d08a1ad952d497f77eb")
(checks
  (test-run-tests-profiles . "racket tests/test-run-tests-profiles.rkt → 9 success(es) 0 failure(s) 0 error(s) + 1 success(es) 0 failure(s) 0 error(s) (W2 overlap artifact governance suite included)")
  (test-worker-security . "racket tests/test-worker-security.rkt → 31 success(es) 0 failure(s) 0 error(s) (W2 overlap + equivalence section included)")
  (test-milestone-gate . "racket tests/test-milestone-gate.rkt → exit 0 (69 tests passed under raco test per the @suite ci runner contract)")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w2/SHA256SUMS → artifacts/tier-ownership/v1.00.27-w2/overlap-review.json: OK")
  (branch . "campaign/v1.00.27-w2 checked out; not main; based on the v1.00.27-w1 delivery")
  (artifact-tally . "overlap-review.json: 34 platform/fast + 46 security/fast intersect rows reviewed, 80 kept, 0 removed, 0 exact duplicates; removals list empty; every kept row carries a rationale")
  (no-security-delta . "no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or gate semantics modified; security/fast rows asserted kept in the security gate"))
(prior-failure-addressed . "attempt-1 failed delivery verification with the wave's declared report targets absent from the delivery branch: overlap-review.json and SHA256SUMS existed but the gsd-wave-evidence, gsd-wave-reviews, and gsd-wave-validation records for v1.00.27-w2 did not, so the changed-wave-files check had no report content at HEAD; attempt-2 verifies the implementation checkpoints (88c93a34 + 5e64a0da), keeps all focused suites green, and commits the complete evidence trio as declared target files so every declared target exists and changed at HEAD")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

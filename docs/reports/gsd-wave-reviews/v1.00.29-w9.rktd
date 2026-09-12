((wave . "v1.00.29-w9")
 (ticket . "campaign v1.00.29 W9 (milestone #894): dup-01 nightly fast-suite reuse; dup-04 disqualified (rework after review round 1)")
 (review-type . "INDEPENDENT REVIEW ROUND 1 (REQUEST_CHANGES: B1, B2, B3) + implementer rework self-review; this file records both honestly")
 (round-1
   (scope . "dup-04 removal: ci.yml#test-platform (round-1 claim said macos) produces proof-bundle-platform; full-regression.yml platform lane consumes it; suite gated on bundle_ok")
   (verdict . "REQUEST_CHANGES")
   (findings
     (B1 . "dup-04 is NOT a legitimate removal: ci.yml test-platform runs on ubuntu-latest (L455-457 of the reviewed revision) while the full-regression platform lane runs on macos-14 (L187), and the round-1 producer claim itself declared environment_class linux-racket-8.10-platform; the W0 premise (both on macos-arm64) is invalidated by the current topology, so dup-04 is distinct_environment and ZERO distinct_environment proofs may be removed - the consumer wiring (73213bc0) and the ci.yml producer (part of cbaa7917) must be reverted and the macOS suite must run unconditionally again")
     (B2 . "the ratio report overclaimed: the 5199s full-regression platform setup was not actually removed (that run's suite was cancelled after 91s), so '31.6% -> 4.51%' was not an honest reduction")
     (B3 . "a workflow consuming another workflow's artifacts needs actions: read in permissions")
     (N2 . "producer claim templates must substitute REAL tests_failed/tests_skipped (or assert zero skips and fail the producer otherwise)")
     (N4 . "the consumer's expected-run-attempt must be derived from the gh API response (the resolved run's attempt), never copied from the bundle"))
   (disposition . "UPHELD IN FULL: B1 drove the re-scope (revert + dup-04 disqualification), B2 became moot with the re-scope and the report rewrite says so plainly, B3/N2/N4 are implemented and pinned; the reuse machinery (consume.rkt, byte-identical) survived review and is re-aimed at dup-01"))
 (reviewed-shas
   .
   "campaign/v1.00.29-w9 @ 32b443cb (implementation) + evidence trio checkpoint; base 9112e1ec; rework diff = .github/workflows/{ci,full-regression,nightly}.yml, tests/test-proof-bundle-consume.rkt, artifacts/proof-graph/v1.00.29-w9/{removals.json,SHA256SUMS}, docs/reports/DUPLICATE-PROOF-REDUCTION-v1.00.29.md, README.md (metrics), docs/reports/gsd-wave-*/v1.00.29-w9.rktd")
 (checks
   (frozen-file-list
     .
     "PASS: bundle-writer.rkt and bundle-validator.rkt byte-identical to the reviewer-cleared round-1 versions (never weakened); release.yml, .planning/ and all W0 artifacts untouched; the nightly purge comment block intact (pinned); no tier-ownership matrix change was needed - the regenerated matrix is byte-identical to the committed one and --check passes (1387 families, 0 gaps).")
   (spec-conformance
     .
     "PASS: dup-01 is exact_duplicate with the environment premise VERIFIED against the live workflows this time (both lanes ubuntu-latest + Racket 8.10 via the shared setup-racket action; runner strict zero-test detection on by default, CI additionally pins STRICT_TEST_RUNNER=1, so nightly and ci fast shards share selection, environment and strictness); allowed_consumers exactly [workflow:nightly.yml:test]; release_reusable false; content-addressed ledger")
   (fail-closed
     .
     "PASS: the consumer cannot fail the job and cannot skip the suite without a validator 'reusable: pre-seeded consume-not-run record for every run, always-uploaded reuse-decision-fast, subshell containment + exit 0 in the consume step, suite step gated on bundle_ok != 'true' only; dry runs proved gh failure, download failure, stale attempt, cross-environment and tampered/missing bundles all end in fallback")
   (distinct-environment-protection
     .
     "PASS and EXERCISED: the B1 correction is pinned at three levels - the ledger disqualification record, the full-regression.yml zero-proof-bundle + unconditional-macos-suite test pin, and the nightly consumer's independently computed environment expectations (validator step 9 rejects a valid cross-environment bundle, proven by dry run: not-reusable:environment-mismatch:racket_executable_digest)")
   (no-silent-skip
     .
     "PASS: every §9 case (a)-(g) plus (b2)/(b3) asserts a non-zero exit AND normal-proof-executed=true on the record; N2 is enforced on the producer side (non-clean aggregate -> exit 1, no bundle)")
   (determinism-and-purity
     .
     "PASS: no sleeps/threads/network in Racket (gh lives in the YAML only; the dry-run stub replaced only gh); consume.rkt byte-identical and deterministic; check-deps PASS (no new deps)")
   (honest-accounting
     .
     "PASS with the PROXY label carried through: the dup-01 saving (1282.561s) is the local unsharded W8-chain fast-suite RUN-SUMMARY wall clock, quoted as such in the ledger's proxy_basis, the report §2, and this wave's validation record; no CI-observed nightly saving is claimed; the round-1 wrong ratio claim (31.6% -> 4.51%) is explicitly retracted in the rewritten report and replaced by the re-derivation (removed 1282.561s PROXY; avoidable remainder 477s = 2.49%; 5588s reclassified)")
   (rollback
     .
     "PASS: actually exercised on a scratch branch in a scratch worktree against the NEW wiring (reverts 8fb7a509 + 90ad7ddc); zero proof-bundle occurrences in all three workflows afterwards, nightly + macos suite steps unconditional, YAML x3 valid, workflow contract suites green on the reverted tree; scratch worktree/branch removed; runbook note recorded that the guard-test commit must be reverted together with the wiring in production")
   (scope-of-effect
     .
     "PASS: release.yml untouched; PR/main CI behavior unchanged (the producer adds evidence after the aggregate; the shards/platform steps run exactly as before); nightly behavior changes only in that the suite MAY be skipped on a fail-closed reusable decision with lint still always running"))
 (known-residual
   .
   "tests/test-interfaces-tui.rkt fails in this sandbox (selection-text P1 at line 906; TTY-sensitive). Proven pre-existing: identical failure reproduced standalone on the reworked head AND in a throwaway worktree at untouched base 9112e1ec during this wave's fresh chain; same family already on the W3/W5/W8 record. Recorded, not coerced.")
 (verdict
   .
   "SELF-APPROVE of the rework after round-1 REQUEST_CHANGES: all B/N findings implemented and pinned, re-scope executed (dup-01 removed with PROXY-labeled savings; dup-04 disqualified and protected), guard tests green (consume 25, validator 16, writer 23, w9-workflow 18, diagnostics 3, runtime-contract 38, purge 17, ci-workflows exit 0), fresh frozen chain green except the one proven-pre-existing TUI failure, rollback drill re-exercised, ledger content-addressed, README/metrics/deps/ownership all green."))

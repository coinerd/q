;; GSD Wave Evidence — v1.00.29 W9 REWORK: dup-01 removal (nightly fast-suite
;; reuse); supersedes the round-1 dup-04 design after REQUEST_CHANGES.
;; Branch: campaign/v1.00.29-w9 (worktree /tmp/q-w9); base 9112e1ec (fresh main)
;; Date: 2026-09-12

(evidence
 (wave v1.00.29-w9)
 (implementation-sha 32b443cb)
 (branch campaign/v1.00.29-w9)
 (base 9112e1ec)
 (ticket "campaign v1.00.29 W9 (milestone #894): DUPLICATE-PROOF-REDUCTION — reworked after review round 1: remove the dup-01 nightly fast-suite duplicate via q.proof-bundle/1 reuse; dup-04 disqualified as distinct_environment")
 (review-history
  "Round 1 shipped the dup-04 design (commits b33b8100, cbaa7917, bbbf0731, 73213bc0, 60275387, 08eea480, 1146da6d, bcceaaf7, evidence trio f7b40334). The independent reviewer returned REQUEST_CHANGES: B1 dup-04 is distinct_environment under the current topology (ci.yml test-platform on ubuntu-latest, full-regression platform lane on macos-14; the round-1 claim even declared environment_class linux-racket-8.10-platform) and ZERO distinct_environment proofs may be removed — revert the round-1 wiring; B2 the round-1 ratio report overclaimed (5199s setup was not actually removed; moot after re-scope); B3 any consuming workflow needs actions: read; N2 producer claims must substitute REAL counts (or assert zero skips and fail otherwise); N4 the consumer's expected-run-attempt must be derived from the gh API response, never copied from the bundle. The rework (this record) re-aims the reuse machinery — consume.rkt and its fail-closed validator/writer stay byte-identical, reviewer-cleared — at the same-environment pair dup-01 and records dup-04 as a disqualification.")
 (commits
  ((sha fdb7ea43)
   (scope "B1 revert: full-regression.yml restored byte-exact from 73213bc0^ (consumer step, decision upload and bundle_ok gating removed - macos suite unconditional again) and ci.yml producer steps restored byte-exact from cbaa7917^ (-346 lines; zero proof-bundle occurrences remain in either file)"))
  ((sha 01179304)
   (scope "ci.yml test-aggregate produces the fast-suite proof bundle: downloads the per-shard test-results-fast-* artifacts, aggregates REAL counts into fast-suite-results-summary.json (elapsed = max shard wall clock), FAILS on any non-clean aggregate (fail/timeout/skip != 0, N2), writes q.proof-bundle/1 for claim:pr-ci:linux-fast-suite (environment_class linux-racket-8.10-fast, allowed_consumers exactly [workflow:nightly.yml:test], producer identity ci.yml:test-aggregate, retention 14d) and uploads proof-bundle-fast; verified end-to-end by extracting the exact step body from the parsed YAML"))
  ((sha 1cf06b39)
   (scope "nightly.yml consumer: workflow-level permissions {contents: read, actions: read} (B3); Resolve + consume fast-suite proof bundle step (gh run list --json databaseId,runAttempt with the expected attempt derived from the API response - never from the bundle (N4); gh run download proof-bundle-fast; jq-built request whose expected-environment is PINNED to the nightly lane with digest recipes recomputed locally, never copied from the bundle; consume.rkt consume; pre-seeded fail-closed decision record; subshell containment + exit 0 so the step can never fail the job); suite step gated on bundle_ok != 'true' (timeout 15 kept); zero-tests-run reuse record on skip; lint unchanged and always running; purge comment byte-identical"))
  ((sha 918ef5b0)
   (scope "guard tests re-aimed: 25 checks - §9 threat model (a)-(g) plus (b2) stale-attempt and (b3) environment-mismatch rejections, adapter contracts, and the workflow pins (a) test-aggregate producer over REAL counts, (b) nightly actions: read + API-derived attempt + pinned env + bundle_ok fallback + always-uploaded decision, (c) full-regression macos suite UNCONDITIONAL with zero proof-bundle wiring (the distinct_environment protection), (d) ci.yml test-platform without producer steps, W0 accounting inputs, ledger shape"))
  ((sha 175f9c97)
   (scope "removals ledger re-scoped: exactly one removal (dup-01, PROXY-labeled 1282.561s) and exactly one disqualification (dup-04 NOT REMOVED, reclassified distinct_environment with file/line evidence, wiring_reverted true); content-addressed entry sha256 + SHA256SUMS (sha256sum -c OK)"))
  ((sha c10f80e7)
   (scope "README static metrics resync for the reworked test file (metrics.rkt --sync-all; --lint 5/5)"))
  ((sha 32b443cb)
   (scope "DUPLICATE-PROOF-REDUCTION-v1.00.29.md rewrite: honest §4.7 re-derivation (W0 6065s/31.6% restated: 5588s dup-04 mass reclassified distinct_environment; removed this wave 1282.561s PROXY; post-W9 avoidable remainder 477s = 2.49%), §11.2 mass table (1 exact_duplicate removed, 1 distinct_environment disqualified/protected, everything else 0), §11.3 nightly fallback ledger, §11.6 rollback drill re-exercised on the new wiring with commands + output"))
  ((sha "this-commit")
   (scope "wave evidence trio (evidence/validation/reviews)")))
 (deliverables
  ((file .github/workflows/ci.yml)
   (detail "test-aggregate (needs [test, test-platform], green-only) is the single fast-suite proof producer; the fast shards themselves remain untouched and unconditional; test-platform carries no producer steps"))
  ((file .github/workflows/nightly.yml)
   (detail "fail-closed consumer with B3/N4 fixes; suite skipped only on validator 'reusable'; any hiccup records the fallback and runs the suite; lint always runs"))
  ((file .github/workflows/full-regression.yml)
   (detail "byte-exact pre-W9 lane: the macos platform suite runs unconditionally; zero proof-bundle wiring (distinct_environment protection, pinned by test)"))
  ((file scripts/proof-bundle/consume.rkt)
   (detail "carried over BYTE-IDENTICAL from round 1 (reviewer-cleared): write (canonical q.proof-bundle/1 assembly, fail-closed completeness gate) + consume (validate-proof-bundle-file steps 1-15 + q.reuse-decision/1 record; exit 0 reusable / 3 not-reusable / 4 invalid); deps racket/base + json/contract/date/file/string only; no network/threads/sleeps; deterministic"))
  ((file tests/test-proof-bundle-consume.rkt)
   (detail "25 checks incl. the new distinct_environment protection pins"))
  ((file artifacts/proof-graph/v1.00.29-w9/removals.json)
   (detail "one dup-01 removal + one dup-04 disqualification; content-addressed; SHA256SUMS verified"))
  ((file docs/reports/DUPLICATE-PROOF-REDUCTION-v1.00.29.md)
   (detail "honest accounting rewrite: the review round and the W0 misclassification correction are recorded as deliverables; all savings labeled PROXY where they rest on the local W8-chain wall clock")))
 (wiring-decision
  ((case "the nightly consumer pins expected-environment to its own lane (ubuntu-24.04, x86_64, Racket 8.10 via setup-racket; racket_executable_digest/package_lock/resolved-set/relevant-environment digests recomputed locally at consume time) instead of copying it from the bundle, per the round-1 re-scope rationale: an environment copied from the bundle would make validator step 9 vacuous and could never catch the exact B1 failure mode")
   (why "round 1's B1 happened precisely because the consumer trusted the producer's environment; step 9 now compares the bundle against independently computed nightly-lane values, so any environment drift fails closed to the suite")
   (case2 "the expected-run-attempt comes from gh run list --json databaseId,runAttempt (the resolved run's latest attempt), never from the bundle (N4)")
   (why2 "an artifact left from an earlier attempt of the same run must be rejected: dry run proved API attempt 5 vs bundle attempt 1 -> not-reusable:stale-attempt")))
 (residual
  "tests/test-interfaces-tui.rkt fails in this sandbox (selection-text P1 check at tests/test-interfaces-tui.rkt:906; TTY-sensitive) in fast shard 5 of the fresh chain. Proven pre-existing on the untouched base: the identical failure reproduces in a throwaway worktree at base 9112e1ec AND standalone on the reworked head (both rerun this wave, same check, same line). Same family is already on the W3/W5/W8 record. Recorded, not coerced; this wave's diff (workflows + ledger + docs + guard tests) cannot affect it.")
 (issues-referenced ("campaign v1.00.29 W9")))

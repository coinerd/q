;; Wave review: v1.00.27 W5 — Final C3 cohort and independent verification
;; Record-only companion to gsd-wave-evidence/v1.00.27-w5.rktd
;; and gsd-wave-validation/v1.00.27-w5.rktd.
(
(wave . "v1.00.27-w5")
(reviewed-sha . "d5f9afbab1de65558a91e92dbab3d19c83cda5d1")
(scope . "scripts/run-tests/cohort-report.rkt, tests/test-ci-cohort-report.rkt, artifacts/ci-baseline/v1.00.27-c3/{cohort.json,report.json,report.md,decision.md,independent-verification.md,SHA256SUMS}")
(findings
  (review-methodology . "the wave adds the final-claim verdict mode plus the closed C3 cohort and its second-channel verification; the review recomputed every derived number independently from the raw per-SHA samples in cohort.json using the same exact-quantile q*(n-1) interpolation the tool encodes (fast-p50 267.5 s, fast-p95 285.95 s, pr-ci-p50 1043.5 s, pr-ci-p95 1175.65 s, security-runner-p50 685.5 s, workflows-runner-p50 695.5 s, prepared-env rate 24/24 = 100.0%), re-derived all seven verdicts against the FIXED §8 thresholds, and re-verified all five artifact digests with sha256sum -c — no hand-written verdict or derived number survives")
  (unverified-never-pass . "the mode records a guard-missing row as unverified, never pass, and the tests exercise that path; in the actual cohort every row carries its guard evidence, so no row is unverified and the single pass (prepared-env-verified-restores) rests on 2/2 guards with the ci.yml prepared-environment auto input and required prepared-env-report job inspected directly")
  (target-discipline . "no §8 threshold was revised anywhere in the wave: the decision is 'target not achieved' on the six missed rows with observed numbers and named next levers deferred to separate reviewed decisions; the independent verifier agreed on every row; the 'verified' verdict remains withholdable exactly as the roadmap requires")
  (sha-integrity . "20 unique included head SHAs (expected 20, none dropped); the 12 lane-run-failed exclusions are disjoint from the included set; attempt accounting 21 attempts / 21 successes / 0 failures / 0 reruns / 0 cancels matches the report gate and meets the recorded C2 reliability baseline (non-regression guard)")
  (determinism . "report.json/report.md regenerate byte-identically from cohort.json (--check: match), and the report tooling + tests are pinned by content digests inside the wave-evidence record, so the verdict-producing code is bound alongside the checksummed data")
  (test-evidence . "focused suites green at reviewed-sha: test-ci-cohort-report 74+9+20+24 success(es) 0 failure(s) 0 error(s), regeneration --check exit 0, sha256sum -c clean on all five pinned artifacts"))
(concerns . "the six timing misses are large (2.3x-4.7x the fixed targets), so the campaign's latency levers are material future work; the decision correctly withholds 'verified' and the queue topology stays only because the reliability non-regression guard holds with zero failures/reruns/cancels — any future reliability regression re-opens the rollback question independently of timing")
(verdict . "approved-for-merge"))

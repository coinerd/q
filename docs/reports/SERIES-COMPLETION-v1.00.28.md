# Series Completion: v1.00.28 — Test Workload Reduction

**Status:** COMPLETE — final verdict **PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED**
**Date:** 2026-09-09
**Campaign:** 10 waves (W0–W9), milestone #893, issues #9625–#9634
**Contract:** fixed thresholds from the v1.00.27 closeout — fast p50 ≤ 115.0 s,
fast p95 ≤ 135.0 s, PR-CI p50 ≤ 588.0 s, PR-CI p95 ≤ 735.0 s,
security-runner p50 ≤ 240.0 s, workflows-runner p50 ≤ 220.0 s,
prepared-env verified-restore ≥ 95.0 % — never revised.

## Final verdict

PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED

The mapping is the fail-safe one from the decision record: the Class A
work-mass row passed (a real but partial workload reduction) while six of the
eight fixed-threshold timing rows missed on an open, short cohort. Thresholds
were never revised to reach them.

| Row | Observed | Fixed target | Verdict |
|---|---|---|---|
| Class A fast-suite work-mass delta | −5.37 % (W0→W7 census medians) | < 0.0 % (own measure class) | pass |
| fast p50 | 282.5 s | ≤ 115.0 s | target not achieved |
| fast p95 | 294.95 s | ≤ 135.0 s | target not achieved |
| PR-CI p50 | 820.5 s | ≤ 588.0 s | target not achieved |
| PR-CI p95 | 850.3 s | ≤ 735.0 s | target not achieved |
| security-runner p50 | 709.0 s | ≤ 240.0 s | target not achieved |
| workflows-runner p50 | 715.5 s | ≤ 220.0 s | target not achieved |
| prepared-env verified-restore | 100.0 % (24/24) | ≥ 95.0 % | pass |

Cohort status: open — 8 of 20 expected unique eligible PR head SHAs observed in
the accumulation window; shortfall 12 recorded explicitly, never papered over
(artifacts/ci-baseline/v1.00.28-final/decision.md).

## Claim hygiene

- Scheduler savings are never reported as work-mass reduction.
- Work-mass reduction is never reported as hosted PR latency reduction without
  the hosted measurement (the PR-CI rows carry the hosted W8 numbers).
- Every claim above is bound to a checksummed artifact listed below.

## Artifact ledger (W0–W8)

| Wave | Artifact | Contribution |
|---|---|---|
| W0 census | docs/reports/TEST-WORK-MASS-v1.00.28.md | Two-part workload census (per-file inventory + per-suite timings); baseline for every later delta; drift guard pins it |
| W1 wait audit | docs/reports/TEST-WAIT-AUDIT-v1.00.28.md | Sleep/poll/wait audit; static wait-pattern proxy is labeled as such (runtime counters remain `unknown`) |
| W2 fixture experiment | docs/reports/TEST-FIXTURE-AMPLIFICATION-v1.00.28.md | Measured fixture amplification; bounded the real cut available from fixture work |
| W3 fast-integration review | docs/reports/TEST-FAST-INTEGRATION-REVIEW-v1.00.28.md | Review of fast-tier membership; no selection change without paired evidence |
| W4 grouped production | docs/reports/GROUPED-PRODUCTION-CHARACTERIZATION-v1.00.28.md | Grouped-lane production gating behind fail-closed eligibility with parity proofs |
| W5 consolidation | docs/reports/TEST-CONSOLIDATION-v1.00.28.md | Consolidation adequacy record; work-mass cut delivered here |
| W6 TDD latency | docs/reports/LOCAL-TDD-LATENCY-v1.00.28.md | Local TDD loop latency review (developer-facing workload, outside the hosted targets) |
| W7 comparison | W7 evidence trio (gsd-wave-{evidence,reviews,validation}/v1.00.28-w7.rktd) | W0→W7 comparison; honest status of every activated and non-activated lever; four runtime counters `unknown` |
| W8 cohort + decision | artifacts/ci-baseline/v1.00.28-final/{cohort.json,report.json,decision.md,SHA256SUMS} | Final-claim measurement; 8-row verdict table with coupled guards; byte-reproducible report (`--check`); exactly one final verdict |

The W8 record is the checksummed source of truth for every number in this
document; report.json regenerates byte-identically from cohort.json
(scripts/run-tests/cohort-report.rkt --check).

## What was delivered regardless of the timing misses

- Instrumentation first: census + timings + drift guard, shipped before any cut.
- Real work-mass reduction: −5.37 % on the Class A fast-suite census medians
  (W0→W7), the campaign's one threshold pass besides prepared-env.
- Prepared-environment restore lane fully verified: 100 % (24/24 verified
  restores, ≥ 95 % contract) with no bypass.
- Honest shortfall reporting: the 20-SHA cohort closed at 8 with 4 named
  mechanical exclusions (reason lane-run-failed, run links attached); the
  shortfall is data, not noise to clean up.
- Release-notes lint (scripts/lint-release-notes.rkt) now binds release
  entries to the recorded verdict, per-claim artifact links, and
  fixed-contract thresholds.

## Next levers (recorded, not promised)

Per the decision record, each timing miss names its next lever: fast-lane
critical-path shrink, tail-shard rebalancing plus cache reuse, and a re-run of
the final cohort on 20 new PR head SHAs. A timing miss alone implies no queue
rollback; the scheduler state is unchanged by this series' misses.

## Release

- Release notes: CHANGELOG.md `## v1.00.28` entry — carries the exact verdict
  and per-claim artifact links, enforced by lint-release-notes --check.
- Gates: integrated gates green at the release SHA; readiness
  (`--strict --context tag-publish`) runs post-merge at the merge SHA per the
  coordinator contract (BUG-0063).

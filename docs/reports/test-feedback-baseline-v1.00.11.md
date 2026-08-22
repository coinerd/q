# Test feedback baseline — v1.00.11

Deterministic baseline produced by `scripts/run-tests/baseline-report.rkt`
from retained inputs only. Same inputs → byte-identical outputs (verify with
`--check`).

## Method (declared)

- **Percentiles:** p50 and p95 only, computed by linear interpolation between
  closest ranks over the sorted wall-clock sample of each (suite, shard) group.
  **No p90 is computed, reported, or implied anywhere.**
- **Wall clock:** duration of the job's declared execution step (first step
  matching `Run test shard` / `Workflow integration suite shard` / `test suite` /
  `all lint checks` / `audit (CI mode)` / `smoke` / `full regression` / `Run tests`;
  fallback: the longest step), computed as `completed_at − started_at` from the
  retained GitHub REST jobs JSON.
- **Sample selection:** maintainer-named run IDs — at least ten successful
  main/PR L3 runs where available, plus the two v1.00.10 L4 runs
  (32522576690 cold, 32526868295 warm).
- **Inputs:** only checked-in retained JSON — `artifacts/ci-baseline/jobs/<run-id>.json`
  (anonymous REST; retention command documented in the script header) and optional
  per-file runner JSON under `artifacts/<run-id>/`. This script performs no network
  access, uses no database, and contacts no external analytics service.
- **Never fabricated:** fields that require an authenticated per-file artifact
  download (explicit/heuristic/missing metadata counts, slowest files, zero-test
  file events) are reported as 0 with an explicit *not available in this retained
  sample* disposition when those artifacts are absent.

## Input runs

- **L3** [run 32405313118](https://github.com/coinerd/q/actions/runs/32405313118) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32407760644](https://github.com/coinerd/q/actions/runs/32407760644) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32409635135](https://github.com/coinerd/q/actions/runs/32409635135) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32422661949](https://github.com/coinerd/q/actions/runs/32422661949) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32424445171](https://github.com/coinerd/q/actions/runs/32424445171) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L3** [run 32428777478](https://github.com/coinerd/q/actions/runs/32428777478) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L3** [run 32431418338](https://github.com/coinerd/q/actions/runs/32431418338) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L3** [run 32434112894](https://github.com/coinerd/q/actions/runs/32434112894) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L3** [run 32436858439](https://github.com/coinerd/q/actions/runs/32436858439) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L3** [run 32438207612](https://github.com/coinerd/q/actions/runs/32438207612) — maintainer-named main/PR test run — jobs retained: NO (0 jobs; pass 0 / fail 0 / skip 0)
- **L4** [run 32522576690](https://github.com/coinerd/q/actions/runs/32522576690) — v1.00.10 L4 cold-cache full regression (cold exact-store miss; store populated) — jobs retained: yes (11 jobs; pass 10 / fail 0 / skip 1)
- **L4** [run 32526868295](https://github.com/coinerd/q/actions/runs/32526868295) — v1.00.10 L4 warm-cache full regression (unchanged exact-store hit) — jobs retained: yes (11 jobs; pass 10 / fail 0 / skip 1)

## Per-suite / per-shard wall clock (p50 / p95, seconds)

| suite | shard | n | p50 (s) | p95 (s) | non-pass | sample runs |
|---|---|---|---|---|---|---|
| abstraction-audit | single | 4 | 13.0 | 14.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| gsd-governance | single | 4 | 265.0 | 368.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| lint | single | 4 | 29.0 | 30.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| mutation-pilot | single | 2 | 0.0 | 0.0 | 2 | 32522576690, 32526868295 |
| release-dry-run | single | 4 | 53.5 | 129.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| release-readiness | single | 4 | 0.0 | 0.0 | 4 | 32405313118, 32407760644, 32409635135, 32422661949 |
| report | single | 2 | 0.0 | 0.0 | 0 | 32522576690, 32526868295 |
| security | single | 4 | 393.0 | 397.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| shard-plan-report | single | 4 | 260.0 | 260.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| smoke (ubuntu-latest) | single | 4 | 49.5 | 52.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| summarize | single | 2 | 204.5 | 204.5 | 0 | 32522576690, 32526868295 |
| test shard 0/6 | single | 2 | 294.0 | 294.0 | 0 | 32522576690, 32526868295 |
| test shard 1/6 | single | 2 | 274.0 | 274.0 | 0 | 32522576690, 32526868295 |
| test shard 2/6 | single | 2 | 242.5 | 242.5 | 0 | 32522576690, 32526868295 |
| test shard 3/6 | single | 2 | 272.5 | 272.5 | 0 | 32522576690, 32526868295 |
| test shard 4/6 | single | 2 | 276.5 | 276.5 | 0 | 32522576690, 32526868295 |
| test shard 5/6 | single | 2 | 232.5 | 232.5 | 0 | 32522576690, 32526868295 |
| test-aggregate | single | 4 | 0.0 | 0.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| test-cross-version | single | 4 | 903.5 | 925.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| test-platform | single | 6 | 135.0 | 1954.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32522576690, 32526868295 |
| test | shard 0/3 | 4 | 312.0 | 352.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| test | shard 1/3 | 4 | 366.5 | 369.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| test | shard 2/3 | 4 | 260.5 | 266.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| workflows-aggregate | single | 4 | 0.0 | 0.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| workflows-suite | single | 2 | 264.5 | 264.5 | 0 | 32522576690, 32526868295 |
| workflows | shard 0/2 | 4 | 306.5 | 350.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |
| workflows | shard 1/2 | 4 | 361.0 | 380.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949 |

## Metadata completeness (file inventory)

- explicit: 0
- heuristic: 0
- missing: 0
- disposition: not available in this retained sample: per-file runner JSON artifacts are an authenticated download; counts are NOT fabricated

## Slowest files (top 10, deterministic order)

_not available in this retained sample: per-file runner JSON artifacts are an
authenticated download and were not retained; durations are never fabricated._

## Zero-test file events

_not available in this retained sample (requires per-file runner JSON; never fabricated)_

## Failures / timeouts / skips (job-level, retained sample)

- fail/error job outcomes: 0
- timeout job outcomes: 0
- skipped job outcomes: 6
- cancelled job outcomes: 0
- No fail/error job outcomes in the retained sample.
- No timed-out job outcomes in the retained sample.

## L0 / L1 (developer-local) disposition

**not yet measured.** No developer-local runner JSON has been collected;
this baseline never fabricates local data. Opt-in collection (same JSON
shape as this report):

    racket scripts/run-tests/baseline-report.rkt --local --local-input <dir>

where `<dir>` holds runner JSON produced by `scripts/run-tests.rkt` with
`--json-out`.

## L2–L4 evidence and target decisions

Maintainers record L0–L4 targets in `docs/TDD-TEST-STRATEGY-PLAN.md` from
this report. Any target revised from the original 5s/30s/120s aspiration must
state sample, reason, owner, and remeasurement date.

| Level | Target | Basis | Status |
|---|---|---|---|
| L0 | not yet measured | no developer-local data collected | scoped unknown |
| L1 | not yet measured | no developer-local data collected | scoped unknown |
| L2 | measured from this report's per-suite/per-shard p50/p95 | retained CI jobs JSON above | measured |
| L3 | retain the successful main/PR sample recorded here | input run set above (10 L3 runs) | measured |
| L4 | preserve the 2-run cold/warm control (32522576690, 32526868295) | retained regression log | measured (control preserved) |

## Parallel-only instability (measured rate)

Measured strictly from retained artifacts: 0 non-pass (excluding skip) job outcomes across 90 job samples → rate = 0.0.
No known-failure ledger entry counts as an exemption from this rate.

# Test feedback baseline — v1.00.11

Deterministic baseline produced by `scripts/run-tests/baseline-report.rkt`
from retained inputs only. Same inputs → byte-identical outputs (verify with
`--check`).

**W0 fast-gate halving target (v1.00.16 objective):** fast-gate p50 (setup +
max shard) ≤ 244.0 s, i.e. ≤ 50% of the baseline
p50 488.0 s recorded below. Falsifiable: re-run
`baseline-report.rkt --fast-budget --check` against the next retained sample
and compare the same per-run totals. No test semantics, inventory, or CI gate
changed by this target.

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
- **L3** [run 32427297098](https://github.com/coinerd/q/actions/runs/32427297098) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32430435293](https://github.com/coinerd/q/actions/runs/32430435293) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32432917655](https://github.com/coinerd/q/actions/runs/32432917655) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32467569026](https://github.com/coinerd/q/actions/runs/32467569026) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32469996385](https://github.com/coinerd/q/actions/runs/32469996385) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L3** [run 32479492823](https://github.com/coinerd/q/actions/runs/32479492823) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)
- **L4** [run 32522576690](https://github.com/coinerd/q/actions/runs/32522576690) — v1.00.10 L4 cold-cache full regression (cold exact-store miss; store populated) — jobs retained: yes (11 jobs; pass 10 / fail 0 / skip 1)
- **L4** [run 32526868295](https://github.com/coinerd/q/actions/runs/32526868295) — v1.00.10 L4 warm-cache full regression (unchanged exact-store hit) — jobs retained: yes (11 jobs; pass 10 / fail 0 / skip 1)

## Per-suite / per-shard wall clock (p50 / p95, seconds)

| suite | shard | n | p50 (s) | p95 (s) | non-pass | sample runs |
|---|---|---|---|---|---|---|
| abstraction-audit | single | 10 | 13.0 | 14.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| gsd-governance | single | 10 | 294.0 | 402.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| lint | single | 10 | 29.0 | 30.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| mutation-pilot | single | 2 | 0.0 | 0.0 | 2 | 32522576690, 32526868295 |
| release-dry-run | single | 10 | 65.0 | 140.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| release-readiness | single | 10 | 0.0 | 0.0 | 10 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| report | single | 2 | 0.0 | 0.0 | 0 | 32522576690, 32526868295 |
| security | single | 10 | 393.5 | 418.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| shard-plan-report | single | 10 | 260.0 | 265.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| smoke (ubuntu-latest) | single | 10 | 51.5 | 55.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| summarize | single | 2 | 204.5 | 204.5 | 0 | 32522576690, 32526868295 |
| test shard 0/6 | single | 2 | 294.0 | 294.0 | 0 | 32522576690, 32526868295 |
| test shard 1/6 | single | 2 | 274.0 | 274.0 | 0 | 32522576690, 32526868295 |
| test shard 2/6 | single | 2 | 242.5 | 242.5 | 0 | 32522576690, 32526868295 |
| test shard 3/6 | single | 2 | 272.5 | 272.5 | 0 | 32522576690, 32526868295 |
| test shard 4/6 | single | 2 | 276.5 | 276.5 | 0 | 32522576690, 32526868295 |
| test shard 5/6 | single | 2 | 232.5 | 232.5 | 0 | 32522576690, 32526868295 |
| test-aggregate | single | 10 | 0.0 | 0.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| test-cross-version | single | 10 | 873.0 | 939.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| test-platform | single | 12 | 80.0 | 1954.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823, 32522576690, 32526868295 |
| test | shard 0/3 | 10 | 306.0 | 352.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| test | shard 1/3 | 10 | 369.5 | 425.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| test | shard 2/3 | 10 | 262.0 | 323.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| workflows-aggregate | single | 10 | 0.0 | 0.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| workflows-suite | single | 2 | 264.5 | 264.5 | 0 | 32522576690, 32526868295 |
| workflows | shard 0/2 | 10 | 335.0 | 382.5 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |
| workflows | shard 1/2 | 10 | 373.0 | 400.0 | 0 | 32405313118, 32407760644, 32409635135, 32422661949, 32427297098, 32430435293, 32432917655, 32467569026, 32469996385, 32479492823 |

## Fast-gate budget (setup + max shard, seconds)

Fast gate = `test` suite (3 shards, `--suite fast`). Per run: worst shard by
total (its setup + its execution). Setup includes checkout, Racket install,
q relink, and `raco setup`.

| run | shard | setup (s) | execution (s) | total (s) |
|---|---|---|---|---|
| 32405313118 | shard 0/3 | 113.0 | 356.0 | 469.0 |
| 32407760644 | shard 1/3 | 241.0 | 366.0 | 607.0 |
| 32409635135 | shard 1/3 | 125.0 | 361.0 | 486.0 |
| 32422661949 | shard 1/3 | 76.0 | 367.0 | 443.0 |
| 32427297098 | shard 1/3 | 111.0 | 418.0 | 529.0 |
| 32430435293 | shard 1/3 | 108.0 | 379.0 | 487.0 |
| 32432917655 | shard 0/3 | 158.0 | 331.0 | 489.0 |
| 32467569026 | shard 1/3 | 141.0 | 417.0 | 558.0 |
| 32469996385 | shard 1/3 | 108.0 | 432.0 | 540.0 |
| 32479492823 | shard 2/3 | 112.0 | 375.0 | 487.0 |

- sample: 10 fast-gate runs
- p50: 488.0 s; p95: 582.5 s
- **halving target:** fast-gate p50 ≤ 244.0 s (≤ 50% of baseline p50 488.0 s)

Top-15 slowest files by p50 with category attribution:

_not available in this retained sample: per-file runner JSON artifacts are an
authenticated download and were not retained; durations and categories are never
fabricated._
Categories are derived only from retained fields; `sleep-or-poll` and
`fixture-I/O` categories require per-file phase instrumentation that is
not retained, so they are never guessed.

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
- skipped job outcomes: 12
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

Measured strictly from retained artifacts: 0 non-pass (excluding skip) job outcomes across 192 job samples → rate = 0.0.
No known-failure ledger entry counts as an exemption from this rate.

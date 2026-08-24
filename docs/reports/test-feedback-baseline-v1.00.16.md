# Test feedback baseline — v1.00.16

Deterministic baseline produced by `scripts/run-tests/baseline-report.rkt`
from retained inputs only. Same inputs → byte-identical outputs (verify with
`--check`).

**v1.00.16 halving objective (vs the W0 baseline of record):** fast-gate p50
(setup + max shard) ≤ 244.0 s, i.e. ≤ 50% of the W0 baseline p50 488.0 s
of record (see `fast-gate-budget-v1.00.11.md` and the v1.00.16
regression-log entry). The target is fixed at 50% of the W0 value and is
never re-derived from this sample (no target massaging). Falsifiable: re-run
`baseline-report.rkt --check` against the next retained sample
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

- **L3** [run 32745843124](https://github.com/coinerd/q/actions/runs/32745843124) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 14 / fail 0 / skip 3)
- **L3** [run 32748197712](https://github.com/coinerd/q/actions/runs/32748197712) — maintainer-named main/PR test run — jobs retained: yes (17 jobs; pass 16 / fail 0 / skip 1)

## Per-suite / per-shard wall clock (p50 / p95, seconds)

| suite | shard | n | p50 (s) | p95 (s) | non-pass | sample runs |
|---|---|---|---|---|---|---|
| abstraction-audit | single | 2 | 15.0 | 15.0 | 0 | 32745843124, 32748197712 |
| gsd-governance | single | 2 | 110.5 | 110.5 | 1 | 32745843124, 32748197712 |
| lint | single | 2 | 27.0 | 27.0 | 0 | 32745843124, 32748197712 |
| release-dry-run | single | 2 | 221.0 | 221.0 | 0 | 32745843124, 32748197712 |
| release-readiness | single | 2 | 0.0 | 0.0 | 2 | 32745843124, 32748197712 |
| security | single | 2 | 362.5 | 362.5 | 0 | 32745843124, 32748197712 |
| shard-plan-report | single | 2 | 232.0 | 232.0 | 0 | 32745843124, 32748197712 |
| smoke (ubuntu-latest) | single | 2 | 54.5 | 54.5 | 0 | 32745843124, 32748197712 |
| test-aggregate | single | 2 | 0.0 | 0.0 | 0 | 32745843124, 32748197712 |
| test-cross-version | single | 2 | 392.0 | 392.0 | 1 | 32745843124, 32748197712 |
| test-platform | single | 2 | 272.0 | 272.0 | 0 | 32745843124, 32748197712 |
| test | shard 0/3 | 2 | 255.5 | 255.5 | 0 | 32745843124, 32748197712 |
| test | shard 1/3 | 2 | 252.0 | 252.0 | 0 | 32745843124, 32748197712 |
| test | shard 2/3 | 2 | 281.5 | 281.5 | 0 | 32745843124, 32748197712 |
| workflows-aggregate | single | 2 | 0.5 | 0.5 | 0 | 32745843124, 32748197712 |
| workflows | shard 0/2 | 2 | 326.5 | 326.5 | 0 | 32745843124, 32748197712 |
| workflows | shard 1/2 | 2 | 309.0 | 309.0 | 0 | 32745843124, 32748197712 |

## Fast-gate budget (setup + max shard, seconds)

Fast gate = `test` suite (3 shards, `--suite fast`). Per run: worst shard by
total (its setup + its execution). Setup includes checkout, Racket install,
q relink, and `raco setup`.

| run | shard | setup (s) | execution (s) | total (s) |
|---|---|---|---|---|
| 32745843124 | shard 2/3 | 343.0 | 276.0 | 619.0 |
| 32748197712 | shard 2/3 | 348.0 | 287.0 | 635.0 |

- sample: 2 fast-gate runs
- p50: 627.0 s; p95: 627.0 s
- **halving target:** fast-gate p50 ≤ 244.0 s (≤ 50% of baseline p50 488.0 s)
- **v1.00.16 vs W0 baseline:** achieved ratio 1.2848360655737705× of the W0 baseline p50 (488.0 s); halving verdict: **MISSED** (627.0 s ≤ 244.0 s)

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
- skipped job outcomes: 4
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
| L3 | retain the successful main/PR sample recorded here | input run set above (2 L3 runs) | measured |
| L4 | preserve the 2-run cold/warm control (32522576690, 32526868295) | retained regression log | measured (control preserved) |

## Parallel-only instability (measured rate)

Measured strictly from retained artifacts: 0 non-pass (excluding skip) job outcomes across 34 job samples → rate = 0.0.
No known-failure ledger entry counts as an exemption from this rate.

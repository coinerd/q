# Fast-gate time budget — v1.00.11

Baseline of record companion to `docs/reports/test-feedback-baseline-v1.00.11.md`, produced by `baseline-report.rkt --fast-budget` from
retained inputs only (same inputs → byte-identical outputs; verify with
`--fast-budget --check`).

**Halving target (v1.00.16 objective):** fast-gate p50 (setup + max shard) ≤ 244.0 s — ≤ 50% of baseline p50 488.0 s. Falsifiable against the next retained sample.

## Per-run setup vs execution split (worst fast-gate shard)

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
- halving target: p50 ≤ 244.0 s

## Top-15 slowest fast-gate files by p50

_not available in this retained sample: per-file runner JSON artifacts are an
authenticated download and were not retained; never fabricated._
Categories are derived only from retained fields; `sleep-or-poll` and
`fixture-I/O` need per-file phase instrumentation that is not retained and
are never guessed.

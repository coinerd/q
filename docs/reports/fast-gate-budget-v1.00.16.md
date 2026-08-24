# Fast-gate time budget — v1.00.16

Baseline of record companion to `docs/reports/test-feedback-baseline-v1.00.16.md`, produced by `baseline-report.rkt --fast-budget` from
retained inputs only (same inputs → byte-identical outputs; verify with
`--fast-budget --check`).

**Halving target (v1.00.16 objective):** fast-gate p50 (setup + max shard) ≤ 244.0 s — ≤ 50% of baseline p50 488.0 s. Falsifiable against the next retained sample.

## Per-run setup vs execution split (worst fast-gate shard)

| run | shard | setup (s) | execution (s) | total (s) |
|---|---|---|---|---|
| 32745843124 | shard 2/3 | 343.0 | 276.0 | 619.0 |
| 32748197712 | shard 2/3 | 348.0 | 287.0 | 635.0 |

- sample: 2 fast-gate runs
- p50: 627.0 s; p95: 627.0 s
- halving target: p50 ≤ 244.0 s

## Top-15 slowest fast-gate files by p50

_not available in this retained sample: per-file runner JSON artifacts are an
authenticated download and were not retained; never fabricated._
Categories are derived only from retained fields; `sleep-or-poll` and
`fixture-I/O` need per-file phase instrumentation that is not retained and
are never guessed.

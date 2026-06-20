# Benchmark Gate Policy — v0.99.37

**Date:** 2026-06-22
**Wave:** W2 (#8442) — P0 Priority

---

## Problem

Three strict wall-clock timing assertions in `tests/test-bench-streaming-render.rkt`
caused false release blockers under scheduler noise (the #8435 pattern):

| Line | Assertion | Risk |
|------|-----------|------|
| 101 | `(check-true (<= batch-time (+ (* 2 percell-time) 100)))` | Fails when per-cell rendering is faster than batched under CPU contention |
| 127 | `(check-true (< per-frame 5.0))` | Fails on slow CI machines (4 vCPU) when scheduling delays push render time past 5ms |
| 146 | `(check-true (< diff-time 100.0))` | Fails on slow machines when 10K cell diff takes >100ms under load |

These assertions conflate **performance characteristics** with **correctness**, 
making the broad test gate fragile and non-deterministic.

---

## Policy

### Principle: Correctness gates test OUTPUT, not TIME

Benchmark tests running inside the broad correctness suite must:
1. **Assert output correctness** — the operation produces valid, non-empty results.
2. **Report timing as advisory** — wall-clock measurements are printed for 
   human inspection but never cause test failure.
3. **Separate strict performance gates** — micro-benchmark thresholds belong in
   a dedicated benchmark runner (`benchmarks/`), not the correctness suite.

### Applied Pattern

```racket
;; BEFORE (fragile — fails under scheduler noise):
(define elapsed (measure ...))
(check-true (< elapsed 100.0) "should be fast")

;; AFTER (robust — tests correctness, reports timing):
(define elapsed (measure ...))
(printf "Advisory: elapsed=~a ms~n" (real->decimal-string elapsed 2))
(define result (compute ...))
(check-true (valid? result) "should produce valid output")
```

---

## Changes Applied

### tests/test-bench-streaming-render.rkt

**Benchmark 1** (batch vs per-cell rendering):
- Removed: `(check-true (<= batch-time (+ (* 2 percell-time) 100)))`
- Added: Advisory `printf` with ratio. Existing output-correctness checks retained.

**Benchmark 2** (VDOM pipeline overhead):
- Removed: `(check-true (< per-frame 5.0))`
- Added: Advisory `printf` with per-frame timing. Buffer-population check added.

**Benchmark 3** (cell-diff throughput):
- Removed: `(check-true (< diff-time 100.0))`
- Added: Advisory `printf` with diff timing. Delta-production check added.

### tests/test-benchmarks.rkt

No timing assertions found — already compliant.

---

## Verification

```
$ raco test tests/test-bench-streaming-render.rkt
Batched: 1634.82 ms, Per-cell: 2421.02 ms
Speedup: 32.5%
Advisory: batch=1634.82 ms, per-cell=2421.02 ms, ratio=0.68
VDOM pipeline: 95.42 ms total, 1.91 ms/frame
Advisory: per-frame=1.91 ms
Cell-diff 10K cells: 4.74 ms
Advisory: diff-time=4.74 ms for 10K cells
3 tests passed
```

All tests pass deterministically. Timing data is still visible for human inspection.

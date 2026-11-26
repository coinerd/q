# CL5 P2-D — concurrency-aware timeout policy: measurement and applied limits

Status: **applied.** Three `@timeout` declarations changed, each on the owner-approved formula and each
visible in a committed registry that a test pins to the declarations. Nothing else changed: no test
body, no fixture, no production code, no CI definition. Fail-closed behaviour is untouched (a timeout
is still `incomplete`, an incomplete producer still derives `degraded`, and a degraded record still
cannot support a GO).

## Policy (as approved)

`T = max(T_existing, ceil_30s(1.5 × p95_n))`, where `p95_n` is the 95th percentile wall time measured
**at the measurement's own job count** (`n = 4`), not standalone. Existing declarations are floors:
nothing is lowered. The registry carries an explicit allow-list so the policy cannot spread.

## Measurement method

Each target was executed in a **real surface invocation**: the target plus three fast-lane fillers
(`tests/test-gsd-delivery-merge.rkt`, `tests/test-gsd-auto-review-loop-contract.rkt`,
`tests/test-gsd-delivery-status.rkt`), `--jobs 4`, censoring disabled, one recorded warm-up followed by
five samples. The per-file `elapsed_ms` comes from the run's own `--json-out` artifact — the
production adapter's number, not a shell timer.

One substitution, disclosed: the evidence-repair file **declares** `@timeout`, and the adapter honours
a declared timeout over `--timeout`, so measuring it uncensored required a byte-identical probe copy
with only the declaration raised. The samples below are that copy's, and the registry entry records
the substitution.

## Results

| File | Existing limit | Standalone p95 (P2-C) | **p95 at `--jobs 4`** | Samples (ms) | Derived limit | Margin |
|---|---|---|---|---|---|---|
| `tests/test-gsd-executor-ergonomics-prompt.rkt` | 120 s (default) | ~71 s | **215 357 ms** | 215 357 / 206 331 / 204 706 / 207 432 / 209 433 | **330 s** | 1.53× |
| `tests/test-gsd-evidence-repair-production-contract.rkt` | 210 s | ~202 s | **350 756 ms** | 335 654 / 350 756 / 337 712 / 339 936 / 342 990 | **540 s** | 1.54× |
| `tests/test-gsd-branch-delivery-verification.rkt` | 120 s (default) | — | **166 636 ms** | 166 636 / 164 171 / 161 857 / 165 152 / 155 190 | **270 s** | 1.62× |

Warm-ups (recorded, not samples): 211 832 ms, 147 696 ms and 160 074 ms.

**Concurrency inflates all three files by ~1.4–3×.** Standalone timings were never a sound basis for
these limits: the ergonomics file is 71 s alone and 215 s alongside three real tests, the evidence-repair
file is ~200 s alone and ~350 s in the same conditions, and the branch-delivery file is ~120 s in a
451-file shard and ~167 s beside three heavy tests.

**About the two kinds of sample.** The probe (`target + 3 fast-lane fillers`) is a deliberately *worst
case*: the target always has three heavy neighbours, which a random slot in a 451-file shard does not
guarantee. It therefore yields a larger p95 than the shard passes (167 s vs 118–120 s for the branch
file) — conservative for a limit, but it is a **different surface** and is labelled as such rather than
presented as the shard's own number.

### The third file: found by the baseline itself

The P2-E baseline passes produced the case for it. `tests/test-gsd-branch-delivery-verification.rkt`
has no declared timeout, so it inherits 120 s, and its elapsed time under the measured surface was
**117 790 ms, 119 825 ms, then a timeout at 120 089 ms** across the three passes — 98–100 % of its
limit, the same signature that motivated the policy. It was measured at `--jobs 4` with the same
protocol (warm-up 160 074 ms, samples as above) and gets 270 s.

This is the policy working as intended: the guard refused the registry merge until the allow-list was
extended deliberately, so the third file could not slip in unnoticed.

## Applied changes (the exact diff)

```diff
 tests/test-gsd-executor-ergonomics-prompt.rkt
-;; @speed fast  ;; @suite extensions
+;; @speed fast  ;; @suite extensions
+;; @timeout 330  ;; CL5 P2-D: concurrency-calibrated (p95 215s at --jobs 4; see docs/reports/CL5-P2D-TIMEOUT-MARGIN.json)

 tests/test-gsd-evidence-repair-production-contract.rkt
-;; @timeout 210
+;; @timeout 540 ;; CL5 P2-D: concurrency-calibrated (p95 351s at --jobs 4; see docs/reports/CL5-P2D-TIMEOUT-MARGIN.json)

 tests/test-gsd-branch-delivery-verification.rkt
+#lang racket/base
+
+;; @timeout 270 ;; CL5 P2-D: concurrency-calibrated (p95 167s at --jobs 4; see docs/reports/CL5-P2D-TIMEOUT-MARGIN.json)
```

Registry: `docs/reports/CL5-P2D-TIMEOUT-MARGIN.json` (`cl5-slow-tests/1`), verified by
`racket scripts/cl5/timeout-margin.rkt --verify` and pinned to the declarations by
`tests/test-cl5-timeout-margin.rkt` — that test fails if a declaration and its evidence drift apart,
and it also fails if the allow-list grows beyond the three measured files. `--verify` additionally
refuses a registry whose allow-list differs from the module's authoritative list, and re-derives each
margin from the numbers, so a registry cannot authorise itself to cover a fourth file or declare a
margin its own p95 does not support.

## What this does and does not fix

- **Fixes:** all three files can now finish under the measured surface. The comprehensive record can
  become `ok` instead of `degraded`, which is the precondition for P2-E.

**Not yet a baseline.** These are *limits derived from measurements*, not a comprehensive cost
measurement. Each p95 is the maximum of five samples, so it is a **lower bound** on the true p95, and
the derived limits inherit that uncertainty. The P2-E baseline must be re-measured on the remediated
surface (warm-up + two paired passes + the CI-equivalent single-schedule runs for `C_comp` wall) before
any number from it is used.
- **Does not fix:** the wall time itself. A shard containing these files can now spend up to 330 s,
  540 s or 270 s on one file in the worst case instead of failing at 120 s or 210 s. The typical case is
  unchanged (the file takes what it takes); only the tail bound moves.
- **Deliberately not done:** no timeout was raised anywhere else, no cap was changed (7 200 s per C
  producer and 14 400 s for L remain), no test body was touched, and no CI definition changed.

## Note for the owner's expectation

The owner's memo called a 300 s limit for the evidence-repair file "reasonable, subject to concurrent
measurements". The concurrent measurement says **540 s**: the file needs ~351 s at `--jobs 4`, and
1.5 × 351 = 527 s rounds up to 540 s. The gap is not policy drift — it is the concurrency factor that
P2-C identified. If 540 s is unacceptable, the alternatives are (a) accept a smaller margin for this
one file (e.g. 420 s ≈ 1.2×), which violates the approved formula, or (b) treat the record as
`degraded` and stop here. Both are decisions for the owner; neither is taken here.

## Reproduce

```bash
# per-file, at the surface's job count, uncensored, with a recorded warm-up
racket scripts/cl5/timeout-margin.rkt \
  --measure tests/test-gsd-executor-ergonomics-prompt.rkt --existing-limit 120 \
  --fillers tests/test-gsd-delivery-merge.rkt,tests/test-gsd-auto-review-loop-contract.rkt,tests/test-gsd-delivery-status.rkt \
  --job-count 4 --repeats 5 --timeout 900

racket scripts/cl5/timeout-margin.rkt --verify docs/reports/CL5-P2D-TIMEOUT-MARGIN.json
raco test tests/test-cl5-timeout-margin.rkt
```

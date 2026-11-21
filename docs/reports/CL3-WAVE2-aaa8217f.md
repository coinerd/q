# CL3 Wave 2 — module-load and source-change workflow comparison (#9980 / #9911)

Evaluation-only artifact at revision `aaa8217f65bc93fc54a582e14738f456a8d6d3e1`. Raw samples:
[`CL3-WAVE2-aaa8217f.json`](CL3-WAVE2-aaa8217f.json). No agent-facing Racket tool, permission policy,
test-runner change or CI-policy change was made; CL4's contract-driven pilot is untouched.

This revision incorporates the independent-review methodology corrections: the injected-function
scope is stated explicitly, the reset-dependent approach is renamed, trials are interleaved with a
recorded rotation, and compilation/resource measurements carry narrower definitions.

## What this benchmark actually measures

Three real q modules are copied into a disposable git worktree and given an injected
`cl3-probe-value` binding returning a fixed integer; the driver edits that integer and checks the
returned value against an independent expected value.

| Task | Module | Injected value before → after |
| --- | --- | --- |
| Small pure utility | `util/hash-utils.rkt` | 41 → 42 |
| Typed dependency chain | `runtime/iteration/retry-policy.rkt` | 7 → 8 |
| GSD decision module | `extensions/gsd/campaign-decision.rkt` | 3 → 4 |

**Scope limit.** This measures module-load and source-change overhead in representative q
dependency environments. It does **not** exercise the existing retry-policy, campaign-decision or
hash-utility logic, and it is not evidence about end-to-end debugging of real component behaviour.
A separate Wave 3 comparison uses real logic changes.

## Approches, ordering and cache policy

- `one-shot` — two `racket -e` processes (baseline and post-edit), each loading the module fresh.
- `persistent-namespace-reset` — one worker process: load → evaluate → edit → reload → stale
  evaluation → namespace reset → load → correct evaluation. The name reflects what it takes to get
  a correct post-edit value; ordinary reload alone is insufficient.
- `process-replacement` — baseline evaluation in one worker, post-edit evaluation in a second fresh
  worker (clean process boundary).

Trials are **interleaved**: each of the n iterations rotates the approach order, and the realised
order is recorded per trial (`trial.order`). This prevents OS/Racket cache warmth from
systematically favouring one approach. `compiled/` for the target module is cleared once per task
before the reported compilation block; the rest of the tree's compiled artifacts are left in place.
Workers are tracked and killed in a `dynamic-wind`, so cleanup runs even if an assertion fails.

Quantiles use CL0-compatible linear interpolation at rank `q*(n-1)`, rounded to 0.001 ms.
Memory is the maximum `(current-memory-use 'cumulative)` seen during a workflow: an allocation
proxy, **not** retained heap or OS peak RSS. Process count is direct Racket launches; process-tree
ownership and descendants are not verified.

## Full-workflow latency (ms, n = 10 per cell, interleaved)

| Task | Approach | p50 | p90 | max | Processes | All correct |
| --- | --- | ---: | ---: | ---: | ---: | --- |
| small pure | one-shot | 2225.6 | 2442.3 | 2551.7 | 2 | yes |
| small pure | persistent + namespace reset | **848.9** | 883.5 | 902.4 | 1 | yes |
| small pure | process replacement | 1466.7 | 1627.4 | 1644.8 | 2 | yes |
| typed chain | one-shot | 7182.0 | 7592.9 | 7661.0 | 2 | yes |
| typed chain | persistent + namespace reset | **5359.1** | 5516.6 | 5522.3 | 1 | yes |
| typed chain | process replacement | 7001.5 | 7433.7 | 7902.3 | 2 | yes |
| GSD decision | one-shot | 8336.1 | 8586.5 | 8846.1 | 2 | yes |
| GSD decision | persistent + namespace reset | **6843.4** | 7072.6 | 7367.2 | 1 | yes |
| GSD decision | process replacement | 7421.6 | 7786.9 | 8383.9 | 2 | yes |

Relative to one-shot p50: namespace reset −61.9% / −25.4% / −17.9%; clean process replacement
−34.1% / −2.5% / −11.0%. The clean approach's advantage is therefore modest on the larger
components, while the large reset-path gain depends on retaining one process.

## Compilation, memory and persistent state

| Task | Cold target `raco make` (ms) | Warm target `raco make` (ms) | One-shot memory p50 | Reset-path memory p50 | Replacement memory p50 |
| --- | ---: | ---: | ---: | ---: | ---: |
| small pure | 970.0 | 708.2 | 348,890,472 | 265,625,152 | 232,789,168 |
| typed chain | 13,025.6 | 943.4 | 1,251,789,776 | 1,946,766,376 | 1,196,006,288 |
| GSD decision | 4,420.8 | 698.4 | 1,527,168,256 | 2,622,200,304 | 1,424,082,512 |

- "Cold" means the target module's own `compiled/` directory was deleted; dependency `.zo` files
  shared with the rest of the tree were **not** cleared, so this is not a fully cold
  dependency-chain build. Warm runs reuse all compiled artifacts.
- The reset path retained the pre-edit value in all 30 trials before the reset; the correct value
  appeared only after reset plus reload. Process replacement was correct in all 30 trials.
- The reset path's allocation proxy grew to 1.56× (typed) and 1.72× (GSD) of one-shot.
- Compilation-cache effects are large, which is why an optimized one-shot baseline is the correct
  comparison for any production decision.

## Decision: REVISE (experiment), and a lower production priority

All 90 workflows returned the oracle-checked correct value; no error response or stale value was
counted as success. The benchmark is accepted as an **exploratory timing artifact** covering
module-load and source-change overhead, not as definitive comparative evidence for q debugging.

The Wave 1 process-tree-cleanup limitation remains: only the direct child was killed and reaped,
and descendants were not enumerated or proven cleaned up. That stays an explicit blocker for any
future production implementation.

Process-tree cleanup and permission integration are therefore **not** the next priority: the
evidence does not yet show that production promotion is worthwhile. The next step is one bounded
comparison of optimized one-shot execution (warm compilation cache) against a clean persistent
worker, using changes to real component logic. If the worker cannot beat that optimized baseline
substantially, CL3 should close with REJECT for production integration, keep these artifacts, and
leave #9911 as a non-priority proposal. CL0 fresh-context cognitive-load trials remain assigned to
CL5 (#9982).

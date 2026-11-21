# CL3 Wave 3 — optimized one-shot vs clean worker on real logic (#9980 / #9911)

Evaluation-only artifact at revision `df6beace` (Wave 2 corrections). Raw samples:
[`CL3-WAVE3-df6beace.json`](CL3-WAVE3-df6beace.json). No agent-facing Racket tool, permission
policy, test-runner change or CI-policy change was made.

## Question

Wave 2 measured module-load and source-change overhead with an injected probe. Wave 3 asks the
narrower production question: **does a clean worker beat optimized one-shot execution when the edit
changes real component logic?**

Each task edits actual behaviour and checks the module's real public export, with expected values
declared in the fixture (not derived from the implementation):

| Task | Module | Real edit | Public check | Before → after |
| --- | --- | --- | --- | --- |
| `hash-ref* default` | `util/hash-utils.rkt` | default `#f` → `'absent` | `(hash-ref* (hash 'a 1) '(b))` | `#f` → `absent` |
| `count-occurrences filters non-symbols` | `runtime/iteration/retry-policy.rkt` | skip non-symbol items (Typed Racket) | `(hash-count (count-occurrences '(a b 1 2)))` | `4` → `2` |
| `route-code for the no-route case` | `extensions/gsd/campaign-decision.rkt` | `'campaign-action` → `'campaign-noop` | `(route-code 'none)` | `campaign-action` → `campaign-noop` |

## Approaches

| Approach | Measured path |
| --- | --- |
| `one-shot` | edit → `racket -e` (loads and compiles on demand) |
| `one-shot-warm` | edit → incremental `raco make` → `racket -e` |
| `clean-worker` | edit → fresh worker `load` + `eval` (killed afterwards) |
| `clean-worker-warm` | edit → incremental `raco make` → fresh worker `load` + `eval` |

Measured total is exactly **edit → compile (where applicable) → correct post-edit value**. A
pre-edit baseline evaluation verifies the fixture but is excluded from the total. The target
module's `.zo` is deleted before each baseline and each post-edit evaluation so on-demand
compilation cannot reuse a stale artifact; dependency `.zo` files stay warm, which is the normal
incremental development state. Trials are interleaved with a recorded per-iteration rotation, and
workers are tracked and killed in a `dynamic-wind`.

## Results (ms, n = 10 per cell, all 120 workflows oracle-correct)

| Task | Approach | p50 | p90 | max | vs `one-shot` |
| --- | --- | ---: | ---: | ---: | ---: |
| hash-ref* | one-shot | 1102.7 | 1157.0 | 1252.0 | — |
| hash-ref* | one-shot-warm | 1873.6 | 1916.8 | 1933.7 | +69.9% |
| hash-ref* | **clean-worker** | **711.1** | 850.7 | 874.2 | **−35.5%** |
| hash-ref* | clean-worker-warm | 1549.4 | 1621.7 | 1659.9 | +40.5% |
| count-occurrences | one-shot | 3648.7 | 3993.2 | 4003.0 | — |
| count-occurrences | one-shot-warm | 5155.3 | 5504.8 | 6159.6 | +41.3% |
| count-occurrences | **clean-worker** | **3505.2** | 3748.6 | 3855.1 | **−3.9%** |
| count-occurrences | clean-worker-warm | 5035.8 | 5164.7 | 5348.6 | +38.0% |
| route-code | one-shot | 1334.1 | 1544.0 | 1677.8 | — |
| route-code | one-shot-warm | 2230.8 | 2420.4 | 2425.3 | +67.2% |
| route-code | **clean-worker** | **1062.8** | 1129.8 | 1133.1 | **−20.3%** |
| route-code | clean-worker-warm | 1822.0 | 1963.9 | 2059.1 | +36.6% |

Cold target-module `raco make` was 824.2 / 13,331.5 / 4,599.2 ms; the incremental `raco make`
inside `-warm` still cost 814.3 / 3792.7 / 1064.7 ms, which is why explicit pre-compilation loses.

## What this means

1. **Explicit `raco make` before evaluation is counter-productive.** It duplicates compilation the
   load step performs anyway and adds a process, making every `-warm` approach 37–70% slower.
   The "optimized one-shot baseline" in practice *is* plain `racket -e` with warm dependency
   caches.
2. **The clean worker's only advantage is cheaper startup.** It is a *fresh* process each time, so
   no stateful benefit is being measured: 711 vs 1103 ms, 3505 vs 3649 ms, 1063 vs 1334 ms.
   That is −35.5%, −3.9% and −20.3%, i.e. negligible on the Typed Racket chain.
3. **Persistence contributes nothing here.** Wave 2's large gains came from reusing one process
   across reloads, and every one of those 30 reload trials returned a stale value that required a
   namespace reset. Once correctness demands clean state, the worker advantage collapses to
   launcher overhead.
4. Real-edit costs are dominated by recompiling the edited module, which both approaches pay.

## Decision: REJECT production integration of a persistent evaluation worker

- The evidence does not show a substantial, reliable advantage over the optimized one-shot
  baseline (−3.9% on the Typed Racket chain), and the remaining gap is startup overhead rather than
  state continuity.
- Wave 1/2 blockers remain unresolved and are not worth financing: process-tree cleanup is
  unverified, namespace reset is not a process boundary, and arbitrary evaluation has no
  permission model.
- Preserve the evaluation artifacts (Wave 1 prototype, Wave 2 comparison, Wave 3 final comparison).
  Leave #9911 as a **non-priority proposal**; do not implement an agent-facing Racket tool, REPL,
  permission policy or test-scheduler change.
- A narrower, non-stateful hypothesis remains open for later consideration: reducing Racket
  interpreter startup cost for one-shot execution. That is not part of CL3 and requires its own
  scoped proposal.

CL0 fresh-context cognitive-load trials remain assigned to CL5 (#9982). CL4's contract-driven
architecture pilot is untouched.

# CL3 Wave 3 — optimized one-shot vs clean worker on real logic (#9980 / #9911)

Evaluation-only artifact at revision `5a5cd7b0` (Wave 2 merged). Raw samples:
[`CL3-WAVE3-5a5cd7b0.json`](CL3-WAVE3-5a5cd7b0.json). No agent-facing Racket tool, permission
policy, test-runner change or CI-policy change was made.

This revision applies the two reporting corrections from independent review: worker timing now
includes process teardown, and the compilation/persistence conclusions are narrowed to what the
measurements actually support.

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

## Approaches and measurement boundaries

| Approach | Measured path |
| --- | --- |
| `one-shot` | edit → `racket -e` (loads and compiles on demand) |
| `one-shot-warm` | edit → incremental `raco make` → `racket -e` |
| `clean-worker` | edit → fresh worker `load` + `eval`, then worker cleanup |
| `clean-worker-warm` | edit → incremental `raco make` → fresh worker `load` + `eval`, then cleanup |

Measured total is **edit → compile (where applicable) → correct post-edit value → process exit**.
Worker timing includes spawn, load, evaluation and teardown; one-shot timing already contains
process exit because the subprocess call does. A pre-edit baseline evaluation verifies the fixture
but is excluded from the total, so each workflow starts two direct Racket processes (baseline +
post-edit) even though only one appears on the measured path — recorded as
`measured-path-process-count` / `workflow-process-count`.

The target module's `.zo` is deleted before each baseline and each post-edit evaluation so
on-demand compilation cannot reuse a stale artifact; dependency `.zo` files stay warm, which is the
normal incremental development state. Trials are interleaved with a recorded per-iteration
rotation, and workers are tracked and killed in a `dynamic-wind` so cleanup also runs if an
assertion fails.

## Results (ms, n = 10 per cell, all 120 workflows oracle-correct)

| Task | Approach | p50 | p90 | max | vs `one-shot` |
| --- | --- | ---: | ---: | ---: | ---: |
| hash-ref* | one-shot | 1110.7 | 1181.0 | 1204.7 | — |
| hash-ref* | one-shot-warm | 1840.0 | 1952.9 | 1953.0 | +65.7% |
| hash-ref* | **clean-worker** | **733.1** | 793.6 | 815.6 | **−34.0%** |
| hash-ref* | clean-worker-warm | 1489.9 | 1585.1 | 1612.0 | +34.1% |
| count-occurrences | one-shot | 3462.8 | 3754.0 | 3891.0 | — |
| count-occurrences | one-shot-warm | 5052.3 | 5297.2 | 5478.1 | +45.9% |
| count-occurrences | **clean-worker** | **3440.1** | 3544.9 | 3586.3 | **−0.7%** |
| count-occurrences | clean-worker-warm | 4856.8 | 5343.2 | 5535.2 | +40.3% |
| route-code | one-shot | 1330.2 | 1417.3 | 1419.6 | — |
| route-code | one-shot-warm | 2127.8 | 2275.6 | 2280.4 | +60.0% |
| route-code | **clean-worker** | **1029.1** | 1043.8 | 1067.1 | **−22.6%** |
| route-code | clean-worker-warm | 1764.3 | 1882.3 | 1968.4 | +32.6% |

Cold target-module `raco make` was 874.3 / 12,999.8 / 4,671.8 ms. The incremental `raco make`
inside the `-warm` approaches still cost 791.1 / 3694.2 / 1000.1 ms, which is where their extra
latency comes from.

## What the measurements support

1. **In these workflows an explicit pre-compile step did not pay off.** `-warm` approaches were
   32.6–65.7% slower than their plain counterparts. The measurement covers a single edited target
   module with warm dependency `.zo` files; it does not show that compilation is duplicated in
   general, and it does not establish that precompilation is universally counterproductive.
2. **A clean worker is faster, mostly through cheaper startup.** It was −34.0%, −0.7% and −22.6%
   versus plain one-shot; the gain is negligible on the expensive Typed Racket dependency chain.
   Because every post-edit evaluation uses a fresh process, this measures launch overhead, not
   state continuity.
3. **The tested persistent-state strategy is not usable for reliable post-edit evaluation.** In
   Wave 2 every reload-only trial returned a stale value, and the correct value required a
   namespace reset plus reload. These experiments do not rule out benefits from persistence for
   repeated *read-only* evaluation; they show that the reload-based strategy is unsound once the
   source changes.
4. Real-edit cost is dominated by recompiling the edited module, which both approaches pay.

## Decision: REJECT production integration of a persistent evaluation worker

- No substantial, reliable advantage over optimized one-shot execution: −0.7% on the Typed Racket
  chain, and the remaining gap is launcher overhead rather than state continuity.
- Wave 1/2 blockers are unresolved: process-tree cleanup is unverified, namespace reset is not a
  process boundary, and arbitrary evaluation has no permission model.
- Preserve the evaluation artifacts (Wave 1 prototype, Wave 2 comparison, Wave 3 final
  comparison). Leave #9911 as a **non-priority proposal**; do not implement an agent-facing Racket
  tool, REPL, permission policy or test-scheduler change.
- A narrower, non-stateful hypothesis remains open outside CL3: reducing Racket interpreter startup
  cost for one-shot execution.

CL3 stops here; no Wave 4. CL0 fresh-context cognitive-load trials remain assigned to CL5 (#9982),
and CL4's contract-driven architecture pilot is untouched.

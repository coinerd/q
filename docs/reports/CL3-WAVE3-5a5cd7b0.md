# CL3 Wave 3 — optimized one-shot vs clean worker on real logic (#9980 / #9911)

Evaluation-only artifact at revision `5a5cd7b0` (Wave 2 merged). Raw samples:
[`CL3-WAVE3-5a5cd7b0.json`](CL3-WAVE3-5a5cd7b0.json). No agent-facing Racket tool, permission
policy, test-runner change or CI-policy change was made.

This revision applies the independent-review reporting corrections: worker timing now includes
process teardown, per-approach process counts include the explicit `raco make` launch, the
compilation/persistence conclusions are narrowed to what the measurements support, and each
`-warm` approach is compared with its own non-warm counterpart rather than only with `one-shot`.

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
but is excluded from the total. Counts are direct process launches, not verified process trees:

| Approach | Measured path | Whole workflow |
| --- | ---: | ---: |
| `one-shot` | 1 | 2 |
| `one-shot-warm` | 2 (evaluation + `raco make`) | 3 |
| `clean-worker` | 1 | 2 |
| `clean-worker-warm` | 2 (evaluation + `raco make`) | 3 |

The target module's `.zo` is deleted before each baseline and each post-edit evaluation so
on-demand compilation cannot reuse a stale artifact; dependency `.zo` files stay warm, which is the
normal incremental development state. Trials are interleaved with a recorded per-iteration
rotation, and workers are tracked and killed in a `dynamic-wind` so cleanup also runs if an
assertion fails.

## Results (ms, n = 10 per cell, all 120 workflows oracle-correct)

| Task | Approach | p50 | p90 | max | vs `one-shot` | vs own non-warm |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| hash-ref* | one-shot | 1156.2 | 1207.1 | 1250.9 | — | — |
| hash-ref* | one-shot-warm | 1872.6 | 1969.5 | 2093.5 | +62.0% | +62.0% |
| hash-ref* | **clean-worker** | **731.3** | 774.9 | 891.9 | **−36.8%** | — |
| hash-ref* | clean-worker-warm | 1517.4 | 1566.3 | 1605.2 | +31.2% | +107.5% |
| count-occurrences | one-shot | 3559.2 | 3716.3 | 3732.4 | — | — |
| count-occurrences | one-shot-warm | 4966.9 | 5326.7 | 5598.1 | +39.6% | +39.6% |
| count-occurrences | **clean-worker** | **3325.7** | 3587.4 | 3689.0 | **−6.6%** | — |
| count-occurrences | clean-worker-warm | 4934.4 | 5108.2 | 5427.3 | +38.6% | +48.4% |
| route-code | one-shot | 1266.3 | 1340.2 | 1449.1 | — | — |
| route-code | one-shot-warm | 2100.8 | 2122.7 | 2146.8 | +65.9% | +65.9% |
| route-code | **clean-worker** | **970.8** | 1053.5 | 1117.3 | **−23.3%** | — |
| route-code | clean-worker-warm | 1743.8 | 2007.2 | 2010.7 | +37.7% | +79.6% |

Cold target-module `raco make` was 839.8 / 12,225.2 / 3,776.1 ms. The incremental `raco make`
inside the `-warm` approaches still cost 813.3 / 3619.0 / 969.5 ms, which is where their extra
latency comes from.

## What the measurements support

1. **In these workflows an explicit pre-compile step did not pay off.** Each `-warm` approach was
   **39.6–107.5% slower than its own non-warm counterpart** (and +31.2–65.9% versus `one-shot`).
   The measurement covers a single edited target module with warm dependency `.zo` files; it does
   not show that compilation is duplicated in general, and it does not establish that
   precompilation is universally counterproductive.
2. **A clean worker is faster, mostly through cheaper startup.** It was −36.8%, −6.6% and −23.3%
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

- No substantial, reliable advantage over optimized one-shot execution: −6.6% on the Typed Racket
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

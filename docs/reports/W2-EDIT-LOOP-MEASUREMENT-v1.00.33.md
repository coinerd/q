# W2 Edit-Loop Measurement (v1.00.33)

Review 7002474250 (milestone #900, wave W2) requires a matched, end-to-end
measurement of the everyday edit loop: focused runs, committed attributable
impact, uncommitted attributable impact, plus the deliberate broad fallback
and the admission-refresh cost reported separately. Every number below is a
recorded run, not a projection; misses are stated, not smoothed.

## Baseline setup

| Item | Value |
|---|---|
| Tree | worktree `wt-900-w2`, commit `bc520059` (W2 attribution commit: 20 of 28 quick owners carry `@covers`; covers manifest regenerated; admission record refreshed) |
| Machine | 4-vCPU KVM guest, 7.8 GiB RAM, Linux, Racket 8.10 [cs] |
| Warm definition | repository bytecode caches present (`raco make` equivalent already run); one discarded warm-up run before each sampled series; OS page cache hot |
| Runner | `./qt` (default `--jobs 2`, default per-file timeouts), the supported interface |
| Percentiles | nearest-rank over the listed samples; per-sample wall-clock around the whole command, subprocess spawn to exit |
| Reference commands | `python3` timing harness shelling out to `racket qt …` in the worktree root |

Sample counts are stated per row. Small-sample rows (2–3 samples) are
labeled as such and their p90 equals or approaches the max — read them as
"all observed samples", not as distribution estimates.

## CLI startup and admission re-verification

| What | n | p50 | p90 | max |
|---|---|---|---|---|
| `./qt --help` (pure CLI startup, no admission) | 10 | 0.95 s | 1.02 s | 1.04 s |
| `./qt quick tests/test-ansi.rkt --explain` (startup + admission re-verification of one owner, nothing executed) | 10 | 0.89 s | 0.94 s | 0.94 s |
| `./qt quick --changed HEAD --explain` (8-owner selection: manifest + dependency graph + admission) | 3 | 3.06 s | 3.07 s | 3.07 s |
| `./qt quick --audit` (full-table audit VIEW: scans all 1551 candidates) | 3 | 29.3 s | 31.0 s | 31.0 s |

Reading: the per-run admission gate is cheap — one selected owner is
re-verified inside ≈ 0.9 s end-to-end. The expensive admission number is the
`--audit` report view (≈ 30 s) because it classifies every discovered test
file; that is a review surface, not the run path. The `--changed` explain
floor is ≈ 3 s because the impact machinery builds the production dependency
graph on every call.

## Focused red-green (`./qt quick <file>`) — target warm p90 ≤ 3 s

21 samples, 7 per owner, rotating three representative small owners:

| Owner (area) | Tests | n | p50 | p90 | max | Target verdict |
|---|---|---|---|---|---|---|
| `tests/test-ansi.rkt` (TUI) | 23 | 7 | 1.96 s | 2.16 s | 2.16 s | ✅ ≤ 3 s |
| `tests/test-agent-state.rkt` (agent/runtime) | 7 | 7 | 1.84 s | 2.03 s | 2.03 s | ✅ ≤ 3 s |
| `tests/test-gsd-campaign-decision-pure.rkt` (GSD) | 6 | 7 | 4.95 s | 5.42 s | 5.42 s | ❌ miss |
| **Pooled** | — | 21 | 2.03 s | 4.96 s | 5.42 s | ❌ **missed** |

Honest miss cause: 17 of 21 samples sit at ≤ 2.2 s; the GSD owner misses
because its fresh child recompiles a `typed/racket` closure from source on
every run (`quick` children load repository sources only — the reviewed
source-only rule in `scripts/testing/quick-child.rkt`). The child-side
compile of that closure costs ≈ 3 s; CLI startup + admission add ≈ 1 s. The
miss is the owner's closure weight, not selection or admission overhead.

## Committed attributable impact (`./qt quick --changed BASE..HEAD`) — target p90 ≤ 15 s

Three representative single-module commits (comment-only edits, one per
area, made on a scratch branch of the same tree and removed after
measurement). Selected owners per diff via `--explain`:

| Sample commit | Changed file | Selected owners | n | p50 | p90 | Verdict |
|---|---|---|---|---|---|---|
| `70e8e9c1` (TUI) | `util/ansi.rkt` | `test-ansi` (direct) + 8 documented riders = 9 | 5 | 12.25 s | 12.53 s | ✅ ≤ 15 s |
| `f5418b54` (GSD) | `extensions/gsd/campaign-decision.rkt` | `test-gsd-campaign-decision-pure` (direct) + 8 riders = 9 | 5 | 13.30 s | 13.92 s | ✅ ≤ 15 s |
| `84def6fe` (agent) | `agent/state.rkt` | `test-agent-state` (direct) + `test-audit-v09945-w3-gsd` and `test-browser-close-contract` (transitive) + 8 riders = 11 | 5 | 24.19 s | 25.37 s | ❌ **missed** |
| Pooled | — | — | 15 | 13.30 s | 24.89 s | ❌ missed (driven by the agent row) |

Honest miss cause: `agent/state.rkt` is a dependency hub — the graph
correctly fans out through its dependents to owners attributed to them, so
the selection is 11 owners, not 1. Two of the pulled-in owners are heavy
under source-only child loading (per-owner warm end-to-end, each including
≈ 0.9 s CLI startup + admission):

| Pulled-in owner (agent sample) | Warm end-to-end |
|---|---|
| `tests/test-audit-v09945-w3-gsd.rkt` (requires 7 GSD modules incl. a `typed/racket` closure) | 12.9 s |
| `tests/test-arch-parameters.rkt` (recursive source scan) | 5.6 s |
| `tests/test-arch-tool-constructor.rkt` | 4.4 s |
| `tests/test-arch-boundaries.rkt` | 3.1 s |
| remaining 7 owners | 1.5–2.2 s each |

This fan-out is CORRECT conservative behavior (a hub change SHOULD re-run
everything that depends on it, and `--explain` names every inclusion with
its reason); the cost driver is child-side source recompilation, not the
selector.

## Uncommitted attributable impact (`./qt quick --changed HEAD`) — target p90 ≤ 15 s

Same three modules, edited in the working tree (comment-only), one area at a
time, refreshed between areas:

| Area | Changed file | Selected owners | n | p50 | p90 | Verdict |
|---|---|---|---|---|---|---|
| TUI | `util/ansi.rkt` (unstaged content edit) | 9 | 5 | 12.06 s | 12.51 s | ✅ ≤ 15 s |
| GSD | `extensions/gsd/campaign-decision.rkt` | 9 | 2 | 12.30 s | 13.11 s | ✅ ≤ 15 s |
| agent | `agent/state.rkt` | 12 | 2 | 24.60 s | 25.20 s | ❌ missed (same fan-out as the committed row, plus the area's own edit interacting in one tree) |
| Pooled | — | — | 9 | 12.38 s | 25.20 s | ❌ missed (driven by the agent row) |

The working-tree input itself adds no measurable cost over the committed
range: equal selections time the same (TUI committed p50 12.25 s vs
uncommitted p50 12.06 s). Untracked and staged files are seen by the same
diff (pinned by `tests/test-qt-quick-working-tree.rkt`); the measured edits
were unstaged, which is the common case.

## Admission refresh after an ordinary content edit

An edit to any file in an admitted owner's closure (the test file or a
module it requires) makes that owner review-required until refresh — this
is the fail-closed design, and it fired exactly as specified during this
measurement:

```text
$ racket qt quick --changed HEAD
qt: quick refuses tests/test-ansi.rkt: review-required (source changed since the record was generated (refresh required))
exit 1
```

The documented refresh path (existing `refresh-record!`, one invocation,
production record, 28 owners):

| What | n | p50 | p90 | max |
|---|---|---|---|---|
| `racket -e '(begin (require (file "scripts/testing/quick-audit.rkt")) (refresh-record!))'` | 5 | 1.53 s | 1.72 s | 1.72 s |

Refresh properties pinned by `tests/test-testing-audit.rkt` and observed on
the production tree during this wave: the refresh is a pure function of the
tree (re-running it on an unchanged tree reproduces the committed bytes
byte-identically — verified three times while restoring measurement edits),
a content-only edit moves exactly the edited file's hash (closure and
verdict unchanged), and a structure change or capability hit stays
review-required after a refresh. `quick` re-checks admission after the run,
so a mid-run source change cannot become a pass line.

## Deliberate broad fallback and complete loop (reported separately, as required)

| What | n | p50 | p90 | max |
|---|---|---|---|---|
| Whole audited lane after an escalation (`./qt quick`, 28 files / 325 tests) | 3 | 35.7 s | 36.1 s | 36.1 s |

An escalation (config change, runner-helper change, unmapped source, graph
parse failure) includes every owner; this table is what that fallback costs,
and it is the same number as the plain complete loop.

## Ordinary edit-to-test workflow (verified, in order)

1. Edit the test owner or one of its closure modules.
2. `./qt quick <file>` refuses with `review-required (source changed …
   (refresh required))` if a closure file changed — the fail-closed gate.
3. Refresh once: `racket -e '(begin (require (file
   "scripts/testing/quick-audit.rkt")) (refresh-record!))'` (≈ 1.5 s). The
   rewritten `tests/.quick-admission.rktd` is a reviewed committed diff and
   lands in the same commit as the edit.
4. `./qt quick <file>` — focused loop, ≈ 2 s for a small non-typed owner,
   ≈ 5 s for a `typed/racket`-closure owner (miss vs ≤ 3 s documented above).
5. `./qt quick --changed HEAD` before moving on (≈ 12–25 s depending on
   fan-out), `./qt quick --changed BASE..HEAD` for committed work, and
   `./qt default` before push.

## Honest residual misses (unchanged by this wave, named for the next one)

1. **Focused pooled p90 4.96 s vs ≤ 3 s.** Two of the three sampled areas
   meet the target; the GSD owner's `typed/racket` closure recompiles in
   every fresh child. Fixing it means either a lighter closure for that
   owner or a reviewed change to the source-only loading rule — both out of
   scope for W2.
2. **Agent-hub impact p90 24–25 s vs ≤ 15 s** (committed and uncommitted).
   The graph's conservative fan-out plus two heavy pulled-in owners. The
   selection is right; the child-side compile cost is the lever.
3. **Admission `--audit` view ≈ 30 s.** Review surface only; not on the
   everyday loop path.
4. **CLI startup ≈ 0.9 s** bounds every focused sample from below; the
   `qt` script eagerly loads the lane machinery (`full-plan.rkt`) that
   `quick` never uses, which is most of the gap between the ≈ 0.9 s floor
   and the ≈ 1.9 s minimum focused run. Shaving it is a startup change
   outside W2's stated file scope and was not done.

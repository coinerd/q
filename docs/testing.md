# Developer Testing Quick-Start

One-command feedback levels, using only runner flags the CLI actually
implements. Full rationale and measured budgets:
[`TDD-TEST-STRATEGY-PLAN.md`](TDD-TEST-STRATEGY-PLAN.md) (Target Feedback
Model) and [`reports/LOCAL-TDD-LATENCY-v1.00.28.md`](reports/LOCAL-TDD-LATENCY-v1.00.28.md) (v1.00.28).

## The L0–L3 loop (v1.00.28 W6)

| Level | When | Command | Measured p90 |
|---|---|---|---|
| **L0** — current behavior | Every red-green cycle, before widening scope | `racket scripts/run-tests.rkt path/to/test-file.rkt` | **2.0 s** (target ≤ 5 s ✅) |
| **L1** — direct impact | Before commit / before push | `racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD` | 52.7 s on the W6 sample file (miss attributed to the selected file's own runtime, not selection ≈ 2 s) |
| **L2** — transitive impact | Before push, risky cross-module edits | `racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD --impact-dry-run --explain` (selection preview; execution escalates to L3) | selection ≈ 8 s (target ≤ 120 s ✅) |
| **L3** — broad fast | Pre-PR sanity, or when L2 escalates | `racket scripts/run-tests.rkt --suite fast` | CI lane (fast-gate p50 627 s) |

## Rules of thumb

1. **Never start a red-green cycle with `--suite fast`.** Positional L0 on the
   file you are editing is a ~2 s loop; the broad fast suite is a ~10 minute
   lane. Widen only when L0 is green.
2. **Preview impact selection before running it.** Pair
   `--impact-dry-run` with `--explain`: with `--explain` the command is
   selection-only (exit 0 even on an empty diff). `--impact-dry-run` *without*
   `--explain` proceeds to execute the selection and refuses with exit 3 on an
   empty selection — a deliberate zero-test safety rail.
3. **Unmapped files escalate, never silently select zero tests.** If impact
   selection reports a fallback (e.g. `fast workflows`), read the `--explain`
   reason before trusting the run.
4. **L2 is a selection gate, not an execution tier, locally.** The transitive
   graph walk is fast; executing its full selection is what the L3/lane
   budget is for. Class E L2 execution cost is bounded by suite composition.

## Command details

- `--changed-base <ref> --changed-head <ref>` — diff-based selection between
  two git refs; also accepts a positional test file path alongside the flags.
- `--impact-dry-run` — restrict the run to the impact-selected set.
- `--explain` — print the selection/fallback reason tree; makes the command
  selection-only.
- `--suite <name>` — run a named suite (`fast`, `default`, `all`, …).

Machine-readable W6 evidence: `artifacts/test-runtime/v1.00.28-w6/` (SHA-256 checksummed), report `reports/LOCAL-TDD-LATENCY-v1.00.28.md` (v1.00.28).

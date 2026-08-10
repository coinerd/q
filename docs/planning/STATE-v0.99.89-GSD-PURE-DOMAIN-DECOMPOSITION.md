# State: v0.99.89 — GSD Pure Domain Decomposition

**Status:** ACTIVE — W0/W1/W2 DONE; W3 next
**Baseline SHA:** `598fbd00` (v0.99.88 release)
**Current main:** `74da7e8a` (W2 merge, PR #9256)
**Current wave:** W3 — Command Parsing & Intent Boundary (#9229)

| Wave | Issue | Status | Merge | Gates |
|---|---|---|---|---|
| W0 Golden Workflow Traces | #9226 | ✅ DONE (PR #9254, `8214e6a4`) | golden 16/16; workflows 29/161; Fast 1057/15420; lint-format 0/0 | APPROVED (1 recheck round) |
| W1 Pure Transition Kernel | #9227 | ✅ DONE (PR #9255, `eb7807ae`) | kernel 29/29; facade 123/123; golden 16/16; GSD batch 1038; Fast 1058; lint-format 0/0 | APPROVED (0 recheck; 2 MINOR + 5 INFO folded) |
| W2 Plan/State Projection Kernel | #9228 | ✅ DONE (PR #9256, `74da7e8a`) | kernel 22/22; golden 16/16 (crash-resume repair); GSD batch 1038; workflows ✅; Fast 1059; Broad 1237; lint-format 0/0 | APPROVED (0 recheck) |
| W3 Command Parsing & Intent Boundary | #9229 | backlog | — | parser fitness + command corpus + Fast |
| W4 Facade Thinning + Release | #9230 | backlog | — | Broad + Arch + Workflow + Smoke + Release + review |

## W0 deliverables

- `tests/helpers/gsd-golden-trace.rkt` — deterministic semantic trace
  harness (oracle). Production campaign machinery
  (`execute-campaign-request!` → `run-campaign!` → `try-complete-wave!`),
  nondeterministic fields excluded by construction, plan-id deterministic.
- `tests/workflows/gsd/test-gsd-golden-traces.rkt` — 16-test matrix
  (plan-creation, go-success, verifier-reject, failure, interruption,
  retry, resume, replan, milestone-close via production `/done` archive
  path, crash-between-commit-and-projection, crash-resume, determinism
  probes).
- `docs/reports/GSD-GOLDEN-TRACES-v0.99.89.md` — oracle docs + findings.

## Oracle findings (current behavior pins for W1–W4)

1. Interruption updates only the durable record — projections untouched.
2. Crash between commit and projection leaves stale projections; resume
   does not repair them (W2 scope; plan overall reflects stale projection).
3. `campaign-result` `completed` = waves completed in the current run.
4. Campaign path ends FSM in `verifying`; `/done` (milestone close) resets
   to fresh `idle` with cleared history.

## Projection consistency

Tracked scaffolds in `q/docs/planning/`; executor mirror `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.

## W1 deliverables

- `extensions/gsd/transition-kernel.rkt` — PURE transition kernel. Requires
  only `racket/match` + `racket/set` (purity enforced by require-scan
  fitness test). Neutral `gsd-transition-state (mode total-waves
  current-wave completed-waves)`; `GSD-STATES`/`TRANSITIONS`/
  `TRANSITIONS-FLAT`; new `GSD-TERMINAL-STATES (verifying idle)` +
  `terminal-state?`; `campaign-complete?` (pure `/done` precondition,
  coverage + count checked); `valid-transition?`/`valid-targets`/
  `find-transition-path`/`compute-next-state`/`check-transition-invariants`/
  `compute-next-pending-wave`/`transition-idempotent?`; ok/err results.
- `extensions/gsd/transition-logic.rkt` — thin facade
  `(all-from-out transition-kernel.rkt)` + runtime adapters
  `compute-next-gsm-state`/`check-state-invariants`. Executor-clearing
  policy + executor-presence invariant stay facade-only. Additive surface;
  no removals/redefinitions.
- `tests/test-transition-kernel.rkt` — 29 tests (tables, cross-product
  property sweep, idempotency, neutral invariants, terminal classification,
  purity fitness, facade↔kernel equivalence).
- Inventory: `transition-kernel.rkt` classified (transition-logic domain,
  no effects); count 26 → 27.
- `docs/reports/PURE-TRANSITION-KERNEL-v0.99.89.md` — wave report +
  W2+ open considerations (DEFERRED semantic gap vs archive
  `done-or-deferred?`; `campaign-complete?` wiring into `/done`).

## W1 equivalence

Golden traces 16/16 UNCHANGED; GSD surface batch 1038 tests; Fast 1058
files; lint-format 0/0. Production behavior provably identical — W1 is a
pure structural refactor.

## W2 scope preview

Plan/State Projection Kernel — pure complete projection plan + atomic
effect shell; crash cannot leave stale projections (oracle finding #2).
Gate: GSD governance/workflow + Broad.

## W2 deliverables

- `extensions/gsd/projection-kernel.rkt` — PURE projection kernel (only
  racket/base + racket/string). Neutral projection set data
  (PROJECTION-KINDS, status markers); pure transforms byte-identical to the
  legacy writers (plan-index / wave-doc header / state-table row);
  `project-wave-status-set` (complete per-wave plan) and
  `project-reconciliation-set` (full durable re-derivation, idempotent).
- `extensions/gsd/projection-effects.rkt` — atomic effect shell:
  temp-write-then-rename (no torn files), batch apply, path resolution,
  `apply-wave-status-projections!` (completion/failure entry),
  `reconcile-projections-from-waves!` (crash-repair entry).
- Wiring: `try-complete-wave!` DONE/FAILED + go-orchestrator error paths →
  single atomic shell call; `run-campaign!` reconciles stale projections at
  start (never blocks); `wave-docs.rkt` gains `wave-slug`/`plan-slug-map`.
- `tests/test-projection-kernel.rkt` — 22 tests incl. byte-equivalence vs
  legacy writers and crash-repair reconcile.
- Golden oracle: 15/16 byte-identical; crash-resume trace now asserts
  REPAIRED projections (`((0 done) (1 done))`, plan-overall `all-done`).
- Inventory: +2 modules (projection-kernel/effects, event-projection
  domain), count 29.
- `docs/reports/PLAN-STATE-PROJECTION-KERNEL-v0.99.89.md` — wave report.

## W2 acceptance recap

"Projections remain equivalent and a crash cannot leave stale partial
tracking." Equivalence: golden 15/16 byte-identical + GSD batch 1038.
Crash safety: per-file atomic (temp+rename) + set-level batch apply +
durable-record reconcile on resume (golden crash-resume repair pin).

## W3 scope preview

Command Parsing & Intent Boundary — pure command parsing + intent
classification boundary; parser fitness + command corpus + Fast gate.

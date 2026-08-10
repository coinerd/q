# Plan: v0.99.89 — GSD Pure Domain Decomposition

**Status:** ACTIVE — W0 DONE, W1 DONE (per wave-amendment entries below)
**Authority:** roadmap + v0.99.87 freeze contract
**Plan-ID / Hash:** generated at campaign start
**Dependency:** v0.99.88 released
**GitHub:** milestone #876; waves #9226–#9230
**Findings:** MA-06, MA-07 (intermediate closure)

## Goal

Isolate GSD planning/state logic as an explicit pure domain core while preserving `/go`, persistence formats, projections, event order, and public facades.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Golden Workflow Traces | Pin plan/go/success/failure/interruption/retry/replan/resume/close semantics | GSD workflow + Fast; deterministic semantic traces complete |
| W1 | Pure Transition Kernel | Pure transitions/preconditions/terminal states; facade delegates | pure-kernel + Arch + Fast; no FS/GitHub/Runtime/event-bus imports |
| W2 | Plan/State Projection Kernel | Pure complete projection plan + atomic effect shell | GSD governance/workflow + Broad; crash cannot leave stale projections |
| W3 | Command Parsing and Intent Boundary | I/O-free parser intent vs executor; preserve `/go N` assertion | parser fitness + command corpus + Fast |
| W4 | Facade Thinning and Release | Composition/re-export only; consumer-proven wrapper deletion | Broad + Arch + Workflow + Smoke + Release + review; Golden Traces equivalent |

**Broad gates:** W2 and W4. Fast + independent review every wave.

## Amendment policy

Immutable after start; changes require dated amendment, new hash, controlled migration, and preserved MA history.

## Amendments

- **2026-08-10 (W0):** Status FROZEN → ACTIVE — W0 DONE. Golden workflow
  traces merged (`8214e6a4`, PR #9254, #9226 closed): deterministic
  semantic trace matrix = refactoring oracle for W1–W4; report
  `docs/reports/GSD-GOLDEN-TRACES-v0.99.89.md`. No wave-map change.
- **2026-08-10 (W1):** W1 DONE — Pure Transition Kernel merged
  (#9227, PR pending): `extensions/gsd/transition-kernel.rkt` (pure,
  neutral domain data, base-collections only) + facade rewrite
  (`transition-logic.rkt` delegates via all-from-out; additive surface
  expansion — no removals/redefinitions). Golden traces 16/16 unchanged;
  GSD batch 1038 tests passed. Report
  `docs/reports/PURE-TRANSITION-KERNEL-v0.99.89.md`. No wave-map change.

## Amendments (tracked — dated)

### 2026-08-11 — W2: Plan/State Projection Kernel (additive surface expansion)
Delivered on `feature/v09989-w2-plan-state-projection-kernel` (PR pending):
new PURE `projection-kernel.rkt` (projection-set neutral data + pure
transforms byte-identical to the legacy writers) and atomic effect shell
`projection-effects.rkt` (temp+rename, batch apply, crash-repair
reconcile). Completion/failure paths (`try-complete-wave!`, go-orchestrator
error paths) now apply PLAN.md+wave-doc+STATE.md through the shell in one
batch; `run-campaign!` reconciles stale projections from the durable record
on resume. Golden traces 15/16 byte-identical; crash-resume trace now
asserts repaired projections (plan-overall `all-done`). GSD batch 1038;
Fast 1059 files; Broad 1237 files; lint-format 0/0. Report
`docs/reports/PLAN-STATE-PROJECTION-KERNEL-v0.99.89.md`. Inventory 26 → 29
(+projection-kernel/effects, event-projection domain). Wave-map unchanged
(no new state names / persistence formats).

### 2026-08-11 — W2 MERGED `74da7e8a` (PR #9256, #9228 closed)
W2 Plan/State Projection Kernel merged. Reviewer APPROVED (0 recheck). All
gates green (kernel 22/22, golden 16/16 with crash-resume repair pin, GSD
batch 1038, Fast 1059, Broad 1237, lint-format 0/0). Metrics re-synced
(post-add). Next: W3 Command Parsing & Intent Boundary (#9229).

### 2026-08-11 — W3: Command Parsing & Intent Boundary (additive surface expansion)
Delivered on `feature/v09989-w3-command-parsing-intent-boundary` (PR pending):
parser stays pure (no new deps); new intent classification
(`command-wave-intent` / `gsd-command-intent` / `go-wave-valid?`) in
command-parser.rkt; executor `requested-wave-index` delegates to
`command-wave-intent` (no re-parsing); /go N assertion semantics preserved
(assert-go-n untouched). New `tests/test-gsd-command-intent.rkt` (13 tests:
30-entry corpus, malformed pins, /go N cross-product equivalence, parser
fitness). Golden 16/16 unchanged; GSD batch 1087; Fast 1060; lint-format
0/0. Report `docs/reports/COMMAND-PARSING-INTENT-BOUNDARY-v0.99.89.md`.
Also carries the W2 post-merge review fold (`f3a6fc87`). Wave-map unchanged.

### 2026-08-11 — W3 MERGED `927c8024` (PR #9257, #9229 closed)
W3 Command Parsing & Intent Boundary merged. Reviewer APPROVED (0 recheck; 3
MINOR folded in `42adcdf4` + metrics sync `372311ef`). All gates green
(intent/corpus 13/13, golden 16/16, GSD batch 1087, Fast 1060, lint-format
0/0). Next: W4 GSD Facade Thinning + Release (#9230) — version bump
v0.99.89, tag, milestone #876 close.

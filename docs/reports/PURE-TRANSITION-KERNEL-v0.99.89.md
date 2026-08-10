# Pure Transition Kernel — v0.99.89 W1 Report

**Wave:** v0.99.89 W1 — Pure Transition Kernel (#9227, milestone #876)
**Branch:** `feature/v09989-w1-pure-transition-kernel` (base `8214e6a4`)
**Gate:** pure-kernel + Arch + Fast ✅

## Goal (roadmap)

Extract state transitions, preconditions and terminal states into a small
pure module with neutral GSD-domain data. No filesystem/GitHub/Runtime/
event-bus imports. Existing facade delegates; no state-name or persistence
changes. Property/table tests for allowed/forbidden transitions and
idempotency. Fitness test forbids I/O imports in the kernel.

## Delivered

### 1. New pure kernel: `extensions/gsd/transition-kernel.rkt`

- Requires ONLY `racket/match` + `racket/set` (base collections) — by
  construction and enforced by the purity fitness test.
- **Neutral domain data:** `gsd-transition-state (mode total-waves
  current-wave completed-waves)` — a plain projection of the runtime
  aggregate containing exactly the fields the transition logic reads.
  Deliberately excludes `wave-executor`, `plan-path`, `pinned-dir`,
  `edit-limit`, `transition-history` (runtime concerns owned by the facade).
- **Constants:** `GSD-STATES`, `TRANSITIONS`, `TRANSITIONS-FLAT` (moved),
  plus NEW `GSD-TERMINAL-STATES (verifying idle)` with `terminal-state?`
  and `campaign-complete?` (the pure `/done` precondition:
  `total-waves > 0 ∧ completed-waves = all`; agrees with
  `compute-next-pending-wave`).
- **Pure logic:** `valid-transition?`, `valid-targets`,
  `find-transition-path`, `compute-next-state` (neutral-domain version),
  `check-transition-invariants` (neutral preconditions),
  `compute-next-pending-wave`, `transition-idempotent?` (apply-twice
  stability guard), `ok-result`/`err-result` + wrappers.

### 2. Facade rewrite: `extensions/gsd/transition-logic.rkt`

- `(provide (all-from-out "transition-kernel.rkt"))` — the full kernel
  surface is re-exported verbatim; public API unchanged.
- `compute-next-gsm-state` adapts `gsd-runtime-state` → neutral → kernel
  `compute-next-state` → re-materialized runtime state; the runtime policy
  (wave-executor clearing when leaving executing mode) stays in the facade.
- `check-state-invariants` delegates to the kernel's neutral invariants,
  then applies the facade-only executor-presence rule.
- All consumers (`state-machine.rkt`, `responsibility-inventory.rkt`,
  `runtime/task-memory/gsd-adapter.rkt`, `gsd-planning.rkt`, all tests)
  import identical names — no call-site changes.

### 3. Tests: `tests/test-transition-kernel.rkt` (29 tests)

- **Table tests:** all 9 transitions + idle self-loop allowed; full
  forbidden cross-product rejected; event-gated transitions accept only
  their own event.
- **Property sweep:** over the GSD-STATES × GSD-STATES cross product —
  `valid-transition?` consistency with `TRANSITIONS-FLAT`, `valid-targets`
  completeness, `find-transition-path` validity (path ends at target, every
  hop is a valid transition) / `#f` for unreachable pairs.
- **Idempotency:** `compute-next-state` deterministic (identical result
  structs and next states on repeated calls); `transition-idempotent?`
  holds for every state/target pair, with and without events.
- **Neutral invariants:** valid/invalid mode, wave counters, completed set.
- **Terminal states:** classification table + `campaign-complete?` cases +
  agreement with `compute-next-pending-wave`.
- **Purity fitness:** `extract-requires` on the kernel file must yield only
  `racket/base`, `racket/match`, `racket/set` — any filesystem/GitHub/
  Runtime/event-bus import fails the gate.
- **Facade ↔ kernel equivalence:** `compute-next-gsm-state` mode parity and
  `check-state-invariants` subset relation on every state/target pair;
  facade retains runtime-only executor rules.

### 4. Inventory update

- `responsibility-inventory.rkt`: new `transition-kernel.rkt` entry
  (domain `transition-logic`, no effects, deps `racket/match racket/set`);
  `transition-logic.rkt` deps updated (`+ transition-kernel`).
- `test-gsd-responsibility-inventory.rkt`: GSD module count 26 → 27.

## Equivalence proof (W0 oracle)

The golden workflow traces (16/16) pass **unchanged** with the new
architecture. The oracle pins commands, FSM transitions, campaign record,
PLAN/STATE/VALIDATION/wave projections, completion outbox, campaign result
and event order — all byte-identical. Production behavior is provably
unchanged; W1 is a pure structural refactor.

## Gates

| Gate | Result |
|---|---|
| `tests/test-transition-kernel.rkt` | ✅ 29/29 |
| Legacy facade contract (`test-transition-logic.rkt`, `test-transition-matrix.rkt`, `test-gsd-transition-logic.rkt`) | ✅ unchanged, green |
| GSD surface batch (`test-gsd-*.rkt`, `test-transition-*.rkt`, golden traces) | ✅ 1038 tests passed |
| workflows suite | ✅ PASS |
| fast suite | ✅ 1058 files, PASS |
| lint-format | ✅ 0/0 |
| inventory (direct run) | ✅ 5/5 |

## Notes / lessons

1. **Stale compiled caches**: rewriting `transition-logic.rkt` caused
   `instantiate-linklet` mismatches in pre-compiled dependents
   (`gsd-planning.rkt`, `state-machine.rkt`). A full compiled-cache wipe
   (`find . -type d -name compiled -exec rm -rf {} +`) + recompile resolved
   it; `raco test` alone does not force dependent recompilation.
2. **rackunit `check-true` is strict**: only `#t` passes; truthy lists
   (e.g. `(member x xs)`) must be coerced (`(and (member ...) #t)`).
3. **`raco test -t` does not execute `module+ main`** — the
   responsibility-inventory test is latent-dead in CI (pre-existing
   weakness, kept truthful for direct runs; out of scope for W1).
4. `raco test` runs each file with `current-directory` = the test file's
   directory; cross-module file reads must use `q-dir` (from
   `tests/helpers/arch-utils.rkt`), not relative paths.

## Abstraction gate

- **What was introduced:** one new pure module (`transition-kernel.rkt`)
  with one new neutral struct (`gsd-transition-state`) and two new
  predicates (`terminal-state?`, `campaign-complete?`).
- **Gate criteria met:** names a real q domain concept (the GSD transition
  kernel, identified as the top responsibility in the v0.99.87 inventory);
  narrows a public API (facade is now delegation-only); simplifies tests
  (kernel exhaustively testable without runtime fixtures); reduces boundary
  errors (purity enforced by fitness test).
- **Alternatives considered:** keeping the logic in `transition-logic.rkt`
  and merely adding an import-scan test (rejected — the struct-level
  runtime coupling `gsd-runtime-state` stays); moving the kernel out of
  `extensions/gsd/` into a new `domain/` directory (rejected — path churn
  for every consumer, no immediate benefit; revisit in W4 facade thinning).

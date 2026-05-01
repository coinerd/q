# ADR-0011: GSD State Machine & Architecture

Date: 2025-01 (v0.25.2)
## Status
Accepted

## Context
The GSD (Getting Stuff Done) planning system managed wave state through ad-hoc flag checks and imperative mutations scattered across multiple modules. This made it difficult to reason about valid state transitions, debug wave execution failures, and add new features without regressions.

An architecture audit (v0.24.x) identified 8 findings (F1–F8) across state management, policy logic, observability, and documentation.

## Decision

### State Machine Core (v0.25.2 → v0.25.2)
Rewrote the GSD wave executor as an explicit state machine with defined states (idle, exploring, plan-written, executing, verifying) and validated transitions. States: idle → exploring → plan-written → executing → verifying → idle.

### v0.25.2 — State Unification & Command Contracts (F1, F2, F5)
- **Canonical runtime state**: `gsd-runtime-state` struct in `runtime-state-types.rkt` replaces hash-table state. All consumers use struct accessors.
- **Command result structs**: `gsd-command-result` with `gsd-ok`/`gsd-err` constructors replace ad-hoc hasheq responses.
- **Shim purification**: `gsd-planning-state.rkt` is now pure delegation (DEPRECATED, removal planned for v0.25.2).
- **Invariant checker**: `gsd-invariants-hold?` validates mode, wave bounds, completed set integrity.

### v0.25.2 — Policy Engine & Transaction Wrappers (F3, F4)
- **Unified policy module** (`extensions/gsd/policy.rkt`): `gsd-decide-action` centralizes all guard decisions. Inline `BLOCKED-TOOLS` table removed from state-machine.rkt.
- **Transaction wrappers** (`with-gsd-transaction` in core.rkt): snapshot+rollback semantics for multi-step commands like `cmd-wave-done`.

### v0.25.2 — Pipeline Refactor & Observability (F7, F6)
- **Normalized Plan IR** (`gsd-normalized-plan`/`gsd-normalized-wave` in plan-types.rkt): Immutable IR between parsing, validation, and execution. `normalize-plan` validates structure; `validate-normalized-plan` produces `gsd-validated-plan`.
- **Event telemetry** (`extensions/gsd/events.rkt`): Stable event names following `gsd.<category>.<action>` convention with correlation IDs. Command dispatch and state transitions emit events.

### v0.25.2 — Documentation & Fitness Gates (F8)
- Updated architecture documentation to reflect all v0.24.x changes.
- Cross-module fitness tests validate all architectural improvements work together.

### v0.25.2 — Production Integration Wiring
- **Unified event system**: Replaced local `emit-gsd-event!` in gsd-planning.rkt with delegation to events.rkt + session bus fallback. String event names converted to symbols. Bus bridge connects GSD event bus to session event bus.
- **Normalized pipeline for /go**: `handle-go-command` refactored to 3-step pipeline: `normalize-plan` → `validate-normalized-plan` → `make-wave-executor-from-validated`. Emits `gsd.plan.parsed`, `gsd.plan.normalized`, `gsd.plan.validated` events at each stage.
- **Archive result migration**: `archive-completed-plan!` returns `gsd-command-result` (gsd-ok/gsd-err) instead of hasheq. `cmd-done` simplified to direct delegation.
- **Transaction coverage**: `cmd-done` wrapped with `with-gsd-transaction` for atomic archive with rollback on failure.
- **Write guard policy migration**: `gsd-write-guard` returns `policy-decision` directly instead of hasheq/#t. Consumers use `policy-allowed?`/`policy-blocked?` predicates.
- **Event taxonomy expansion**: Added `gsd.mode.changed`, `gsd.plan.archived`, `gsd.archive.failed`, and pipeline events to stable taxonomy.

## Key Files
| Module | Responsibility |
|--------|---------------|
| `extensions/gsd/state-machine.rkt` | State transitions, tool permissions (via policy) |
| `extensions/gsd/runtime-state-types.rkt` | `gsd-runtime-state` struct definition |
| `extensions/gsd/command-types.rkt` | `gsd-command-result`, `gsd-ok`, `gsd-err` |
| `extensions/gsd/policy.rkt` | Unified policy decisions, `gsd-decide-action` |
| `extensions/gsd/events.rkt` | Event telemetry, `emit-gsd-event!` |
| `extensions/gsd/core.rkt` | Command dispatch, transaction wrappers |
| `extensions/gsd/plan-types.rkt` | Plan/wave types, normalized plan IR |
| `extensions/gsd/plan-validator.rkt` | Plan validation (raw + normalized) |
| `extensions/gsd/wave-executor.rkt` | Wave execution engine |
| `extensions/gsd-planning-state.rkt` | DEPRECATED shim (pure delegation) |

## Consequences
- All wave state transitions are explicit and validated
- Invalid transitions raise clear errors instead of silently corrupting state
- Policy decisions centralized in one module for consistency
- Transaction wrappers provide failure atomicity
- Event telemetry enables observability without coupling
- Normalized plan IR enables clean parse→validate→execute pipeline
- Test coverage: 221 GSD tests across policy, state-machine, core, planning, events, fitness

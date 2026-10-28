# ADR-0011: GSD State Machine & Architecture

Date: 2025-01 (v0.25.3)
## Status
Accepted

## Context
The GSD (Getting Stuff Done) planning system managed wave state through ad-hoc flag checks and imperative mutations scattered across multiple modules. This made it difficult to reason about valid state transitions, debug wave execution failures, and add new features without regressions.

An architecture audit (v0.24.x) identified 8 findings (F1–F8) across state management, policy logic, observability, and documentation.

## Decision

### State Machine Core (v0.25.3 → v0.25.3)
Rewrote the GSD wave executor as an explicit state machine with defined states (idle, exploring, plan-written, executing, verifying) and validated transitions. The planning/execution mode machine (`idle`, `exploring`, `plan-written`, `executing`, `verifying`, with `/replan` back to `exploring`) is separate from the campaign lifecycle: the single campaign-lifecycle diagram and the operator recovery table live in [the GSD user guide](../gsd-guide.md#campaign-lifecycle) and are not repeated here.

### v0.25.3 — State Unification & Command Contracts (F1, F2, F5)
- **Canonical runtime state**: `gsd-runtime-state` struct in `runtime-state-types.rkt` replaces hash-table state. All consumers use struct accessors.
- **Command result structs**: `gsd-command-result` with `gsd-ok`/`gsd-err` constructors replace ad-hoc hasheq responses.
- **Shim purification**: `gsd-planning-state.rkt` is now pure delegation (DEPRECATED, removal planned for v0.25.3).
- **Invariant checker**: `gsd-invariants-hold?` validates mode, wave bounds, completed set integrity.

### v0.25.3 — Policy Engine & Transaction Wrappers (F3, F4)
- **Unified policy module** (`extensions/gsd/policy.rkt`): `gsd-decide-action` centralizes all guard decisions. Inline `BLOCKED-TOOLS` table removed from state-machine.rkt.
- **Transaction wrappers** (`with-gsd-transaction` in core.rkt): snapshot+rollback semantics for multi-step commands like `cmd-wave-done`.

### v0.25.3 — Pipeline Refactor & Observability (F7, F6)
- **Normalized Plan IR** (`gsd-normalized-plan`/`gsd-normalized-wave` in plan-types.rkt): Immutable IR between parsing, validation, and execution. `normalize-plan` validates structure; `validate-normalized-plan` produces `gsd-validated-plan`.
- **Event telemetry** (`extensions/gsd/events.rkt`): Stable event names following `gsd.<category>.<action>` convention with correlation IDs. Command dispatch and state transitions emit events.

### v0.25.3 — Documentation & Fitness Gates (F8)
- Updated architecture documentation to reflect all v0.24.x changes.
- Cross-module fitness tests validate all architectural improvements work together.

### v0.25.3 — Production Integration Wiring
- **Unified event system**: Replaced local `emit-gsd-event!` in gsd-planning.rkt with delegation to events.rkt + session bus fallback. String event names converted to symbols. Bus bridge connects GSD event bus to session event bus.
- **Normalized pipeline for /go**: `handle-go-command` refactored to 3-step pipeline: `normalize-plan` → `validate-normalized-plan` → `make-wave-executor-from-validated`. Emits `gsd.plan.parsed`, `gsd.plan.normalized`, `gsd.plan.validated` events at each stage.
- **Archive result migration**: `archive-completed-plan!` returns `gsd-command-result` (gsd-ok/gsd-err) instead of hasheq. `cmd-done` simplified to direct delegation.
- **Transaction coverage**: `cmd-done` wrapped with `with-gsd-transaction` for atomic archive with rollback on failure.
- **Write guard policy migration**: `gsd-write-guard` returns `policy-decision` directly instead of hasheq/#t. Consumers use `policy-allowed?`/`policy-blocked?` predicates.
- **Event taxonomy expansion**: Added `gsd.mode.changed`, `gsd.plan.archived`, `gsd.archive.failed`, and pipeline events to stable taxonomy.

### W5 (v1.00.31) — one owner per decision

The simplification campaign removed parallel decision paths. Each contract now
has exactly one owner; the operator surface (one lifecycle diagram, one
recovery table, the precise Verify grammar with parser-tested examples) is
published in [the GSD user guide](../gsd-guide.md#campaign-lifecycle) and the
module responsibilities in [gsd-architecture.md](../gsd-architecture.md).

| Contract | Single owner | Removed duplication |
|----------|--------------|---------------------|
| Campaign routing | `campaign-decision.rkt` (pure) + `go-orchestrator.rkt` (the only interpreter) | ready/coordinator routing copies and message-string branching |
| Durable state | `campaign-state.rkt`, `campaign-repository.rkt` | status tables and ad-hoc writers elsewhere |
| Wave status | `wave-status.rkt` | duplicate display/durable/marker tables |
| Wave paths | `planning-paths.rkt` | three incompatible path regexps |
| Wave-doc headers/sections | `wave-docs.rkt` (`WAVE-DOC-*`) — the prompt template is generated from the same constants | hand-maintained template vs validator drift |
| Verify grammar + reasons | `verify-command-grammar.rkt` (via `verify-command-format.rkt`) | duplicate span/chain parsers and a second reason vocabulary |
| Plan preflight | `planning-artifact-validation.rkt` (standalone validator + `/go`) | duplicate validation logic |
| Scratch | `tools/scratch.rkt` — one trusted per-session root | separate prompt/bash/direct-write/worker roots |

Two consequences worth recording:

- **Unknown wave statuses are a typed rejection** (`exn:fail:wave-status`),
  not a silent `pending` fallback. Documented legacy aliases are still read;
  writers emit one canonical spelling; durable record schemas are unchanged.
- **Compatibility wrappers delegate only** and carry a dated removal marker
  (`TODO(#v1.00.40)` for `wave-docs.rkt`'s `wave-status-markers` and its
  internal `status->symbol` alias). No wrapper keeps an old decision table or
  safety exception.

Measured before/after values for this campaign (inventory entry counts, routing
authorities, module sizes and the honest LOC accounting) are recorded once in
[the architecture document](../gsd-architecture.md#simplification-campaign-v10031-measured-beforeafter);
this ADR does not restate them.

## Key Files
| Module | Responsibility |
|--------|---------------|
| `extensions/gsd/state-machine.rkt` | Mode transitions, tool permissions (via policy) |
| `extensions/gsd/runtime-state-types.rkt` | `gsd-runtime-state` struct definition |
| `extensions/gsd/command-types.rkt` | `gsd-command-result`, `gsd-ok`, `gsd-err` |
| `extensions/gsd/policy.rkt` | Unified policy decisions, `gsd-decide-action` |
| `extensions/gsd/events.rkt` | Event telemetry, `emit-gsd-event!` |
| `extensions/gsd/core.rkt` | Command dispatch, transaction wrappers |
| `extensions/gsd/plan-types.rkt` | Plan/wave types, normalized plan IR |
| `extensions/gsd/plan-validator.rkt` | Plan validation (raw + normalized) |
| `extensions/gsd/wave-executor.rkt` | Wave execution engine |
| `extensions/gsd/campaign-decision.rkt` | ONE pure campaign decision per `/go` pass |
| `extensions/gsd/go-orchestrator.rkt` | The one `/go` effect interpreter |
| `extensions/gsd/wave-status.rkt` | Wave-status vocabulary (canonical + legacy aliases) |
| `extensions/gsd/wave-docs.rkt` | Required wave-doc header/section contract + lint |
| `extensions/gsd/verify-command-grammar.rkt` | ONE Verify grammar and reason taxonomy |
| `extensions/gsd/planning-artifact-validation.rkt` | Shared plan preflight kernel |
| `tools/scratch.rkt` | One trusted per-session scratch root |
| `extensions/gsd-planning.rkt` | Extension entry point (registers commands/tools) |

## Consequences
- All wave state transitions are explicit and validated
- Invalid transitions raise clear errors instead of silently corrupting state
- Policy decisions centralized in one module for consistency
- Transaction wrappers provide failure atomicity
- Event telemetry enables observability without coupling
- Normalized plan IR enables clean parse→validate→execute pipeline
- One owner per contract (W5): status, path, Verify, preflight, decision, scratch
- Unknown wave statuses refuse by name instead of defaulting to `pending`
- Test coverage: 221 GSD tests across policy, state-machine, core, planning, events, fitness

# Typed Racket Migration Target Audit — v0.70.9

## Objective
Identify 2–3 leaf modules with stable structs and low macro complexity for Typed Racket pilot migration.

## Criteria
- **Leaf module**: few or no downstream dependents; not a facade/re-export module.
- **Stable API**: no recent churn; existing test coverage.
- **Low macro complexity**: no `define-typed-event`, `define-syntax`, or macro-heavy patterns.
- **Typed neighbor**: already has a `#lang typed/racket` dependency or adjacent typed module.

## Candidate Evaluation

| Module | Lang | Lines | Tests | Macros | Typed neighbor | Downstream dependents | Verdict |
|--------|------|-------|-------|--------|---------------|----------------------|---------|
| `util/event-types.rkt` | racket/base | 13 | None | None | `util/event.rkt` (typed) | `runtime/agent-session.rkt`, `runtime/session-lifecycle.rkt` | **REJECT** — too small (1 constant), low value |
| `util/event-access.rkt` | racket/base | 33 | `test-event-access.rkt` (6 cases) | None | `util/event.rkt` (typed) | `agent/loop-stream.rkt`, `runtime/session-events.rkt` | **ACCEPT** — thin wrapper, stable, tested, no macros |
| `agent/event-structs/stream-events.rkt` | racket/base | 62 | `test-event-structs-v2.rkt` | `define-typed-event` (heavy) | `base.rkt` (typed) | `agent/loop-stream.rkt` | **REJECT** — macro-heavy, churn risk |
| `util/event-payloads.rkt` | typed/racket | 120 | `test-event-payloads.rkt` | None | Already typed | Many | **ALREADY DONE** — reference pattern |
| `runtime/iteration/loop-state.rkt` | typed/racket | ~80 | `test-loop-state.rkt` | None | Already typed | `agent/loop.rkt` | **ALREADY DONE** — reference pattern |

## Selected Targets

### W1: `util/event-access.rkt`
- **Rationale**: Thin selector wrapper over typed `event.rkt`. Converting removes manual `racket/contract` boilerplate and lets TR boundary generate contracts automatically. Existing tests verify selectors post-conversion.
- **Risk**: Low. Module is pure functions, no I/O, no macros.
- **Compile-time impact**: Negligible (33 lines).

### W2 (conditional): `util/event-types.rkt` OR stop
- **Rationale**: 13-line constants module. Could be converted for completeness but minimal value. Decision gate: proceed only if W1 had zero issues.
- **Alternative**: Skip and document that constants modules don't benefit from typing.

## Blockers Identified
- None for `event-access.rkt`.
- General concern: converting facade modules (`util/protocol-types.rkt`, `agent/event-structs.rkt`) would cause wide contract churn. Explicitly excluded per plan risk decision.

## Reference Patterns
- `util/event.rkt` — struct + serialization in typed/racket
- `util/event-payloads.rkt` — payload structs in typed/racket
- `runtime/iteration/loop-state.rkt` — state struct in typed/racket

## W1 Results

- **Converted**: `util/event-access.rkt` → `#lang typed/racket`
- **Compile**: ✅ `raco make` passes
- **Tests**: ✅ `test-event-access.rkt` passes (6/6)
- **Downstream consumers**: ✅ `agent/event-types.rkt` compiles; `tests/test-stream-error-wrapping.rkt` passes (16/16)
- **Contract churn**: None — removed manual `contract-out`; TR boundary auto-generates contracts for untyped consumers.
- **Compile-time impact**: Negligible.

## W2 Decision: STOP

Per the v0.70.9 plan risk decision, the second target (`util/event-types.rkt`) is a 13-line constants module with negligible typing value. The next viable candidate (`util/tool-types.rkt`) is re-exported by facade `util/protocol-types.rkt`, which violates the "no facade typing" rule.

**Conclusion**: Pilot succeeded with one clean conversion. No blockers. Series stops here for v0.70.9 to maintain conservative risk posture. Future milestones may revisit additional leaf targets.


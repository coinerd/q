# State-Transition Boundary Report — v0.99.40 W4

**Date:** 2026-06-23
**Issue:** #8478
**Base:** `main@4801980f`

## Purpose

Manual §40–§41: state machines should have explicit transition functions, and event handlers should delegate to pure model transitions. This report inventories all state machines in the codebase, assesses test coverage, and documents the transition matrix tests added in W4.

## State Machine Inventory

### SM1: GSD Lifecycle FSM (`extensions/gsd/transition-logic.rkt`)

**States:** idle, exploring, plan-written, executing, verifying (5 states)
**Transitions:** 9 enriched ((from . event) . to) pairs
**Pure logic module:** `transition-logic.rkt` (extracted in v0.99.35 W5)
**Mutable shell:** `state-machine.rkt` (event-bus emission, session-state mutation, rework-loop protection)

**Transition Table:**
```
idle          --(explore)-->  exploring
exploring     --(plan)-->     plan-written
exploring     --(cancel)-->   idle
plan-written  --(execute)-->  executing
plan-written  --(cancel)-->   idle
executing     --(verify)-->   verifying
executing     --(cancel)-->   idle
verifying     --(done)-->     idle
verifying     --(rework)-->   executing
```

**Complete Validity Matrix (5×5):**
```
              idle  exploring  plan-written  executing  verifying
idle           ✓        ✓           ✗            ✗          ✗
exploring      ✓        ✗           ✓            ✗          ✗
plan-written   ✓        ✗           ✗            ✓          ✗
executing      ✓        ✗           ✗            ✗          ✓
verifying      ✓        ✗           ✗            ✓          ✗
```

**Test Coverage:**

| Test File | Checks | Scope |
|-----------|--------|-------|
| test-transition-logic.rkt | 112 | Individual transition cases, invariants, wave computation |
| test-state-machine-pure.rkt | 87 | Valid/invalid transitions, executor clearing, event dispatch |
| **test-transition-matrix.rkt (NEW)** | **75** | **Complete 5×5 matrix, all invariant branches, BFS reachability, event completeness** |
| test-gsd-state-machine.rkt | — | Integration tests with mutable session state |
| test-gsd-state-machine-ctx.rkt | — | Context-aware API integration tests |
| test-gsd-rework-limit.rkt | — | Rework-loop protection tests |

**Total pure transition test checks: 274**

### SM2: Agent Turn FSM (`agent/loop-fsm.rkt`)

**States:** emit-start, build-context, pre-hook, stream, post-hook, complete, blocked (7 states)
**Transitions:** 10 enriched ((state -> state) event) pairs
**Definition:** Uses `define-fsm-machine` macro from `util/fsm/fsm.rkt`

**Test Coverage:**

| Test File | Checks | Scope |
|-----------|--------|-------|
| test-loop-fsm.rkt | 42 | State transitions, state/event predicates, invalid transitions |

### SM3: Session Forward Transitions (`runtime/session/session-events.rkt`)

**Not a full FSM** — a simple predicate for major forward transitions:
```racket
(define major-forward-transitions
  '((exploration . planning) (planning . implementation) (implementation . verification)))
```

**Test Coverage (NEW in W4):** 8 checks in test-transition-matrix.rkt covering all 3 valid forward transitions + negative cases (backward, same-state, unrelated, non-symbol inputs).

### SM4: TUI State Events (`tui/state-events/`)

**Not a formal FSM** — event-reducer pattern with handler dispatch via registry. State transitions are implicit in handler return values. No explicit transition table.

**Test Coverage:** Various TUI test files cover individual handler behavior but no systematic transition matrix (appropriate since there's no formal FSM).

## W4 Deliverables

### 1. Complete Transition Matrix Tests (`tests/test-transition-matrix.rkt`)

75 new tests in 8 suites:

- **§1 Complete Transition Matrix (5 tests):** All 25 from→to pairs systematically verified. If any transition rule changes, exactly the right tests fail.
- **§1b compute-next-gsm-state Matrix (25 dynamic tests):** Generates test cases for all 5×5 pairs, verifying both result type and state side-effects.
- **§2 Invariant Branch Tests (12 tests):** All 7 invariant check branches exercised including edge cases (negative indices, zero waves, non-set completed-waves).
- **§3 BFS Reachability Matrix (12 tests):** Documents path lengths and reachability for all meaningful state pairs.
- **§4 Forward Transition Matrix (8 tests):** All 3 session major-forward-transitions + negative cases.
- **§5 Event Transition Completeness (3 tests):** Verifies all 9 event-matched transitions and wrong-event rejection.
- **§6 Executor Clearing Matrix (4 tests):** Side-effect verification for executor lifecycle across transitions.
- **§7 Valid-targets Completeness (6 tests):** All 5 states + unknown + total count verification.

### 2. Key Findings

**Finding F1: Transition table well-separated from mutable state (HEALTHY)**
The v0.99.35 extraction of `transition-logic.rkt` successfully separated pure transition computation from mutable session-state interactions. All transition predicates, BFS path-finding, and invariant checking are pure and directly testable.

**Finding F2: Cancel transitions are the escape hatch (DESIGN NOTE)**
Three states (exploring, plan-written, executing) have cancel→idle transitions. This is the rework/cleanup escape route. The matrix tests now explicitly document these.

**Finding F3: Rework cycle is bounded (v0.99.20 W1)**
The verifying→executing (rework) transition has a rework-loop protection counter in the mutable `state-machine.rkt` layer (not in the pure logic). This was correctly placed in the mutable layer since it requires session-level state tracking.

**Finding F4: Session forward-transitions predicate is pure but under-tested (NOW COVERED)**
`major-forward-transition?` was exported but had no direct unit tests. W4 adds 8 tests covering all valid and invalid cases.

## Assessment

| Criterion | Status |
|-----------|--------|
| Transition logic is pure | ✅ Extracted in v0.99.35 |
| All 25 cells tested | ✅ Complete matrix in W4 |
| All invariant branches tested | ✅ 12 tests in W4 |
| BFS reachability documented | ✅ All pairs tested |
| Event-matched transitions tested | ✅ All 9 with correct/wrong events |
| Mutable shell separated from pure logic | ✅ state-machine.rkt delegates to transition-logic.rkt |
| No production code changes needed | ✅ Tests only |

## Recommendations

1. **No production changes needed** — the pure/mutable separation is healthy.
2. **Future consideration:** If the TUI event-reducer grows more complex, consider formalizing it as an FSM with explicit transition table (currently appropriate as-is).
3. **Monitor:** The rework-loop counter in the mutable layer is a potential source of bugs if session context is shared incorrectly across threads — existing `test-gsd-rework-limit.rkt` covers this.

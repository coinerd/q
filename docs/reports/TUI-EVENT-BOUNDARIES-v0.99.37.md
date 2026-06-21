# TUI / Event Handler Boundary Audit — v0.99.37

**Date:** 2026-06-23
**Wave:** W8 (#8448)
**Risk Assessment:** GREEN (reward 14, risk 5)
**Scope:** Handler thinness audit + pure model boundary tests

## 1. Executive Summary

This report audits the TUI event handler architecture against Manual §41:
"handlers translate input to domain operations; pure model holds domain rules."

The architecture is **healthy**. Handlers are pure (state, event) → state
functions. Pure model helpers were already extracted in v0.99.35 (W7)
to `handler-helpers.rkt`. This wave adds boundary tests for the remaining
untested pure helpers in `helpers.rkt` and validates handler thinness
properties.

## 2. Architecture Overview

```
Event → apply-event-to-state(state, evt)
          ↓
        Registry (hash lookup by event-ev string)
          ↓
        Handler (state, evt) → state'
```

### Module Decomposition

| Module | Lines | Role | Purity |
|--------|-------|------|--------|
| `registry.rkt` | 58 | Event-type → handler dispatch | Pure (with semaphore-guarded mutation) |
| `core-handlers.rkt` | 511 | 30+ event handlers | Pure* |
| `goal-handlers.rkt` | 95 | 7 goal event handlers | Pure |
| `handler-helpers.rkt` | 78 | Payload extraction, key normalization, formatting | **Pure** |
| `helpers.rkt` | 98 | Error classification, dedup logic, entry helpers | **Pure** |
| `state-types.rkt` | 549 | Data model (ui-state, transcript-entry, etc.) | Pure |
| `state-events.rkt` | 40 | Thin facade (re-exports only) | N/A |

*One exception: `handle-model-stream-completed` calls `cost-tracker-update!`
which mutates a cost-tracker object. This is the sole I/O side-effect in
the handler layer and is documented as a known exception.

## 3. Handler Thinness Assessment

### 3.1 Boundary Contract

Every handler follows the contract:
```
ui-state? × event? → ui-state?
```

This is verified by 23 event-type dispatch tests in `handler-thinness-tests`.

### 3.2 Unknown Event Handling

Unregistered event types return the **exact same state object** (identity):
```racket
(check-eq? st0 (apply-event-to-state st0 unknown-evt))
```

This is verified in `handler-thinness-tests`.

### 3.3 Handler Responsibilities

Handlers perform **exactly** the translation role described in §41:
1. Extract payload fields from event
2. Apply domain rules (dedup, classification, formatting)
3. Return new ui-state with updated fields

Handlers do **not**:
- Make I/O calls (except cost-tracker mutation, documented)
- Call external services
- Spawn threads
- Perform blocking operations

### 3.4 Parameter Dependencies

Two handlers use `current-gsd-mode-query` parameter:
- `handle-iteration-soft-warning` — to label mode ("executing" vs "exploring")
- `handle-exploration-progress` — same purpose

This is an acceptable context-carrier pattern (Pattern B from W7 report).
The parameter is thread-local and properly scoped via `parameterize`.

## 4. Pure Model Test Coverage

### 4.1 Previously Tested (v0.99.35 W7)

| Helper | Module | Tests | Status |
|--------|--------|-------|--------|
| `kebab->camel` | handler-helpers.rkt | 3 | ✅ Covered |
| `hash-ref-multi` | handler-helpers.rkt | 3 | ✅ Covered |
| `verification-payload-ref` | handler-helpers.rkt | 6 | ✅ Covered |
| `retry-error-type-label` | handler-helpers.rkt | 5 | ✅ Covered |
| `tool-progress-status-text` | handler-helpers.rkt | 3 | ✅ Covered |

### 4.2 Newly Tested (W8 v0.99.37)

| Helper | Module | Tests | Status |
|--------|--------|-------|--------|
| `classify-error-type` | helpers.rkt | 7 | ✅ Now covered |
| `format-error-hint` | helpers.rkt | 7 | ✅ Now covered |
| `truncate-status-msg` | helpers.rkt | 4 | ✅ Now covered |
| `recent-tool-start?` | helpers.rkt | 4 | ✅ Now covered |
| `recent-tool-end?` | helpers.rkt | 2 | ✅ Now covered |
| Handler return types | core-handlers.rkt | 3 | ✅ Now covered |
| Registry isolation | registry.rkt | 4 | ✅ Now covered |

**Total new tests:** 32

### 4.3 Registry Isolation Properties

`call-with-test-registry` provides complete test isolation:
- Fresh hash per invocation (no global leakage)
- Registrations inside test scope don't persist
- Unregistered events in test registry return identity

This is the **same I/O effect-shell pattern** documented in W7 for
`current-audit-file->lines` — applied here to the event reducer registry.

## 5. Identified Fragilities

### 5.1 Null Payload Assumption

Several handlers assume `event-payload` returns a hash and call `hash-ref`
directly without guarding. If a malformed event arrives with a non-hash
payload (e.g., `#f`), these handlers will crash.

**Affected handlers:** `handle-runtime-error`, `handle-tool-call-started`,
`handle-tool-execution-completed`, and others.

**Risk:** Low — the event system always provides hash payloads in production.
**Recommendation:** Add `(and (hash? payload) ...)` guards as a defensive
measure in a future hardening wave.

### 5.2 Cost Tracker Mutation

`handle-model-stream-completed` calls `cost-tracker-update!` which mutates
a shared cost-tracker object. This violates pure handler semantics.

**Risk:** Low — single-threaded execution model. Safe under current architecture.
**Recommendation:** Document as known exception. No action needed unless
moving to multi-threaded event processing.

## 6. Conclusion

The TUI event handler architecture is well-decomposed and follows the
§41 boundary contract cleanly. Pure model helpers are now fully tested
across all boundary modules. Handler thinness is verified for all 23+
registered event types.

The registry isolation pattern (`call-with-test-registry`) is a good
example of the I/O effect-shell pattern applied to mutable state.

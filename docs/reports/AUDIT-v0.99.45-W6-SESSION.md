# Audit: Session Subsystem — v0.99.45 W6

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w6-session.rkt`
**Tests:** 66 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `runtime/session/lifecycle-state.rkt` — Mutable lifecycle state struct (9 fields)
  - `runtime/session/session-types.rkt` — agent-session struct (16 fields), facets, lifecycle accessors
  - `runtime/session/session-fsm.rkt` — Derived session lifecycle FSM (6 states, 8 events)
  - `runtime/session/session-config.rkt` — Typed config wrapper (gen:dict), 27+ smart accessors
  - `runtime/session/session-controls.rkt` — Model switching, thinking levels, shutdown
  - `runtime/session/session-mutation.rkt` — Invariant-guarded mutations (W-04 guards)
  - `runtime/session/session-lifecycle-transitions.rkt` — Pure message builders
  - `runtime/session/session-store-integrity.rkt` — Hash chain, write-ahead markers
  - `runtime/session/session-context.rkt` — Context path settings extraction
  - `runtime/session/session-metadata.rkt` — Session naming, entry labeling
- RED module skipped: `runtime/session/session-lifecycle.rkt` (large, I/O-heavy)
- Special flags: None

## Test Matrix

| Area | Tests | Status |
|------|-------|--------|
| Lifecycle State (defaults, mutations, transparency) | 5 | PASS |
| Session Types (fields, lifecycle accessors, paths, facets) | 4 | PASS |
| Session FSM (states, derived state, transitions) | 6 | PASS |
| Session Config (create, accessors, defaults, gen:dict, round-trip) | 9 | PASS |
| Session Controls (thinking levels, budgets, model, shutdown) | 6 | PASS |
| Session Mutation Guards (invariants, phases, validation) | 8 | PASS |
| Session Lifecycle Transitions (message building, parent ID) | 6 | PASS |
| Session Store Integrity (hash chain, canonical JSON, markers) | 9 | PASS |
| Session Context (path settings extraction) | 3 | PASS |
| Session Metadata (label types, name/label predicates) | 4 | PASS |
| **Total** | **66** | **ALL PASS** |

## Findings

### FINDING-001 (low): Lifecycle state setters not directly exported

**Severity:** Low
**Category:** API Design
**Description:** The `lifecycle-state.rkt` module exports `struct-out lifecycle-state` which includes all field accessors and setters. However, `session-types.rkt` imports the setters via `only-in` and places them in the `internal` submodule, making them inaccessible to external consumers without explicitly importing `(submod session-types internal)`.

The intended pattern is: use `session-mutation.rkt` guarded setters (`guarded-set-*!`) instead of raw setters. This is a defensive design choice, not a bug.

**Impact:** Low — well-encapsulated mutation access through guarded wrappers.

### FINDING-002 (info): FSM-derived state is robust and well-designed

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The session FSM overlay is elegantly designed:
- **6 states**: created → active → streaming → active → compacting → active → idle → terminated
- **Derived state**: Computed from boolean flags (active?, compacting?, prompt-running?) without requiring a separate FSM state variable
- **Transition validation**: `session-can-transition?` validates event acceptance from current state
- **Backward compatible**: Boolean flags remain the backing store; FSM is a read-only overlay

**Impact:** Positive — provides formal state machine semantics without breaking 203+ existing callsites.

### FINDING-003 (info): Mutation guards enforce strong invariants

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The `session-mutation.rkt` module provides invariant-guarded wrappers:
- **prompt-running?**: Rejects `#t→#t` transitions (prevents double-start)
- **compacting?**: Rejects `#t→#t` transitions (prevents recursive compaction)
- **persisted?**: Only allows `#f→#t` transition (prevents un-marking persisted)
- **shutdown-requested?**: Idempotent (`#t→#t` allowed for safety)
- **task-fsm-state**: Validates FSM transitions against allowed transition table
- **recent-tool-calls**: Validates list of symbols/strings

**Impact:** Positive — prevents invalid state transitions at runtime.

### FINDING-004 (info): Hash chain integrity is robust

**Severity:** Info (positive finding)
**Category:** Security/Integrity
**Description:** The session store integrity system provides:
- **SHA-256 hash chain**: Each entry's hash includes the previous entry's hash
- **Canonical JSON**: Deterministic serialization (sorted keys, consistent formatting)
- **Write-ahead markers**: Crash recovery via `.pending` files
- **Verification**: `verify-hash-chain` checks all `prev_hash` links
- **Repair**: `repair-session-log!` can rebuild broken chains
- **Genesis hash**: Starting point for chain validation

**Impact:** Positive — provides tamper detection and crash recovery for session logs.

### FINDING-005 (info): Session config has comprehensive typed accessors

**Severity:** Info (positive finding)
**Category:** API Design
**Description:** The `session-config` struct:
- **gen:dict implementation**: Full dictionary protocol support (dict-ref, dict-set, dict-has-key?, etc.)
- **27+ smart accessors**: Each config field has a typed accessor with sensible defaults
- **String→symbol coercion**: `normalize-session-config-hash` auto-converts thinking-level strings to symbols
- **Profile system**: 5-level graduated activation (off, observe, bounded, self-healing, full)
- **Memory config**: Auto-extraction config with min-confidence validation

**Impact:** Positive — eliminates mutable hash anti-pattern, provides type-safe config access.

## Architecture Summary

The Session subsystem is organized in layers:

1. **Data layer** (`session-types.rkt`, `lifecycle-state.rkt`):
   - `agent-session`: 16-field struct (2 identity + 5 runtime services + 8 session data + 1 lifecycle)
   - `lifecycle-state`: 9 mutable lifecycle fields (compacting, shutdown, prompt-running, task-fsm, etc.)
   - Facets: provider/tool/identity views for reduced coupling

2. **State management** (`session-fsm.rkt`, `session-mutation.rkt`):
   - Derived FSM overlay (6 states, 8 events)
   - Guarded mutations with invariant enforcement
   - Phase computation from boolean flags

3. **Configuration** (`session-config.rkt`):
   - Typed wrapper around immutable hash
   - gen:dict protocol for transparent access
   - Graduated feature activation profiles

4. **Persistence** (`session-store.rkt`, `session-store-integrity.rkt`, `session-migration.rkt`):
   - Append-only JSONL storage
   - SHA-256 hash chain for tamper detection
   - Write-ahead markers for crash recovery
   - Version migration chain (v1→v2)

5. **Lifecycle** (`session-lifecycle-transitions.rkt`, `session-controls.rkt`, `session-switch.rkt`):
   - Pure message builders (user message, system instructions)
   - Model/thinking-level controls
   - Atomic session switching (teardown → rebind → emit)
   - Graceful/forced shutdown

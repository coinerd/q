# W1: agent-session.rkt Decomposition

**Version:** 0.22.9
**Dependencies:** W0 complete
**Effort:** L (~2-3 hours)
**Delivery:** Remote pi + local review
**Findings:** EXT-01 (P0-1)

## Context

`runtime/agent-session.rkt` is 795 lines combining lifecycle, persistence, event wiring, model control, thinking levels, and shutdown handling. The external audit (P0-1) identified this as the highest-priority decomposition target.

## Current Module Map

```
runtime/agent-session.rkt (795 lines)
├── Struct definition (agent-session struct + 14 mutable fields)
├── Persistence: ensure-persisted!, buffer-or-append!, session-index-path
├── Lifecycle: make-agent-session, resume-agent-session, close-session!
├── Accessors: session-id, session-history, session-active?
├── Fork: fork-session, fork-session-internal
├── Context: build-session-context
├── Iteration: dispatch-iteration, run-prompt!, run-prompt-internal
├── Model control: set-model!, cycle-model!
├── Thinking levels: set-thinking-level!, thinking-level accessors
├── Shutdown: request-shutdown!, force-shutdown!, reset-shutdown-flags!
└── Event wiring: wire-session-event-handlers!
```

## Target Architecture

```
runtime/agent-session.rkt         → façade (~120 lines)
runtime/session-lifecycle.rkt     → new (~250 lines)
runtime/session-persistence.rkt   → new (~180 lines)
runtime/session-events-bridge.rkt → new (~120 lines)
```

Note: `runtime/session-types.rkt` already exists (extracted in v0.22.8) and contains the struct definition.

## Tasks

### T1: Create runtime/session-lifecycle.rkt

**Extract from agent-session.rkt:**
- `make-agent-session` (lines 204–280)
- `resume-agent-session` (lines 282–365)
- `close-session!` (lines 767+)
- `fork-session`, `fork-session-internal` (lines 366–483)
- `build-session-context` (lines 484–570)
- `dispatch-iteration` (lines 572–638)
- `run-prompt!`, `run-prompt-internal` (lines 640–758)
- `session-id`, `session-history`, `session-active?` (lines 711–766)

**Imports:** session-store, compactor, iteration, context-builder, provider-factory, event-bus, extension-catalog, session-switch, token-budget, settings, model-registry, safe-mode, hooks, session-types

**Provides:** All 16 functions above + `maybe-compact-context`

### T2: Create runtime/session-persistence.rkt

**Extract from agent-session.rkt:**
- `ensure-persisted!` (line 159)
- `buffer-or-append!` (line 176)
- `session-index-path` (line 181)

**Imports:** session-store, session-index, session-types

### T3: Create runtime/session-events-bridge.rkt

**Extract from agent-session.rkt:**
- `wire-session-event-handlers!`
- `set-model!`, `cycle-model!`
- `set-thinking-level!`, thinking level accessors
- `request-shutdown!`, `force-shutdown!`, `reset-shutdown-flags!`
- Shutdown predicates: `shutdown-requested?`, `force-shutdown-requested?`

**Imports:** event-bus, session-store, hooks, session-types

### T4: Convert agent-session.rkt to façade

Replace all definitions with:
```racket
(require "session-lifecycle.rkt"
         "session-persistence.rkt"
         "session-events-bridge.rkt"
         "session-types.rkt")

(provide (all-from-out "session-lifecycle.rkt")
         (all-from-out "session-persistence.rkt")
         (all-from-out "session-events-bridge.rkt")
         (all-from-out "session-types.rkt"))
```

**Critical:** Verify ALL 44 original exported symbols are available. Compare `provide` lists before and after.

### T5: Verify

```bash
raco make runtime/agent-session.rkt
raco test tests/test-agent-session.rkt
raco test tests/test-iteration.rkt
raco test tests/test-sdk-gsd-live.rkt
racket scripts/run-tests.rkt --suite fast
```

**No test modifications needed** — façade preserves all symbols.

## Expected Module Sizes

| Module | Lines | Notes |
|--------|-------|-------|
| agent-session.rkt (façade) | ~120 | Re-exports only |
| session-lifecycle.rkt | ~250 | Core session operations |
| session-persistence.rkt | ~180 | Write buffering + flush |
| session-events-bridge.rkt | ~120 | Event wiring + controls |

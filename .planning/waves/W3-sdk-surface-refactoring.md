# W3: SDK Surface Refactoring

**Version:** 0.22.9
**Dependencies:** W2 complete
**Effort:** M (~1-2 hours)
**Delivery:** Remote pi + local review
**Findings:** EXT-03 (P1-3)

## Context

`interfaces/sdk.rkt` is 763 lines containing two distinct sections: a stable core API (struct definitions, lifecycle operations, contracts) and a compat/convenience layer (`q:` aliases, GSD convenience wrappers). The external audit recommends splitting into `sdk-core` and `sdk-compat` to separate stable API from evolving convenience surface.

## Current Module Map

```
interfaces/sdk.rkt (763 lines)
├── Structs: runtime-config, runtime (lines 190–230)
├── Cancellation token re-exports
├── Core API contracts (lines 72–170): contract-out for all core operations
├── Core API impl (lines 223–548): make-runtime, open-session, run-prompt!, etc.
├── Factory: create-agent-session (lines 552–588)
├── q: aliases (lines 590–675): q:create-session through q:session-tree-info
├── GSD convenience (lines 681–765): gsd-status, q:plan, q:go, q:gsd-status, q:reset-gsd!
```

## Target Architecture

```
interfaces/sdk.rkt          → façade (~100 lines)
interfaces/sdk-core.rkt     → new (~350 lines) — stable API surface
interfaces/sdk-compat.rkt   → new (~300 lines) — aliases + GSD convenience
interfaces/sdk-public.rkt   → unchanged (229 lines)
```

## Tasks

### T1: Create interfaces/sdk-core.rkt

**Extract from sdk.rkt:**
- Struct definitions: `runtime-config`, `runtime` (with `make-runtime-internal` constructor)
- Cancellation token imports + re-exports
- Core API with contracts: `make-runtime`, `open-session`, `run-prompt!`, `subscribe-events!`, `interrupt!`, `fork-session!`, `compact-session!`, `session-info`, `steer!`, `follow-up!`, `navigate!`
- Factory: `create-agent-session`
- Type re-exports: `compaction-result?`, `navigate-result?`
- Thinking level re-exports from agent-session
- Context usage re-exports from token-budget
- In-memory session manager re-exports

**This is the stable public API.** All symbols here should have `STABILITY: stable`.

### T2: Create interfaces/sdk-compat.rkt

**Extract from sdk.rkt:**
- `q:` aliases: `q:create-session`, `q:session-send`, `q:session-subscribe`, `q:session-interrupt`, `q:session-fork`, `q:session-compact`, `q:session-info`, `q:session-branch`, `q:session-navigate`, `q:session-tree-info`
- `dispatch-command!`
- GSD convenience: `gsd-status`, `q:plan`, `q:go`, `q:gsd-status`, `q:reset-gsd!`

**Imports from sdk-core.rkt:** `runtime`, `open-session`, `fork-session!`, etc.

### T3: Convert sdk.rkt to façade

```racket
#lang racket/base
;; interfaces/sdk.rkt — q SDK (façade)
;; STABILITY: stable

(require "sdk-core.rkt"
         "sdk-compat.rkt")

(provide (all-from-out "sdk-core.rkt")
         (all-from-out "sdk-compat.rkt"))
```

### T4: Verify SDK consumers unaffected

**Key test files:**
- `tests/test-integration.rkt` — uses `make-runtime`, `run-prompt!`
- `tests/test-sdk-gsd-live.rkt` — uses `q:plan`, `q:go`
- `tests/test-self-hosting-workflow.rkt` — uses `create-agent-session`
- `scripts/sdk-gsd-integration-test.rkt` — uses `q:plan`, `q:go`

```bash
raco make interfaces/sdk.rkt
raco test tests/test-integration.rkt
raco test tests/test-sdk-gsd-live.rkt
racket scripts/run-tests.rkt --suite fast
```

## Expected Module Sizes

| Module | Lines | Notes |
|--------|-------|-------|
| sdk.rkt (façade) | ~100 | Re-exports only |
| sdk-core.rkt | ~350 | Stable API + contracts |
| sdk-compat.rkt | ~300 | Aliases + GSD convenience |
| sdk-public.rkt | ~229 | Unchanged |

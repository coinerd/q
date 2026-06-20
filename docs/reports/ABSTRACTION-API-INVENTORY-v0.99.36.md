# Public API / struct-out Inventory — v0.99.36 W3

**Date:** 2026-06-22
**Wave:** W3 (#8416)
**Risk Assessment:** GREEN (reward 15, risk 4)

## 1. struct-out Inventory

**Total:** 38 modules containing `(struct-out ...)` exports.
**Total struct-out forms:** 60+ across production code.

### Classification

| Category | Count | Examples | Action |
|----------|-------|----------|--------|
| Public stable data type | ~15 | gui-state, browser-settings, cell-buffer, event-structs | Keep |
| Internal convenience | ~20 | budgeting structs, rollback-action, result types | **2 converted to explicit exports (W3)** |
| Test-only exposure | ~5 | mock-browser-adapter | Consider test helper |
| Protocol/representation | ~15 | ok-result, err-result, check-result | **2 converted (W3)** |
| Already migrated to explicit | ~5 | sdk-core, cli-config | Done |

### Top struct-out Modules

| Module | struct-outs | Risk | W3 Action |
|--------|-------------|------|-----------|
| scripts/abstraction-audit.rkt | 10 | Low | Keep (test fixtures) |
| agent/event-structs/memory-events.rkt | 7 | RED | Do not edit |
| agent/event-structs/tool-events.rkt | 7 | RED | Do not edit |
| scripts/benchmark/task.rkt | 4 | Low | Keep (benchmark types) |
| **runtime/context-assembly/budgeting.rkt** | 3 | Low | **Defer (Typed Racket)** |
| benchmarks/metrics.rkt | 2 | Low | Keep |
| **extensions/gsd/transition-logic.rkt** | 2 | Low | **✅ Converted to explicit exports** |
| gui/gui-types.rkt | 2 | Medium | Keep (GUI protocol) |
| **runtime/context-assembly/rollback-actions.rkt** | 2 | Low | **✅ Converted to explicit exports** |
| scripts/benchmark/report.rkt | 2 | Low | Keep |
| scripts/check-imports.rkt | 2 | Low | Keep (script types) |
| skills/resource-loader.rkt | 2 | Medium | Keep (skill types) |
| browser/settings.rkt | 1 | Low | Keep (config type) |
| interfaces/doctor.rkt | 1 | Medium | Keep (public-facing) |
| util/security/capability-tokens.rkt | 1 | Medium | Keep (security) |

### Rationale for Selection

**`extensions/gsd/transition-logic.rkt`** — 2 struct-outs:
- `ok-result` (2 fields: from, to) — Used by state-machine.rkt via all-from-out
- `err-result` (3 fields: reason, from, attempted) — Same
- No `struct-copy` usage anywhere
- Module already has wrapper functions (ok?, ok-from, ok-to, err?, err-reason)
- **Benefit:** Explicit exports document which fields are part of the public contract

**`runtime/context-assembly/rollback-actions.rkt`** — 2 struct-outs:
- `rollback-action` (4 fields: type, reason, severity, metadata) — Internal
- `rollback-actions-config` (4 fields: execution?, force-distill, expand-context, revert-state)
- No `struct-copy` usage
- Already uses `contract-out` for function exports
- **Benefit:** Explicit exports complement existing contract-out pattern

**Not selected:**
- `budgeting.rkt` — Typed Racket module; struct-out → explicit conversion in Typed Racket requires different handling
- `event-structs/*.rkt` — RED modules (protocol-critical), do not edit
- `gui-types.rkt`, `resource-loader.rkt`, `doctor.rkt` — Medium risk, public-facing types

## 2. all-defined-out Usage

| Module | Risk | W3 Action |
|--------|------|-----------|
| browser/events.rkt | Low | Acceptable (browser event types) |
| extensions/gsd/event-structs.rkt | RED | Do not edit |
| scripts/abstraction-audit.rkt | Low | Acceptable (test fixtures) |
| scripts/check-imports.rkt | Low | Acceptable (script utility) |

## 3. Provide Form Shapes

| Shape | Count | Assessment |
|-------|-------|------------|
| Explicit IDs | Majority | Good discipline |
| contract-out | ~15 modules | Appropriate for public APIs |
| struct-out | 38 modules (60+ forms) | **2 converted to explicit in W3** |
| all-defined-out | 4 modules | Acceptable for specific cases |

## 4. Changes Made

### Change 1: extensions/gsd/transition-logic.rkt
- Replaced `(struct-out ok-result)` with: `ok-result`, `ok-result?`, `ok-result-from`, `ok-result-to`
- Replaced `(struct-out err-result)` with: `err-result`, `err-result?`, `err-result-reason`, `err-result-from`, `err-result-attempted`
- API surface: unchanged (same names exported)
- Benefit: explicit documentation of public contract

### Change 2: runtime/context-assembly/rollback-actions.rkt
- Replaced `(struct-out rollback-action)` with: `rollback-action`, `rollback-action?`, `rollback-action-type`, `rollback-action-reason`, `rollback-action-severity`, `rollback-action-metadata`
- Replaced `(struct-out rollback-actions-config)` with: `rollback-actions-config`, `rollback-actions-config?`, `rollback-actions-config-execution?`, `rollback-actions-config-force-distill`, `rollback-actions-config-expand-context`, `rollback-actions-config-revert-state`
- API surface: unchanged (same names exported)
- Benefit: explicit documentation, complements existing contract-out pattern

## 5. Verification

- `raco make main.rkt` — PASS
- Affected tests: 139 tests passed across 7 test files
- No consumer requires changes (same export names)

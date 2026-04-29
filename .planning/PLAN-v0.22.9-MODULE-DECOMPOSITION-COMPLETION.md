# PLAN: v0.22.9 — Module Decomposition + Completion

**Version:** 0.22.9
**Date:** 2026-04-29
**Base:** v0.22.8 (`7aa7ea8` — local fixes)
**Status:** Planning

## Sources

1. **Internal audit:** `.planning/AUDIT-v0.22.8-IMPLEMENTATION.md` (7 findings: 2 critical, 2 high, 1 medium, 2 low)
2. **External audit:** `.planning/AUDIT_ARCHITECTURE_v0.22.8_2026-04-29.md` (8 recommendations: 3 P0, 3 P1, 2 P2)
3. **Combined review:** `.planning/COMBINED_REVIEW_v0.22.8_ROADMAP_v0.22.9.md` (9 actionable, 4 deferred)
4. **Codebase metrics:** 316 source modules (58,326 LOC), 438 test files (86,969 LOC), 409/409 tests pass

## Source Findings Cross-Reference

### Internal Audit (AUDIT-v0.22.8-IMPLEMENTATION.md)

| ID | Severity | Finding | v0.22.9 Disposition |
|----|----------|---------|---------------------|
| CRITICAL-01 | 🔴 | iteration.rkt bracket corruption (W3) | ✅ Fixed in v0.22.8 local commit |
| CRITICAL-02 | 🔴 | Missing dependency-policy.rktd | ✅ Fixed in v0.22.8 local commit |
| HIGH-01 | 🟡 | TR make-gsd-task backward compat | ✅ Fixed in v0.22.8 local commit |
| HIGH-02 | 🟡 | wave-executor type mismatches | ✅ Fixed in v0.22.8 local commit |
| MEDIUM-01 | 🟡 | Missing "require" in build-deps | ✅ Fixed in v0.22.8 local commit |
| LOW-01 | 🟢 | assert-payload not applied to iteration.rkt | **→ W0 T1** |
| LOW-02 | 🟢 | plan-types-parser.rkt extraction is clean | ✅ Observation — no action needed |

### External Audit (AUDIT_ARCHITECTURE_v0.22.8_2026-04-29.md)

| Rec | Priority | Description | v0.22.9 Disposition |
|-----|----------|-------------|---------------------|
| P0-1 | HIGH | Split agent-session.rkt by responsibility | **→ W1** |
| P0-2 | HIGH | Split session-store.rkt into submodules | **→ W2** |
| P0-3 | HIGH | Create SDK public-core + compat layer | **→ W3** |
| P1-4 | MEDIUM | CI architecture fitness checks | **→ W4** |
| P1-5 | MEDIUM | ADRs for runtime boundaries | **→ W0 T4** |
| P1-6 | MEDIUM | Deprecation lifecycle for SDK aliases | ❌ Deferred to v0.23.0 (process, not code) |
| P2-7 | LOW | Test granularity around extracted seams | **→ W1/W2** (included per-wave) |
| P2-8 | LOW | Dependency graph auto-generation | ❌ Deferred to v0.24.0 (tooling investment) |

### Combined Review Additional Findings

| ID | Source | Severity | Finding | v0.22.9 Disposition |
|----|--------|----------|---------|---------------------|
| INT-02 | Internal | MEDIUM | 291/316 modules lack stability annotations | **→ W0 T2** |
| BOTH-01 | Both | MEDIUM | TR optional arg pattern undocumented | **→ W0 T4** |
| BOTH-02 | Both | LOW | .rktd files blocked by .gitignore | **→ W0 T3** |
| EXT-05 | External | LOW | No auto-generated dependency graph docs | ❌ Deferred to v0.24.0 |

## Design Principles

1. **Façade pattern**: Every extraction keeps the original file as a re-export façade. Zero API breakage for existing consumers.
2. **Sequential extraction**: Each wave extracts one large module. The next wave's extraction depends on the previous being stable.
3. **Test-first gates**: Architecture fitness tests validate module sizes before and after extraction.
4. **Remote-pi-safe scope**: Waves are narrow — one file decomposed per wave, ≤4 submodules created.
5. **No TR expansion this release**: Consolidate existing TR modules, don't add new ones.

## Wave Plan Overview

```
W0 (local fixes + annotations)
 │
W1 (agent-session.rkt decomposition)
 │
W2 (session-store.rkt decomposition)
 │
W3 (sdk.rkt surface refactoring)
 │
W4 (CI gate + version bump)
```

All sequential. W0 is local-only (safe structural edits). W1–W3 are decompositions suitable for remote pi with local review. W4 validates everything.

---

## W0 — Deferred Fixes + Stability Annotations + ADR

**Effort:** M (local, ~90 min)
**Dependencies:** None
**Delivery:** Local commit (no remote pi)
**Findings:** INT-01, INT-02, BOTH-01, BOTH-02

### T1: INT-01 — Re-apply assert-payload wrappers to iteration.rkt

**File:** `runtime/iteration.rkt`
**Action:** Add event contract assertions at 11 high-value `emit-session-event!` call sites.

The v0.22.8 remote pi attempted this but caused catastrophic bracket corruption. This time, use `racket_codemod` for each wrapper — one at a time, with format+compile check after each.

**Target emission points** (by event topic):
1. `"budget.warning"` → `budget-payload/c` (~line 171)
2. `"context.compacted"` → `compact-result-payload/c` (~line 213)
3. `"context.compacted"` (recovery) → `compact-result-payload/c` (~line 219)
4. `"agent.blocked"` → `reason-payload/c` (~line 373)
5. `"message.injected.drain"` → `injection-count-payload/c` (~line 411)
6. `"turn.cancelled"` (×3 calls) → `turn-cancelled-payload/c` (~lines 421, 440, 462)
7. `"iteration.decision"` → `iteration-decision-payload/c` (~line 524)
8. `"runtime.error"` → `error-detail-payload/c` (~line 549)

**Pattern** for each wrapper:
```racket
;; Before:
(emit-session-event! bus session-id "topic.name" (hasheq 'key val ...))
;; After:
(emit-session-event! bus session-id "topic.name" (assert-payload "topic.name" (hasheq 'key val ...) contract-name))
```

**Additional changes:**
- Add import: `(only-in "../util/event-contracts.rkt" reason-payload/c budget-payload/c ...)` to the require form
- Add helper: `(define (assert-payload topic-name payload ctrct) (unless (ctrct payload) (raise-argument-error topic-name "valid event payload" payload)) payload)` before the shared helpers section

**Verify:**
```bash
raco fmt -i runtime/iteration.rkt
raco make runtime/iteration.rkt
raco test tests/test-iteration.rkt
raco test tests/test-event-payload-contracts.rkt
```

### T2: INT-02 — Complete stability tier annotations

**Scope:** All ~291 untagged source modules
**Action:** Add `;; STABILITY:` annotation as second line of each module.

**Tier assignment rules:**

| Layer | Tier | Rationale |
|-------|------|-----------|
| `agent/event-bus.rkt`, `agent/event-types.rkt` | stable | Core pub/sub — public API |
| `agent/loop.rkt` | evolving | Core loop — actively refactored |
| `agent/state.rkt`, `agent/queue.rkt` | internal | Internal state management |
| `llm/*.rkt` | stable | Provider adapters — external consumers |
| `llm/model-defaults.rkt`, `llm/token-budget.rkt` | evolving | Active tuning |
| `llm/http-helpers.rkt`, `llm/provider-errors.rkt` | internal | Implementation details |
| `runtime/*.rkt` (already tagged) | — | Already done |
| `runtime/session-store.rkt` | evolving | Being decomposed in W2 |
| `runtime/compactor.rkt` | evolving | Active optimization |
| `runtime/session-index.rkt` | evolving | Under refactoring |
| `runtime/settings.rkt`, `runtime/model-registry.rkt` | stable | Configuration — public API |
| `runtime/context-manager.rkt`, `runtime/context-builder.rkt` | internal | Implementation detail |
| `runtime/session-switch.rkt`, `runtime/session-migration.rkt` | internal | Lifecycle plumbing |
| `runtime/auto-retry.rkt`, `runtime/trace-logger.rkt` | internal | Cross-cutting infrastructure |
| `runtime/safe-mode.rkt`, `runtime/extension-catalog.rkt` | stable | Public configuration |
| `tools/tool.rkt` (already tagged) | — | Already done |
| `tools/builtins/*.rkt` | stable | Built-in tool implementations |
| `tools/registry-defaults.rkt`, `tools/scheduler.rkt` | stable | Tool infrastructure |
| `tools/permission-gate.rkt`, `tools/file-mutation-queue.rkt` | internal | Safety infrastructure |
| `extensions/*.rkt` (already tagged) | — | Already done |
| `extensions/github-integration.rkt` | evolving | Active development |
| `extensions/racket-tooling.rkt` | stable | Public extension API |
| `extensions/message-inject.rkt`, `extensions/compact-context.rkt` | evolving | Active use by GSD |
| `extensions/loader.rkt`, `extensions/define-extension.rkt` | stable | Extension framework |
| `extensions/ui-surface.rkt`, `extensions/dialog-api.rkt` | stable | TUI extension API |
| `extensions/dynamic-tools.rkt`, `extensions/ext-commands.rkt` | stable | Extension capabilities |
| `extensions/quarantine.rkt`, `extensions/package-audit.rkt` | internal | Security infrastructure |
| `extensions/gsd/*.rkt` (already tagged) | — | Already done |
| `interfaces/sdk.rkt` (already tagged) | — | Already done |
| `interfaces/cli.rkt`, `interfaces/json-mode.rkt` | stable | User-facing modes |
| `interfaces/tui.rkt`, `interfaces/rpc-mode.rkt` | evolving | Under active development |
| `interfaces/sessions.rkt`, `interfaces/doctor.rkt` | internal | Mode plumbing |
| `tui/*.rkt` | evolving | TUI implementation |
| `util/*.rkt` | stable | Shared utilities |
| `util/protocol-types.rkt`, `util/hook-types.rkt` | stable | Public protocol types |
| `util/errors.rkt`, `util/truncation.rkt` | internal | Implementation details |
| `cli/args.rkt` | stable | CLI interface |
| `sandbox/*.rkt` | stable | Security infrastructure |
| `wiring/run-modes.rkt` | stable | Mode entry points |
| `skills/*.rkt` | internal | Skill loading |
| `scripts/*.rkt` | internal | Build/CI tooling |
| `main.rkt` | stable | Application entry point |

**Verify:**
```bash
# Count annotated vs total
grep -rl 'STABILITY:' --include='*.rkt' q/ | grep -v compiled | grep -v tests | wc -l
# Should equal total source modules
find q/ -name '*.rkt' -not -path '*/compiled/*' -not -path '*/tests/*' | wc -l
```

### T3: BOTH-02 — Fix .gitignore for .rktd files

**File:** `q/.gitignore`
**Action:** Add negation pattern so `docs/architecture/*.rktd` is tracked:
```
*.rktd
!docs/architecture/*.rktd
```

**Verify:** `git status docs/architecture/dependency-policy.rktd` shows tracked (not ignored)

### T4: BOTH-01 — Write ADR for TR optional arg pattern

**File:** `q/docs/adr/0013-typed-racket-optional-args.md`
**Action:** Document the rest-arg workaround pattern for TR struct constructors:

```markdown
# ADR-0013: Typed Racket Optional Positional Arguments

## Status: Accepted

## Context
Typed Racket does not support optional positional arguments in the same way as
untyped Racket. `(define (f [x 10]) x)` compiles in `#lang racket` but NOT in
`#lang typed/racket`.

## Decision
Use rest-arg pattern for constructors that need optional trailing arguments:

    (: make-thing : String String * -> thing)
    (define (make-thing name . rest)
      (thing name (if (null? rest) "default" (car rest))))

## Consequences
- Callers can use `(make-thing "foo")` or `(make-thing "foo" "bar")`
- Type signature shows `String *` rather than `String String` — less self-documenting
- Only works for at most one optional arg at the end
- Alternative: use keyword args `#:done [done ""]` which TR supports
```

**Verify:** `ls docs/adr/0013-typed-racket-optional-args.md`

### T5: Update dependency-policy.rktd with current state

**File:** `docs/architecture/dependency-policy.rktd`
**Action:** Review and update to reflect current module inventory. Add any new modules from v0.22.8 (plan-types-parser.rkt, event-contracts.rkt, session-events.rkt, session-types.rkt, session-controls.rkt, session-compaction.rkt).

**Verify:** `racket scripts/check-deps.rkt` passes

---

## W1 — agent-session.rkt Decomposition

**Effort:** L (remote pi, ~2-3 hours)
**Dependencies:** W0 complete
**Findings:** EXT-01 (P0-1)
**Delivery:** Remote pi + local review

### Current State
- `runtime/agent-session.rkt`: 795 lines
- 16 `define` forms (functions)
- 44 exported symbols
- Imports from: session-store, compactor, iteration, context-builder, provider-factory, event-bus, extension-catalog, session-switch, token-budget, settings, model-registry, safe-mode, hooks

### Target Architecture

```
runtime/agent-session.rkt        → façade (~120 lines, re-exports only)
runtime/session-lifecycle.rkt    → new (~250 lines)
runtime/session-persistence.rkt  → new (~180 lines)
runtime/session-events-bridge.rkt → new (~120 lines)
```

### T1: Extract `runtime/session-lifecycle.rkt`

**Extract:**
- `make-agent-session` (lines 204–280) — session creation, config parsing, DI param setup, extension loading
- `resume-agent-session` (lines 282–365) — session resumption, DI param setup, event handler wiring
- `close-session!` (lines 767+) — session cleanup, index save, event emission
- `session-id`, `session-history`, `session-active?` (lines 711–766) — simple accessors
- `run-prompt!`, `run-prompt-internal` (lines 640–758) — prompt dispatch
- `build-session-context` (lines 484–570) — context construction
- `dispatch-iteration` (lines 572–638) — iteration dispatch
- `fork-session`, `fork-session-internal` (lines 366–483) — session forking

**Imports needed:**
- `session-store` (append-entry!, load-session-log, etc.)
- `compactor` (maybe-compact-context)
- `iteration` (run-iteration-loop)
- `context-builder` (build-context)
- `provider-factory` (build-provider)
- `event-bus` (subscribe, emit)
- `extension-catalog` (load-extensions)
- `session-switch` (session-switch-procedure)
- `token-budget` (estimate-context-tokens)
- `settings` (get-setting)
- `model-registry` (resolve-model)
- `safe-mode` (safe-mode?)
- `hooks` (dispatch-hook)
- `session-types` (agent-session struct)

**Re-export from agent-session.rkt:** All 44 original symbols via `(require (only-in "session-lifecycle.rkt" ...))`.

### T2: Extract `runtime/session-persistence.rkt`

**Extract:**
- `ensure-persisted!` (line 159) — flush pending entries to disk
- `buffer-or-append!` (line 176) — buffered write with pending marker
- `session-index-path` (line 181) — index file path computation

**Imports needed:**
- `session-store` (append-entry!, append-entries!)
- `session-index` (add-or-update-entry)
- `agent-session` struct accessors

### T3: Extract `runtime/session-events-bridge.rkt`

**Extract:**
- `wire-session-event-handlers!` — event subscription wiring (fork, compact events)
- Model control functions: `set-model!`, `cycle-model!`
- Thinking level functions: `set-thinking-level!`, thinking level accessors
- Shutdown functions: `request-shutdown!`, `force-shutdown!`, `reset-shutdown-flags!`

**Imports needed:**
- `event-bus` (subscribe)
- `session-store` (fork-session!)
- `hooks` (dispatch-hook)
- `agent-session` struct accessors

### T4: Convert agent-session.rkt to façade

**Action:** Replace all function definitions with re-exports:
```racket
#lang racket/base
;; runtime/agent-session.rkt — session lifecycle orchestration (façade)
;; STABILITY: evolving
;;
;; Re-exports from session-lifecycle, session-persistence, session-events-bridge.
;; All 44 original symbols preserved. Zero API breakage.

(require "session-lifecycle.rkt"
         "session-persistence.rkt"
         "session-events-bridge.rkt"
         "session-types.rkt")

(provide (all-from-out "session-lifecycle.rkt")
         (all-from-out "session-persistence.rkt")
         (all-from-out "session-events-bridge.rkt"))
```

### T5: Update tests and verify

**Verify:**
```bash
raco make runtime/agent-session.rkt
raco test tests/test-agent-session.rkt
raco test tests/test-iteration.rkt
raco test tests/test-sdk-gsd-live.rkt
racket scripts/run-tests.rkt --suite fast
```

**Expected result:** All tests pass unchanged. No test file modifications needed (façade preserves all symbols).

**Architecture fitness check:**
- `agent-session.rkt`: ~120 lines (from 795)
- `session-lifecycle.rkt`: ~250 lines
- `session-persistence.rkt`: ~180 lines
- `session-events-bridge.rkt`: ~120 lines

---

## W2 — session-store.rkt Decomposition

**Effort:** L (remote pi, ~2-3 hours)
**Dependencies:** W1 complete
**Findings:** EXT-02 (P0-2)
**Delivery:** Remote pi + local review

### Current State
- `runtime/session-store.rkt`: 894 lines
- 40+ `define` forms
- 35 exported symbols
- Provides: append-only writes, hash-chain integrity, session loading, tree operations, in-memory manager, forking, import, migration, naming, versioning

### Target Architecture

```
runtime/session-store.rkt              → façade (~150 lines, re-exports only)
runtime/session-store-append.rkt       → new (~200 lines)
runtime/session-store-integrity.rkt    → new (~250 lines)
runtime/session-store-tree.rkt         → new (~200 lines)
```

### T1: Extract `runtime/session-store-integrity.rkt`

**Extract:**
- `GENESIS-HASH` constant (line 111)
- `canonical-jsexpr->string` (line 118)
- `compute-event-hash` (line 134) — hash-chain computation
- `read-last-hash` (line 141) — read last hash from log
- `recompute-hash-chain` (line 153) — recompute all hashes
- `verify-session-integrity` (lines 229–345) — full integrity verification
- `repair-session-log!` (lines 346–423) — log repair
- `verify-hash-chain` (lines 424–486) — chain verification

**This is the most self-contained concern:** Hash-chain integrity and verification are pure functions that operate on log paths and entry lists.

### T2: Extract `runtime/session-store-tree.rkt`

**Extract:**
- `append-tree-entry!` (line 769) — tree append with hooks
- `load-tree` (line 792) — tree loading
- `get-tree-branch` (line 812) — branch retrieval
- `get-children` (line 835) — children query
- `resolve-active-branch` (line 839) — active branch resolution
- `tree-info` (line 862) — tree metadata

**Clean seam:** Tree operations are independent of append/integrity logic.

### T3: Extract `runtime/session-store-append.rkt`

**Extract:**
- `pending-marker-path` (line 76)
- `write-pending-marker!` (line 85)
- `remove-pending-marker!` (line 94)
- `has-pending-marker?` (line 100)
- `append-entry!` (line 164) — core append with hash-chain
- `append-entries!` (line 180) — batch append
- `load-session-log` (line 206) — log loading
- `replay-session` (line 221) — session replay
- `CURRENT-SESSION-VERSION` (line 487)
- `write-session-version-header!` (line 601)
- `read-first-log-entry` (line 625)
- `ensure-session-version-header!` (line 639)
- `migrate-session-log!` (line 662)
- `write-session-name!` (line 546)
- `import-session!` (line 501)
- `fork-session!` (line 558)

**Imports from submodules:** `compute-event-hash`, `read-last-hash` from integrity module.

### T4: Convert session-store.rkt to façade

```racket
#lang racket/base
;; runtime/session-store.rkt — session storage (façade)
;; STABILITY: evolving
;;
;; Re-exports from session-store-append, session-store-integrity,
;; session-store-tree, and session-store-inmemory.
;; All 35 original symbols preserved.

(require "session-store-append.rkt"
         "session-store-integrity.rkt"
         "session-store-tree.rkt")

(provide (all-from-out "session-store-append.rkt")
         (all-from-out "session-store-integrity.rkt")
         (all-from-out "session-store-tree.rkt"))
```

### T5: In-memory manager stays in session-store.rkt façade

The `make-in-memory-session-manager` and its operations (~65 lines) are small enough to stay in the façade module. Alternatively, extract to `runtime/session-store-inmemory.rkt` if the façade exceeds 200 lines.

### T6: Update tests and verify

**Verify:**
```bash
raco make runtime/session-store.rkt
raco test tests/test-session-store.rkt  # (if exists)
raco test tests/test-session-index.rkt
raco test tests/test-agent-session.rkt
racket scripts/run-tests.rkt --suite fast
```

**Architecture fitness check:**
- `session-store.rkt`: ~150 lines (from 894)
- `session-store-append.rkt`: ~200 lines
- `session-store-integrity.rkt`: ~250 lines
- `session-store-tree.rkt`: ~200 lines

---

## W3 — SDK Surface Refactoring

**Effort:** M (remote pi, ~1-2 hours)
**Dependencies:** W2 complete
**Findings:** EXT-03 (P1-3)
**Delivery:** Remote pi + local review

### Current State
- `interfaces/sdk.rkt`: 763 lines
- Two sections: (1) Core API functions (lines 1–450), (2) `q:` aliases + GSD convenience (lines 450–763)
- 70+ exported symbols
- `interfaces/sdk-public.rkt`: 229 lines (factory function + model builder)

### Target Architecture

```
interfaces/sdk.rkt          → façade (~100 lines, re-exports only)
interfaces/sdk-core.rkt     → new (~350 lines) — core API + contracts
interfaces/sdk-compat.rkt   → new (~300 lines) — q: aliases + GSD convenience
interfaces/sdk-public.rkt   → unchanged (229 lines)
```

### T1: Extract `interfaces/sdk-core.rkt`

**Extract from sdk.rkt:**
- Struct definitions: `runtime-config`, `runtime` (lines 190–230)
- Core API: `make-runtime`, `open-session`, `run-prompt!`, `subscribe-events!`, `interrupt!`, `fork-session!`, `compact-session!`, `session-info`, `steer!`, `follow-up!`, `navigate!` (lines 223–548)
- Factory: `create-agent-session` (lines 552–588)
- Cancellation token imports and re-exports
- All `contract-out` forms for core API

**This is the stable API surface** — version it carefully.

### T2: Extract `interfaces/sdk-compat.rkt`

**Extract from sdk.rkt:**
- `q:` aliases: `q:create-session`, `q:session-send`, `q:session-subscribe`, `q:session-interrupt`, `q:session-fork`, `q:session-compact`, `q:session-info`, `q:session-branch`, `q:session-navigate`, `q:session-tree-info` (lines 590–675)
- GSD convenience: `gsd-status`, `q:plan`, `q:go`, `q:gsd-status`, `q:reset-gsd!` (lines 681–765)
- `dispatch-command!` (lines 698–716) — command routing

**This is the compat/convenience layer** — may be deprecated in v0.23.0.

### T3: Convert sdk.rkt to façade

```racket
#lang racket/base
;; interfaces/sdk.rkt — q SDK (façade)
;; STABILITY: stable
;;
;; Re-exports from sdk-core and sdk-compat for backward compatibility.
;; All 70+ original symbols preserved.

(require "sdk-core.rkt"
         "sdk-compat.rkt")

(provide (all-from-out "sdk-core.rkt")
         (all-from-out "sdk-compat.rkt"))
```

### T4: Verify SDK consumers unaffected

**Key consumers to test:**
- `tests/test-integration.rkt` — uses `make-runtime`, `open-session`, `run-prompt!`
- `tests/test-sdk-gsd-live.rkt` — uses `q:plan`, `q:go`
- `tests/test-self-hosting-workflow.rkt` — uses `create-agent-session`
- `scripts/sdk-gsd-integration-test.rkt` — uses `q:plan`, `q:go`

**Verify:**
```bash
raco make interfaces/sdk.rkt
raco test tests/test-integration.rkt
raco test tests/test-sdk-gsd-live.rkt
racket scripts/run-tests.rkt --suite fast
```

**Architecture fitness check:**
- `sdk.rkt`: ~100 lines (from 763)
- `sdk-core.rkt`: ~350 lines
- `sdk-compat.rkt`: ~300 lines

---

## W4 — CI Architecture Gate + Version Bump

**Effort:** S (remote pi or local, ~30 min)
**Dependencies:** W3 complete
**Findings:** EXT-04 (P1-4)
**Delivery:** Either

### T1: Create `scripts/arch-report.rkt`

**Action:** New script that emits a per-module size report and checks fitness thresholds.

```racket
#lang racket
;; scripts/arch-report.rkt — Architecture fitness report
;;
;; Usage: racket scripts/arch-report.rkt [--ci]
;; --ci: exit with error code if any threshold violated

;; Reports:
;; 1. Modules exceeding size threshold (600 LOC default)
;; 2. Layer violation summary (from test-arch-boundaries)
;; 3. Stability annotation coverage
;; 4. Known-large exceptions status
```

**Threshold:** 600 lines (lowered from 900 after W1/W2/W3 decompositions).
**Known-large exceptions after decompositions:**
- `agent/event-types.rkt` (852 lines) — data-only module, pure definitions, no logic to extract
- `agent/loop.rkt` (853 lines) — core agent loop, tightly coupled state machine

### T2: Add arch-report step to CI

**File:** `q/.github/workflows/ci.yml`
**Action:** Add step after "Dependency completeness":

```yaml
- name: Architecture fitness
  run: racket scripts/arch-report.rkt --ci
```

### T3: Update CHANGELOG.md

**Action:** Add v0.22.9 entry:

```markdown
## v0.22.9 — 2026-04-29

### Module Decomposition + Completion

**W0 — Deferred Fixes + Annotations**
- INT-01: Re-applied assert-payload wrappers to 11 iteration.rkt emission points
- INT-02: Stability tier annotations added to all 316 source modules
- BOTH-01: ADR-0013 for Typed Racket optional arg pattern
- BOTH-02: .gitignore exception for docs/architecture/*.rktd

**W1 — agent-session.rkt Decomposition (EXT-01)**
- Extracted session-lifecycle.rkt (~250 lines)
- Extracted session-persistence.rkt (~180 lines)
- Extracted session-events-bridge.rkt (~120 lines)
- agent-session.rkt converted to ~120-line façade
- All 44 symbols preserved, zero API breakage

**W2 — session-store.rkt Decomposition (EXT-02)**
- Extracted session-store-integrity.rkt (~250 lines)
- Extracted session-store-tree.rkt (~200 lines)
- Extracted session-store-append.rkt (~200 lines)
- session-store.rkt converted to ~150-line façade
- All 35 symbols preserved, zero API breakage

**W3 — SDK Surface Refactoring (EXT-03)**
- Extracted sdk-core.rkt (~350 lines) — stable API surface
- Extracted sdk-compat.rkt (~300 lines) — aliases + GSD convenience
- sdk.rkt converted to ~100-line façade
- All 70+ symbols preserved, zero API breakage

**W4 — CI Architecture Gate + Version Bump**
- New scripts/arch-report.rkt with 600-LOC threshold
- CI step added for per-PR architecture fitness check
- Version bump 0.22.8 → 0.22.9
```

### T4: Version bump

**Files:** `util/version.rkt`, `README.md`, `info.rkt`
**Action:** Run `racket scripts/sync-version.rkt --write --all`

### T5: Final verification

```bash
racket scripts/run-tests.rkt  # full suite
racket scripts/arch-report.rkt --ci  # fitness gate
racket scripts/check-deps.rkt  # dependency check
racket scripts/sync-version.rkt --check --all  # version sync
```

**Expected:** 409+ files, 6500+ tests, 0 failures.

---

## Success Metrics

| Metric | Current (v0.22.8) | Target (v0.22.9) | Wave |
|--------|-------------------|-------------------|------|
| Modules > 800 LOC | 5 | 0 | W1, W2 |
| Modules > 600 LOC | 11 | ≤ 2 (event-types, loop) | W1, W2, W3 |
| Stability annotations | 25/316 (8%) | 316/316 (100%) | W0 |
| assert-payload coverage | 0/17 emission points | 11/17 (top events) | W0 |
| agent-session.rkt | 795 LOC | ~120 LOC façade | W1 |
| session-store.rkt | 894 LOC | ~150 LOC façade | W2 |
| sdk.rkt | 763 LOC | ~100 LOC façade | W3 |
| CI architecture gate | None | Per-PR fitness report | W4 |

## Deferred to v0.23.0+

| ID | Finding | Rationale |
|----|---------|-----------|
| EXT-P1-06 | SDK deprecation lifecycle for aliases | Process documentation, not code |
| EXT-P2-08 | Auto-generated dependency graphs | Tooling investment, requires arch-report.rkt as base |
| INT-03 | event-contracts.rkt → Typed Racket | Low priority, 70-line module works fine untyped |
| TR expansion | protocol-types.rkt migration | 240 consumers, highest-risk TR migration |
| tui/state.rkt split | 771 lines but tightly coupled | TUI-specific, not architecture-layer concern |
| agent/loop.rkt split | 853 lines but single coherent loop | Core agent state machine, extraction risky |

## Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Façade re-export misses a symbol | Medium | High | Full test suite run after each extraction; `all-from-out` used |
| Remote pi bracket corruption in extraction | High | Critical | W0 done locally; W1–W3 require local review of all .rkt edits |
| Circular imports between new submodules | Low | High | Dependency analysis before extraction; integrity has no imports from append/tree |
| CI gate too strict (600 LOC) | Low | Low | known-large exceptions for event-types and loop |
| SDK compat breakage | Low | Critical | sdk-public.rkt unchanged; sdk.rkt façade preserves all symbols |

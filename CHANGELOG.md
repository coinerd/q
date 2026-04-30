# Changelog

## v0.23.5 — 2026-04-29

### Performance Fix & Code Hygiene

**CRITICAL — O(n²) → O(n) fix:**
- Built id→msg hash in `fit-messages-pair-preserving`, replaced 4× `for/first` linear scans with O(1) `hash-ref` lookups
- Function complexity reduced from O(n²) to O(n) for pair resolution

**Dedup — format-messages-for-summary:**
- Moved canonical implementation to `compaction-prompts.rkt`
- Deleted local copies from `compactor.rkt` and `context-assembly.rkt` (2 → 1)

**Dead export cleanup:**
- Removed `requires-pair-inclusion?` from provide in `context-policy.rkt`
- Removed 2 dedicated tests (no production callers)

**Docstrings added to 5 functions:**
- `generate-catalog`, `collapse-consecutive-tools`, `generate-context-summary`
- `build-tiered-context-with-hooks`, `truncate-messages-to-budget`

**Other:**
- Updated test suite names: `context-builder` → `context-assembly-tree`, `context-builder-agents` → `context-assembly-agents`
- Added config guard comment for zero max-catalog values

**Tests:** 101 context+compactor tests, 67 assembly+policy tests pass

## v0.23.4 — 2026-04-29

### Combined Audit Remediation & Hardening

**Dead Code Removal:**
- Deleted unused `fit-recent` function from context-assembly.rkt
- Deleted custom `filter-map` — uses racket/list built-in instead
- Replaced `pin-first-user-internal` with canonical `ensure-first-user-pinned`
- Moved `build-pair-index`/`requires-pair-inclusion?` from contract-out to plain provide
- context-assembly.rkt: 849 → 800 lines

**Config Validation:**
- Added `#:guard` to `context-assembly-config` struct with clear error messages
- 5 new config validation tests (negative/zero/string values rejected)
- Cached `(length ...)` results in locals for trace paths (6 calls optimized)

**Test Cleanup:**
- Renamed test-context-builder.rkt → test-context-assembly-tree.rkt
- Renamed test-context-builder-agents.rkt → test-context-assembly-agents.rkt
- Updated TR pilot suite label to v0.23.x
- Added session-lifecycle integration test

**Tests:** All context + lifecycle tests pass

## v0.23.3 — 2026-04-30

### Audit Remediation

**Architecture:**
- Merged context-builder.rkt into context-assembly.rkt — single unified context module (ADR-0012)
- Removed tiered-context duplication from compactor.rkt — canonical implementations only in context-assembly
- Added `contract-out` on context-assembly.rkt and context-policy.rkt public APIs
- Fixed summary fallback count mismatch — simple-summary-text truncates to 20 entries

**Changes:**
- `runtime/context-assembly.rkt`: Tree-walk merge, `contract-out`, `simple-summary-count`, `(struct-out context-assembly-payload)`
- `runtime/context-policy.rkt`: `contract-out` on all public APIs
- `runtime/compactor.rkt`: Removed tiered-context structs, build-tiered-context, payload helpers
- `runtime/context-builder.rkt`: DELETED (merged into context-assembly)
- Updated 11 test files with import changes

**Tests:** All 149 context+TR tests pass

## v0.23.2 — 2026-04-29

### Context Performance + Observability

**Architecture:**
- Per-assembly token memoization in `build-assembled-context` — each message estimated at most once via `hash-ref!` memo table
- LRU cache eviction for summary-cache — configurable `max-entries` (default 50), lookup promotes entry to front, store evicts oldest at capacity
- O(1) catalog counting — `generate-catalog` uses counter variable instead of `(length acc)` per iteration
- Assembly trace hook — optional `#:trace-callback` parameter emits structured phase events (start, phase1-pinned, phase3-fitted, phase2-summary, phase4-catalog, done)

**Changes:**
- `runtime/context-assembly.rkt`: Token memoization, LRU summary-cache, O(1) catalog counting, trace hook
- `tests/test-context-assembly-perf.rkt` (NEW): 4 benchmark tests (determinism, 200-msg perf, consistency, catalog max-entries)
- `tests/test-summary-cache-eviction.rkt` (NEW): 8 LRU eviction tests (capacity, promotion, stress)
- `tests/test-context-assembly-trace.rkt` (NEW): 8 tests (trace integration + property tests for ordering, pinning, budget, partition)

**Tests:** 20 new tests, all 50 context tests pass

## v0.23.2 — 2026-04-29

### Unified Context Assembly

**Architecture:**
- Created `runtime/context-assembly.rkt` (530 LOC) — single module for all context assembly logic
- Replaces `runtime/context-manager.rkt` (deleted)
- Production callers (`session-lifecycle.rkt`, `turn-orchestrator.rkt`) migrated to context-assembly
- Test files migrated from context-manager to context-assembly imports
- `context-assembly-config` with backward-compat aliases for `context-manager-config`
- `build-assembled-context` returns `context-result` struct with messages, catalog, metadata

**Changes:**
- `runtime/context-assembly.rkt` (NEW): `build-assembled-context`, `build-tiered-context-with-hooks`, `tiered-context`, `context-result`, catalog, summary cache
- `runtime/session-lifecycle.rkt`: Imports from context-assembly, uses `build-assembled-context`
- `runtime/turn-orchestrator.rkt`: Imports tiered-context functions from context-assembly
- `runtime/context-manager.rkt` (DELETED): All logic moved to context-assembly
- Test files updated: test-context-summary, test-summary-integration, test-context-manager-polish

**Tests:** 16 new context-assembly tests, 92 context tests pass, all 6500+ tests pass

## v0.23.2 — 2026-04-29

### Context Policy + LLM Summarization

**Architecture:**
- Extracted `runtime/context-policy.rkt` (202 LOC) — shared token estimation, first-user pinning, pair-preserving budget fitting
- Wired `context-manager.assemble-context` as production pipeline in `session-lifecycle.rkt`
- LLM summarization via compactor `llm-summarize` when provider available
- Updated ADR-0012 to reflect new three-module context architecture

**Changes:**
- `runtime/context-policy.rkt` (NEW): `estimate-message-tokens`, `ensure-first-user-pinned`, `fit-messages-pair-preserving`, `build-pair-index`
- `runtime/context-manager.rkt`: `generate-context-summary` now dispatches to `llm-summarize` via compactor
- `runtime/session-lifecycle.rkt`: Production pipeline uses `assemble-context` with provider + model-name
- `runtime/context-builder.rkt`: Bridge module importing from context-policy (fallback pipeline)
- `runtime/token-compaction.rkt`: Imports `estimate-message-tokens` from context-policy

**Tests:** 16 new context-policy tests, 8 new summarization tests, all 6500+ tests pass


## v0.23.2 — 2026-04-28

### Module Decomposition + Completion

**W0** — Assert-payload wrappers (11 sites), stability annotations (314 modules),
.gitignore fix, ADR-0013, dependency-policy update.

**W1** — agent-session.rkt decomposition: extracted session-lifecycle.rkt (327 LOC),
converted agent-session.rkt to façade (401 LOC). Fixed iteration.rkt paren regression.
Fixed compact-result-payload/c contract.

**W2** — session-store.rkt decomposition: extracted session-store-integrity.rkt (321 LOC)
and session-store-tree.rkt (129 LOC), converted session-store.rkt to façade (342 LOC).

**W3** — sdk.rkt decomposition: extracted sdk-core.rkt (439 LOC) and
sdk-compat.rkt (237 LOC), converted sdk.rkt to thin façade (15 LOC).

**W4** — arch-report.rkt script (CI gate), version bump 0.23.2 → 0.23.2.


## v0.23.2 — 2026-04-29

### Architecture Enforcement + DI Fix + Event Schema Hardening

**W0 — DI Fixes + Lazy Loading**
- LAZY-01: Lazy-require for `injection-event-topic` in iteration.rkt
- DI-03: `resolve-compact-proc` helper with fallback to lazy-loaded impl
- DI-04: `resolve-estimate-tokens` helper with fallback to lazy-loaded impl
- LOW-05: Remove unused `injected-box` parameter from iteration loop

**W1 — Dependency Policy + Arch Test Refactors**
- Updated dependency-policy.rktd with current module inventory
- Refactored arch boundary tests for policy-driven validation
- Added CI drift gate to catch dependency policy staleness

**W2 — Typed Racket Expansion + Boundary Docs**
- Migrated `gsd/plan-types.rkt` to Typed Racket with cast-based parser boundary
- Migrated `gsd/plan-validator.rkt` to Typed Racket
- Added TR boundary documentation to event-payloads.rkt
- 14 TR tests, 129 GSD tests, 13 arch tests passing

**W3 — Event Schema Contracts**
- Created `util/event-contracts.rkt` with 9 reusable payload contracts
- Applied `assert-payload` assertions to top-7 events in iteration.rkt
- 18 golden payload contract tests passing
- Updated event-taxonomy.md with contract-validated event docs

**W4 — Hook Golden Tests + Stability Tiers + Version Bump**
- 20 hook golden payload shape tests (regression canaries)
- Stability tier annotations (stable/evolving/internal) on 25+ modules
- Version bump 0.23.2 → 0.23.2

## v0.23.2 — 2026-04-29

### Regression Fixes + DI Completion + SDK Surface + TR Hardening
- **REG-01**: Fixed tool result construction in racket-tooling-handlers.rkt (plain hasheq → proper tool-result structs)
- **REG-02**: Fixed test-context-overflow retry mock to handle compaction recovery re-raise
- **REG-03**: Fixed loop-result-metadata event-payload handling (payload→hash conversion)
- **DOC-01**: Synced README.md metrics and version references across all docs
- **DI-01**: Replaced 3 concrete imports in iteration.rkt with lazy-require + parameter-based DI (current-compact-proc, current-estimate-tokens, current-inject-topic)
- **DI-02**: agent-session.rkt sets DI parameters at session init, no compile-time dependency on extensions/ or llm/
- **FMT-01**: Updated lint-format.rkt to accept #lang typed/racket and #lang info (3 false warnings eliminated)
- **ARCH-02**: Added TR boundary contract enforcement tests + documentation comments
- **ARCH-01**: Verified SDK surface module (sdk.rkt + sdk-public.rkt), added arch-fitness coverage
- 2 Typed Racket contract tests added (9/9 TR pilot tests pass)
- 2 arch-fitness tests added (8/8 pass)
- 188+ tests pass, 0 regressions

## v0.23.2 — 2026-04-29

### Regression Fixes + Module Splits + DI + Typed Racket Pilot
- **PAY-01**: Fixed test-hooks-complete.rkt regression from struct payload adoption
- **DOC-01**: Synced README.md + docs, added CI version sync gate
- **SDK-01a**: Completed SDK contract-out for in-memory session helpers
- **PAY-02**: Completed event payload struct adoption in agent-session.rkt
- **DEF-01**: Fixed iteration.rkt header comment
- **DEF-02**: Fixed sdk-gsd-integration-test to use provider-name
- **DEF-03a**: Split tui/state.rkt (994→4 files: state-types, state-events, state-ui, state facade)
- **DEF-03b**: Split extensions/racket-tooling.rkt (922→3 files: racket-tooling, -helpers, -handlers)
- **MOD-02**: DI keyword args for run-iteration-loop + call-with-overflow-recovery + run-provider-turn
- **RKT-01**: Typed Racket pilot on util/version.rkt + util/event-payloads.rkt (8 structs typed)
- **BUILDER**: Updated version sync/lint scripts to handle Typed Racket multi-line format
- 109+ tests added across 6 waves, 0 regressions

## v0.23.2 — 2026-04-29

### Audit Remediation (v0.23.2 Post-Merge Fixes)
- **FIT-01**: Replaced line-based `extract-requires` with read-based S-expression parser in arch test files
- **FIT-02**: Module size threshold 900 lines with known-large tracking (tui/state.rkt, extensions/racket-tooling.rkt)
- **FIT-03**: Updated stale known-exceptions list for runtime layer boundary tests
- **FIT-04**: Extracted shared arch test helpers into `tests/helpers/arch-utils.rkt` (eliminated ~80 lines duplication)
- **GEN-01**: Fixed trace regression — `provider-name` instead of `object-name` in agent/loop.rkt
- **GEN-02**: Adopted event payload structs in runtime/agent-session.rkt (4 hasheq→struct replacements)
- **GEN-03**: Added `provider-count-tokens` to `contract-out` in llm/provider.rkt
- **SDK-01**: Moved all SDK callables into `contract-out` in sdk-public.rkt (get-context-usage, enriched aliases, etc.)
- **SDK-02**: Added 8 negative contract-rejection tests to test-contracts.rkt
- **VER-01**: Version sync verified

## v0.23.2 — 2026-04-28

### Architecture Modularity & Racket Idiom Remediation
- **MOD-01**: Extracted `runtime/turn-orchestrator.rkt` (250 lines) from `runtime/iteration.rkt` (801→601 lines), eliminated upward imports
- **MOD-03**: Created `interfaces/sdk-public.rkt` with curated SDK exports + `contract-out` on 20+ public functions
- **MOD-04**: Declarative tool registry: `tools/registry-table.rkt` with 14-tool spec table + `register-tools-from-specs!`
- **MOD-05**: Architecture fitness tests in `tests/test-arch-fitness.rkt` (module coupling, size, API surface checks)
- **RKT-02**: Replaced symbol-dispatch provider with `racket/generic` (`define-generics gen:provider`)
- **RKT-03**: Explicit struct payloads for event types (`util/event-payloads.rkt`)
- **RKT-04**: Contract enforcement at SDK boundary via `sdk-public.rkt`
- **RKT-05**: Schema macro: `tools/schema-macro.rkt` with `define-tool-schema` + `tool-schema` + `build-properties-hash`
- **FIX-01**: Fixed SDK GSD live test — save/restore `gsd-event-bus` and `pinned-planning-dir` around `reset-all-gsd-state!`
- Simplified `tools/registry-defaults.rkt` from 374→15 lines (delegates to registry-table)
- Added 37 new tests across 4 test files

## v0.23.2 — 2026-04-28

### CI Pipeline Hardening
- **CI-01**: Fixed `wave_finish()` docs-sync bug — `git diff --name-only` ran without `cwd=Q_DIR`, so auto-fixed docs were never committed
- **CI-02**: Extended `sync-version.rkt` with `--all` flag to sync all `.md` files (not just info.rkt + README.md)
- **CI-03**: Simplified `ci_preflight()` to use `sync-version.rkt --write --all` instead of inline Python regex
- **CI-04**: Consolidated CI jobs: single lint gate → focused test matrix → smoke/release
- **CI-05**: Added concurrency group to cancel superseded CI runs
- **CI-06**: Removed redundant lint checks from test matrix cells

## v0.23.2 — 2026-04-28

### Review Remediation
- **REV-01**: Reverted GSD session-state from Racket parameters to boxes — hook handlers run in child threads where parameter mutations are invisible to parent
- **REV-03**: Removed dead `cleanup-thunk` from agent-session.rkt
- **REV-05**: DRY `session-log-path` — extracted to session-types.rkt, removed duplicates from agent-session.rkt and session-events.rkt
- **REV-06a**: Fixed test-destructive-warning default assertion (`'safe-mode-default`)
- **REV-06b**: Fixed test-registry-defaults tool count (13→14, added `delete-lines`)
- **REV-06c**: Fixed test-self-hosting-deep version check (added 0.22.x)
- **REV-11**: Removed stale AUDIT-04 comment from core.rkt

## v0.23.2 — 2026-04-28

### Audit Remediation + Deferred Refactors
- **AUDIT-01**: Fixed backtick detection regex (unanchored → paired anchored)
- **AUDIT-02**: Added HTML/entity sanitization for bash tool error output
- **AUDIT-03**: Strengthened delete-lines out-of-range validation with boundary checks
- **AUDIT-04**: Removed dead `gsd-tool-guard` from core.rkt (canonical in gsd-planning.rkt)
- **AUDIT-09**: Deduplicated imports in events.rkt and message-inject.rkt
- **AUDIT-10**: Verified shell-quote direct import fix (already done in v0.23.2)
- **AUDIT-11**: Replaced 3 `check-not-false` with `check-pred values` in GSD planning tests
- **AUDIT-12**: Added session-lifecycle edge-case tests (empty history, partial writes)
- **AUDIT-13**: Added delete-lines security boundary tests
- **QUAL-02**: Created `extensions/gsd/session-state.rkt` with Racket parameters for per-session GSD state
- **QUAL-03**: Migrated state-machine.rkt and gsd-planning-state.rkt from global boxes to session parameters
- **ARCH-05**: Split agent-session.rkt (1016→769 lines) into session-types/events/controls/compaction modules
- **ARCH-06**: Split tui/commands.rkt (936→311 lines) into commands/{context,branch,session,model,extension} modules

## v0.23.2 — 2026-04-28

### Architecture Remediation
- **SEC-09**: Extended error sanitizer with API key, /tmp/ path, and email pattern redaction
- **SEC-10**: Documented write budget thread-safety invariant (per-thread Racket parameters)
- **SEC-14**: Documented process limit scope limitation (global vs per-session)
- **SEC-13**: Safe-mode defaults pattern with sentinel values for tool parameters
- **SEC-12**: Error sanitizer strips home directory paths from error messages
- **TH-01**: Expanded write tool tests (safe-mode, budget boundary, home path, nested dirs)
- **TH-02**: Added dynamic-wind cleanup wrapper for GSD test isolation
- **TH-03**: Upgraded GSD assertions from check-not-false to type-specific predicates
- **TH-11**: System-instruction preservation test under budget pressure
- **TH-14**: Error sanitizer tests wrapped in test-case forms with edge cases
- **ARCH-01**: Removed dead imports from runtime/iteration.rkt
- **ARCH-02**: Extracted util/safe-mode-state.rkt leaf module (eliminated upward import)
- **ARCH-03**: Created extensions/tool-api.rkt facade for extension tool access
- **ARCH-04**: Added extensions/api.rkt event bus re-exports for extensions
- **ARCH-05**: Extracted runtime/session-context.rkt for path settings
- **QUAL-01–04**: Code quality improvements (shell quoting, shadowing, type narrowing)
- **DOC-02**: Updated releasing.md verified-against to v0.23.2
- **DOC-03**: Sorted CHANGELOG entries monotonically
- **DOC-06**: Added ADR-0011 (GSD state machine) and ADR-0012 (context manager)
- **DOC-08**: Added docstrings to ext-package-manager.rkt and compact-context.rkt
- **DOC-09**: Created extension authoring guide
- **DOC-11**: Clarified source tree vs installed package paths
- **DOC-14**: Added Racket 8.10 TUI skip comment in CI
- **MAT-05**: Added --fail-on-regression flag to run-benchmark.rkt
- **MAT-06**: Updated CONTRIBUTING.md
- **MAT-07**: Created getting-started index
- **MAT-09**: Updated releasing.md smoke test section


## v0.23.2 — 2026-04-28

### Execution Architecture Improvements
- **delete-lines tool**: Line-range deletion tool (avoids chunked edit for removals of 3+ consecutive lines)
- **/wave-done command**: Marks wave complete in PLAN.md + STATE.md, emits `gsd.wave.completed` event
- **Planning path resolution**: `.planning/` write paths auto-rewrite to pinned project root
- **Backup timestamp fix**: Eliminated rational numbers in backup filenames (`inexact->exact` → `current-milliseconds`)
- **Execution prompt**: Updated to mention `/wave-done N` for wave completion

## v0.23.2 — 2026-04-28

### GSD Plan Archival + Execution Polish
- `/done` command archives completed plans to `.planning/archive/<slug>/`
- PLAN.md status markers update automatically on wave completion (`[Inbox]→[DONE]`)
- Empty subdirectories cleaned up after archive
- `ensure-state-md!` auto-creates STATE.md during `/plan` initialization
- TUI shows `✅ Plan archived` notification
- Iteration label shows `[executing...]` during execution mode vs `[exploring...]`
- Execution prompt includes edit chunking rules (≤20 lines, ≤500 chars oldText)

## v0.23.2 — 2026-04-28

### Security
- `safe-manifest-file-path?` predicate rejects `..`, absolute paths, Windows drive letters
- Defense-in-depth path check in `install-package-from-dir` copy loop
- `clean-file-path` strips backticks from GSD parser file paths
- Checksum enforcement during package install (backward compatible)
- Canonicalized `allowed-paths` with `resolve-path` + boundary matching

### Improvements
- `oauth-available?` predicate for OAuth stub detection
- Planning prompt: max 3 reads, must write wave docs after reading
- Documented planning-write mode-set timing (no race found)

## v0.23.2 — 2026-04-27

### Fixed
- Planning prompt now shows exact `- File:` syntax with concrete template
- LLM instructed to write N separate wave docs for N waves
- Parser accepts `- File:`, `- Files:`, and `## Files` heading formats
- Validation relaxed: individual waves can be file-less (plan-level check remains)
- Planning prompt consolidated into `prompts.rkt` (single source of truth)

## v0.23.2 — 2026-04-26

### GSD Extension Rewrite (5 waves)

Complete rewrite of scattered GSD state management into a proper state machine
with structured plan types, validation, and error recovery.

**Wave 0 — State Machine + Structured Plan Types** (PR #2058)
- `extensions/gsd/state-machine.rkt`: Explicit state transitions, tool guards,
  semaphore-protected, transition history, snapshot
- `extensions/gsd/plan-types.rkt`: gsd-plan/gsd-wave/gsd-task structs,
  markdown parsing, validation, accessor aliases
- 57 new tests

**Wave 1 — Core Module + Context Assembly** (PR #2059)
- `extensions/gsd/core.rkt`: Command dispatch (/plan, /go, /replan, /skip,
  /reset, /gsd), tool guard, write guard
- `extensions/gsd/context-bundle.rkt`: Role-specific context assembly
  (explorer/executor/verifier), bundle size warnings
- `extensions/gsd/steering.rkt`: Mode-aware stall detection, only active
  during executing, consecutive-identical-reads trigger
- 48 new tests

**Wave 2 — Plan Validation + Wave Executor** (PR #2060)
- `extensions/gsd/plan-validator.rkt`: Strict validation before /go.
  Error rules (no waves, missing title, no files) block execution.
  Warning rules (no verify, no root-cause) allow with notice.
- `extensions/gsd/wave-executor.rkt`: Wave lifecycle tracking with error
  recovery (DD-5). Failed waves don't block subsequent waves.
- 21 new tests

**Wave 3 — Prompts + Bash Detection + Write Guard Hardening** (PR #2061)
- `extensions/gsd/prompts.rkt`: 5 prompt templates (exploring, executing,
  wave-failure, verifying, status)
- `extensions/gsd/bash-detect.rkt`: File-read bypass detection for sed,
  cat, head, tail, awk, python open (DD-2)
- Write guard hardened with path normalization (DD-6)
- 37 new tests

**Wave 4 — Migration + Backward Compatibility** (PR #2062)
- `extensions/gsd-planning-state.rkt` rewritten as thin shim over new modules
- Legacy API preserved: gsd-mode maps idle↔#f, exploring↔planning
- Multi-step transitions for legacy direct planning→executing paths
- `interfaces/sdk.rkt`: gsd-status recognizes 'idle as inactive
- 11 integration tests, all 207 legacy tests pass unchanged


## v0.23.2 — 2026-04-26

### GSD Planning Architecture Remediation (6 waves)

**Wave 0 — Thread-Safe State Foundation** (PR #1983)
- Extract all shared state into `gsd-planning-state.rkt` with semaphore protection
- Fix C1: Atomic `decrement-budget!` for concurrent read tracking
- Fix C2: Replace `make-parameter` with box+semaphore for `pinned-planning-dir`
- Add `reset-all-gsd-state!` for full atomic reset
- 23 new tests for thread safety

**Wave 1 — Invisible Budget Warning Fix** (PR #1984)
- Fix C3: Move budget warning from `tool-call-pre` (args, invisible) to `tool-result-post` (result content, visible)
- Tool guard now only handles pass/block decisions
- Budget warning fires at ≤5 remaining, works for all read-only tools
- 5 new tests + 1 updated test

**Wave 2 — Lifecycle Management & Logging** (PR #1985)
- Register `session-shutdown` hook for cleanup (resets all state)
- Add `log-debug` at every mode transition for easier debugging
- 4 new tests

**Wave 3 — Prompt Constants & Artifact Registry** (PR #1986)
- Extract prompt magic numbers into named constants
- Fix I1: Allow `planning-read` during `/go` (align prompt with behavior)
- Fix I6: Add REVIEW and ANALYSIS to artifact registry
- 8 new tests + 1 updated test

**Wave 4 — Integration Test Suite** (PR #1987)
- New `test-gsd-planning-integration.rkt` with 15 full-pipeline tests
- Tests cover concurrent budget, warning visibility, lifecycle, hard block
- These would have caught the C2 parameter→box and C3 invisible warning bugs

**Wave 5 — Version Bump**
- Bump 0.23.2 → 0.23.2

**Total**: 176 GSD tests across 7 files. 0 failures.

## v0.23.2 — 2026-04-26

### GSD Planning Hardening (5 waves)

**W0 — Planning/Execution Boundary Enforcement**
- `gsd-mode` parameter: `#f` → `'planning` → `'plan-written` → `'executing`
- `gsd-tool-guard` blocks edit/write/bash after PLAN written
- `gsd-tool-guard` blocks planning-write during execution
- `tool-call-pre` hook integration

**W1 — Dynamic Edit Limit**
- `current-max-old-text-len` parameter (was constant 500)
- Raised to 1200 during `/go` execution
- Reset to 500 during `/plan`

**W2 — Redundant Read Detection**
- Per-file read count tracking via `tool-result-post` hook
- Hint injected after 3+ reads of the same file
- Resets on `/plan` and `/go` transitions

**W3 — /go Budget Counter**
- 30 read-only call budget per `/go` session
- Warning at ≤5 remaining, block at <−3 overage
- Tracks read/grep/find/ls/glob calls

## v0.23.2 — 2026-04-25

### Feature Gap Closure — pi→q Parity (4 waves, 16 features)

**W0 — Context Safety**
- G5.2: Tool output truncation with temp-file overflow (`~/.q/output-overflow/`)
- G2.3: Context file discovery (AGENTS.md) wired into context-builder
- G3.1: TUI message queue during streaming (enqueue-followup when busy)
- G2.1: Proactive auto-compaction (verified — already complete)

**W1 — Session Mastery**
- G1.1: Interactive tree browser overlay (↑↓ navigate, Enter/f fold, q/Esc close)
- G1.2: Session resume (verified — already complete)
- G1.3: Fork/clone subscriber (verified — already complete)
- G8.1: Session info display (verified — already complete)

**W2 — UX Enhancements**
- G3.2: `@` file reference expansion with Tab key
- G3.3: `!!` inline bash expansion (repeats last prompt)
- G8.4: Cost tracking ($ per model) displayed in TUI status bar
- G9.3: Print mode (`-p`/`--print`) for non-interactive plain-text output

**W3 — Extension System**
- G6.2: Hot-reload `/reload` command for extensions
- G3.4: Permission gates for tool execution (auto-approved vs needs-approval)
- G6.3: State persistence (verified — already complete)
- G2.4: Custom compaction (verified — already complete)

**Test baseline**: 382 files, 5966 tests all pass.

## v0.23.2 — 2026-04-25

### /plan Exploration Cap + Context Usage Visibility (4 waves)

**W0 — /plan Exploration Cap**
- Replaced 'Do NOT limit exploration' with 30-tool-call budget in `/plan` prompt
- Prevents 110+ turn exploration spirals (observed in live session)

**W1 — Context Usage in Events + TUI Status Bar**
- Added `tokenCount` to `context.assembled` and `context.built` events
- TUI status bar now displays estimated context token usage (e.g. "23K")
- `ui-state` gains `context-tokens` field

**W2 — /plan Overwrite Stale Plans**
- Added OVERWRITE directive to planning-system-prompt
- Detects existing PLAN.md and injects stale warning on `/plan <text>`
- Prevents LLM from merging old+new plan content

**W3 — Version Bump**
- Version 0.23.2 → 0.23.2

## v0.23.2 — 2026-04-25

### Budget Counter & Steering Resilience (4 commits)

- **Added**: Budget counter — explore-vs-implement tracking in iteration loop
  (`steering.budget`, `steering.budget-soft`, `steering.budget-hard` events)
- **Added**: Error-wrapped steering injection — steering messages wrapped in
  structured error context for robust delivery
- **Added**: `/go` prompt hardening — implement-only directives, anti-exploration
  budget, no re-reading plan during execution
- **Added**: `/plan` prompt hardening — actionable plans with root causes,
  `old-text` snippets, line numbers for precise implementation
- **Fixed**: `make-text-part` arity mismatch in intent-without-action steering

## v0.23.2 — 2026-04-25

### Edit Tool Hardening — Corruption Prevention (4 waves)

**W0 — Post-Edit Integrity Check + Backup**
- 500-character `old-text` limit to prevent large-block edits
- Post-edit line-count delta check with ±2 tolerance and auto-revert
- Pre-edit backup saved to `~/.q/edit-backups/` (last 10 per file)
- Updated prompt-guidelines to mention safeguards

**W1 — Recovery Spiral Breaker**
- Consecutive error tracking in iteration loop (resets on success)
- Steering injection at 6+ consecutive tool errors ("re-read + git revert")
- Bash-only streak detection (10+ consecutive bash calls)
- Steering injection to break bash spirals ("use edit tool instead")
- Events: `spiral.error-warning`, `spiral.bash-only-warning`, `spiral.bash-breaker`

**W2 — Test Coverage**
- 6 new edit tests: 500-char limit, boundary acceptance, backup creation, line-count integrity
- 3 new iteration tests: spiral event structure, multi-tool paths, seen-path dedup

**W3 — Version Bump**
- Version 0.23.2 → 0.23.2

## v0.23.2 — 2026-04-25

### Edit Tool Hardening — Hallucination Prevention (4 waves)

**W0 — Path Normalization Fix**
- Fixed `expand-home-path` producing double-slash paths (`/home/user//file`)
- Uses `simplify-path` to normalize expanded home paths

**W1 — Edit Tool Near-Match Hints + Stronger Descriptions**
- Added `find-nearest-match` helper using longest-common-substring for fuzzy line matching
- When `old-text` not found, edit tool now shows the nearest matching line with line number
- Updated edit tool description: "old-text MUST be copied verbatim from a prior read result"
- Added `prompt-guidelines` to edit tool registration warning against guessing

**W2 — Test Coverage**
- 6 new near-match tests: nonexistent text, close-but-wrong text, multiple candidates, whitespace differences
- 2 new registry tests: prompt-guidelines set, description contains "verbatim"

**W3 — Version Bump**
- Version 0.23.2 → 0.23.2

## v0.23.2 — 2026-04-25

### Extension Tool Fix & Steering Improvement (3 bugs, 3 waves)

**W0 — Extension Tool Arity Fix (Bug A)**
- Fixed 8/16 extension tool handlers that crashed with arity mismatch
- Central wrapper in `dynamic-tools.rkt`: all extension handlers now accept `(args exec-ctx)`
- Defense-in-depth: added `[exec-ctx #f]` to all 8 broken handlers in gsd-planning.rkt and github-integration.rkt
- Affected tools: planning-read, planning-write, gh-issue, gh-pr, gh-milestone, gh-board, gh-wave-start, gh-wave-finish

**W1 — Exploration Steering Fix (Bug B)**
- Replaced write-tools whitelist with read-tools blacklist approach
- Any tool not in `('read 'find 'grep 'ls 'planning-read)` now resets the steering counter
- Extension tools (planning-write, bash, gh-*) no longer waste iteration budget

**W2 — Planning Preamble Fix + Integration Tests (Bug C)**
- Updated planning-system-prompt: removed exploration encouragement, added 5-call exploration limit
- Added `test-extension-tool-dispatch.rkt`: 5 integration tests verifying scheduler dispatch

## v0.23.2 — 2026-04-25

### CI Workflow Hardening (6 root causes, 5 waves)

**W0 — Merge Post-Release into Release workflow**
- Merged `post-release.yml` smoke tests into `release.yml` as sequential `smoke` job
- Eliminated Release→Post-Release race condition (7/7 failures before this fix)
- `release.yml` now has 3 sequential jobs: `test` → `release` → `smoke`

**W1 — Fix metrics fragility**
- Excluded `__pycache__/`, `.git/` from `rkt-files` in `scripts/metrics.rkt`
- Fixes CI vs local count mismatches that caused cascading failures

**W2 — Normalize workflows to composite action**
- `benchmark.yml`: upgraded to `checkout@v6`, removed redundant `Install dependencies` step
- `setup-racket` composite action: added stale bytecode cleanup step

**W3 — CI dry-run + workflow validation**
- Added `workflow-lint` job: validates all workflow YAML + composite actions on every PR
- Added `release-dry-run` job: builds tarball, verifies manifest + notes generation on every PR

**CI Status**: All workflows green (CI ✅, Release ✅, Benchmark ✅)

## v0.23.2 — 2026-04-25

### Project Review Remediation (55 findings across 6 axes)

**W0 — Critical Quick Fixes**
- **Fixed**: Moved `runtime/model-defaults.rkt` → `llm/model-defaults.rkt` (upward import fix)
- **Fixed**: README tool count corrected 10→13, status block updated
- **Fixed**: SSH `StrictHostKeyChecking=no` replaced with `accept-new` + configurable `ssh-strict-mode`
- **Fixed**: `GH_PAT` and `_PAT$` patterns added to secret sanitization

**W1 — Architecture Remediation**
- **Added**: `extensions/ui-surface.rkt` — parameter-based callback layer for UI decoupling
- **Removed**: `util/package-audit.rkt` re-export shell (ARCH-03)
- **Removed**: `agent/types.rkt` deprecated facade — all importers migrated to `util/protocol-types.rkt`

**W2 — Security Hardening**
- **Added**: `validate-base-url` and `safe-base-url` in model-registry (SSRF prevention)
- **Added**: `valid-ssh-host?` in ssh-helpers (shell injection prevention)
- **Added**: Cumulative write budget (50MB default) in write tool

**W3 — Code Quality**
- **Added**: `with-logged-catch` and `with-cleanup` macros in `util/errors.rkt`
- **Extracted**: `extensions/github/helpers.rkt` from monolithic `github-integration.rkt`

**W4 — Test Health**
- **Added**: `flush-trace-logger!` — eliminates timing-dependent trace tests
- **Added**: `tests/test-frontmatter.rkt` — 8 skill frontmatter tests
- **Added**: `tests/test-iteration-edge-cases.rkt` — 9 edge-case tests
- **Added**: `tests/helpers/temp-fs.rkt` — temp file/directory test macros
- **Added**: `reset-process-count!` for test isolation in sandbox/limits

**W5 — Documentation Refresh**
- **Fixed**: README metrics updated to current values
- **Updated**: All verified-against markers to v0.23.2

## v0.23.2 — 2026-04-25

### CI Tooling & Test Guard Improvements

- **Added**: CI log preservation — failure summary + test log artifact on CI failure (#1767)
- **Added**: CI readiness lint — stray file, symlink, and gitignore hygiene checks (#1764)
- **Fixed**: Replaced hardcoded tool counts with range checks in tests (#1761)
- **Added**: CI-aware test guards — shared `ci-detection` helper, out-of-repo lint (#1758)
- **Added**: `check-deps.rkt` for dependency completeness verification (#1755)
- **Added**: Version + metrics lint in pre-commit hook (#1752)
- **Fixed**: Pipeline test fixture fixes, metrics sync, CI lint failures

## v0.23.2 — 2026-04-25

### Self-Hosting Workflow Gaps

- **Fixed**: Extension tools now register correctly (GAP-2) — `register-tools` hook
  dispatch passes proper `extension-ctx?` to handlers instead of raw `(hasheq)`.
  All 9 extension register-tools handlers updated to 2-arg signature `(ctx payload)`.
- **Added**: Subagent children can execute tools (GAP-1) — 7 child-safe tools
  (read, write, edit, bash, grep, find, ls) registered with recursive tool dispatch
  loop in `run-subagent-loop`. Tool calls are dispatched via `run-tool-batch`
  instead of returning `'stopped`.
- **Added**: Slash command handlers for github-integration (`/milestone`, `/issue`, `/pr`)
  and racket-tooling (`/fmt`, `/check`, `/expand`) extensions (GAP-3).
- **Fixed**: Removed phantom `racket-find-files` reference from guardrails skill (M1).
- **Improved**: GitHub projects skill now prefers extension tools over bash+curl (M2).
- **Added**: SDK extension wiring integration tests (`test-sdk-extensions.rkt`).
- **Added**: Spawn-subagent tool dispatch tests (`test-spawn-subagent-tool-dispatch.rkt`).
- **Added**: Extension tool registration tests (`test-extension-tool-registration.rkt`).

## v0.23.2 — 2026-04-24

### Tool Error Feedback & Agent Loop Improvements

- **Tool error feedback with schema hints** (`tools/tool.rkt`, `tools/scheduler.rkt`):
  New `format-tool-schema-hint` function generates one-line parameter hints.
  Scheduler preflight captures exception detail and appends usage hint to error messages.
- **Auto-retry skips permanent tool errors** (`runtime/auto-retry.rkt`):
  New `permanent-tool-error?` predicate. Validation failures are never retried,
  saving iteration budget for actual transient errors.
- **Context seeding with project file tree** (`runtime/project-tree.rkt`):
  New module generates shallow directory tree injected into system prompt.
  LLM can skip initial `find`/`ls` exploration, saving 1-2 iterations per task.
- **Benchmark scoring improvements** (`scripts/benchmark/scorer.rkt`):
  Partial credit scoring: N/M checks pass → N/M × max score.
  New `must_not_contain` negative content checks reduce score on violation.
  Trace-based iteration counting on timeout via `tool.call.started` events.
- **Benchmark selective tool registration** (`scripts/benchmark/executor.rkt`):
  Excludes `session-recall` and `skill-router` from benchmark tool set.
  Directory fixture copy fixed to copy contents into existing tmp-dir.

## v0.23.2 — 2026-04-24

### Benchmark Suite Hardening

- **Executor pipeline fixes** (`scripts/benchmark/executor.rkt`): Register default tools
  after making tool registry, fix timeout arithmetic (seconds not ms), channel-based
  concurrency for thread-safe result passing, use correct `history-length` field
- **Task fixture wiring** (`scripts/benchmark/task.rkt`): New `fixtures_dir` field on
  `benchmark-task` struct, auto-copies fixture files (single file or directory) into temp
  project dir before execution
- **Tool name migration**: All 12 task JSONs updated from pi tool names
  (`read_file`, `edit_file`, `run_shell`) to q tool names (`read`, `edit`, `bash`)
- **CLI scoring pipeline** (`scripts/run-benchmark.rkt`): Scoring wired into CLI runner —
  each task scored across 5 dimensions, verdicts shown in human and JSON output,
  progress messages on stderr for clean JSON piping
- **Trace schema fix** (`scripts/benchmark/scorer.rkt`): Tool name extraction updated
  from OpenAI format (`type: "tool_use"`) to q format (`phase: "tool.call.started"`)
- **65 benchmark tests passing** across 6 test files

## v0.23.2 — 2026-04-24

### Systematic Live Benchmark Suite

- **Live benchmark executor** (`scripts/benchmark/executor.rkt`): Mock and live execution
  modes using SDK runtime with configurable provider and model
- **Enhanced task format** (`scripts/benchmark/task.rkt`): JSON task definitions with
  category, difficulty (1-3★), scoring spec, setup/teardown, and file fixtures
- **Five-dimension scoring engine** (`scripts/benchmark/scorer.rkt`): Correctness (40%),
  Tool Discipline (20%), Efficiency (15%), Skill Compliance (15%), No Regressions (10%)
  with PASS/PARTIAL/FAIL verdicts
- **Report generator** (`scripts/benchmark/report.rkt`): Human-readable and Markdown
  output, per-dimension breakdown, comparison between reports
- **12 benchmark tasks**: 4 implementation, 3 bug-fix, 3 planning/analysis, 2 full-workflow
  with 7 fixture files containing deliberate bugs
- **Baseline management** (`scripts/benchmark/baseline.rkt`): Version-pinned baseline
  capture and loading for regression tracking
- **Comparison engine** (`scripts/benchmark/compare.rkt`): Regression detection (>10 point
  drop) and improvement detection (>10 point gain) with human-readable output
- **CLI runner** (`scripts/run-benchmark.rkt`): `--live`, `--mock`, `--task-dir`, `--output-dir`,
  `--keep-on-failure`, `--provider`, `--json`, `--summary` flags
- **CI workflow** (`.github/workflows/benchmark.yml`): Runs on release tags with trace
  artifact upload and GitHub step summary
- **Benchmark README** (`scripts/benchmark/README.md`): Usage guide and scoring docs
- **32 new tests**: 14 task, 21 scorer, 11 report, 5 baseline, 8 comparison

## v0.23.2 — 2026-04-23

### Release & Polish

**Wave 1 — Self-hosting validation suite** (PR #1661)
- Valid GSD artifact name validation
- Skill router tool-result contract
- Extension registry loading and listing
- Event bus pub/sub
- Dogfood task format validation
- 5 tests

**Wave 2 — Documentation** (PR #1662)
- `docs/self-hosting.md`: GSD planning, dogfood infrastructure, extension loading
- `docs/workflow-testing.md`: test structure, mock provider patterns, conventions

**Wave 3 — Version bump 0.23.2 → 0.23.2** (PR #1663)
- Version bump and CHANGELOG update

## v0.23.2 — 2026-04-23

### Sandbox & Safety

**Wave 1 — Dogfooding infrastructure** (PR #1658)
- New `scripts/run-dogfood-session.rkt`: task loader, validator, mock runner
- New `scripts/capture-regression.rkt`: baseline capture and regression comparison
- Dogfood task format spec (JSON) with setup/teardown
- 3 sample tasks: basic-file-ops, planning-workflow, skill-routing
- 11 tests

**Wave 2 — Dogfood execution + analysis** (PR #1659)
- Mock execution trace validation
- Baseline capture and comparison workflow tests
- Regression detection (pass/fail cases)
- Multi-task comparison
- 6 tests

**Wave 3 — Version bump 0.23.2 → 0.23.2** (PR #1660)
- Version bump and CHANGELOG update

## v0.23.2 — 2026-04-23

### Context-Aware Exploration Steering

**Wave 1 — Same-file dedup + raised thresholds** (PR #1611)
- Raise exploration steering thresholds: gentle 5→8, strong 7→12, hard cap 12→20
- Add same-file dedup: reading the same file multiple times counts as 1 exploration step
- New `extract-tool-target-path` and `update-seen-paths` helpers exported for testing
- 18 comprehensive steering tests in `tests/test-steering.rkt`

**Wave 2 — Configurable thresholds** (PR #1612)
- Steering thresholds now configurable via `config.json` under `steering` key
  - `steering.gentle_threshold` (default: 8)
  - `steering.strong_threshold` (default: 12)
  - `steering.hard_cap` (default: 20)
  - `steering.same_file_dedup` (default: true)
- New accessors in `runtime/settings.rkt`: `steering-gentle-threshold`, `steering-strong-threshold`, `steering-hard-cap`, `steering-same-file-dedup?`
- 10 config tests added to `tests/test-steering.rkt`

## v0.23.2 — 2026-04-23

### GSD Planning Workflow + Review Cleanup

Milestone #92 — Post-v0.23.2 review follow-ups and planning prompt augmentation.

**Wave 1 — Review cleanup + test coverage (#1596)**
- Removed all `/tmp/q-cmd-dispatch.log` diagnostic tracing from 4 files.
- Replaced with `log-debug` where useful, removed entirely where not.
- Fixed stale `-> void?` contract comment in `loader.rkt` (now returns `boolean?`).
- Restricted `/plan <text>` submit to `/plan` and `/p` only —
  `/state` and `/handoff` always display artifact regardless of trailing text.
- Added 5 tests for execute-command with/without args.

**Wave 2 — Planning prompt augmentation (#1599)**
- Defined `planning-system-prompt` constant with GSD planning instructions.
- When `/plan <text>` submits, agent prompt is augmented with planning preamble
  instructing it to write a structured plan to `.planning/PLAN.md`.
- Display text shows "Planning: <original>" without full preamble.
- Added test verifying augmented submit text contains `[gsd-planning]` preamble.

**Wave 4 — Fix pre-existing test failures (#1605)**
- Synced all version surfaces: info.rkt, README.md, docs/*.md, wiki-src/ to 0.23.2.
- Added `.planning/` and `.pi/` to `lint-version.rkt` skip list (historical version refs).
- Synced README metrics (source line counts).
- Fixed `test-tui-enter.rkt`: updated expected command return format
  from `(command quit)` to `(command quit "/quit")`.
- All 3 previously-failing tests now pass: 348/348 files, 5629/5629 tests.

## v0.23.2 — 2026-04-23

### Extension Commands & Activation Fix

Milestone #91 — Fix extension activation path, command dispatch plumbing,
and gsd-planning execute-command handler for end-to-end `/activate` → command workflow.

**Wave 1 — Fix project-dir in /activate and /deactivate (#1588)**
- `handle-activate-command`: Changed project-dir source from `(path-only session-dir)`
  to `(current-directory)` — q always starts with cwd set to project root.
- `handle-deactivate-command`: Same fix.
- Removed dead `session-dir` bindings and `not project-dir` branches.
- Tests updated with `(parameterize ([current-directory tmp-dir]) ...)`."

**Wave 2 — Extension command dispatch plumbing (#1589)**
- Added `extension-registry-box` field to `cmd-ctx` (10th) and `tui-ctx` (14th).
- Wired extension-registry from `run-modes.rkt` through `tui-init.rkt` to `tui-ctx`.
- In `process-slash-command`, unknown commands dispatch via `'execute-command`
  extension hook before falling back to "Unknown command" error.
- Updated all test `cmd-ctx` constructors for new field arity.

**Wave 3 — gsd-planning execute-command handler + hot-load (#1590)**
- Added `'execute-command` hook point to `hook-action-schemas` in `util/hook-types.rkt`.
- `gsd-planning.rkt`: Added `handle-execute-command` handler for `/plan`, `/state`,
  `/handoff` — reads artifact content and returns via `hook-amend`.
- `commands.rkt`: Added `try-hot-load-extension` — after `/activate`, loads the
  newly activated extension into the running session registry.
- 4 new tests for execute-command handler.

## v0.23.2 — 2026-04-23

### Review Remediation

Milestone #90 — Security hardening, extension system integrity, and broken
registration fixes from v0.23.2 review.

**Wave 1 — Fix broken extension registrations (#1573)**
- `remote-collab/remote-collab.rkt`: Fixed `ext-register-tool!` from 2-arg
  `(ctx (make-tool ...))` to 5-arg `(ctx name desc schema handler)` form
- `session-export.rkt`: Same arity fix — unwrapped `make-tool` into direct args
- Added tool-registration tests for both extensions

**Wave 2 — Security hardening (#1576)**
- **Shell injection eliminated** in `github-integration.rkt`: Replaced `/bin/sh -c`
  with arg-list `subprocess` pattern (same as `racket-tooling.rkt`). All command
  construction now passes args directly — no shell interpolation.
- **Input validation consistency**: `valid-number?` applied to all issue/PR/milestone
  number params. `valid-state?`/`valid-method?` applied to all state/method branches.
- **Rsync args fixed** in `q-sync.rkt`: `sync-pi-config` and `sync-scripts` now
  concatenate trailing `/` into the path string instead of passing `/` as a separate
  rsync argument (which rsync interprets as "sync root directory").
- **Backup safety** added to `q-sync.rkt`: `--backup --backup-dir=.rsync-backup`
  on all rsync calls.
- **Path traversal fix** in `extension-catalog.rkt`: Added `valid-extension-name?`
  whitelist (`[a-zA-Z0-9_-]+`) before all `build-path` calls in `activate-extension!`
  and `deactivate-extension!`. Same validation wired into TUI `/activate` handler.
- Regression tests for `../../foo`, `foo/bar`, `..`, `.hidden` traversal attempts.

**Wave 3 — Extension system integrity (#1580)**
- **EXTENSIONS_INVENTORY.md**: Rewritten to list all 8 extensions (was 4).
  Corrected tool counts (16), command counts (6), versions, and API versions.
  Removed loadable extensions from "supporting infrastructure" table.
- **Tier validation wired**: `extension-tier-valid?` called during `load-extension!`
  in `loader.rkt`. Default tier is `hooks`; violations are logged as warnings.
- **Catalog type fix**: `list-active-extensions` now uses `#f` for `source-path`
  instead of symbol `'active` (violates struct contract).
- **`/activate --global` UX**: Returns usage error when no name provided after `--global`.
- **`/deactivate` TUI command**: New slash command for symmetry with `/activate`.
  Supports `/deactivate <name>` (project-local) and `/deactivate --global <name>`.

**Tests**: +22 new tests across 4 test files. 347 files, 5631 tests, 0 failures.

---

## v0.23.2 — 2026-04-23

### Extension Discovery & Activation

- **Removed `.pi/extensions/` loading**: TypeScript pi extensions are incompatible
  with q's Racket `dynamic-require` loader. The loader no longer scans
  `<project>/.pi/extensions/`.
- **Added `~/.q/extensions/` global directory**: Extensions placed in
  `~/.q/extensions/` are loaded for all projects. Global extensions load
  first; project-local `<project>/.q/extensions/` overrides global.
- **Extension catalog module** (`runtime/extension-catalog.rkt`): New module
  providing `known-extensions-dir`, `list-known-extensions`,
  `list-active-extensions`, `activate-extension!`, `deactivate-extension!`.
  Supports flat files and subdirectory extensions (e.g., `remote-collab`).
- **`/activate` TUI command**: New slash command for in-session extension
  management:
  - `/activate` — show active + available extensions
  - `/activate <name>` — activate extension in project-local dir
  - `/activate --global <name>` — activate in `~/.q/extensions/`
  - `/activate --available` — list all known extensions from source tree

**Tests**: +18 new tests (4 run-modes, 13 extension-catalog, 5 activate-command,
3 cmd-ctx arity fixes). 347 files, 5609 tests, 0 failures.

---

## v0.23.2 — 2026-04-23

### Audit Remediation

Security and robustness fixes from comprehensive audit of v0.23.2–v0.23.2
(remote pi implementation). 25 findings addressed: 5 CRITICAL, 7 MAJOR, 13 MINOR.

**CRITICAL fixes:**
- **C1**: Fixed `ext-register-tool!` arity — all 7 extensions now use 5-positional-arg
  form instead of wrapping `make-tool` inside a 2-arg call
- **C2**: Replaced `system/exit-code` with `subprocess` in `racket-tooling.rkt`,
  eliminating shell injection via filenames with metacharacters
- **C3–C5**: Added input validation to `github-integration.rkt` — whitelist `state`,
  `method` params; validate `issue_number`, `pr_number`; fixed regex from `#rx` to
  `#px` for `(?:...)` non-capturing groups

**MAJOR fixes:**
- **M2**: `gsd-planning.rkt` rejects path traversal in artifact names (`/`, `..`, null bytes)
- **M3**: `session-export.rkt` HTML-escapes all user content (`&`, `<`, `>`, `"`)
- **M4**: `gh-wave-finish` checks exit codes at every step (PR merge, checkout, pull, close)
- **M5**: Removed hardcoded default host from `q-sync.rkt` — `remote_host` now required
- **M6**: Replaced rsync `--delete` with `--backup --backup-dir=.rsync-backup`
- **M7**: `remote-collab.rkt` validates session names (alphanumeric + hyphens only)

**MINOR fixes:**
- **m1**: Rewrote `find-form-end` with proper depth tracking, string escape handling,
  semicolon comment skipping, and square bracket support

**Regression tests:** 30+ new tests across 6 test files. 168 tests pass.

---

## v0.23.2 — 2026-04-23

### Phase E: Polish

- **ext-package-manager**: Wraps `runtime/package.rkt` as a tool. Actions: list,
  install, remove, info for extension packages.
- **image-input**: Multi-modal image support. Base64 encodes images (PNG, JPEG,
  GIF, WebP) and constructs multi-modal messages for vision-capable LLMs.
- **session-export**: Export session JSONL logs to HTML, JSON, or Markdown.
  Supports output to file or inline text result.

### Testing

- 16 tests across 3 Phase E extensions
- All existing tests continue to pass

---

## v0.23.2 — 2026-04-23

### Phase D: GSD Skills

All 15 GSD skills verified present in `.pi/skills/`:

- **Core skills**: q-gsd-orchestrator, q-gsd-milestone-builder, q-gsd-reviewer,
  q-gsd-project-reviewer, q-gsd-bug-orchestrator, q-gsd-bugfix-builder
- **Builder skills**: q-gsd-foundation-builder, q-gsd-core-builder, q-gsd-tools-builder,
  q-gsd-runtime-builder, q-gsd-interfaces-builder, q-gsd-hardening-builder
- **Support skills**: q-gsd-docs-orchestrator, q-gsd-skillsmith, q-gsd-testing-improver
- **GitHub skills**: q-gsd-github-init, q-gsd-github-issues, q-gsd-github-branches, q-gsd-github-projects

No new Racket code — skills are pure markdown discovered by existing skill system.

---

## v0.23.2 — 2026-04-23

### Phase C: Remote Collaboration Extension

- **C1**: `remote-collab` extension — multi-file extension using subdirectory
  support (B3). `remote-q` tool controls remote q instances via SSH + tmux.
  Actions: status, start, send, capture, wait, interrupt, stop.
  `ssh-helpers.rkt` and `tmux-helpers.rkt` as separate modules.
- **C2**: `q-sync` extension — multi-domain sync tool. Directions: push, pull,
  status, handoff. Domains: planning, pi-config, scripts, git, all.
  Uses `rsync` for file sync, `git` for version control.

### Testing

- 11 tests for remote-collab extension
- 8 tests for q-sync extension
- All existing tests continue to pass

---

## v0.23.2 — 2026-04-23

### Phase B: Self-Editing & Extension Infrastructure

- **B1**: `racket-tooling` extension — 3 tools for structural Racket editing:
  `racket-check` (format, syntax, test, expand, all), `racket-edit` (9 modes:
  replace, form, skeleton, struct-add-field, provide-append, cond-insert-clause,
  match-insert-clause, rewrite-form, constructor-add-arg), `racket-codemod`
  (pattern/template with @@PLACEHOLDER matching). All modes validate with
  `raco fmt` + `raco make`, revert on failure.
- **B2**: `compact-context` extension — agent-invocable context compaction tool
  that reads `.planning/` state and injects it into the compaction context.
- **B3**: Extension subdirectory support — `discover-extensions` now scans
  `extensions/<name>/<name>.rkt` and `extensions/<name>/main.rkt` for
  multi-file extensions, alongside existing flat `.rkt` files.

### Testing

- 16 tests for racket-tooling extension
- 10 tests for compact-context extension
- 3 new tests for extension subdirectory discovery
- All existing tests continue to pass

---

## v0.23.2 — 2026-04-23

### Phase A: Foundation Extensions

- **A1**: `spawn-subagents` tool — parallel batch execution with output aggregation
  (up to 3 concurrent subagent processes, structured results, partial failure handling)
- **A2**: `gsd-planning` extension — registers `planning-read` and `planning-write` tools
  plus `/plan`, `/state`, `/handoff` slash commands
- **A3**: `github-integration` extension — registers 6 GitHub tools:
  `gh-issue`, `gh-pr`, `gh-milestone`, `gh-board`, `gh-wave-start`, `gh-wave-finish`
  Uses `gh` CLI via subprocess with `gh-binary-path` parameter for test injection
- **A4**: `skill-route` tool — skill discovery by description match, full content loading
  Actions: `list`, `match`, `load`. Registered in `registry-defaults.rkt`

### Testing

- 60 tests for github-integration extension (mock `gh` CLI)
- 11 tests for skill-route tool
- All existing tests continue to pass

---

## v0.23.2 — 2026-04-22

### Critical Fixes (from PROJECT_REVIEW_v0.23.2)

- **SEC-07**: Fix `subprocess-result` arity bug — error handler had `#f` nested inside
  `inexact->exact` call instead of being the 6th field (`truncated?`). Any subprocess
  execution failure (command not found, permission denied) would crash with arity error.
  1-line fix: move `#f` to correct position.

### Documentation Fixes

- **D1-D3**: Rewrite wiki Architecture Overview — stale metrics (124→228 modules, 140→349
  test files), removed reference to deleted `runtime/resource-loader.rkt`, corrected provider
  API description from `complete`/`stream-complete` to `make-provider` dispatch protocol.
- **D4**: Add missing ADR index entries for 0008 (safe-mode enforcement),
  0009 (credential redaction), 0010 (streaming port lifecycle).

### Housekeeping

- Bump version references across all docs to 0.23.2

## v0.23.2 — 2026-04-22

### Architecture Hardening & Documentation Refresh

Milestone #81 — 17 issues, 11 PRs merged. Full review findings in
`.planning/REVIEW-v0.23.2.md` (73 findings: 6 CRITICAL, 23 MAJOR, 32 MINOR, 12 NIT).

#### Wave 0 — Housekeeping (#1475, #1477, #1488)
- Version drift sync, STATE.md + SUMMARY.md reconciliation

#### Wave 1 — Shell Injection Fix (#1474)
- FFI `getpid` via isolated submodule (avoids `ffi/unsafe` → `racket/contract` conflict)
- `/proc/<pid>` filesystem check for `pid-alive?` (container-safe)
- `truncated?` field on subprocess results

#### Wave 2 — Azure Hardening (#1479)
- `dynamic-wind` port cleanup for streaming generators
- Shared response parser extracted from Azure-specific code
- Configurable request timeout

#### Wave 3 — Safe-Mode Enforcement (#1482)
- Symlink resolution in path validation
- One-shot lock via box parameter
- `dangerous?` field on tool descriptors

#### Wave 4 — Error Consolidation (#1478, #1489)
- Eliminated duplicate `provider-error` struct definitions
- Unified in `llm/provider-errors.rkt`

#### Wave 5 — Sandbox, Credentials & OAuth (#1485, #1483, #1486)
- SHA-256 HMAC for credential verification
- Opaque credential structs with `gen:equal+hash` + `gen:custom-write`
- OAuth scope separator: `+` → `%20` (spec-compliant)
- OAuth stubs now raise errors instead of silently returning `#f`

#### Wave 6 — CI Pipeline (#1480)
- `scripts/ci-local.rkt`: 10 automated checks (format, compile, imports, security lint)
- `scripts/lint-security.rkt`: hardcoded secret scanner with exemption patterns

#### Wave 7 — Documentation Refresh (#1476, #1484)
- 7 source files updated, 3 ADRs added (0008–0010)
- CHANGELOG backfilled from git history

#### Wave 8a — Iteration.rkt set! Fix (#1481)
- Replaced 3x `set!` with `let-over-cond` binding in exploration escalation

#### Wave 9 — Test Infrastructure (#1487)
- Event bus concurrency test: mutex-protected counter via `call-with-semaphore`
- Subprocess `truncated?` field tests (overflow vs. fits budget)

#### Wave 10 — Port Cleanup & Quality (#1490)
- `dynamic-wind` port cleanup in `anthropic.rkt` and `gemini.rkt` streaming
- Fixed double warning in `load-session-log`
- O(n²) → O(n) `jsonl-read-all-valid-with-count` via `cons`/`reverse`
- Idempotent `cancel-token!` guard

### Metrics
- 332 test files, 5307 tests passing
- 10/10 CI local checks
- 3 new ADRs (0008-safe-mode, 0009-credential-redaction, 0010-streaming-port-lifecycle)

## v0.23.2 — 2026-04-21

### Bug Fixes
- **P1**: Detect silent stream EOF — emit synthetic `model.stream.completed` with
  `finish_reason: "eof"` when API closes without finish chunk (BUG-SILENT-STREAM-EOF)
- **P2**: Strengthen Level 1 exploration steering — "Consider" → "You MUST now"
  (BUG-STEERING-LEVEL1-WEAK)
- **P2**: Detect intent-without-action pattern — if model says "I'll rewrite" but
  no tool call follows, inject steering nudge capped at 1 retry (BUG-INTENT-WITHOUT-ACTION)
- **P0**: Steering messages use `'user` role instead of `'system` (fixes 400 errors)
- **P0**: Context builder preserves tool_call/tool_result pairing during truncation
- **P0**: Tiered context builder preserves system-instruction and first user message
- **P1**: Index rebuild infers missing parentIds from log order (fixes context amnesia)

## v0.23.2 — 2026-04-21

### Trace Logger Hardening
- **[P0]** Fix malformed JSONL: added `sanitize-for-json` to recursively convert
  non-jsexpr values (event structs, procedures) to safe string representations
  before `write-json`, preventing partial writes that corrupt the trace file
- **[P0]** Wrap `write-json` in error handler to skip non-serializable events
  gracefully instead of crashing
- **[P0]** Fix `model` field in `model.request.started` event: use string instead
  of `(object-name provider)` symbol for safe JSON serialization
- Raise circuit breaker threshold from 5 to 100 (sanitization now prevents the
  most common failure mode; high threshold is safety net only)
- 4 new tests: struct sanitization, 100 rapid events, procedure values,
  circuit breaker resilience

### max_tokens Event Timing Fix
- **[P1]** Resolve max-tokens from multiple config paths: top-level,
  `providers.<name>.max-tokens`, `models.default.max-tokens` — not just the
  flat runtime config hash which never contains it
- Import `setting-ref*` for nested config path resolution in iteration.rkt

### Stream Timeout Tuning
- **[P1]** SSE stream timeout formula: `max(120, timeout/4)` → `max(180, timeout/2)`
- For glm-5.1 (request=900s): 225s → 450s, preventing premature SSE timeouts
  during slow model generation

### Exploration Steering Escalation
- **[P2]** 3-level escalation: gentle nudge at 5, strong at 7, hard cap at 12
  consecutive read-only tool calls
- Tool-type-aware counting: counter resets when file writes detected
  (write/edit/replace/create tools)
- Hard cap emits `exploration.hard-cap` event for observability

## v0.23.2 — 2026-04-21

### Request-Cycle Trace Logger Module

Structured diagnostic trace of every LLM request cycle for post-mortem debugging.
Disabled by default — zero overhead when off. Enable via `logging.trace.enabled` in
config.json.

**Core**: New `runtime/trace-logger.rkt` subscribes to event bus, writes `trace.jsonl`
per session with sequence numbers, ISO 8601 timestamps, and full event data.
Flush-on-write for crash safety.

**Enriched events**:
- `model.request.started` now includes `model`, `max_tokens`, `settings`
- `model.stream.completed` now includes `finish_reason` (stop/length/tool_calls)
- `stream-chunk` struct now has 6th field `finish-reason` for actual API value
- New `iteration.decision` event at each loop iteration with termination, consecutive_tools

**Config**: `logging.trace.enabled` (boolean), `logging.trace.max-files` (int, default 10)

**CLI**: `q sessions trace <id>` — formatted, `--json` raw, `--summary` counts

- `runtime/trace-logger.rkt`: New trace logger module (#1452)
- `llm/model.rkt`: Added `finish-reason` field to `stream-chunk` (#1453)
- `llm/stream.rkt`: Pass finish_reason through normalizers (#1453)
- `agent/loop.rkt`: Enriched request/completed events (#1453)
- `runtime/iteration.rkt`: New `iteration.decision` event (#1453)
- `runtime/settings.rkt`: `trace-enabled?`, `trace-max-files` (#1454)
- `wiring/run-modes.rkt`: Wire trace logger into startup (#1454)
- `interfaces/sessions.rkt`: `q sessions trace` command (#1455)

## v0.23.2 — 2026-04-21

### Config Validation + Iteration Budget + Provider Settings Wiring

**P1**: Invalid `config.json` silently fell back to mock provider with only a
WARNING. Now `config-parse-error` in `settings.rkt` detects broken JSON and
`provider-factory.rkt` prints a clear ERROR with file path and fix instructions
before falling back to mock.

**P1**: Slow models (glm-5.1) hit the default `max-iterations=20` from
exploration overhead. Default soft limit raised 20→50. Hard limit now
calculated as `max(soft*1.6, 80)` instead of matching soft limit. After 8+
consecutive tool calls without file writes, a steering message is injected:
"Focus on producing the actual output using the write or edit tool now."

**P2**: Provider settings (e.g. `max-tokens`) from `config.json` never reached
the API request body. Settings are now threaded through `run-provider-turn` →
`run-agent-turn` → `make-model-request` → `openai-build-request-body`.

- `settings.rkt`: `config-parse-error` function for JSON validation (#1444)
- `provider-factory.rkt`: Clear error messages on broken config (#1444)
- `agent-session.rkt`: Default `max-iterations` 20→50 (#1445)
- `iteration.rkt`: Hard limit formula + exploration steering hint (#1445)
- `loop.rkt`: `#:provider-settings` param in `run-agent-turn` (#1446)
- `iteration.rkt`: Config threaded to `run-provider-turn` (#1446)

## v0.23.2 — 2026-04-21

### Second-Prompt Crash + SSE Timeout + Streaming Text Fix

**P0**: Fixed `hash-set` contract violation that crashed any second prompt. Production
runtime uses mutable config hash (`make-hash`), but `hash-set` requires immutable.
Now detects hash mutability and uses `hash-set!` or `hash-set` accordingly.

**P1**: SSE stream timeout now scales with per-model request timeout. Previously
hardcoded at 60s between chunks, causing timeouts on slow models (e.g. glm-5.1
with `request: 900`). Stream timeout = `max(120, request/4)` seconds.

**P2**: Partial streaming text preserved on error. When SSE timeout or other error
fires during model streaming, accumulated grey text is now committed to the
transcript as a partial assistant entry before clearing.

- `agent-session.rkt`: Mutable/immutable config hash detection (#1438)
- `openai-compatible.rkt`: Scaled SSE stream timeouts (#1439)
- `tui/state.rkt`: Partial streaming text preservation (#1440)
- 10 new/updated tests across 3 test files

## v0.23.2 — 2026-04-20

### Retry Robustness & TUI Crash Fix

**Wave 0 (P0 TUI crash)**: Wrapped both TUI runner thread call sites with
`with-handlers exn:fail?` that emit `runtime.error` + `turn.completed` events.
Defense-in-depth `turn.completed` in `agent-session.rkt`. New test file:
`test-tui-error-recovery.rkt` (6 tests).

**Wave 1 (P1 retry budgets)**: Per-type retry budgets (`timeout=2`,
`rate-limit=4`, `provider-error=2`) via `#:per-type-budgets` keyword argument.
`retry-exhausted` struct gains `error-history` field tracking all error types
across retries. Agent session includes `errorHistory` in `runtime.error` payload.
State module renders recovery hints from full error history (mixed-type detection).
4 new tests in `test-auto-retry.rkt`.

**Wave 2 (P2 work preservation)**: `/retry` command enriched with previous
attempt's tool summary. New `get-last-turn-tool-summary` in `tui/state.rkt`.
Modified `/retry` handler to include `[Context from previous attempt: ...]`.
8 new tests in `test-retry-enrichment.rkt`.

**Wave 3 (P2 model timeouts)**: Per-model timeout profiles via
`current-model-timeouts` parameter and `effective-request-timeout-for`.
OpenAI-compatible provider extracts model name from request body and applies
per-model timeout overrides. Config schema:
`{ timeouts: { models: { "glm-5.1": { "request": 900 } } } }`.
10 new tests in `test-model-timeouts.rkt`.

## v0.23.2 — 2026-04-20

### Exploration & Generation Robustness

- **Wave 0**: Increased HTTP timeout defaults (300→600s request, 30→60s stream) to prevent premature timeouts during long generation
- **Wave 1**: Soft/hard iteration limits — `max-iterations` becomes a soft warning, `max-iterations-hard` (default = soft) is the hard stop. TUI shows `[exploring... iteration N, M remaining before hard stop]`
- **Wave 2**: Context-aware retry messages — auto-retry events now include classified error type (timeout, rate-limit, context-overflow, provider-error) for type-aware TUI display
- **Wave 3**: Exploration progress hints — after 4+ consecutive tool-only turns, shows `[exploring... N tool calls: read, bash, ...]`
- **Wave 4**: Adaptive stream timeout — doubles per-chunk SSE timeout after `stream-secs` total streaming time for long generation pauses
- **Wave 5**: Mid-turn token budget check — emits `context.mid-turn-over-budget` event when context exceeds 90% of `max-context-tokens` during tool execution

### Architecture Boundary Fixes

- **Wave 6**: Lifted TUI mock-provider detection to `provider-factory.rkt` — eliminated `tui→llm` layer violation
- **Wave 7**: Moved `runtime/resource-loader.rkt` → `extensions/resource-discovery.rkt` — eliminated `runtime→extensions` boundary violation
- **Wave 8**: Session-switch dependency injection — replaced direct `extensions/` imports with DI via `dynamic-require` + keyword args
- **Wave 9**: Removed `tui-init.rkt` from arch-boundaries test exceptions — TUI layer now has zero boundary violations

### Post-Review Fixes (Waves 10–13)

- **Wave 10**: Removed dead code in `classify-error` (R1) — no-op `when` block. Fixed `rate-limit-error?` pattern — replaced `"too many"` with `"too many requests"` to prevent context-overflow misclassification (R2)
- **Wave 11**: Fixed README v0.23.2 status block — replaced v0.13.x description with accurate v0.23.2 features (D1). Synced metrics (D2)
- **Wave 12**: Added 12 TUI event handler tests covering `iteration.soft-warning`, `exploration.progress`, `context.mid-turn-over-budget`, and `auto-retry.start` with `errorType` (TC1)
- **Wave 13**: Added `session-rebind` to `hook-action-schemas` (H1). Wrapped `dynamic-require` with descriptive error messages in `session-switch.rkt` (SW1). Added argument validation in `resource-discovery.rkt` (RD1)

### Metrics
- 315 test files, 5365 tests, 0 failures
- Remaining runtime exceptions: `iteration.rkt` (documented ARCH-01), `package.rkt` (manifest audit)

## v0.23.2 — 2026-04-20

### Context Manager Architecture

Replaces mechanical context truncation with a strategy-driven context assembly engine. The session log is now immutable — the context manager decides what goes into the LLM context window using pluggable strategies:

1. **Pin**: System prompt + first user message (always present)
2. **Summary**: LLM-generated or concatenation summary of excluded entries
3. **Recent**: Last N tokens kept verbatim
4. **Catalog**: One-line-per-entry summary of excluded entries
5. **Budget enforcement**: Total context ≤ token budget

#### New Modules
- `runtime/context-manager.rkt` — Strategy-driven context assembly with configurable budgets, summary generation, catalog creation, and consecutive tool result collapsing
- `tools/builtins/session-recall.rkt` — `session_recall` tool: lets the agent retrieve excluded session entries by ID or range

#### Removed Modules
- `runtime/context-reducer.rkt` — Old pair-aware mechanical trimming, fully replaced by context-manager

#### Features
- **Session Recall Tool** (#1391): Agent can now retrieve excluded context entries via `session_recall(id="...")` or `session_recall(range="from..to")`. Returns formatted message details.
- **LLM Summary Generation** (#1395): Structured summary template with caching. Falls back to concatenation when no LLM provider is available.
- **Summary Integration** (#1396): Excluded entries auto-summarized and injected as `compaction-summary` messages between pinned and recent context.
- **Catalog Token Caps** (#1394): Catalog entries capped at 40 entries / 2K tokens. Consecutive tool results collapsed to single entry.
- **Budget Enforcement**: Catalog dropped first, then summary truncated, recent window last. Pinned items never dropped.

#### Bug Fixes
- **Flaky CI tests fixed**: `test-bump-version.rkt` and `test-ci-local.rkt` now dynamically read current version instead of hardcoding. (#1393)
- **Metrics lint drift**: Resolved stale prose counts causing intermittent CI failures.

### Metrics
- 312 test files, 5,358+ tests passing, 0 failures
- 224+ source modules

---

## v0.23.2 — 2026-04-20

### Bug Fixes
- **Removed context reduction from retry path**: `#:context-reducer` parameter removed from `with-auto-retry`. Retries now always use the same context — no trimming, no reduction. Eliminates P0 class of 400 errors from malformed reduced context after retry trimming. (#1388, PR #1389)

---

## v0.23.2 — 2026-04-20

### Performance
- **TUI transcript O(n²) → O(1)**: Transcript append now uses `cons` instead of `append`, eliminating quadratic slowdown on long sessions. Added `transcript-entries` accessor that reverses on read for backward-compatible oldest-first ordering. (#1386, PR #1387)

### Bug Fixes (from v0.23.2)
- **Settings contract**: Fixed `make-settings` field contracts that rejected valid values. (#1376)
- **Context reducer pair-awareness**: Context reduction now correctly handles paired tool-start/tool-end entries. (#1377)
- **`/retry` + iteration limit**: `/retry` command now correctly updates `last-prompt-box`. Max iterations raised from 10 to 20. (#1378, PR #1383)
- **Newline bleed**: Fixed trailing newline bleed in assistant message rendering. (#1379)
- **Prompt pinning**: First user message now survives context truncation via `pin-first-user` helper. (#1380, PR #1384)
- **Progressive timeout**: Fixed timeout recovery for long-running tool calls. (#1381)
- **O(n²) audit**: Documented all O(n²) hotspots in TUI codebase for future remediation. (#1382, PR #1385)

### Metrics
- 325 test files, 68,903 test lines, 10,786 assertions
- 224 source modules, 41,753 source lines
- 5,330+ tests passing (full suite)

---

## v0.23.2 — 2026-04-20

### Features
- **Session tree navigation**: Navigate between parent/child sessions
- **Context reducer**: Progressive context trimming for long conversations
- **Extension power-user API**: Advanced extension hooks for tool/loop interception
- **Provider improvements**: Better error handling, retry logic, timeout tuning

### Bug Fixes
- Settings contract fixes for `make-settings` field validation
- Context reducer pair-awareness for tool-start/tool-end entries
- `/retry` + iteration limit interaction
- Newline bleed in assistant message rendering
- First user message prompt pinning

---

## v0.23.2 — 2026-04-19

### Features
- Extension power user API
- Session tree navigation
- SDK foundations

---

## v0.11.x — 2026-04-19

### Features
- **Tool scheduler**: Priority-based tool execution with concurrency limits
- **Extension hooks v2**: Lifecycle hooks for tool dispatch, loop iteration, and session events
- **Credential store**: Centralized API key management with environment variable support
- **OAuth framework**: OAuth 2.0 authorization flow (stubs for token exchange/refresh)
- **Safe-mode guard**: One-shot safe-mode lock for restricting dangerous operations

### Infrastructure
- CI local lint suite (8 checks)
- Security lint for hardcoded secrets
- Compatibility matrix documentation

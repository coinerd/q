## v0.96.3 — Architecture Audit Hotfix (2026-06-08)

### Architecture
- **AF1 (dead imports) — RESOLVED**: Removed 8 dead import entries from `turn-orchestrator.rkt` (33→25 imports).
- **AF2 (misleading comment) — RESOLVED**: Fixed `format-iso-now` comment in `memory-tools-shared.rkt`.
- **AF3 (behavioral tests) — RESOLVED**: Added 3 behavioral test cases for `symbol->task-state` and `hash->session-config`.
- **AF4 (dead imports) — RESOLVED**: Removed `dispatch-hooks` + `hook-result?` from `extension-setup.rkt`.
- **AF5 (stability markers) — RESOLVED**: Added `STABILITY: public` markers to 4 PUBLIC event definitions.
- **AF6 (dead imports) — RESOLVED**: Removed 10 unused shared imports from `memory-tools.rkt` facade.

### Metrics
| Metric | Before (v0.96.2) | After (v0.96.3) | Change |
|--------|------------------|-----------------|--------|
| turn-orchestrator.rkt imports | 33 | 25 | -24% |
| Behavioral tests for extracted fns | 0 | 3 | +3 |
| Event stability markers | 0 | 4 | +4 |

## v0.96.2 — Module Sizing + Low-Priority Cleanups (2026-06-08)

### Architecture
- **F4 (memory-tools god module) — RESOLVED**: Split 928-line `memory-tools.rkt` into 5 focused modules:
  - `memory-tools-shared.rkt` (316 LOC) — shared helpers + event emitters
  - `memory-tools-store.rkt` (213 LOC) — store/update handlers
  - `memory-tools-query.rkt` (89 LOC) — search/list handlers
  - `memory-tools-manage.rkt` (314 LOC) — delete/clear/consolidate/cleanup
  - `memory-tools.rkt` (130 LOC) — thin registration facade (-86% LOC)
- **F6 (stream-from-provider complexity) — DEFERRED**: Single 200-line function with deeply nested state. Extraction assessed as high-risk, low-reward. Existing tests provide safety net.
- **F7 (wire-session-event-handlers complexity) — DEFERRED**: Similar assessment.
- **F8 (build-runtime-from-cli) — BELOW THRESHOLD**: 166 LOC, under 200-line limit. No action needed.
- **F10 (session config accessors) — ALREADY ADEQUATE**: 24 hash-ref accessors documented and consistent.
- **F11 (format-iso-now duplicate) — DOCUMENTED**: Thin wrapper retained for compatibility.
- **File headers**: Added to 7 files missing them (json-mode, rpc-mode, grep, firecrawl, scrollback, tui-init, frame-diff).

### Metrics
| Metric | Before (v0.96.1) | After (v0.96.2) | Change |
|--------|------------------|-----------------|--------|
| memory-tools.rkt LOC | 928 | 130 | -86% |
| Files without headers | 7 | 0 | -7 |
| God-modules (>500 LOC) | 14 | 13 | -1 |

## v0.96.1 — Representation Hiding + Parameter Reduction (2026-06-08)

### Architecture
- **F2 (Event struct-out over-exposure) — DOCUMENTED**: Classified 60 event types as PUBLIC (4) vs INTERNAL (56). PUBLIC events marked with stability commitment. Assessment: `struct-out` for `#:transparent` immutable structs is idiomatic Racket; explicit provides would be more verbose with no security benefit.
- **F3 (Parameter proliferation) — RESOLVED**: Audited all 129 production parameters. Dead `circuit-breaker-state` removed. Already at ≤130 target. Classification: 42 function-arg candidates, 27 groupable, 36 keep-as-is.

### Metrics
| Metric | Before (v0.96.0) | After (v0.96.1) | Change |
|--------|------------------|-----------------|--------|
| Production parameters | 130 | 129 | -1 |
| Event types classified | 0 | 60 | +60 |
| Dead parameters | 1 | 0 | -1 |

## v0.96.0 — Turn Orchestrator Decomposition (2026-06-08)

### Architecture
- **F1 (God Module) — RESOLVED**: `turn-orchestrator.rkt` reduced from 534 LOC / 42 imports to **286 LOC / 24 imports** (47% LOC reduction, 43% import reduction).
- **F5 (166-line Function) — RESOLVED**: `build-assembled-context` decomposed into 4 clear phases via named helpers.
- Extracted `runtime/context-assembly/turn-context.rkt` (279 LOC): `symbol->task-state`, `assemble-context/pure`, `prepare-turn-context-state`, `emit-context-assembly-events!`, `current-last-task-fsm-state`.
- Extracted `runtime/extension-setup.rkt` (51 LOC): `register-session-extensions!`.
- 18 unused imports cleaned up from turn-orchestrator.rkt.

### Metrics
| Metric | Before (v0.95.21) | After (v0.96.0) | Change |
|--------|-------------------|-----------------|--------|
| turn-orchestrator.rkt LOC | 534 | 286 | -46% |
| turn-orchestrator.rkt imports | 42 | 24 | -43% |
| build-assembled-context LOC | ~169 | ~42 (coordinator) | -75% |
| New modules | 0 | 2 | +2 |

## v0.95.21 — Memory Lifecycle Completion (2026-06-08)

### New Features
- **G1 — Background Reflection Trigger**: Auto-reflection now fires per-turn after auto-extraction when `memory.auto-reflection.enabled` is set to `true`. Non-fatal wrapper `maybe-reflect-session-memories!` catches all exceptions. Defaults to disabled.
- **G2 — User-Scope Config Wiring**: User-scope memory can be enabled via `memory.user-scope.enabled` config key. Policy updated at session startup from config.

### Configuration Keys
- `(memory auto-reflection enabled)` — boolean, default `false`
- `(memory auto-reflection min-items)` — integer >= 2, default `5`
- `(memory user-scope enabled)` — boolean, default `false`

### Verification
- 18 lifecycle tests, 18 policy tests passing
- All new features default off — no behavioral change for existing users

## v0.95.20 — Memory Tools Event Publisher Fix (2026-06-08)

### Bug Fixes
- **Event publisher arity mismatch**: `publish-memory-event!` in `memory-tools.rkt` called the event publisher with 1 arg (combined hash) but `tool-coordinator.rkt` provides a `(event-type payload)` 2-arg lambda. All memory tool events failed with arity mismatch, leaking errors into the TUI prompt area.

### Verification
- 493 focused memory tests passing
- 1 new regression test verifying 2-arg publisher contract
- 5 test publisher lambdas updated from 1-arg to 2-arg

## v0.95.19 — Memory Injection Hotfix (2026-06-07)

### Bug Fixes
- **HF1 (HIGH)**: Thread `#:session-config config` to `build-tiered-context/state-aware` in `turn-orchestrator.rkt` — enables memory injection in production context assembly path
- **HF2 (HIGH)**: Change default `#:scope` from `'session` to `#f` in `observe-memory-for-context`, `inject-memory-for-context`, and `observe-memory-telemetry` — project and user-scoped memories now visible during retrieval
- **LF1 (Low)**: Check `gen:store-memory!` result in `reflect-session-memories!` — failed stores no longer produce phantom reflection items
- **LF2 (Low)**: Thread `session-id`/`project-root` from payload into `decode-mem0-items` — Mem0 external backend uses actual query context instead of hardcoded values

### Verification
- 174+ focused memory tests passing across 10+ test files
- 4 new regression tests (3 previously failing, now green)

## v0.95.18 — Memory Quality Remediation (2026-06-07)

### Regression Harness
- **W0**: Red/regression harness and validation baseline — 9 test files mapping F1-F9 audit findings (4 pass, 5 expected-fail)

### Transport & Extraction Fixes
- **W1**: Mem0 fail-closed retrieval and transport hardening — propagate transport errors before Mem0-specific decode; auth header fix (`Api-key` not `Token`); error sanitization (10 tests)
- **W2**: Blank `[Auto]` auto-distillation and state preamble filter — `make-deterministic-fallback` treats blank/whitespace content as absent; `build-state-awareness-preamble` filters bare `[Auto]` conclusions (14+21 tests)
- **W3**: Validation gate repair and behavioral extraction evidence — structural invariant proving `run-agent-turn` has no LLM-response path bypassing auto-extraction (7 tests)

### Consolidation & Reflection
- **W4**: Consolidation lineage and policy hardening — target-type validation; supersession metadata (`superseded-by`) replaces silent deletion; partial failure reporting (80 tests)
- **W5**: Reflection determinism and conservative grouping — min-group-size raised to 3; shared-tags requires >=2 tags; Jaccard threshold raised to 0.4; fully deterministic IDs (45 tests)

### Release Gate
- **W6**: Version bump, CHANGELOG, docs update, focused memory gate validation

### New Files
- `tests/test-memory-regression.rkt` — F1-F9 regression harness
- `tests/test-memory-consolidation-tool-g3.rkt` — consolidation edge-case tests

## v0.95.17 — Memory Quality (2026-06-06)

### Characterization & Bug Fixes
- **W0**: Characterization tests for remaining gaps — tool-call turn extraction, non-streaming hook, Mem0 transport, consolidation tool (4 test files)
- **W1**: Fix tool-call turn extraction — `maybe-auto-extract-after-response!` now fires for all response types, not just text-only turns
- **W2**: Verified non-streaming extraction path — `run-agent-turn` always delegates to streaming path

### Features
- **W3**: Mem0 HTTP transport — real `http-sendrecv` transport replacing `not-implemented` stub, sanitized error messages (7 tests)
- **W4**: `consolidate-memory` tool — merge multiple items into one with supersedes lineage, optional original deletion (6 tests)
- **W5**: Deterministic memory reflection — group session items by shared tags/Jaccard overlap, produce project-scope reflections (16 tests)

### Integration & Release
- **W6**: Documentation updates, changelog, version bump, focused memory test validation

### New Files
- `runtime/memory/reflection.rkt` — deterministic session-to-project reflection
- `tests/test-memory-tool-turn-extraction-g1.rkt`, `tests/test-memory-nonstreaming-extraction-g1.rkt`
- `tests/test-mem0-http-transport-g2.rkt`, `tests/test-memory-consolidation-tool-g3.rkt`
- `tests/test-memory-reflection-w5.rkt`

## v0.95.12 — Modular Memory System (2026-06-03)

### Memory System Foundation
- **W1**: Protocol and types foundation — `memory-item`, `memory-query`, `memory-result`, `memory-backend` structs with validators (14 tests)
- **W2**: Deterministic in-memory hash backend — `memory-hash-backend` with store/retrieve/update/delete/list (24 tests)
- **W3**: Disabled-by-default config wiring — `config-memory-backend`, `config-memory-enabled?` (9 tests)

### Memory Tools and Events
- **W4**: Explicit memory tools with policy gating — `store_memory`, `search_memory`, `delete_memory` with sensitivity/scope checks (22 tests)
- **W5**: Memory events — `memory.stored`, `memory.retrieved`, `memory.deleted` typed events (13 tests)
- **W9**: Management UX tools — `list_memory` (inspect), `clear_memory` (scoped batch delete with confirmation) (14 tests)

### Context Integration
- **W6**: Observe-only memory context retrieval — `observe-memory-for-context` gathers memory without injection (18 tests)
- **W7**: Bounded memory prompt injection — `inject-memory-for-context` with budget/framing/safety (18 tests)

### Persistence and Backends
- **W8**: Project-local JSONL memory backend — append-only file backend with snapshot loading (14 tests)
- **W10**: Auto-extraction alpha — optional post-response fact extraction, disabled by default, secret pattern blocking (17 tests)
- **W11**: Chained L1/L2 backend with write-through and dedup; external backend adapter skeleton with redaction (26 tests)

### Release Hardening
- **W12**: Release gate tests — validates disabled-by-default, no hidden writes, safe tool responses (14 tests)
- All 14 memory test suites passing (196 total tests)
- Memory is **disabled by default** — no persistent writes unless explicitly configured

### New Files
- `runtime/memory/types.rkt`, `runtime/memory/protocol.rkt`, `runtime/memory/policy.rkt`
- `runtime/memory/backends/memory-hash.rkt`, `runtime/memory/backends/file-jsonl.rkt`
- `runtime/memory/backends/chained.rkt`, `runtime/memory/backends/external-protocol.rkt`
- `runtime/memory/auto-extraction.rkt`
- `runtime/context-assembly/memory-builder.rkt`
- `tools/builtins/memory-tools.rkt`
- `agent/event-structs/memory-events.rkt`
- 14 test files in `tests/test-memory-*.rkt`


## v0.95.16 — Memory Gap Closure

### Auto-Extraction Integration
- **W1**: Config/session-config accessors for auto-extraction
- **W2**: Runtime parameter wiring in build-runtime-from-cli
- **W3**: Post-turn lifecycle hook maybe-auto-extract-after-response! in loop-stream.rkt

### Backend Configuration
- **W4**: Recursive backend factory — chained backends from json config specs
- **W5**: Mem0 external adapter with injectable transport

### Memory Management
- **W6**: Deterministic management.rkt module with dedup, expiry, dry-run

### Context Injection Polish
- **W7**: Tiered injection with type/scope grouping and untrusted header

### Release
- **W8**: Documentation, focused test gate, milestone closure
- 24 files changed, +1860/-78 lines, 79 new tests

## v0.94.9 — GUI-TUI Output Parity (2026-06-03)

### GUI Enhancements
- **W1**: Typed GUI messages — `kind` field on `gui-message` (message, tool-start, tool-end, tool-fail, thinking, system, error)
- **W1**: Extended `gui-state` with `context-info`, `cost`, `model` fields
- **W2**: Tool argument + result display with separate entries and per-kind coloring
- **W3**: Event coverage — thinking, retry, compaction, exploration, goal, session events
- **W4**: Rich status bar — context %, cost, active goal in GUI status
- **W5**: Markdown rendering — headers, lists, numbered lists, inline code parsing

### Tests
- test-gui-typed-messages.rkt: 15 tests
- test-gui-tool-display.rkt: 11 tests
- test-gui-event-coverage.rkt: 18 tests
- test-gui-status-bar.rkt: 8 tests
- test-gui-markdown-render.rkt: 11 tests

### PRs
- #7109 (W1), #7110 (W2), #7111 (W3), #7112 (W4), #7113 (W5)

## v0.93.2 — Browser Feature Audit Closure (2026-06-03)

### Security Fixes (Critical)
- **F1**: All 10 browser tools blocked in safe-mode
- **F2**: Absolute scheme deny-list (file://, data://, javascript://)
- **F3**: IPv6 address classification (blocks loopback, link-local, ULA, mapped)

### Architecture Fixes (High)
- **F4**: Service layer dispatches through adapter interface
- **F5**: load-browser-settings reads from config
- **F6**: Explicit browser tool risk tiers in permission gate
- **F7**: browser-service field on exec-context
- **F8**: Browser lifecycle wired into agent-session

### Completeness Fixes (Medium)
- **F9**: emit-browser-event! publishes real events to bus
- **F10**: Browser events re-exported from event-structs facade
- **F11**: Heartbeat thread in Playwright adapter (30s ping)
- **F17**: Removed unused imports from audit.rkt

### Quality Fixes (Low)
- **F15**: Removed dead viewport schema entries from browser_open
- **F16**: Added browser-url-blocked-error? and browser-adapter-error? predicates

### Documentation
- **F13**: Updated overview.md to v0.93.2 with browser/ layer
- **F14**: Updated README to 27 tools (17 core + 10 browser)

### Tests
- 49 new tests across 6 new test files + 8 existing files updated
- Total browser tests: 282+

## v0.93.1 (2026-06-03)

### Final Audit Closure (Phase 7)

- Post-implementation audit: APPROVED — 0 blocking findings
- Architecture: layer boundaries clean (browser/ imports only agent/event + util)
- Security: default-deny URL policy, dangerous tool classification, safe mode integration
- Tests: 240 browser tests across 14 test files
- Documentation: browser-guide.md + ADR 0020
- **Browser Feature Complete**: 11 source modules, 10 registered tools, Playwright sidecar

## v0.92.2 (2026-06-03)

### Workflow Tool + Documentation (Phase 6)

- Add `browser/workflow.rkt`: `browser_check_local_app` composite tool
  - Open URL → observe → screenshot → close → return composite result
  - Returns: url, title, text, console_errors, loaded_successfully, load_time_ms, screenshot
- Register `browser_check_local_app` as MEDIUM risk in tool registry
- Add `docs/browser-guide.md`: feature overview, architecture, tool reference, security model
- Add `docs/adr/0020-browser-feature-architecture.md`: sidecar rationale, JSONL protocol, adapter contract
- 15 workflow + integration tests
- **240 total browser tests**

## v0.91.3 (2026-06-03)

### Playwright Sidecar + Real Adapter (Phase 5)

- Add `sidecars/playwright/`: Node.js/Playwright sidecar with JSONL stdin/stdout protocol
  - 9 command handlers: navigate, click, type, press, scroll, extract, screenshot, ping, close
  - Page state extraction, screenshot capture with size limits, health check ping/pong
  - Error mapping: Playwright errors → q-browser-error categories (timeout, adapter-error, sidecar-crash, policy-violation)
  - Node.js unit tests for command validation
- Add `browser/adapters/playwright-sidecar.rkt`: Racket adapter for sidecar process lifecycle
  - JSONL IPC with UUID-based request/response and timeout
  - Zombie prevention via custodian management
  - Full browser-adapter interface implementation
- 4 new Racket unit tests + 3 integration tests (skipped without Node.js)
- **225 total browser tests**

## v0.90.2 (2026-06-03)

### Browser Tool Registry Integration (Phase 4)

- Add `tools/builtins/browser-tools.rkt`: 9 tool handlers (browser_open, browser_observe, browser_click, browser_type, browser_press, browser_extract, browser_screenshot, browser_scroll, browser_close)
- Register all 9 browser tools in `tools/registry-table.rkt` with JSON schemas and prompt guidelines
- Risk classification: click/type/press = HIGH risk (requires approval), open = MEDIUM, observe/extract/screenshot/scroll/close = LOW
- 16 new tool handler tests
- **217 total browser tests**

## v0.89.3 (2026-06-03)

### Browser Secure Service + Audit + Mock Adapter (Phase 3)

- Add `browser/adapters/mock.rkt`: deterministic mock adapter with configurable responses, call recording, error simulation
- Add `browser/audit.rkt`: JSONL audit trail for all browser actions
- Add `browser/service.rkt`: `SecureBrowserService` composing policy+session+adapter+audit with open/observe/act/navigate/screenshot/close
- Add `current-browser-service` parameter for runtime integration
- 48 new tests: 16 mock adapter, 6 audit, 26 service
- **185 total browser tests**

## v0.88.3 (2026-06-03)

### Browser Policy Engine + Session Manager (Phase 2)

- Add `browser/policy.rkt`: default-deny URL policy engine with scheme validation, IP classification (RFC 1918, cloud metadata 169.254.169.254, loopback, link-local), domain allowlist, path blocking, action risk classification
- Add `browser/settings.rkt`: 13-field `browser-settings` struct with defaults, `current-browser-settings` parameter
- Add `browser/session.rkt`: session lifecycle manager with create/destroy/get/list, max-sessions (3), max-actions-per-session (100) enforcement
- 74 new tests: 43 policy, 17 settings, 14 session

## v0.87.3 (2026-06-03)

### Browser Domain Model (Phase 1)

- Add `browser/` directory with domain types: `browser-target`, 8 `browser-action` variants, `browser-observation`, `browser-session-info` with JSON roundtrip codecs
- Add `browser/adapter.rkt` with adapter interface (open/close/navigate/observe/act/screenshot) + contract-out
- Add `browser/events.rkt` with 10 typed browser events using `define-typed-event` macro
- Add `browser` layer to `docs/architecture/dependency-policy.rktd` (forbidden from runtime/tui/extensions)
- Add `q-browser-error` branch to error hierarchy with 7 category predicates
- Register 10 `browser.*` event types in `agent/event-json.rkt`
- 63 new tests: 19 types, 13 errors, 11 events, 10 adapter, 10 contracts

## v0.86.5 (2026-06-04)

### Audit Closure (Complete Remediation)

- Fix `turn.completed` → `stream.turn.completed` in 5 test files (F1)
- Fix `enqueue-steering!` to accept string instead of message struct (F4)
- Widen `branch-summary-entry-entry-range` contract in tree-entries.rkt (F3)
- Fix `cli-config` arity 16→17 for context-profile field (F2)
- Update version markers from 0.86.3→0.86.4 across 653 references (F5)
- Add `turn-orchestrator.rkt` risk note to dependency-policy.rktd (F5)
- Fix `record_conclusion` tool name in permission-gate.rkt (F5)
- Fix stale file paths in test-arch-fitness.rkt (F5)
- Fix truncate-string ellipsis mismatch in tui/state test (F6)
- Widen `styled-line?` placeholder to accept struct instances (F9)
- Fix runner `file-has-rackunit-tests?` for self-running rackunit/text-ui files (F10)
- Add `assistant.message.completed` + `stream.turn.completed` to event registry (F11)

## v0.86.4 (2026-06-04)

### Audit Closure

Post-implementation audit closure for v0.86.xx Test Suite Remediation series.

**Test fixes (19 files):**
- Fixed stale module paths in test-skeleton.rkt (auth-store→auth/auth-store, jsonl→json/jsonl)
- Fixed hardcoded version in test-util-reclassification.rkt (0.80.4→0.86.3)
- Updated facade test for explicit provides in event-structs.rkt (test-facade-surface.rkt)
- Fixed stale require path in test-goal-state.rkt (util/message→util/message/message)
- Exported internal accessors for test-gsd-production-session-isolation.rkt
- Updated 5 moved module paths in test-arch-fitness.rkt
- Added risk notes for llm/stream.rkt and tui/commands.rkt in dependency-policy.rktd
- Fixed tool classification for save-conclusion/record-conclusion/set-task-state (permission-gate.rkt)
- Updated 11 docs from version 0.83.9→0.86.3 for lint-doc-freshness
- Widened make-branch-summary-entry contract for opaque entry-range (tree-entries.rkt)
- Fixed classify-error test assertions (test-cli.rkt)
- Fixed loop-state and event-bus args in test-effect-update-fsm-contract.rkt
- Fixed deep-merge-hash test to expect contract violation (test-settings-config-validation.rkt)
- Widened directive-recurse contracts to any/c (directive.rkt)
- Fixed gui-state struct usage in test-gui-subscriber.rkt
- Removed #:no-serialize event from all-known-event-types (event-json.rkt)
- Updated turn.completed→stream.turn.completed in test-iteration-event-golden.rkt
- Fixed enqueue-steering! string contract in test-iteration-steering.rkt

**Source improvements:**
- Added TODO tracking for any/c widenings in turn-model.rkt
- Added doc comments for internal settings accessors
- Updated dependency-policy.rktd with moved paths and new risk notes

## v0.86.3 (2026-06-03)

### Performance & Parallel Safety

**W0 (#6873): Parallel-safety fixes**
- Replaced fixed /tmp/test* paths with unique paths using (random 1000000)
- 6 files fixed: test-iteration, test-iteration-integration, test-iteration-working-set, test-iteration-transitions, test-tool-coordinator-duration, test-arch-01-regression

## v0.86.2 (2026-06-03)

### Mock Updates & Logic Fixes

**W0+W1 (#6870, #6871):**
- test-goal-checks: pipe-to-bash command for safety validation
- test-registry-defaults: tool list updated 14→17
- test-firecrawl: truncate-string format fix
- test-pipeline-smoke: accept stream.turn.completed

## v0.86.1 (2026-06-03)

### Contract & Structural Drift Remediation

**W0 (#6864): cli-config arity fix (9 calls)**
- Updated cli-config calls from 16→17 args (context-profile field) in 2 test files

**W1+W2 (#6865, #6866): Contract drift and event rename fixes**
- turn-model.rkt: widen payload contracts from hash? to any/c
- http-helpers.rkt: widen translate-stop-reason contract to any/c
- context-policy.rkt: fix estimate-message-tokens-cached string estimator
- retry-policy.rkt: relax dict-ref TR boundary to avoid opaque-value error
- tool-coordinator.rkt: guard provider string check before make-minimal-settings
- test-golden-flows.rkt: accept stream.turn.completed alongside turn.completed
- test-iteration.rkt: fix enqueue-steering! to pass strings not messages

**W3+W4 (#6867, #6868): Structural and stale test fixes**
- Fixed 10 test files: preamble changes, struct arity, queue API, event types
- Updated architecture tests: file paths, command list, thresholds

## v0.86.0 (2026-06-03)

### Test Foundation: Module Paths, Exports, False Positives

**W0 (#6859): Module path fixes (12 files)**
- Updated all stale require paths from v0.83-0.85 restructuring
- util/protocol-types.rkt → util/message/protocol-types.rkt (6 files)
- runtime/session-config.rkt → runtime/session/session-config.rkt
- runtime/iteration/ → agent/iteration/ (2 files)
- runtime/context-assembly.rkt → runtime/context/context-assembly.rkt
- util/event.rkt → util/event/event.rkt

**W1 (#6860): Duplicate identifier fixes (3 files)**
- Fixed tool? import conflicts in test-tool-coordinator-phases, test-tool-result-unique
- Fixed truncate-string conflict in tui/builtins.rkt

**W2 (#6861): Missing export fixes (2 source files)**
- Exported gsd-session-ctx-state-box, gsd-session-ctx-sem from session-state.rkt
- Exported q-settings-global, q-settings-project from settings.rkt

**W3 (#6862): False positive investigation (5 files)**
- Confirmed: all 5 files pass individually or hang under raco test (no failure)

## v0.85.3 (2026-06-03)

### God Struct Decomposition

**W0 (#6837): Create lifecycle-state struct**
- Create runtime/session/lifecycle-state.rkt with 9 mutable lifecycle fields
- Add lifecycle field to agent-session struct
- Backward compatible: all existing callers unaffected

**W1 (#6838): Migrate callers, remove old fields**
- Remove 9 lifecycle fields from agent-session (25→16 fields)
- Define compatibility accessors that delegate to lifecycle-state
- agent-session-compacting?, -shutdown-requested?, etc. still work
- Update all test helpers for new 16-field struct layout

**Acceptance:**
- `raco make q/main.rkt` — clean compile
- 90+ session/lifecycle/compaction tests pass
- `agent-session` struct now 16 fields (was 24)

## v0.85.2 (2026-06-03)

### Layer Dependency Cleanup

**W0 (#6833): Create util/types/ forwarding (A1-02)**
- Create util/types/session-config.rkt, session-types.rkt, working-set.rkt
- Update agent/iteration/loop-state.rkt to import from util/types/ instead of runtime/
- Agent layer now has ZERO runtime/ imports

**W1 (#6834): Break wiring↔TUI coupling (A1-03)**
- Create runtime/gsd-query.rkt with current-gsd-mode-query parameter
- Create runtime/command-registry-bridge.rkt re-exporting 5 TUI functions
- Wiring layer now has ZERO tui/ imports

**W2 (#6835): Tighten wiring contracts (C2-03)**
- build-runtime-from-cli: any/c → cli-config? → session-config?
- mode-for-config: any/c → cli-config? → symbol?
- reload-config!: any/c → session-config? → (values session-config? model-registry?)
- Add 7 contract tests for wiring/run-modes

## v0.85.1 (2026-06-03)

### Typed Racket Migration & Facade Assessment

**W0 (#6829): Preflight**
- Audited all 20 #lang typed/racket modules
- Documented 40+ consumers of llm/model.rkt

**W1 (#6830): Revert llm/model.rkt from TR to racket/base (C2-07)**
- Replaced #lang typed/racket with #lang racket/base + explicit contracts
- Removed all TR type annotations (fields were all Any — zero compile-time safety)
- Added contract-out for all constructors and conversion functions
- 9 model tests, 75 conformance tests, 7 provider smoke tests pass

**W2 (#6831): Facade Assessment (C2-08)**
- Assessed tools/tool.rkt facade — KEEP as stable public API
- Verified: zero consumers bypass facade to import sub-modules directly

## v0.85.0 (2026-06-03)

### Test Quality & Quick Wins

**W0 (#6825): Security Test Gaps (T3-01, T3-08)**
- Fix weak path-traversal assertion in test-sandbox-security.rkt
- Add find root guard tests for /run, /snap, symlink-to-/proc
- Add grep missing-path argument error test
- Add ls edge-case tests (broken symlink, invalid sort-by, unreadable dir)

**W1 (#6826): Test Quality Improvements (T3-02, T3-05)**
- Add injectable #:clock parameter to event-bus (default: current-seconds)
- Replace sleep(1.1) with injectable fake clock in event-bus tests
- Enhance provider smoke tests with provider? and export checks

**W2 (#6827): Provide Hygiene (A1-07, A1-08)**
- Replace all-from-out in extensions/gsd/core.rkt with explicit list
- Replace all-defined-out in extensions/gsd/event-structs.rkt with explicit list

## v0.84.4 (2026-06-03)

### Fixed

- **P0**: `find` tool could hang indefinitely when scanning large directory trees — now early-terminates recursion once `max-results` matches are found (BUG-1, fixes `/goal` hang at 93% CPU)
- **P1**: `find` tool now rejects scans from filesystem root (`/`) and system directories (`/proc`, `/sys`, `/dev`, `/run`, `/snap`) — returns clear error instead of walking entire filesystem (BUG-2)
- **P1**: `find` tool now has a hard scan budget of 50,000 entries — prevents unbounded traversal regardless of other parameters (BUG-3)
- **P2**: Tool execution CWD now falls back to `(current-directory)` instead of session directory when no project-dir is configured (BUG-4, `tool-coordinator.rkt`)
- `find` result metadata now includes `scanned` count for observability

## v0.84.3 (2026-06-03)

### Documentation Reconciliation

**W0 (#6805):**
- README accuracy: tool count 14 → 17, add missing subcommands
- Fix subcommands table (q sessions info/delete/trace)
- Trim Status section to 10 entries

**W1 (#6806):**
- Archive pre-v0.70 CHANGELOG entries to CHANGELOG-ARCHIVE.md
- Main CHANGELOG.md ≤ 1500 lines

**W2 (#6807):**
- Add .github/workflows/nightly.yml
- Add docs/getting-started/dev-setup.md
**W0 (#6802):**
- Replace 16 silent `(with-handlers ([exn:fail? void]) ...)` with logging
- Persistence/state errors → `log-warning` (session-lifecycle, session-persistence, lockfile)
- Cleanup/teardown errors → `log-debug` (terminal, clipboard, image-pipeline, streaming-cursor, gsd)
- OAuth errors → `log-warning` with redacted details (oauth, oauth-callback)

**W1 (#6803):**
- spawn-subagent.rkt: per-minute spawn rate limit (30/min)
- safe-mode-state.rkt: `allowed-path?` has `simplify-path` fallback when `resolve-path` fails
- write.rkt + edit.rkt: `log-warning` on path canonicalization failure

## v0.84.1 (2026-06-03)

### Contract Tightening

**W0 (#6799):**
- `build-stream-result` in `agent/loop-stream.rkt`: all params tightened (hash? event-bus? string? loop-state? tool? provider? loop-result?)
- `run-streaming-phase` in `agent/loop-dispatch.rkt`: req model-request?, tools (listof tool?), cancellation-token?, return loop-result?
- `make-tui-ctx` in `tui/context.rkt`: model-registry?, extension-registry?, queue?, box? for optional args

**W1 (#6800):**
- `turn-model.rkt`: extension-registry (or/c extension-registry? #f), payload hash?, result hash?
- `state-types.rkt`: cache key exact-integer?, busy-since (or/c exact-integer? #f)
- `context-files.rkt`: path params use path-string?
- `queue.rkt`: define queue-element? as string?, tighten enqueue/dequeue contracts
- 20 contract tests in `test-contract-tightening.rkt` (all pass)

## v0.84.0 (2026-06-03)

### Facade Purge & Version Sync

- **Remove 87 deprecated facades** (49 runtime + 38 util)
  - All runtime/*.rkt facades deleted; imports updated to canonical sub-package paths (runtime/auth/, runtime/compaction/, runtime/context/, runtime/goal/, runtime/session/, runtime/provider/, etc.)
  - All util/*.rkt facades deleted; imports updated to canonical sub-package paths (util/content/, util/error/, util/event/, util/export/, util/json/, util/message/, util/safe-mode/, util/tool/, etc.)
  - 620 files changed, 1048 insertions(+), 1399 deletions(-)
  - Zero deprecated facades remain in q/

## v0.83.12 (2026-06-03)

### TUI Status Bar Visual Gap

**W0 (#6784):**
- TUI status bar goal-state coverage: `test-tui-goal-status-bar.rkt` with 7 tests
- `handle-stream-turn-completed` preserves active-goal visual state (re-sets busy and status-message)
- `handle-context-built` accepts both `'tokenCount` and `'token-count` payload keys
- `handle-model-stream-completed` tolerates missing/`#f` usage (hash? guard, positive? check)
- `handle-goal-turn-started` sets `busy?` to `#t` and `busy-since` to event timestamp
- `handle-goal-status` sets `ui-state-status-message` in addition to transcript entry

**W1 (#6785):**
- Steering normalization: raw steering strings become `message?` values (role `'user`, meta source `'steering`) before context append
- Bash warning suppression: benign command substitution (e.g., `name=$(basename "$f")`) no longer prints destructive warning or `[CLASSIFIER-DIAG]`
- Regex-only risky commands (e.g., `source /tmp/...`) still warn/block as before

**F8 (post-milestone):**
- `inject-system-instructions` always prepends a system message, even when system instructions are empty
- Empty instructions → `""` content, accepted by OpenAI-compatible providers
- Prevents provider 500 "System message must be at the beginning" when first message was user role
- Consecutive system messages already merged by `merge-consecutive-roles` in `build-raw-messages`

## v0.83.11 (2026-06-03)

### Audit Closure Hotfix

- Fix BLOCKER: `context-event` called with keyword args in `session-lifecycle.rkt` → use `make-context-event`
  - Runtime crash on every prompt in TUI mode; status bar now correctly shows `ctx:NK`
- Fix stale test: `spawn-subagents rejects maxParallel > 3` → `clamps maxParallel > 3`
  - Test now expects success since v0.83.10 T15 changed behavior to clamp

## v0.83.10 (2026-06-03)

### Audit Closure — Runtime Fixes, Workflow Contracts, Metadata Tags

**W0 (#6775):**
- F1: Unique failure log names via `equal-hash-code` on full test path
- F2: `@boundary`, `@isolation`, `@timeout` metadata tags recognized by runner
- F3: `scenario-eof` factory for empty-stream provider testing
- F4: `make-goal-provider-per-turn-cap` for goal loop turn-limit testing

**W1 (#6776):**
- T5: `test-goal-workflow-contract.rkt` — 8 goal lifecycle workflow tests
- T7: Emit `context.built` event so TUI status bar shows token count
- T9: `stream.turn.completed` handler clears busy state in TUI
- T10: `stream_options.include_usage` in OpenAI-compatible streaming requests
- T12: Preserve `session-index` in config when setting working-set
- T13: Use `project-dir` for tool working-directory instead of log path
- T14: Wire session queue into `make-loop-config` + use `enqueue-steering!` in submit-handler
- T15: Clamp `maxParallel` instead of rejecting in `spawn-subagent`

**W2 (#6777):**
- T6: Document 6 metadata tags in `TEST_CONVENTIONS.md` with reference table
- T16: Version bump to v0.83.10

## v0.83.9 (2026-06-03)

### Series Closure, Metrics, and Release Readiness
- **Metrics report**: Before/after metrics for v0.83.xx series (10 milestones, 23 PRs, 176+ tests).
- **Test conventions doc**: `docs/TEST_CONVENTIONS.md` with suite guide, metadata tags, sandbox, scenario harnesses, gate evidence.
- **Series complete**: All 10 milestones (v0.83.0–v0.83.9) delivered.

## v0.83.8 (2026-06-03)

### Process Cleanup, Output Bounds, and Flake Burn-In
- **Unique failure log names**: `make-unique-log-name` uses path hash to avoid basename collisions.
- **Output truncation**: `truncate-test-output` caps test output at 64KB with head+tail+marker.
- **Timeout cleanup tests**: `test-run-tests-timeout-cleanup.rkt` with 7 tests.

## v0.83.7 (2026-06-03)

### Workflow Contract Suite
- **Command surface contracts**: `tests/workflows/test-command-surface-contract.rkt` with 12 tests covering cmd-ctx construction, command parsing, state transitions.
- **Provider/tool workflow contracts**: `tests/workflows/test-provider-tool-workflow-contract.rkt` with 7 tests covering single tool roundtrip, blocked tool, multi-tool ordering, result/response structure, error scenarios.

## v0.83.6 (2026-06-03)

### Goal and TUI Scenario Harnesses
- **Goal scenario harness**: `tests/helpers/goal-scenarios.rkt` with event/status capture, fake run-prompt factories, goal provider factories (no-progress, tool-timeout), shutdown check helpers.
- **Goal loop scenario tests**: `test-goal-loop-scenarios.rkt` with 15 tests covering capture, fake run-prompt, goal-run! integration (shutdown, max-turns cap, event emission, status messages).
- **TUI event pipeline**: `tests/helpers/tui-scenarios.rkt` with synchronized event capture (semaphore-based), channel pairs, frame snapshot.
- **TUI concurrency tests**: `test-tui-event-pipeline-concurrency.rkt` with 8 tests (thread-safe concurrent put!, concurrent put+read, frame snapshot).
- **TUI frame integrity**: `test-tui-frame-integrity.rkt` with 6 tests (row independence, dirty flag determinism, concurrent snapshot creation).

## v0.83.5 (2026-06-03)

### Provider and Tool-Turn Scenario Harnesses
- **Provider scenario DSL**: `tests/helpers/provider-scenarios.rkt` with `scenario-text`, `scenario-tool-call`, `scenario-multi-tool`, `scenario-streaming`, `scenario-error`, `scenario-rate-limit`, `scenario-finish-length`, `make-scenario-provider` with request capture.
- **Tool-turn scenario harness**: `tests/helpers/tool-turn-scenarios.rkt` with `turn-scenario-text/tool-call/multi-tool/blocked-tool/malformed-tool`, mock tool/registry factories, assertion helpers.
- **Shell tokenizer progress tests**: `test-shell-tokenizer-progress.rkt` with 6 tests covering 45+ special character payloads, termination, nested bracket handling, mismatched quote resilience.
- **Tests**: 16 provider scenario tests, 15 tool-turn scenario tests, 6 tokenizer tests.

## v0.83.4 (2026-06-03)

### Suite Taxonomy Migration and No-New-Drift Lints
- **Metadata-backed classification**: `slow-file?`, `tui-file?`, `mutating-file?` now check `@speed`/`@suite`/`@mutates` metadata tags before falling back to heuristics.
- **Metadata cache**: `get-file-metadata` in `scripts/run-tests.rkt` parses file headers once and caches.
- **Test architecture lint**: new checks in `scripts/lint-tests.rkt` for missing metadata on high-risk suites, local `with-temp-dir` definitions, and env mutation without `@mutates`/`@isolation` tags.
- **Tests**: `test-run-tests-metadata-classification.rkt` (9 tests), `test-test-lint.rkt` (7 tests).
- Report-only mode — no enforcement in this milestone.

## v0.83.3 (2026-06-03)

### Canonical Test Sandbox and Isolation Pilot
- **Sandbox helper**: new `tests/helpers/test-sandbox.rkt` with `with-test-sandbox`.
- Creates isolated temp project/session/home/quarantine directories.
- Parameterizes `current-directory` and HOME/XDG env vars; restores on exit.
- Cleans up temp dirs unless `preserve?` is `#t`.
- 14 self-tests covering creation, isolation, cleanup, exception paths.
- **Workflow pilot**: `tests/workflows/test-sandbox-integration.rkt` verifies sandbox coexists with existing workflow fixtures.
- **Fixture adapter**: `with-isolated-temp-dir` in `tests/helpers/fixtures.rkt` wraps sandbox for existing tests.
- No mass migration — all existing tests backward compatible.

## v0.83.2 (2026-06-03)

### Metadata Parser and Fixture Self-Test Inclusion
- **Metadata parser**: new `scripts/test-metadata.rkt` parses `@suite`, `@boundary`, `@speed`, `@mutates`, `@isolation`, `@timeout` annotations from test file headers.
- Report-only: no test selection changes. Validates values, reports warnings.
- `scan-files-metadata` and `metadata-report` for batch scanning.
- **Fixture self-tests**: `tests/**/fixtures/test-*.rkt` now selected by runner (2 files, 27 tests).
- Support modules in `fixtures/` remain excluded.
- `all` suite: 760→762 files, `workflows` suite: 24→26 files.
- 10 new metadata parser tests.

## v0.83.1 (2026-06-03)

### Runner Safety and Gate Evidence v2
- **Strict argument validation**: reject unknown suite names, `--jobs 0`/negative, `--repeat 0`/negative, `--timeout 0`/negative, unknown `--flags`.
- **Gate evidence v2**: structured JSON evidence with version, git SHA, suite, args, counts, inventory hash, timestamp.
- Guards: no evidence on empty results, zero parsed tests, or any failure.
- `run-suite-once` now returns `(values exit-code results)` for evidence capture.
- 17 new runner tests (13 arg validation + 4 gate evidence).

## v0.83.0 (2026-06-03)

### Test Truth Baseline and Inventory
- Add `--inventory` report mode to `scripts/run-tests.rkt`.
- Reports: selected/excluded files per suite, classifier hits, high-risk flags (env/cwd/temp/subprocess/perf/terminal), inventory hash.
- 9 new runner inventory tests.
- No suite selection behavior changed.

## v0.82.6 (2026-06-02)

### Goal Feature Hotfix
- **F-1 CRITICAL**: Wire `agent-session-box` through TUI context pipeline — was always `#f`, so `/goal` always failed.
- **F-2 CRITICAL**: Add `goal-cancel-box` — `/goal clear` now signals running thread to stop.
- **F-5**: Fix `--evaluator` flag parsing — only check next token after `--evaluator`, not entire goal text.
- **F-6**: Inject `GOAL-EVIDENCE-SYSTEM-PROMPT` into every goal turn for evidence-driven behavior.
- **F-3**: Integration tests for session-box wiring and cancel mechanism (5 test cases).
- **F-4**: Add TUI slash commands reference to README.
- Update `cmd-ctx` struct (12→13 fields) and `tui-ctx` struct (19→21 fields).
- Update 10 test files for new cmd-ctx constructor arity.

## v0.82.5 (2026-06-02)

### Feature Flag Activation
- Flip `current-goal-loop-enabled?` default to `#t` — goal loop is now active by default.
- Updated `docs/getting-started/goal.md` with activation note.

## v0.82.4 (2026-06-02)

### GUI Activation + Integration Tests
- Wire GUI `/goal` handler to `goal-run!` (mirrors TUI pattern).
- Feature flag guard with session-null safety.
- 2 new GUI goal tests (feature flag guard + enable path).

## v0.82.3 (2026-06-02)

### System Instruction Injection
- Prepend turn context and evidence instructions to every goal turn.
- System instructions include turn count, max turns, and goal text.

## v0.82.2 (2026-06-02)

### TUI /goal Core Activation
- Wire `goal-run!` in `handle-goal-command` replacing stub message.
- Thread spawning with `exn:fail?` handler and shutdown-check.
- Event bridge adapter for goal-runner → event bus.
- Feature flag guard (`current-goal-loop-enabled?` default `#f`).

## v0.82.1 (2026-06-02)

### Goal Wiring Foundation
- Add `agent-session-box` field to `cmd-ctx` struct for goal-runner access.
- Create `tui/commands/goal-bridge.rkt` with `make-goal-event-bridge` and `make-goal-run-prompt!`.

## v0.82.0 (2026-06-02)

### Goal Feature Foundation
- Fix `extract-transcript-from-result` to handle `loop-result?` struct.
- Add `current-goal-loop-enabled?` feature flag (default `#f`).
- Remove all facade-dependent imports in `runtime/session/`, `runtime/compaction/`, `util/message/`.

## v0.81.5 (2026-06-02)

### runtime/ Safe Sub-Packaging
- Moved 10 compaction files into `runtime/compaction/` sub-package.
- Moved 17 session core files into `runtime/session/` sub-package.
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.4 (2026-06-02)

### runtime/ Safe Sub-Packaging
- Moved 8 goal files into `runtime/goal/` sub-package.
- Moved 8 provider+auth files into `runtime/provider/` and `runtime/auth/`.
- Moved 6 context surface files into `runtime/context/` sub-package.
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.3 (2026-06-02)

### util/ Event + Error + Message Sub-Packages
- Moved 9 event files into `util/event/` sub-package.
- Moved 6 error files into `util/error/` sub-package.
- Moved 3 message files into `util/message/` sub-package (including deprecated protocol-types.rkt).
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.2 (2026-06-02)

### util/ Safe Reclassification
- Moved 20 files into 8 sub-packages with re-export facades.
- `util/export/`, `util/tool/`, `util/json/`, `util/safe-mode/`, `util/path/`, `util/fsm/`, `util/content/`, `util/extension/`
- All facades marked DEPRECATED with target removal in v0.83.

## v0.81.1 (2026-06-02)

### Protocol-Types Migration
- Migrated source imports away from `util/protocol-types.rkt` to focused canonical sub-modules.
- Added source migration guardrail test for non-test facade references.
- Marked `util/protocol-types.rkt` as deprecated with target removal in v0.82+.

## v0.81.0 (2026-06-02)

### SSE Dedup + Contract Tightening
- Azure OpenAI SSE inline loop migrated to shared stream-sse-events
- any/c contracts tightened in stream-runner.rkt (7) and loop-phases.rkt (15)
- with-safe-fallback logging + placeholder test fixes

## v0.80.5 (2026-06-01)

### Architecture Polish
- Protocol-types facade consumer migration (14 files migrated to direct imports)
- Runtime sub-package documentation in dependency-policy.rktd
- T3-1: streaming-message.rkt — replaced 5 boxes with #:mutable struct fields
- T3-9: wave-executor.rkt — replaced statuses-box with #:mutable field
- T3-5: Fixed tui-init.rkt layer violation — moved GSD wiring to wiring/run-modes.rkt
- Session config regression guards (4 tests)
- Struct mutability tests (8 tests)


## v0.79.0 (2026-06-01)

### Final Context Assembly Activation

**6 gaps resolved across 4 milestones.** All feature flags OFF by default.

#### GAP-1: CLI/Config Entry Point for Profile
- Add `setting-context-assembly-profile` to settings.rkt (reads `(context-assembly profile)` from config.json)
- Add `context-assembly-profile` to session-config known-keys
- Add `--context-profile` CLI flag to args.rkt with validation
- CLI flag overrides settings in run-modes.rkt

#### GAP-2: WS Evolution Old-State Tracking
- Add `current-last-task-fsm-state` parameter to track previous task state across turns
- WS evolution now receives real old-state instead of always #f

#### GAP-3: Auto-Distillation Content Summaries
- Auto-distillation accepts optional content-summary hash
- Fallback conclusions use actual file content (truncated to 200 chars) instead of placeholder text
- Turn-orchestrator builds summaries from working-set messages

#### GAP-4: Archived WS Entries Persistence
- Add `current-archive-entry-fn` injectable callback for persisting archived WS entries
- `guarded-set-working-set-evolved!` logs archived entries count + calls callback when wired

#### GAP-5: Idle State Preamble
- `build-state-awareness-preamble` now returns proper preamble for idle state
- Agent sees guidance and conclusions even before first state transition

#### GAP-6: Revert-State Rollback Action
- Add `current-revert-state-fn` injectable callback to rollback-actions.rkt
- `maybe-execute-action` dispatches revert-state when fn is wired, safe #f default
- Wire callback in state-aware-builder.rkt with logging

### Test Summary
- settings accessor: 4 tests
- session-config round-trip: 2 tests
- CLI: 32 tests
- state-aware assembly: 20 tests
- rollback actions: 17 tests
- WS evolution: 12 tests
- session mutation: expanded (archive callback)
- auto-distillation: 12 tests

## v0.78.6 (2026-06-01)

### Post-Audit Remediation

Resolves 8 CRITICAL and 4 WARNING findings from v0.78.xx post-implementation audit.
All feature flags remain OFF by default — no behavior change without explicit activation.

#### C1: WS Evolution Type Mismatch (W0)
- Replaced `evolve-working-set-for-state` with `/result` variant in turn-orchestrator
- Returns `evolution-result?` struct that `guarded-set-working-set-evolved!` expects

#### C2: Recent Tool Calls Not Threaded (W0)
- Added `#:recent-tool-calls` keyword to `assemble-context/pure` contract and signature
- Now forwards `agent-session-recent-tool-calls` from session to `build-tiered-context/state-aware`

#### C3: G3 Auto-Distill Persistence Test (W1)
- Added test verifying auto-distilled conclusions persist through `guarded-set-task-conclusions!`

#### C4: G9 Amnesia Trigger Assertion (W1)
- Fixed weak test — now verifies `task-amnesia-detected` warning fires for repeat tool count > 2

#### C5: G10 FSM Preamble Test (W1)
- Replaced broken `let-values` with proper preamble text extraction
- Verifies `record_conclusion` and `set_task_state` appear in preamble text

#### C6: STATE.md Stale (W2)
- Updated to reflect v0.78.6 and all completed milestones

#### C7: HANDOFF.json Stale (W2)
- Updated with current version, milestone status, and wave PR references

#### C8: PLAN Status Stale (W2)
- Marked PLAN-v0.78.6-AUDIT-REMEDIATION.md as COMPLETE

#### W1: WS Evolution Old-State (W0)
- Changed hardcoded `task-idle` to `#f` (unknown) for inline WS evolution

#### W2: Orphaned Event Subscriber (W0)
- Removed `context.ws-evolve-requested` subscriber from session-events.rkt
- Event was emitted but never consumed; inline evolution is the actual path

#### W3: Weak Integration Assertions (W1)
- Strengthened `check-not-false tc` assertions with meaningful content checks
- Added preamble verification and budget enforcement assertions

#### W4: README Sync (W2)
- Synced README status section to reflect v0.78.6

## v0.78.0 (2026-06-01)

### Comprehensive Context Assembly

Resolves 10 audit gaps (G1-G10) from v0.77.9 deep audit across 5 sub-milestones.
All feature flags remain OFF by default -- no behavior change without explicit activation.

#### G1: Profile Activation Matrix (v0.78.1)
- Rewrote apply-context-assembly-profile! with 5 profiles: off/observe/bounded/self-healing/full
- Each profile is strict superset of previous
- Profile parameter now properly tracked via current-context-assembly-profile
- Moved current-ws-evolution-enabled? to state-aware-builder.rkt to break cycle

#### G2: WS Evolution Wiring (v0.78.2)
- Fixed subscriber race condition: event payloads now include old-state captured BEFORE mutation
- Wired WS evolution into turn-orchestrator context assembly path

#### G3: Auto-Distill Persistence (v0.78.2)
- Auto-distilled conclusions now persist back to session via guarded-set-task-conclusions!

#### G4: Rollout Gate (v0.78.4)
- session-rollout-enabled? bypasses rate check when profile is not off
- Profile-based activation is cleaner than rate-based A/B testing

#### G5: WS Evolution Merge (v0.78.0)
- guarded-set-working-set-evolved! now merges (union by ID) instead of replacing

#### G6: Planning Inference Confidence (v0.78.3)
- Planning heuristic confidence raised from 0.6 to 0.75 (above 0.7 threshold)

#### G7: Preamble Budgeting (v0.78.3)
- build-state-awareness-preamble now receives budgeted conclusions (not raw)

#### G8: Summary Mode Ranking (v0.78.3)
- Summary mode uses rank-and-budget instead of first+last heuristic

#### G9: Repeat Tool Count (v0.78.0)
- repeat-tool-count now computed from recent-tool-calls parameter (was hardcoded 0)

#### G10: FSM Workflow Instructions (v0.78.4)
- Preamble now includes brief FSM workflow guidance (~30 tokens)

#### Integration Tests
- Expanded from 7 to 12 integration tests covering all 10 gaps

## v0.77.10 (2026-06-01)

### Deep Audit Finding Resolution

Resolves 3 minor findings (M1–M3) from deep post-remediation audit of v0.77.9.
All feature flags remain OFF by default — no behavior change without explicit activation.

#### M1: WS Evolution Mutation Wiring
- Session-events subscriber now emits `context.ws-evolve-requested` event on state transitions
- Removed unused `evolve-working-set-for-state` import from session-events.rkt
- Event enables turn-orchestrator to handle WS evolution with WS in scope

#### M2: Real Rollback Action Execution
- `maybe-execute-action` dispatches to injectable callbacks (`current-force-distill-fn`, `current-expand-context-fn`)
- `force-distill` enables auto-distillation flag; `expand-context` doubles conclusion budget
- `current-rollback-action-log` records executed actions for observability
- `'revert-state` remains blocked even when execution enabled

#### M4: Graph Selection Convenience
- Added `graph-select-conclusions` to conclusion-graph.rkt — returns conclusion objects directly
- State-aware-builder uses convenience wrapper instead of manual ID→object hash lookup

#### M5: Integration Tests
- New `test-context-assembly-integration.rkt` with 5 tests exercising full pipeline
- Tests: graph selection with dependencies, bounded profile, rollback callbacks, WS evolution, auto-distill

## v0.77.9 (2026-06-01)

### Post-v0.77.xx Audit Remediation

Fixes 5 CRITICAL + 8 WARNING findings from v0.77.xx post-implementation audit.
v0.77.0 delivered pure library modules; v0.77.9 wires them into the runtime.

#### Runtime Wiring (was dead code in v0.77.0)
- Graph selection: `build-conclusion-graph` + `graph-select-by-seeds` wired into `state-aware-builder.rkt` when `current-graph-conclusion-selection?` is ON
- Budget enforcement: `rank-and-budget` wired into conclusion injection path when `current-conclusion-token-budget` > 0
- Auto-distillation: `auto-distill` wired into `turn-orchestrator.rkt` pre-assembly when `current-auto-distillation-enabled?` is ON
- Rollback actions: `check-rollback-triggers-with-actions` + `maybe-execute-action` wired into `state-aware-builder.rkt` when `current-rollback-action-execution?` is ON
- WS evolution: subscriber on `task.state.transitioned`/`task.state.inferred` events for observability
- Profile activation: `apply-context-assembly-profile!` wired into `build-assembled-context` via `config-context-assembly-profile`

#### Bug Fixes
- Fixed `make-conclusions` arity in E2E test (missing dependencies field)
- Fixed `dynamic-require` in test-record-conclusion.rkt (replaced with direct require)
- Fixed stale version refs (0.74.6 → 0.77.0) across 12 docs/wiki files
- Synced `info.rkt` version 0.76.9 → 0.77.0

#### Tests
- Added multi-node cycle detection test (A→B→C→A)
- Updated budget enforcement test to reflect active trimming
- All 69+ context-assembly tests pass

#### Changed Files
- `runtime/turn-orchestrator.rkt` — auto-distill wiring, profile activation
- `runtime/context-assembly/state-aware-builder.rkt` — graph selection, budget, rollback actions
- `runtime/session-events.rkt` — WS evolution subscriber
- `runtime/session-config.rkt` — config-context-assembly-profile accessor

## v0.77.0 (2026-06-01)

### Advanced Context Assembly Completion

9 milestones (v0.77.0-v0.77.8) turning state-aware context plumbing into a measured, bounded, dependency-aware, self-healing context system. All new behavior is behind feature flags and disabled by default.

#### New Modules
- `runtime/context-assembly/conclusion-graph.rkt` - Pure DAG with cycle detection, topological sort, seed selection
- `runtime/context-assembly/conclusion-ranker.rkt` - Deterministic scoring by state match, category, recency, tags
- `runtime/context-assembly/auto-distillation.rkt` - Uncovered WS entry detection + deterministic/LLM fallback
- `runtime/context-assembly/rollback-actions.rkt` - Bounded action model for self-healing assembly
- `scripts/context-assembly-report.rkt` - Activation summary report

#### Key Changes
- `token-metrics.rkt`: conclusion-budget-remaining telemetry field
- `ws-evolution.rkt`: evolution-result struct with kept/archived/evicted entries
- `session-mutation.rkt`: guarded-set-working-set-evolved!
- `session-events.rkt`: current-ws-evolution-enabled? flag; dependency wiring
- `session-store.rkt`: append-archive-marker! + load-conclusions-archived
- `record-conclusion.rkt`: Optional dependencies parameter
- `task-memory.rkt`: conclusions-for-context with graph + degraded fallback
- `state-aware-builder.rkt`: graph selection flag, hard token budget, triggers-with-actions
- `budgeting.rkt`: conclusion-budget-config (Typed Racket)
- `session-config.rkt`: 5-level graduated activation profiles

#### Feature Flags (all disabled by default)
- current-task-state-aware-assembly? - master switch
- current-ws-evolution-enabled? - WS evolution
- current-graph-conclusion-selection? - graph-based selection
- current-conclusion-token-budget - hard budget (2000)
- current-auto-distillation-enabled? - auto conclusions
- current-rollback-action-execution? - self-healing actions
- current-context-assembly-profile - activation profile (default: 'off)

#### New Tests (69+)
- 12 ws-evolution, 14 conclusion-graph, 8 ranker, 8 auto-distillation
- 10 rollback-actions, 3 session-config profiles, 24 token-metrics, 14 builder

#### Migration
- Fully backward-compatible. No behavior changes without explicit flag activation.
- record_conclusion gains optional dependencies arg; old calls unaffected.


## v0.76.9 (2026-06-01)

### Post-v0.76.8 Audit Hotfix

Fixes 2 CRITICAL + 3 WARNING findings from v0.76.8 audit.

#### Changes
- Fixed VALIDATION gates SG-12 + M7-G4: ADR reference corrected from 0017 to 0019 (was claimed in v0.76.8 but not applied)
- Fixed PLAN-v0.76.xx series header and status
- Fixed STATE.md version (0.76.8) and status (RELEASED)
- Fixed CHANGELOG v0.76.8 date (was 2026-05-30, now 2026-06-01)
- Updated HANDOFF.json current_version to 0.76.8
- Moved 2 test-cases inside their test-suite forms (test-record-conclusion.rkt, test-session-task-state.rkt)

## v0.76.8 (2026-06-01)

### Post-v0.76.7 Audit Closure

1 CRITICAL documentation integrity fix + 8 WARNING fixes + 3 INFO fixes.

#### Changes
- Fixed VALIDATION gates SG-12 + M7-G4: ADR reference corrected from 0017 to 0019
- Fixed STATE.md version (was stale at 0.76.6)
- Fixed PLAN-v0.76.xx series header (main series vs audit closure distinction)
- Removed dead `tc-result` binding in `state-aware-builder.rkt`
- Renamed `check-rollback-triggers` parameters from `#:before-tokens`/`#:after-tokens` to `#:before-messages`/`#:after-messages`
- Added `[DEPRECATED]` prefix to `save-conclusion` tool description
- Fixed `record-conclusion.rkt` event payload to send validated category symbol instead of raw string
- Added error guard around `set-task-state` event emission
- Fixed `when` guard in `test-v0756-audit-closure.rkt` that could swallow test failures

#### Tests Added
- Persistence round-trip test for string `origin-message-ids` (C1 regression guard)
- Assertion for `origin-message-id` in record_conclusion event payload (C2 regression guard)
- Integration test for `tool.set-task-state.completed` → session state change (C3 regression guard)
- Session handler test: `origin-message-ids` populated from event payload

## v0.76.7 (2026-06-01)

### Audit Closure for v0.76.xx Context Assembly Activation

Post-implementation audit fix: 3 CRITICAL + 11 WARNING findings resolved.
No behavior change (feature flag still defaults `#f`).

#### Critical Fixes
- **C1**: `origin-message-ids` deserialization type corruption — kept as strings
  (were being converted to symbols, breaking conclusion-first WS replacement)
- **C2**: `origin-message-id` now wired from `exec-context-call-id` into
  `record_conclusion` event payload; session subscriber reads and persists it
- **C3**: `set-task-state` tool now emits `tool.set-task-state.completed` event;
  session subscriber calls `guarded-set-task-fsm-state!` for explicit transitions

#### Warning Fixes
- **W1**: `save-conclusion` deprecated (emits `tool.deprecated` event)
- **W2**: `record_conclusion` added to inference `'meta` category
- **W5**: Debugging WS changed from `'full` to `'filtered` per decision A7
- **W6**: `check-rollback-triggers` wired into `build-tiered-context/state-aware`
- **W7**: Fixed tautological test assertion in `test-token-metrics.rkt`
- **W8**: Added `'excluded` WS path test for implementation state
- **W9**: Refactored brittle manual constructors in 3 test files to shared fixture
- **W10**: Fixed VALIDATION SG-12 ADR reference (0017 → 0019)
- **W11**: Fixed plan header series description

#### Tests Improved
- test-token-metrics.rkt: preamble verification (non-tautological)
- test-ws-conclusion-fallback.rkt: excluded WS path coverage
- test-session-task-state.rkt, test-v0756-audit-closure.rkt, test-record-conclusion.rkt:
  migrated to shared `make-test-session` fixture


## v0.76.6 (2026-05-31)

### Context Assembly Activation — Production Hardening (M7)

Final milestone: context efficiency dashboard, ADR, cleanup.

#### Changes
- **Dashboard**: `scripts/benchmark/context-efficiency.rkt` — parallel benchmark
  runner with per-scenario metrics, token savings table, and low-coverage flags
- **ADR 0019**: `docs/adr/0019-context-assembly-activation.md` — architecture
  decision record for the full v0.76.xx activation series
- **Cleanup**: removed dead code paths, consolidated imports, final integration
  tests across all 7 milestones

#### Tests Added
- test-v0756-audit-closure.rkt: persistence round-trip, preamble integration,
  FSM validation edge cases
- test-state-aware-assembly.rkt: end-to-end state-dependent assembly


## v0.76.5 (2026-05-31)

### Context Assembly Activation — Conclusion Dependencies (M6)

Sixth milestone: track file dependencies on conclusions for traceability.

#### Changes
- **Dependencies field**: `task-conclusion` struct gains `dependencies` field
  (list of file paths), acyclic by construction (simple tagging, no DAG)
- **Serialization**: `conclusion->hash` / `hash->conclusion` handle the new field
  with backward-compatible deserialization (defaults to `'()`)
- **Record tool**: `record_conclusion` accepts optional `dependencies` parameter

#### Tests Added
- test-conclusion-dependencies.rkt: dependency tracking, serialization round-trip,
  backward compatibility


## v0.76.4 (2026-05-31)

### Context Assembly Activation — Working-Set Evolution (M5)

Fifth milestone: replace working-set entries with conclusions when state
filters or excludes the WS.

#### Changes
- **Conclusion-first replacement**: `ws-entry->conclusion-or-self` replaces
  WS entries that match a conclusion's `origin-message-ids` with compact text
- **State-dependent WS behavior**: `evolve-working-set-for-state` applies
  state-specific WS levels (`'full`, `'filtered`, `'excluded`)
- **WS evolution module**: `runtime/context-assembly/ws-evolution.rkt` —
  per-state WS relevance configuration with conclusion overlay

#### Tests Added
- test-ws-conclusion-fallback.rkt: conclusion-first replacement, fallback,
  mixed WS, token reduction verification
- test-ws-evolution.rkt: per-state WS levels, conclusion overlay


## v0.76.3 (2026-05-31)

### Context Assembly Activation — Graduated Activation (M4)

Fourth milestone: safe rollout infrastructure with per-session config,
A/B comparison harness, and rollback triggers.

#### Changes
- **Per-session config**: `task-state-aware?` field on session config —
  deterministic hash-based rollout (default 0%, no behavior change)
- **A/B harness**: benchmark scenarios comparing state-aware vs. legacy assembly
  with 6 test scenarios across all task states
- **Rollback triggers**: `check-rollback-triggers` — detects excessive savings,
  amnesia-risk, and task-amnesia conditions for safety rollback

#### Tests Added
- test-session-rollout.rkt: rollout hash, per-session config, default safety
- test-rollback-triggers.rkt: all 3 trigger conditions, threshold boundaries


## v0.76.2 (2026-05-31)

### Context Assembly Activation — Measurement Infrastructure (M3)

Third milestone: build instrumentation to prove token savings.
Purely additive — no behavior change.

#### Changes
- **Token metrics**: `runtime/context-assembly/token-metrics.rkt`
  - `context-metrics` struct, `measure-context-size`, `compute-savings`
  - `category-breakdown`: per-tier token counts
  - `measure-context-assembly`: wrapper returning `(values context metrics)`
- **Conclusion coverage**: `compute-conclusion-coverage` — ratio of conclusion
  tool calls to total tool calls
- **Report script**: `scripts/report-context-efficiency.rkt` — text report
  with token savings and low-coverage warning

#### Tests Added
- test-token-metrics.rkt: 24 tests (token counting, savings, coverage,
  assembly wrapper)


## v0.76.1 (2026-05-31)

### Context Assembly Activation — System Prompt Integration (M2)

Second milestone: force the agent to use conclusions by injecting
state-aware instructions into the system prompt.

#### Changes
- **Prompt injection**: `state-guidance-table` updated with action-oriented
  instructions using `record_conclusion` (was: `save_conclusion`)
- **Dynamic preamble**: `build-state-awareness-preamble` now includes
  conclusion count and encouragement when empty
- **Benchmark**: `scripts/benchmark/task-state-aware.rkt` verifies
  instruction presence across all 5 states, token overhead, and
  conclusion count accuracy

#### Tests Added
- test-prompt-injection.rkt: 21 tests (presence, labels, instructions,
  record_conclusion references, conclusion count, no duplicates)


## v0.76.0 (2026-05-31)

### Context Assembly Activation — Gap Closure (M1)

First milestone of the v0.76.xx series: close all v0.75.xx infrastructure gaps
so state-aware context assembly is technically complete.

#### New Features
- **Tools**: `record_conclusion` tool — agent can save distilled insights as
  `task-conclusion` structs with category, tags, and FSM state origin.
  Emits `tool.record_conclusion.completed` event; session layer persists.

#### Architecture
- **Extraction**: `serialization.rkt` reduced from 494 → 173 lines
  - `context-floor.rkt`: core tiered context building (structs, partitioning, hooks)
  - `state-aware-builder.rkt`: state-aware extensions (preamble, conclusion injection)
- **FSM**: `guarded-set-task-fsm-state!` now validates state→state transitions
  (rejects e.g. exploration → verification; allows reset to idle)
- **Inference**: `guarded-set-recent-tool-calls!` validates accumulated tool history
- **Relevance**: `implementation` state hard-excludes `working-set` (was filtered)

#### Tests Added
- test-record-conclusion.rkt: 13 tests (tool validation, event emission, persistence)
- test-task-state.rkt: +7 tests (transition validation, guarded setter)
- test-task-state-inference.rkt: +4 tests (symbol names, batch caps, multi-turn)
- test-state-aware-builder.rkt: 13 tests (feature flag, preamble, conclusions)
- test-state-relevance.rkt: 11 tests (all states × categories)


## v0.75.9 (2026-05-31)

### Post-v0.75.8 Audit Fix

#### Bug Fixes
- **GUI**: Fixed tool result misalignment — parallel tool completions now correctly correlate to their own tool call (was: all → OK hit the last message)
- **Tools**: Fixed `set-task-state` always failing from LLM — `hasheq` symbol keys now accept string args via `string->symbol` conversion
- **Streaming**: Emit `model.stream.completed` on provider stream exceptions — prevents GUI text concatenation on next turn

#### Tests Added
- test-gui-state-sync.rkt: 27 tests (+3: parallel tool correlation, mixed OK/FAIL, orphan completion)
- test-save-conclusion.rkt: 16 tests (+2: string args for set-task-state)

## v0.75.8 (2026-05-31)

### Post-v0.75.7 Audit Fix

#### Bug Fixes
- **GUI**: Fixed tool results showing `→ #f` — now shows `→ OK` or `→ FAIL` with correct event payload keys (`toolName`/`resultSummary`)
- **GUI**: Tool results now inline with tool calls (`Tool: [bash] → OK`) instead of separate messages
- **GSD**: `/plan` wave cleanup now instant — uses `delete-directory/files` instead of per-file iteration
- **GSD**: Plan validator no longer blocks docs-only plans with no file references — downgraded to WARNING

#### Tests Updated
- test-gui-state-sync.rkt: 24 tests (updated tool.execution.completed tests for inline display)
- test-gsd-plan-validator.rkt: 10 tests (updated docs-only plan expectation)
- test-gsd-plan-validator-unit.rkt: 14 tests (updated docs-only plan expectations)

## v0.75.7 (2026-05-30)

### GUI/GSD Workflow Usability Fixes

#### Bug Fixes
- **GUI**: Tool messages now show arguments and results in transcript (was: `Tool: [xxx]`, now: `Tool: [bash] command: ls`)
- **GUI**: Tool execution results now appear in transcript via `tool.execution.completed` handler
- **GUI**: `gui-message` struct extended with `meta` field for structured tool data
- **GSD**: `/plan` now cleans old wave files and emphasizes wave-first ordering in prompt
- **GSD**: `/go` now validates wave doc existence before state transition
- **GSD**: `/go` resilient to relaxed wave index format (`- W0: Title` without `[Status]` brackets)
- **GSD**: `/go` now includes persistent task summary to prevent focus loss during execution
- **Context**: `gsd-progress-message?` extended to pin execution-instruction messages

#### Tests Added
- 13 tests in test-gui-types.rkt (5 new: meta field)
- 25 tests in test-gui-state-sync.rkt (5 new: tool args/results, 20 fixed: struct migration)
- 5 tests in test-gsd-wave-gen-validation.rkt (wave doc validation)
- 5 tests in test-gsd-relaxed-index.rkt (relaxed parser)
- 3 tests in test-gsd-task-focus.rkt (task summary)
- 6 tests in serialization.rkt module+test (execution instruction pinning)


## v0.75.6 (2026-05-30)

### Audit Closure (v0.75.6)

- **Assembly Wiring**: Thread session through `build-assembled-context` for task-state-aware context.
- **Preamble Integration**: Wire `build-state-awareness-preamble` into `build-tiered-context/state-aware`.
- **FSM Validation**: `guarded-set-task-fsm-state!` now rejects invalid state symbols.
- **Unique IDs**: Replace duplicate conclusion message IDs with `generate-id`.
- **Persistence**: Task-state and conclusion round-trip to session log.
- **Inference Accumulation**: `recent-tool-calls` field accumulates last 10 tool calls for better state inference.
- **Dual Input**: Context assembly accepts both fsm-state structs and bare symbols.
- **14 new tests** in `test-v0756-audit-closure.rkt`.

## v0.75.5 (2026-05-30)

### Advanced Context Assembly (v0.75.0-v0.75.5)

- **Task-State FSM**: 6-state FSM with 8 events using `define-fsm-machine`
- **Tools + Session**: `save-conclusion` and `set-task-state` tools, session fields
- **State Inference**: Heuristic tool-call pattern analysis, event-wired
- **State-Aware Assembly**: Relevance matrix, feature-flagged context assembly
- **Working-Set Evolution**: State-transition-driven WS clearing/filtering
- **System Prompt Injection**: State-awareness preamble with guidance + conclusions
- ~106 new tests across 7 test files


## v0.74.8 (2026-05-31)

### Context-Assembly Contract Bugfix + Test Quality Remediation

- **P0 FIX**: `build-tiered-context-with-hooks` contract declared `(or/c hash? #f)` but returned `(or/c hook-result? #f)` — broke all prompt submission with hooks active
- **R1**: Removed trivially-true `check-true #t` assertions in `test-session-persistence.rkt`
- **R2**: Added `current-crash-log-dir` parameter for test isolation — zero `~/.q/` pollution
- **R3**: Added 3 contract-rejection tests for all exported functions
- **R4**: Added 4 direct function tests for `ensure-persisted!` and `buffer-or-append!`
- Test count: 6 → 13 (all meaningful assertions, temp-dir isolated)
- Version bump 0.74.7 → 0.74.8

## v0.74.7 -- 2026-05-31

### Audit Closure (W0 + W1 + W2)
- **W0**: Fixed stale `loop-state.rkt` references (moved from `runtime/` to `agent/` in v0.73.4)
  - Updated `dependency-policy.rktd`: moved exception to `agent` layer, updated sub-modules
  - Updated `test-arch-fitness.rkt`: corrected TR module path
  - Arch-fitness test failures: 2 → 0
- **W1**: Session persistence extraction
  - Extracted `write-crash-log!`, `ensure-persisted!`, `buffer-or-append!` to `runtime/session-persistence.rkt`
  - Added 6 crash-logger tests (was zero)
  - `session-lifecycle.rkt`: 478 → 438 lines (target ≤450)
  - Backward-compatible re-export from `session-lifecycle.rkt`
- **W2**: Arch-fitness tests + version bump
  - Added 6 arch-fitness test-cases (41 → 47)
  - Added agent layer policy tests
  - Added session persistence module verification
  - Added session lifecycle line-budget check
  - Version bump 0.74.6 → 0.74.7


## v0.74.6 -- 2026-05-30

### Architecture Controls & Release (M7)
- **W0**: Activated CI gates
  - Hotspot block-threshold (>20,000 score) now BLOCKING — top files must have risk-notes
  - Added risk-notes for 9 top hotspot files (tui/state-events, tui/terminal-input, llm/gemini, etc.)
  - Cycle-detection tests already blocking
  - Metrics synced to README
- **W1**: Version bump 0.74.5 → 0.74.6, final release

### v0.74.xx Series Summary
7 milestones (v0.74.0–v0.74.6), ~1,000 LOC across:
- Hotspot fitness function with scoring
- Failure-domain classifier (7 error domains)
- Session store cycle elimination (parameter injection)
- Cycle-detection CI gate
- Widened contract reconciliation (~44 contracts tightened)
- Session lifecycle FSM extraction
- GSD boundary clarification + TUI render phase extraction
- Architecture controls activation


## v0.74.5 -- 2026-05-30

### GSD Boundary Clarification + TUI Render Extraction (M6)
- **W0**: Added 8 boundary tests for gsd/core.rkt contract-out API
  - Command dispatch, write guard, status, reset, accessors, contract rejection
- **W1**: GSD state already well-extracted into gsd/session-state.rkt
  - Verified facade pattern: gsd-planning.rkt → gsd/session-state.rkt + gsd/core.rkt
  - Version bump 0.74.4 → 0.74.5


## v0.74.4 -- 2026-05-30

### Session Lifecycle FSM Extraction (M5)
- **W0**: Expanded pure function tests (8 test cases, 19 assertions)
- **W1**: Extracted `session-lifecycle-transitions.rkt` from session-lifecycle
  - `build-user-message`, `compute-parent-id`, `inject-system-instructions`
  - ~40 LOC reduction in session-lifecycle.rkt hotspot
  - Fixed `build-tiered-context-with-hooks` contract: `hash?` → `(or/c hash? #f)`
  - Registered in dependency-policy.rktd
- **W2**: Added 5 fault-injection tests for edge cases
  - Special characters, long strings, malformed entries, message order preservation


## v0.74.3 -- 2026-05-30

### Widened Contract Reconciliation — Investigation Batch (M4)
- Deep caller analysis of 17 investigate-category contracts
- Tightened 14 contracts after confirming actual types:
  - `session-index.rkt` / `mutations.rkt`: branch!, branch-with-summary!,
    append-to-leaf!, lookup-entry, resolve-active-leaf, active-leaf,
    estimate-entry-tokens — all now use `message?` or `(or/c message? #f)`
  - `session-manager.rkt`: sm-fork! kept at `any/c` (returns polymorphic fork result)
  - `settings.rkt`: make-minimal-settings #:provider → `(or/c string? #f)`
  - `context-assembly.rkt`: build-tiered-context-with-hooks 2nd value → `hash?`
  - `llm/stream.rkt`: normalize-openai-chunk(s) → `(or/c stream-chunk? hash?)`
  - `llm/http-helpers.rkt`: translate-stop-reason 2nd arg → `(or/c string? symbol? #f)`
  - `gsd/tool-handlers.rkt`: get-base-dir optional → `(or/c exec-context? #f)`
  - `gsd/wave-docs.rkt`: parse-wave-doc-from-string → specific arg types


## v0.74.2 -- 2026-05-30

### Widened Contract Reconciliation (M3)
- **W0**: Audited 120+ `any/c` positions in contract-out forms across all layers
  - Categorized: ~45 tighten, ~15 investigate, ~55 keep
- **W1**: Tightened ~30 safe contracts across 6 layers:
  - `runtime/`: session-manager (message?), compaction-hooks (procedure?),
    provider-factory (q-settings?), session-controls (model-registry?),
    settings (hash?), iteration/directive (working-set?, list?),
    iteration/fsm-types (fsm-state?, fsm-event?)
  - `tools/`: registry-defaults (tool-registry?), tool-internal (tool?→procedure?),
    tool.rkt (or/c string? hash? list?), builtins/bash+grep+edit+firecrawl+session-recall (tool-result?)
  - `util/`: message-helpers (message?→boolean?), tree-entries (or/c string? #f),
    content-parts (boolean? for is-error)
  - `llm/`: openai-compatible (model-request?, model-response?)
  - `extensions/`: tiers (extension?)
  - `sandbox/`: subprocess (custodian?)
- Reverted 2 auth-store tightenings that broke intentional polymorphic tests


## v0.74.1 -- 2026-05-30

### Session Store Cycle Elimination (M2)
- **W0**: Eliminated `lazy-require` circular dependency between `session-store.rkt`
  and `session-store-tree.rkt` using runtime parameter injection
- **W1**: Added cycle-detection CI gate (DFS-based) to `test-arch-fitness.rkt`
  for runtime, agent, and tools layers
- **W2**: Version bump to 0.74.1


## v0.74.0 -- 2026-05-30

### Fitness Functions & Quick Wins (M1 W0)

- **Fix** `test-registry-snapshot.rkt` — add missing `timeout-seconds` field
- **New** C1 regression test for `extension-registry?` predicate
- **New** `log-stream-setup-timing` unit test (contract accepts `real?`)
- **Tighten** `step-interpreter.rkt` contracts — replace `any/c` with struct predicates
- **Clean** README — remove 214 excess blank lines
- **New** 11 tool security edge-case tests using shell-risk classifier

## v0.73.8 -- 2026-05-30

### Audit Closure (A-7)

- **CRITICAL:** Fix `extension-registry?` contract violation — replaced fake `procedure?` predicate with real re-export from `extensions/api.rkt` (C1)
- **W1:** Activate dead loggers — remove dead `define-logger` from `loop-fsm.rkt`, add 4 `log-q-main-loop-info` calls at FSM transition sites in `main-loop.rkt`
- **W2:** Complete shared timing adoption — fix `timing.rkt` contract to accept `real?`, replace inline timing logs in `anthropic.rkt` and `gemini.rkt`
- **W3:** Add `contract-out` to `step-interpreter.rkt` for `interpret-step`, `handle-stop-action`, `execute-pending-tool-calls`, `sink-append-entries!`

## v0.73.7 -- 2026-05-30

### Extension Tests + Contract Completion (T-6/A-6b)

| Change | Detail |
|--------|--------|
| T-6 | 27 unit tests for GSD command parsing + dispatch |
| T-6 | 14 unit tests for GSD plan validator |
| A-6b | Contract-out added to `sandbox/subprocess.rkt` |
| A-6b | Contract-out added to `sandbox/evaluator.rkt` |

41 new test cases. Sandbox security boundary now has typed contracts.

## v0.73.6 -- 2026-05-30

### Observability Infrastructure (T-3/T-4/T-5)

| Change | Detail |
|--------|--------|
| T-3 | Added `define-logger` to 6 critical modules |
| T-4 | Added trace logging to tool register/unregister |
| T-5 | Reduced flaky `sleep 10` → `sleep 2` in sandbox limits test |

**New loggers:** `q-loop-fsm`, `q-main-loop`, `q-tool-registry`, `q-openai`,
`q-turn-orch`, `q-session-lifecycle`. All additive — no behavior changes.

## v0.73.5 -- 2026-05-30

### Credential Backend Decomposition (A-4)

Decomposed the 650-line `credential-backend.rkt` god module into 7 focused
modules under `runtime/credentials/`:

| Module | Content |
|--------|---------|
| `protocol.rkt` | Struct + generic ops + command runner parameters |
| `file-backend.rkt` | JSON file credential storage |
| `env-backend.rkt` | Environment variable backend (read-only) |
| `memory-backend.rkt` | In-memory backend (testing) |
| `keychain-backend.rkt` | Linux `secret-tool` backend |
| `platform-backends.rkt` | macOS `security` + Windows `cmdkey` + capabilities |
| `chained-backend.rkt` | Chained fallback + policy-aware wrapper |

`credential-backend.rkt` becomes a re-export facade. Zero API changes.

## v0.73.4 -- 2026-05-30

### Iteration Layer Migration (A-1)

**Moved 7 files** from `runtime/iteration/` to `agent/iteration/`:
- `counters.rkt`, `loop-config.rkt`, `loop-phases.rkt`, `loop-state.rkt`
- `main-loop.rkt`, `step-interpreter.rkt`, `tool-turn-bridge.rkt`

**Rationale:** These 7 files have dense coupling to `agent/` (event emission,
queue operations, event bus types) and implement the agent loop's iteration
mechanics. Moving them aligns code location with conceptual ownership.

**~80 import paths** updated across source and test files.
Zero logic changes. All existing tests pass.

## v0.73.3 -- 2026-05-30

### Core Loop Tests: State Machines + Dispatch (T-1b)

| Finding | Change | Tests |
|---------|--------|-------|
| T-1b | Loop FSM state machine tests | test-loop-fsm.rkt (19 cases) |
| T-1b | Loop dispatch surface tests | test-loop-dispatch.rkt (5 cases) |
| T-1b | Tool registry unit tests | test-tool-registry-unit.rkt (14 cases) |

**38 new test cases** across 3 test files. Zero logic changes.

## v0.73.2 -- 2026-05-30

### Tool Builtins + Security Tests (T-2/T-7/S-1/S-2)

| Finding | Change | Tests |
|---------|--------|-------|
| T-7 | FSM unit tests | test-fsm-unit.rkt (8 cases) |
| T-7 | Shell-quote injection tests | test-shell-quote.rkt (16 cases) |
| T-2 | Read builtin tests | test-tool-read-builtin.rkt (7 cases) |
| T-2 | Write + edit builtin tests | test-tool-write-builtin.rkt (5), test-tool-edit-builtin.rkt (7) |
| S-1 | Sandbox limits tests | test-sandbox-limits-unit.rkt (4 cases) |
| S-2 | Image-pipeline temp cleanup reviewed | Already safe — no source change needed |

**47 new test cases** across 6 test files. Zero logic changes.

## v0.73.1 -- 2026-05-30

### Architecture Fixes + Code Quality (A-2/A-3/Q-1/Q-2/Q-3)

| Finding | Change | Files |
|---------|--------|-------|
| A-2 | Extract `tool?` to `util/tool-types.rkt` | util/tool-types.rkt, loop-dispatch.rkt, loop-phases.rkt |
| A-3 | Move `extension-registry?` to `util/extension-types.rkt` | util/extension-types.rkt, loop-state.rkt |
| Q-1 | Adopt `now-epoch-ms`/`now-epoch-secs` across 4 files | util/time.rkt, goal-*.rkt, sdk-core.rkt |
| Q-2 | Extract `with-stream-timing` into `llm/timing.rkt` | llm/timing.rkt, 4 provider files |
| Q-3 | Migrate 2 files to `#lang racket/base` | sdk-public.rkt, tui/state.rkt |

Zero logic changes. Pure mechanical refactoring.

## v0.73.0 — 2026-05-30

### Core Loop Tests: Pure Functions (T-1a)

| Module | Tests | Focus |
|--------|-------|-------|
| `test-iteration-decision.rkt` | 19 | decide-next-action, compute-step-result, step-action?, struct accessors |
| `test-iteration-retry-policy.rkt` | 16 | compute-mid-turn-estimate, detect-exploration-loop, count-occurrences, overflow recovery |
| `test-iteration-counters-unit.rkt` | 8 | compute-next-counters (explore/implement/tool counts), documented check-cancellation gap |

**43 new test cases** across 3 previously untested modules. Zero source changes.

## v0.72.8 — 

### Audit Closure

Close 5 remaining findings from v0.72.xx post-implementation audit.

| Finding | Fix |
|---------|-----|
| AU-2: 6 internal structs #:transparent | Removed #:transparent from directive-recurse, directive-stop, directive-yield, tool-call-actions, subscription, in-memory-session-manager |
| AU-3: turn-reducer any/c contracts | Tightened decide-after-pre-hook and decide-after-msg-hook to (or/c hook-result? #f) |
| AU-4: guarded-set-index! any/c | Tightened to (or/c session-index? #f) |
| AU-5: q-component? placeholder | Documented cycle constraint; placeholder retained with explanation |
| Fix: test-component-model.rkt | Updated component-state-set! → component-state-update (missed in v0.72.7) |

**Files changed:** directive.rkt, tool-coordinator.rkt, event-bus.rkt, in-memory.rkt, turn-reducer.rkt, session-mutation.rkt, state-types.rkt, test-step-directive.rkt, test-turn-reducer.rkt, test-component-model.rkt

## v0.72.7 — 

### Phase 5: Long-Term Patterns + Sweep

TUI reducer pattern + event registry lifecycle + mutable hash audit + naming cleanup + misc fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| W6: TUI slash command mutation | WARNING | Added apply-slash-command return-based wrapper |
| W7: event-reducer test isolation | WARNING | Added current-event-reducers parameter + call-with-test-registry |
| W15: mutable hash thread safety | WARNING | Documented thread-safety guarantees for 3 registries |
| W17: component-state-set! naming | WARNING | Renamed to component-state-update |
| I4: goal-state-total-token-cost location | INFO | Moved from goal-runner.rkt to goal-types.rkt |
| I5: tool-id provide | INFO | Documented as backward-compat |
| I9: compactor current-seconds | INFO | Replaced with now-epoch-ms for consistency |
| I10: gsd/core boundary | INFO | Documented pure/effectful sections |
| I11: scheduler contracts | INFO | Added contract-out for plan-tool-batch, execute-tool-plan |
| I12: field->json-key duplication | INFO | Documented compile-time vs runtime split |

**Files changed:** commands.rkt, state-events.rkt, component.rkt, tui-render-loop.rkt, goal-types.rkt, goal-runner.rkt, define-tool.rkt, compactor.rkt, core.rkt, scheduler.rkt, event-macro.rkt, settings.rkt, model-registry.rkt

## v0.72.6 — 

### Phase 4b: Module Decomposition II

Event-structs explicit provides + event-bus documentation.

| Finding | Severity | Fix |
|---------|----------|-----|
| W12: event-bus layer violation | WARNING | Documented as foundational utility in extensions/api.rkt |
| W11: event-structs all-from-out | WARNING | Replaced with 239 explicit identifier provides |
| I13: context-assembly facade | INFO | Documented sub-module origins for future migration |

**Files changed:** event-structs.rkt, extensions/api.rkt, context-assembly.rkt

## v0.72.5 — 

### Phase 4a: Module Decomposition I

Session-store split + errors split + protocol-types documentation.

| Finding | Severity | Fix |
|---------|----------|-----|
| W3: session-store monolith | WARNING | Split into versioning.rkt + in-memory.rkt sub-modules |
| W14: warn-deprecated! in errors.rkt | WARNING | Extracted to util/deprecation.rkt, re-exported |
| I2: protocol-types transitional | INFO | Documented remaining consumers for future migration |

**Files changed:** session-store/versioning.rkt (new), session-store/in-memory.rkt (new), session-store.rkt (facade), deprecation.rkt (new), errors.rkt, protocol-types.rkt

## v0.72.4 — 

### Phase 3b: Opaque Representations

Struct opacity + goal-state split + mutable-set elimination.

| Finding | Severity | Fix |
|---------|----------|-----|
| W2: goal-state monolith | WARNING | Split into goal-types.rkt + goal-codec.rkt, facade re-exports |
| W1: turn-model #:transparent structs | WARNING | Removed #:transparent from 5 structs |
| I7: append-entries! uses set-box! | INFO | Already pure — no change needed |
| I8: compactor mutable-set | INFO | Replaced with immutable set + for/fold |

**Files changed:** `goal-types.rkt` (new), `goal-codec.rkt` (new), `goal-state.rkt` (facade), `turn-model.rkt`, `compactor.rkt`

## v0.72.3 — 

### Phase 3a: Purity Enforcement

Pure/impure separation in agent loop.

| Finding | Severity | Fix |
|---------|----------|-----|
| C3: run-streaming-phase is impure in pure module | CRITICAL | Extracted to `agent/loop-dispatch.rkt` |
| W5: tool-coordinator lacks purity labels | WARNING | Added EFFECTFUL/PURE labels to phase comments |

**Files changed:** `loop-dispatch.rkt` (new), `loop-phases.rkt`, `loop.rkt`, `tool-coordinator.rkt`

## v0.72.2 — 

### Phase 2b: Boundary Contract Precision

Boundary tightening for tool-registry, settings, and state-types.

| Finding | Severity | Fix |
|---------|----------|-----|
| W10: tool-registry internal fields publicly exported | WARNING | Moved to `module+ internal` |
| W9: settings globals in public API | WARNING | Removed q-settings-global/project from provide |
| W8: state-types uses `any/c` for styled-line/q-component | WARNING | Replaced with real predicates |
| I6: tool-schema naming | INFO | Verified — no rename needed |

**Files changed:** `tool-registry-struct.rkt`, `registry.rkt`, `settings.rkt`, `state-types.rkt`

## v0.72.1 — 

### Phase 2a: Core Contract Precision

Contract tightening for core modules.

| Finding | Severity | Fix |
|---------|----------|-----|
| C1: 28 `any/c` in loop-phases contracts | CRITICAL | Replaced with string?, event-bus?, provider?, loop-state?, procedure?, (listof tool?) |
| W13: turn-reducer lacks contracts | WARNING | Reviewed — contracts already specific where possible |
| I14: session-mutation uses `any/c` for typed fields | INFO | Tightened config→hash?, start-time→exact-nonnegative-integer?, last-compaction-time→(or/c exact-nonnegative-integer? #f) |

**Files changed:** `loop-phases.rkt`, `session-mutation.rkt`

## v0.72.0 — 

### Phase 1: Zero-Cost Fixes

Abstraction quality remediation — zero-risk fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| C2: Raw setter breach — 13 deprecated setters in public API | CRITICAL | Moved to `module+ internal`, only `session-mutation.rkt` can import |
| W4: `epoch-ms` pattern repeated across codebase | WARNING | Extracted `now-epoch-ms` into `util/time.rkt` |
| W16: No effect-base supertype | WARNING | Added `effect-base` struct, `effect?` uses supertype predicate |
| I1: Dead `with-cleanup` macro exported | INFO | Deleted from `errors.rkt` |
| I3: Duplicate bare+only-in import in `goal-runner.rkt` | INFO | Replaced with `except-in` |

**Files changed:** `session-types.rkt`, `session-mutation.rkt`, `session-compaction.rkt`, `effect-types.rkt`, `errors.rkt`, `goal-runner.rkt`, `util/time.rkt` (new)

## v0.71.8 — 

### v0.71.7 Audit Closure

Post-implementation audit closure for the v0.71.7 hotfix. Test gaps + event payload alignment + doc fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| WARN-1: No test for goal-check-completed event | WARN | Added test via goal-loop-step |
| WARN-2: No test for concurrent goal guard rejection | WARN | Added guard condition verification test |
| INFO-1: string-truncate missing docstring | INFO | Added docstring clarifying output length |
| INFO-2: No payload key assertions in event tests | INFO | Added goal-achieved/goal-started payload checks |
| INFO-3: No test for goal-failed on shutdown-cancel | INFO | Enhanced shutdown test with event tracking |
| INFO-4: Cancelled state not verified in failed event | INFO | Assert goal-state-status in failed payload |
| INFO-5: Event payloads use raw structs | INFO | Aligned 4 event types to hasheq payloads |
| DOC-1: VALIDATION header stale | DOC | Fixed header reference |

**Files changed:** `goal-state.rkt`, `goal-runner.rkt` + 2 test files
**Tests added:** 4 new/enhanced tests

## v0.71.7 — 

### Goal Audit Hotfix

Post-implementation audit hotfix for the `/goal` autonomous loop feature.

| Finding | Severity | Fix |
|---------|----------|-----|
| F1: Type confusion — evaluation-result mixed into checks field | FAIL | Added `evaluations` field to goal-state |
| F2: goal.achieved / goal.check.completed events never emitted | FAIL | Runner now emits both events |
| W1: No shutdown-requested check in runner loop | WARN | Added `#:shutdown-check` parameter |
| W2: No concurrent goal guard — silent overwrite | WARN | TUI rejects `/goal` when active-goal set |
| W3: Shell injection via $(...) only rejected at critical | WARN | Raised to high severity |
| I1: string-truncate duplicated across modules | INFO | Consolidated into goal-state.rkt |
| I2: NO-PROGRESS-THRESHOLD duplicated in runner | INFO | Removed duplicate, uses goal-state export |

**Files changed:** `goal-state.rkt`, `goal-runner.rkt`, `goal-checks.rkt`, `shell-risk.rkt`, `tui/commands.rkt` + 3 test files
**Tests added:** 9 new tests across 3 files
**Breaking changes:** None — new field defaults to empty list


## v0.71.6 — 2026-05-30

### Agent Evaluator + Series Audit

Seventh and final milestone of the `/goal` feature — agent-based evaluation and series closure.

- **Agent evaluator**: New `runtime/goal-agent-evaluator.rkt` with `evaluate-with-agent` — deeper investigation with enhanced system prompt, 2000 token budget, JSON extraction from prose responses.
- **Evaluator mode dispatch**: `goal-runner.rkt` dispatches to `evaluate-with-agent` when `evaluator-mode` is `'agent`. New `--evaluator agent` flag in `/goal` command.
- **Regression verified**: All 9 goal test files pass (state, evaluator, runner, evidence, checks, command, TUI, agent-evaluator, GUI).

#### v0.71.x Series Summary

| Milestone | Theme | New Files | Tests |
|-----------|-------|-----------|-------|
| v0.71.0 | Goal State & Event Foundation | goal-state.rkt + 6 events | 18+ |
| v0.71.1 | Transcript Evaluator + Core Loop | goal-evaluator.rkt, goal-runner.rkt | 22 |
| v0.71.2 | Evidence Discipline + No-Progress | goal-evidence.rkt | 14 |
| v0.71.3 | Deterministic Checks | goal-checks.rkt | 14 |
| v0.71.4 | TUI Integration & Status Display | state-types/event extensions | 11 |
| v0.71.5 | GUI Integration + Docs | gui-types, slash-commands | 5 |
| v0.71.6 | Agent Evaluator + Audit | goal-agent-evaluator.rkt | 7 |

## v0.71.5 — 2026-05-30

### GUI Integration + Release Polish

Sixth milestone of the `/goal` feature — GUI parity and documentation.

- **GUI active-goal**: Added `active-goal` field to `gui-state` struct with `gui-state-set-active-goal` helper.
- **GUI /goal handler**: `/goal clear/status/set` in `gui/slash-commands.rkt`. Added to help text.
- **Documentation**: New `docs/getting-started/goal.md` covering usage, checks, monitoring, limitations, and security.
- **Tests**: 5 GUI tests for struct, setter, hash round-trip.

## v0.71.4 — 2026-05-30

### TUI Integration & Status Display

Fifth milestone of the `/goal` feature — visibility in the TUI.

- **ui-state extension**: New `active-goal` field with `goal-display-info` struct (goal-text, turns-used, max-turns, status).
- **Event reducers**: 6 goal event reducers registered in `state-events.rkt` — `goal.started`, `goal.turn.started`, `goal.evaluated`, `goal.check.completed`, `goal.achieved`, `goal.failed`.
- **Status bar badge**: Goal progress shown in status bar when active: `◎ goal 3/8 · active`. Terminal states: `✓ achieved`, `✗ failed`.
- **Command handler**: `/goal status` shows active goal info. `/goal clear` clears `active-goal` from ui-state.
- **Tests**: 11 TUI tests covering event reducers, display info, badge rendering.

## v0.71.3 — 2026-05-30

### Deterministic Checks

Fourth milestone of the `/goal` feature — user-defined shell commands as verifiable evidence.

- **goal-checks.rkt**: Parse `--check` arguments from `/goal` command. Validate safety via `classify-shell-risks` (blocks critical commands like `rm -rf`). Execute checks via subprocess with timeout + custodian isolation.
- **Evaluator integration**: `evaluate-transcript` gains `#:check-results` parameter. Check results appended to evaluator prompt as deterministic evidence alongside transcript.
- **Runner integration**: `execute-checks-for-goal` runs all checks before LLM evaluation. Results feed directly into evaluator.
- **Command handler**: `/goal "tests pass" --check 'raco test'` parses check arguments, validates safety, shows check summary.
- **Tests**: 14 new tests — parsing, safety, execution, output capture, check-augmented evaluation.

## v0.71.2 — 2026-05-30

### Evidence Discipline + No-Progress Detection

Third milestone of the `/goal` feature — reliable evaluation with evidence requirements.

- **goal-evidence.rkt**: Evidence system prompt injection forces worker agent to produce verifiable evidence (commands, exit codes, file diffs). `evidence-prompt-for-goal` builds continuation prompts with evaluation feedback.
- **No-progress detection**: `detect-no-progress` and `consecutive-same-reason?` detect stall conditions from 3 consecutive same-reason evaluation failures.
- **Runner integration**: Goal runner now uses evidence prompts for continuations and accumulates evaluation history in goal-state checks field. No-progress detection replaces simple counter.
- **Tests**: 17 new tests — evidence prompts (5), no-progress detection (6), runner integration (3), collect-evaluations (1).

## v0.71.1 — 2026-05-30

### Transcript Evaluator + Core Loop

Second milestone of the `/goal` feature — first working autonomous goal loop.

- **goal-evaluator.rkt**: Transcript evaluation via cheap LLM. Sends goal + transcript, parses JSON response `{ok, reason}`. Graceful fallback for non-JSON responses.
- **goal-runner.rkt**: Main goal loop orchestration. Bounded by `max-turns` (default 8). No-progress detection after 3 consecutive same-reason failures. Events emitted at each phase. `goal-run-simulated!` for testing with predefined responses.
- **TUI /goal command**: Added `/goal` and `/g` to command table. Supports `/goal clear`, `/goal status`, `/goal "<description>"`.
- **Tests**: 27 tests — evaluator parsing (5), mock provider evaluation (7), runner continuation (2), simulated runs (4), events (1), single step (2), command parsing (5).

## v0.71.0 — 2026-05-30

### Goal State & Event Foundation

First milestone of the `/goal` autonomous goal-driven loop feature.

- **goal-state.rkt**: Core data model with `goal-state` (11 fields), `goal-check`, `evaluation-result`, and `check-result` structs. Full serialization (hash round-trip) with JSON-safe symbol→string conversion. Contracts on all public constructors.
- **Typed events**: 6 goal events added to `session-events.rkt` — `goal.started`, `goal.turn.started`, `goal.evaluated`, `goal.check.completed`, `goal.achieved`, `goal.failed`.
- **JSONL persistence**: `append-goal-state!` and `load-latest-goal-state` in `session-store.rkt` for session-scoped goal state storage.
- **Context filtering**: `goal-state` kind excluded from context assembly in `session-walk.rkt`.
- **Tests**: 36 tests covering construction, contracts, serialization round-trip, backward compatibility, event construction, persistence, and context filtering.

## v0.70.13 — 2026-05-30

### Audit Hotfix Round 3 + GUI Bug Fixes

- Fix: README v0.70.3 status entry corrected (F1)
- Fix: USER fallback test relative path now works under `raco test` CWD (F2)
- Fix: Slash commands (/help, /status, /clear, etc.) now display immediately
  in GUI transcript — `add-system-msg!` calls `notify-gui!` callback (F3)
- Fix: Text selection in transcript no longer overwritten by streaming output
  or new messages — `insert-message-into-text!` uses explicit position (F4)

## v0.70.12 — 2026-05-30

### Audit Hotfix Round 2 — Closure of v0.70.11 Audit Findings

- Remove duplicate `racket/string` import in credential-backend (F1)
- Add `log-warning` to `drop-old` backpressure handler in trace-sink (F2)
- Fix 9 wrong v0.70.x status descriptions in README (F3)
- Apply `shell-escape` to Windows `cmdkey store!` for defense-in-depth (W1)
- Inline identity function `content-hash` in token-estimate-cache (W2)
- Improve USER fallback test coverage (W3)

## v0.70.11 — 2026-05-30

### Post-Audit Hotfix — Critical/Warning Finding Remediation

- **W0**: Credential backend security fixes
  - New `current-shell-command-runner` parameter for macOS/Windows/capability backends
  - Shell-escape applied to all macOS backend interpolated values (USER, provider, key)
  - Windows backend returns `#f` on load instead of sentinel string
  - Security warning for Windows cmdkey API key exposure
  - File permission race fix: set 0600 before writing content
  - macOS USER env var fallback chain (USER → LOGNAME → who-am-i → "unknown")
  - Fixed mock tests using wrong runner signature
- **W1**: Async sink + trace-logger + token cache fixes
  - `async-session-sink%` flush now propagates to inner sink
  - `trace-logger` closes file handle when async? is true, events routed through sink
  - `token-estimate-cache` uses string identity as key (fixes equal-hash-code collision risk)
  - Removed unused `racket/math` import
- **W2**: Contracts + lint + code quality
  - Added contracts to shell-risk main API functions
  - Fixed tautological string checks in `lint-credential-policy.rkt`
  - `shell-risk-summary` `critical?` now returns boolean
  - `drop-old` policy now logs warning instead of silently falling back to `drop-new`

## v0.70.10 — 2026-05-29

### Series Stabilization + Post-Series Audit

- **W0**: Gate truth lint fixes
  - Fixed `runtime/auth-store.rkt` line-too-long (174→150 chars)
  - Fixed CHANGELOG dash format (–→—) for v0.70.4–v0.70.9
  - Synced README metrics via `metrics.rkt --sync-all`
  - Synced README status block via `sync-readme-status.rkt --sync`
- **W1**: Updated `docs/reports/v0.70.0-audit-baseline.md` with series completion summary
- **W2**: Created `docs/reports/v0.70.10-post-series-audit.md` — full post-series audit report
- **Version bump**: 0.70.10, tag `v0.70.10`

## v0.70.9 — 2026-05-29

### Typed Racket Gradual Migration — Boundary Pilot

- **W0**: Audited 5 candidate modules; selected `util/event-access.rkt` as first target
- **W1**: Converted `util/event-access.rkt` to `#lang typed/racket`
  - Removed manual `racket/contract` boilerplate; TR boundary auto-generates contracts
  - All downstream consumers compile and pass (`agent/event-types.rkt`, `test-stream-error-wrapping.rkt`)
  - Zero contract churn, negligible compile-time impact
- **W2**: Decision — STOP after one successful conversion
  - Second candidate (`util/event-types.rkt`) too small for value
  - Next viable candidate (`util/tool-types.rkt`) re-exported by facade — excluded per risk decision
  - Pilot documented in `docs/reports/TYPED-RACKET-TARGETS-v0.70.9.md`

## v0.70.8 — 2026-05-29

### Large Test File Decomposition

- **W0**: Decomposed `test-cli.rkt` (1202 → 464 lines, 61% reduction)
  - Extracted `test-cli-format.rkt` — format-event-for-terminal (22 tests)
  - Extracted `test-cli-interactive.rkt` — run-cli-interactive prompts, slash commands, error handling (32 tests)
  - Extracted `test-cli-markdown.rkt` — markdown rendering + stream writer (15 tests)
- **W1**: Decomposed `test-gemini.rkt` (1148 → 632 lines, 45% reduction)
  - Extracted `test-gemini-stream.rkt` — stream parsing, single-event parsing, tool-call indices
  - Extracted `test-gemini-provider.rkt` — provider construction, HTTP status, API key validation, security
- **W2**: Verified test runner auto-discovers new subdirectories; version bump

## v0.70.7 — 2026-05-29

### Per-Tool Timeout Override

- **W0**: `timeout-seconds` field added to `tool` struct (10th positional field)
- **W0**: `make-tool` accepts `#:timeout-seconds` keyword; `tool-timeout-seconds` accessor
- **W0**: `tool->jsexpr` serializes `timeoutSeconds` when set
- **W1**: Scheduler `execute-single` injects per-tool timeout into args as `'timeout`
- **W1**: Bash tool respects injected timeout via existing `resolve-exec-limits`
- **W1**: User-provided timeout is not overwritten by tool default

## v0.70.6 — 2026-05-29

### Token Estimation Memoization

- **W0**: `util/token-estimate-cache.rkt` — content-addressed memoization for text token estimation
- **W0**: `cached-estimate-text-tokens` with hit/miss stats and `clear-token-estimate-cache!`
- **W0**: `make-token-estimate-cache` for isolated per-session caches
- **W1**: Replaced ad-hoc cache in `runtime/context-policy.rkt` with `token-estimate-cache.rkt`
- **W1**: Added `token-estimate-cache-hit-stats` for observability
- **W2**: Benchmark shows ~43% speedup for repeated token estimation (100 texts, 10 runs)

## v0.70.5 — 2026-05-29

### Opt-In Async Session/Event Sink Pilot

- **W0**: Session sink ordering characterization tests (file + in-memory sinks)
- **W0**: Hash-chain sequential linking verification
- **W0**: Pending marker cleanup after successful append
- **W1**: `async-session-sink%` class with worker thread and bounded capacity
- **W1**: Opt-in only — default remains sync; no production wiring change
- **W1**: `sink-flush!` blocks until queued writes complete; `sink-close!` stops worker
- **W1**: Integrity proof: `verify-hash-chain` passes after async appends

## v0.70.4 — 2026-05-29

### Async Trace/Event Logging

- **W0**: `async-trace-sink%` class with worker thread and bounded mailbox
- **W0**: Backpressure policies: `block` (default), `drop-new`, `drop-old` (fallback)
- **W0**: `trace-flush!` blocks until worker ack; `trace-close!` terminates worker gracefully
- **W1**: `#:async?` flag in `make-trace-logger` (default `#f` for backward compatibility)
- **W1**: `json-file-trace-sink%` for JSON-formatted trace output
- **W2**: Stress benchmark script `scripts/bench-trace-logger.rkt`

## v0.70.3 — 2026-05-29

### Structured Shell Risk Classification

- **W0**: Conservative shell tokenizer + structured risk classifier (`tools/shell-risk.rkt`)
- **W0**: Risk types: destructive, network-pipe, command-substitution, eval, exec, windows-destructive
- **W1**: Shadow-mode integration in `bash.rkt` — logs diagnostic when regex/classifier disagree
- **W2**: Feature flag `shell-risk-classifier` setting (`regex`, `structured`, `both`)
- **W2**: Default `regex` for backward compatibility

## v0.70.2 — 2026-05-29

### Cross-Platform Credential Backends + Windows Install


## v0.94.8 — Audit remediation, UX hardening, release closure

### Fixed
- **W1 Null-safety**: Guard all callback invocations in `ui-surface.rkt` with `when cb`; fix `ui-callbacks-installed?` to check all 10 fields
- **W2 Overlay + Timeout**: Add `DELTA-SHOW-OVERLAY`/`DELTA-DISMISS-OVERLAY` delta types; enforce render hook timeout via `racket/engine`
- **W3 Lifecycle + Contracts**: Export `current-gui-event-runtime`; add error isolation in `dispatch-gui-hook!`; use action name constants in `ui-delta.rkt`; add contracts on `ui-action->deltas`
- **W4 Schema + Widget**: Consume `ui-action-schema` for payload validation; validate `zone`/`kind` against enumerations in `widget-descriptor-valid?`; add `widget-descriptor-matches-lifecycle?`
- **W5 Layering + Nits**: Remove `tui/state.rkt` import from `ui-surface.rkt` (ARCH-02); add logging to `apply-render-hook-safe`; make `classify-layout-breakpoint` composable; fix `/` query in command filter
- **W6 Test Quality**: Replace manual save/restore with `parameterize`; strengthen parity assertions; add GUI adapter coverage for 7 delta types
- **W7 Integration Gates**: G0-G3 verification — 8 gate tests all pass
- **W9 Regression**: 381+ tests across 44 UI/GUI test files, zero failures

### Changed
- Version bumped to 0.94.8

### Audit Findings Addressed
- C-1: Overlay delta types (W2)
- C-2: Render hook timeout enforcement (W2)
- C-3: Null-safety guards (W1)
- H-1: Export `current-gui-event-runtime` (W3)
- H-2: `ui-make-styled-*` return sensible defaults (W1)
- H-3: Error isolation in `dispatch-gui-hook!` (W3)
- M-1: Contract on `ui-action->deltas` (W3)
- M-2: Consume `ui-action-schema` (W4)
- M-3: Action name constants (W3)
- M-5: Widget zone/kind validation (W4)
- m-1: Hook name validation (W3)
- m-2: `ui-callbacks-installed?` 10-field check (W1)
- m-3: Remove ARCH-02 layering violation (W5)
- m-6: Render hook error logging (W5)
- m-8: Lifecycle token consumer (W4)
- N-1..N-6: Schema comment, member idiom, filter fixes, composable breakpoints
- T-1..T-4: Test isolation, assertion quality, GUI adapter coverage

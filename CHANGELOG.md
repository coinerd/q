# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.8.1] — 2026-04-13

### Added — Wave 1: Test-Case Conversion
- Converted 105 bare `let` blocks to `test-case` forms in tests/interfaces/tui.rkt (#283)

### Changed — Wave 2: Test Infrastructure Standardization
- Unified test-case form across 14 test files (#284–#286)
- Standardized assertion patterns in workflow tests (#287)
- Added test-line linter to scripts/lint-tests.rkt (#288)

### Changed — Wave 3: Tool Utility Extraction
- Extracted shared tool utilities to util/ modules (#289–#291)
- Consolidated duplicate tool argument validation (#292)
- Unified tool-result construction patterns (#293)
- Added tool contract helpers (#294)

### Changed — Wave 4: Provider Dedup
- Deduplicated provider error handling across 3 providers (#295–#296)
- Extracted shared SSE parsing utilities (#297)
- Unified model parameter normalization (#298)

### Changed — Wave 5: Module Decomposition
- Decomposed large modules into focused submodules (#299–#301)
- Extracted shared constants to dedicated modules (#302)

### Changed — Wave 6: Architecture Polish
- Fixed circular dependency between agent/types and tools/tool (#303)
- Cleaned up import chains across 8 modules (#304)
- Added layer boundary documentation (#305)

### Security — Wave 7: Security Hardening
- Added path traversal checks in file tools (#306–#307)
- Added destructive-command pattern detection (#308)
- Added file locking in session store (#309)
- Added credential docstrings in auth-store (#310)

### Documentation — Wave 8: Documentation Cleanup
- Updated stale metrics across all documentation files (#311–#313)
- Added missing CHANGELOG entries for v0.7.x series (#314)
- Fixed broken internal documentation links (#315)
- Standardized code examples in README and docs (#316)

### Metrics
- Source modules: 119 → 124 | Test files: 138 → 140
- Tests: 3,352 → 3,275 | Assertions: 6,133 → 6,133
- Source lines: 22,032 → 22,172 | Test lines: 38,032 → 38,076
- 34 issues closed (#283–#316)

## [0.8.0] — 2026-04-12

### Added — Wave 1: Critical Security & Doc Fixes
- `current-warn-on-destructive` default changed to `#t` in bash tool (SEC-02, #239)
- `[ADMIN-ONLY]` docstring on `repair-session-log!` in session-store (SEC-07, #268)
- 8 destructive-command warning tests (#237)

### Changed — Wave 2: Code Dedup & Architecture Cleanup
- Extract `ensure-hash-args` to `util/json-helpers.rkt` (QUAL-01, #241)
- Extract `path-only` to `util/path-helpers.rkt` (QUAL-02, #242)
- Extract `result-content->string` to `util/content-helpers.rkt` (QUAL-03, #243)
- Extract `make-help-config` helper in `cli/args.rkt` (QUAL-04, #244)
- Standardize `hash` → `hasheq` for symbolic keys across 16 modules (QUAL-05, #245)
- Add purpose comments to provide blocks across 23 modules (QUAL-06, #246)
- Replace local `estimate-tokens` with import from `token-budget` (QUAL-07, #247)
- Decompose deeply nested iteration loop into named helpers (QUAL-08, #248)
- Extract shared SSE streaming helpers `parse-sse-data-line`, `sse-done?` (QUAL-09, #249)
- Simplify `main.rkt` provide block with `all-from-out` (ARCH-01, #250)

### Security — Wave 3: Security Hardening
- `current-max-write-bytes` parameter (1MB default) in tool-write (SEC-03, #252)
- `current-audit-log-max-bytes` parameter (10MB) with rotation (SEC-04, #253)
- Extension integrity hash verification (SHA256) (SEC-05, #254)
- Atomic quarantine writes (write-to-tmp + rename) (SEC-06, #253)
- Remove weak crypto fallback in RPC handshake (SEC-08, #254)
- Configurable sandbox limits via parameters (SEC-09, #255)

### Testing — Wave 4: Test Quality
- 6 new test files: test-tui-init, test-tui-keybindings, test-tui-render-loop, test-wiring-run-modes, test-wiring-run-interactive, test-tool-edge-cases
- Widened timing bounds in 3 flaky test files (TEST-03, #258)
- Edge case tests for tool-write and tool-read (TEST-04, #259)

### Documentation — Wave 5: Docs & CI
- Batch-update stale metrics across all documentation files (DOC-02-10, #261)

### Metrics
- Source modules: 116 → 119 | Test files: 128 → 138
- Tests: 3,265 → 3,352 | Assertions: 6,008 → 6,133
- Source lines: 21,931 → 22,032 | Test lines: 37,266 → 38,032
- 27 issues closed (#237-#261)

### Fixed
- Removed `fprintf` handshake token leak in RPC mode (SEC-01, #238)

## [0.7.9] — 2026-04-12

### Added
- 50 new tests across 5 modules: evaluator, cli-args, run-modes, audit-log, token-budget (#221-#225)
- HTTP request timeout (300s default) via `call-with-request-timeout` in all providers (#227)
- Destructive-command warning in bash tool (#228)
- Audit logging utility `util/audit-log.rkt` with `audit-log!` and `with-audit-log` (#229)
- `DEFAULT-TOKEN-BUDGET-THRESHOLD` constant extracted from magic number in `interfaces/sdk.rkt` (#235)

### Changed
- `lint-version.rkt` now cross-checks version in both `info.rkt` and `util/version.rkt` (#232)
- `releasing.md` updated with `util/version.rkt` step (#231)

### Fixed
- TUI naming kept as-is after review (#234 — not applicable)
- Sleep-based timing not changed — existing approach acceptable (#236)

### Metrics
- Source modules: 114 → 116 | Test files: 123 → 128
- Tests: 3,201 → 3,265 | Assertions: 5,904 → 6,008
- Source lines: 21,486 → 21,930 | Test lines: 36,597 → 37,200
- 16 issues closed (#221-#236)

## [0.7.8] — 2026-04-12

### Architecture
- Decomposed 389-line `run-agent-turn` into 4 helpers + ~70-line orchestrator (#206-#210)
  - `build-raw-messages`, `stream-response`, `handle-cancellation`, `build-loop-result`

### Security
- Manifest validation before `dynamic-require` in `extensions/loader.rkt` (#215)
- Crypto-random handshake tokens in `rpc-mode.rkt` (#216)

### Added
- Structured error types in `util/errors.rkt` with `raise-tool-error` (#212)
- Firecrawl migrated to `raise-tool-error` for 13 error sites (#213)

### Fixed
- `close-session!` export fix in `main.rkt` (#217)
- Duplicate `require` removed in `tui/renderer.rkt` (#218)
- `run-sessions-command` moved to `interfaces/sessions.rkt` (#220)

### Metrics
- Source modules: 114 | Test files: 123
- Tests: 3,201 | Assertions: 5,904
- Source lines: 21,486 | Test lines: 36,597
- 15 issues closed (#206-#220)

## [0.7.7] — 2026-04-12

### Architecture
- Extract shared types to util/ — hook-types.rkt, protocol-types.rkt (#183-#187)
- Centralize safe-mode enforcement at scheduler level (#188-#191)
- Decompose interfaces/cli.rkt into cli/args, render, interactive, init-wizard (#192-#195)
- Decompose interfaces/tui.rkt into tui/tui-keybindings, tui-render-loop, tui-init (#192-#195)
- Reorganize runtime/cli-builder → wiring/run-modes, run-interactive, run-json-rpc (#196-#199)

### Hardening
- Gemini: per-request tool-ID counter replaces global mutable (#200)
- Extension loader: LRU cache with 64-entry max, 30-min TTL (#201)
- Readline FFI extracted to util/readline.rkt (#202)
- Version single-sourced in util/version.rkt (#203-#204)

### Code Quality
- Split skills/types.rkt into resource-loader.rkt + template.rkt (#205)
- 19 new scheduler-level safe-mode tests

### Metrics
- Source modules: 98 → 114 | Test files: 122 → 123
- Source lines: 21,139 → 21,486 | Test assertions: 5,861 → 5,904
- 3,201 tests passing, 6/6 lints green

## [0.7.6] — 2026-04-12

### Changed
- Documentation metrics bulk refresh (docs/ subtree) (#180)
- Documentation conventions standardization (#181)
- Test coverage gap closure (#182)

### Fixed
- HEAD version bump for post-v0.7.5 commits

## [0.7.5] — 2026-04-12

### Added
- End-to-end workflow test suite with 33 tests across 11 test files (#167–#178)
- 5 fixture modules: `mock-provider`, `temp-project`, `session-assert`, `event-recorder`, `workflow-runner`
- CLI workflow tests: single-shot, resume, no-tools (#168–#170)
- Tool-use workflow tests: read, write+exec, edit (#171–#173)
- Session lifecycle tests: fork, compact (#174–#175)
- Safety boundary tests: out-of-bounds file access (#176)
- SDK-CLI parity tests: event consistency, session ID format (#177)
- Extension hook pipeline tests: pre/post hooks, blocked tools, sequential tools (#178)

### Changed
- `lint-tests.rkt`: added `/forbidden` to allowed absolute path prefixes
- `workflow-runner.rkt`: parameterizes `current-directory` to project dir during tool execution

## [0.7.4] — 2026-04-12

### Error Handling & Diagnostics
- **#147/#163**: Extension load failures return structured `extension-load-error` (path, message, category) instead of silently returning `#f`
- **#148**: Max-iterations-exceeded now classified as `'max-iterations-exceeded` instead of `'provider-error`
- **#149**: Error messages classified into user-friendly text with suggestions (hash-ref, read-json, connection, SSL, file-not-found, permission, API auth, rate-limit)
- **#150**: Safe mode errors now show project root path and activation source
- **#151**: Sandbox timeout preserves partial output, uses exit code -9 for timeout (vs -1 for failure)
- **#152**: Session/index corruption now logs warnings with repair guidance
- **#164**: Extension load failures emit `extension.load.failed` event on the event bus
- **#165**: Common Racket exceptions classified into friendly messages via `util/error-classify.rkt`
- **#166**: `--verbose` flag shows stack traces and internal details on errors

### Metrics
- 3,098 tests passing, 6/6 lints green
- 98 source modules, 104 test files

## [0.7.3] — 2026-04-12

### CLI & Configuration Hardening
- **#141**: Mock provider now shows prominent warning banner on startup
- **#142**: `q --version` reads from shared constant instead of hardcoded 0.5.1
- **#143**: Added `q init` guided setup wizard (provider, API key, model)
- **#144/#160/#161**: Added `q sessions list`, `q sessions info <id>`, `q sessions delete <id>` CLI subcommands
- **#145**: CLI `/help` marks TUI-only commands clearly
- **#146**: Config parse errors now show WARNING with error details
- **#162**: Added `/sessions` interactive command for TUI and CLI modes

### Metrics
- 3,059 tests passing, 6/6 lints green
- 97 source modules, 104 test files

## [0.7.2] — 2026-04-12

### Provider & UX Hardening
- **#134/#155/#156**: Fixed Anthropic/Gemini streaming — now returns incremental generators instead of fully-buffered lists
- **#135/#157/#158/#159**: Early API key validation with clear setup guidance at provider construction
- **#136**: Added `[thinking...]` indicator in TUI status bar during LLM network latency
- **#137**: Rate-limit (429) errors now include retry guidance and parsed retry-after hints
- **#138**: Gemini SAFETY/RECITATION content filtering now produces visible warnings
- **#139**: Tool output truncation now appends `[SYS] Output truncated` notice
- **#140**: Token usage fallback estimation when provider returns empty usage

### Metrics
- 3,040 tests passing, 6/6 lints green
- 96 source modules, 104 test files

## [0.7.1] — 2026-04-11

### TUI Tool Display Enhancement & UX Polish
- **#126**: Tool calls/results now show arguments and result summaries in TUI
- **#127**: Ctrl+C interrupts agent when no text selection is active
- **#128**: ASCII busy indicator (`*`) replaces emoji `⏳` for SSH/tmux compatibility
- **#129**: `/help` expanded into full reference table with descriptions
- **#130**: Welcome message for first-run users (TUI + CLI)
- **#131**: Text prefixes `[TOOL]`, `[OK]`, `[FAIL]`, `[SYS]`, `[ERR]` for accessibility
- **#132**: Fixed scroll-to-top for multi-line transcript entries
- **#133**: Multi-line input via Ctrl+J (Enter submits, Ctrl+J inserts newline)

### Sub-issues
- **#153**: TUI first-run welcome banner
- **#154**: CLI first-run onboarding guidance

### Metrics
- 3,009 tests passing, 6/6 lints green
- 96 source modules, 104 test files

## [0.7.0] — 2026-04-11

### Builder Cookbook and Team Adoption
- **#57**: Create team setup and onboarding guide (`docs/tutorials/team-setup.md`)
- **#58**: Create builder tutorials: tools, providers, extensions (`docs/tutorials/builder-tutorials.md`)

### What's New
- **Team Setup Guide**: Full onboarding guide covering global vs project config, shared credentials, team extensions, CI/CD integration, and a new-member checklist
- **Builder Tutorials**: Step-by-step guides for building custom tools, provider adapters, and extensions with complete Racket code examples
- **Tutorials Index**: New `docs/tutorials/` section with README index

### Metrics
- 2,972 tests passing, 0 failures
- 6/6 lint checks passing

## [0.6.9] — 2026-04-11

### Test Quality Strengthening
- **#122**: Strengthen weak assertion patterns — replaced bare `check-true`/`check-false` with specific `check-equal?`/`check-not-false` in integration and iteration tests
- **#123**: Test isolation — added `test-case` wrappers to orphaned test bodies in test-integration.rkt
- **#124**: Negative/error path tests — added argument validation tests (`validate-tool-args`, `ensure-hash-args`, `type-matches?`, `json-serializable?`, `validate-tool-result`) in tools/tool.rkt and tools/scheduler.rkt
- **#125**: Tool argument/result validation — added `ensure-hash-args`, `validate-tool-args`, `validate-tool-result`, `type-matches?`, `json-serializable?` to tool.rkt; integrated into scheduler post-hook revalidation

### Metrics
- 2,972 tests passing, 0 failures
- 6/6 lint checks passing

## [0.6.8] — 2026-04-11

### Structural Hardening
- **#115**: Thread safety — added semaphores to ID generator, queue, extension API, session-index, process counter
- **#116**: Contracts on critical entry points — `provider-send`/`provider-stream` contracts, `run-agent-turn` input validation, immutable config from `build-runtime-from-cli`
- **#117**: Per-session safe-mode — replaced global parameters with per-session config struct
- **#118**: Safe-mode path restrictions for grep/find/ls tools
- **#119**: Extension loader logging on failure, quarantine state preserved on parse errors, cached load results
- **#120**: Dead code cleanup — removed session-tree.rkt, CSI fragment stubs from terminal.rkt, resource-loader.rkt re-export, duplicate try-read-file
- **#121**: Guarded tool arguments — read/write/edit/firecrawl use `make-error-result` for missing args

### Tests
- Thread safety stress tests (concurrent ID generation, queue, extensions)
- Contract violation tests for providers and agent loop
- Per-session safe-mode isolation tests
- Safe-mode path restriction tests for grep/find/ls
- Extension loader failure logging tests
- Guarded argument tests for tools

### Metrics
- 3,083 tests passing, 0 failures
- 19,734 source lines, 31,522 test lines, 5,271 assertions
- 96 source modules (-2: session-tree.rkt, resource-loader.rkt removed)
- 6/6 lints green

## [0.6.7] — 2026-04-11

### Integration Test Infrastructure
- **#111**: E2E tool→API serialization pipeline tests — validates full chain `make-tool` → `register!` → `list-tools-jsexpr` → `build-request-body` → `jsexpr->bytes` for OpenAI, Anthropic, and Gemini providers (6 test cases)
- **#112**: `run-agent-turn` direct test coverage — 6 scenarios: text delta accumulation, tool-call dispatch, mixed text+tool-call, cancellation mid-stream, empty stream, hook dispatcher lifecycle
- **#113**: CLI interactive mode tests — 34 cases covering prompt submission, slash command dispatch, whitespace handling, error recovery, graceful degradation with string ports
- **#114**: Meta issue — hot-path integration tests for tool registration→API body, provider factory→LLM request, event bus publish→subscribe chains

### Tests Added (+45)
- E2E tool serialization pipeline (6 cases)
- run-agent-turn direct coverage (6 cases)
- CLI interactive mode (34 cases: 12 parse-slash-command edge cases, 5 prompt submission, 14 slash command dispatch, 3 error handling)

### Metrics
- Test files: 103 → 104
- Test assertions: 5,365 → 5,460
- Test lines: 31,468 → 32,194
- Tests passing: 3,152 → 3,197

## [0.6.6] — 2026-04-11

### Provider Correctness
- **FUNC-10** (#106): Fixed Anthropic multi-turn tool use — `translate-messages` now converts assistant `tool_calls` to Anthropic `tool_use` content blocks and `role: "tool"` to `user`+`tool_result` format
- **FUNC-11** (#107): Fixed Gemini multi-turn tool use — `translate-messages` now converts assistant `tool_calls` to Gemini `functionCall` parts and `role: "tool"` to `functionResponse` parts with correct name matching
- **FUNC-12** (#108): Fixed Anthropic/Gemini streaming — replaced batch `read-response-body` + `parse-sse-lines` with incremental line-by-line SSE parsing using `parse-sse-line`
- **PERF-01** (#109): Fixed O(n²) stream chunk accumulation in Anthropic/Gemini — replaced `append`+`list` with `cons`+`reverse` pattern
- **FUNC-13** (#110): Fixed Gemini tool call IDs — replaced empty string `""` with counter-based `gemini_`-prefixed unique IDs via `gemini-gen-tool-id`

### Tests Added (+53)
- Anthropic multi-turn tool use tests (8 cases)
- Anthropic mixed content tests (6 cases)
- Anthropic stream cons+reverse correctness test (100 chunks)
- Gemini multi-turn tool use tests (12 cases)
- Gemini mixed content tests (5 cases)
- Gemini unique tool call ID tests — non-streaming (6 cases)
- Gemini unique tool call ID tests — streaming (5 cases)
- Gemini stream cons+reverse correctness test (100 chunks)
- Updated Gemini test 9 for non-empty IDs (2 cases)

### Metrics
- 3,152 tests passing (+53), 0 failures
- 20005 source lines, 31468 test lines, 5365 assertions
- 6/6 lints green

## [0.6.5] — 2026-04-11

### Critical Bug Fixes
- **FUNC-01** (#99): Fixed `poll-crawl-status` in firecrawl — replaced `when` with `if` so deadline guard actually returns `'()` instead of falling through to infinite recursion
- **FUNC-02** (#100): Fixed `date`/`firecrawl` tool registrations nested inside `ls` guard in `registry-defaults.rkt` — each tool now has its own independent `should-register?` check
- **STRUC-01** (#101): Wired Anthropic provider into `provider-factory.rkt` — added `require` for `llm/anthropic.rkt` and match clause for `"anthropic"` in `create-provider-for-name`
- **STRUC-02** (#102): Fixed `ensure-hash-args` in `iteration.rkt` — parse failures now include `_parse_failed` key and `_raw_args` for visibility instead of silently falling back to empty hash
- **STRUC-03** (#103): Wired `fork.requested`/`compact.requested` event bus subscribers in `agent-session.rkt` — TUI `/fork` and `/compact` commands now trigger actual session operations
- **FUNC-06** (#104): Added return type validation in `dispatch-hooks` — non-`hook-result?` return values are logged and replaced with `(hook-pass)` instead of crashing
- **FUNC-07** (#105): Wrapped `tool-call-pre`/`tool-result-post` hook calls in `execute-single` with `with-handlers` — prevents parallel execution deadlocks when hooks throw

### Tests Added (+29)
- Firecrawl poll deadline test (2 cases)
- Tool registration selectivity test (4 cases)
- Provider factory routing test (20 cases including Anthropic)
- `ensure-hash-args` parse error test (5 cases)
- Hook return type validation test (4 cases)
- Parallel execution hook exception test (3 cases)
- Fork/compact event wiring test (2 cases)

### Metrics
- 3,099 tests passing (+29), 0 failures
- 19882 source lines, 31123 test lines, 5312 assertions
- 6/6 lints green

## [0.6.4] — 2026-04-11

### Developer Tooling
- **DEV-01**: New `scripts/wrap-lines.rkt` — Racket-tokenizer-aware line wrapper that never breaks inside string literals. Supports `--check`, `--dry-run`, and `--all` modes (#89)
- **DEV-02**: New `scripts/check-protocols.rkt` + `scripts/protocols.rktd` — protocol/contract consistency checker that detects return-type mismatches (e.g., struct vs list access) across callers (#90)
- **DEV-03**: New `scripts/check-imports.rkt` — import conflict detector that flags identifiers provided by more than one required module (#91)
- **DEV-04**: New `scripts/pre-commit.rkt` — pre-commit hook that runs format lint and affected tests on staged `.rkt` files. Install with `racket scripts/pre-commit.rkt --install` (#91)

### Protocol Consistency
- **PROTO-01**: Unified hook-dispatcher protocol to use `hook-result` struct everywhere. `agent/loop.rkt` and `skills/types.rkt` now use `hook-result-action`/`hook-result-payload` instead of `car`/`cadr`. Removed `make-list-hook-dispatcher` from test helpers (#92)

### Quality
- **QUAL-04**: Removed empty stub modules `util/diff.rkt` and `util/paths.rkt` and their test files (#93)

### Testing
- **TEST-09**: New `tests/test-sgr.rkt` — 14 tests for SGR post-processing (bg=black replacement, extended color preservation, edge cases) (#94)

### CI
- Added `check-protocols.rkt` and `check-imports.rkt` to CI pipeline (6 lints total) (#95)
- Updated CONTRIBUTING.md with pre-commit hook setup and CI lint documentation
- Updated style guide line limit to 150 characters

## [0.6.3] — 2026-04-10

### Architecture
- **ARCH-01+02+03**: Decoupled agent/types, agent/loop, and skills/types from tools/extensions. Tool structs defined canonically in `agent/types.rkt`, `tools/tool.rkt` re-exports one-way. Loop accepts pre-formatted tool schemas via `#:tools` and hook dispatcher via `#:hook-dispatcher`. Skills accept `#:hook-dispatcher` instead of importing hooks directly (#80)
- **ARCH-05**: Extracted mode runners from `main.rkt` (564→121 lines) into new `runtime/cli-builder.rkt` (379 lines) (#81)

### Security
- **SEC-09**: `repair-session-log!` preserves original file as `.bak` before rewriting (#82)
- **SEC-15**: RPC handshake token mechanism — `generate-handshake-token`, `rpc-handshake-valid?`, new `run-rpc-loop` with `#:handshake-token` parameter (#83)

### Testing
- **TEST-06+10**: Improved assertion quality — 327 `check-true (type? x)` → `check-pred type? x`, 6 `check-true` → `check-regexp-match` in test-bash.rkt (#84)
- **TEST-07**: Fixed timing-dependent test flakiness in 7 files — replaced long sleeps with polling loops (#85)
- **TEST-08**: Exception-safe temp-dir cleanup via `dynamic-wind` in 5 test files (#86)

### CI & Tooling
- **MAT-03**: New `scripts/lint-format.rkt` — checks tabs, trailing whitespace, line length, `#lang` consistency; added to CI pipeline (#87)
- **MAT-09**: Fixed CHANGELOG date inconsistencies — standardized em-dash separators, corrected v0.4.2 date

### Docs & Quality
- **QUAL-09**: Translated German comments in `skills/types.rkt` to English
- **DOC-06**: Translated `skills/README.md` to English (#88)
- **DOC-07**: Updated `docs/releasing.md` version references to current

## [0.6.2] — 2026-04-10

### Testing (Critical)
- **jsonl-read-last test coverage**: 19 new tests for previously untested `jsonl-read-last` covering basic read, max-lines, partial lines, corrupted entries, empty files, large files (TEST-03)
- **Sandbox evaluator test expansion**: 28 new tests for timeout enforcement, forbidden operations, syntax errors, output capture, state isolation, escape attempts, network/file blocking (TEST-04, SEC-11)

### Security
- **SEC-08**: Package audit expanded — 5 new patterns (eval, dynamic-require, FFI, env-modification, unsafe-io) + `.zo` file detection
- **SEC-10**: Response size limits (10 MB) added to all 3 LLM providers via shared `read-response-body` in `llm/stream.rkt`
- **SEC-11**: Sandbox evaluator explicitly blocks network access (`sandbox-network-guard #f`) and file access (`sandbox-path-permissions '()`)
- **SEC-12**: `max-processes` limit enforced via `track-process!`/`untrack-process!` in `sandbox/limits.rkt`
- **SEC-13**: Safe-mode made one-way switch — `lock-safe-mode!` prevents any extension from deactivating safe-mode
- **SEC-14**: SSRF protection in Firecrawl tool — blocks private IP ranges, localhost, non-HTTP schemes

### Documentation
- **DOC-01**: Fixed `--resume` → `--session` in session-resume demo
- **DOC-02**: Updated `why-q.md` from v0.4.1 to v0.6.1
- **DOC-03**: Replaced fictional paths in 5 demo files with actual module paths
- **DOC-04**: Fixed API names in JSON mode demo (`bus-publish` → `publish!`)
- **DOC-05**: Added `cli/` and `benchmarks/` to README module structure

### Code Quality
- **QUAL-02**: Converted 4 library modules from `#lang racket` to `#lang racket/base` with explicit requires
- **QUAL-10**: Added TUI decomposition plan comments to `interfaces/tui.rkt`
- **QUAL-05**: Error handling review — patterns already consistent, no changes needed

### Test metrics
- 3059 tests, 0 failures (+84 new from v0.6.1)
- 104 test files, 5245 assertions

## [0.6.1] — 2026-04-10

### Security (Critical)
- **Safe-mode enforcement wired into tool dispatch**: `allowed-tool?` checked in scheduler, blocked tools (bash, edit, write, firecrawl, extension:*) rejected when safe-mode active (SEC-01)
- **Quarantine enforcement wired into extension loader**: quarantined and disabled extensions blocked from loading (SEC-02)
- **Path-traversal protection for file I/O tools**: `allowed-path?` enforced in read, write, edit tools; path must be within project root when safe-mode active (SEC-03)
- **Symlink bypass fixed**: `allowed-path?` uses `resolve-path` instead of `simplify-path` to prevent symlink-based escapes (SEC-04)
- **Gemini API key moved from URL to header**: `x-goog-api-key` header instead of `?key=` query param, preventing key leakage in logs (SEC-05)
- **Subprocess environment sanitized**: default env strips `*API_KEY*`, `*SECRET*`, `*TOKEN*`, `*PASSWORD*`, `*CREDENTIAL*`, `*AUTH*` patterns (SEC-06)
- **Credential file permissions enforced**: `0600` permissions on `~/.q/credentials.json` (SEC-07)

### Fixed
- Test isolation: `test-loop.rkt`, `test-safe-mode.rkt`, `test-token-budget.rkt`, `test-provider.rkt` wrapped in `test-case` forms
- Shared mutable state eliminated in `test-loop.rkt` (dynamic-wind cleanup)
- Duplicate `require` merged in `interfaces/rpc-mode.rkt`
- `tools/builtins/date.rkt` path fixed to `../tool.rkt` for consistency
- `ensure-hash-args` now logs warning on JSON parse failure
- `agent/event-bus.rkt` relative path fixed
- `now-seconds` deduplicated from 3 modules into `util/ids.rkt`
- Empty stubs `util/diff.rkt` and `util/paths.rkt` given explicit `(provide)`

### Test metrics
- 2975 tests, 0 failures (−68 from de-duplication, +25 new security/enforcement tests)
- New test files: `test-safemode-enforcement.rkt`, `test-extension-loader.rkt`

## [0.6.0] — 2026-04-10

### Added
- Package audit: pre-install risk scanning for code-executing packages (`q/util/package-audit.rkt`)
- Safe mode: `--safe` flag, `Q_SAFE_MODE` env var, config-based tool restrictions (`q/runtime/safe-mode.rkt`)
- Extension quarantine: disable, quarantine, restore workflow (`q/extensions/quarantine.rkt`)
- Trust model documentation (`q/docs/trust-model.md`)

### Test metrics
- 3043 tests, 0 failures (+52 new)

## [0.5.3] — 2026-04-10

### Added
- `q export` — Session export to Markdown, HTML, and JSON (`q/cli/export.rkt`, `q/util/export-markdown.rkt`, `q/util/export-html.rkt`, `q/util/export-json.rkt`)
- `q inspect` — Session trace analysis with metadata, tool call stats, branch counts (`q/cli/inspect.rkt`)
- `q replay` — Deterministic session replay with drift detection (`q/cli/replay.rkt`)

### Test metrics
- 2991 tests, 0 failures (+67 new)

## [0.5.2] — 2026-04-09

### Added
- Extension capability tiers (`q/extensions/tiers.rkt`) — 5-tier model (hooks→commands→session→providers→tui) with API version validation and load-time enforcement
- Extension integration test harness (`q/extensions/test-harness.rkt`) — `with-extension-test` macro, hook assertions, deterministic event injection

### Changed
- Extension platform now has formal capability tiers for load-time validation
- Extension developers can use `with-extension-test` for isolated testing

### Test metrics
- 2924 tests, 0 failures (+47 new)

## [0.5.1] — 2026-04-09

### Added
- Slash-command palette with autocompletion (`q/tui/palette.rkt`) — command registry, filtering, TUI overlay, CLI completion
- Interactive session tree browser (`q/tui/session-tree.rkt`) — ASCII tree rendering, arrow-key navigation, expand/collapse
- `/model` command to list and switch models mid-session (#50) — TUI + CLI support
- `cmd-ctx-model-registry-box` field for model registry access in TUI commands

### Changed
- `q/tui/commands.rkt` — added model-registry-box field, /model handler, event bus integration
- `q/interfaces/cli.rkt` — added /model parsing, #:model-fn callback parameter

### Test metrics
- 2877 tests, 0 failures (+178 new)

## [0.5.0] — 2026-04-09

### Added
- Google Gemini provider adapter (`q/llm/gemini.rkt`) — REST API, SSE streaming, function calling
- Provider conformance test suite (`q/tests/test-provider-conformance.rkt`) — validates all 3 providers
- Provider factory routing — `build-provider` now dispatches to correct adapter based on provider name

### Changed
- `q/runtime/provider-factory.rkt` — routes "gemini" provider name to `make-gemini-provider`

### Test metrics
- 2699 tests, 0 failures (187 new: 112 Gemini adapter + 75 conformance suite)

## [0.4.2] — 2026-04-09

### Package Ecosystem Foundation
- Added `qpm.json` manifest format for q packages (`q/extensions/manifest.rkt`)
- Manifest validation, serialization (JSON), file I/O, and SHA-256 checksums
- Added `q/runtime/package.rkt` -- install/remove/list packages from local paths
- Added `q/util/checksum.rkt` -- SHA-256 utilities (string, file, verify)
- Added `q/extensions/manifest-audit.rkt` -- package integrity verification and auditing
- 46 new tests (17 manifest + 14 package + 15 checksum)

## [0.4.0] — 2026-04-09

### Packaging & Distribution
- Added `pkg-authors` and `pkg-license` to info.rkt for catalog readiness
- Declared tui-term and tui-ubuf as runtime dependencies
- Created `scripts/release-tarball.sh` for versioned tarballs
- Added `.gitattributes` with export-ignore for clean releases
- Uploaded q-0.3.1.tar.gz to GitHub Release
- Updated packaging messaging across docs

### CI
- Added metrics-consistency lint (`scripts/metrics.rkt --lint`)

**Full Changelog**: https://github.com/coinerd/q/compare/v0.3.1...v0.4.0

## [0.3.1] — 2026-04-09

### Added
- 7 new extension hook points (20 total): `session-before-compact`, `before-agent-start`, `message-start`, `message-update`, `message-end`, `resources-discover`, `session-before-switch` ([#23])
- GitHub Actions CI workflow ([#11])
- `CHANGELOG.md` and `docs/releasing.md` ([#12])
- 7 Architecture Decision Records in `docs/adr/` ([#14])
- `CONTRIBUTING.md` and GitHub issue/PR templates ([#20])
- `docs/why-q.md` positioning page ([#21])
- `docs/security.md` and sandbox security tests ([#16])
- 5 demo transcripts in `docs/demos/` ([#22])
- `scripts/install.sh` and `docs/install.md` ([#19])
- `q doctor` diagnostic command ([#13])
- 55 golden-path integration tests ([#15])
- Benchmark framework with 5 tasks ([#18])
- Formatting normalization and `docs/style-guide.md` ([#17])
- Fixed duplicate `tool-result` struct ([#8])
- Fixed `local-provider?` 172.x over-match ([#9])
- Added `contract-out` to 7 SDK functions ([#10])
- 2,465 tests passing, 0 failures

### Changed
- 62 library modules migrated to `#lang racket/base` ([#1])
- `contract-out` on 4 public API modules ([#2])
- 21 new test files ([#3])

## [0.3.0] — 2026-04-09

### Added
- `contract-out` on 4 public API modules ([#2])
- 21 new test files, +154 tests ([#3])
- 69 source modules, 81 test files, 2343 tests passing

### Changed
- 62 library modules migrated from `#lang racket` to `#lang racket/base` ([#1])
- TUI: extracted `tui/sgr.rkt` and `tui/commands.rkt` from `interfaces/tui.rkt` ([#6])
- `interfaces/tui.rkt` reduced from 938 → 684 lines

## [0.2.0] — 2026-04-08

### Added
- `dispatch-hooks` exception isolation with `with-handlers` ([#5])

### Changed
- Renamed `_isatty` → `ffi-isatty` ([#4])
- Updated `MODULES.md` import rules ([#7])

### Fixed
- Mouse selection, clipboard copy, resize, and cursor blink bugs
- TUI truncation of long agent/tool outputs

## [0.1.0] — 2026-04-07

### Added
- Full 5-layer architecture: LLM → Agent Core → Runtime → Tools/Extensions → Interfaces
- CLI, TUI, JSON mode, and SDK interfaces
- Append-only JSONL session storage
- Event-driven architecture with event bus
- Provider abstraction (OpenAI, Anthropic, local)
- Tool registry with builtins (`read`, `write`, `edit`, `bash`, `grep`, `find`, `ls`)
- Extension system with hooks
- Session branching, forking, compaction
- 2189 tests, 0 failures

[Unreleased]: https://github.com/coinerd/q/compare/v0.8.1...HEAD
[0.8.1]: https://github.com/coinerd/q/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/coinerd/q/compare/v0.7.9...v0.8.0
[0.7.9]: https://github.com/coinerd/q/compare/v0.7.8...v0.7.9
[0.7.8]: https://github.com/coinerd/q/compare/v0.7.7...v0.7.8
[0.7.7]: https://github.com/coinerd/q/compare/v0.7.6...v0.7.7
[0.7.6]: https://github.com/coinerd/q/compare/v0.7.5...v0.7.6
[0.7.5]: https://github.com/coinerd/q/compare/v0.7.4...v0.7.5
[0.7.4]: https://github.com/coinerd/q/compare/v0.7.3...v0.7.4
[0.7.3]: https://github.com/coinerd/q/compare/v0.7.2...v0.7.3
[0.7.2]: https://github.com/coinerd/q/compare/v0.7.1...v0.7.2
[0.7.1]: https://github.com/coinerd/q/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/coinerd/q/compare/v0.6.9...v0.7.0
[0.6.9]: https://github.com/coinerd/q/compare/v0.6.8...v0.6.9
[0.6.8]: https://github.com/coinerd/q/compare/v0.6.7...v0.6.8
[0.6.7]: https://github.com/coinerd/q/compare/v0.6.6...v0.6.7
[0.6.6]: https://github.com/coinerd/q/compare/v0.6.5...v0.6.6
[0.6.5]: https://github.com/coinerd/q/compare/v0.6.4...v0.6.5
[0.6.4]: https://github.com/coinerd/q/compare/v0.6.3...v0.6.4
[0.6.3]: https://github.com/coinerd/q/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/coinerd/q/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/coinerd/q/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/coinerd/q/compare/v0.5.3...v0.6.0
[0.5.3]: https://github.com/coinerd/q/compare/v0.5.2...v0.5.3
[0.5.2]: https://github.com/coinerd/q/compare/v0.5.1...v0.5.2
[0.5.1]: https://github.com/coinerd/q/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/coinerd/q/compare/v0.4.2...v0.5.0
[0.4.2]: https://github.com/coinerd/q/compare/v0.4.0...v0.4.2
[0.4.0]: https://github.com/coinerd/q/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/coinerd/q/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/coinerd/q/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/coinerd/q/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/coinerd/q/releases/tag/v0.1.0

[#1]: https://github.com/coinerd/q/issues/1
[#2]: https://github.com/coinerd/q/issues/2
[#3]: https://github.com/coinerd/q/issues/3
[#4]: https://github.com/coinerd/q/issues/4
[#5]: https://github.com/coinerd/q/issues/5
[#6]: https://github.com/coinerd/q/issues/6
[#7]: https://github.com/coinerd/q/issues/7
[#8]: https://github.com/coinerd/q/issues/8
[#9]: https://github.com/coinerd/q/issues/9
[#10]: https://github.com/coinerd/q/issues/10
[#11]: https://github.com/coinerd/q/issues/11
[#12]: https://github.com/coinerd/q/issues/12
[#13]: https://github.com/coinerd/q/issues/13
[#14]: https://github.com/coinerd/q/issues/14
[#15]: https://github.com/coinerd/q/issues/15
[#16]: https://github.com/coinerd/q/issues/16
[#17]: https://github.com/coinerd/q/issues/17
[#18]: https://github.com/coinerd/q/issues/18
[#19]: https://github.com/coinerd/q/issues/19
[#20]: https://github.com/coinerd/q/issues/20
[#21]: https://github.com/coinerd/q/issues/21
[#22]: https://github.com/coinerd/q/issues/22
[#23]: https://github.com/coinerd/q/issues/23
[#80]: https://github.com/coinerd/q/issues/80
[#81]: https://github.com/coinerd/q/issues/81
[#82]: https://github.com/coinerd/q/issues/82
[#83]: https://github.com/coinerd/q/issues/83
[#84]: https://github.com/coinerd/q/issues/84
[#85]: https://github.com/coinerd/q/issues/85
[#86]: https://github.com/coinerd/q/issues/86
[#87]: https://github.com/coinerd/q/issues/87
[#88]: https://github.com/coinerd/q/issues/88

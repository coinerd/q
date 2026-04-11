# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/coinerd/q/compare/v0.6.2...HEAD
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

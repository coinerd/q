# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

## [0.4.2] - 2026-04-10

### Package Ecosystem Foundation
- Added `qpm.json` manifest format for q packages (`q/extensions/manifest.rkt`)
- Manifest validation, serialization (JSON), file I/O, and SHA-256 checksums
- Added `q/runtime/package.rkt` -- install/remove/list packages from local paths
- Added `q/util/checksum.rkt` -- SHA-256 utilities (string, file, verify)
- Added `q/extensions/manifest-audit.rkt` -- package integrity verification and auditing
- 46 new tests (17 manifest + 14 package + 15 checksum)

## [0.4.0] - 2026-04-10

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

## [0.3.1] - 2026-04-09

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

## [0.3.0] - 2026-04-09

### Added
- `contract-out` on 4 public API modules ([#2])
- 21 new test files, +154 tests ([#3])
- 69 source modules, 81 test files, 2343 tests passing

### Changed
- 62 library modules migrated from `#lang racket` to `#lang racket/base` ([#1])
- TUI: extracted `tui/sgr.rkt` and `tui/commands.rkt` from `interfaces/tui.rkt` ([#6])
- `interfaces/tui.rkt` reduced from 938 → 684 lines

## [0.2.0] - 2026-04-08

### Added
- `dispatch-hooks` exception isolation with `with-handlers` ([#5])

### Changed
- Renamed `_isatty` → `ffi-isatty` ([#4])
- Updated `MODULES.md` import rules ([#7])

### Fixed
- Mouse selection, clipboard copy, resize, and cursor blink bugs
- TUI truncation of long agent/tool outputs

## [0.1.0] - 2026-04-07

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

[Unreleased]: https://github.com/coinerd/q/compare/v0.5.2...HEAD
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

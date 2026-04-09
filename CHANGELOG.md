# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

[Unreleased]: https://github.com/coinerd/q/compare/v0.3.0...HEAD
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

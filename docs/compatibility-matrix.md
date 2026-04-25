# Compatibility Matrix

<!-- verified-against: 0.19.7 -->

This document tracks compatibility between q versions, Racket versions, and
extension API versions.

## Racket Version Compatibility

| q Version | Min Racket | Tested Racket | Notes |
|-----------|-----------|---------------|-------|
| 0.15.x | 8.10 | 8.10 | Event-driven core, sandboxing, extensions, TUI |
| 0.14.x | 8.10 | 8.10 | Trace logger, steering fixes, max_tokens resolution |
| 0.13.x | 8.10 | 8.10 | Session tree, provider improvements |
| 0.12.x | 8.10 | 8.10 | OAuth framework, credential store |
| 0.11.x | 8.10 | 8.10 | Tool scheduler, extension hooks v2 |
| 0.10.x | 8.10 | 8.10 | Requires `racket/base`, `rackunit` |
| 0.9.x | 8.10 | 8.10 | Same as above |
| 0.8.x | 8.9 | 8.9, 8.10 | Backward compatible with 8.9 |

## Extension API Version Compatibility

| q Version | Extension API | Breaking? | Migration |
|-----------|--------------|-----------|-----------|
| 0.15.0+ | `0.5` | No | Additive — safe-mode hooks, quarantine |
| 0.13.0+ | `0.4` | No | Additive — session-tree hooks |
| 0.10.0+ | `0.3` | No | Additive — new hook types |
| 0.9.0+ | `0.2` | No | Additive — extension context fields |
| 0.8.0+ | `0.1` | Yes | Changed `extension` struct to 4-arg |

## Provider Compatibility

| Provider | Min q Version | Status |
|----------|--------------|--------|
| OpenAI-compatible | 0.1.0 | Stable |
| Anthropic (Claude) | 0.5.0 | Stable |
| Gemini | 0.7.0 | Stable |
| Local (llama.cpp server) | 0.6.0 | Stable |

## TUI Dependencies

| Dependency | Min Version | Required For |
|------------|-------------|--------------|
| `tui-term` | 0.1 | Terminal abstraction |
| `tui-ubuf` | 0.1 | Unicode buffer |
| `charterm` | 1.0 | Direct terminal I/O (fallback) |

## Updating This Matrix

This matrix should be updated:
1. When a new q version is released
2. When minimum Racket version changes
3. When extension API version bumps
4. When new providers are added

The lint-docs script (`racket scripts/lint-docs.rkt`) checks that this file
has a valid `verified-against` marker matching the current version.

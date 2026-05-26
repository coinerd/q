# Architecture Overview

## Version

v0.59.2

## Layer Diagram

```
┌─────────────────────────────────────────────┐
│              INTERFACES                       │
│  CLI (cli/) · TUI (tui/) · SDK (interfaces/) │
├─────────────────────────────────────────────┤
│              WIRING                           │
│  wiring/ — run modes, mode helpers            │
├─────────────────────────────────────────────┤
│           RUNTIME & EXTENSIONS                │
│  runtime/ · extensions/                       │
├─────────────────────────────────────────────┤
│            AGENT CORE                         │
│  agent/ — event bus, loop, event structs      │
├─────────────────────────────────────────────┤
│            LLM PROVIDERS                      │
│  llm/ — OpenAI, Anthropic, Azure              │
├─────────────────────────────────────────────┤
│             TOOLS                             │
│  tools/ — builtins, registry, scheduler       │
├─────────────────────────────────────────────┤
│            FOUNDATION                         │
│  util/ — types, JSONL, events, errors         │
├─────────────────────────────────────────────┤
│            SANDBOX                            │
│  sandbox/ — subprocess isolation              │
└─────────────────────────────────────────────┘
```

## Module Counts (428 source modules, ~65,000 LOC)

| Layer | Modules | Key Files |
|-------|---------|-----------|
| Foundation (util/) | ~25 | `protocol-types.rkt`, `event.rkt`, `errors.rkt`, `version.rkt` |
| LLM (llm/) | ~10 | `provider.rkt`, `openai-compatible.rkt`, `anthropic.rkt` |
| Agent Core (agent/) | ~15 | `loop.rkt`, `event-bus.rkt`, `event-structs/` |
| Tools (tools/) | ~15 | `tool.rkt`, `registry-defaults.rkt`, `builtins/` |
| Runtime (runtime/) | ~45 | `iteration.rkt`, `agent-session.rkt`, `session-lifecycle.rkt` |
| Extensions (extensions/) | ~30 | `gsd/`, `github/`, `racket-tooling/` |
| Interfaces (interfaces/, cli/, tui/) | ~45 | `sdk-core.rkt`, `args.rkt`, `renderer.rkt`, `tui/keybindings/` |
| Wiring (wiring/) | ~3 | `run-modes.rkt`, `mode-helpers.rkt` |
| Sandbox (sandbox/) | ~3 | `subprocess.rkt` |
| Scripts (scripts/) | ~25 | `lint-all.rkt`, `sync-version.rkt`, `run-tests.rkt` |

## Key Contracts

- **Tool contracts**: `tools/tool.rkt` — `(tool? → (or/c text? json?))`
- **SDK contracts**: `interfaces/sdk-core.rkt` — `contract-out` on public API
- **Error hierarchy**: `util/errors.rkt` — `q-error` → `session-error`, `extension-error`, `policy-error`
- **Event types**: `util/event.rkt` (raw) + `agent/event-structs/` (typed)

## Dependency Policy

See `docs/architecture/dependency-policy.rktd` for layering rules and known violations.

## Event System

Two tiers:
1. **Raw events**: `make-event` + `emit-event!` (legacy, stable for interop)
2. **Typed events**: 27 structs in `agent/event-structs/` + `emit-typed-event!` (introduced v0.33.7, stable)

## Typed Racket Modules

Per ADR 0014, stable struct-heavy modules are migrated to Typed Racket:
- `util/event.rkt`, `util/event-payloads.rkt`, `util/hook-types.rkt`
- `util/version.rkt`, `util/loop-result.rkt`
- `extensions/gsd/plan-types.rkt`, `extensions/gsd/plan-validator.rkt`

## Contract Metrics

- Baseline: 882 any/c (148 files)
- Current: 812 any/c (143 files) — 7.9% reduction
- See `docs/reports/f2-contract-precision-trend.md` for trend analysis

- 500+ test files in `tests/`
- Parallel runner: `racket scripts/run-tests.rkt`
- 22-check lint suite: `racket scripts/lint-all.rkt`
- Contract metrics: `racket scripts/contract-metrics.rkt`
- Hotspot report: `racket scripts/hotspot-report.rkt`
- IVG invariants: 8 checks in `scripts/lint-ivg.rkt`

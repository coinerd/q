# Architecture Overview

## Version

v0.71.0

## Native TUI Stack

The TUI layer uses a seven-layer native Racket architecture (no external dependencies).
See ADR-0016 for full details.

```
┌───────────────────────────────────────┐
│       Bridge (vdom-bridge.rkt)        │  Integration with render loop
├───────────────────────────────────────┤
│    Components (vdom-components.rkt)   │  Typed zone components + state bag
├───────────────────────────────────────┤
│   Virtual DOM (vdom*.rkt, 3 files)    │  vnode trees + layout + render
├───────────────────────────────────────┤
│   Cell Diff (cell-diff*.rkt, 2 files) │  Incremental rendering
├───────────────────────────────────────┤
│    Cell Buffer (cell-buffer.rkt)      │  2D cell grid storage
├───────────────────────────────────────┤
│  Terminal I/O (terminal-native.rkt)   │  ANSI sequences, raw mode
└───────────────────────────────────────┘
```

Key files: `tui/terminal-native.rkt`, `tui/cell-buffer.rkt`, `tui/cell-diff.rkt`,
`tui/cell-diff-render.rkt`, `tui/vdom.rkt`, `tui/vdom-layout.rkt`,
`tui/vdom-render.rkt`, `tui/vdom-bridge.rkt`, `tui/vdom-components.rkt`, `tui/component.rkt`

### Render Pipeline (since v0.61.5)

The TUI uses a fully vdom-mediated, multi-backend render path. Components persist
across frames with local state, and the same vdom output can target multiple backends.

```
ui-state → render-frame-vdom!
              ├── Header    → component (cached) → vnodes
              ├── Status    → component (cached) → vnodes
              ├── Input     → component → vnodes
              ├── Transcript → render-transcript → styled-lines
              ├── Overlay   → styled-lines
              └── Widgets   → vnodes
         ↓
      vdom-layout → styled-lines
         ↓
         ├── cell-buffer (terminal)
         │     ↓ diff → deltas → batched ANSI output
         │
         └── html-render (HTML/CSS)
               ↓ HTML string (web preview)
               └── (future: gui-easy native widgets)
```

Component lifecycle (since v0.62.0):
- Components created once, stored in `tui-ctx-component-registry-box`
- `component-render` uses cache (invalidates on width/state change)
- `component-state-ref/set!` for per-component local state

Key dispatch (since v0.62.1):
- `focused-component-id-box` on `tui-ctx` tracks which component receives keys
- `handle-key` checks focused component first, falls back to keymap
- Opt-in via `wants-focus? #t` + `handle-input-fn`

Zero direct `render-styled-line-to-buffer!` calls in production path.
Transcript retains direct `render-transcript` call for ui-state render cache side effect.

Key optimizations:
- **Batch rendering**: Consecutive same-row/same-SGR cells → single cursor move (37% faster)
- **SGR dedup**: Only emit SGR when attributes change
- **Cell-diff**: XOR row hashing, smart full vs incremental threshold (50%)
- **DECAWM**: Auto-wrap disabled during render to prevent last-column wrap glitch

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

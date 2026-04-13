# Architecture Overview

q is a layered, event-driven AI coding agent written in Racket. Each layer only depends on layers below it.

## Layers (bottom-up)

| Layer | Responsibility | Key Modules |
|-------|---------------|-------------|
| **Foundation** | Types, JSONL, IDs, session storage | `agent/types.rkt`, `util/jsonl.rkt`, `util/ids.rkt`, `runtime/session-store.rkt` |
| **Core** | LLM providers, event bus, agent loop | `llm/provider.rkt`, `agent/event-bus.rkt`, `agent/loop.rkt` |
| **Tools** | Tool contracts, scheduler, built-in tools | `tools/tool.rkt`, `tools/scheduler.rkt`, `tools/builtins/*.rkt` |
| **Runtime** | Session lifecycle, resources, extensions | `runtime/agent-session.rkt`, `runtime/resource-loader.rkt`, `extensions/*.rkt` |
| **Interfaces** | CLI, TUI, RPC, SDK | `interfaces/cli.rkt`, `interfaces/tui.rkt`, `interfaces/sdk.rkt` |
| **Hardening** | Sandbox, limits, recovery | `sandbox/evaluator.rkt`, `sandbox/limits.rkt` |

## Key Design Principles

- **Event-driven**: All state changes flow through the event bus (`agent/event-bus.rkt`). Interfaces subscribe to events and react independently.
- **Provider protocol**: All LLM providers implement the same interface (`llm/provider.rkt`). Adding a new provider requires implementing `complete` and `stream-complete`.
- **Session storage**: Append-only JSONL files. Each turn is a single line. Sessions can be branched, compacted, and replayed.
- **Safe mode**: Dual-layer enforcement — scheduler checks destructive commands, sandbox enforces resource limits.
- **Extension API**: Extensions register hooks (`on-tool-call`, `on-response`, `on-session-start`) via `extensions/api.rkt`. Manifest validation and integrity checks protect against malformed extensions.

## Data Flow

```
User input → Interface (CLI/TUI/RPC)
  → Agent loop (agent/loop.rkt)
    → LLM provider (streaming response)
      → Tool calls (if any)
        → Tool execution (sandbox if safe-mode)
        → Tool result back to agent loop
    → Response rendered to interface
    → Session log appended (JSONL)
    → Events emitted to bus
```

## Metrics (v0.8.1)

> _See `racket scripts/metrics.rkt` for current numbers._

- 124 source modules, 140 test files
- 22,171 source lines, 38,076 test lines
- 3,275 tests, 6,133 assertions (test:source ratio 1.72:1)
- 6/6 lint checks enforced in CI

For the canonical architecture description, see the [repository README](https://github.com/coinerd/q/blob/main/README.md) and [docs/adr/README.md](https://github.com/coinerd/q/blob/main/q/docs/adr/README.md).

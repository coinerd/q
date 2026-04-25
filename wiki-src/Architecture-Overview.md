# Architecture Overview

q is a layered, event-driven AI coding agent written in Racket. Each layer only depends on layers below it.

## Layers (bottom-up)

| Layer | Responsibility | Key Modules |
|-------|---------------|-------------|
| **Foundation** | Types, JSONL, IDs, session storage | `agent/types.rkt`, `util/jsonl.rkt`, `util/ids.rkt`, `runtime/session-store.rkt` |
| **Core** | LLM providers, event bus, agent loop | `llm/provider.rkt`, `agent/event-bus.rkt`, `agent/loop.rkt` |
| **Tools** | Tool contracts, scheduler, built-in tools | `tools/tool.rkt`, `tools/scheduler.rkt`, `tools/builtins/*.rkt` |
| **Runtime** | Session lifecycle, compaction, extensions | `runtime/agent-session.rkt`, `runtime/compactor.rkt`, `extensions/*.rkt` |
| **Interfaces** | CLI, TUI, RPC, SDK | `interfaces/cli.rkt`, `tui/tui-init.rkt`, `interfaces/sdk.rkt` |
| **Hardening** | Sandbox, limits, recovery | `sandbox/evaluator.rkt`, `sandbox/limits.rkt`, `runtime/safe-mode.rkt` |

## Key Design Principles

- **Event-driven**: All state changes flow through the event bus (`agent/event-bus.rkt`). Interfaces subscribe to events and react independently.
- **Provider protocol**: All LLM providers implement the `provider` struct protocol via `make-provider` in `llm/provider.rkt`. Adding a new provider requires implementing the `provider` struct with `request-fn` and (optionally) `stream-fn` dispatch fields.
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

## Metrics (0.19.9)

> _See `racket scripts/metrics.rkt` for current numbers._

- 228 source modules, 349 test files
- 43,544 source lines, 71,526 test lines
- 11,057 assertions (test:source ratio 1.64:1)
- 10/10 lint checks enforced in CI

For the canonical architecture description, see the [repository README](https://github.com/coinerd/q/blob/main/README.md) and [docs/adr/README.md](https://github.com/coinerd/q/blob/main/q/docs/adr/README.md).

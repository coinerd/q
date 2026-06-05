# q Coding Agent — System Overview

> **Version 0.67.3** · Racket · MIT License
> The definitive reference for developers who want to understand, extend, or contribute to q.

---

## 1. What is q

**q** is a local-first coding agent runtime written in [Racket](https://racket-lang.org/). It provides an LLM-powered agent loop that reads/writes files, executes shell commands, searches code, and delegates subtasks — all from the developer's machine, with no cloud dependency.

Key characteristics:

- **Provider-agnostic** — works with OpenAI, Anthropic, Gemini, and local OpenAI-compatible models. Swap providers without changing workflow.
- **5 interfaces** — CLI, TUI (full terminal UI), JSON mode, RPC mode, and an SDK for embedding.
- **14 built-in tools** — file I/O, shell execution, code search, web access, subagent delegation, and more.
- **Event-sourced** — every action produces typed events; sessions are append-only JSONL logs.
- **Extensible** — lifecycle hooks let extensions customize behavior at every phase.

---

## 2. Architecture Layers

```
┌─────────────────────────────────┐
│          Interfaces             │  CLI · TUI · JSON · RPC · SDK
├─────────────────────────────────┤
│       Tools & Extensions        │  14 built-in tools + hook system
├─────────────────────────────────┤
│          Runtime                │  Session · Resources · Compaction
├─────────────────────────────────┤
│        Agent Core               │  Loop · Event Bus · State
├─────────────────────────────────┤
│           LLM                   │  OpenAI · Anthropic · Gemini · Local
└─────────────────────────────────┘
```

### Layer 1 — Interfaces (top)

Entry points that accept user input and render output. The core never depends on any interface.

| Interface | Module | Purpose |
|-----------|--------|---------|
| CLI | `interfaces/cli.rkt` | Single-shot prompts, subcommands (`init`, `doctor`, `sessions`) |
| TUI | `interfaces/tui.rkt`, `tui/` | Full interactive terminal UI with VDOM rendering |
| JSON | `--json` flag | Machine-readable output for scripting |
| RPC | `--rpc` flag | stdin/stdout JSONL protocol for editor/tool integration |
| SDK | `interfaces/sdk-public.rkt` | Stable contracted API for embedding in Racket apps |
| GUI | `interfaces/gui.rkt`, `gui/` | Graphical interface with sidebar, toolbar, themes |

### Layer 2 — Tools & Extensions

- **14 built-in tools** (file ops, execution, web, agent delegation, session recall, skill routing, utility)
- **Extension hook system** at every lifecycle point (session start, tool call, LLM request, etc.)
- Tools live in `tools/`, extensions in `extensions/`

### Layer 3 — Runtime

- **Agent session management** (`runtime/agent-session.rkt`) — create, resume, fork, list, delete
- **Context compaction** — when context approaches token limits, older messages are summarized
- **Resource loading** — system prompts, skill files, project instructions
- **Auth/credentials** — pluggable backends: env vars → OS keychain → file
- **Session storage** — append-only JSONL in `~/.q/sessions/`

### Layer 4 — Agent Core

- **Event bus** (`agent/event-bus.rkt`) — typed pub/sub backbone for inter-layer communication
- **Agent loop** (`agent/loop.rkt`) — FSM-based iteration engine with phase management
- **Queue** (`agent/queue.rkt`) — ordered event/message processing
- **State** (`agent/state.rkt`) — loop state management
- **Typed event structs** — 37+ event types organized in `agent/events/`

### Layer 5 — LLM (bottom)

- **Provider abstraction** (`llm/provider.rkt`) — normalized interface for completion, streaming, tool-use
- **Adapters** — OpenAI (streaming SSE, function calling), Anthropic (streaming SSE, tool-use blocks), Gemini (streaming SSE, function declarations), Local (OpenAI-compatible endpoint)
- **Provider factory** — builds providers from configuration
- Swap providers without changing any workflow code

---

## 3. Request Lifecycle

A single user prompt travels through all layers:

```
User types prompt in TUI/CLI
        │
        ▼
Interface layer creates message, passes to runtime
        │
        ▼
Runtime loads session, assembles context
(system prompt + history + tool definitions)
        │
        ▼
Agent loop calls LLM provider with context
        │
        ▼
LLM responds (possibly with tool calls)
        │
        ▼
Tool scheduler executes tool calls, returns results
        │
        ▼
Agent loop feeds results back to LLM
        │
        ▼
Repeat until no more tool calls (or max turns reached)
        │
        ▼
Final response rendered to user via interface
        │
        ▼
All events appended to JSONL session log
```

---

## 4. Key Data Structures

| Structure | Description | Location |
|-----------|-------------|----------|
| **message** | User/assistant/tool messages with content parts | Core message types |
| **event** | Typed structs for every system occurrence (37+ types) | `agent/events/` |
| **agent-session** | Session handle with ID, history, provider, config | `runtime/` |
| **provider** | Normalized LLM interface with `complete` and `stream-complete` | `llm/` |
| **tool** | Registered function with schema, handler, and safety metadata | `tools/` |

---

## 5. Module Map

```
q/
├── agent/          Core types, event bus, agent loop, queue, state
│   ├── events/     Typed event structs (37+ types)
│   └── loop/       FSM phases, turn model, stream processing
├── benchmarks/     Performance benchmarks and task suites
├── cli/            CLI subcommands: inspect, replay, export
├── extensions/     Extension API, hook system, loader
├── interfaces/     CLI, TUI, JSON mode, RPC mode, SDK
├── llm/            Provider abstraction, OpenAI, Anthropic, Gemini, streaming
├── runtime/        Agent session, compaction, resource loading, auth
├── sandbox/        Subprocess management, execution limits
├── skills/         Skill loading, context files, prompt templates
├── tests/          Full test suite (688 files, 5835+ tests)
├── tools/          Tool registry, scheduler, 14 built-in tools
├── tui/            Terminal UI: rendering, input, state, clipboard
├── util/           JSONL, ANSI, markdown, IDs, cancellation, paths
└── wiring/         Run modes: interactive, JSON-RPC, mode dispatch
```

---

## 6. Entry Points

| Entry Point | Description |
|-------------|-------------|
| `main.rkt` | Thin facade: CLI parsing → runtime construction → mode dispatch |
| `bin/q` | Stale-bytecode-safe wrapper for single-shot/CLI use |
| `bin/q-tui` | Stale-bytecode-safe wrapper for TUI mode |
| `interfaces/sdk-public.rkt` | Stable contracted public API for embedding |

---

## 7. Configuration

| File | Purpose |
|------|---------|
| `~/.q/config.json` | Global: providers, models, session limits |
| `~/.q/credentials.json` | API keys (chmod 600) |
| `<project>/.q/config.json` | Project-local overrides |
| `<project>/.q/instructions.md` | Project-specific system prompt |
| CLI flags | Override config for single runs |

---

## 8. Security Model

- **Credentials**: env vars → OS keychain → file (pluggable backends)
- **Session logs**: append-only JSONL, tamper-detectable but not tamper-proof
- **Shell commands**: `shell-quote` for argument escaping, sandbox with resource limits
- **Safe mode**: per-session, path canonicalization, boundary matching
- **Sandboxing**: subprocess management with timeouts, environment sanitization
- **Credential redaction**: keys are never logged, printed, or included in output

---

## 9. Agent Loop Internals

The agent loop (`agent/loop.rkt`) is an FSM-based iteration engine.

### Phases

The loop proceeds through phases defined in `agent/loop-phases.rkt`:

1. **Initialize** — set up loop state, load session context
2. **Send-to-LLM** — assemble and transmit the prompt
3. **Receive-response** — process streaming or batch response
4. **Dispatch-tools** — execute any tool calls in the response
5. **Collect-results** — gather tool execution results
6. **Check-termination** — decide whether to continue or stop
7. **Finalize** — clean up, emit final events

### State Machine

`agent/loop-fsm.rkt` manages transitions between loop states, ensuring deterministic behavior.

### Turn Model

`agent/turn-model.rkt` and `agent/turn-reducer.rkt` model each turn as a reducible event stream — each turn consumes events and produces state updates.

### Stream Processing

`agent/loop-stream.rkt` and `agent/stream-runner.rkt` handle streaming LLM responses, enabling real-time rendering in the TUI.

### Iteration Limits

- **Soft limits**: exploration hints that suggest the agent should wrap up
- **Hard limits**: max turns (default 10), enforced by the FSM

### Context Pressure

`agent/events/context-pressure-events.rkt` signals when the context window is near capacity, triggering automatic compaction.

---

## 10. Event Bus Architecture

The event bus (`agent/event-bus.rkt`) is the backbone of inter-layer communication.

### Typed Events

37+ event types organized in `agent/events/`:

| Category | Module | Examples |
|----------|--------|---------|
| Base | `base.rkt` | Core event struct |
| Iteration | `iteration-events.rkt` | Loop iteration start/end |
| Tool | `tool-events.rkt` | Tool call, tool result |
| Stream | `stream-events.rkt` | Streaming tokens, completion |
| Message | `message-events.rkt` | User message, assistant message |
| Session | `session-events.rkt` | Session create, resume, close |
| Provider | `provider-events.rkt` | LLM request, response, error |
| Hook | `hook-events.rkt` | Extension hook invocation |
| Turn | `turn-events.rkt` | Turn start, end |
| Context pressure | `context-pressure-events.rkt` | Token limit warnings |

### Design Principles

- **Struct-based**: Events are Racket structs with type predicates (`agent/events/typed-event-predicates.rkt`)
- **Coverage tracking**: `agent/event-struct-coverage.rkt` ensures all event types are tested
- **Serialization**: `agent/event-json.rkt` provides JSON serialization for session logging
- **Subscribers**: Any module can subscribe to event types; the TUI subscribes to render events, the logger subscribes to persist events

---

## 11. Provider Abstraction

The LLM layer (`llm/`) normalizes multi-provider interaction.

### Provider Interface

`llm/provider.rkt` defines a `provider` struct with methods for:

- **Completion** — batch request/response
- **Streaming** — real-time token delivery
- **Tool-use** — function calling with JSON schemas

### Adapters

| Provider | Streaming | Tool Format | Key Validation |
|----------|-----------|-------------|----------------|
| OpenAI | SSE | Function calling | `sk-` prefix |
| Anthropic | SSE | Tool-use blocks | `sk-ant-` prefix |
| Gemini | SSE | Function declarations | API key format |
| Local | SSE (OpenAI-compatible) | Function calling | Varies |

### Provider Factory

`runtime/provider-factory.rkt` builds providers from configuration, handling adapter selection, API key retrieval, and error handling with provider-specific retry hints.

---

## 12. Tool System

The tool layer (`tools/`) manages 14 built-in tools.

### Registry

`tools/registry-defaults.rkt` registers all built-in tools at startup. Each tool has a JSON schema for parameters, enabling LLM tool-use.

### Built-in Tools

| Category | Tools |
|----------|-------|
| File ops | `read`, `write`, `edit`, `delete-lines`, `find`, `grep`, `ls` |
| Execution | `bash` (sandboxed subprocess) |
| Web | `firecrawl` (web search/scrape via Firecrawl API) |
| Agent | `spawn-subagent`, `spawn-subagents` (delegated tasks) |
| Session | `session-recall` (retrieve earlier session entries) |
| Routing | `skill-route` (skill discovery and loading) |
| Utility | `date` |

### Safety Controls

- Tools can be disabled (`--no-tools`) or selectively enabled (`--tool <name>`)
- The `bash` tool runs in a sandbox with resource limits and environment sanitization
- Extension tools can be registered via the `register-tools` hook

---

## 13. Extension System

Extensions (`extensions/`) provide lifecycle hooks at every phase of agent operation.

### Hook Model (ADR 0006)

Hooks fire at: session start, session end, tool call (before/after), LLM request (before/after), context assembly, error handling, and more.

### Key Components

- **Loader**: Discovers and loads extensions from configured paths with LRU caching
- **Extension context**: Extensions receive a scoped context, not raw system access
- **Hook types**: Defined in dedicated type modules for contract enforcement

---

## 14. Session & Storage

Session management lives in `runtime/`.

### JSONL Format

Sessions are stored as append-only JSONL files in `~/.q/sessions/`. Every event is appended, never rewritten.

### Operations

| Operation | Description |
|-----------|-------------|
| Create | New session with fresh ID |
| Resume | `--session <id>` loads existing session |
| Fork | `fork-session` creates a branch to explore alternatives |
| List | Show all sessions with metadata |
| Delete | Remove session and log file |

### Compaction

When context approaches token limits, older messages are automatically summarized to free space while preserving key information.

### Log Integrity

- Append-only by convention
- `repair-session-log!` for crash recovery
- Metrics tracked by `runtime/metrics.rkt`

---

## 15. Interface Layer

The interface layer (`interfaces/`) provides 5 ways to interact with q.

### CLI (`interfaces/cli.rkt`)

- Single-shot prompts: `q "explain this code"`
- Subcommands: `q init`, `q doctor`, `q sessions list/info/delete`
- CLI flags override config for a single run (model, session, max-turns, etc.)
- CLI arg parsing in `cli/args.rkt`

### TUI (`interfaces/tui.rkt`, `tui/`)

- Full interactive terminal UI with native Racket rendering (no external deps)
- VDOM-based component system with per-zone caching
- Features: streaming display, tool result rendering, session tree, command palette
- Input editor with bracketed paste, IME support, configurable keybindings
- Theme system, markdown rendering, mouse support (SGR mode 1006)
- Kitty keyboard protocol, grapheme-aware cursor
- Slash commands: `/compact`, `/done`, `/wave-done`, `/milestone`, `/issue`, `/pr`

### JSON Mode (`--json`)

- Machine-readable output for scripting and automation
- Structured event stream

### RPC Mode (`--rpc`)

- stdin/stdout JSONL protocol for editor/tool integration
- Crypto-random handshake tokens for security
- Per-method rate limiting

### SDK (`interfaces/sdk-public.rkt`)

- Stable, contracted public API for embedding q in other Racket applications
- Functions: `make-agent-session`, `run-prompt!`, `fork-session`, `close-session!`
- Backward-compatible re-exports in `main.rkt` (deprecated, use SDK directly)

### GUI (`interfaces/gui.rkt`, `gui/`)

- Graphical interface with sidebar, toolbar, themes
- Extension integration via dialogs
- Core views: transcript, input, status

---

## 16. Test Infrastructure

The test suite is comprehensive:

| Metric | Value |
|--------|-------|
| Test files | 688 |
| Test lines | 114,770 |
| Assertions | 18,113 |
| Tests passing | 5,835+ |

### Test Categories

- **Unit tests** per module (`tests/test-*.rkt`)
- **Integration tests** (tool→API serialization pipeline)
- **Workflow tests** (33 tests across 11 files covering CLI, tool-use, session lifecycle, safety)
- **TUI parity tests** (native vs legacy render paths)
- **Provider tests** (OpenAI, Anthropic, Gemini multi-turn tool use)

### Running Tests

```bash
raco test tests/                    # Full suite
raco test tests/test-loop.rkt       # Specific module
```

---

## 17. Architecture Decision Records

18 ADRs document key design decisions:

| ADR | Title | Key Decision |
|-----|-------|-------------|
| 0001 | Small Trusted Core | Minimal core with contracts enforcing boundaries |
| 0002 | Append-Only JSONL Session Log | Every action recorded, never rewritten |
| 0003 | Event Bus Architecture | Deterministic events between layers |
| 0004 | Provider Abstraction | Normalized interface for multi-provider support |
| 0005 | Interface Separation | Core never depends on UI |
| 0006 | Extension Hook Model | Lifecycle hooks for custom behavior |
| 0007 | Sandboxing Boundary | Subprocess isolation with resource limits |
| 0008 | Safe Mode Enforcement | Per-session path and command safety |
| 0009 | Credential Redaction | Keys never logged, printed, or in output |
| 0010 | Streaming Port Lifecycle | Managed streaming ports with cleanup |
| 0011 | GSD State Machine Rewrite | FSM-based agent loop |
| 0012 | Context Manager Strategy | Token-aware context assembly |
| 0013 | Typed Racket Optional Args | Gradual typing strategy |
| 0014 | Typed Racket Migration | Migration path from untyped to typed |
| 0015 | TR Boundary Callback | Callback patterns at Typed/Untyped boundary |
| 0016 | Native TUI Architecture | Native Racket rendering, no external deps |
| 0017 | GUI Architecture | Graphical interface design |
| 0018 | GUI Extension Integration | Extension hooks in GUI context |

---

## 18. Development Workflow

```bash
# Setup
git clone https://github.com/coinerd/q.git && cd q
raco pkg install --auto

# Run
bin/q-tui                           # Interactive TUI
bin/q "explain what this codebase does"  # Single-shot

# Test
raco test tests/                    # Full suite
raco test tests/test-loop.rkt       # Specific module

# Diagnostics
bin/q --version                     # Check version
bin/q doctor                        # Setup and provider diagnostics
bin/q init                          # Guided setup wizard
```

---

## 19. Key Metrics

| Metric | Value |
|--------|-------|
| Version | 0.67.3 |
| Language | Racket |
| License | MIT |
| Source modules | 495 |
| Source lines | 76,308 |
| Test files | 688 |
| Test lines | 114,770 |
| Test assertions | 18,113 |
| Tests passing | 5,835+ |
| Built-in tools | 14 |
| LLM providers | 4 (OpenAI, Anthropic, Gemini, Local) |
| Interfaces | 5 (CLI, TUI, JSON, RPC, SDK) |
| ADRs | 18 |
| Event types | 37+ |

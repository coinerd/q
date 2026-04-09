# q

> A local-first, extensible coding agent runtime written in Racket

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/coinerd/q)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Language](https://img.shields.io/badge/language-Racket-red.svg)](https://racket-lang.org)

---

## What is q?

**q** is a coding agent that runs entirely on your machine. It reads, writes, edits files and executes shell commands on your behalf — driven by an LLM-powered agent loop you can interact with through a terminal UI, CLI, JSON protocol, or embedded SDK.

The trusted core is deliberately small. Strong contracts enforce boundaries between layers, and an append-only JSONL session log records every action for full auditability. Optional behaviors live in extensions, not the core.

q is provider-agnostic: it ships with adapters for OpenAI and Anthropic, and supports local models through a normalized provider interface. Swap providers without changing your workflow or configuration.

## Key Advantages

- 🔒 **Local-first** — no cloud dependency, your code stays on your machine
- 📝 **Auditable** — append-only session log, every action recorded in JSONL
- 🔌 **Provider-agnostic** — swap LLM providers without changing your workflow
- 🧩 **Extensible** — hook system for custom behavior at every lifecycle point
- 🖥️ **Multiple interfaces** — CLI, TUI (terminal UI), JSON mode, RPC, SDK
- 🔧 **9 built-in tools** — read, write, edit, bash, grep, find, ls, date, firecrawl
- 🌿 **Session branching** — fork conversations, explore alternatives, merge back
- 🏗️ **5-layer architecture** — clean separation from LLM to interfaces

## Architecture

```
┌─────────────────────────────────┐
│          Interfaces             │  CLI · TUI · JSON · RPC · SDK
├─────────────────────────────────┤
│       Tools & Extensions        │  9 built-in tools + hook system
├─────────────────────────────────┤
│          Runtime                │  Session · Resources · Compaction
├─────────────────────────────────┤
│        Agent Core               │  Loop · Event Bus · State
├─────────────────────────────────┤
│           LLM                   │  OpenAI · Anthropic · Local
└─────────────────────────────────┘
```

The core agent loop never depends on TUI, CLI, or any interface-specific concern. Each layer communicates through deterministic events and strict contracts.

## Quick Start

### Install

```bash
# 1. Install Racket 8.10+ from https://racket-lang.org

# 2. Install TUI dependencies (optional, for terminal UI mode)
raco pkg install charterm raart

# 3. Clone and install q
git clone https://github.com/coinerd/q.git
cd q
raco pkg install --link .
```

### Run

```bash
# Terminal UI (interactive)
raco q --tui

# Single-shot prompt
raco q "explain what this codebase does"

# Resume a session
raco q --session <session-id>

# JSON mode (machine-readable output)
raco q --json
```

### Verify

```bash
raco q --version        # q version 0.1.0
raco test tests/        # run the full test suite
```

## Configuration

| Location | Purpose |
|----------|---------|
| `~/.q/config.json` | Global settings: default provider, models, session limits |
| `.q/` | Project-local: instructions, extensions, overrides |
| `~/.q/credentials.json` | API keys (chmod 600) — or use `OPENAI_API_KEY` / `ANTHROPIC_API_KEY` env vars |

Minimal config to get started:

```json
{
  "default-provider": "openai",
  "default-model": "gpt-4o"
}
```

## Module Structure

```
q/
├── agent/          Core types, event bus, agent loop, queue, state
├── extensions/     Extension API, hook system, loader
├── interfaces/     CLI, TUI, JSON mode, RPC mode, SDK
├── llm/            Provider abstraction, OpenAI, Anthropic, streaming
├── runtime/        Agent session, compaction, resource loading, auth
├── sandbox/        Subprocess management, execution limits
├── skills/         Skill loading, context files, prompt templates
├── tests/          Full test suite (~2189 tests, 60 files)
├── tools/          Tool registry, scheduler, 9 built-in tools
├── tui/            Terminal UI: rendering, input, state, clipboard
└── util/           JSONL, ANSI, markdown, IDs, cancellation, paths
```

## Test Suite

| Metric | Value |
|--------|-------|
| Test files | 60 |
| Source modules | 66 |
| Source lines | ~13,700 |
| Test lines | ~21,400 |
| Total test assertions | ~3,700 |
| `raco test` results | 2,189 tests passing |

```bash
# Run all tests
raco test tests/

# Run specific module tests
raco test tests/test-loop.rkt
raco test tests/tui/
```

## License

[MIT](LICENSE)

## Status

**v0.1.0** — Active development. All core features are implemented:

- ✅ Canonical types and utilities
- ✅ JSONL session storage with tree indexing and forking
- ✅ Provider abstraction and core agent loop
- ✅ Tool system with 9 built-in tools
- ✅ Runtime orchestration with compaction and extensions
- ✅ 5 interfaces (CLI, TUI, JSON, RPC, SDK)
- ✅ Sandboxing and hardening
- ✅ Full test coverage (~2189 tests)

# q

> A local-first, extensible coding agent runtime written in Racket

[![CI](https://github.com/coinerd/q/actions/workflows/ci.yml/badge.svg)](https://github.com/coinerd/q/actions/workflows/ci.yml)
[![Version](https://img.shields.io/badge/version-0.7.5-blue.svg)](https://github.com/coinerd/q)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)
[![Language](https://img.shields.io/badge/language-Racket-red.svg)](https://racket-lang.org)

---

## Quick Install

```bash
# One-command install (macOS / Linux)
curl -fsSL https://raw.githubusercontent.com/coinerd/q/main/scripts/install.sh | bash

# Or manual install
git clone https://github.com/coinerd/q.git
cd q
raco pkg install --auto
racket main.rkt --version

# Or install from release tarball
curl -fsSL https://github.com/coinerd/q/releases/latest/download/$(curl -sL https://api.github.com/repos/coinerd/q/releases/latest | grep -oP '"tag_name": "v\K[^"]+' | xargs -I{} echo q-{}.tar.gz) | tar xz
cd q-* && raco pkg install --auto
```

Add q to your shell:

```bash
# Option A: add to PATH
export PATH="$HOME/.q:$PATH"

# Option B: alias
echo "alias q='racket $HOME/.q/main.rkt'" >> ~/.bashrc
```

📖 See the full [Install Guide](docs/install.md) for prerequisites, verification, and troubleshooting.

> **Note:** q is not yet on a package registry. The install script handles everything in one command. Published packages are planned — see [packaging roadmap](docs/why-q.md#packaging-roadmap).

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

# 2. Clone q
git clone https://github.com/coinerd/q.git
cd q

# 3. Install dependencies
raco pkg install --auto

# 4. (Optional) Enhanced TUI rendering — works without these via built-in fallbacks
raco pkg install tui-term tui-ubuf
```

See [docs/install.md](docs/install.md) for the one-command installer and detailed instructions.

### First-Time Setup

q needs an LLM provider before it can respond to prompts. Create `~/.q/config.json`:

**Option A — OpenAI (cloud):**
```json
{
  "default-provider": "openai",
  "default-model": "gpt-5.4",
  "providers": {
    "openai": {
      "base-url": "https://api.openai.com/v1",
      "api-key-env": "OPENAI_API_KEY"
    }
  }
}
```
Then set your API key: `export OPENAI_API_KEY=sk-...`

**Option B — Anthropic (cloud):**
```json
{
  "default-provider": "anthropic",
  "default-model": "claude-sonnet-4-6",
  "providers": {
    "anthropic": {
      "base-url": "https://api.anthropic.com/v1",
      "api-key-env": "ANTHROPIC_API_KEY"
    }
  }
}
```
Then set your API key: `export ANTHROPIC_API_KEY=sk-ant-...`

**Option C — Local LLM (no API key needed):**
```json
{
  "default-provider": "local",
  "default-model": "llama3",
  "providers": {
    "local": {
      "base-url": "http://localhost:8080/v1"
    }
  }
}
```
Start your local server (e.g. `llama-server`), then run q.

> **No config?** q starts with a mock provider that returns canned responses — useful for exploring the UI.

**Alternative: store API keys in a file** instead of env vars — see `~/.q/credentials.json`:
```json
{
  "providers": {
    "openai": { "api-key": "sk-..." },
    "anthropic": { "api-key": "sk-ant-..." }
  }
}
```

### Run

```bash
# Terminal UI (interactive)
racket main.rkt --tui

# Single-shot prompt
racket main.rkt "explain what this codebase does"

# Resume a session
racket main.rkt --session <session-id>

# JSON mode (machine-readable output)
racket main.rkt --json

# Override model for this run
racket main.rkt --model gpt-5.4 "write a test"
```

### Verify

```bash
racket main.rkt --version   # q version 0.7.5
raco test tests/            # run the full test suite
```

## Configuration

| Location | Purpose |
|----------|----------|
| `~/.q/config.json` | Global settings: providers, models, session limits, tools |
| `~/.q/credentials.json` | API keys for providers (chmod 600) |
| `<project>/.q/config.json` | Project-local overrides |
| `<project>/.q/instructions.md` | Project-specific system prompt |

**Environment variables** (alternative to credentials.json):

| Variable | Provider |
|----------|----------|
| `OPENAI_API_KEY` | OpenAI (key must start with `sk-`) |
| `ANTHROPIC_API_KEY` | Anthropic (key must start with `sk-ant-`) |

**CLI flags** (override config for a single run):

| Flag | Purpose |
|------|----------|
| `--model <name>` | Override model (e.g. `gpt-5.4`, `claude-sonnet-4-6`) |
| `--session <id>` | Resume existing session |
| `--session-dir <path>` | Override session storage directory |
| `--config <path>` | Explicit config file path |
| `--max-turns <n>` | Max agent loop iterations (default: 10) |
| `--verbose` | Enable verbose output |
| `--no-tools` | Disable all tool use |
| `--tool <name>` | Enable specific tool(s) only (repeatable) |
| `--project-dir <path>` | Override project directory |

## Module Structure

```
q/
├── agent/          Core types, event bus, agent loop, queue, state
├── benchmarks/     Performance benchmarks and task suites
├── cli/            CLI subcommands: inspect, replay, export
├── extensions/     Extension API, hook system, loader
├── interfaces/     CLI, TUI, JSON mode, RPC mode, SDK
├── llm/            Provider abstraction, OpenAI, Anthropic, streaming
├── runtime/        Agent session, compaction, resource loading, auth
├── sandbox/        Subprocess management, execution limits
├── skills/         Skill loading, context files, prompt templates
├── tests/          Full test suite (86 files)
├── tools/          Tool registry, scheduler, 9 built-in tools
├── tui/            Terminal UI: rendering, input, state, clipboard
└── util/           JSONL, ANSI, markdown, IDs, cancellation, paths
```

## Test Suite

| Metric | Value |
|--------|-------|
| Test files | 120 |
| Source modules | 98 |
| Source lines | 21135 |
| Test lines | 35604 |
| Test assertions | 5765 |
| `raco test` results | 3,098 tests passing |

> _These metrics are generated by `racket scripts/metrics.rkt`. Do not edit manually._

```bash
# Run all tests
raco test tests/

# Run specific module tests
raco test tests/test-loop.rkt
raco test tests/tui/
```

## License

[MIT](LICENSE)

## Documentation

| Doc | Description |
|-----|-------------|
| [Install Guide](docs/install.md) | Full setup instructions |
| [Tutorials](docs/tutorials/) | Team setup and builder cookbook |
| [Demos](docs/demos/) | Example session transcripts |
| [Architecture](docs/architecture/) | System design deep dives |

## Status

**v0.7.4** — Builder Cookbook and Team Adoption. Team setup guide, builder tutorials for custom tools/providers/extensions:

**v0.6.8** — Structural Hardening. Thread safety (semaphores), contracts on critical entry points, per-session safe-mode, safe-mode path checks for grep/find/ls, extension loader logging, dead code cleanup (session-tree, CSI stubs, resource-loader, duplicate try-read-file), guarded tool arguments:

**v0.6.7** — Integration Test Infrastructure. E2E tool→API serialization pipeline tests (OpenAI/Anthropic/Gemini), run-agent-turn direct test coverage (6 scenarios), CLI interactive mode tests (34 cases), hot-path integration tests for all dead-code/misroute gaps:

**v0.6.6** — Provider Correctness. Anthropic/Gemini multi-turn tool use message translation, incremental SSE streaming, O(n²)→O(n) stream chunk accumulation, Gemini tool call unique IDs:

**v0.6.5** — Critical Bug Fixes. Firecrawl poll deadline fix, tool registration nesting fix, Anthropic provider wiring, ensure-hash-args error propagation, TUI fork/compact event subscribers, hook return type validation, parallel execution deadlock fix:

**v0.6.4** — Developer Tooling & Protocol Consistency. Racket-aware line wrapper, protocol checker, import conflict detector, pre-commit hook, unified hook-dispatcher protocol, SGR tests:

**v0.6.3** — Architecture & Test Reliability. Decoupled agent/types ↔ tools/tool, extracted CLI builders from main.rkt, session log backup, RPC handshake tokens, improved test assertions, fixed flaky tests, format lint CI:

**v0.6.2** — Hardening & Quality. Test coverage expansion, sandbox hardening, SSRF protection, response size limits, safe-mode locking, doc drift fixes, code quality:

**Previous** — Security Hardening. Wire safe-mode/quarantine into enforcement, path validation, API key protection, env sanitization, test isolation:

- ✅ Canonical types and utilities
- ✅ JSONL session storage with tree indexing and forking
- ✅ Provider abstraction and core agent loop
- ✅ Tool system with 9 built-in tools
- ✅ Runtime orchestration with compaction and extensions
- ✅ 5 interfaces (CLI, TUI, JSON, RPC, SDK)
- ✅ Sandboxing and hardening
- ✅ Full test coverage (2,466 tests, 0 failures)

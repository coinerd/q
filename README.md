# q

> A local-first, extensible coding agent runtime written in Racket

[![CI](https://github.com/coinerd/q/actions/workflows/ci.yml/badge.svg)](https://github.com/coinerd/q/actions/workflows/ci.yml)
[![Version](https://img.shields.io/badge/version-0.97.2-blue.svg)](https://github.com/coinerd/q)
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

q is provider-agnostic: it ships with adapters for **OpenAI**, **Anthropic**, and **Google Gemini**, and supports local or OpenAI-compatible models through a normalized provider interface. Swap providers without changing your workflow or configuration. Each adapter handles streaming, tool-use, and multi-turn conversation with provider-specific validation and error handling.

## Key Advantages

- 🔒 **Local-first** — no cloud dependency, your code stays on your machine
- 📝 **Auditable** — append-only session log, every action recorded in JSONL
- 🔌 **Provider-agnostic** — swap LLM providers without changing your workflow
- 🧩 **Extensible** — hook system for custom behavior at every lifecycle point
- 🖥️ **Multiple interfaces** — CLI, TUI (terminal UI), JSON mode, RPC, SDK
- 🔧 **27 built-in tools** (17 core + 10 browser) — read, write, edit, delete-lines, bash, grep, find, ls, date, firecrawl, spawn-subagent, spawn-subagents, session-recall, skill-router, record-conclusion, save-conclusion, set-task-state, browser_open, browser_close, browser_observe, browser_click, browser_type, browser_press, browser_extract, browser_screenshot, browser_scroll, browser_check_local_app
- 🌿 **Session branching** — fork conversations, explore alternatives, merge back
- 🏗️ **5-layer architecture** — clean separation from LLM to interfaces

## Architecture

```
┌─────────────────────────────────┐
│          Interfaces             │  CLI · TUI · JSON · RPC · SDK
├─────────────────────────────────┤
│       Tools & Extensions        │  27 built-in tools + hook system
├─────────────────────────────────┤
│          Runtime                │  Session · Resources · Compaction
├─────────────────────────────────┤
│        Agent Core               │  Loop · Event Bus · State
├─────────────────────────────────┤
│           LLM                   │  OpenAI · Anthropic · Gemini · Local
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

# 4. TUI uses native Racket (no external deps needed)
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

**Option C — Google Gemini (cloud):**
```json
{
  "default-provider": "gemini",
  "default-model": "gemini-2.5-pro",
  "providers": {
    "gemini": {
      "base-url": "https://generativelanguage.googleapis.com/v1beta",
      "api-key-env": "GEMINI_API_KEY"
    }
  }
}
```
Then set your API key: `export GEMINI_API_KEY=AIza...`

**Option D — Local LLM (no API key needed):**
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
    "anthropic": { "api-key": "sk-ant-..." },
    "gemini": { "api-key": "AIza..." }
  }
}
```

### Run

```bash
# Terminal UI (interactive) — using stale-bytecode-safe wrapper
bin/q-tui
# ...or directly:
racket main.rkt --tui

# Single-shot prompt
bin/q "explain what this codebase does"

# Resume a session
bin/q --session <session-id>

# JSON mode (machine-readable output)
bin/q --json

# Override model for this run
bin/q --model gpt-5.4 "write a test"
```

> **Tip:** The `bin/q` and `bin/q-tui` wrappers use `racket --make` for automatic
> stale-bytecode recompilation after `git pull`. Use them instead of bare
> `racket main.rkt` to avoid running outdated `.zo` files.

### Verify

```bash
bin/q --version            # q version 0.93.1
raco test tests/           # run the full test suite
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
| `GEMINI_API_KEY` | Google Gemini (key must start with `AIza`) |

**CLI flags** (override config for a single run):

| Flag | Purpose |
|------|----------|
| `--model <name>` | Override model (e.g. `gpt-5.4`, `claude-sonnet-4-6`, `gemini-2.5-pro`) |
| `--session <id>` | Resume existing session |
| `--session-dir <path>` | Override session storage directory |
| `--config <path>` | Explicit config file path |
| `--max-turns <n>` | Max agent loop iterations (default: 10) |
| `--verbose` | Enable verbose output |
| `--no-tools` | Disable all tool use |
| `--tool <name>` | Enable specific tool(s) only (repeatable) |
| `--print`, `-p` | Print plain-text assistant response to stdout (no TUI) |
| `--rpc` | RPC mode (stdin/stdout JSONL protocol) |
| `--project-dir <path>` | Override project directory |

**Subcommands:**

| Command | Purpose |
|---------|----------|
| `q init` | Guided setup wizard (config, provider, API key) |
| `q doctor` | Run setup and provider diagnostics |
| `q sessions list` | List sessions |
| `q sessions info <id>` | Show session details |
| `q sessions delete <id>` | Delete a session |
| `q sessions trace <id>` | Show session event trace |

**TUI Slash Commands** (inside the TUI, type `/` followed by the command):

| Command | Purpose |
|---------|----------|
| `/goal <description>` | Start autonomous goal loop — agent works toward a goal for up to 8 turns |
| `/goal clear` | Cancel active goal and clear display |
| `/goal --evaluator agent <desc>` | Run goal with agent-based self-evaluation |
| `/compact` | Trigger context compaction |
| `/model [name]` | Show or switch active model |
| `/activate <extension>` | Activate a loaded extension |
| `/help` | Show available commands |

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
├── tests/          Full test suite (835 files)
├── tools/          Tool registry, scheduler, 27 built-in tools
├── tui/            Terminal UI: rendering, input, state, clipboard
├── util/           JSONL, ANSI, markdown, IDs, cancellation, paths
├── browser/        Browser types, adapter, policy, service, audit
├── sidecars/       Playwright sidecar integration
└── wiring/         Run modes: interactive, JSON-RPC, mode dispatch
```

## Test Suite

| Metric | Value |
|--------|-------|
| Test files | 835 |
| Source modules | 577 |
| Source lines | 88190 |
| Test lines | 132224 |
| Test assertions | 21046 |
| Tests passing | 5835+ | `racket scripts/run-tests.rkt` results |

> _These metrics are generated by `racket scripts/metrics.rkt`. Do not edit manually._

```bash
# Run all tests
raco test tests/

# Run specific module tests
raco test tests/test-loop.rkt
raco test tests/tui/
```

## Security

📖 Full security model: [Security & Trust Model](docs/security-trust-model.md)

### Credential Storage

API keys are resolved through a **pluggable backend system** with priority:
env vars → OS keychain → file. See [Credential Management](docs/getting-started/credentials.md) for full details.

The default file backend stores keys in `~/.q/credentials.json` with owner-only permissions (`0600` / `chmod 600`). Keys are never logged, printed, or included in session output.

**For production or CI**, prefer environment variables (`OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`) or the **OS keychain backend** (`secret-tool` on Linux, macOS Keychain). Both avoid writing secrets to disk.

### Session Log Integrity

Session logs (`~/.q/sessions/`) are **append-only by convention** — the agent loop only appends events, never rewrites history. However, this is not cryptographically enforced. The `repair-session-log!` function can rewrite logs for crash recovery, which means logs are tamper-detectable but not tamper-proof.

For audit compliance, consider:
- Per-line hash chaining (planned)
- OS-level append-only file flags (`chattr +a` on Linux)
- External log aggregation with write-once storage

### Shell Command Safety

When q executes shell commands on behalf of an LLM, arguments are quoted via `shell-quote` to prevent injection. This function is designed for **LLM-generated arguments**, not adversarial input. Sandboxing (resource limits, timeouts, environment sanitization) provides defense-in-depth but does not replace proper input validation for untrusted sources.

---

## License

[MIT](LICENSE)

> **Contributors:** Clone the repo for the full contributor guidelines (`CONTRIBUTING.md`).

## Documentation

| Doc | Description |
|-----|-------------|
| [Install Guide](docs/install.md) | Full setup instructions |
| [Tutorials](docs/tutorials/) | Team setup and builder cookbook |
| [Demos](docs/demos/) | Example session transcripts |
| [Architecture](docs/adr/) | Architecture decision records |
| [Tooling](docs/tooling.md) | Audit scripts, linters, and developer tools |

<!-- DO NOT EDIT: Status section is historical. Use sync-version.rkt for version bumps. -->
## Status

**v0.82.5** — Core Loop Tests: Pure Functions (T-1a)

**v0.78.6** — Core Loop Tests: Pure Functions (T-1a)

**v0.78.0** — Core Loop Tests: Pure Functions (T-1a)

**v0.77.10** — Core Loop Tests: Pure Functions (T-1a)

**v0.77.0** — Core Loop Tests: Pure Functions (T-1a)

**v0.76.1** — Core Loop Tests: Pure Functions (T-1a)

**v0.76.0** — Core Loop Tests: Pure Functions (T-1a)

**v0.75.7** — Core Loop Tests: Pure Functions (T-1a)

**v0.74.8** — Core Loop Tests: Pure Functions (T-1a)

**v0.74.7** — Core Loop Tests: Pure Functions (T-1a)


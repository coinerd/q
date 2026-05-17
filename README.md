# q

> A local-first, extensible coding agent runtime written in Racket

[![CI](https://github.com/coinerd/q/actions/workflows/ci.yml/badge.svg)](https://github.com/coinerd/q/actions/workflows/ci.yml)
[![Version](https://img.shields.io/badge/version-0.45.22-blue.svg)](https://github.com/coinerd/q)
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
- 🔧 **14 built-in tools** — read, write, edit, delete-lines, bash, grep, find, ls, date, firecrawl, spawn-subagent, spawn-subagents, session-recall, skill-router
- 🌿 **Session branching** — fork conversations, explore alternatives, merge back
- 🏗️ **5-layer architecture** — clean separation from LLM to interfaces

## Architecture

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
bin/q --version            # q version 0.45.22
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
├── tests/          Full test suite (587 files)
├── tools/          Tool registry, scheduler, 14 built-in tools
├── tui/            Terminal UI: rendering, input, state, clipboard
├── util/           JSONL, ANSI, markdown, IDs, cancellation, paths
└── wiring/         Run modes: interactive, JSON-RPC, mode dispatch
```

## Test Suite

| Metric | Value |
|--------|-------|
| Test files | 587 |
| Source modules | 429 |
| Source lines | 66087 |
| Test lines | 103895 |
| Test assertions | 16195 |
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











































































**v0.45.22** — Fixed

**v0.45.22** — Added

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Added

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Changed

**v0.45.22** — Changed

**v0.45.22** — Changed

**v0.45.22** — Changed

**v0.45.22** — Added

**v0.45.22** — Added

**v0.45.22** — Added

**v0.45.22** — Changed

**v0.45.22** — Fixed

**v0.45.22** — Added

**v0.45.22** — Added

**v0.45.22** — Fixed

**v0.45.22** — Changed

**v0.45.22** — Added

**v0.45.22** — Changed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Fixed

**v0.45.22** — Goal: Audit Remediation — fix 8 compile regressions, struct-safety, test coverage

**v0.45.22** — Goal: Runtime Loop Config Struct & Event DSL Polish (Milestone 8 of v0.38.x)

**v0.45.22** — Goal: TUI State Decomposition Part 2 (Milestone 7 of v0.38.x)

**v0.45.22** — Goal: Extension & GSD Cleanup (Milestone 5 of v0.38.x)

**v0.45.22** — Goal: Audit Remediation — Comment Cleanup + Import + Test Optimization

**v0.45.22** — Goal: Audit Remediation — Test Gaps + Dead Code + Contract Tightening

**v0.45.22** — Goal: Audit Remediation — Contract Fixes + Dead Code + Test Gaps

**v0.45.22** — Goal: Error Classification & Exception Hygiene (M-11, L-07, L-09)

**v0.45.22** — Goal: Module Boundary Hardening (M-06, M-07, M-14, L-02)

**v0.45.22** — Goal: TUI State Decomposition (H-06, M-08, M-09, L-01, L-05)

**v0.45.22** — Goal: Tool System Data Representation (M-03, M-04, M-13)

**v0.45.22** — Goal: Context Assembly Purity (H-04, M-02, M-05, L-03, L-04)

**v0.45.22** — Goal: GSD Concurrency & State Safety (H-03, H-05)

**v0.45.22** — Goal: Security & Runtime Contracts (H-02, L-08, L-10)

**v0.45.22** — Goal: Event Serialization Auto-Generation (H-01, M-10, M-12, L-06)

**v0.45.22** — Goal: Hotfix — Deadlock + Arity Crash + Dead Code Cleanup

**v0.45.22** — Goal: Deep Audit Remediation (N-01–N-13, I-07)

**v0.45.22** — Goal: Extension & GSD Cleanup (v0.45.22 milestone — FINAL)

**v0.45.22** — Goal: Agent Session Invariants (v0.45.22 milestone)

**v0.45.22** — Goal: Tool System Contracts & DSL (v0.45.22 milestone)

**v0.45.22** — Goal: TUI Dispatcher Refactoring (v0.45.22 milestone)

**v0.45.22** — Goal: Iteration Loop Decomposition (v0.45.22 milestone)

**v0.45.22** — Goal: Runtime Config Struct & Scheduler Types (v0.45.22 milestone)

**v0.45.22** — Goal: Global State Isolation (v0.45.22 milestone)

**v0.45.22** — Goal: Event System Integrity (v0.45.22 milestone)

**v0.45.22** — Goal: Documentation integrity fixes (v0.45.22 findings)

**v0.45.22** — Deep Audit Remediation Round 2

**v0.45.22** — Deep Audit Remediation

**v0.45.22** — Architecture Decomposition (A-01, A-02)

**v0.45.22** — Docs/Lint Remediation + Contract Quick Wins + Test Hygiene

**v0.45.22** — Session Boundary Encapsulation (RA-05)

**v0.45.22** — Deep Audit Remediation

**v0.45.22** — Deep Audit Remediation

**v0.45.22** — Hotfix — Critical Audit Findings from v0.45.22

**v0.45.22** — Audit Remediation (v0.45.22)

**v0.45.22** — Test Regression Fix (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction Roadmap Remediation (v0.45.22-W0)

**v0.45.22** — Architecture Abstraction: HOF Combinators for Hook System

**v0.45.22** — Test Regression Fix + Contract Repair + Event Migration

**v0.45.22** — Audit Remediation + Test Coverage + Cleanup

**v0.45.22** — Audit Remediation + Struct Refactor + Completion

**v0.45.22** — Warning Remediation + Event Wiring + Fan-in Reduction

**v0.45.22** — Audit Remediation + Deferred Event Adoption

**v0.45.22** — Architecture Remediation + Event Adoption Phase 1

**v0.45.22** — Test Health Restoration

**v0.45.22** — Audit Remediation (v0.45.22–v0.45.22 Findings)

**v0.45.22** — Architecture Re-Audit (Verification Gate)

**v0.45.22** — Contract Tightening + Cleanup

**v0.45.22** — Production Wiring

**v0.45.22** — Test Suite Restoration

**v0.45.22** — Tool Middleware HOF + Facade Curation

**v0.45.22** — Stream Purity + DI Cleanup

**v0.45.22** — Runtime State Encapsulation

**v0.45.22** — Typed Racket at Provider Boundary

**v0.45.22** — Event Struct Adoption

**v0.45.22** — Match Dispatch + Pure Decisions

**v0.45.22** — Contract Boundaries

**v0.45.22** — Data Corrections + CHANGELOG Date Validator

**v0.45.22** — Audit Remediation — Permanent Tooling Automation

**v0.45.22** — Status Restoration + Tooling Hardening

<!-- DO NOT EDIT: Status section managed by sync-readme-status.rkt -->

**v0.45.22** — GSD Pin Wiring + Tests. Wire gsd-pin flag into GSD wave-done and done handlers. 12 isolated unit tests for gsd-progress-message?. Expanded regex fallback for GSD patterns.

**v0.45.22** — Summary Injection Fix. Inject computed summary into LLM context when messages are excluded. Fix hash-ref on context-summary struct.


**v0.45.22** — Audit Remediation — Docs Integrity + Tooling Hardening

**v0.45.22** — Audit Remediation (7 Warnings from v0.45.22). GSD role guard prevents false-positive pinning from user messages. `make-text-part` canonicalized in tool summarization. `check-mid-turn-budget!` split into `estimate-mid-turn-tokens` + `maybe-compact-mid-turn`. Integration tests with mock agent-session.

**v0.45.22** — Context Loop Prevention Wiring. Mid-turn compaction wired into iteration loop (session threading). Exploration loop detection emits `iteration.exploration-loop` event. GSD progress auto-detected and pinned in Tier A. 3 critical findings from v0.45.22 resolved.

**v0.45.22** — TUI Thinking Leak Fix + Context Circular Loop Prevention. Thinking persisted as transcript entries on tool-call turns. Cyan dim italic styling with truncation. Dynamic Tier-B sizing. Tool result summarization (>8000 chars). Exploration loop detection. GSD progress pinning. Mid-turn compaction trigger.

**v0.45.22** — Audit Remediation + Pre-existing Test Fixes. Fix README corruption guard, `check-provider-status!` arity in 4 test files, ADR completion, test-types keyword fix.

**v0.45.22** — Audit Remediation. Fix 9 findings from v0.45.22–v0.45.22 audit: TR type fixes (time/session-id), README version restoration, telemetry migration, event codec type-tags, hook violation events, GitHub helper dedup, ADR update, metrics fix.

**v0.45.22** — Audit Remediation. TUI core rendering fixes (highlight arity, wrapping, scroll, ANSI, CJK word-breaking), context trace done event with memo-hits, overflow detection consolidation. 4 issues across 2 waves.

**v0.45.22** — Audit Tooling Quality & Test Coverage. Audit script fixes (safe reads, skip-lists, help flag, no double scan), test expansion (+19 tests), with-handlers noise fix, tooling docs, decomposition fitness tests. 3 issues across 3 waves.

**v0.45.22** — Execution Architecture Improvements. `delete-lines` tool, `/wave-done` command, planning path resolution hardening, backup timestamp fix. 4 issues across 4 waves.

**v0.45.22** — GSD Plan Archival + Execution Polish. `/done` command, PLAN.md status auto-update, STATE.md auto-creation, TUI archive notification, iteration label fix, edit chunking guidance. 10 issues across 4 waves.

**v0.45.22** — Security Hardening + GSD Parser Fix. Path traversal guard (safe-manifest-file-path?), backtick stripping (clean-file-path), checksum enforcement, safe-mode canonicalization (resolve-path + boundary matching), OAuth stub predicate, planning prompt hardening. 14 issues across 4 waves.
**v0.45.22** — GSD Planning Architecture Remediation. Thread-safe state (semaphores), visible budget warnings (tool-result-post), lifecycle management (session-shutdown hook), prompt constants, artifact registry expansion, 176 GSD tests. 13 findings resolved across 6 waves.
**v0.45.22** — Audit Remediation. Typed event bridge, HTTP helper consolidation, CI version matrix, security hardening, dead code removal, documentation consistency, layer extraction, protocol types split, sandbox config extraction. 44 findings resolved across 10 waves.

**v0.45.22** — Self-Hosting Workflow Gaps. Extension tool registration fix (register-tools hook passes proper extension-ctx). Subagent tool execution (children get 7 tools + recursive dispatch). Slash commands (/milestone, /issue, /pr, /fmt, /check, /expand). 5300+ tests.

**v0.45.22** — Exploration & Generation Robustness. Soft/hard iteration limits, context-aware retry messages, exploration progress hints, adaptive stream timeout, mid-turn token budget check. Architecture boundary fixes: TUI mock-provider lift, resource-discovery move, session-switch DI. Zero TUI layer violations. 5365 tests.

**v0.45.22** — Timeout Render Fix + Extensibility + Test Runner. TUI streaming state cleanup on auto-retry/error, extension tool registration API, session tree UX, rich component model, built-in components + overlays, extension lifecycle, event coverage (37 events), custom editor + IME, SDK ergonomics, provider OAuth flow, image rendering, skills frontmatter, graceful shutdown, rewritten test runner with per-file result tracking. 4960+ tests.

**v0.45.22** — SGR Mouse Fix. SGR mouse event decoding (mode 1006), mouse enable/disable simplified, test assertion corrections, cleanup robustness.

**v0.45.22** — Mouse Selection Crash Fix. Configurable keybindings via JSON, session import, provider registry, truncation constants.

**v0.45.22** — CHANGELOG Remediation & Docs. Backfilled CHANGELOG entries, API doc comments, per-method RPC rate limiting, test timing fixes.

**v0.45.22** — Documentation & Release Pipeline. API stability tiers, migration templates, package ecosystem docs, SDK catalog, release pipeline hardening, single-source version + CI guard.

**v0.45.22** — Release Pipeline Hardening & Docs. Single-source version, CI guard, API stability tiers, package ecosystem docs.

**v0.45.22** — TUI Component System & Overlay Composition. Component-based rendering with per-zone caching, overlay composition framework for command palette, token-aware context assembly pipeline, test infrastructure cleanup, 3750+ tests.

**v0.45.22** — SDK Ergonomics & Provider Improvements. Unified session factory, context usage tracking, thinking level support.

**v0.45.22** — TUI Component System. Component-based rendering, overlay composition, token-aware context assembly.

**v0.45.22** — TUI Polish & Session Tree. Session tree navigation, markdown tokens, configurable keybindings.

**v0.45.22** — TUI Component System Foundation. Component abstraction, overlay composition framework, 4000+ tests.

**v0.45.22** — TUI Polish & Release. Markdown tokens, configurable keybindings, session tree foundation, input editor bracketed paste, frame-diff and theme fixes.

**v0.45.22** — TUI Correctness & Input Editor. CSI sequence parsing, bright color support, frame-diff fixes, theme wiring, input editor power features, 3681 tests.

**v0.45.22** — Themes & Component Abstraction. Theme system, SGR performance, clipboard support, IME cursor markers, render debounce, component abstraction, OpenAI-compatible provider in init wizard.

**v0.45.22** — Bug Fixes Wave 1-3. Gemini streaming tool call fix, circuit breaker, markdown, thread leak, anchored regexps, port leak.

**v0.45.22** — Kitty Keyboard & Markdown. Kitty keyboard protocol, buffered stdin parsing, markdown expansion, theme system, grapheme-aware cursor, bracketed paste.

**v0.45.22** — Review Remediation & Documentation Accuracy. 28 issues across 6 waves: immediate fixes (stale metrics, CHANGELOG links, version bump), documentation drift (dead links, subcommand docs, wiki metrics), architecture quality (contracts, logging, layer docs), security hardening (destructive blocking, SECURITY section), test coverage (12 new test files, PBT quickcheck invariants), CI maturity (composite action, coverage reporting). PRs #360–#365.

**v0.45.22** — Critical Security & Architecture. Destructive-command warnings default on, shared type extraction (hook-types, protocol-types), CLI/TUI decomposition into submodules, agent turn refactor, manifest validation, crypto-random RPC tokens, structured error types, 50 new tests across 5 modules, HTTP request timeouts.

**v0.45.22** — Hardening & Developer Tooling. 50 new tests (evaluator, cli-args, run-modes, audit-log, token-budget), HTTP request timeouts (300s), destructive-command warning in bash, audit logging utility, version cross-check linter.

**v0.45.22** — Agent Loop Decomposition & Security. Refactored 389-line `run-agent-turn` into 4 helpers, manifest validation before `dynamic-require`, crypto-random RPC handshake tokens, structured error types, Firecrawl error migration.

**v0.45.22** — Architecture Refactor. Extracted shared types to util/, centralized safe-mode at scheduler, decomposed CLI/TUI interfaces into submodules, reorganized runtime run-modes, LRU extension loader cache.

**v0.45.22** — Documentation & Coverage. Documentation metrics bulk refresh, conventions standardization, test coverage gap closure.

**v0.45.22** — Workflow Test Suite. 33 workflow tests across 11 files with 5 fixture modules, covering CLI workflows, tool-use flows, session lifecycle, safety boundaries, SDK-CLI parity, and extension hooks.

**v0.45.22** — Error Handling & Diagnostics. Extension structured error reporting, error classification with remediation hints, verbose diagnostics mode, provider error surfacing, replay error recovery.

**v0.45.22** — CLI & Configuration Hardening. `q sessions` command suite (list/info/delete), sessions TUI command, mock-provider detection warning, Bash shebang handling fix.

**v0.45.22** — Provider & UX Hardening. Anthropic/Gemini streaming (generator-based incremental SSE), API key validation with provider-specific checks, streaming indicator in TUI, improved error messages.

**v0.45.22** — TUI Tool Display & UX Polish. Tool result rendering with truncation, scroll-to-top sentinel, Ctrl+J/Ctrl+Enter multi-line input, Enter-to-submit.

**v0.45.22** — Builder Cookbook and Team Adoption. Team setup guide, builder tutorials for custom tools/providers/extensions.

**v0.45.22** — Structural Hardening. Thread safety (semaphores), contracts on critical entry points, per-session safe-mode, safe-mode path checks, dead code cleanup.

**v0.45.22** — Integration Test Infrastructure. E2E tool→API serialization pipeline tests (OpenAI/Anthropic/Gemini), CLI interactive mode tests (34 cases).

**v0.45.22** — Provider Correctness. Anthropic/Gemini multi-turn tool use message translation, incremental SSE streaming, Gemini tool call unique IDs.

**v0.45.22** — Critical Bug Fixes. Firecrawl poll deadline, tool registration nesting, Anthropic wiring, TUI event subscribers, hook validation.

**v0.45.22** — Developer Tooling & Protocol Consistency. Racket-aware line wrapper, protocol checker, import conflict detector, pre-commit hook.

**v0.45.22** — Architecture & Test Reliability. Decoupled agent/types ↔ tools/tool, extracted CLI builders, session log backup, RPC handshake tokens.

**v0.45.22** — Hardening & Quality. Test coverage expansion, sandbox hardening, SSRF protection, response size limits.

**Previous** — Security Hardening through Foundation:

- ✅ Canonical types and utilities
- ✅ JSONL session storage with tree indexing and forking
- ✅ Provider abstraction and core agent loop
- ✅ Tool system with 14 built-in tools
- ✅ Runtime orchestration with compaction and extensions
- ✅ 5 interfaces (CLI, TUI, JSON, RPC, SDK)
- ✅ Sandboxing and hardening

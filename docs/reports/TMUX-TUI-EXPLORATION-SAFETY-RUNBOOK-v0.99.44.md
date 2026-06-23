# tmux TUI Exploration Safety Runbook — v0.99.44

**Date:** 2026-07-04

## Overview

This runbook documents the safety policies, test suite configuration, and
operational procedures for running real-LLM tmux TUI exploration in the
q-agent project. It covers the exploratory testing infrastructure delivered
in v0.99.44 waves W0–W9.

---

## 1. Safety Policies

### 1.1 Temp HOME Isolation

All real-provider tmux TUI sessions MUST use a temporary HOME directory:

```
q-tmux-home-<timestamp>/
  .q/
    config.json       # copied from real config (bytes, never printed)
    credentials.json  # copied from real credentials (bytes, never printed)
  .config/...
```

**Violation:** Running `q --tui` with real `$HOME` while using real credentials.

**Enforcement:** `make-real-provider-tmux-env` in
`tests/helpers/tmux-q-harness.rkt` creates the isolated environment and
returns `tmux-env` with `temp-home` set. The `copy-q-config-to-temp-home!`
helper copies config/credentials by bytes without printing contents.

### 1.2 Credential Redaction

All captured text, trace summaries, and environment reports are redacted
before being written to disk:

- Bearer tokens: `Authorization: Bearer sk-...` → `Authorization: Bearer <REDACTED>`
- API keys: `sk-...` → `<REDACTED>`
- Config file paths: real `$HOME` path replaced with `<HOME>`
- Any `key=value` pairs with `key`, `token`, `secret`, `password` in key name

**Enforcement:** `redact-sensitive` handles bearer tokens, `sk-` API keys,
`HOME` path substitution, and key-value patterns. `redact-explore-text`
wraps all explorer report output.

### 1.3 Real-Provider Opt-In Gate

Real-provider mode refuses to start unless ALL THREE conditions are met:

1. `Q_TMUX_REAL_PROVIDER=1` — explicit opt-in
2. `Q_TMUX_REAL_PROVIDER_KEY` — non-empty API key
3. `Q_TMUX_REAL_PROVIDER_HOME` — HOME containing valid `.q/credentials.json`

**Enforcement:** `real-provider-authorized?` checks all three env vars.
`require-real-provider-authorization!` raises `exn:fail` if any are missing.

### 1.4 No `tmux kill-server`

Broad tmux cleanup (e.g., `tmux kill-server`) is FORBIDDEN. It would
destroy unrelated user sessions. Only narrowly-named test sessions may be
killed:

```
q-test-<scenario>     # OK to kill
q-tmux-test-<tag>     # OK to kill
```

**Enforcement:** `stop-session!` kills only the named session via
`tmux kill-session -t <name>`.

### 1.5 Mock/No-Network by Default

The default exploration mode is `mock` — no network access, no API
credentials, no cost. Mock mode uses the mock worker provider.

**Enforcement:** `scripts/tmux-tui-explore.rkt` defaults to `--mode mock`.
Real-provider mode requires explicit `--mode real`.

---

## 2. Test Suite Policy

### 2.1 Suite Classification

| Suite | Environment | tmux Tests | Real Provider | CI Default |
|-------|-------------|------------|---------------|------------|
| fast | Any | Excluded | Excluded | Yes |
| smoke | Any | Excluded | Excluded | Yes |
| full | Any | Excluded | Excluded | No |
| tui-tmux | `Q_TMUX_TUI_TESTS=1` | Included | Excluded | No |
| tui-tmux-real | `Q_TMUX_REAL_PROVIDER=1` | Included | Included | No |

### 2.2 Annotations

tmux test files carry these annotations:

```racket
;; @speed slow
;; @suite tui-tmux
;; @boundary e2e
```

The `scripts/run-tests.rkt` test classifier recognizes these annotations
and routes files to the correct suite. tmux test files are never included
in fast, smoke, or full suites.

### 2.3 Test File Inventory

| File | Purpose | Tests |
|------|---------|-------|
| `tests/test-tmux-q-harness.rkt` | Pure unit tests for harness helpers | 149 |
| `tests/test-tmux-tui-smoke.rkt` | T0/T1: Startup, prompt, quit | 5 |
| `tests/test-tmux-tui-session-artifacts.rkt` | T2/T3: Session artifacts | 7 |
| `tests/test-tmux-tui-commands.rkt` | C0-C4: Slash commands | 5 |
| `tests/test-tmux-tui-resize-cleanup.rkt` | U1-U5: Resize, unicode, cleanup | 4 |
| `tests/test-tmux-tui-context-memory.rkt` | X0/M0: Context and memory | 4 |
| `tests/test-tmux-tui-gsd.rkt` | G0-G2: GSD planning | 3 |
| `tests/test-tmux-tui-tools-approval.rkt` | A0-A3: Tools, approval | 3 |
| `tests/test-tmux-tui-explore.rkt` | Explorer scenarios | 7 |
| `tests/test-tmux-tui-report.rkt` | Report generator | 3 |

### 2.4 Real-Provider Test Policy

Real-provider tests are:

1. **Never default CI blockers** — they require explicit opt-in
2. **Cost-bearing** — each run consumes API tokens
3. **Credential-bearing** — real API keys are involved
4. **Environment-specific** — results may vary by model/provider

Real-provider test results should be classified as:

| Classification | Meaning |
|----------------|---------|
| `pass` | Scenario completed with expected evidence |
| `partial` | Some evidence found but incomplete |
| `flake` | Intermittent pass/fail — needs investigation |
| `fail` | Scenario failed deterministically |
| `incomplete` | Not yet run with real provider |
| `skip` | Intentionally skipped (unsupported) |

---

## 3. Exploratory Testing Infrastructure

### 3.1 Components

```
scripts/tmux-tui-explore.rkt       Official exploratory runner (mock/real)
scripts/tmux-tui-smoke.rkt          Scenario smoke runner
scripts/tmux-tui-report.rkt         Artifact report generator
tests/helpers/tmux-q-harness.rkt    Reusable test harness (149 tests)
tests/test-tmux-q-harness.rkt       Harness unit tests
tests/test-tmux-tui-explore.rkt     Explorer unit tests
```

### 3.2 Harness API Surface (W0–W9)

#### W2: Structured Turn Completion
- `find-trace-jsonl-paths` — locate trace.jsonl files in session dirs
- `read-trace-events` — parse JSONL entries
- `turn-completion-trace-entry?` — predicate for completion events
- `latest-turn-completion-event` — find latest completion event
- `wait-for-turn-completion-event` — poll for completion with timeout

#### W3: Real-Provider Safe Helpers
- `real-provider-authorized?` — three-env-var gate check
- `make-real-provider-tmux-env` — create isolated temp env
- `copy-q-config-to-temp-home!` — copy config by bytes (no printing)
- `send-prompt-and-wait!` — send prompt + wait for trace completion
- `write-exploration-artifacts!` — write normalized capture + env summary

#### W4: Approval-Prompt Automation
- `detect-approval-prompt` / `parse-approval-prompt` — detect and extract
- `classify-approval-safety` — safe/dangerous/caution classification
- `handle-approval-if-present!` — auto-approve safe, auto-deny dangerous
- `approve-approval!` / `deny-approval!` — send y/n keys

#### W5: Tool-Execution Trace Verification
- `tool-execution-phases` — 6 known tool event phases
- `find-tool-execution-events` — scan traces for tool events
- `parse-tool-events` — convert to tool-info structs
- `compute-file-fingerprint` — len+sum fingerprint
- `verify-file-unchanged!` — compare before/after fingerprints
- `detect-sensitive-leak` / `verify-artifact-redacted!` — leak detection

#### W6: MAS/Subagent Lifecycle Evidence
- `mas-lifecycle-phases` — 11 known MAS event phases
- `find-mas-events` — scan traces for MAS events
- `parse-spawn-approval-event` — extract spawn approval data
- `parse-mas-lifecycle` — full lifecycle parse
- `verify-subagent-spawn-lifecycle` — verify spawn→tool→complete chain

#### W7: Durable Memory Restart Round-Trip
- `memory-event-phases` / `session-lifecycle-phases` — known phases
- `find-memory-events` / `find-session-lifecycle-events` — scan traces
- `parse-memory-store-event` / `parse-memory-retrieval-event` — extract data
- `parse-session-start-event` — extract reason/prev-id/session-dir
- `parse-durable-memory-roundtrip` — full lifecycle: store→restart→retrieve
- `verify-durable-memory-roundtrip` — verify complete round-trip present

#### W8: GSD Lifecycle and Release/Audit Truthfulness
- `gsd-event-phases` — 16 known GSD event phases
- `find-gsd-events` — scan traces for GSD events
- `parse-gsd-transition-event` / `parse-gsd-wave-event` / `parse-gsd-plan-event`
- `parse-gsd-lifecycle` — full lifecycle parse
- `verify-gsd-transition-succeeded` — verify transition from→to
- `release-refusal-patterns` — 7 known refusal phrases
- `detect-release-authorization-refusal` — scan for refusal patterns
- `find-release-manifest` / `verify-release-manifest-present!`
- `verify-release-authorization-refused!` — assert refusal present

#### W9: Flake Classification and Centralized Reporting
- `flake-classifications` — 8 standard outcome categories
- `flake-record` struct — per-scenario classification
- `classify-result-status` — map status to classification
- `flake-indicator-patterns` / `detect-flake-indicators` — flake text detection
- `flaky-run?` — check if any record is flake-classified
- `classify-exploration-run` — produce records from results
- `central-log-entry` struct — structured log entry with counts
- `make-central-log-entry` — aggregate records
- `write-central-log-entry!` — append JSONL to log
- `parse-central-log` — read JSONL entries
- `exploration-bundle` struct — aggregated summary
- `make-exploration-bundle` — create bundle with pass-rate
- `render-bundle-index-markdown` — markdown summary table

### 3.3 Explorer Scenarios

| Tag | Title | Mock Status | Real Status |
|-----|-------|-------------|-------------|
| memory | Memory and Context Retention | pass | pending-real-run |
| gsd | GSD Planning Lifecycle | pass | pending-real-run |
| mas | MAS/Subagent Coordination | pass (caveat) | pending-real-run |
| tools | Tool Execution and Approval | pass | pending-real-run |
| release-audit | Release Authorization Audit | pass | pending-real-run |
| durable-memory | Durable Memory Restart Round-Trip | partial | pending-real-run |

### 3.4 Running the Explorer

```bash
# Mock mode (default, no network, no cost)
racket scripts/tmux-tui-explore.rkt

# Real-provider mode (requires explicit opt-in)
Q_TMUX_REAL_PROVIDER=1 \
Q_TMUX_REAL_PROVIDER_KEY=$ANTHROPIC_API_KEY \
Q_TMUX_REAL_PROVIDER_HOME=$HOME \
racket scripts/tmux-tui-explore.rkt --mode real

# List scenarios
racket scripts/tmux-tui-explore.rkt --list

# Run specific scenario
racket scripts/tmux-tui-explore.rkt --filter memory
```

### 3.5 Centralized Log Protocol

Exploration runs append structured JSONL entries to a central log:

```json
{"timestamp":"2026-07-04T12:00:00","run_id":"run-001","total":6,"pass":4,"partial":1,"fail":0,"flake":0,"skip":0,"incomplete":1,"mode":"mock"}
```

The log enables:
- Historical trend tracking across runs
- Pass-rate evolution over time
- Flake detection and classification
- Bundle-level summary generation

---

## 4. Known Limitations

### 4.1 Model Turn Completion Signal

The visible `q>` prompt is NOT a reliable model-turn completion signal.
The TUI may show `q>` while the model is still thinking or streaming.

**Mitigation:** Use `trace.jsonl` `turn.completed` or
`stream.turn.completed` events instead. The harness provides
`wait-for-turn-completion-event` for this purpose.

### 4.2 Queued Prompts

If automation sends prompts too early (before the model finishes), the
TUI queues them. This produces `[Queued — will run after current task]`.

**Mitigation:** Always wait for turn completion events before sending the
next prompt. Use `assert-no-queued-prompts!` to verify.

### 4.3 Sentinel Echo and Wrapping

Sentinel text may appear in echoed prompts. Pane capture may truncate or
wrap markers. This causes false-positive and false-negative matches.

**Mitigation:** Use structured trace evidence (trace.jsonl events) rather
than visible text matching. Redact and normalize captures before comparison.

### 4.4 Pane Capture Truncation

tmux pane capture may truncate long outputs, losing evidence at the
bottom of the scrollback.

**Mitigation:** Use `capture-pane -p -S -` (full scrollback) and
normalize before matching. For critical evidence, rely on trace.jsonl
events rather than pane text.

---

## 5. Failure Artifact Handling

When a scenario fails, the harness saves diagnostic artifacts:

```
/var/tmp/q-tmux-art-<timestamp>/
  raw-capture.txt            # Full pane with ANSI escapes
  normalized-capture.txt     # Stripped/normalized output
  env-summary.txt            # Command line, env (redacted)
  temp-tree.txt              # Directory listing of temp dirs
  trace-summary.txt          # Trace event summary (W3+)
  reason.txt                 # Failure reason (if present)
```

**Retained success dirs:** Empty `q-tmux-art-*` directories without
`reason.txt` are classified as `retained-success-dir`, not failures.

Generate a report from artifacts:

```bash
racket scripts/tmux-tui-report.rkt
racket scripts/tmux-tui-report.rkt --dir /path/to/artifacts
```

---

## 6. CI Integration Recommendations

### 6.1 Current State

tmux tests are NOT included in CI fast/smoke/full suites. They are opt-in
only via `Q_TMUX_TUI_TESTS=1`.

### 6.2 Recommended Nightly Job

```yaml
# .github/workflows/tmux-tui-nightly.yml (future)
name: tmux TUI Nightly
on:
  schedule:
    - cron: '0 4 * * *'  # 4 AM UTC daily
jobs:
  tmux-tui:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: ./.github/actions/setup-racket
      - run: sudo apt-get install -y tmux
      - run: Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt
      - run: racket scripts/tmux-tui-explore.rkt --mode mock
      - run: racket scripts/tmux-tui-report.rkt
```

### 6.3 Real-Provider CI

Real-provider tests should NOT run in CI due to:
- API cost per run
- Credential exposure risk
- Model/provider variability

Real-provider exploration should be run manually with the safety
procedures in this runbook.

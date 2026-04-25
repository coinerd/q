<!-- verified-against: 0.19.7 -->
# Security & Trust Model

This document provides an honest, per-area assessment of what q enforces
today and what is planned. It is the authoritative reference for security
features, trust boundaries, and the extension trust model.

**Architecture context:** q is structured as a 5-layer system —
LL providers → Agent Core → Runtime → Tools/Extensions → Interfaces.
Session storage is append-only JSONL. Provider adapters exist for OpenAI,
Anthropic, and Gemini. An extension API with a hook system allows
custom behavior at lifecycle points.

---

## 1. Safe Mode Guarantees

Safe mode restricts the agent to read-only operations, preventing any
file modification or command execution.

### Activation

Safe mode activates when **any** of the following is true:

| Method | Example |
|--------|---------|
| CLI flag | `q --safe` |
| Environment variable | `Q_SAFE_MODE=1` |
| Config file | `"safe-mode": true` in `~/.q/config.json` |

### What is enforced

When safe mode is active:

- **Tool blocking** — The scheduler (`q/tools/scheduler.rkt`) blocks
  execution of `bash`, `edit`, `write`, `firecrawl`, and all
  extension-provided tools before they run. Only `read`, `ls`, `grep`,
  `find`, and `date` are permitted.

- **Path restriction** — File read operations are restricted to the
  project directory only. `allowed-path?` checks are enforced by the
  scheduler for every tool call containing a path argument.

- **Destructive command warnings** (default on) — Even when bash *is*
  available (full trust mode), the bash tool (`q/tools/builtins/bash.rkt`)
  emits a `WARNING` to stderr for commands matching destructive patterns
  such as `rm -rf`, `mkfs`, `dd of=/dev/`, `shutdown`, `reboot`,
  `git push --force`, and pipe-to-shell patterns.

- **Destructive command blocking** (opt-in) — When
  `current-block-destructive` is set to `#t`, destructive commands
  return an error instead of executing. This parameter is off by default.

### Implementation

Safe mode is implemented in `q/runtime/safe-mode.rkt` as **query
functions only** — it does not enforce restrictions itself. Callers
(scheduler, tool dispatch, extension loader) check `allowed-tool?`
and `allowed-path?` before executing operations.

---

## 2. Credential Handling

### Current state

API keys for LLM providers are resolved through a **pluggable backend
system** (`q/runtime/credential-backend.rkt`) with the following
priority chain:

1. **Environment variables** — e.g. `Q_OPENAI_API_KEY`,
   `Q_ANTHROPIC_API_KEY`, `Q_GEMINI_API_KEY` (read-only)
2. **OS keychain** — `secret-tool` (Linux/libsecret) or `security`
   (macOS Keychain) — encrypted at rest
3. **Credential file** — `~/.q/credentials.json` (dedicated storage)
4. **Config file** — `~/.q/config.json` provider entries with an
   `api-key` field

The original `q/runtime/auth-store.rkt` remains for backward
compatibility. New code should use the backend abstraction.

Credential files are written with **owner-only permissions (`0600`)**
using atomic write (temp file + rename) to prevent corruption.

**Note on plaintext:** The file backend stores keys as plaintext on
disk. On shared systems, use the OS keychain backend or environment
variables. See `docs/getting-started/credentials.md` for full details.

### Display safety

All credential structs implement `gen:custom-write` and display masked
keys (e.g. `sk-...7k3d`). The `cred->redacted` function converts a
credential to a `redacted-credential` for logging.

### Format validation

`validate-credential-format` performs basic prefix checks:
- OpenAI keys must start with `sk-`
- Anthropic keys must start with `sk-ant-`

### Roadmap

| Feature | Status |
|---------|--------|
| Plaintext file storage (`~/.q/credentials.json`) | ✅ Enforced |
| Owner-only file permissions (`0600`) | ✅ Enforced |
| Environment variable fallback | ✅ Enforced |
| Key masking in logs and display | ✅ Enforced |
| Pluggable credential backend abstraction | ✅ Enforced |
| OS keychain integration (secret-tool / macOS Keychain) | ✅ Enforced |
| Chained backend resolution (env → keychain → file) | ✅ Enforced |
| Encrypted credential backends (beyond OS keychain) | 📋 Planned |
| Hardware security module (HSM) support | 📋 Planned |

---

## 3. Session Log Integrity

### Storage format

Session logs are stored as **append-only JSONL** files in
`~/.q/sessions/`. Each line is a JSON object representing a single
event with required fields: `id`, `role`, `kind`, `content`,
`timestamp`.

### Integrity verification

`verify-session-integrity` (in `q/runtime/session-store.rkt`) scans a
session log and returns a report with:

| Field | Description |
|-------|-------------|
| `total-entries` | Count of non-empty lines |
| `valid-entries` | Entries passing all checks |
| `invalid-entries` | List of `(line-number, reason)` pairs |
| `truncated-at-end?` | Whether the file ends without a newline |
| `entry-order-valid?` | Whether timestamps are non-decreasing |

**Checks performed:**
- Valid JSON per line
- All required fields present (`id`, `role`, `kind`, `content`, `timestamp`)
- No duplicate entry IDs
- Chronological timestamp ordering
- File ends with a newline (no truncated writes)

### Crash safety

- **Write-ahead marker** — A `.pending` file is created before appending
  entries and removed after successful write. If a `.pending` marker
  exists, the last entry may be truncated.
- **Truncated entry detection** — Lines not ending with a newline are
  flagged.
- **Repair** — `repair-session-log!` removes invalid entries and
  rewrites the log cleanly, creating a `.bak` backup first.

### CLI access

Session integrity verification is accessible via the CLI:
```
q sessions inspect <id>
```
This runs `verify-session-integrity` and reports results.

### Important limitation

Session logs are **tamper-detectable but not tamper-proof**:
- The `repair-session-log!` function can rewrite logs for crash recovery
- There is no cryptographic hash chaining of entries today
- A `repair-session-log!` call is admin-only and never automatic

### Roadmap

| Feature | Status |
|---------|--------|
| Append-only JSONL storage | ✅ Enforced |
| Write-ahead marker for crash safety | ✅ Enforced |
| Integrity verification (structural) | ✅ Enforced |
| Truncated entry detection | ✅ Enforced |
| Repair with backup | ✅ Enforced |
| Legacy log loading (without hashes) | ✅ Enforced |
| Hash-chained entries (`prev_hash` + `hash`, SHA-256) | 📋 Planned |
| Cryptographic tamper-proofing | 📋 Planned |
| `q verify-session <path>` CLI command | 📋 Planned |

---

## 4. Tool Restrictions and Sandbox Boundaries

### Built-in tools

q ships with **10 built-in tools** registered by
`q/tools/registry-defaults.rkt`:

| Tool | Purpose | Risk Level |
|------|---------|------------|
| `read` | Read file contents | Low |
| `write` | Create/overwrite files | **High** — blocked in safe mode |
| `edit` | Precise file edits | **High** — blocked in safe mode |
| `bash` | Shell command execution | **High** — blocked in safe mode |
| `grep` | Search file contents | Low |
| `find` | Find files by glob | Low |
| `ls` | List directory contents | Low |
| `date` | Current date/time | Low |
| `firecrawl` | Web search (requires API key) | Medium — blocked in safe mode |
| `spawn-subagent` | Delegate task to child agent | Medium |

### Bash sandbox

The bash tool (`q/tools/builtins/bash.rkt`) executes commands via
`q/sandbox/subprocess.rkt` with the following controls:

- **Custodian-based subprocess management** — Each subprocess runs
  under its own Racket custodian, enabling complete resource cleanup on
  timeout or kill.
- **Configurable resource limits** (from `q/sandbox/limits.rkt`):
  - Timeout (default: 120s, configurable via settings)
  - Max output (default: 10 MB)
  - Max memory (default: 512 MB)
  - Max concurrent processes (default: 10, tracked via `track-process!`)
- **Destructive command detection** — Regex-based pattern matching
  against `rm -rf`, `mkfs`, `dd`, `shutdown`, `reboot`, force push,
  pipe-to-shell, and other dangerous patterns.
- **Two-tier response:**
  - `current-warn-on-destructive` (default: `#t`) — logs warning
  - `current-block-destructive` (default: `#f`) — returns error

### Path restrictions

In safe mode, the `edit` and `write` tools check `safe-mode?` and
`allowed-path?` before operating. The scheduler also validates path
arguments in the preflight phase.

### Tool isolation

Tools do **not** write session logs directly. All logging goes through
the session store's `append-entry!` function. Tools return structured
results (`tool-result`) consumed by the agent loop.

---

## 5. Extension Trust Model

### Extension quarantine

Extensions can be placed in three states via
`q/extensions/quarantine.rkt`:

| State | Description |
|-------|-------------|
| **Active** | Loaded and functional |
| **Disabled** | Marked as disabled; no file moves |
| **Quarantined** | Moved to `~/.q/quarantine/`; completely isolated |

Quarantine operations:
- `quarantine-extension!` — moves the extension directory to the
  quarantine root and records metadata (original path, timestamp)
- `restore-extension!` — moves a quarantined extension back to its
  original location
- `disable-extension!` — marks as disabled without moving files

All state mutations use **file locking** (`with-quarantine-lock`) to
prevent TOCTOU races.

### Hook system

Extensions interact with the agent lifecycle through a hook system
(`q/extensions/hooks.rkt`):

| Hook Point | Purpose |
|------------|---------|
| `tool-call` | Intercept/amend/block tool calls |
| `tool-result` | Post-process tool results |
| `message-start` | Intercept messages before processing |
| `message-end` | Post-process messages |
| `model-request-pre` | Modify LLM requests before sending |
| `before-provider-request` | Last chance to block/amend provider requests |
| `session-before-compact` | Block or modify context compaction |
| `session-before-switch` | Block session switching |
| `session-before-fork` | Block session forking |
| `input` | Intercept user input |

**Hook actions:**
- `pass` — continue with unchanged payload
- `amend` — replace payload for next handler
- `block` — stop dispatch immediately

**Critical hooks** (`tool-call`, `session-before-switch`,
`session-before-fork`, `session-before-compact`, `input`) default to
`block` on error (safety-first). Advisory hooks default to `pass`
(liveness-first).

**Per-hook timeout** — handlers that exceed `current-hook-timeout-ms`
(default: 100ms) are skipped with a warning.

### Extension lifecycle

Extensions follow this lifecycle:

```
load → initialize → activate → (running) → deactivate → unload
                                         ↘ quarantine ↙
```

Extensions can be quarantined at any point during the active phase.

### Roadmap

| Feature | Status |
|---------|--------|
| Extension quarantine (disable/quarantine/restore) | ✅ Enforced |
| File-locked state mutations | ✅ Enforced |
| Hook dispatch with pass/amend/block | ✅ Enforced |
| Critical vs advisory hook classification | ✅ Enforced |
| Per-hook timeout | ✅ Enforced |
| Trust tiers (untrusted/verified/trusted) | 📋 Planned |
| Automatic quarantine of new extensions | 📋 Planned |
| Extension permission manifests | 📋 Planned |
| Signed extensions | 📋 Planned |

---

## 6. What is Enforced vs Planned — Honest Status

### Summary table

| Area | Feature | Status |
|------|---------|--------|
| **Safe Mode** | Tool blocking in safe mode | ✅ Enforced |
| **Safe Mode** | Path restriction to project root | ✅ Enforced |
| **Safe Mode** | Destructive command warnings (default on) | ✅ Enforced |
| **Safe Mode** | Destructive command blocking (opt-in) | ✅ Enforced |
| **Credentials** | Plaintext file storage with `0600` perms | ✅ Enforced |
| **Credentials** | Environment variable fallback | ✅ Enforced |
| **Credentials** | Key masking in logs | ✅ Enforced |
| **Credentials** | OS keychain integration | 📋 Planned |
| **Credentials** | Encrypted backends | 📋 Planned |
| **Session Logs** | Append-only JSONL | ✅ Enforced |
| **Session Logs** | Write-ahead markers for crash safety | ✅ Enforced |
| **Session Logs** | Structural integrity verification | ✅ Enforced |
| **Session Logs** | Legacy log loading (graceful degradation) | ✅ Enforced |
| **Session Logs** | Hash-chained entries (SHA-256) | 📋 Planned |
| **Session Logs** | Cryptographic tamper-proofing | 📋 Planned |
| **Session Logs** | `q verify-session` CLI command | 📋 Planned |
| **Sandbox** | Custodian-based subprocess cleanup | ✅ Enforced |
| **Sandbox** | Resource limits (time, memory, output, processes) | ✅ Enforced |
| **Sandbox** | Concurrent process tracking (SEC-12) | ✅ Enforced |
| **Extensions** | Quarantine/disable/restore | ✅ Enforced |
| **Extensions** | Hook dispatch with pass/amend/block | ✅ Enforced |
| **Extensions** | Trust tiers | 📋 Planned |
| **Extensions** | Automatic new extension quarantine | 📋 Planned |
| **Extensions** | Permission manifests | 📋 Planned |
| **Extensions** | Signed extensions | 📋 Planned |

### Design philosophy

q follows a **defense-in-depth** approach:
1. Safe mode provides a hard wall for untrusted contexts
2. Destructive command detection adds a safety net even in full trust
3. Sandbox limits prevent resource exhaustion
4. Extension quarantine isolates problematic extensions
5. Session log integrity enables post-hoc audit

No single layer is sufficient — each provides independent protection
should another fail.

---

## Release Checklist

When security features change, update this document:

- [ ] Update the `verified-against` version comment at the top
- [ ] Reflect new enforced features with ✅ in the status tables
- [ ] Move planned features to enforced when implemented
- [ ] Update tool lists if new built-in tools are added
- [ ] Update hook point tables if new hooks are added
- [ ] Update credential handling section if backends change
- [ ] Update session integrity section if hash chaining lands
- [ ] Update extension trust section if trust tiers are implemented
- [ ] Verify all code references still point to correct modules

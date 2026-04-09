# Security Model

This document describes q's security model: what it protects against, what it
does not, and how the sandbox, tools, and cancellation mechanisms interact.

## Trust Model

q runs as the invoking user with their full filesystem and network permissions.
The sandbox provides **defense-in-depth**, not isolation. It limits resource
consumption and catches runaway computations, but it is not a security boundary
against a determined adversary.

## Trust Boundaries

```
User ──trusts──▶ q          (file I/O, shell execution as user)
  q ──sends───▶ LLM provider (prompts → responses over HTTPS)
  q ──loads───▶ Extensions   (in-process, same trust level as core)
  q ──runs───▶ Tools         (sandboxed evaluator or subprocess)
```

| Boundary | Direction | Protocol |
|---|---|---|
| User → q | User delegates actions | CLI / TUI / SDK |
| q → LLM provider | HTTP(S) request/response | Provider API |
| q → Extensions | In-process function call | Extension API |
| q → Shell tools | Subprocess under custodian | `sandbox/subprocess.rkt` |
| q → Racket eval | Sandboxed evaluator | `sandbox/evaluator.rkt` |

## What the Sandbox Protects Against

### Sandboxed Evaluator (`sandbox/evaluator.rkt`)

The evaluator uses Racket's built-in `racket/sandbox` module to run Racket code
with strict limits:

- **Memory**: 256 MB per evaluation (`sandbox-memory-limit`)
- **Wall-clock timeout**: configurable, default 30 seconds (`sandbox-eval-limits`)
- **Output capture**: stdout and stderr are captured as strings, not inherited
  from the parent process

### Resource Limits (`sandbox/limits.rkt`)

The `exec-limits` struct and `with-resource-limits` wrapper enforce:

- **Timeouts**: wall-clock timeout via `sync/timeout`; custodian shutdown on
  expiry
- **Output caps**: bounded port reader truncates stdout/stderr at a configurable
  byte budget (default 1 MB, strict preset 64 KB)
- **Memory**: tracked via custodian hierarchy; child custodians can be shut down
  independently
- **Process count**: configurable ceiling on child processes

Three presets are available:

| Preset | Timeout | Output cap | Memory | Max processes |
|---|---|---|---|---|
| `strict-exec-limits` | 30 s | 64 KB | 128 MB | 3 |
| `default-exec-limits` | 120 s | 1 MB | 512 MB | 10 |
| `permissive-exec-limits` | 600 s | 10 MB | 2 GB | 50 |

`merge-limits` takes the **minimum** of each field, so the stricter policy always
wins when limits are composed.

### Subprocess Management (`sandbox/subprocess.rkt`)

Every shell command runs under its own **custodian**. On timeout or error, the
custodian is shut down, which kills the subprocess and closes all ports. Key
properties:

- Shell commands execute via `/bin/sh -c` with single-quoted argument escaping
- Output is incrementally read with a byte budget (truncation marker appended on
  overflow)
- The subprocess exit code, stdout, stderr, and timed-out flag are captured in
  the result struct

## What the Sandbox Does NOT Protect Against

These are **known gaps**, not bugs:

1. **Shell command injection via bash tool**: Commands run as the invoking user.
   q trusts the LLM to produce reasonable commands; there is no allowlist or
   denylist.

2. **File system access**: `read` and `write` tools operate as the user. Any file
   the user can access, q can access. There is no path validation beyond what
   the OS enforces.

3. **Malicious LLM responses**: A compromised or adversarial LLM can craft tool
   calls that delete files, exfiltrate data, or make network requests. q has no
   confirmation step before executing tool calls.

4. **Network access from subprocess commands**: Shell commands can make network
   calls (`curl`, `wget`, etc.). The sandbox does not restrict network access.

5. **Extension code**: Extensions run in-process at the same trust level as the
   core. A malicious extension has full access to the Racket runtime.

## Command Execution Constraints

- Shell commands run via `run-subprocess` with a configurable timeout (default
  120 seconds for the bash tool)
- No command allowlist or denylist currently exists
- Working directory is controlled by the execution context (`exec-context`) or
  defaults to `(current-directory)`
- Each subprocess runs under a dedicated custodian for cleanup isolation
- Output is truncated at the configured byte limit with a `[output truncated at
  N bytes]` marker

## Cancellation and Timeouts

### Agent Loop

- The agent loop (`agent/loop.rkt`) accepts an optional `cancellation-token`
- On each iteration, the loop checks `cancellation-token-cancelled?`; if true,
  it returns immediately with a cancellation reason
- The loop also has a configurable `max-iterations` ceiling

### Cancellation Token (`util/cancellation.rkt`)

- Cooperative cancellation via a boxed boolean flag
- Optional callback fires on `cancel-token!` (called each time, not
  idempotent — callers must guard if needed)
- Propagated through `exec-context` to tool invocations

### Tool Timeouts

- `with-resource-limits` wraps thunk execution with `sync/timeout`
- On timeout, the custodian is shut down via `custodian-shutdown-all`
- Subprocess tool uses `sync/timeout` on the subprocess object; kills the
  process on expiry

## Known Non-Goals

- q is **not a security boundary** against determined adversaries
- q does **not** provide container-level isolation (use Docker/nspawn for that)
- q is **not** multi-tenant safe — all sessions share the same OS user
- Session storage is **not encrypted** — JSONL files are plain text on disk
- No audit logging of tool invocations (beyond session journal entries)
- No rate limiting on LLM API calls

## Regression Tests

Security-relevant regression tests live in `tests/test-sandbox-security.rkt` and
cover:

- Path traversal handling in read/write tools
- Cancellation token propagation
- Timeout enforcement in subprocess execution
- Sandboxed evaluator blocking dangerous operations

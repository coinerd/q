# Audit: Integration Subsystem — v0.99.45 W10

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w10-integration.rkt`
**Tests:** 74 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `runtime/provider/provider-registry.rkt` — Centralized provider and model registry
  - `runtime/auto-retry.rkt` — Auto-retry with exponential backoff and error classification
  - `runtime/settings-core.rkt` — Settings loading, deep-merge, struct
  - `runtime/settings-query.rkt` — Settings query functions (40+ typed accessors)
  - `sandbox/limits.rkt` — Resource ceilings, timeout policy, process tracking
  - `sandbox/ipc-protocol.rkt` — Wire protocol for gateway↔worker IPC

## Test Matrix

| Area | Tests | Status |
|------|-------|--------|
| Provider Registry (empty, register/lookup, unregister, reregister, reject non-provider, config merge) | 7 | PASS |
| Model Registration (register, find by ID/name, case-insensitive, not found, unregister, fuzzy search, cascade delete) | 8 | PASS |
| Provider Metadata (summary, metadata extraction) | 2 | PASS |
| Auto-Retry Classification (rate-limit, timeout, auth, unknown, retryable, context-overflow, permanent-tool) | 9 | PASS |
| Auto-Retry Execution (success first try, success on second, non-retryable fail, exhausted, callback, policy) | 6 | PASS |
| Settings Loading (deep-merge simple/nested, make-minimal, with-overrides, ref-with-default, ref*-nested) | 6 | PASS |
| Settings Queries (parallel-tools, http-timeout, steering, context-assembly, credential-policy, shell-risk, execution-plane, verifier, blackboard, hot-swap, auto-reload, mcp, broker) | 13 | PASS |
| Sandbox Limits (default, strict, permissive, merge-stricter-wins, within-limits ok/timeout/output/partial, process tracking) | 9 | PASS |
| IPC Protocol (request/response construction, round-trip, error response, make-error/timeout, invalid jsexpr, constants, too-large) | 11 | PASS |
| **Total** | **74** | **ALL PASS** |

## Findings

### FINDING-001 (info): Provider registry with thread-safe config merging

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The provider registry provides:
- **Thread-safe**: All operations use semaphore-guarded box mutation
- **Config merge on reregister**: When re-registering a provider, config is deep-merged (existing + new)
- **Cascade model deletion**: Unregistering a provider removes all its models
- **Provider validation**: `register-provider!` rejects non-provider instances with `exn:fail:contract`
- **Model search**: 4-level search: exact ID → exact name → prefix ID → substring names

The `find-model` function is case-insensitive and uses progressive relaxation: exact match first, then prefix, then substring.

**Impact:** Positive — robust concurrent provider management with fuzzy search.

### FINDING-002 (info): Error classification with structured + string fallback

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The auto-retry system classifies errors using a two-tier approach:
1. **Structured** (`provider-error?`): Checks the `provider-error-category` field for categories like `rate-limit`, `timeout`, `server`, `network`, `context-overflow`
2. **String fallback**: For non-structured exceptions, uses pattern matching against known error strings

The `retryable-error?` function correctly excludes permanent tool errors (validation failures) from retry. The `classify-error` function maps to symbolic types: `rate-limit`, `timeout`, `auth`, `context-overflow`, `max-iterations`, `provider-error`.

Per-type retry budgets prevent one error type from exhausting the global budget (e.g., rate-limit retries don't consume timeout budget).

**Impact:** Positive — comprehensive error classification with defense-in-depth.

### FINDING-003 (info): Retry exhaustion wraps original exception with metadata

**Severity:** Info (positive finding)
**Category:** API Design
**Description:** When retries are exhausted, the original exception is wrapped in a `retry-exhausted` struct that includes:
- `original-exn` — the last exception that caused exhaustion
- `attempts` — number of retry attempts made
- `last-error-type` — symbolic category of the last error
- `total-delay-ms` — cumulative time spent in backoff
- `error-history` — list of error types encountered in order

This allows callers (agent-session, TUI) to distinguish retry exhaustion from first-time failures and provide appropriate user feedback.

**Impact:** Positive — rich error context for debugging and UX.

### FINDING-004 (info): Settings deep-merge with right-biased nested hash merging

**Severity:** Info (positive finding)
**Category:** Configuration
**Description:** The `deep-merge-hash` function provides:
- **Right-biased**: Project config overrides global config for scalar values
- **Recursive**: Nested hashes are merged recursively (not replaced)
- **Type-safe**: Non-hash values are replaced outright, not merged
- **Memoized**: Settings are cached by `(project-dir, home-dir)` with mtime invalidation

The settings query layer provides 40+ typed accessor functions covering: execution plane, verifier, blackboard, hot-swap, MCP, broker, memory, steering, credential policy, shell risk, context assembly, trace logging, and more. Each has documented defaults and coercion behavior.

**Impact:** Positive — comprehensive configuration system with type coercion and caching.

### FINDING-005 (info): Sandbox limits with stricter-wins merge and process tracking

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The sandbox limits system provides:
- **3 presets**: `strict` (30s, 64KB, 128MB, 3 procs), `default` (120s, 1MB, 512MB, 10 procs), `permissive` (600s, 10MB, 2GB, 50 procs)
- **Stricter-wins merge**: `merge-limits` takes the minimum of each dimension
- **Thread-safe process tracking**: Semaphore-guarded box counter with `track-process!`/`untrack-process!`
- **Process limit enforcement**: `track-process!` errors when count exceeds `current-max-processes` (default 10)
- **`within-limits?`**: Partial checking — only checks dimensions that are provided

The `with-resource-limits` combinator runs a thunk with custodian-based cleanup: on timeout, the entire custodian subtree is shut down.

**Impact:** Positive — defense-in-depth resource management with graceful timeout handling.

### FINDING-006 (info): IPC protocol with schema versioning and size limits

**Severity:** Info (positive finding)
**Category:** Security / Architecture
**Description:** The IPC wire protocol provides:
- **Schema versioning**: Both requests and responses carry `schema-version` (currently 1)
- **Size limits**: Max request 1MB, max response 10MB, enforced via `ipc-request-too-large?`
- **4 response statuses**: `ok`, `error`, `timeout`, `crashed`
- **Graceful deserialization**: `jsexpr->ipc-request` and `jsexpr->ipc-response` return `#f` on malformed input (no exceptions)
- **Capability field**: Each request carries a capability symbol for permission checking

The protocol is JSON-serializable (all values are jsexpr-compatible) for cross-process communication over stdio.

**Impact:** Positive — robust wire protocol with defense against malformed input.

### FINDING-007 (info): Settings defaults follow safety-first principles

**Severity:** Info (positive finding)
**Category:** Configuration
**Description:** The default settings follow a security-first approach:
- **Verifier enabled by default** (since v0.99.15) with `high` risk threshold
- **Blackboard enabled by default** (since v0.99.14)
- **Hot-swap enabled by default** (since v0.99.18)
- **Auto-reload disabled by default** (opt-in, even when hot-swap is on)
- **MCP disabled by default** (feature gate)
- **Broker disabled by default** (always opt-in)
- **Parallel tools disabled by default**
- **Credential policy**: `'auto` (backward-compatible)
- **Shell risk classifier**: `'regex` (backward-compatible)
- **Max rework iterations**: 3 (prevents infinite verifier loops)

**Impact:** Positive — sensible security defaults with explicit opt-in for experimental features.

## Architecture Summary

The Integration subsystem ties together all other subsystems:

1. **Provider Registry** (`runtime/provider/provider-registry.rkt`):
   - Centralized LLM provider and model management
   - Thread-safe with semaphore-guarded boxes
   - Config merging, cascade deletion, fuzzy model search

2. **Auto-Retry** (`runtime/auto-retry.rkt`):
   - Error classification (structured + string fallback)
   - Per-type retry budgets with exponential backoff
   - Retry-exhausted exception wrapping

3. **Settings** (`runtime/settings-core.rkt`, `settings-query.rkt`):
   - Deep-merge of global + project configs
   - 40+ typed accessor functions with coercion
   - Memoized with mtime invalidation

4. **Sandbox** (`sandbox/limits.rkt`, `ipc-protocol.rkt`):
   - Resource ceilings with stricter-wins merge
   - Thread-safe process tracking with limit enforcement
   - JSON-RPC-over-stdio wire protocol for execution plane

# Adapter Layer Contracts — v0.99.38 W6

**Date:** 2026-06-23
**Issue:** #8480
**Base:** `main@157472f9`

## Purpose

Manual §14 (Different Layer, Different Abstraction) and §48 (External Adapters):
adapters should hide OS/process/network/terminal details and expose stable domain
operations. The #8466 GUI hotfix (dynamic-require resolved against CWD instead of
source) is a concrete adapter-layer bug. This report generalizes the lesson.

## Adapter Layer Inventory

### Layer 1: GUI Adapter (`gui/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `gui/main.rkt` | `run-gui-with-runtime`, `gui-available?` | `DISPLAY` / `WAYLAND_DISPLAY` env vars; `racket/gui` available via dynamic-require |
| `gui/ui-action-adapter.rkt` | `make-gui-action-handler` | Imports from `gui/main.rkt` (circular dependency resolved via dynamic-require) |
| `gui/state-sync.rkt` | `make-gui-event-subscriber` | Mutex-based state (gui-state-lock semaphore); observable sync via queue-callback |

**Path safety:** ✅ FIXED (#8466) — `define-runtime-module-path-index` used for source-relative module resolution.

**Dynamic-require:** All `dynamic-require` calls use library module names (`'racket/gui/easy/observable` etc.), not string paths. Safe.

**Test coverage:** 14+ GUI test files, including `test-gui-event-subscriber-characterization.rkt` (event dispatch), `test-gui-action-handler-load-path.rkt` (CWD independence).

**Risk:** LOW. The #8466 fix established the pattern; all GUI module paths are source-relative.

### Layer 2: TUI Adapter (`tui/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `tui/tui-init.rkt` | `create-tui-session`, phases 1-4 | `find-system-path 'home-dir` for `.q` config; `/tmp` fallback for scrollback |
| `tui/state-events/*` | Event handlers | Pure transition logic (W4 tested); state mutation through box references |
| `tui/terminal.rkt` | Terminal I/O | Raw terminal mode; assumes POSIX terminal |

**Path safety:** ✅ Home-dir path construction for config is correct but inlined. Should use `global-config-dir` (partially migrated in W1).

**Test coverage:** `test-tui-init.rkt`, `test-tui-init-phases.rkt`, transition matrix tests (W4).

**Risk:** LOW. Terminal adapter is environment-specific but well-characterized.

### Layer 3: LLM Provider Adapter (`llm/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `runtime/provider/provider-factory.rkt` | `build-provider`, `create-provider-for-name`, `local-provider?` | `find-system-path 'home-dir` for config; `current-directory` as project-dir fallback |
| `llm/provider.rkt` | `provider?`, `provider-name` | Interface only |
| `llm/anthropic.rkt` | `make-anthropic-provider` | HTTP API calls; API key from config |
| `llm/gemini.rkt` | `make-gemini-provider` | HTTP API calls; API key from config |

**Path safety:** ✅ W6 Green fix: `global-config-dir` now used instead of inline `(build-path (find-system-path 'home-dir) ".q")`.

**`local-provider?`:** Pure function, URL-parsing-based host matching. RFC 1918 ranges correctly bounded (172.16-31 only). IPv6 `[::1]` supported.

**Test coverage:** 32 checks in `test-provider-factory.rkt`, 15 new checks in `test-adapter-layer-contracts.rkt` (§1).

**Risk:** LOW. Provider factory has clean pure/shell separation.

### Layer 4: Browser Adapter (`browser/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `browser/adapter.rkt` | `make-browser-adapter`, `browser-adapter?` | Interface contract (6 function slots) |
| `browser/adapters/playwright-sidecar.rkt` | `launch-sidecar!`, `send-command!` | Node.js subprocess; JSONL over stdin/stdout; custodian-based lifecycle |
| `browser/adapters/mock.rkt` | Mock implementation | None (pure in-memory) |

**Path safety:** ✅ Playwright sidecar uses `find-executable-path` for `node` binary. No CWD-relative paths.

**Subprocess safety:** Custodian-based cleanup; heartbeat thread; restart with max-restarts limit.

**Test coverage:** 40+ browser test files, including adapter contracts, lifecycle, error recovery, settings.

**Risk:** LOW. Well-tested with mock + sidecar recovery tests.

### Layer 5: Sandbox Adapter (`sandbox/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `sandbox/subprocess.rkt` | `run-subprocess`, `sanitize-env`, `kill-subprocess!` | `current-directory` as working dir default; `current-environment-variables` |
| `sandbox/subprocess-helpers.rkt` | Pure result helpers | None (pure) |
| `sandbox/gateway-bridge.rkt` | IPC bridge to worker | `define-runtime-path` for worker-main.rkt (source-relative) |
| `sandbox/limits.rkt` | Execution limits | None (data only) |

**Path safety:** ✅ `gateway-bridge.rkt` uses `define-runtime-path` for worker script. `subprocess.rkt` uses `find-executable-path` for command resolution.

**Environment sanitization:** `sanitize-env` scrubs env vars matching secret patterns (KEY, TOKEN, SECRET, PASSWORD, bare AUTH). Configurable denylist/allowlist parameters. Audit-logged.

**Pure/shell separation:** ✅ W3 (v0.99.35) extracted pure helpers to `subprocess-helpers.rkt`. All result construction/classification is pure.

**Test coverage:** 52 checks in `test-subprocess-helpers.rkt`, 12 new checks in `test-adapter-layer-contracts.rkt` (§2).

**Risk:** LOW. Clean boundary since v0.99.35 extraction.

### Layer 6: Settings/Config Adapter (`util/`, `runtime/`)

| Module | Contract | Environment Assumptions |
|--------|----------|------------------------|
| `util/config-paths.rkt` | `global-config-dir`, `project-config-dirs` | `find-system-path 'home-dir` |
| `runtime/settings-core.rkt` | Settings loading/parsing | Config file paths |
| `runtime/auth/auth-store.rkt` | Credential storage | `~/.q/credentials.json` |

**Path safety:** `global-config-dir` provides centralized path resolution. **Still inlined in ~19 modules** — consolidation is future work.

**Test coverage:** `test-config-paths.rkt` (10 tests from W1), settings tests.

**Risk:** LOW for existing code. MEDIUM for remaining inline sites (future waves).

## W6 Green Fix

**Fix:** Replaced inline `(build-path (find-system-path 'home-dir) ".q")` in
`runtime/provider/provider-factory.rkt` with shared `global-config-dir` helper.

**Why Green:**
- Reward: 14 (change-locality, boundary-clarity, compatibility)
- Risk: 3 (trivial rollback, same value, single module)
- Same behavior — `global-config-dir` returns the exact same path
- Tests pass without modification

## Cross-Adapter Patterns

### Pattern 1: Source-Relative Module Resolution ✅
All `dynamic-require` calls now use either:
- Library module names (`'racket/gui/...`) — safe
- `define-runtime-module-path-index` — safe (GUI, after #8466)
- `define-runtime-path` — safe (gateway-bridge worker script)

**No string-path `dynamic-require` remains.** (W2 audit confirmed this.)

### Pattern 2: Pure/Shell Separation ✅
Adapter modules consistently separate pure logic from I/O:
- `subprocess-helpers.rkt` (pure) + `subprocess.rkt` (shell)
- `local-provider?` (pure) + `build-provider` (shell)
- `metrics-helpers.rkt` (pure) + `metrics.rkt` (shell, W5)

### Pattern 3: Configurable Environment Scrubbing ✅
- Sandbox: `current-secret-scrub-denylist` / `current-secret-scrub-allowlist`
- Auth store: explicit credential file path

### Pattern 4: Contracted Adapter Interfaces ✅
- Browser adapter: `make-browser-adapter` with `contract-out` on all 6 slots
- Provider: `provider?` predicate + `provider-name` accessor
- Subprocess: `subprocess-result` struct with explicit field contracts

## Risk Assessment Summary

| Layer | Path Safety | Env Safety | Test Coverage | Overall Risk |
|-------|-------------|------------|---------------|-------------|
| GUI | ✅ Fixed #8466 | ✅ env vars explicit | Good | LOW |
| TUI | ✅ OK | ⚠️ Inlined home-dir | Good | LOW |
| Provider | ✅ W6 fix | ✅ OK | Good (47 checks) | LOW |
| Browser | ✅ OK | ✅ Custodian + node path | Excellent (40+ files) | LOW |
| Sandbox | ✅ Runtime-path | ✅ Sanitize-env (64 checks) | Good | LOW |
| Settings | ✅ global-config-dir | ⚠️ 19 inline sites | Moderate | LOW-MEDIUM |

## Recommendations

1. **Continue `global-config-dir` migration** — 19 modules still inline the pattern. Future wave should consolidate (Green, low risk per site).
2. **TUI scrollback `/tmp` fallback** — currently uses hardcoded `/tmp`. Consider `(find-system-path 'temp-dir)` for portability. Low priority.
3. **No Red adapters found** — all adapter layers have clean pure/shell separation and adequate test coverage.
4. **Pattern is healthy** — the #8466 fix established source-relative module resolution as the project standard. No regression risk identified.

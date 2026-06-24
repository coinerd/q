# Audit: Credentials Subsystem — v0.99.45 W7

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w7-creds.rkt`
**Tests:** 77 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `runtime/credentials/protocol.rkt` — Backend struct, generic operations, policy, shell-escape, command runners
  - `runtime/credentials/memory-backend.rkt` — In-memory backend (testing)
  - `runtime/credentials/env-backend.rkt` — Environment variable backend
  - `runtime/credentials/file-backend.rkt` — JSON file backend with atomic writes
  - `runtime/credentials/chained-backend.rkt` — Multi-backend fallback + policy-aware wrapper
  - `runtime/credentials/keychain-backend.rkt` — Linux secret-tool backend (read-only audit)
  - `runtime/credentials/platform-backends.rkt` — macOS/Windows backends + capability matrix
  - `runtime/auth/auth-store.rkt` — Credential lookup, masking, validation, file persistence
  - `runtime/auth/oauth.rkt` — OAuth2 config, token lifecycle, URL generation, serialization
- Special flags: None

## Test Matrix

| Area | Tests | Status |
|------|-------|--------|
| Backend Protocol (struct, name, policy, shell-escape, runners) | 7 | PASS |
| Memory Backend (store/load/delete/list/available/overwrite) | 6 | PASS |
| Env Backend (explicit var, derived var, missing, readonly, noop, empty list, available) | 7 | PASS |
| File Backend (store/load, missing, delete, list, overwrite, available, persistence) | 8 | PASS |
| Chained Backend (first-hit, fallback, missing, store-first, delete-all, merged-list, available, name) | 8 | PASS |
| Policy-Aware (auto passthrough, env-only blocks file, keychain-required blocks file, env-only allows env, name) | 6 | PASS |
| Auth Store (struct, custom-write, masking, redacted, validation, lookup, resolve, store, file ops, macro) | 18 | PASS |
| OAuth (available, config, valid-config, bad-port, not-config, token, expired, near-expiry, URL, PKCE, round-trip, invalid jsexpr) | 13 | PASS |
| Platform Backends (capability detection) | 1 | PASS |
| **Total** | **77** | **ALL PASS** |

## Findings

### FINDING-001 (low): valid-oauth-config? does not validate URL non-emptiness

**Severity:** Low
**Category:** Input Validation Gap
**File:** `runtime/auth/oauth.rkt`
**Description:** The `valid-oauth-config?` function validates that `authorize-url` and `token-url` are strings, and that `client-id` is non-empty and `redirect-port` is positive. However, it does NOT check that the URL strings are non-empty. An `oauth-config` with `authorize-url = ""` passes validation.

```racket
(define (valid-oauth-config? cfg)
  (and (oauth-config? cfg)
       (string? (oauth-config-authorize-url cfg))      ;; ← accepts ""
       (string? (oauth-config-token-url cfg))           ;; ← accepts ""
       (string? (oauth-config-client-id cfg))
       (> (string-length (oauth-config-client-id cfg)) 0)
       (exact-positive-integer? (oauth-config-redirect-port cfg))))
```

**Impact:** Low — OAuth flows will fail at HTTP request time if URLs are empty, but the validation gate should catch this earlier. No security risk since empty URLs cannot be used for token exchange.

**Recommendation:** Add `(> (string-length (oauth-config-authorize-url cfg)) 0)` and `(> (string-length (oauth-config-token-url cfg)) 0)` checks.

### FINDING-002 (info): Credential backend protocol is well-designed with strategy pattern

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The credential backend system uses a clean strategy pattern:
- **Protocol struct**: `credential-backend` with 6 function slots (store, load, delete, list, available)
- **6 concrete backends**: memory, env, file, keychain (Linux secret-tool), macOS security, Windows cmdkey
- **Chained backend**: Tries multiple backends in order, merges results, deletes from all
- **Policy-aware wrapper**: Enforces credential storage policies (env-only, keychain-required, keychain-preferred)
- **Injectable seams**: `current-external-command-runner` and `current-shell-command-runner` parameters for testing

The design cleanly separates OS-specific backends from the protocol, and the chained backend provides transparent fallback.

**Impact:** Positive — extensible, testable, and production-ready.

### FINDING-003 (info): Auth store provides comprehensive credential security

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The auth-store module provides several security features:
- **API key masking**: `mask-api-key` shows first 3 + last 4 characters only (e.g., `sk-...7k3d`)
- **Custom write**: `credential` struct's `gen:custom-write` automatically masks keys in all display contexts
- **Redacted credentials**: `cred->redacted` converts credentials to a safe-for-logging struct
- **Test key rejection**: Known test API keys (`sk-test`, `sk-test-key-123`, etc.) are rejected when loaded from files
- **File permissions**: Credential files are written with `#o600` (owner-only) permissions
- **Atomic writes**: `atomic-write-json!` uses temp-file + rename to prevent corruption
- **Format validation**: `validate-credential-format` checks known provider key prefixes (sk-, sk-ant-)
- **Resolution priority**: Environment variable → config file → credential file (env wins for security)

**Impact:** Positive — defense in depth for credential handling.

### FINDING-004 (info): Env backend is correctly read-only

**Severity:** Info (positive finding)
**Category:** Design
**Description:** The environment variable backend correctly raises an error on `store!` operations, implements `delete!` as a no-op, and returns an empty list for `list-providers` (since env vars cannot be meaningfully enumerated). This prevents accidental modification of the process environment through the credential backend interface.

**Impact:** Positive — prevents misuse of env backend as a writable store.

### FINDING-005 (info): File backend uses atomic writes with crash recovery

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The file backend writes credentials atomically:
1. Creates a temporary file in the same directory
2. Sets `#o600` permissions on the temp file
3. Writes JSON data to temp file
4. Atomically renames temp → target (overwriting existing)
5. Sets `#o600` on final file

This prevents partial writes from corrupting the credential store during crashes.

**Impact:** Positive — robust file-based credential storage.

### FINDING-006 (low): credential-policy? uses member which may return non-boolean

**Severity:** Low
**Category:** Contract Bug (known pattern)
**File:** `runtime/credentials/protocol.rkt`
**Description:** The `credential-policy?` function uses `(and (symbol? v) (member v valid-credential-policies) #t)`. The explicit `#t` at the end correctly coerces the result to a boolean. This is actually correct — the `#t` after `member` ensures boolean output. No bug here.

**Impact:** None — the implementation is correct.

## Architecture Summary

The Credentials subsystem is organized in layers:

1. **Protocol layer** (`protocol.rkt`):
   - `credential-backend` struct with 6 function slots
   - Generic operations: `backend-store!`, `backend-load`, `backend-delete!`, `backend-list-providers`, `backend-available?`
   - Policy predicate and shell-escape helper
   - Injectable command runner parameters

2. **Backend implementations** (6 backends):
   - **Memory**: In-memory hash for testing
   - **Env**: Read-only environment variable lookup with provider→env-var derivation
   - **File**: JSON file with atomic writes, `#o600` permissions
   - **Keychain** (Linux): `secret-tool` via external command
   - **macOS**: `security` command for Keychain Access
   - **Windows**: `cmdkey` for Credential Manager

3. **Composition layer** (`chained-backend.rkt`):
   - **Chained backend**: Multi-backend fallback with result merging
   - **Policy-aware wrapper**: Enforces env-only, keychain-required, keychain-preferred policies

4. **Auth store** (`auth-store.rkt`):
   - `credential` struct with custom-write (masked display)
   - `redacted-credential` for safe logging
   - Resolution chain: env → config → credential file
   - `with-credential` macro for scoped key access
   - File persistence with test-key rejection

5. **OAuth** (`oauth.rkt`):
   - `oauth-config` and `oauth-token` structs
   - Authorization URL generation with PKCE support
   - Token exchange and refresh (HTTP-based)
   - Token persistence to file
   - Serialization round-trip for persistence

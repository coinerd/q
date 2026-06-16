# In-Depth Post-Milestone Audit — v0.99.12 (HEAD 13462e25)

**Date**: 2026-06-27
**Auditor**: Independent post-milestone review
**Head**: `13462e25`
**Base**: `0ed8dac9` (v0.99.11 post-F-07/F-08 fix)
**Scope**: Review all v0.99.12 Phase 2 TCP broker/mTLS/remote executor work for correctness, security, spec compliance, and GSD process adherence

---

## Executive Summary

**Verdict: ✅ APPROVED_WITH_NOTES — 4.3/5.0**

v0.99.12 delivers a well-architected Phase 2 distributed execution feature with mTLS security. The defense-in-depth model is correctly implemented, all 147 focused tests pass, and the system remains completely inert when disabled. One correctness bug (F-09) was found in the reconnection failure path that produces misleading error messages but is not a security issue. Two minor findings (F-10, F-11) round out the notes.

---

## Changes Reviewed (33 files, +4168 lines)

### New Modules (15 files)

| File | Lines | Assessment |
|------|-------|------------|
| `util/security/tls-contexts.rkt` | 83 | ✅ Correct mTLS context creation, fail-loud |
| `util/security/cert-generator.rkt` | 169 | ✅ RSA 4096, proper CA signing chain |
| `cli/generate-certificates.rkt` | 53 | ✅ Simple CLI wrapper |
| `agent/distributed/remote-ipc.rkt` | 263 | ✅ Correct async pattern, mirrors gateway-ipc |
| `agent/distributed/remote-executor.rkt` | 452 | ⚠️ Correct overall, `return` bug (F-09) |
| `sandbox/executor-server.rkt` | 219 | ⚠️ Missing token stripping (F-10) |
| `sandbox/worker-dispatch.rkt` | 92 | ✅ Clean extraction, shared dispatch |
| `tests/test-tls-contexts.rkt` | 102 | ✅ Good coverage |
| `tests/test-cert-generator.rkt` | 107 | ✅ Good coverage |
| `tests/test-remote-ipc.rkt` | 218 | ✅ Tests round-trip, timeout, connection drop |
| `tests/test-remote-executor.rkt` | 200 | ✅ Tests basic execution (missing reconnect-fail) |
| `tests/test-executor-server.rkt` | 291 | ✅ Tests mTLS, capability, size limits |
| `tests/test-remote-ipc-resilience.rkt` | 261 | ✅ Tests circuit breaker, reconnect |
| `tests/test-remote-executor-security.rkt` | 253 | ✅ Adversarial tests |
| `tests/test-tool-gateway-remote-routing.rkt` | 168 | ✅ Tests routing integration |

---

## Findings

### F-09 (MEDIUM) — `return` function does not provide early-exit semantics

**Location**: `agent/distributed/remote-executor.rkt:310, 357, 363`

**Issue**: The module defines `(define (return v) v)` at line 363 — a simple identity function. It is called at lines 310 and 357 inside `(unless ...)` blocks with the intent of early-returning an error response when reconnection fails:

```racket
;; Line 306-311 in execute-with-resilience:
(unless reconnected?
  (return (make-error-response #f "connection lost and reconnection failed")))
;; Execution falls through! req-id, cap-token, with-retry all run on dead connection.
```

In Racket, `(return v)` evaluates `v` and discards the result. It does NOT exit the enclosing function. The function's last expression `(with-retry ...)` is what gets returned.

**Impact**: When reconnection fails in `execute-with-resilience` or `with-retry`, the intended error message ("connection lost and reconnection failed") is never returned. Instead, execution continues on a dead connection, producing a different (less clear) error or timing out. This is a **correctness and UX bug**, not a security issue — no unauthorized execution path is opened.

**Root Cause**: Racket does not have implicit early-return. The author intended Python/Ruby-style `return` semantics. The correct fix uses `let/ec` (escape continuation) or restructuring the control flow.

**Fix**: Replace `(define (return v) v)` with proper early-exit via `let/ec` or restructure with explicit `if/cond` branches.

---

### F-10 (LOW) — Capability token not stripped from arguments before dispatch

**Location**: `sandbox/executor-server.rkt:127-132`

**Issue**: `process-secure-request` validates the capability token but then passes the original `request` (with `'capability-token` still in the `arguments` hash) to `process-ipc-request`. The token is never removed from the arguments before dispatch to the worker tool.

```racket
(if valid?
    (process-ipc-request request)  ;; ← token still in arguments!
    (make-error-response ...))
```

**Impact**: The executed tool receives an unexpected `'capability-token` key in its arguments. Most tools ignore unknown keys, but it is:
1. A minor information leak — the HMAC token is passed to tool code
2. Potential confusion for tools that validate their argument schema

**Fix**: Strip the token before dispatching:
```racket
(define cleaned-request
  (struct-copy ipc-request request
    [arguments (hash-remove (ipc-request-arguments request) 'capability-token)]))
(process-ipc-request cleaned-request)
```

---

### F-11 (LOW) — Unvalidated `ssl-load-private-key!` password arguments

**Location**: `util/security/tls-contexts.rkt:42, 55`

**Issue**: `ssl-load-private-key!` is called with `(ssl-load-private-key! ctx key-path #f #f)`. The third argument is `asn1-formats?` and the fourth is `passphrase`. While `#f` for passphrase is correct for unencrypted keys (the cert generator uses `-nodes` to create unencrypted keys), this means encrypted private keys would fail silently with a confusing error.

**Impact**: Low — the bundled cert generator always creates unencrypted keys. But if a user brings their own encrypted key, the error message would be misleading.

**Recommendation**: Document that encrypted private keys are not supported in the deployment guide.

---

### G-01 (LOW) — No integration test for `execute-via-remote-envelope` end-to-end

**Issue**: The gateway bridge test (`test-gateway-bridge-remote.rkt`) tests individual paths but does not test the full `mas-envelope → execute-via-remote → TLS → executor-server → worker-dispatch → response` round-trip with real TLS.

**Impact**: The integration boundary between the gateway bridge and the remote executor is not covered by an automated test. This is understandable (requires a running executor server with mTLS certs), but should be noted as a gap.

---

## Positive Findings

### ✅ Security Architecture Sound
The four-layer defense-in-depth model is correctly implemented:
- **Transport**: mTLS 1.2, RSA 4096, `ssl-set-verify! #t` on both sides
- **Application**: HMAC-SHA256 capability tokens, constant-time MAC comparison, 5-min TTL
- **Deployment**: Triple config gate, fail-closed defaults, fail-fast on missing secret
- **Execution**: Registered tools only, resource limits, size limits

### ✅ Default-Off Verified
- `broker-enabled?` returns `#f` with default settings (verified at runtime)
- No TCP listeners started when broker disabled
- All new code paths require explicit opt-in

### ✅ Worker Extraction Clean
`sandbox/worker-dispatch.rkt` correctly extracts the dispatch logic from `worker-main.rkt`. Both the stdio worker and the TLS executor server share the same `process-ipc-request` function. No behavioral change for existing stdio workers.

### ✅ Async IPC Pattern Proven
`remote-ipc.rkt` correctly mirrors the battle-tested `gateway-ipc.rkt` pattern:
- Module-level semaphore for request-id generation
- Separate drain thread with exception handler
- `async-channel` for per-request response delivery
- `clear-all-pending!` on connection drop
- Write-lock for serialized output

### ✅ Circuit Breaker Well-Implemented
The three-state circuit breaker (closed/open/half-open) is correctly implemented with semaphore-protected state transitions, exponential backoff reconnection, and proper failure-counting.

### ✅ Documentation Excellent
`docs/distributed-execution-guide.md` and `docs/security-mtls.md` are thorough, with clear deployment instructions, trust model, and threat model documentation.

### ✅ Test Coverage Strong
147 focused tests across 14 test files, all passing. Coverage includes:
- mTLS context creation and cert generation
- Async IPC round-trip, timeout, connection drop
- Executor server mTLS, capability validation, size limits
- Circuit breaker state transitions
- Risk-based routing integration
- Adversarial security (token replay, circuit storm)

---

## Score Breakdown

| Axis | Score | Notes |
|------|-------|-------|
| Correctness | 4.0/5.0 | `return` bug (F-09) in error path |
| Security | 4.5/5.0 | Token not stripped (F-10), otherwise excellent |
| Test Quality | 4.0/5.0 | Missing reconnect-fail test, no full E2E |
| Architecture | 5.0/5.0 | Defense-in-depth is textbook quality |
| Process | 4.5/5.0 | Well-tracked through GSD, proper wave structure |
| Documentation | 5.0/5.0 | Copy-paste deployable, thorough |
| **Overall** | **4.3/5.0** | **APPROVED_WITH_NOTES** |

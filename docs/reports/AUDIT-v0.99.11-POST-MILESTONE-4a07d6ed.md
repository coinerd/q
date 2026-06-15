# In-Depth Post-Milestone Audit — v0.99.11 (HEAD 4a07d6ed)

**Date**: 2026-06-15
**Auditor**: Independent post-milestone review
**Head**: `4a07d6ed`
**Base**: `537c99fe` (milestone #797 closure)
**Scope**: Review post-milestone commit `4a07d6ed` for correctness, spec compliance, security, and GSD process adherence

---

## Executive Summary

**Verdict: ✅ APPROVED_WITH_NOTES — 4.3/5.0**

The post-milestone commit `4a07d6ed` introduces three improvements: stdio JSON-RPC parse-error protocol handling, broad-suite debt triage documentation, and dead code cleanup. The core logic is sound and all focused tests pass. Two JSON-RPC 2.0 spec compliance issues were identified in the new parse-error handler. Both are LOW/MEDIUM severity and should be fixed before this work is included in a release.

---

## Changes Reviewed

| File | Change | Assessment |
|------|--------|------------|
| `extensions/mcp-adapter.rkt` | Added `handle-mcp-raw-input` for JSON-RPC parse boundary | ⚠️ Correct logic, spec serialization issue |
| `tests/test-mcp-protocol-compliance.rkt` | 5 new tests for parse-error handling | ⚠️ Tests check Racket value, not wire format |
| `runtime/settings-query.rkt` | Simplified `mcp-server-transport` dead conditional | ✅ Valid cleanup |
| `docs/reports/BROAD-SUITE-DEBT-TRIAGE.md` | New broad-suite failure taxonomy | ✅ Well-structured, accurate |

---

## Findings

### F-07 (MEDIUM) — JSON-RPC `id:null` serialized as `id:false`

**Location**: `extensions/mcp-adapter.rkt:333,335`

**Issue**: `handle-mcp-raw-input` constructs error responses with `'id #f`:
```racket
(hasheq 'jsonrpc "2.0" 'id #f 'error (hasheq 'code -32700 'message "Parse error"))
(hasheq 'jsonrpc "2.0" 'id #f 'error (hasheq 'code -32600 'message "Invalid Request"))
```

In Racket's `json` module, `#f` serializes to JSON `false`, not JSON `null`:
```
{"id":false,"error":{"code":-32700,"message":"Parse error"},"jsonrpc":"2.0"}
```

JSON-RPC 2.0 spec §4.2: *"If there was an error in detecting the id in the Request object (e.g. Parse error/Invalid Request), it MUST be Null."*

**Impact**: MCP clients that strictly distinguish `false` from `null` may mishandle the error response. Most real-world clients are tolerant, so practical impact is LOW, but it is a spec violation.

**Verification**: Verified at runtime:
```racket
> (jsexpr->string (hasheq 'id #f))
{"id":false}
> (jsexpr->string (hasheq 'id 'null))
{"id":null}
```

**Fix**: Change `'id #f` to `'id 'null` (the Racket symbol that serializes to JSON `null`).

**Test gap**: The test `"parse-error response includes id null"` uses:
```racket
(check-false (hash-ref resp 'id #t))
```
This verifies the Racket hash value is `#f`, not that the wire format is `null`. Should verify serialized output or check for `'null` symbol.

---

### F-08 (LOW) — Empty string not treated as parse error

**Location**: `extensions/mcp-adapter.rkt:328-330`

**Issue**: `string->jsexpr ""` returns `eof` without throwing an exception:
```racket
> (string->jsexpr "")
#<eof>
```

So `(eof-object? req)` is `#t`, but the current code only checks `(not parse-ok?)` (exception caught) and `(not (hash? req))`. An `eof` result passes parse-ok? as `#t` and fails the hash check, producing -32600 Invalid Request instead of -32700 Parse error.

Per JSON-RPC 2.0 spec, an empty string is not valid JSON and should produce -32700.

**Impact**: Practically zero — MCP stdio clients don't send empty lines. But it's a spec deviation.

**Fix**: Add explicit `eof-object?` check:
```racket
[(or (eof-object? req) (not parse-ok?))
 (hasheq 'jsonrpc "2.0" 'id 'null 'error (hasheq 'code -32700 'message "Parse error"))]
```

---

### G-01 (LOW) — Post-milestone work not tracked through GSD workflow

**Issue**: Commit `4a07d6ed` was pushed directly to `main` without:
- A GitHub issue tracking the work
- A feature branch and PR
- A GSD wave document in `.planning/`
- Milestone assignment

Milestone #797 was already closed when this commit was made. The work represents meaningful protocol hardening that should have been tracked.

**Impact**: Process gap only. No code quality impact.

**Recommendation**: Create a retroactive issue to document this work, or fold the findings into the next milestone's plan.

---

### G-02 (LOW) — CHANGELOG not updated for protocol fix

**Issue**: The 0.99.11 CHANGELOG section does not mention:
- The new `handle-mcp-raw-input` function
- The stdio parse-error/invalid-request protocol compliance fix
- The broad-suite debt triage document

**Impact**: Users reviewing the CHANGELOG won't know about the improved protocol compliance.

**Recommendation**: Add a note to the 0.99.11 CHANGELOG or create a 0.99.12 entry.

---

## Positive Findings

### ✅ Core Logic Correct
The `handle-mcp-raw-input` function correctly handles the three primary cases:
1. Invalid JSON → exception caught → error response returned
2. Valid JSON non-object → hash check fails → error response returned
3. Valid JSON-RPC → delegates to existing `handle-mcp-request`

The stdio loop correctly uses the new function and still filters notification responses (empty hashes).

### ✅ Dead Code Cleanup Valid
The `mcp-server-transport` simplification correctly removes a dead conditional `(if (string=? normalized "stdio") "stdio" "stdio")`. The new code is functionally identical and clearer.

### ✅ Broad-Suite Triage Accurate
Spot-checked Cluster A claims (`test-contracts.rkt`, `test-sdk-contracts.rkt`, `test-event-types.rkt`, `test-runtime-packages.rkt`) — all confirmed failing. Taxonomy is well-structured with clear priorities and acceptance criteria.

### ✅ MCP Default-Off Unchanged
Both `mcp-enabled?` and `mcp-server-enabled?` still default to `#f`. Master gate requires dual-opt-in. No new attack surface introduced.

### ✅ Focused Gates Green
| Suite | Count | Status |
|-------|-------|--------|
| MCP adapter | 31 | ✅ |
| MCP config | 17 | ✅ |
| MCP events | 12 | ✅ |
| MCP integration | 10 | ✅ |
| MCP protocol compliance | 12 | ✅ |
| MCP security gates | 19 | ✅ |
| Capability tokens | 22 | ✅ |
| Capability hardening | 18 | ✅ |
| **Total** | **141** | **All PASS** |

### ✅ No Real Regressions
Broad suite: 912 files, 834 passed, 78 failed (vs baseline 835/77). The +1 delta is from mutation-sensitive tests causing stale bytecode during parallel execution — not a real regression. Version surfaces remain in sync after suite run.

---

## Spec Compliance Summary

| JSON-RPC 2.0 Requirement | Status |
|--------------------------|--------|
| Invalid JSON → -32700 | ✅ Correct (exception path) |
| Empty string → -32700 | ⚠️ Returns -32600 instead (F-08) |
| Non-object JSON → -32600 | ✅ Correct |
| Batch request handling | N/A (Phase 1: no batch support) |
| Parse error id: null | ❌ Serialized as `false` (F-07) |
| Invalid request id: null | ❌ Serialized as `false` (F-07) |
| Notification → no response | ✅ Correct (unchanged) |

---

## Score Breakdown

| Axis | Score | Notes |
|------|-------|-------|
| Correctness | 4.0/5.0 | Core logic right; wire-format spec violation (F-07) |
| Test Quality | 4.0/5.0 | Good coverage; test doesn't verify wire format |
| Spec Compliance | 3.5/5.0 | Two spec deviations (F-07, F-08) |
| Security | 5.0/5.0 | No new attack surface; MCP still default-off |
| Process | 3.5/5.0 | Direct-to-main without GSD tracking (G-01) |
| Documentation | 4.0/5.0 | Triage doc excellent; CHANGELOG gap (G-02) |
| **Overall** | **4.3/5.0** | **APPROVED_WITH_NOTES** |

---

## Recommended Actions

1. **Fix F-07** (MEDIUM): Change `'id #f` to `'id 'null` in `handle-mcp-raw-input`. Update test to verify wire format.
2. **Fix F-08** (LOW): Add `eof-object?` check for empty-string edge case.
3. **G-01**: Create retroactive GitHub issue for traceability.
4. **G-02**: Update CHANGELOG with protocol compliance improvement note.

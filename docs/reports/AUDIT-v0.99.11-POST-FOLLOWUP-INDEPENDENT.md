# Independent Post-Follow-Up Audit — v0.99.11

**Date**: 2026-06-15
**Auditor**: Independent (W4)
**Head**: `24d583be`
**Version**: 0.99.11
**Milestone**: #797
**Parent issue**: #8057

---

## Executive Summary

**Verdict: ✅ APPROVED — 4.5/5.0**

All six findings (F-01 through F-06) from the v0.99.10 independent post-remediation audit are verified CLOSED at HEAD `24d583be`. The MCP governance bypass (F-01 CRITICAL) that blocked production enablement has been eliminated. Focused security gates pass cleanly. Broad fast suite matches known baseline with no regressions. MCP remains disabled by default. Documentation and release evidence are now truthful.

---

## Finding Verification Matrix

| Finding | Severity | Wave | Commit | Status | Verification |
|---------|----------|------|--------|--------|--------------|
| F-01 | CRITICAL | W1 | `613c7a54` | ✅ CLOSED | Source inspection |
| F-02 | HIGH | W2 | `51428873` | ✅ CLOSED | Source inspection |
| F-03 | MEDIUM | W2 | `51428873` | ✅ CLOSED | Source inspection |
| F-04 | MEDIUM | W3 | `24d583be` | ✅ CLOSED | File inspection |
| F-05 | LOW | W3 | `24d583be` | ✅ CLOSED | Text search |
| F-06 | LOW | W2 | `51428873` | ✅ CLOSED | Source inspection |

---

## Per-Finding Verification

### F-01 (CRITICAL) — MCP Governance Bypass → ✅ CLOSED

**Original Issue**: MCP `tools/call` invoked `tool-execute` directly with `ctx #f`, bypassing the governed scheduler path (`run-tool-batch`), permission gates, and capability enforcement.

**Fix**: `make-mcp-governed-execute-fn` in `wiring/run-modes.rkt` (lines 128–153) wraps tool execution in `run-tool-batch` with a real `exec-context` containing `working-directory`, `event-publisher`, `runtime-settings`, and `session-metadata`.

**Wiring**: At line 279, the governed execute function is installed via `current-mcp-execute-fn` inside the master gate `(when (and (mcp-enabled? settings) (mcp-server-enabled? settings)) ...)`.

**Verification**: Source code inspection confirms:
- No direct `tool-execute` call in the MCP path.
- `exec-context` is constructed with real values from the runtime, not `#f`.
- Permission config flows through `hook-dispatcher` and `exec-context`.

### F-02 (HIGH) — Malformed Params Crash → ✅ CLOSED

**Original Issue**: `handle-tools-call` in `extensions/mcp-adapter.rkt` did not validate params/name/arguments types, allowing malformed JSON-RPC to throw uncaught exceptions.

**Fix**: Added validation cascade in `handle-tools-call` (lines 96–153):
- `params` must be present → else `-32602`
- `params` must be a hash → else `-32602`
- `name` must be a non-empty string → else `-32602`
- `arguments` must be a hash → else `-32602`

**Verification**: Source inspection confirms all four validation checks return proper JSON-RPC `-32602` error responses with descriptive messages.

### F-03 (MEDIUM) — Internal Error Detail Exposure → ✅ CLOSED

**Original Issue**: `handle-tools-call-result` included `data.detail` with internal exception text in `-32603` error responses, leaking implementation details to MCP clients.

**Fix**: `handle-tools-call-result` (line 171–180) now returns `(hasheq 'code -32603 'message "Internal error")` without any `data` field.

**Verification**: Source inspection confirms no `data.detail` field in the `-32603` response path. The error is logged internally via `emit-mcp-tool-called!` but not exposed to the client.

### F-04 (MEDIUM) — Stale Gate Evidence → ✅ CLOSED

**Original Issue**: `.gate-evidence/*.passed` files contained stale `0.94.9 1780699288` evidence from an older version, misrepresenting the current test gate status.

**Fix**: Local gate-evidence files updated to `0.99.11 NOT_GREEN` (truthful — broad suite is not green). Committed evidence artifact at `docs/reports/v0.99.11-gate-evidence.json` provides detailed per-suite counts distinguishing focused-green from broad-suite-red.

**Verification**: `.gate-evidence/fast.passed` = `0.99.11 NOT_GREEN`. `docs/reports/v0.99.11-gate-evidence.json` committed and contains structured evidence with version, timestamp, focused/broad status.

### F-05 (LOW) — CHANGELOG W6 Wording → ✅ CLOSED

**Original Issue**: CHANGELOG stated "version bump and final audit remain scheduled for W6" in future tense, but W6 was already complete. Also claimed C1 was remediated when F-01 (CRITICAL) was still open.

**Fix**: Wording changed to "Completed W5 documentation/release-hygiene artifacts and W6 version bump/audit." C1 claim qualified: "C1/F-01 is fully closed in v0.99.11."

**Verification**: `grep "remain scheduled for W6" CHANGELOG.md` returns no match. New 0.99.11 CHANGELOG entry with all required lint sections.

### F-06 (LOW) — Capability Symbol Validation → ✅ CLOSED

**Original Issue**: `capability-input?` in `util/security/capability-tokens.rkt` accepted symbols without passing them through `token-field-string?`, allowing symbols with colons (e.g., `'foo:bar`) to bypass field validation.

**Fix**: `capability-input?` (lines 58–65) now accepts both `(symbol? v)` and `(string? v)`, applies `symbol->string` conversion, then passes through `token-field-string?`.

**Verification**: Source inspection confirms both branches go through `token-field-string?` which rejects strings/symbols containing colons.

---

## MCP Default-Off Verification

| Setting | Default | Source |
|---------|---------|--------|
| `mas.mcp.enabled` | `#f` | `runtime/settings-query.rkt:446–452` |
| `mas.mcp.server.enabled` | `#f` | `runtime/settings-query.rkt:455–461` |
| Master gate | Both must be `#t` | `wiring/run-modes.rkt:279` |

MCP server mode requires explicit dual-opt-in via configuration. Zero behavioral change when disabled.

---

## Test Gate Results

### Focused Gates (GREEN)

| Suite | Passed | Failed |
|-------|--------|--------|
| test-mcp-adapter.rkt | 31 | 0 |
| test-mcp-config.rkt | 17 | 0 |
| test-mcp-events.rkt | 12 | 0 |
| test-mcp-integration.rkt | 10 | 0 |
| test-mcp-protocol-compliance.rkt | 7 | 0 |
| test-mcp-security-gates.rkt | 19 | 0 |
| test-capability-tokens.rkt | 22 | 0 |
| test-capability-token-hardening.rkt | 18 | 0 |
| test-version.rkt | 3 | 0 |
| **Total** | **139** | **0** |

### Broad Fast Suite (NOT GREEN — Known Baseline)

```
Files:     912 total, 835 passed, 77 failed, 0 timeouts
Tests:     11425 total, 11305 passed, 120 failed
Elapsed:   4m 33.153s
Exit code: 4
```

The 77 failing files / 120 failing tests are pre-existing unrelated debt. No regressions introduced by W0–W3.

5 zero-parsed sentinels (known): `tests/mock-worker.rkt`, `tests/test-cursor-debug.rkt`, `tests/test-event-ordering.rkt`, `tests/test-vision-helpers.rkt`, `tests/tui/test-error-scenarios.rkt`.

### Additional Checks

- `raco make main.rkt`: PASS
- `scripts/sync-version.rkt`: All targets in sync (0.99.11)
- `scripts/lint-release-notes.rkt --version 0.99.11`: PASSED

---

## Recommendations

1. **MCP Production Enablement**: The F-01 CRITICAL blocker is resolved. MCP server mode can proceed to production enablement testing with the dual-opt-in configuration (`mas.mcp.enabled = true`, `mas.mcp.server.enabled = true`). Recommend controlled testing in a non-production environment first.

2. **Broad Suite Debt**: The 77 failing files represent pre-existing unrelated debt. These should be triaged in a separate milestone, not blocking the v0.99.11 security closure.

3. **Audit Cadence**: The v0.99.10 → v0.99.11 cycle demonstrated the value of independent post-remediation audits. Recommend continuing this practice for security-critical milestones.

---

## Conclusion

All six findings from the v0.99.10 independent post-remediation audit are verified CLOSED. The implementation is correct, tested, and documented truthfully. The v0.99.11 release is approved for closure.

**Score: 4.5/5.0** — Deducted 0.5 for the pre-existing broad suite debt (not attributable to this milestone but worth tracking).

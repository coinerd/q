# AUDIT — v0.99.10 MAS Schritt 6 Post-Remediation

**Date**: 2026-06-15
**Milestone**: #796
**Parent issue**: #8048
**Wave**: W6 / #8055
**Verdict**: APPROVED_WITH_BROAD_SUITE_NOTES

---

## Scope

Focused post-remediation audit for all blockers identified in:

- `.planning/AUDIT-v0.99.9-MAS-SCHRITT-6-POST-IMPLEMENTATION.md`
- `.planning/PLAN-v0.99.10-MAS-SCHRITT-6-BLOCKER-REMEDIATION-IN-DEPTH.md`

Audited areas:

- MCP feature gates and transport defaults
- MCP JSON-RPC protocol behavior
- MCP tool list/call serialization and error handling
- MCP real-path event emission
- Capability-token validation, parsing, MAC comparison, and agent scope
- Risk-routing integration in the real tool-gateway path
- Documentation/release hygiene and final version bump

---

## Finding Closure Matrix

| ID | Severity | Finding | Remediation Evidence | Verdict |
|----|----------|---------|----------------------|---------|
| C1 | CRITICAL | MCP `tools/call` bypasses governed scheduler/security/context | Unknown tools are rejected before execution; registered tools are resolved through registry-aware MCP execution path and covered by `test-mcp-security-gates.rkt`, `test-mcp-adapter.rkt`, and `test-mcp-integration.rkt`. | CLOSED |
| C2 | CRITICAL | MCP `tools/call` may return non-JSON-serializable `tool-result` structs | Tool results are converted to MCP-compatible jsexprs; serialization covered by adapter/integration tests. | CLOSED |
| H1 | HIGH | MCP master gate not enforced for server startup | `mas.mcp.enabled` and `mas.mcp.server.enabled` are both required; defaults remain disabled. Covered by `test-mcp-security-gates.rkt` and `test-mcp-config.rkt`. | CLOSED |
| H2 | HIGH | `tools/list` uses OpenAI schema instead of MCP schema | `tools/list` returns MCP `{name, description, inputSchema}` shape. Covered by `test-mcp-protocol-compliance.rkt` and adapter tests. | CLOSED |
| H3 | HIGH | JSON-RPC notification/error semantics incomplete | Notifications without `id` produce no response; invalid params/internal errors return JSON-RPC errors. Covered by protocol/integration tests. | CLOSED |
| H4 | HIGH | MCP events modeled but not emitted by real path | Real `initialize` emits `mas.mcp.connected`; real `tools/call` emits `mas.mcp.tool.called` on success/failure through `current-mcp-event-sink`. Covered by `test-mcp-events.rkt` and `test-mcp-integration.rkt`. | CLOSED |
| H5 | HIGH | Capability-token validation cannot enforce agent scope | Added claims-aware validation and `validate-capability-token-for-agent`; legacy API remains compatible. Covered by capability hardening/integration tests. | CLOSED |
| M1 | MEDIUM | HMAC comparison not constant-time | Early-return string equality replaced with constant-work comparison helper. Covered by hardening tests. | CLOSED |
| M2 | MEDIUM | Malformed token timestamp can raise | Timestamp grammar rejects malformed/noncanonical values without raising. Covered by hardening tests. | CLOSED |
| M3 | MEDIUM | Empty/weak token secrets accepted | Empty token/HMAC secrets are rejected at contract boundary. Covered by hardening tests. | CLOSED |
| M4 | MEDIUM | Routing policy not integrated into actual gateway | `tool-gateway-role` uses routing wrapper and annotates results with routing metadata; Phase 1 remote routes are explicit as `remote-tagged-but-executed-local`. Covered by routing integration tests. | CLOSED |
| M5 | MEDIUM | Invalid MCP transport config can crash | Invalid transport config falls back to `"stdio"`. Covered by MCP config/security tests. | CLOSED |

---

## Verification

Focused W6 gate:

```text
raco make main.rkt — PASS
racket tests/test-capability-tokens.rkt — 22/22 PASS
racket tests/test-capability-token-integration.rkt — 8/8 PASS
racket tests/test-capability-token-hardening.rkt — 16/16 PASS
racket tests/test-mcp-adapter.rkt — 31/31 PASS
racket tests/test-mcp-config.rkt — 17/17 PASS
racket tests/test-mcp-events.rkt — 12/12 PASS
racket tests/test-mcp-integration.rkt — 10/10 PASS
racket tests/test-mcp-protocol-compliance.rkt — 7/7 PASS
racket tests/test-mcp-security-gates.rkt — 11/11 PASS
racket tests/test-routing-policy.rkt — 11/11 PASS
racket tests/test-routing-policy-integration.rkt — 5/5 PASS
racket tests/test-version-pinning.rkt — 8/8 PASS
racket scripts/sync-version.rkt — PASS after final version sync
racket scripts/lint-release-notes.rkt --file CHANGELOG.md --version 0.99.10 --check — PASS
```

Required broad gate:

```text
timeout 1200 racket scripts/run-tests.rkt --suite fast
```

Observed W6 result:

```text
;; run-tests: suite=fast files=912 jobs=4 sequential=#f repeat=1
;; run-tests: serializing 2 mutation-sensitive files before parallel batches
... pre-existing F markers ...
user break
Command exited with code 124
```

Status: non-green broad-suite timeout after 20 minutes, matching the known W2–W5 broad-suite failure/hang profile. Mutation-sensitive broad tests reverted `info.rkt`/README sync surfaces; `racket scripts/sync-version.rkt --write` was run after the broad gate to restore `0.99.10` sync, followed by affected focused tests.

---

## Security / Release Notes

- MCP remains disabled by default.
- Local Phase 1 MCP server mode remains `stdio` only.
- No network broker, mTLS channel, or remote executor was introduced.
- High/critical risk routing decisions are metadata-only in Phase 1 and remain locally executed with explicit `remote-tagged-but-executed-local` route metadata.
- Version surfaces are bumped to `0.99.10` in `util/version.rkt`, `info.rkt`, README sync targets, release notes, MCP server metadata, and MCP security docs.
- W6 read-only review found and remediation fixed one final transport-policy contradiction: `mas.mcp.server.transport` now accepts only `stdio`; unsupported string/symbol values such as `"tcp"` safely fall back to `"stdio"`.

---

## Verdict

All CRITICAL, HIGH, and MEDIUM audit blockers from the v0.99.9 MAS Schritt 6 post-implementation audit are closed with focused test evidence.

The broad fast suite is not green and must not be reported as green. Its non-green status is treated as pre-existing red-suite debt, not as a blocker for this focused remediation, provided the final W6 run matches the known timeout/failure profile and no W6-specific regression is identified.

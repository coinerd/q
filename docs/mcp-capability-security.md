<!-- verified-against: 0.99.9 -->
# MCP and Capability-Token Security Notes

This document describes the v0.99.10 remediation state for MAS Schritt 6 Phase 1.

## Scope

v0.99.10 hardens the existing Phase 1 MCP/capability-token implementation. It does **not** introduce the Phase 2 network broker, mTLS channel, or remote executor.

Phase 1 provides:

- MCP JSON-RPC protocol handling for local server transport.
- Feature-gated MCP server startup.
- Capability-token generation and validation primitives.
- MCP real-path events for session/tool observability.
- Risk-routing metadata on the tool-gateway path.

Phase 1 does **not** provide:

- A network broker.
- A remote executor.
- mTLS or cross-host authentication.
- Remote route execution. High/critical risk routes are explicitly tagged as `remote-tagged-but-executed-local` until Phase 2 exists.

## Feature gates

MCP remains disabled by default. Enable it only in trusted local environments.

| Setting | Default | Meaning |
|---------|---------|---------|
| `mas.mcp.enabled` | `false` | Master gate for MCP functionality. If false, MCP server startup is blocked. |
| `mas.mcp.server.enabled` | `false` | Server-mode gate. Requires `mas.mcp.enabled` to also be true. |
| `mas.mcp.server.transport` | `"stdio"` | Local transport selector. Invalid values fall back to `"stdio"` rather than crashing. |

Transport limits in Phase 1:

- `stdio` is the only supported local server transport.
- TCP/network broker support is intentionally deferred.
- Do not expose the Phase 1 server over a network socket or wrapper that bypasses q's local trust assumptions.

## MCP execution safety

The MCP `tools/call` path now rejects unknown tools before calling the execution function. Known tools are executed through the configured execution wrapper and returned as MCP-compatible JSON values.

JSON-RPC behavior hardened in v0.99.10:

- `tools/list` returns MCP tool shape: `{name, description, inputSchema}`.
- Notifications are detected by the absence of an `id` member and receive no response.
- Missing params produce JSON-RPC `-32602` invalid params errors.
- Execution exceptions produce JSON-RPC `-32603` internal errors.
- Tool-result structs are converted to JSON expressions before being returned.

## MCP events

The real MCP handler path emits MAS events through `current-mcp-event-sink`:

| Event | Emitted when | Key metadata |
|-------|--------------|--------------|
| `mas.mcp.connected` | MCP `initialize` request succeeds | `server-name`, `method`, `protocol-version` |
| `mas.mcp.tool.called` | `tools/call` succeeds or fails | `tool-name`, `server-name`, `success?`, `route`, optional `error-code` |

The default event sink is a no-op. Integrations can parameterize `current-mcp-event-sink` to forward these events to the blackboard/event-bus layer.

## Capability-token API

Capability tokens are HMAC-authenticated strings with strict parsing. v0.99.10 preserves the legacy API while adding claim-aware validation.

Primary APIs:

| API | Result | Use |
|-----|--------|-----|
| `generate-capability-token` | token string | Issue a token for an agent/capability/expiry. |
| `validate-capability-token` | capability symbol or `#f` | Backward-compatible capability check. |
| `validate-capability-token/claims` | `capability-token-claims` or `#f` | Validate and inspect agent, capability, issued-at, and expiry claims. |
| `validate-capability-token-for-agent` | capability symbol or `#f` | Validate token and enforce intended agent scope. |

Security properties:

- Empty HMAC/token secrets are rejected at the contract boundary.
- Token grammar requires exactly five non-empty colon-separated fields.
- Timestamp fields must be canonical exact nonnegative integer strings.
- Malformed timestamps return `#f`; they do not raise.
- MAC comparison avoids early-return string equality by using constant-work comparison over equal-length strings.
- Agent scope can be enforced without parsing token fields externally.

## Routing policy status

`current-routing-policy` remains safe by default:

| Policy | Behavior |
|--------|----------|
| `local-only` | All requests execute locally and are tagged `route = 'local`. |
| `risk-based` | Low/medium requests execute locally; high/critical requests receive `routing-decision = 'remote` and `route = 'remote-tagged-but-executed-local`. |

Because Phase 2 remote execution does not exist yet, risk-based remote decisions are explicit metadata, not network execution.

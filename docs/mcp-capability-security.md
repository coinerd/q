<!-- verified-against: 0.99.36 -->
# MCP and Capability-Token Security Notes

This document describes the security state for MAS Schritt 6 Phases 1 and 2.

> **Phase 2 (v0.99.36) update:** Remote execution via mTLS TCP broker is now available. See [Distributed Execution Guide](distributed-execution-guide.md) and [mTLS Security Model](security-mtls.md) for details.

## Scope

v0.99.36 hardens the existing Phase 1 MCP/capability-token implementation. It does **not** introduce the Phase 2 network broker, mTLS channel, or remote executor.

Phase 1 provides:

- MCP JSON-RPC protocol handling for local server transport.
- Feature-gated MCP server startup.
- Capability-token generation and validation primitives.
- MCP real-path events for session/tool observability.
- Risk-routing metadata on the tool-gateway path.

Phase 1 does **not** provide:

- ~~A network broker.~~ **→ Phase 2 provides a Racket-native TCP broker.**
- ~~A remote executor.~~ **→ Phase 2 provides `sandbox/executor-server.rkt`.**
- ~~mTLS or cross-host authentication.~~ **→ Phase 2 provides full mTLS with mutual certificate verification.**
- ~~Remote route execution.~~ **→ Phase 2 routes high/critical risk requests to the remote executor via mTLS.**

> **v0.99.36 change:** The `remote-tagged-but-executed-local` annotation is removed. Risk-based routing now dispatches `'remote` decisions to the actual remote executor when `mas.broker.enabled = true`. When the broker is disabled (default), risk-based routing falls back to local execution with a clear warning.

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

JSON-RPC behavior hardened in v0.99.29:

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

Capability tokens are HMAC-authenticated strings with strict parsing. v0.99.36 preserves the legacy API while adding claim-aware validation.

Primary APIs:

| API | Result | Use |
|-----|--------|-----|
| `sign-capability-token` | token string | Issue a token for an agent/capability/timestamp. |
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

## Phase 2: Remote Execution (v0.99.29)

Phase 2 completes the remote execution path. When `mas.broker.enabled = true`:

- High/critical risk requests are routed to the remote executor via mTLS TCP.
- Capability tokens are injected per-request with the shared HMAC secret.
- The circuit breaker protects against executor outages.
- Reconnection with exponential backoff recovers dead connections.

When `mas.broker.enabled = false` (default):

- No TCP listeners are started.
- All requests execute locally.
- Risk-based routing still computes routing decisions, but `'remote` decisions fall back to local execution.

### Key Phase 2 References

- [Distributed Execution Guide](distributed-execution-guide.md) — deployment, troubleshooting
- [mTLS Security Model](security-mtls.md) — trust model, threat model, defense in depth
- `agent/distributed/remote-executor.rkt` — client with circuit breaker and reconnection
- `agent/distributed/remote-ipc.rkt` — async TLS client
- `sandbox/executor-server.rkt` — TLS server with capability token validation
- `sandbox/worker-dispatch.rkt` — extracted dispatch logic
- `util/security/tls-contexts.rkt` — mTLS context factories
- `util/security/cert-generator.rkt` — CA/cert generation
- `cli/generate-certificates.rkt` — `q generate-certificates` command

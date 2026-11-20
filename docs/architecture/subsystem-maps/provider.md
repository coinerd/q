# Subsystem map — LLM providers & transport

Derived facts: `../subsystem-manifest.rktd` (subsystems `llm`, `runtime` retry rows).
Retry configuration contract: [provider-retry.md](../../provider-retry.md).

## Purpose & entry points

- Provider contract: `llm/provider.rkt` (generic interface all adapters satisfy); adapters
  `llm/openai-compatible.rkt`, `llm/anthropic.rkt`, `llm/gemini.rkt`, `llm/azure-openai.rkt`,
  `llm/openrouter.rkt`; helpers `llm/anthropic-helpers.rkt`, `llm/vision-helpers.rkt`,
  `llm/http-helpers.rkt`, `llm/conn-pool.rkt`.
- Streaming: `llm/stream.rkt` (SSE accumulation), `llm/stream.rkt` error taxonomy
  (`llm/provider-errors.rkt`).
- Retry/transport policy: `runtime/provider-retry.rkt` (retry/backoff policy owner),
  `runtime/auto-retry.rkt` (auto-retry loop), circuit-breaker classification consumed from
  the provider error layer.

## Capability ownership

| Responsibility | Owner |
|---|---|
| Provider interface contract | `llm/provider.rkt` |
| Error classification primitives | `llm/provider-errors.rkt` |
| Retry/backoff policy + circuit breaker | `runtime/provider-retry.rkt` (held-request = HTTP 200 + SSE headers + zero chunks ⇒ non-retryable, PN-4) |
| Auto-retry scheduling | `runtime/auto-retry.rkt` |
| Connection pooling | `llm/conn-pool.rkt` (shared dependency of every adapter) |
| Model defaults/registry | `llm/model-defaults.rkt`, `llm/model.rkt` (typed) |

## Behavioral contracts established by tests

- Retry classification: `tests/test-retry-error-classification.rkt`,
  `tests/test-retry-purity.rkt`; ceiling config: `tests/test-provider-retry-ceiling-config.rkt`;
  telemetry: `tests/test-provider-retry-telemetry.rkt`.
- Streaming liveness: `tests/test-stream-peer-close.rkt` (peer FIN without close_notify),
  `tests/test-midstream-stall.rkt`, `tests/test-sse-phase-timeout-bounds.rkt`.
- Pool: `tests/test-conn-pool.rkt` (chunked body reassembly is a shared-dependency contract).
- Conformance: `tests/test-provider-contract-golden-matrix.rkt` (provider golden matrix —
  the provider/composition conformance owner), adapter owner tests under `tests/`
  (area `provider`, suite mapping in `reports/test-ownership-map.json`).

## Provider / consumer relationships

- Consumers: `runtime/agent-session.rkt` / `agent/loop-stream.rkt` (stream deltas and
  message shapes), `llm/stream.rkt` accumulators; on-prem/remote executors use the same
  adapter surface.
- Boundary rule: consumers bind to MESSAGE/STREAM SHAPES (BUG-0003 class: reasoning lost when
  content present) and to the retry classification vocabulary — a classification change is a
  behavioral-contract change even when no signature changes.

## Boundaries, unresolved dependencies, escalation

- `llm/` is a leaf layer by policy (`forbidden-from runtime/tools/extensions`) — the retry
  policy living in `runtime/` is the documented crossing; the typed iteration-side policy
  `runtime/iteration/retry-policy.rkt` governs LOOP retry and is a common **near-miss**
  when locating transport retry — the transport policy owner is `runtime/provider-retry.rkt`.
- `llm/conn-pool.rkt` + `llm/http-helpers.rkt` are shared dependencies: their change contract
  is all adapters + transport liveness tests (corruption class: BUG-0021).
- **Broader verification required when:** classification vocabulary changes, pooling changes,
  SSE framing changes, or provider fixture matrices change. Held-request behavior (zero-chunk
  circuit breaker) is security/availability-relevant — keep the boundary tests in every
  selection.

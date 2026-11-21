# Subsystem map — Agent session & core loop

Derived facts: `../subsystem-manifest.rktd` (subsystems `agent`, `runtime`, `util`).
Structure and data flows: [overview.md](../overview.md) (layer diagram, session lifecycle,
turn dispatch, context assembly, streaming events).

## Purpose & entry points

- Session facade: `runtime/agent-session.rkt` (composition root — make/resume/fork/close).
- Prompt pipeline: `runtime/session/session-lifecycle.rkt` (`run-prompt!`), iteration
  `agent/iteration/main-loop.rkt`, per-step decision `agent/iteration/decision.rkt`.
- Turn orchestration: `agent/loop.rkt` (composition root), streaming `agent/loop-stream.rkt`,
  dispatch `agent/loop-dispatch.rkt`.
- Context assembly facade: `runtime/context/context-assembly.rkt` (budgeting, selection,
  serialization, task-state FSM in `runtime/context-assembly/`).
- Events: `util/event/event-bus.rkt` (ordered, exception-isolated, circuit-breaker),
  typed envelopes `agent/event-structs.rkt` (+ `agent/event-structs/*`), serialization
  `util/event/event.rkt`.

## Capability ownership

| Responsibility | Owner |
|---|---|
| Durable session storage (single writer) | `runtime/session/session-store.rkt` — append-only JSONL, hash chain (`runtime/session/session-store-integrity.rkt`); forks via store tree |
| Ephemeral loop state (single turn) | `agent/state.rkt` — **fan-out 0, 13 direct dependents**; persists nothing by design |
| Recurse/stop decision | `agent/iteration/decision.rkt` (+ typed loop result `util/loop-result.rkt`) |
| Retry policy (iteration side) | `runtime/iteration/retry-policy.rkt` (typed) |
| Cancellation | `runtime/session/session-controls.rkt` (request/force shutdown) |
| Compaction | `runtime/compaction/compactor.rkt` |

## Behavioral contracts established by tests

- Ephemeral state: `tests/test-agent-state.rkt` (owner).
- Store integrity/recovery: `tests/test-session-store.rkt`, `tests/test-hash-chain.rkt`,
  `tests/test-session-store-integrity.rkt`; forks/trees: `tests/test-session-tree.rkt`,
  `tests/test-agent-session-tree.rkt`.
- Event ordering/taxonomy: `tests/test-event-bus.rkt`, golden iteration events
  `tests/test-iteration-event-golden.rkt`, hook golden `tests/test-hook-golden.rkt`.
- Iteration policy: `tests/test-iteration-retry-policy.rkt`, `tests/test-iteration-retry.rkt`.
- Composition: session lifecycle runs through `tests/test-agent-session-config.rkt` and the
  workflow e2e owners (see `reports/test-ownership-map.json`, area `session`).

## Provider / consumer relationships

- Consumers of the session store: resume/fork (`runtime/agent-session.rkt`), context assembly
  (reads messages), TUI/SDK (event projections only).
- Providers into the loop: provider adapters (llm/, see [provider.md](provider.md)), tools
  (see [tools-permissions.md](tools-permissions.md)), extensions via 47 hook points
  (`util/hook-types.rkt`).
- Boundary rule: event TAXONOMY is a shared contract — emitter changes require the golden
  event tests; consumers (TUI/SDK) bind to envelope shapes, not signatures.

## Boundaries, unresolved dependencies, escalation

- **Impact hub, not defect:** an edit to `agent/state.rkt` selects ~11 audited owners in
  `./qt quick --changed` (CL0-measured ≈25 s) because the owners transitively exercise the
  loop modules that require it — an inbound-closure (impact-graph) effect. Structural
  independence (`fan-out 0`) is NOT behavioral independence: `util/error/errors.rkt` (365
  transitive dependents) and `util/event/event.rkt` (289) can change observable behavior
  everywhere without any signature change.
- **Broader verification required when:** hash-chain/store format changes; event ordering or
  taxonomy changes; hook interception (`util/hook-types.rkt`); context-budget semantics;
  anything on the `util/` foundation layer used by 796 inbound edges.

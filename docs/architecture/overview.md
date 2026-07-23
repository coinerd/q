<!-- verified-against: 0.99.58 -->
# Architecture Overview

> **Navigation**: [Layer Diagram](#layer-diagram) · [Module Ownership](#module-ownership-map) · [Key Data Flows](#key-data-flows) · [Key Design Decisions](#key-design-decisions) · [Event System](#event-system) · [Dependency Policy](#dependency-policy) · [TUI Stack](#native-tui-stack) · [Typed Racket](#typed-racket-modules) · [Contract Metrics](#contract-metrics)

---

## Layer Diagram

```
+---------------------------------------------+
|              INTERFACES                     |
|  CLI (cli/) . TUI (tui/) . SDK (interfaces/) |
+---------------------------------------------+
|              WIRING                         |
|  wiring/ - run modes, mode helpers           |
+---------------------------------------------+
|           RUNTIME & EXTENSIONS              |
|  runtime/ . extensions/                      |
+---------------------------------------------+
|            AGENT CORE                       |
|  agent/ - event bus, loop, event structs    |
+---------------------------------------------+
|            LLM PROVIDERS                    |
|  llm/ - OpenAI, Anthropic, Azure            |
+---------------------------------------------+
|             TOOLS                           |
|  tools/ - builtins, registry, scheduler     |
+---------------------------------------------+
|            BROWSER                          |
|  browser/ - types, adapter, policy, service |
|  session, settings, audit, workflow, events |
|  sidecars/ - Playwright integration         |
+---------------------------------------------+
|            FOUNDATION                       |
|  util/ - types, JSONL, events, errors       |
+---------------------------------------------+
|            SANDBOX                          |
|  sandbox/ - subprocess isolation            |
+---------------------------------------------+
```

**Layering rule**: Each layer may only depend on layers below it (foundation is lowest).
See [docs/architecture/dependency-policy.rktd](./dependency-policy.rktd) for machine-readable
exceptions and boundary debt tracking.

---

## Module Ownership Map

Every source module belongs to a layer. Key files, their layer, and the authoritative
inline comment that defines their role:

| File | Layer | Owner Annotation (from source) |
|------|-------|--------------------------------|
| `main.rkt` | Wiring | "Thin facade: CLI parsing to runtime construction to mode dispatch. All business logic lives in wiring/ and interface modules." |
| `wiring/run-modes.rkt` | Wiring | "Runtime construction and mode dispatch facade - extracted from main.rkt." |
| `runtime/agent-session.rkt` | Runtime | "Session lifecycle orchestration (facade) - ties together session store, core loop, provider, tool registry, and event bus." |
| `runtime/session/session-lifecycle.rkt` | Runtime | "Session prompt execution lifecycle - extracted from agent-session.rkt. Core prompt execution pipeline: context building, iteration dispatch, run-prompt!." |
| `agent/iteration/main-loop.rkt` | Agent | "Main iteration loop orchestrator." |
| `runtime/context/context-assembly.rkt` | Runtime | "Unified context assembly pipeline - thin facade re-exporting from budgeting, selection, serialization." |
| `runtime/context-assembly/turn-context.rkt` | Runtime | "Turn-level context assembly helpers - extracted from turn-orchestrator.rkt." |
| `agent/loop.rkt` | Agent | "COMPOSITION ROOT - effectful agent turn orchestrator. Should not be imported by other production modules." |
| `agent/loop-messages.rkt` | Agent | "Message helpers (build-raw-messages, emit! etc.)" |
| `agent/loop-stream.rkt` | Agent | "Streaming, cancellation, result building." |
| `agent/state.rkt` | Agent | "Ephemeral loop state for a single run. Mutable accumulators for messages and events. Must NOT persist state directly." |
| `agent/event-bus.rkt` | Agent | "RE-EXPORT SHIM - canonical location is util/event/event-bus.rkt." |
| `util/event/event-bus.rkt` | Foundation | "Publish/subscribe event bus - synchronous, ordered, exception-isolated, thread-safe, circuit-breaker guarded." |
| `util/event/event.rkt` | Foundation | "Event envelope struct and serialization - migrated to lang typed/racket." |
| `agent/event-structs.rkt` | Agent | "Facade re-exporting all event struct definitions from 9 sub-modules." |
| `llm/provider.rkt` | LLM | "Provider interface contract - generic interface that all LLM provider adapters must satisfy. Uses racket/generic." |
| `tools/tool.rkt` | Tools | "Tool contract, execution context, and registry facade. Stable public API for the tool subsystem." |
| `tools/registry-table.rkt` | Tools | "Declarative tool spec table (thin facade) - encodes all built-in tools as tool-spec structs." |
| `runtime/session/session-store.rkt` | Runtime | "Append-only JSONL session storage (facade) - hash chain, verification, repair, write-ahead markers." |
| `sandbox/subprocess.rkt` | Sandbox | "Subprocess management under custodians - resource limits and custodian-based cleanup." |
| `tui/tui-init.rkt` | TUI | "TUI initialization, teardown, and entry points." |
| `extensions/gsd/core.rkt` | Extensions | "GSD workflow core - milestone/wave lifecycle management." |
| `extensions/gsd/state-machine.rkt` | Extensions | "GSD state machine - FSM for milestone transitions." |
| `util/error/errors.rkt` | Foundation | "Error hierarchy - q-error to session-error, extension-error, policy-error." |
| `util/hook-types.rkt` | Foundation | "47 hook points where extensions can intercept. Typed Racket." |

> **Note**: Files annotated as COMPOSITION ROOT (agent/loop.rkt, runtime/agent-session.rkt,
> runtime/session/session-lifecycle.rkt, agent/iteration/main-loop.rkt) wire together
> dependencies from lower layers. They should not be imported by other production modules
> - they sit at the top of the dependency chain within their layer.

## Key Data Flows

### 1. Session Lifecycle (Create to Init to Turn to Destroy)

```
+----------+    +-----------+    +--------------+    +-----------+
|  CREATE  |--->|   INIT    |--->|  TURN LOOP   |--->|  DESTROY  |
|          |    |           |    |  (repeated)   |    |           |
+----------+    +-----------+    +--------------+    +-----------+
     |               |                 |                   |
     | generate-id   | create store    | run-prompt!       | close-session!
     | init log      | wire events     | dispatch-turn     | persist
     |               | register tools  |                   | shutdown hooks
```

**Key modules**:
- runtime/agent-session.rkt - make-agent-session, resume-agent-session, fork-session, close-session!
- runtime/session/session-store.rkt - append-only JSONL storage
- runtime/session/session-lifecycle.rkt - run-prompt! entry point
- runtime/session/session-config.rkt - configuration assembly
- runtime/session/session-controls.rkt - set-model!, cycle-model!, request-shutdown!, force-shutdown!

**Store architecture**: Append-only JSONL. Each entry is a JSON-encoded message struct,
written sequentially to session.jsonl. Integrity maintained by hash chain
(session-store-integrity.rkt). Forking creates a new log that shares history up to the fork point.

### 2. Turn Dispatch (Receive Message, Build Context, LLM Call, Tool Execution, Respond)

```
User Input -> run-prompt! -> build-session-context -> run-agent-turn -> run-streaming-phase -> interpret-step -> iteration loop -> loop-result
```

Phases within run-agent-turn:
1. emit-turn-started - event bus to TUI
2. build-context - raw messages + effects
3. build-model-request - model-request hash
4. pre-hook (agent-start) - may block

After streaming: interpret-step decides directive-stop (turn complete) or directive-recurse (execute tool, next iteration).
Iteration loop repeats until max-iterations or model signals completion.

**Key modules**:
- runtime/session/session-lifecycle.rkt - run-prompt! (outer loop)
- agent/loop.rkt - run-agent-turn (single turn orchestration)
- agent/iteration/main-loop.rkt - run-iteration-loop (multi-turn iteration)
- agent/iteration/step-interpreter.rkt - interpret-step (decide recurse vs stop)
- agent/loop-stream.rkt - streaming accumulation
- agent/loop-dispatch.rkt - run-streaming-phase

### 3. Context Assembly (Collect Messages, Select Tiers, Build Prompt)

```
session messages -> build-tiered-context/state-aware
    |-- Tier A: Recent messages (full fidelity)
    |-- Tier B: Summarized catalog entries
    |-- Tier C: Task-state-aware conclusions and working set
    v
tiered-context (tier-a, tier-b, tier-c, token-count)
    v
tiered-context->message-list -> LLM request
```

**Key modules**:
- runtime/context/context-assembly.rkt - facade
- runtime/context-assembly/serialization.rkt - build-tiered-context/state-aware, tiered-context->message-list
- runtime/context-assembly/selection.rkt - message selection strategies
- runtime/context-assembly/budgeting.rkt - token budget calculations
- runtime/context-assembly/task-state.rkt - task FSM state (idle, exploration, planning, implementation, verification, debugging)
- runtime/context-assembly/state-aware-builder.rkt - working-set evolution
- runtime/context-assembly/auto-distillation.rkt - automatic context distillation
- runtime/context-assembly/conclusion-graph.rkt - graph-based conclusion selection
- runtime/context/context-summary.rkt - summary entities, catalog generation
- runtime/context/context-pressure.rkt - context overflow detection
- runtime/context/context-pinning.rkt - pinned messages

### 4. Streaming Events (Model Output Deltas, Event Bus, TUI Render)

```
LLM stream (provider-stream)
    v
accumulate-tool-call-deltas (llm/stream.rkt)
    v
Event bus (util/event/event-bus.rkt)
    |-- message-start-event
    |-- message-update-event
    |-- tool-execution-start/update/end-event
    |-- message-end-event
    |-- turn-end-event
    v
TUI subscriber -> ui-state mutations -> render -> ANSI output
```

**Key modules**:
- util/event/event-bus.rkt - sync pub/sub bus with circuit breakers
- agent/event-emitter.rkt - emit-typed-event!
- llm/stream.rkt - accumulate-tool-call-deltas
- tui/tui-init.rkt - subscribe-runtime-events!, event->action-hash bridge
- tui/vdom-bridge.rkt - render loop integration

## Key Design Decisions

### 1. Event Bus Architecture

**Location**: util/event/event-bus.rkt (stable, public)

**Design**:
- Synchronous publish/subscribe with ordered subscriber notification
- Exception isolation: one failing subscriber does not break others
- Optional predicate-based filtering (subscribe! with filter)
- Thread-safe via semaphore guarding
- Per-bus circuit breaker state (threshold + cooldown)
- publish! returns the event for chaining

**Why synchronous**: Simple reasoning about ordering. The bus is used primarily for
TUI updates and extension hook dispatch - neither requires async delivery guarantees.
Heavy processing happens outside the bus (in iteration loops).

**Event tiers**:
| Tier | Format | Location | Stability |
|------|--------|----------|-----------|
| Raw | event struct with string ev + any payload | util/event/event.rkt | Stable for interop |
| Typed | typed-event struct hierarchy (37 structs) | agent/event-structs/ | Public vs internal |
| Contract-validated | Payload contracts enforced at emission | util/event-contracts.rkt | Evolving |

See docs/event-taxonomy.md for complete event reference.

### 2. Append-Only Session Store

**Location**: runtime/session/session-store.rkt (public, evolving)

**Design**:
- Every message, event, and state transition is appended to session.jsonl as a JSON line
- Hash chain integrity: each entry references the previous entry hash
- Forking creates a new log file sharing history up to the fork point
- Replay: loading a session replays the full log to reconstruct state
- Write-ahead markers for crash recovery

**Why append-only**: Immutable history enables fork/resume, audit trails, and
deterministic replay. No in-place mutation means no corruption from partial writes.

**Key files**:
- session-store-integrity.rkt - hash chain, verification, repair
- session-store-tree.rkt - tree store operations (forks)
- session-store/versioning.rkt - versioning constants
- session-store/in-memory.rkt - in-memory manager

### 3. GSD Workflow (Goal-oriented, Self-Directed)

**Location**: extensions/gsd/ (25+ files)

**Design**: An extension that adds structured milestone/wave planning to the agent loop.
The GSD subsystem defines:
- Plan types: milestones, waves, tasks, gates (plan-types.rkt)
- State machine: FSM for milestone transitions (state-machine.rkt)
- Wave execution: step-by-step wave progress (wave-executor.rkt, wave-docs.rkt)
- Context bundles: plan-aware context assembly (context-bundle.rkt, plan-context-builder.rkt)
- Command handlers: user-facing GSD CLI commands (command-handlers.rkt)
- Validation: plan structure and consistency checks (plan-validator.rkt)
- Governance: documented process rules in docs/gsd-process-governance.md

### 4. Parameter-Based State (No Global State)

**Location**: agent/state.rkt (public)

**Design**:
- loop-state struct holds mutable boxes for message/event accumulation during a turn
- Passed explicitly through the turn pipeline (not global)
- current-loop-state-for-error-recovery parameter is the only parameter - used
  solely for error-path recovery to flush partial messages on stream failure
- Session state is stored in the agent-session struct (opaque, accessor functions)

### 5. Hook System (Extension Interception Points)

**Location**: util/hook-types.rkt (Typed Racket, stable)

**Design**: 47 hook points where extensions can intercept the turn lifecycle.
Key hooks: agent-start (can block), agent-end, message-start (can block),
message-update (can block), message-end, tool-call (can block), tool-result,
session-start, session-shutdown, context-reduce (can block).

### 6. Tool Registry and Execution

**Location**: tools/tool.rkt (stable facade)

**Design**:
- Tools are tool? structs with name, description, JSON schema, and execute function
- Registry is a mutable hash mapping tool names to tool structs
- register-default-tools! populates built-in tools (bash, write, edit, read, grep, find, etc.)
- tools/registry-table.rkt encodes tools as declarative tool-spec structs
- Tool execution context (exec-context.rkt) provides cancellation, settings, and permissions
- Dangerous tools (bash, write, edit, browser_click) require capability validation
- Externalizable tools (bash, write, edit, delete-lines) can run in a worker process

### 7. Provider Abstraction

**Location**: llm/provider.rkt (public)

**Design**:
- Generic interface using racket/generic: provider-send, provider-stream, provider-count-tokens
- Adapters: openai-compatible.rkt (OpenAI, Azure, Anthropic), anthropic.rkt
- Factory: runtime/provider/provider-factory.rkt - build-provider, build-mock-provider
- Model registry: runtime/provider/model-registry.rkt
- Token budgeting: llm/token-budget.rkt - estimate-turn-tokens, estimate-context-tokens

## Native TUI Stack

The TUI layer uses a seven-layer native Racket architecture (no external dependencies).
See ADR-0016 for full details.

```
+---------------------------------------+
|  Bridge (vdom-bridge.rkt)            |
|  Integration with render loop        |
+---------------------------------------+
|  Components (vdom-components.rkt)    |
|  Typed zone components + state bag   |
+---------------------------------------+
|  Virtual DOM (vdom*.rkt, 3 files)    |
|  vnode trees + layout + render       |
+---------------------------------------+
|  Cell Diff (cell-diff*.rkt, 2 files) |
|  Incremental rendering               |
+---------------------------------------+
|  Cell Buffer (cell-buffer.rkt)       |
|  2D cell grid storage                |
+---------------------------------------+
|  Terminal I/O (terminal-native.rkt)  |
|  ANSI sequences, raw mode            |
+---------------------------------------+
```

Key files: tui/terminal-native.rkt, tui/cell-buffer.rkt, tui/cell-diff.rkt,
tui/cell-diff-render.rkt, tui/vdom.rkt, tui/vdom-layout.rkt,
tui/vdom-render.rkt, tui/vdom-bridge.rkt, tui/vdom-components.rkt, tui/component.rkt

### Render Pipeline (since v0.61.5)

The TUI uses a fully vdom-mediated, multi-backend render path. Components persist
across frames with local state, and the same vdom output can target multiple backends.

```
ui-state -> render-frame-vdom!
    |-- Header -> component (cached) -> vnodes
    |-- Status -> component (cached) -> vnodes
    |-- Input -> component -> vnodes
    |-- Transcript -> render-transcript -> styled-lines
    |-- Overlay -> styled-lines
    |-- Widgets -> vnodes
    v
vdom-layout -> styled-lines
    v
    |-- cell-buffer (terminal)
    |     diff -> deltas -> batched ANSI output
    |
    |-- html-render (HTML/CSS)
          HTML string (web preview)
```

Component lifecycle (since v0.62.0):
- Components created once, stored in tui-ctx-component-registry-box
- component-render uses cache (invalidates on width/state change)
- component-state-ref/set! for per-component local state

Key dispatch (since v0.62.1):
- focused-component-id-box on tui-ctx tracks which component receives keys
- handle-key checks focused component first, falls back to keymap
- Opt-in via wants-focus? #t + handle-input-fn

Key optimizations:
- Batch rendering: Consecutive same-row/same-SGR cells to single cursor move (37% faster)
- SGR dedup: Only emit SGR when attributes change
- Cell-diff: XOR row hashing, smart full vs incremental threshold (50%)
- DECAWM: Auto-wrap disabled during render to prevent last-column wrap glitch

---

## Module Counts (428 source modules, ~65,000 LOC)

| Layer | Modules | Key Files |
|-------|---------|-----------|
| Foundation (util/) | ~25 | protocol-types.rkt, event.rkt, errors.rkt, version.rkt |
| LLM (llm/) | ~10 | provider.rkt, openai-compatible.rkt, anthropic.rkt |
| Agent Core (agent/) | ~15 | loop.rkt, event-bus.rkt, event-structs/ |
| Tools (tools/) | ~15 | tool.rkt, registry-defaults.rkt, builtins/ |
| Browser (browser/) | ~11 | types.rkt, adapter.rkt, policy.rkt, service.rkt, workflow.rkt |
| Runtime (runtime/) | ~45 | iteration.rkt, agent-session.rkt, session-lifecycle.rkt |
| Extensions (extensions/) | ~30 | gsd/, github/, racket-tooling/ |
| Interfaces (interfaces/, cli/, tui/) | ~45 | sdk-core.rkt, args.rkt, renderer.rkt, tui/keybindings/ |
| Wiring (wiring/) | ~3 | run-modes.rkt, mode-helpers.rkt |
| Sandbox (sandbox/) | ~3 | subprocess.rkt |
| Scripts (scripts/) | ~25 | lint-all.rkt, sync-version.rkt, run-tests.rkt |

## Key Contracts

- **Tool contracts**: tools/tool.rkt - (tool? -> (or/c text? json?))
- **SDK contracts**: interfaces/sdk-core.rkt - contract-out on public API
- **Error hierarchy**: util/errors.rkt - q-error to session-error, extension-error, policy-error
- **Event types**: util/event.rkt (raw) + agent/event-structs/ (typed)

## Dependency Policy

See docs/architecture/dependency-policy.rktd for layering rules and known violations.

Key layer rules (from dependency-policy.rktd):
- **Runtime**: may depend downward on LLM provider contracts; must not import into tools/extensions
  except via documented layer-adapters.rkt exceptions (max 12 exceptions)
- **Agent**: minimal cross-boundary imports (max 2 exceptions)
- **TUI**: may not import from llm/ or tools/ (max 0 exceptions)
- **LLM**: leaf modules - no upward imports (max 0 exceptions)
- **Extensions**: may import from runtime/core but never TUI (max 5 exceptions)
- **Browser**: peer of tools - imports only agent/event and util; never runtime or TUI (max 0 exceptions)

Boundary debt tracking (GAP-7): violations must have a resolution milestone scheduled
before revisit-by date. See docs/gsd-process-governance.md section 7.

## Event System

Two tiers:
1. Raw events: make-event + emit-event! (legacy, stable for interop)
2. Typed events: 37 structs in agent/event-structs/ (incl. 10 browser events) +
   emit-typed-event! (introduced v0.33.7, stable)

Event API classification (v0.96.1):
- PUBLIC (extension-facing, stability commitment): typed-event, context-event,
  injection-event, turn-start-event. Changing fields or removing accessors is BREAKING.
- INTERNAL (runtime-internal, no stability commitment): All other events.
  Consumers are in agent/, runtime/, llm/, tools/. Field names may change.

See docs/event-taxonomy.md for complete event reference.

## Typed Racket Modules

Per ADR 0014, stable struct-heavy modules are migrated to Typed Racket:
- util/event.rkt, util/event-payloads.rkt, util/hook-types.rkt
- util/version.rkt, util/loop-result.rkt
- extensions/gsd/plan-types.rkt, extensions/gsd/plan-validator.rkt

## Contract Metrics

- Baseline: 882 any/c (148 files)
- Current: 812 any/c (143 files) - 7.9% reduction
- See docs/reports/f2-contract-precision-trend.md for trend analysis

## Related Documentation

- [GSD Process Governance](../gsd-process-governance.md) - milestone/wave governance rules
- [Event Taxonomy](../event-taxonomy.md) - complete event type reference
- [Dependency Policy](./dependency-policy.rktd) - machine-readable layering rules
- docs/reports/ - 50+ audit reports covering architecture, abstraction, contracts, TUI, testing
- docs/reports/f2-contract-precision-trend.md - contract precision trend analysis

## Developer Tooling

- 500+ test files in tests/
- Parallel runner: racket scripts/run-tests.rkt
- 22-check lint suite: racket scripts/lint-all.rkt
- Contract metrics: racket scripts/contract-metrics.rkt
- Hotspot report: racket scripts/hotspot-report.rkt
- IVG invariants: 8 checks in scripts/lint-ivg.rkt

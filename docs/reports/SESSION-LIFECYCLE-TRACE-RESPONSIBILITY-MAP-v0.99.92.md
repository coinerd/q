# Session Lifecycle Trace & Responsibility Map — v0.99.92 W0

**Status:** CHARACTERIZATION BASELINE
**Baseline:** v0.99.91 release `a4b85569`
**Machine oracle:** `docs/architecture/session-lifecycle-trace-v0.99.92.rktd`
**Scope:** evidence only; no production behavior or API changed

## Executive decision

`run-prompt!` is not one responsibility. It is the composition boundary for
prompt ownership, input interception, session-state parameterization, context
preparation, advisory compaction, persistence, Agent iteration dispatch,
index repair, eventing, rollback save-back, interrupt acknowledgement, and
cleanup. W1 may extract only a coherent **pure prompt-preparation plan**. It
must not move persistence, event publication, FSM ownership, wiring, or effect
ordering merely to reduce the lifecycle module's LOC.

The machine oracle freezes six path families as 31 explicit variants, ten
responsibility units, 27 direct/transitive consumer edges, 38 exceptional
boundaries, two parameter scopes, and five explicitly classified findings.
Every locator is checked against a real source file and anchor; unique
control-flow anchors are additionally checked in source order.

## Identity model

Two turn identities coexist:

- The **prompt turn ID** is created by `begin-session-turn!`; outer
  `turn.started` and request-correlated interruption acknowledgement use it.
- A **model turn ID** is created by the Agent iteration loop and reused across
  provider retry attempts in that iteration.

Normal completion relies on the inner stream terminal. Dispatch errors emit
`turn.completed` with no turn ID. Accepted interrupts receive a prompt-ID-
correlated terminal only during outer cleanup. W1–W3 must preserve this current
observable behavior unless a separately approved defect wave changes it.

## Ordered path map

The schema-v2 oracle separates these observable variants instead of treating a
family as one unconditional sequence:

| Family | Explicit path IDs |
|---|---|
| normal / hook exits | `normal-success`, `hook-input-block`, `hook-before-agent-block`, `hook-turn-start-block`, `hook-model-request-block`, `hook-message-start-block`, `hook-message-end-block` |
| error | `handled-error`, `error-then-index-failure` |
| cancel | `cancel-pre-iteration`, `cancel-midstream` |
| close | `close-normal`, `close-repeated`, `close-active-prompt` |
| retry | `retry-success`, `retry-exhausted`, `retry-exhausted-partial`, `retry-held-circuit`, `retry-progressive-circuit`, `retry-health-gate`, `retry-adaptive`, `retry-partial-recovery` |
| compaction | `compact-auto-success`, `compact-auto-hook-block`, `compact-auto-start-failure`, `compact-midturn`, `compact-manual-completed`, `compact-manual-nothing`, `compact-manual-failed`, `compact-manual-tracer-failure`, `compact-manual-contention` |

### Normal

1. Claim prompt ownership.
2. Begin prompt turn and publish outer `turn.started`.
3. Dispatch the input hook and persist `last-user-prompt` for string input.
4. Parameterize prompt ownership and session-owned rollback state.
5. Build/persist the user message and construct context.
6. Optionally compact, emit pressure/context observations, ensure persistence.
7. Dispatch the Agent iteration/provider turn.
8. Rebuild the durable index and emit `session.updated`.
9. Save rollback state in the inner `dynamic-wind` after-thunk.
10. Finish prompt-turn interruption state and release prompt ownership in the
    outer `dynamic-wind` after-thunk.

### Error

Provider/iteration failures inside `dispatch-iteration` persist recovered
partials, classify and emit `runtime.error`, emit `turn.completed/error`, stop
the trace logger, and return an error loop result. The caller then rebuilds the
index, emits `session.updated/error`, saves rollback state, and releases prompt
ownership. Failures before or outside that handler instead use the outer cleanup
terminal when cleanup reaches it.

### Cancellation

The event subscriber validates and records the request, emits
`interrupt.accepted`, then signals the session token. Cancellation is observed
at an iteration checkpoint or stream chunk. The stream/iteration emits its
cancel result, `session.updated` follows, and outer cleanup rotates the one-way
token, releases ownership, and emits the single request-correlated
`turn.cancelled` for the prompt turn. Pre-iteration cancellation may also emit
an uncorrelated Agent terminal.

### Close

`close-session!` marks closed, clears browser state, and—if still active—ensures
persistence, emits `session.closed`, dispatches the shutdown hook, persists
high-value conclusions, and marks inactive. Subscriber/watcher/registry and
repository cleanup follows. Each cleanup group is guarded, but the already-
closed guard currently logs and continues.

### Retry

`call-with-provider-retry` delegates classification/budget/backoff to
`with-auto-retry`. A retry emits `auto-retry.start` before sleep and re-enters
the provider turn with the same model turn ID. Health/circuit-break and attempt
budgets can stop retries. Final failure returns to the dispatch error path;
success continues the normal path without a second user-message append.

### Compaction

Automatic and mid-turn compaction are advisory: threshold/cooldown and nested
prompt ownership are checked, compaction ownership is claimed, start/hook/
warning/completion effects occur, and `dynamic-wind` releases ownership and
sets cooldown. Manual compaction is a separate event-driven durable path that
persists a summary and rebuilds the index.

## Responsibility and consumer map

| Unit | Responsibilities | Primary consumers |
|---|---|---|
| `run-prompt!` | orchestration, eventing, FSM | SDK, TUI, goal runner, façade |
| `run-prompt-internal` | orchestration, persistence, eventing | `run-prompt!` |
| `build-session-context-for-prompt` | pure preparation plus ordered persistence/orchestration | prompt internal |
| `dispatch-iteration` | orchestration, eventing, wiring | prompt internal |
| `run-iteration-loop/v2` | orchestration, FSM | Runtime composition closure |
| `close-session!` | orchestration, persistence, eventing, FSM, wiring | all interfaces |
| provider/auto retry | orchestration, eventing, retry FSM | turn orchestrator |
| automatic compaction | orchestration, eventing, ownership FSM | prompt and step executor |
| durable compaction | orchestration, persistence, eventing, FSM, wiring | event subscribers |

The context builder is deliberately classified as mixed today: parent/message
construction and system injection are pure, but index/user append and settings
mutation are not. W1 must split by effect semantics, not copy this entire
function under a new name.

## Parameter and `dynamic-wind` contract

- `current-prompt-operation-session` enables same-session nested automatic
  compaction and unwinds automatically.
- `current-rollback-state` is initialized from session lifecycle state.
- Its value is copied back by the **inner `dynamic-wind` after-thunk before the
  parameterization unwinds**, on both normal and exceptional prompt exits.
- The outer `dynamic-wind` then performs interruption finalization, prompt
  release, abnormal cleanup terminal, and emergency persistence.

This order is the W1–W3 trace-equivalence oracle.

## Exceptional exits

The machine ledger enumerates all observed exits and records phase, cleanup,
terminal behavior, classification, and source anchor. Important boundaries are:

- closed guard and prompt contention occur before cleanup protection;
- prompt ownership is already claimed when `begin-session-turn!` and the outer
  `turn.started` execute, before the outer `dynamic-wind`;
- input/context/persistence/compaction exceptions are inside outer cleanup and,
  for the prompt body, rollback save-back protection;
- model-select and trace start are outside the dispatch exception handler;
- provider/iteration `exn:fail?` is converted into an error loop result;
- index rebuild occurs after the model terminal and can still fail;
- rollback save-back can fail after a normal result disabled the cleanup
  terminal;
- failure in `finish-session-turn!` or `release-prompt!` can suppress later
  cleanup; non-`exn:fail?` breaks bypass dispatch handling;
- manual durable compaction has its own failed event and release/trace cleanup.

## Findings and disposition

| ID | Severity / classification | Owner | Follow-up | Observation |
|---|---|---|---|---|
| W0-F1 | High / DEFERRED | Runtime Session | `W1 #9243 preserve; W4 #9246 terminal decision` | Prompt ownership is claimed before outer `dynamic-wind` protection. |
| W0-F2 | Medium / DEFERRED | Runtime Session | `W1 #9243 preserve; W4 #9246 terminal decision` | Normal, error, and correlated cancellation use different terminal identities/events. |
| W0-F3 | High / DEFERRED | Runtime Session | `W4 #9246 assign separate concurrency milestone` | Close does not coordinate with an active prompt/repository writer. |
| W0-F4 | Medium / DEFERRED | Runtime Compaction | `W3 #9245 locality assessment; W4 #9246 decision` | Automatic compaction completion/cooldown follows block/body error; start publication failure leaks ownership. |
| W0-F5 | Medium / DEFERRED | Runtime Retry | `W4 #9246 terminal decision` | Retry sleep is not cancellation-aware; partial wrapping can hide retry metadata. |

No finding is silently repaired in W0. W0-F1/F2 constrain W1–W3 equivalence;
W0-F3 is concurrent lifecycle correctness rather than pure preparation; W0-F4
and W0-F5 belong to their respective subsystem hardening scopes.

## W1 extraction constraints

W1 may accept pure input/config and return a preparation plan containing values
needed by orchestration. The caller must retain:

- prompt/compaction ownership and session FSM mutation;
- all hook and event publication;
- user/index persistence and its exact ordering;
- Runtime-owned Context Assembly calls;
- session-owned rollback state and exceptional save-back;
- provider/Agent wiring and trace logger lifecycle;
- cancellation, retry, close, and compaction semantics.

MA-11 and MA-12 stay guarded: Agent iteration must not import Runtime
implementation modules, and no cross-turn parameter side channel may replace
session-owned state.

## Validation limits

This wave freezes a source-anchored structural trace oracle and runs the existing
behavioral lifecycle suites. It does not claim formal concurrency verification,
data-flow analysis, or correction of the classified findings. Generated IDs and
timestamps are intentionally represented by identity roles rather than literal
values.

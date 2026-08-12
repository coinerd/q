# Context-Build Request/Result Boundary — v0.99.93 W2

**Status:** IN PROGRESS — implementation complete, gates pending
**Baseline:** v0.99.93 W1 merge `d190a36b`
**Trace oracle:** `docs/architecture/session-lifecycle-trace-v0.99.93.rktd`
**MA-10:** stays OPEN; this wave makes the Context Assembly boundary explicit.

## Goal and boundary

Introduce an explicit `context-build-request`/`context-build-result` pair for
the prompt context-building step while keeping Context Assembly Runtime-owned
and cross-turn state session-owned.

## Deliverables

- `runtime/session/session-context-boundary.rkt`:
  - `context-build-request` (user-message, history, index, system-instructions,
    provider?, working-set, max-tokens) — the pure inputs the caller already
    resolves (E0 index ensure, history load, E1 working-set reset) before
    invoking.
  - `context-build-result` (canonical-user-message, post-append-index,
    appended-entry, parent-id, context-messages, model-name,
    context-with-system) — the pure outputs the caller applies through effects
    E2 (box set → install → save), E3 (buffer/append), E4 (model-name).
  - `context-build` — pure request → result composition over the W1
    preparation plan; no I/O, no mutation (R-18).
- `tests/test-session-context-boundary.rkt` — RED-first 8-case matrix:
  explicit types, caller-index no-mutation, E2/E3/E4 result values, linear /
  tiered branches, path-model setting, system injection, max-tokens field.
- `build-session-context-for-prompt` refactored to build a request, call
  `context-build`, and apply E2/E3/E4 unchanged.
- `build-prompt-preparation-plan` gained a `#:max-tokens` keyword (default =
  the historical `DEFAULT-TOKEN-BUDGET-THRESHOLD`), a behavioral no-op.
- Trace oracle: the `normal-success` path records the `context-build` boundary
  as the zero-effect step; `working-set-config` stays in its correct order.
- `docs/reports/CONTEXT-BUILD-BOUNDARY-v0.99.93.md` (this report).

## Ownership preservation

- Context Assembly remains Runtime-owned: the boundary module lives under
  `runtime/session/`, imports only runtime-layer pure helpers and the W1 plan,
  and adds no layer exception.
- Cross-turn state stays session-owned: no new parameter is introduced; the
  request carries the caller's resolved index, working set, and history; the
  result carries values only. MA-12 (no hidden cross-turn parameter side
  channel) remains guarded by `test-arch-parameters.rkt`.
- The working set and the session index (active-leaf box + bookmark semaphore)
  cross the boundary, but `context-build` reads both and writes neither, exactly
  as the W1 plan did.

## Effect-order preservation

Historical order is preserved: E0 index ensure → history load → E1 working-set
reset → (pure `context-build` boundary) → E2 box-set/install/save → E3 buffer →
E4 model-name → return `context-with-system`. This also corrected a W1 oracle
ordering defect where `working-set-config` appeared before `claim` and
`context-build` after `user-index-persistence`; the trace now matches the real
runtime order. `session-lifecycle.rkt` remains at 566 LOC (under the frozen
600-line budget).

## Gates

- Boundary matrix 8/8 ×3; W1 plan matrix 19/19.
- Lifecycle/characterization set 68/68.
- Rollback/session-ownership/context-assembly focused set 182/182.
- Arch suite (26 files / 271 tests incl. new R-18 gate; the boundary test matches the arch-file `"boundary"` pattern).
- Broad suite (W2 Broad gate).
- Fast suite.
- PR CI 17/17; required policy NONE unmet.
- Independent read-only review.

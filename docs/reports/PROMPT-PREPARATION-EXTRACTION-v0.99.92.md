# Prompt Preparation Extraction — v0.99.93 W1

**Status:** IN PROGRESS — implementation complete, gates pending
**Baseline:** v0.99.93 W0 merge `16c17030`
**Trace oracle:** `docs/architecture/session-lifecycle-trace-v0.99.93.rktd`
**MA-10:** stays OPEN; this wave supplies trace-equivalent extraction evidence.

## Goal and boundary

Extract only the coherent pure user-input/config-to-preparation-plan
transformation from the prompt lifecycle. The orchestration caller retains every
side effect and its exact ordering; observable behavior and effect order are
unchanged.

## What was extracted

New pure module `runtime/session/session-prompt-preparation.rkt`:

- `build-prompt-preparation-plan` computes, from pure inputs (`user-message`,
  `history`, `index`, `system-instructions`, `provider?`, `working-set`):
  - canonical user message (parent-linked `build-user-message` or the pure
    index append fix),
  - post-append session index (structural, via the new `append-to-leaf/pure`),
  - path-derived model setting (`extract-path-settings`),
  - context source selection (tiered / tree / linear) and the assembled context,
  - system-injected final context.
- It performs no I/O and mutates no session or index state. It is pinned by a
  new R-18 gate (`test-arch-fitness.rkt`) and registered in
  `docs/architecture/dependency-policy.rktd` `pure-modules`.

Supporting change `runtime/session-index/mutations.rkt`:

- `append-to-leaf/pure` — the historical `append-to-leaf!` computation without
  the shared active-leaf box mutation. `append-to-leaf!` now delegates to it and
  applies the box mutation, preserving exact alias semantics for existing
  callers.

## Effect-order preservation

Historical order in `build-session-context-for-prompt`:

1. E0 ensure index (build + install).
2. Load durable history for parent/linear reads.
3. E1 reset the per-prompt working set.
4. E2 canonical index append: shared-box active-leaf mutation → install
   post-append index → save index to disk.
5. E3 buffer/append the canonical user message (deferred persistence).
6. E4 apply the path-derived model setting.
7. Return the system-injected context.

The refactored function computes the pure plan (a zero-effect step) and then
applies E2 → E3 → E4 in the identical relative order, with the pure append's
box mutation applied by the caller so any pre-append index alias (e.g. config
`'session-index`) observes the same post-append active leaf as before.

The linear-context duplicate-log-read risk is unchanged in all reachable
states: when `index` is absent the session is fresh (log absent) or the index
build failed, and `buffer-or-append!` buffers rather than writes until the
session is persisted; the pre-loaded history equals the post-buffer file
content.

## Equivalence evidence

- W0 machine oracle updated: `normal-success` now records a zero-effect
  `preparation-plan` step; `context-persistence-failure` anchor points at the
  new `buffer-or-append!-fn sess` call site. All 33 path variants, 38 boundary
  semantics, and semantic digests remain enforced.
- Existing lifecycle/characterization suites remain green (see gates).
- New pure unit matrix constructs no live session and asserts:
  - exact parent selection (entries and active-leaf),
  - message canonicalization (string and message-struct input with index),
  - path-model setting,
  - system injection,
  - tiered/tree/linear context-source branching,
  - no mutation of the caller's index, history, or context lists.

## Locality measurement

Baseline `runtime/session/session-lifecycle.rkt`:
600 LOC, 11 provides, fan-out 39, hotspot 8400.

Post-W1:
- `session-lifecycle.rkt` reduced from 600 to 563 LOC (within the frozen
  600-line budget at every intermediate commit).
- Pure preparation logic now lives in a low-fan-in, I/O-free module tested
  standalone without a live session.
- A preparation change re-runs the pure unit matrix and the small lifecycle
  regression instead of the full live-session harness surface.

## MA-10 discipline

MA-10 remains **OPEN**. This wave demonstrates trace-equivalent pure extraction
with measurable locality improvement. The terminal MA-10 decision is deferred to
W4 (`#9246`), and W5 cannot close with an unassigned Critical/High finding.

## Gates

- Pure unit matrix (repeated 3×).
- Lifecycle/characterization set (W0 7 + lifecycle 61 + plan).
- Arch suite (25 files / 262 tests incl. new R-18 purity gate).
- Fast suite.
- PR CI 17/17; required policy NONE unmet.
- Independent read-only review.

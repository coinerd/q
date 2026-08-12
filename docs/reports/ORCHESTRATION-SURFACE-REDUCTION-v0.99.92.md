# Orchestration Surface Reduction — v0.99.93 W3

**Status:** HISTORICAL W3 COMPLETE — original rejection evidence retained; later terminal dispositions annotated
**Baseline:** v0.99.93 W2 merge `e13a48e6`
**Machine evidence:** `docs/architecture/orchestration-surface-reduction-v0.99.93.rktd`
**MA-10 at W3:** remained OPEN; W4 later CLOSED MA-10, and v0.99.93 #9281 extracted the separately deferred W3-F2 scope.

## Decision

**REJECTION.** After W1 (pure prompt preparation) and W2 (context-build
boundary) thinning, `runtime/session/session-lifecycle.rkt` retains no coherent
private helper whose extraction would improve locality or test amplification
without changing effect order. MA-10 explicitly allows rejecting an extraction
when the evidence does not support it.

## Locality baseline (architecture-baseline @ `e13a48e6`)

| Metric | Value |
|---|---|
| LOC | 566 (was 600 at v0.99.87; 563 at W1; 566 after W2) |
| Provides | 11 |
| Fan-in | 9 |
| Fan-out | 38 (was 39) |
| Changed commits (last 200) | 14 |
| Hotspot score | 7924 (was 8400) |
| Top co-change pairs | agent/iteration/main-loop 5, tests/test-arch-fitness 5, agent/iteration/loop-config 4, tests/helpers/iteration-loop 4, tests/test-agent-iteration-di 4 |

The W0–W2 thinning already reduced LOC 600→566, fan-out 39→38, and hotspot
8400→7924.

## Candidate classification

The six remaining inline blocks in `run-prompt!` (≈97 lines, 17% of the file)
share nearly all imports with the four provided functions; none has a unique
fan-out footprint that extraction would reduce:

| Block | Verdict | Why |
|---|---|---|
| busy-error-construction | reject | 1-param raise path of the claim primitive; pre-dynamic-wind ownership boundary (W0-F1) |
| input-hook-handling | reject | control-flow early return inside the outer dynamic-wind; caller-owned hook/event publication |
| rollback-prompt-scope | defer-to-W4 (historical W3 verdict) | only structurally coherent candidate; caller-retained save-back + needs oracle regeneration; W4 #9246 decides |
| acknowledgement-tracer | reject | one third of the terminal-identity decision (W0-F2); fragmentation without fan-out gain |
| cleanup-turn-completed | reject | order-critical safety-net terminal glued to the box protocol |
| emergency-persist | reject | 1-param guarded call to an already-extracted primitive (`ensure-persisted!`) |

Claim/release, interruption/turn ownership, and persistence primitives already
live in `session-mutation.rkt`, `session-interruption.rkt`, and
`session-persistence.rkt` respectively. The terminal #9281 follow-up now also
owns prompt/rollback parameter scope in `session-prompt-scope.rkt`.

## Historical W3 findings and disposition

These DEFERRED values record W3-time state; later terminal outcomes are stated in the follow-up column and section below.

| ID | Severity / classification | Owner | Follow-up | Observation |
|---|---|---|---|---|
| W3-F1 | Low / DEFERRED | Runtime Session | `W4 #9246 terminal decision` | Remaining complexity is orchestration glue by design; six inline blocks share nearly all imports with the four provided functions, so extraction would add module requires without reducing fan-out (38). |
| W3-F2 | Low / DEFERRED | Runtime Session | `W4 #9246 terminal decision; implemented by v0.99.93 #9281` | W3 deferred the coherent wrapper; the later terminal follow-up extracted it with save-back timing preserved. |

## Later terminal disposition (v0.99.93)

W3's historical decision and metrics remain unchanged. The separate terminal
record now marks W3-F2 **extracted** by #9281 as
`call-with-session-prompt-scope`, with direct arbitrary-value/exception tests
and coordinated lifecycle-oracle regeneration.

## MA-10 discipline

At W3, MA-10 remained OPEN and this report supplied the evidence for W4's
terminal decision. The later #9281 disposition is recorded separately rather
than rewriting the W3 verdict.

## Gates

- Rejection ledger 3/3 (RED-first validated).
- Lifecycle + agent-session + iteration-DI focused set (17 files / 96 tests):
  `test-session-lifecycle-pure/guards/hooks/errors/ws/smoke/characterization`,
  `test-agent-session-basic/cancellation/config/context/extensions/hooks/pure/tree`,
  `test-agent-iteration-di`, `test-iteration-wiring`.
- Arch suite 27 files (grew 26→27 with the new ledger test).
- Fast suite 1078 files / 15654 tests (grew 1077→1078 files, 15651→15654 tests
  with the new ledger test; no production change).
- PR CI 17/17; required policy NONE unmet.
- Independent read-only review.

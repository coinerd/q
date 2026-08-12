# Repository-wide Maintainability Reassessment — v0.99.92 W4

**Status:** IN PROGRESS — terminal ledger complete, gates pending
**Baseline:** v0.99.92 W3 merge `3773e6f8`
**Machine ledger:** `docs/architecture/maintainability-terminal-v0.99.92.rktd`
**MA-10 terminal decision:** CLOSED (trace-equivalent extraction + evidence-backed rejection)

## Terminal disposition table

| ID | Finding | Disposition | Resolved by |
|---|---|---|---|
| MA-01 | Analysis metrics partially stale | CLOSED | v0.99.87 W0/W4 |
| MA-02 | Extension exceptions stale/broad | CLOSED | v0.99.88 W2 |
| MA-03 | extensions/context.rkt session coupling | CLOSED | v0.99.88 W0–W2 |
| MA-04 | ext-package-manager Runtime coupling | CLOSED | v0.99.88 W3 |
| MA-05 | Extension–TUI bridges | CLOSED | v0.99.88 W4 |
| MA-06 | GSD domain/effects separation | CLOSED | v0.99.89 W0–W4 + v0.99.90 W0–W5 |
| MA-07 | Tracking projections can drift | PARTIAL | atomic projection + campaign repo delivered; residual HANDOFF/STATE drift → W5 hygiene sweep (#9247) |
| MA-08 | GSD external effects deterministic | CLOSED | v0.99.90 W0–W5 |
| MA-09 | Provider co-change/redundancy | CLOSED | v0.99.91 Path B W0–W4-B |
| MA-10 | session-lifecycle.rkt bundles responsibilities | **CLOSED** | v0.99.92 W0–W3 + W4 terminal decision |
| MA-11 | Agent-iteration/Runtime coupling | GUARDED | closed invariant; re-verified W4/W5 |
| MA-12 | Hidden cross-turn state | GUARDED | closed invariant; re-verified W2/W4/W5 |

## MA-10 terminal decision

MA-10 is **CLOSED**. The closure proof is satisfied by the combination of:

1. **Trace-equivalent pure extraction improved locality.** W1 extracted
   `build-prompt-preparation-plan` (pure, I/O-free, 19-test standalone matrix)
   and W2 added the `context-build` request/result boundary (pure, R-18). The
   preparation logic is now testable without a live session; `session-lifecycle.rkt`
   dropped 600 → 566 LOC, fan-out 39 → 38, hotspot 8400 → 7924.
2. **Evidence-backed rejection closed the residual candidate.** W3 documented
   that no remaining coherent private helper qualifies for extraction: the six
   inline orchestration blocks share nearly all imports with the four provided
   functions, so moving them would add module requires without reducing fan-out.
   The only structurally coherent candidate (rollback prompt-scope) is
   caller-retained save-back and was deferred to a follow-up issue (#9281).

MA-09 stays CLOSED by Path B (v0.99.91 terminal no-abstraction decision).
MA-11 and MA-12 remain guarded regression invariants through W5.

## Wave-finding dispositions

| Finding | Severity | Disposition | Follow-up |
|---|---|---|---|
| W0-F1 prompt ownership before outer dynamic-wind | High | DEFERRED | #9276 |
| W0-F2 prompt terminal identity/event split | Medium | DEFERRED | #9277 |
| W0-F3 close/active-prompt concurrency | High | SEPARATE_MILESTONE | #9278 |
| W0-F4 auto compaction start-leak / completion | Medium | DEFERRED | #9279 |
| W0-F5 retry sleep cancellation / partial metadata | Medium | DEFERRED | #9280 |
| W3-F1 orchestration glue by design | Low | CLOSED | — |
| W3-F2 rollback prompt-scope extraction | Low | DEFERRED | #9281 |

All High findings (W0-F1, W0-F3) are assigned concrete follow-up issues, so W5
can close the series with no unassigned Critical/High finding.

## Baseline repeat

`scripts/architecture-baseline.rkt --revision 3773e6f8` reproduces
`session-lifecycle.rkt` at 566 LOC / 11 provides / fan-in 9 / fan-out 38 /
13 changes / hotspot 7358, confirming the W0–W3 thinning trajectory.

## Gates

- Terminal ledger 3/3 (RED-first).
- Arch suite (MA-11/MA-12 guards).
- Fast suite.
- PR CI 17/17; required policy NONE unmet.
- Independent read-only review.

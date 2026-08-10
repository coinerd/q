# GSD Golden Workflow Traces — v0.99.89 W0

**Wave:** v0.99.89 W0 (#9226, milestone #876)
**Scope:** Roadmap v0.99.89 W0 — "Golden Workflow Traces"
**Date:** 2026-08-10
**Status:** IMPLEMENTED — 15/15 golden tests green; workflows + fast gates

## Purpose

Pin the deterministic semantic behavior of the GSD workflows before any
domain decomposition. This trace matrix is the **refactoring oracle** for
W1 (pure transition kernel), W2 (plan/state projection kernel), W3 (command
parsing & intent boundary) and W4 (facade thinning): behavioral equivalence
after each refactor is proven by comparing normalized traces.

## Oracle design

`tests/helpers/gsd-golden-trace.rkt` captures one normalized trace per
scenario through the **production campaign machinery**
(`execute-campaign-request!` → `run-campaign!` → `run-campaign-wave` →
`try-complete-wave!`), with the FSM driven exactly like the real `/go`
handler callbacks (`executing` before the run, `verifying` before
completion). Each trace covers:

| Dimension | Source |
|---|---|
| Commands | scenario's semantic command sequence |
| FSM transitions | context history (chronological `(from to)` pairs) |
| Campaign record | normalized durable `.rktd` (plan-id, fence, provenance, per-wave status/attempts/attempt-id/fence) |
| Projections | PLAN.md index statuses, wave doc statuses, STATE.md rows, VALIDATION.md rows, plan overall |
| Completion outbox | committed completion event ids (deduped) |
| Campaign result | final status + waves completed in this run |
| Event order | ordered `gsd.transition.*` events (attempted/succeeded per hop, with `(from to)` payloads) |

**Nondeterminism**: timestamps, correlation ids, session ids, random temp
names and lease owners never enter the trace — by construction (semantic
projection, not sanitized dump). `plan-id` is deterministic (SHA-256 of the
manifest over fixed fixture content). Determinism is itself asserted:
scenarios run twice must yield identical traces.

## Scenario matrix (tests/workflows/gsd/test-gsd-golden-traces.rkt, 15/15)

| Scenario | Pinned semantics |
|---|---|
| plan-creation | planning turn → PLAN/STATE/VALIDATION + wave docs + seeded campaign (all pending, 0 attempts, provenance plan-and-state); no events; no outbox |
| go-success | both waves complete → campaign-complete; PLAN/STATE/wave docs DONE; VALIDATION untouched (pending); 2 outbox events; FSM auto-routes idle→exploring→plan-written→executing→verifying→executing→verifying; final mode verifying |
| go-verifier-reject | verifier-first: DONE never committed without approval; wave FAILED, outbox empty |
| go-failure | runner error → wave FAILED, no advancement, projections FAILED, FSM stops in executing (no verifier ran) |
| go-interruption | runner cancellation → INTERRUPTED in the durable record only; projections stay pre-completion (no mark-wave-status! for interrupted) |
| retry-interrupted | interrupted wave re-attempted → attempt-2 under fence-2, W1 attempted once under fence-3; both DONE |
| campaign-resume | durable record carries truth across processes; W1 failed in run 1 retried as attempt-2/fence-3 in run 2; per-run completed = (1) |
| replan | rewritten plan → new plan-id, fresh campaign identity, old record file preserved |
| milestone-close | all waves done → campaign-complete, plan overall all-done, 2 outbox events |
| crash-between-commit-and-projection | W0 durably DONE + outbox committed, W1 FAILED; projections restored to pre-completion (crash before projection update) |
| crash-resume | resume completes W1 but the stale W0 projections are NOT repaired; plan overall stays partly-done despite durable both-done |

## Failure injection (roadmap requirement)

"Failure Injection für Crash zwischen Commit und Projektion": the crash
scenario commits W0 via the production path (verifier-first DONE + outbox
event + persist), then simulates the crash by restoring the deterministic
fixture projections to their pre-completion state — exactly the state a
process leaves behind when it dies between `persist-campaign!` and
`mark-wave-status!`/`update-state-table!`. The resume trace documents the
current behavior: **stale projections persist after recovery** (W2's atomic
effect shell must make the crash trace equivalent to the no-crash trace).

## Findings recorded by the oracle (current behavior, pre-W2)

1. Interruption updates only the durable record — PLAN/STATE/wave docs stay
   pre-completion until the wave completes or fails.
2. A crash between commit and projection leaves PLAN/STATE/wave docs stale,
   and resume does not repair them; plan overall reflects the stale
   projection, not the durable record.
3. `campaign-result` `completed` reflects waves completed in the current
   run, not the whole campaign.
4. The FSM ends in `verifying` after a successful campaign (no transition
   back to `idle` in the campaign path).

These are intentional pins for W1–W4; W2 changes #2 by construction, and
the golden expectations will be updated in that wave with a dated
amendment.

## Gates

| Gate | Result |
|---|---|
| golden matrix (test-gsd-golden-traces.rkt) | ✅ 15/15 |
| workflows suite | ✅ 29 files / 161 tests |
| fast suite | (run) |
| lint-format | (run) |

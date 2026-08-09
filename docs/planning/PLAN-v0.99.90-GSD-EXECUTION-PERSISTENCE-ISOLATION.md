# Plan: v0.99.90 — GSD Execution, Persistence & Campaign Reliability Isolation

**Status:** FROZEN — NOT STARTED
**Authority:** roadmap + v0.99.87 freeze contract
**Plan-ID / Hash:** generated at campaign start
**Dependency:** v0.99.89 released
**GitHub:** milestone #877; waves #9231–#9236
**Findings:** MA-06, MA-07, MA-08

## Goal

Concentrate effectful GSD work behind cohesive ports/executors and keep campaign truth plus all human projections logically atomic across interruption and retry.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Port Inventory and Composition Root | Inventory FS/git/GitHub/clock/process/event effects; cohesive ports | DI/contracts + Fast; one port per coherent external domain |
| W1 | Campaign Repository Boundary | Encapsulate `.rktd` persistence/migration fail-closed | persistence + Security + Fast; old fixtures compatible |
| W2 | Atomic Projection Transaction | Campaign record + PLAN/STATE/VALIDATION/Wave as one logical transaction | Broad + Governance + failure injection; no invented DONE/skip |
| W3 | Wave Executor Isolation | Explicit structured outcomes for tools/subagents/cancel/timeout/interrupt | executor/orchestrator + Fast; exactly-once completion |
| W4 | GitHub/Release Side-Effect Adapter | Idempotent correlated external commands; dry-run default | adapter contracts + Security + Workflow + Fast; no duplicates |
| W5 | End-to-End Recovery and Release | Every success/failure/interruption/restart/persisted transition | Broad + Arch + Security + Workflow + Smoke + Release + review |

**Broad gates:** W2 and W5. Fast + independent review every wave.

## Amendment policy

Immutable after start; substantive change requires amendment, new hash, controlled campaign migration, and retained MA history.

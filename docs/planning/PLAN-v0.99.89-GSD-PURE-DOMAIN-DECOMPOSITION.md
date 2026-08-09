# Plan: v0.99.89 — GSD Pure Domain Decomposition

**Status:** FROZEN — NOT STARTED
**Authority:** roadmap + v0.99.87 freeze contract
**Plan-ID / Hash:** generated at campaign start
**Dependency:** v0.99.88 released
**GitHub:** milestone #876; waves #9226–#9230
**Findings:** MA-06, MA-07 (intermediate closure)

## Goal

Isolate GSD planning/state logic as an explicit pure domain core while preserving `/go`, persistence formats, projections, event order, and public facades.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Golden Workflow Traces | Pin plan/go/success/failure/interruption/retry/replan/resume/close semantics | GSD workflow + Fast; deterministic semantic traces complete |
| W1 | Pure Transition Kernel | Pure transitions/preconditions/terminal states; facade delegates | pure-kernel + Arch + Fast; no FS/GitHub/Runtime/event-bus imports |
| W2 | Plan/State Projection Kernel | Pure complete projection plan + atomic effect shell | GSD governance/workflow + Broad; crash cannot leave stale projections |
| W3 | Command Parsing and Intent Boundary | I/O-free parser intent vs executor; preserve `/go N` assertion | parser fitness + command corpus + Fast |
| W4 | Facade Thinning and Release | Composition/re-export only; consumer-proven wrapper deletion | Broad + Arch + Workflow + Smoke + Release + review; Golden Traces equivalent |

**Broad gates:** W2 and W4. Fast + independent review every wave.

## Amendment policy

Immutable after start; changes require dated amendment, new hash, controlled migration, and preserved MA history.

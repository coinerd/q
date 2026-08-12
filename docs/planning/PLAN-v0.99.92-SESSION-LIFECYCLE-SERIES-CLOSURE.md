# Plan: v0.99.93 — Session Lifecycle Thinning, Reassessment & Series Closure

**Status:** FROZEN — NOT STARTED
**Authority:** roadmap + v0.99.87 freeze contract
**Plan-ID / Hash:** generated at campaign start
**Dependency:** v0.99.91 Path B released
**GitHub:** milestone #879; waves #9242–#9247
**Findings:** MA-10 terminal; MA-11/MA-12 guards; final MA-01–MA-12 reassessment

## Goal

Extract only coherent pure prompt/context responsibilities from lifecycle orchestration, prove effect/trace equivalence, reassess maintainability, and close the series without optimizing LOC alone.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Lifecycle Trace and Responsibility Map | Trace normal/error/cancel/close/retry/compaction; no production change | characterization + Fast; complete ordered effect/event map |
| W1 | Pure Prompt Preparation Extraction | Pure input/config → preparation plan only | lifecycle/prompt + Arch + Fast; effect order unchanged |
| W2 | Context-Build Request/Result Boundary | Explicit request/result; Context Assembly runtime-owned, state session-owned | Broad + rollback/session ownership/context assembly |
| W3 | Orchestration Surface Reduction | Move only coherent private helpers; measure locality/test amplification | lifecycle + agent-session + iteration DI + Fast |
| W4 | Repository-wide Maintainability Reassessment | Repeat baseline; classify every finding CLOSED/PARTIAL/OPEN/REJECTED | architecture/document review + Fast |
| W5 | Series Closure and Release | 100% traceability, synchronized projections, public release | Broad + Arch + Security + Workflow + Smoke + Release + Manifest/Bundle + Main CI + review |

**Broad gates:** W2 and W5. `session-lifecycle.rkt` may not exceed its 600-line budget at any intermediate point; success requires better locality/testability, not file movement.

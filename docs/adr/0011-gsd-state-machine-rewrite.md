# ADR-0011: GSD State Machine Rewrite

Date: 2025-01 (v0.22.4)
## Status
Accepted

## Context
The GSD (Getting Stuff Done) planning system managed wave state through ad-hoc flag checks and imperative mutations scattered across multiple modules. This made it difficult to reason about valid state transitions, debug wave execution failures, and add new features without regressions.

## Decision
Rewrote the GSD wave executor as an explicit state machine with defined states (idle, planning, executing, reviewing, done) and validated transitions. The `gsd/state-machine.rkt` module provides the core machine, while `gsd/wave-executor.rkt` drives it.

## Consequences
- All wave state transitions are explicit and validated
- Invalid transitions raise clear errors instead of silently corrupting state
- Easier to add new states (e.g., paused, blocked) in future
- Test coverage improved from ~60% to ~90% for wave lifecycle

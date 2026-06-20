# Abstraction Inventory Summary — v0.99.35

**Date:** 2026-06-20  
**Source HEAD:** `b709f7ee`  
**Full inventory:** `.planning/ABSTRACTION-INVENTORY-v0.99.35.md` (local-only)

## Scorecard

| Module | Lines | Reward | Risk | Class | Wave |
|--------|-------|--------|------|-------|------|
| spawn-subagent.rkt | 809 | 14 | 7 | GREEN | W2 |
| event-structs + event-json | 562+359 | 14 | 6 | GREEN | W4 |
| gsd/state-machine.rkt | 586 | 13 | 6 | GREEN | W5 |
| subprocess.rkt | 369 | 11 | 5 | GREEN | W3 |
| anthropic.rkt | 662 | 13 | 8 | YELLOW | W8 |
| tui/core-handlers.rkt | 549 | 12 | 9 | YELLOW | W7 |
| state-aware-builder.rkt | 584 | 11 | 11 | YELLOW | W6 |
| run-tests.rkt | 592 | 12 | 9 | YELLOW | Doc |
| cli/args.rkt | 634 | 8 | 12 | RED | Audit-only |
| run-modes.rkt | 621 | 7 | 12 | RED | Audit-only |

## Key Findings

- **4 GREEN** candidates (low risk, high reward): spawn-subagent, event-structs, gsd/state-machine, subprocess
- **4 YELLOW** candidates: anthropic, tui-handlers, context-assembly, run-tests (document only)
- **2 RED** candidates: cli/args, run-modes (audit-only, do not touch)
- **243 parameter usages** across codebase — idiomatic, no action needed
- **13 macro usages** — all domain-specific, appropriate
- **No call/cc usage** — no continuation concerns

## Cross-cutting Observations

- Event protocol (structs + JSON) already has good separation; needs round-trip tests
- GSD state-machine has pure transition functions mixed with mutable state holders
- spawn-subagent has highest change amplification (28 imports, 25 exports)
- subprocess pipe draining was already fixed in v0.99.34; needs boundary documentation
- anthropic.rkt already exports pure parse functions for testing

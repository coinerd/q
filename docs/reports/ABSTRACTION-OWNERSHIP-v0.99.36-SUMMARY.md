# Abstraction Ownership Inventory Summary — v0.99.36

**Date:** 2026-06-21  
**Source HEAD:** `575affa8`  
**Full inventory:** `.planning/ABSTRACTION-OWNERSHIP-INVENTORY-v0.99.36.md` (local-only)

## Purpose

v0.99.36 moves from pure-helper extraction (v0.99.36) to abstraction
**ownership discipline**: API surface boundaries, information hiding,
expected-failure design, port/I/O abstraction, parameter ownership,
and macro/DSL safety.

## Scorecard

| Candidate | Reward | Risk | Class | Wave |
|-----------|--------|------|-------|------|
| Version surface registry | 17 | 7 | GREEN | W2 |
| Abstraction-audit red-flag signals | 16 | 5 | GREEN | W1 |
| README/version port formatting | 15 | 7 | GREEN | W6 |
| API/struct-out inventory | 15 | 4 | GREEN | W3 |
| README status result struct | 14 | 6 | GREEN | W5 |
| Parameter ownership map | 13 | 4 | GREEN | W4 |
| Macro/DSL safety tests | 12 | 5 | GREEN | W9 |
| TUI/session invariant tests | 11 | 4 | GREEN | W8 |
| Release boundary hardening | 19 | 9 | YELLOW | W2 |
| Version drift result struct | 17 | 8 | YELLOW | W5 |
| CLI parsed command model | 20 | 16 | RED | W7 (design only) |
| Run-mode request interpreter | 20 | 17 | RED | W7 (design only) |
| TUI state representation rewrite | 18 | 16 | RED | Defer |
| Test-runner ledger refactor | 20 | 12 | RED | Defer |

## Key Findings

### Version Knowledge Duplication (W2 target)
4 scripts independently parse `(define q-version "...")` from
`util/version.rkt` with separate regex implementations. Version surface
knowledge (which files contain version strings) is not centralized.

### struct-out Usage (W3 target)
60 `struct-out` forms across production modules. Mostly appropriate
(protocol types, config records). Only 2 low-risk explicit-export
changes recommended.

### Parameter Usage (W4 target)
Mostly idiomatic. Top consumer: `scripts/run-benchmark.rkt` (12 params).
7-parameter modules need ownership documentation. No anti-patterns found.

### I/O/Logic Mixing (W5/W6 target)
Release tooling (lint-version, sync-version, metrics) has high I/O
coupling with validation logic. Good pilot area for port abstraction.

### Macro/DSL Safety (W9 target)
3 complex macros (`define-typed-event`, `define-fsm-machine`,
`define-q-extension`) lack dedicated expansion tests. 9 simple
`define-syntax-rule` macros are likely safe.

## RED Modules (Do Not Edit)

| Module | Reason |
|--------|--------|
| cli/args.rkt (634L) | Risk 16 — design only in W7 |
| wiring/run-modes.rkt (621L) | Risk 17 — design only in W7 |
| agent/event-structs.rkt (562L) | Protocol-critical, 22 exports |
| scripts/run-tests.rkt (592L) | Gate-critical infrastructure |

## Wave Implementation Plan

- **W0-W4, W7, W9**: Audit/tooling/docs — minimal production changes
- **W2, W5, W6**: Release tooling improvements — characterization tested
- **W3**: ≤2 low-risk struct-out explicit exports
- **W8**: TUI/session invariant tests + narrow cleanup
- **W10**: Final gates, docs, version/changelog/metrics

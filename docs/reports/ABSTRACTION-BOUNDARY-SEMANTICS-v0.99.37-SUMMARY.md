# Abstraction Boundary-Semantics Inventory — v0.99.37 W0 Summary

**Date:** 2026-06-22
**Wave:** W0 (#8440)
**Baseline:** `main@15c1b043`
**Total modules scanned:** 695

---

## Key Counts

| Signal | Production Module Count |
|--------|------------------------|
| `contract-out` | 347 |
| `struct-out` | 40 |
| `all-defined-out` | 1 (`browser/events.rkt`) |
| `jsexpr`/`write-json`/`read-json` | 99 |
| `make-hash` | 22 |
| `file->string`/`file->lines` | 53 |
| `subprocess` | 30 |
| `current-directory` | 54 |
| String parsing (`regexp-match`/`string-split`) | 187 |
| `make-parameter` clusters | 9 clusters (78 modules, 189 sites) |

## Risk-Classified Candidates

### GREEN (7 areas — implement)
- W2: Benchmark timing assertion cleanup (reward 18, risk 6)
- W1: Scanner v3 signal additions (reward 16, risk 5)
- W3: Serialization round-trip tests (reward 17, risk 7)
- W6: Port abstraction for report writer (reward 16, risk 7)
- W8: TUI reducer/model tests (reward 14, risk 5)
- W7: Parameter dynamic-context tests (reward 12, risk 4)
- W9: Data structure encapsulation report (reward 12, risk 3)

### YELLOW (2 areas — design-it-twice)
- W4: Expected-failure/result boundary (reward 17, risk 9)
- W3 variant: Session store round-trip tests (reward 17, risk 10)

### RED (5 areas — design docs only)
- cli/args.rkt, wiring/run-modes.rkt, scripts/run-tests.rkt, agent/event-structs.rkt
- Auth-store result boundary (security-sensitive)

## Critical Finding

**3 strict wall-clock timing assertions** in `tests/test-bench-streaming-render.rkt`
(lines 101, 127, 146) can cause false broad-gate failures under scheduler noise.
This caused the #8435 remediation cycle. W2 is P0 priority.

Full inventory: `.planning/ABSTRACTION-BOUNDARY-SEMANTICS-INVENTORY-v0.99.37.md`

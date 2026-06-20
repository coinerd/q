# Abstraction Red Flags — v0.99.37

**Date:** 2026-06-22
**Scanner version:** v3 (v0.99.37 W1)
**Modules scanned:** 694

---

## New v3 Signals

### 1. Mutable Cache Usage (make-hash / hash-set!)

Top modules with mutable cache operations:

| Count | Path |
|-------|------|
| 23 | runtime/session-index/mutations.rkt |
| 14 | extensions/manifest.rkt |
| 12 | util/event/event-macro.rkt |
| 9 | runtime/context-assembly/conclusion-graph.rkt |
| 9 | wiring/run-json-rpc.rkt |
| 8 | gui/components/streaming-cursor.rkt |
| 8 | runtime/session/session-store-tree.rkt |

**Advisory:** Mutable caches shared across modules risk data-flow obscuration.
W9 will classify each as hidden, semi-exposed, or leaked.

### 2. Benchmark Timing Assertions (current-inexact-milliseconds)

Top modules using wall-clock timing:

| Count | Path |
|-------|------|
| 10 | tui/commands/extension.rkt |
| 8 | llm/stream.rkt |
| 7 | tui/tui-render-loop.rkt |
| 6 | extensions/gsd/events.rkt |
| 6 | runtime/tool-coordinator.rkt |
| 6 | scripts/run-tests.rkt |

**Advisory:** Wall-clock timing in production code is fragile under scheduler noise.
W2 will address benchmark-gate semantics.

### 3. Event Handler Modules (handler/model boundary risk)

Top modules with handler-like definitions:

| Count | Path |
|-------|------|
| 23 | runtime/session/session-switch.rkt |
| 22 | ui-core/dispatch.rkt |
| 17 | agent/loop-stream.rkt |
| 16 | skills/resource-loader.rkt |
| 14 | tools/scheduler.rkt |
| 13 | runtime/turn-orchestrator.rkt |

**Advisory:** Modules with high handler density should be checked for handler/model
separation. W8 will audit TUI handler thinness.

### 4. Ad-Hoc String Parsing (top 20)

| Count | Path |
|-------|------|
| 26 | scripts/run-tests/parse.rkt |
| 20 | scripts/run-tests/classify.rkt |
| 19 | runtime/session-index/mutations.rkt |
| 16 | scripts/abstraction-audit.rkt |
| 15 | runtime/session/session-switch.rkt |

**Advisory:** High ad-hoc parsing density suggests unowned parsing logic.

---

## Existing v2 Signals (unchanged)

- `all-defined-out`: 1 module (browser/events.rkt)
- `struct-out`: 40 modules
- I/O mixed with logic: 53 modules
- Serialization hotspots: detected across 17+ modules

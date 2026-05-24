# F2 Contract Precision Trend Report

Generated after contract precision series completion.

## Trend

| Phase | any/c | Files | Change | Key Action |
|-------|-------|-------|--------|------------|
| Baseline A | 882 | 148 | — | Initial measurement |
| Baseline B | ~882 | 148 | 0% | Characterization only |
| Exact baseline | 846 | 144 | -4.1% | Eliminated false positives |
| Runtime precision | 846 | 144 | 0% | Runtime contract precision |
| Util/tools slice | 813 | 144 | -3.9% | util/tools contract tightening |
| TR migration | 812 | 143 | -0.1% | Typed Racket migration |

## Net Reduction

**Baseline A → current: 882 → 812 = -70 any/c (-7.9%)**
**Files with any/c: 148 → 143 = -5 files**

## Top Remaining Concentrations

| Module | any/c | Category |
|--------|-------|----------|
| agent/loop-phases.rkt | 60 | Core loop (high churn) |
| extensions/racket-tooling-helpers.rkt | 53 | Extension helpers |
| extensions/github/helpers.rkt | 28 | GitHub API |
| runtime/session-index/query.rkt | 25 | Session queries |
| runtime/context-assembly.rkt | 21 | Context assembly |
| extensions/ui-surface.rkt | 20 | UI surface |
| tools/model-bridge.rkt | 4 | Was 19, now precise |
| util/tree-entries.rkt | 6 | Was 15, now precise |
| tools/scheduler-strategy.rkt | 13 | Was 15, partially tightened |

## Analysis

- **Core loop** (loop-phases.rkt, 60 any/c): High churn, genuine polymorphism
- **Extension helpers** (53+28+20 = 101 any/c): Extension boundary flexibility required
- **Runtime** (session-index, context-assembly): Good candidates for next wave

## Widened Ledger Status

27 open entries, 0 overdue. All widened contracts tracked.
No invisible regression.

## Recommendations for next phase

1. **runtime/session-index/query.rkt** (25 any/c) — stable, struct-heavy
2. **runtime/context-assembly.rkt** (21 any/c) — mixed types, medium risk
3. **tui modules** (clipboard, theme, state-types) — if TUI stabilizes

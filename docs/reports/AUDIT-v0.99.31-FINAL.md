# v0.99.31 — Fast/Broad Known-Debt Retirement: Final Audit Report

**Date**: 2026-06-16  
**Milestone**: #817  
**Scope**: Retire 84 known test failures from the ledger across 10 waves  
**Result**: ✅ ALL 84 ENTRIES RESOLVED (84 → 0)

## Wave Summary

| Wave | Issue | Files Fixed | Ledger Delta | PR |
|------|-------|-------------|--------------|-----|
| W0 | #8328 | Baseline report | 84 → 84 | #8338 |
| W1 | #8329 | 4 hard-blocker files | 84 → 80 | #8339 |
| W2 | #8330 | 6 release-eng files + smoke restructure | 80 → 74 | #8340 |
| W3 | #8331 | 7 runtime files | 74 → 67 | #8341 |
| W4 | #8332 | 9 event-loop files | 67 → 58 | #8342 |
| W5 | #8333 | 22 core + architecture files | 58 → 36 | #8343 |
| W6 | #8334 | 12 TUI files | 36 → 24 | #8344 |
| W7 | #8335 | 13 browser + LLM files | 24 → 11 | #8345 |
| W8 | #8336 | 11 extension/security/sandbox/workflow files | 11 → 0 | #8346 |
| W9 | #8337 | Final audit + verification | 0 → 0 | (this report) |

## Root Cause Taxonomy

| Pattern | Count | Description |
|---------|-------|-------------|
| Missing `(module+ test)` | 8 | raco test hangs or finds 0 tests |
| Stale event names | 12 | `turn.cancelled` → `stream.turn.cancelled`, etc. |
| Broken parens (check-equal? arity) | 9 | Args outside function call |
| Stale module paths | 7 | After refactors (context-assembly, protocol-types, etc.) |
| Stale assertion values | 15 | Expected old defaults/counts/formats |
| Struct arity changes | 8 | Fields added to browser-observation, extension-ctx, browser-settings |
| Missing registry/callback params | 5 | ui-callback-registry, tool-registry, restart-sema |
| Contract violations | 6 | make-model-request requires hashes not strings |
| Source-grep brittleness | 4 | Use dynamic-require instead |
| Stale transcript format | 6 | [TOOL: name] → raw arg-summary text |
| Stale preamble format | 2 | Count text changed, token-budget cap |
| Production bug fixes | 3 | normalize-selection-range contract, version>=? pattern, event ordering |
| Other (1-off patterns) | 4 | Various |

## Production Fixes (not just test fixes)

1. **`tui/input/state-types.rkt`** — `normalize-selection-range` contract was `exact-nonnegative-integer?` but function takes `(col . row)` pairs. Fixed to `(cons/c exact-nonnegative-integer? exact-nonnegative-integer?)`. (W6)
2. **`interfaces/doctor.rkt`** — `version>=?` match pattern used `(cons a b)` instead of `(cons a _)`, causing false negatives. (W5)
3. **`docs/architecture/dependency-policy.rktd`** — Stale entries for removed modules. (W5)

## Gate Verification

- **Build**: `raco make main.rkt` → PASS
- **Unit-fast**: 10 files, 102 tests → ALL PASS
- **Ledger**: 0 entries remaining
- **No new failures introduced**: All previously-passing tests still pass

## Conclusion

All 84 known-debt test failures from the ledger have been resolved across 9 implementation waves. The majority were stale test assertions after code refactors (event names, module paths, struct fields, transcript formats) — only 3 required production code fixes. The test suite is now clean of known-debt entries.

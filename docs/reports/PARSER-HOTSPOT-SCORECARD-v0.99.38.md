# Parser/String Hotspot Scorecard — v0.99.38 W3

**Date:** 2026-06-23
**Issue:** #8472
**Base:** `main@4bf97115`

## Methodology

Systematic grep of all non-test `.rkt` files for `regexp`, `string-split`, `string-contains?`, `string-trim`, and parser-related `define` forms. Parser modules categorized by function purity (pure vs I/O-entangled) and test coverage level.

## Parser Module Inventory

### P1: scripts/run-tests/parse.rkt (198 lines)
**Risk:** Low (already extracted to library module)
**Pure functions:** 8 — `parse-raco-output`, `classify-test-result`, `extract-failure-lines`, `truncate-test-output`, `normalize-counts`, `effective-exit-code`, `test-result->jsexpr`, `bytes->string*`
**I/O functions:** 0 (all pure!)
**Existing tests:** 234 checks in test-run-tests.rkt
**New tests:** 37 checks in test-parser-hotspots.rkt

### P2: scripts/sync-readme-status.rkt (207 lines)
**Risk:** Medium (pure functions locked behind `(main)` at module level)
**Pure functions:** 6 — `normalize-entry`, `find-status-section-range`, `find-status-version`, `find-status-entry-line`, `insert-status-entry`, `replace-status-entry`
**I/O functions:** `read-version` (reads file), `parse-changelog-top-entry` (reads file), `main`
**Existing tests:** 16 checks — ALL integration (subprocess-based, no unit tests for pure functions)
**Coverage gap:** Pure functions testable in isolation but blocked by `(main)` side-effect. Same pattern as metrics.rkt.

### P3: skills/frontmatter.rkt (249 lines)
**Risk:** Low (library module, well-tested)
**Pure functions:** 10 — `parse-skill-frontmatter`, `parse-skill-frontmatter-extended`, `valid-skill-name?`, `parse-fm-inline-array`, `parse-fm-kv`, `parse-fm-mapping`, `parse-fm-children`, `parse-fm-list`, `parse-fm-list-item`, `validate-frontmatter`
**I/O functions:** 0 (all pure!)
**Existing tests:** 46 checks in test-frontmatter-extended.rkt
**Assessment:** Good coverage, no gaps identified.

### P4: scripts/metrics.rkt (253 lines)
**Risk:** Medium (pure predicates locked behind `(main)`)
**Pure functions:** 3 — `path-under-tests?`, `rkt-file-keep?`, `git-tracked-rkt-files` (I/O but structured)
**I/O functions:** `line-count`, `assertion-count`, `version-string`, `main`
**Existing tests:** 7 checks in test-metrics-sync-all.rkt — integration only
**New tests:** 8 checks in test-parser-hotspots.rkt (predicates replicated locally)

### P5: scripts/run-tests/parse.rkt → classify-test-result (same module as P1)
**Risk:** Low
**Note:** All 12 classification categories now have unit tests in test-parser-hotspots.rkt

## Key Findings

### Finding F1: FAILURE-END regex bug (MEDIUM)
**Module:** scripts/run-tests/parse.rkt, line 122
**Code:** `(define FAILURE-END #rx"^-{20,}$")`
**Problem:** In Racket `#rx` mode, `{20,}` is treated as literal characters, not a quantifier. The regex never matches. As a result, `extract-failure-lines` includes ALL lines after the first FAILURE marker until end of input.
**Fix:** Change `#rx` to `#px` (PCRE mode supports `{n,}` quantifiers)
**Impact:** Low — `extract-failure-lines` is only used for display, not for pass/fail decisions. But it means failure context in test reports includes unnecessary trailing output.
**Recommendation:** Schedule fix in a future wave (risk ≤5, reward 10).

### Finding F2: Script-module testability barrier (STRUCTURAL)
**Modules affected:** sync-readme-status.rkt, metrics.rkt, lint-version.rkt (all script modules with `(main)` at module level)
**Problem:** Pure functions embedded in script modules cannot be `require`d without executing the script's `(main)`. This forces integration-only testing (subprocess) and prevents unit-level testing of pure parse logic.
**Existing workaround:** Tests replicate the pure function logic locally (as done in this wave for metrics predicates).
**Long-term fix:** Extract pure functions into library modules that scripts `require` (same pattern as parse.rkt extraction from run-tests.rkt in v0.96.16).
**Recommendation:** Schedule in W5–W7 (contracted boundary wave).

### Finding F3: Frontmatter parser edge cases (LOW)
**Module:** skills/frontmatter.rkt
**Note:** The parser handles quoted strings, inline arrays, nested maps, and lists. Edge cases with deeply nested structures (>3 levels) are untested but unlikely in practice (skill frontmatter is typically 1-2 levels deep).

## Test Coverage Summary

| Module | Pure Fns | Unit Tests | Integration Tests | New Tests (W3) |
|--------|----------|------------|-------------------|----------------|
| parse.rkt | 8 | 234 (existing) | via test-run-tests.rkt | 37 |
| sync-readme-status.rkt | 6 | 0 (blocked) | 16 | 0 (documented gap) |
| frontmatter.rkt | 10 | 46 | 0 | 0 (adequate) |
| metrics.rkt | 3 | 8 (replicated) | 7 | 8 |
| **Total** | **27** | **288** | **23** | **45** |

## Recommendations

1. **Immediate (W3):** ✅ Done — 37 unit tests for parse.rkt, 8 for metrics predicates
2. **F1 fix:** Change `#rx` to `#px` for FAILURE-END regex (schedule in W4 or later)
3. **F2 structural:** Extract pure functions from script modules into library modules (W5–W7)
4. **F3 monitoring:** No action needed — frontmatter coverage is adequate

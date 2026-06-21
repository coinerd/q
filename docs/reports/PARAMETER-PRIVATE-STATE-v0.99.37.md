# Parameter / Private-State Ownership Follow-Up — v0.99.37

**Date:** 2026-06-23
**Wave:** W7 (#8447)
**Risk Assessment:** GREEN (reward 12, risk 4)
**Scope:** Documentation + dynamic-context tests — minimal production code changes

## 1. Executive Summary

This report is a delta from the v0.99.36 W4 ownership report
(`PARAMETER-OWNERSHIP-v0.99.36.md`). It covers:

1. **New I/O parameter pattern** established in v0.99.36 W6 (lint-version-io.rkt)
   and v0.99.37 W6 (abstraction-audit.rkt)
2. **Dynamic-context test coverage** — new test file validates parameterize
   isolation, nesting, restoration, and thread-safety semantics
3. **Updated anti-pattern tracking** — mutable-state parameters still flagged

**Total `make-parameter` sites (production, excluding tests):** ~196 (+7 since v0.99.36)
**Total `parameterize` write sites (production):** ~62 (+3 since v0.99.36)

## 2. New Pattern: I/O Effect-Shell (v0.99.36 W6, v0.99.37 W6)

### Pattern H: I/O Effect-Shell Parameter

**Count:** 5 parameters across 2 modules
**Shape:** `(make-parameter <builtin-io-fn>)`
**Purpose:** Wrap built-in I/O functions in parameters to enable test-time
substitution with mock readers, following the effect-shell pattern.

| Parameter | Module | Default | Wave |
|-----------|--------|---------|------|
| `current-lint-file-exists?` | scripts/lint-version-io.rkt | `file-exists?` | v0.99.36 W6 |
| `current-lint-file->string` | scripts/lint-version-io.rkt | `file->string` | v0.99.36 W6 |
| `current-lint-file->lines` | scripts/lint-version-io.rkt | `file->lines` | v0.99.36 W6 |
| `current-lint-read-md-paths` | scripts/lint-version-io.rkt | `#f` | v0.99.36 W6 |
| `current-audit-file->lines` | scripts/abstraction-audit.rkt | `file->lines` | v0.99.37 W6 |

**Assessment:** This is the healthiest pattern in the codebase. Each parameter:
- Has a real-I/O default that preserves production behavior
- Is consumed by a thin shell function that delegates to a pure core
- Enables instant, filesystem-free tests via `parameterize`
- Never leaks its mock state outside test scope

**Expansion candidates:**
- `scripts/metrics.rkt` (reads files for module counting)
- `scripts/sync-version.rkt` (reads/writes version files)
- `scripts/lint-widened-ledger.rkt` (reads ledger files)

## 3. Dynamic-Context Test Coverage (W7 Deliverable)

### 3.1 What Was Under-Tested

The v0.99.36 report documented 5 ownership patterns but had **zero tests**
validating the actual parameter semantics that make the patterns work:

- **Thread-locality:** Does `parameterize` properly isolate per-thread?
- **Nesting:** Do inner `parameterize` forms shadow outer ones?
- **Restoration:** Does the parameter value restore after `parameterize` exits?
- **Direct mutation:** Does `(param new-value)` persist across all scopes?
- **I/O parameter isolation:** Do mock readers properly isolate between tests?

### 3.2 New Test File: `tests/test-parameter-dynamic-context.rkt`

Created `tests/test-parameter-dynamic-context.rkt` with the following suites:

| Suite | Tests | Validates |
|-------|-------|-----------|
| parameterize-isolation-tests | 3 | parameterize scopes don't leak to parent |
| parameterize-nesting-tests | 2 | inner parameterize shadows outer |
| parameterize-restoration-tests | 2 | value restores after parameterize exits |
| direct-mutation-tests | 2 | direct mutation persists (anti-pattern demonstration) |
| thread-locality-tests | 2 | child threads inherit parameterize values; parent unaffected by child |
| io-parameter-isolation-tests | 3 | I/O mock parameters isolate correctly |

**Total:** 14 tests covering the dynamic-context contract.

## 4. Updated Anti-Pattern Summary

### 4.1 Unchanged from v0.99.36

| Anti-Pattern | Parameters | Risk | Status |
|--------------|-----------|------|--------|
| Mutable state via direct mutation | `current-loop-warning-count`, `current-rollback-action-log` | Medium | Unchanged — still relies on single-threading assumption |
| Unwired feature flag | `current-fuzzy-edit-enabled?` | Low | Unchanged |

### 4.2 Improved Since v0.99.36

| Improvement | Details |
|-------------|---------|
| I/O effect-shell pattern | 5 parameters now use the testable I/O pattern |
| Pure audit-content extraction | `audit-content` function enables filesystem-free analysis tests |
| Contract boundary hardening | `version-surface.rkt`, `status-result.rkt`, `lint-version-io.rkt` now have contract-out (W5 v0.99.37) |

### 4.3 Parameter Ownership Boundary Matrix

| Pattern | Write Authority | Mutability | Thread-Safe? |
|---------|----------------|------------|-------------|
| A: Feature Flag | session-config.rkt | parameterize | Yes |
| B: Config Value | session-config.rkt / defaults | parameterize | Yes |
| C: DI Callback | Test scope | parameterize | Yes |
| D: Mutable State | Direct mutation | `(param val)` | **No** |
| E: Registry | Test scope / module init | parameterize | Yes |
| F: CLI Flag | command-line | parameterize | Yes |
| G: Identity | Loop/session init | parameterize | Yes |
| H: I/O Shell | Test scope | parameterize | Yes |

**Only Pattern D is thread-unsafe.** All other patterns use `parameterize`
correctly.

## 5. Parameter Density Delta (v0.99.36 → v0.99.37)

| Metric | v0.99.36 | v0.99.37 | Delta |
|--------|---------|---------|-------|
| Total make-parameter sites | 189 | ~196 | +7 |
| Total parameterize sites | 59 | ~62 | +3 |
| Total modules with parameters | 78 | ~80 | +2 |
| I/O effect-shell parameters | 0 | 5 | +5 (new pattern) |
| Anti-patterns (Medium) | 2 | 2 | 0 |
| Anti-patterns (Low) | 13 | 13 | 0 |

## 6. Recommended Future Actions

1. **Medium priority (unchanged):** Replace `current-loop-warning-count` and
   `current-rollback-action-log` with turn-local accumulators.

2. **Medium priority (new):** Extend I/O effect-shell pattern to `scripts/metrics.rkt`
   and `scripts/sync-version.rkt` — both have direct file I/O in their analysis paths.

3. **Low priority (unchanged):** Group verifier-core parameters into a config struct.

4. **Low priority (new):** Add contract-out to the I/O effect-shell parameters
   to catch type mismatches at the boundary.

5. **Low priority (new):** Consider `current-feature-flags` compound parameter
   that session-config populates atomically.

## 7. Conclusion

The parameter landscape remains healthy. The new I/O effect-shell pattern
(Pattern H) is the most testable parameter usage in the codebase.
The mutable-state anti-pattern (Pattern D) persists but is well-understood
and safe under the current single-threaded execution model.

Dynamic-context tests now validate the core parameterize contract that
all other patterns depend on.

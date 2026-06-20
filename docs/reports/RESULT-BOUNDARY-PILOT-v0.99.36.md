# Expected-Failure / Result-Boundary Pilot — v0.99.36 W5

**Date:** 2026-06-22
**Wave:** W5 (#8418)
**Risk Assessment:** GREEN (reward 12, risk 3)

## 1. Pilot Summary

This wave pilots the **result-type pattern** for error boundaries, replacing
ad-hoc `printf + exit 1` error communication with structured result types that
callers can dispatch on programmatically.

**Pilot target:** `scripts/sync-readme-status.rkt` — the `--check` operation.

## 2. Problem Statement

The `--check` operation in `sync-readme-status.rkt` had 5 distinct failure modes:

1. File not found
2. No `## Status` section in README
3. Status version mismatch (README says vX, q-version is vY)
4. Description mismatch (version matches but CHANGELOG description differs)
5. Everything matches (success)

All 5 outcomes were communicated via inline `printf` + `exit` calls inside a
deeply nested `cond` — 39 lines of branching logic embedded in the CLI `main`
function. This made the check logic:
- **Untestable** without subprocess execution
- **Unparseable** by programmatic callers (only stdout text available)
- **Tangled** with CLI formatting concerns

## 3. Result-Type Solution

### New module: `scripts/status-result.rkt`

Defines 5 `#:transparent` struct variants:

| Variant | Fields | Meaning |
|---------|--------|---------|
| `status-ok` | version, description | All checks passed |
| `status-version-mismatch` | found, expected, path | README version differs from q-version |
| `status-description-mismatch` | found, expected, version, path | Description differs from CHANGELOG |
| `status-missing-section` | path | No `## Status` section found |
| `status-file-not-found` | path | README file doesn't exist |

Plus utility functions:
- `status-check-result?` — union predicate for all variants
- `status-result-kind` — symbol discriminator (`'ok`, `'version-mismatch`, etc.)
- `status-result-exit-code` — 0 for ok, 1 for failures
- `format-status-result` — human-readable message for CLI output
- `check-readme-status` — pure function: takes lines + version info → returns result

### Refactored `--check` branch

**Before:** 39 lines of nested `cond` with inline `printf` + `exit`
**After:** 10 lines using `check-readme-status` + `format-status-result`:

```racket
(define result (check-readme-status lines version cl-version cl-summary path))
(displayln (format-status-result result))
(exit (status-result-exit-code result))
```

## 4. Benefits Demonstrated

1. **Testability:** The check logic is now a pure function testable without subprocess execution (17 new tests)
2. **Composability:** Callers can dispatch on result type programmatically
3. **Separation of concerns:** Check logic separated from CLI formatting
4. **Type safety:** Each failure mode is a distinct struct variant, not a magic string
5. **Backward compatibility:** CLI output and exit codes are identical to before

## 5. Pattern Applicability

This pattern is applicable to:
- Lint scripts with multiple distinct failure modes (`lint-version.rkt`, `lint-security.rkt`)
- Build verification steps with structured outcomes
- Any `printf + exit 1` error path that callers need to handle

**Future candidates (not in W5 scope):**
- `lint-version.rkt`: Currently returns `(list file line found expected)` — could use `version-check-result` types
- `lint-security.rkt`: Security scan results — could use `security-check-result` types

## 6. Verification

- `raco make main.rkt` — PASS
- `test-status-result.rkt` — 17 tests PASS
- `test-sync-readme-status.rkt` — 13 tests PASS (backward compatible)
- Smoke gate: 19/19 files, 286 tests PASS

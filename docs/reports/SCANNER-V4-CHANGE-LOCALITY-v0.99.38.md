# Abstraction Scanner v4 — Change-Locality Signals — v0.99.40 W8

**Date:** 2026-06-23
**Issue:** #8482
**Base:** `main@34c57b2d`

## Purpose

Extend the v0.99.37 abstraction fitness scanner with 4 new change-locality
signal categories that catch concrete failure classes identified in W0–W7.

## What Was Done

### New Scanner Signals

Extended `scripts/abstraction-audit.rkt` with 4 new pure counting functions:

| Signal | Regex | What it catches | Source |
|--------|-------|-----------------|--------|
| `count-cwd-sensitive-paths` | `dynamic-require\|"[^/]\|file->string\|"[^/]` | CWD-dependent file loading with relative-path string literals | W2 audit |
| `count-mutation-sites` | `call-with-output-file\|write-to-file\|display-to-file\|...` | File-writing mutation operations | W5 mutation boundary |
| `count-hash-ref-chains` | `hash-ref\s+\(hash-ref` | Nested hash-ref chains (API pain point P5) | W1 scorecard |
| `count-inline-config-paths` | `build-path\s+\(find-system-path\s+'home-dir` | Inline config dir instead of shared helper | W6 adapter audit |

All signals are **advisory only** — they appear in the report but do not
fail the build unless `--strict` mode adds thresholds for them (not done
in this wave).

### Scanner Architecture Changes

1. **4 new exported functions** added to `provide` block
2. **4 new regex patterns** (`cwd-sensitive-rx`, `mutation-site-rx`, `hash-ref-chain-rx`, `inline-config-path-rx`)
3. **`audit-content`** extended with 4 new hash keys per module finding
4. **`compute-summary`** extended with 4 new aggregate category lists
5. **`format-report`** extended with 4 new report sections

### New Tests

**22 new tests** in `tests/test-abstraction-audit.rkt`:

**`v4-change-locality-signal-tests` (16 unit tests):**
- CWD-sensitive paths: 5 tests (relative dynamic-require, relative file->string, absolute NOT flagged, build-path NOT flagged, clean text)
- Mutation sites: 4 tests (call-with-output-file, write-to-file, read-only returns 0, clean text)
- Hash-ref chains: 4 tests (single nested chain, multiple chains, single hash-ref returns 0, clean text)
- Inline config paths: 3 tests (build-path find-system-path, global-config-dir NOT flagged, clean text)

**`v4-audit-content-integration-tests` (6 integration tests):**
- All 4 new keys present in audit-content output
- CWD-sensitive detection via audit-content
- Mutation site detection via audit-content
- Hash-ref chain detection via audit-content
- Inline config path detection via audit-content
- Clean text has all 0 signals

**Total scanner tests: 61** (39 existing + 22 new)

## How to Interpret New Signals

### CWD-Sensitive Path Patterns

**What it means:** The module uses `dynamic-require` or `file->string` (or
similar) with a string literal that does NOT start with `/`. These patterns
break when the process working directory changes.

**Action:** Audit flagged modules against the W2 path-module-loading audit.
If safe (using `build-path`, `define-runtime-path`, or
`variable-reference->resolved-module-path`), no action needed. Otherwise,
migrate to source-relative resolution.

**Note:** This signal is conservative — it only catches string-literal
relative paths, not computed paths. Modules using `build-path` or variables
are NOT flagged (correctly).

### File Mutation Sites

**What it means:** The module contains file-writing operations
(`call-with-output-file`, `write-to-file`, `display-to-file`, etc.).

**Action:** Cross-reference with the W5 mutation boundary report. Check
whether the module follows the pure-helpers + thin-shell pattern
(compute function extracted, I/O centralized in a `write-or-check` wrapper).

### Hash-Ref Chains

**What it means:** The module contains `(hash-ref (hash-ref ...))` patterns
— nested hash lookups that are fragile to key changes and hard to test.

**Action:** Review against the W1 common-case API scorecard (pain point P5).
Consider extracting an accessor function or using a struct with named fields.

**Limitation:** The regex only catches `hash-ref` immediately nested inside
another `hash-ref` (non-overlapping matches). Triple nesting counts as 1,
not 2.

### Inline Config Paths

**What it means:** The module contains the pattern
`(build-path (find-system-path 'home-dir) ...)` instead of using the shared
`global-config-dir` helper from `util/config-paths.rkt`.

**Action:** Migrate to `global-config-dir` (or `project-config-dirs`) from
`util/config-paths.rkt`. The W6 adapter audit identified ~19 remaining sites.

## Risk Assessment

- **All signals are advisory** — no CI failure unless `--strict` mode is
  explicitly invoked with thresholds.
- **No existing behavior changed** — the 39 prior tests all pass unchanged.
- **Pure functions only** — all 4 new counters are pure regex-match operations.
- **No scanner framework complexity** — regex-based detection only, as per
  the scope controls ("Do not turn scanner into a parser framework").

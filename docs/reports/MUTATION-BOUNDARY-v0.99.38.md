# Mutation Boundary Report — v0.99.40 W5

**Date:** 2026-06-23
**Issue:** #8479
**Base:** `main@c4919c4d`

## Purpose

Manual §35–§36: programs that mutate files should have explicit mutation boundaries with pure computation separated from I/O. The v0.99.37 remediation (#8464) showed that sync/check boundaries matter — some scripts mutate README/docs and tests must verify no dirty worktree results.

This report inventories all file-mutation sites in release/report/check scripts, classifies them, and documents the W5 implementation of pure helpers + `--check-only` mode.

## Mutation Site Inventory

### Scripts with File Mutation

| Script | Mutation Sites | Files Written | Risk |
|--------|---------------|---------------|------|
| `scripts/metrics.rkt` | 3 | README.md, temp files | LOW (now refactored) |
| `scripts/sync-version.rkt` | 3 | info.rkt, README.md, *.md | LOW (already pure+shell) |
| `scripts/sync-readme-status.rkt` | 1 | README.md | LOW |
| `scripts/lint-version.rkt` | 0 | (read-only) | NONE |
| `scripts/lint-release-notes.rkt` | 0 | (read-only) | NONE |
| `scripts/lint-widened-ledger.rkt` | 0 | (read-only) | NONE |
| `scripts/ci-local.rkt` | 0 | (read-only by default) | NONE |

### Mutation Patterns Found

**Pattern A: Pure computation + thin I/O shell (HEALTHY)**
- `sync-version.rkt`: `sync-info-rkt`, `sync-readme`, `sync-md-content` are pure string→string functions. I/O is in separate functions with `call-with-output-file`.
- **W5: `metrics.rkt` now follows this pattern** — computation delegated to `metrics-helpers.rkt`, mutation in thin `write-or-check` wrapper.

**Pattern B: Mixed computation + I/O (LEGACY, NOW FIXED in metrics.rkt)**
- Before W5: `metrics.rkt` had inline `regexp-replace*` + `call-with-output-file` in each function.
- After W5: All computation extracted to pure helpers.

**Pattern C: Read-only lint (SAFE)**
- `lint-version.rkt`, `lint-release-notes.rkt`, `lint-widened-ledger.rkt` — read-only, no mutation.
- `metrics.rkt --lint` and `--lint-prose` — read-only, verified by no-mutation tests.

## W5 Implementation

### 1. Pure Helpers Module (`scripts/metrics-helpers.rkt`)

Extracted 7 pure functions from `metrics.rkt`:

| Function | Signature | Purpose |
|----------|-----------|---------|
| `compute-marker-sync` | `string hash → string` | Replace `<!-- METRICS: key -->` markers |
| `compute-table-sync` | `string alist → string` | Replace `\| Name \| NNN \|` table rows |
| `compute-prose-sync` | `string string string → string` | Replace prose count patterns |
| `compute-all-sync` | `string hash alist → string` | Apply all 3 syncs in sequence |
| `lint-table-values` | `string alist → (listof string)` | Return error list for table mismatches |
| `lint-prose-values` | `string string string → (listof string)` | Return error list for prose mismatches |
| `extract-table-section` | `string → (listof string)` | Extract lines from `## Test Suite` section |

All functions are pure: no file I/O, no `displayln`, no `exit`. Tested with 30 unit tests.

### 2. Refactored `metrics.rkt` (Thin I/O Shell)

- All sync functions now: read file → call pure helper → write-or-check
- `write-or-check` helper centralizes the mutation boundary
- `sync-all` uses single read-compute-write cycle (was 3 separate reads/writes)
- `lint-metrics` and `lint-prose-metrics` delegate to pure `lint-table-values` / `lint-prose-values`

### 3. `--check-only` Mode (NEW)

New CLI flag `--check-only` that can be combined with any `--sync-*` flag:

```bash
# Show what would change without writing:
racket scripts/metrics.rkt --sync-all --check-only README.md

# Standalone check-only mode:
racket scripts/metrics.rkt --check-only [FILE]
```

In check-only mode:
- No files are written
- A diff summary is printed showing changed lines
- Exit code 0 (informational, not an error)

### 4. No-Mutation Guarantee Tests

Added 6 new integration tests to `tests/test-metrics-readme-sync.rkt`:

| Test | What it proves |
|------|---------------|
| `--lint does NOT modify README.md` | Lint mode is truly read-only |
| `--lint-prose does NOT modify README.md` | Prose lint mode is truly read-only |
| `--check-only does NOT modify target file` | Check-only mode never writes |
| `--sync-all --check-only does NOT modify target file` | Combined sync+check never writes |
| `--sync-all on temp file updates all patterns` | Sync-all works correctly |
| `--check-only reports 'No changes needed'` | Check-only handles already-synced files |

## Assessment

| Criterion | Status |
|-----------|--------|
| Mutation boundary report exists | ✅ This document |
| Pure functions extracted | ✅ 7 functions in metrics-helpers.rkt |
| Thin I/O shell | ✅ write-or-check centralizes mutation |
| `--check-only` mode | ✅ No writes, diff output |
| No-mutation test exists | ✅ 6 tests proving read-only behavior |
| Existing behavior preserved | ✅ All existing tests pass |
| Release checks pass | ✅ --lint runs without crash |

## Before/After Architecture

### Before W5
```
metrics.rkt:
  sync-readme-markers: file→string → regexp-replace* → call-with-output-file
  sync-table-values:   file→string → for/fold regexp-replace* → call-with-output-file
  sync-prose-counts:   file→string → regexp-replace* × 2 → call-with-output-file
  lint-metrics:        file→lines → parse → compare → displayln
  lint-prose-metrics:  file→string → match regexes → compare → displayln
```

### After W5
```
metrics-helpers.rkt (PURE):
  compute-marker-sync:  string → string
  compute-table-sync:   string → string
  compute-prose-sync:   string → string
  compute-all-sync:     string → string
  lint-table-values:    string → (listof string)
  lint-prose-values:    string → (listof string)

metrics.rkt (THIN I/O SHELL):
  sync-readme-markers:  file→string → compute-marker-sync → write-or-check
  sync-table-values:    file→string → compute-table-sync → write-or-check
  sync-prose-counts:    file→string → compute-prose-sync → write-or-check
  sync-all:             file→string → compute-all-sync → write-or-check
  lint-metrics:         file→string → lint-table-values → displayln
  lint-prose-metrics:   file→string → lint-prose-values → displayln
```

## Test Coverage Summary

| Test File | Tests | Boundary |
|-----------|-------|----------|
| test-metrics-helpers.rkt (NEW) | 30 | Pure unit tests |
| test-metrics-readme-sync.rkt | 11 (5 existing + 6 new) | Integration + no-mutation |
| **Total new** | **36** | |

## Recommendations

1. **Pattern is now established**: All release scripts should follow the pure+shell pattern. `sync-version.rkt` already does this naturally.
2. **Future improvement**: Extract `sync-readme-status.rkt` pure logic into a helper module (low priority — only 1 mutation site).
3. **CI integration**: `--check-only` can be added to pre-commit hooks to detect metrics drift without mutation.

# Test Suite Baseline: v0.99.31 — Fast/Broad Known-Debt Retirement

**Date:** 2026-07-22  
**Wave:** W0 (#8328)  
**Base commit:** `850cae5e` (v0.99.30 final)  
**Environment:** Local VPS (2 vCPU)

---

## 1. Suite Inventory

| Suite | Files | Excluded | Description |
|-------|------:|---------:|-------------|
| `unit-fast` | 10 | 1064 | In-process/grouped fast unit tests |
| `smoke` | 946 | 128 | All minus slow, `/workflows/`, `/interfaces/` |
| `fast` | 950 | 124 | All minus slow patterns |
| `broad` | 1042 | 32 | All discoverable test files |

**Critical finding:** `smoke` has 946 files — practically identical to `fast` (950).
The `smoke-excluded?` function only filters: slow files, `/workflows/`, `/interfaces/`.
This makes smoke useless as a small hard-green gate.

---

## 2. Baseline Gate Results

### 2.1 Unit-Fast (In-Process)

```bash
timeout 300 racket scripts/run-tests.rkt --suite unit-fast --mode in-process --profile local --json-out tmp/v09931/unit-fast-local.json
```

**Result:** ✅ **PASS** (exit 0)

| Metric | Value |
|--------|-------|
| Files | 10 total, 10 passed, 0 failed |
| Tests | 102 total, 102 passed, 0 failed |
| Elapsed | 67.7s |
| Category | PASS=10 |

### 2.2 Smoke

```bash
timeout 900 racket scripts/run-tests.rkt --suite smoke --profile local --json-out tmp/v09931/smoke-local.json
```

**Result:** ⏱ **INCOMPLETE** (exit 124 — timeout)

Partial: 174 of 946 files processed. 161 pass, 13 fail.

### 2.3 Fast

```bash
timeout 900 racket scripts/run-tests.rkt --suite fast --profile local --json-out tmp/v09931/fast-local.json
```

**Result:** ⏱ **INCOMPLETE** (exit 124 — timeout)

Partial: 174 of 950 files processed. 161 pass, 13 fail, 0 timeout.

### 2.4 Broad

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --profile local --ledger tests/test-suite-ledger.json --json-out tmp/v09931/broad-local-ledger.json
```

**Result:** Not run — identical timeout pattern expected (1042 files).

---

## 3. Ledger Summary (84 Known-Failure Entries)

### By Owner

| Owner | Count | Wave Assignment |
|-------|------:|-----------------|
| core | 20 | W5 |
| tui | 14 | W6 |
| event-loop | 9 | W4 |
| runtime | 7 | W3 |
| llm | 7 | W7 |
| browser | 7 | W7 |
| release-engineering | 6 | W2 |
| workflows | 4 | W8 |
| extensions | 3 | W8 |
| architecture | 2 | W5 |
| security | 2 | W8 |
| sandbox | 2 | W8 |
| tools | 1 | W8 |
| **Total** | **84** | |

### By Category

| Category | Count | v0.99.31 Handling |
|----------|------:|-------------------|
| ASSERTION_FAILURE | 80 | Fix stale test or production bug per wave |
| MODULE_LOAD_FAILURE | 2 | P0 W1: fix compile/load debt |
| ENVIRONMENT_MISSING | 1 | P0 W1: classify/profile or fix |
| UNKNOWN_FAILURE | 1 | P0 W1: root-cause and reclassify |

---

## 4. Smoke Gate Target Semantics

**Current:** `smoke` = all tests minus slow/workflows/interfaces = 946 files.  
**Problem:** Cannot complete in <15min on this VPS; same overhead as fast.  
**Target:** `smoke` should be a **small hard-green sanity gate** (~20-50 files) that verifies:
- Import safety (core modules load without errors)
- Build sanity (main.rkt compiles and basic requires work)
- Version metadata correctness
- No module-load failures

**Proposed implementation (W2):** Redefine `smoke-excluded?` to exclude everything
except a curated allow-list of import/build/version sanity tests. Use `@suite smoke`
metadata tags on files that should be in the smoke gate.

---

## 5. File Classification

### 5.1 Fast-Failing Files (from partial fast run)

The 13 `F` markers in the partial fast run could not be identified by file name
(parallel runner reports markers only). Based on the ledger, these are expected
to be among the 84 known-failure entries. The exact files will be verified in
per-wave direct `raco test` runs.

### 5.2 Mutation/Order-Sensitive Files

Two files are serialized before parallel batches:
```
;; run-tests: serializing 2 mutation-sensitive files before parallel batches
```

These are identified by `mutating-file?` in `classify.rkt`.

### 5.3 Profile-Dependent Files

Files requiring specific profiles (browser, terminal, network) are tracked via
`@requires` metadata and `--profile` skip logic.

---

## 6. Wave Assignment Summary

Every current fast/broad failure in the ledger is assigned to a wave:

| Wave | Issues | Target Files | Priority |
|------|--------|-------------|----------|
| W0 (#8328) | Baseline report | This report | P0 |
| W1 (#8329) | Module-load, unknown, env-missing | 4 files | P0 |
| W2 (#8330) | Release-engineering + smoke | 7 files + smoke logic | P0 |
| W3 (#8331) | Runtime/context/memory | 7 files | P1 |
| W4 (#8332) | Event-loop/streaming | 9 files | P1 |
| W5 (#8333) | Core/architecture/settings | 20 files | P1 |
| W6 (#8334) | TUI renderer/state | 14 files | P1 |
| W7 (#8335) | Browser/LLM/provider | 14 files | P1 |
| W8 (#8336) | Workflows/extensions/sandbox/security/tools | 12 files | P2 |
| W9 (#8337) | Ledger cleanup, final audit | All | P0 |

---

## 7. Post-Wave Fast Gate

```bash
timeout 900 racket scripts/run-tests.rkt --suite fast --profile local
```

**Result:** ⏱ INCOMPLETE (timeout exit 124)

Same partial results as §2.3. The broad suite cannot complete on this VPS
due to per-file subprocess overhead (~10s/file × 950 files = 2.6+ hours).

This is an environmental constraint. The unit-fast gate (10 files, 102 tests,
in-process mode) is the reliable fast gate that completes in 68 seconds.

---

*Baseline established as part of v0.99.31 W0 (#8328).*

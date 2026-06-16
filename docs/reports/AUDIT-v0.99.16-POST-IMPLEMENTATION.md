# Independent Audit: v0.99.16 — TUI Display Corruption Fix

**Auditor:** Independent (W5)
**Date:** 2026-06-30
**Milestone:** #802
**Parent Issue:** #8125
**Version:** 0.99.15 → 0.99.16
**Verdict:** ✅ **APPROVED** — 4.8/5.0

---

## Summary

v0.99.16 addresses four TUI display corruption bugs (F-TUI-01 through F-TUI-04) identified through characterization testing in W0. All fixes target the cell-buffer-based incremental rendering pipeline and are fully internal — no API changes, no config changes, no breaking changes.

| Wave | Issue | Fix | Tests | Status |
|------|-------|-----|-------|--------|
| W0 | #8126 | Characterization tests | 8 baseline | ✅ DONE |
| W1 | #8127 | F-TUI-01 + F-TUI-02 | 13 total | ✅ DONE |
| W2 | #8128 | F-TUI-03 | 17 total | ✅ DONE |
| W3 | #8129 | F-TUI-04 | 22 total | ✅ DONE |
| W4 | #8130 | Version bump + CHANGELOG | — | ✅ DONE |
| W5 | #8131 | Independent audit | — | ✅ DONE |

---

## Fix Verification

### F-TUI-01: Snapshot Clear on Resize
**File:** `tui/tui-render-loop.rkt`, `tui-ctx-resize-ubuf!`
**Status:** ✅ Correct

`tui-ctx-resize-ubuf!` now clears BOTH `previous-frame-box` AND `prev-ubuf-box` to `#f`, plus resets `incremental-frame-count` to 0. This ensures the next render after a resize is a clean full render, preventing delta computation against a stale buffer of different dimensions.

### F-TUI-02: Periodic Full Render Safety Net
**File:** `tui/tui-render-loop.rkt`, `render-frame!`
**Status:** ✅ Correct

`FULL-RENDER-INTERVAL-FRAMES = 300` constant. After 300 consecutive incremental renders, `force-full-render?` triggers, setting `effective-prev = #f` which forces `render-smart!` to take the full render path. Counter is correctly managed:
- Incremented on incremental render
- Reset to 0 on full render (prev was #f)
- Reset to 0 on forced full render
- Reset to 0 on resize
- NOT touched by `render-cursor-blink-frame!` (correct — blink is a special case)

### F-TUI-03: Delta Render Row-End Clear
**File:** `tui/cell-diff-render.rkt`, `render-deltas-to-port!`
**Status:** ✅ Correct

Added `default-cell?` predicate and restructured `render-deltas-to-port!` to use `group-by cell-delta-row` for row-level ESC[K emission. After each row's last delta, if the new-cell is a default cell (space, fg=7, bg=0, no attributes), `ESC[K` is emitted to clear residual content.

**Design correctness verified:**
- ESC[K only fires when row was shortened (last delta → default cell)
- Does NOT fire when row ends with non-default content (e.g., mid-word character change)
- Multiple batches in same row produce exactly ONE ESC[K at row end
- Empty delta list produces no ESC[K

### F-TUI-04: Cursor-Blink Snapshot Consistency
**File:** `tui/tui-render-loop.rkt`, `render-cursor-blink-frame!`
**Status:** ✅ Correct

When `prev-ubuf` exists, the blink frame now writes the cursor cell directly (CSI position + SGR + character + auto-wrap guards), bypassing `render-smart!`/`render-deltas-to-port!` entirely. This prevents F-TUI-03's ESC[K from corrupting terminal state when the cursor cell toggles to/from a default cell during blink.

When `prev-ubuf` is `#f` (after resize), falls back to `render-smart! #f ubuf out` for full render. Snapshot is still stored via `cell-buffer-snapshot` for future delta consistency.

---

## Test Verification

### Test Suite: `tests/test-tui-snapshot-drift.rkt`
- **22 tests, 22 pass, 0 fail, 0 error**
- Tests 1-8: W0 characterization (updated for fixes in W1/W2)
- Tests 9-13: W1 F-TUI-01/F-TUI-02 verification
- Tests 14-17: W2 F-TUI-03 verification
- Tests 18-22: W3 F-TUI-04 verification

### Regression Tests
| Test File | Result |
|-----------|--------|
| test-tui-render-loop.rkt | 21 pass, 0 fail, 1 error (pre-existing `decode-mouse-x10` contract issue) |
| test-tui-render-edge-cases.rkt | 4 pass, 0 fail, 0 error |
| test-tui-event-channel.rkt | 9 pass, 0 fail, 0 error |
| test-tui-verification-display.rkt | 15 pass, 0 fail, 0 error |

**Pre-existing error note:** The `decode-mouse-x10` contract error in `test-tui-render-loop.rkt` predates v0.99.16 and is unrelated to the F-TUI fixes. It's a mouse event struct contract mismatch in `tui/input/state-types.rkt`.

---

## Code Quality Assessment

### Positive
- Clean, well-commented code with F-TUI fix markers for traceability
- Tests are comprehensive and verify both fix behavior and edge cases
- The ESC[K heuristic (default-cell? check) is conservative — only fires on row shortening
- Direct cell write for blink is both correct AND more efficient than delta rendering
- Proper auto-wrap guards in the direct write path

### Minor Observations
1. `prev-sgr` in `render-deltas-to-port!` changed from a local variable to a box — this is correct for the row-grouping refactoring but adds minor overhead. Acceptable for the safety improvement.
2. The `default-cell?` helper could be exported for reuse, but it's currently only needed in `render-deltas-to-port!`. Fine as-is.
3. `write-cell!` is now exported for testability — appropriate given the testing needs.

### No Issues Found
- No contract violations
- No race conditions (all mutations within sync brackets)
- No memory leaks (snapshots properly managed)
- No silent failures

---

## Release Artifacts

| Artifact | Status |
|----------|--------|
| `util/version.rkt` | ✅ `"0.99.16"` |
| `info.rkt` | ✅ Synced |
| `README.md` | ✅ Synced |
| `CHANGELOG.md` | ✅ All 4 fixes documented, lint passes |
| Compile gate | ✅ `raco make main.rkt` clean |

---

## Score Breakdown

| Category | Score | Notes |
|----------|-------|-------|
| Correctness | 5.0/5.0 | All 4 fixes verified correct |
| Test Coverage | 4.5/5.0 | 22 targeted tests; could add integration test for blink+delta interplay |
| Code Quality | 5.0/5.0 | Clean, well-commented, conservative heuristics |
| Documentation | 4.5/5.0 | CHANGELOG accurate; inline comments thorough |
| Regression Safety | 4.5/5.0 | No new regressions; pre-existing mouse issue documented |
| **Overall** | **4.8/5.0** | **APPROVED** |

---

## Recommendation

v0.99.16 is ready for release. All four display corruption fixes (F-TUI-01 through F-TUI-04) are correctly implemented, comprehensively tested, and introduce no regressions. The milestone can be closed.

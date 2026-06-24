# Audit: TUI Subsystem — v0.99.45 W8

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w8-tui.rkt`
**Tests:** 101 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `tui/state-types.rkt` — UI state structs (ui-state, streaming-state, selection-state, cache-state, overlay-state, branch-info, tree-browser-state)
  - `tui/char-width.rkt` — Unicode-aware terminal column width
  - `tui/input/state-types.rkt` — Input state, undo/redo, kill-ring, mouse events, IME cursor markers
  - `tui/command-parse.rkt` — Slash command parsing and validation
  - `tui/palette.rkt` — Command registry, palette overlay rendering
  - `tui/scrollback.rkt` — Transcript serialization and JSONL persistence
  - `tui/clipboard.rkt` — Multi-backend clipboard (OSC 52 + system tools)
  - `tui/approval-channel.rkt` — HITL approval sync (async-channel based)
  - `tui/cell-buffer.rkt` — Native 2D cell buffer
  - `tui/frame-diff.rkt` — Frame diff computation for minimal redraw
  - `tui/state-events/registry.rkt` — Event reducer registry

## Test Matrix

| Area | Tests | Status |
|------|-------|--------|
| State Types (ui-state defaults, keywords, streaming, selection, overlay, cache, branch) | 7 | PASS |
| Transcript Entries (make, system, error, ID assignment, monotonic) | 5 | PASS |
| Render Cache (set/ref, invalidate, clear, width, prune at max) | 5 | PASS |
| Streaming State (busy, clear, status, tool, text, phase, delta, update fn) | 8 | PASS |
| Char Width (ASCII, control, CJK, combining, zero-width, emoji, string width, truncate, display-col, grapheme) | 11 | PASS |
| Input State (initial, construction, constants, undo, strip, kill, cursor markers) | 7 | PASS |
| Mouse Events (X10 click, scroll up/down, invalid, decode release, scroll, normalize) | 7 | PASS |
| Command Parsing (tokenize contract bug, lookup, validate none/required/optional, full parse, resolve) | 8 | PASS |
| Command Palette (registry, all, by-category, filter, resolve, complete, from-hashes, merge, render) | 9 | PASS |
| Scrollback (to-jsexpr, from-jsexpr, round-trip, save/load, nonexistent) | 6 | PASS |
| Clipboard (backend available, OSC 52 encoding, mode off, mode osc52, detect tool) | 5 | PASS |
| Approval Channel (make, custom timeout, default permissive, set/clear, put+await, timeout constant) | 6 | PASS |
| Cell Buffer (create, default cell, set, clear) | 4 | PASS |
| Frame Diff (full redraw, empty prev, same-length changed, same content, grow, shrink, multi-change, string-lines) | 8 | PASS |
| Event Registry (test isolation, no handler, write-once, apply with handler) | 4 | PASS |
| **Total** | **101** | **ALL PASS** |

## Findings

### FINDING-001 (low): tokenize contract bug — returns 2 values, contract promises 1

**Severity:** Low
**Category:** Contract Bug
**File:** `tui/command-parse.rkt`
**Description:** The `tokenize` function returns `(values cmd-string args-list)` or `#f`, but its contract is `(-> string? any/c)` which promises exactly 1 return value. When called from outside the module boundary, Racket's contract system raises a "broke its own contract" error because 2 values were produced but only 1 was promised.

Internally, `parse-command-name` works correctly because it uses `call-with-values` to capture both return values, and contracts are only checked at module boundaries.

```racket
(define (tokenize text)
  (define trimmed (string-trim text))
  (cond
    [(string=? trimmed "") #f]
    [(not (char=? (string-ref trimmed 0) #\/)) #f]
    [else
     (define parts (string-split trimmed))
     (values (car parts) (cdr parts))]))  ;; ← returns 2 values!
```

**Impact:** Low — the function works correctly within the module. External callers cannot call `tokenize` directly but can use `parse-command-name` instead. This is a latent contract bug that would surface if any future code calls `tokenize` from outside the module.

**Recommendation:** Change the contract to `(-> string? (values (or/c string? #f) (listof string?)))` or wrap the return value in a single-value struct/pair.

### FINDING-002 (info): ui-state is a comprehensive 21-field struct with well-documented field groups

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The `ui-state` struct has 21 fields organized into logical groups:
- **Transcript**: transcript, scroll-offset, next-entry-id
- **Streaming**: streaming (streaming-state sub-struct with 8 fields)
- **Selection**: selection (selection-state)
- **Branch**: current-branch, visible-branches
- **Cache**: cache (cache-state with entries hash + width)
- **Overlay**: active-overlay
- **Extension**: extension-widgets, custom-header, custom-footer
- **Input**: focused-component, editor-component
- **Session**: session-id, model-name, mode, mock-provider?
- **Queue**: queue-counts
- **Budget**: context-tokens, cost-tracker, context-pressure-level, context-pressure-percent
- **Goal**: active-goal

The streaming-state sub-struct provides backward-compatible accessors that delegate to the streaming field. Render cache has automatic pruning (max 100 entries, oldest-first eviction). The design is clean and well-organized.

**Impact:** Positive — the state model is comprehensive and maintainable.

### FINDING-003 (info): Char-width implementation follows Unicode UAX #11 East Asian Width

**Severity:** Info (positive finding)
**Category:** Correctness
**Description:** The `char-width` function correctly implements Unicode East Asian Width for:
- ASCII/Latin characters: width 1
- Control characters (C0/C1): width 0
- Combining marks (U+0300–U+036F, extended ranges): width 0
- Zero-width characters (ZWJ, ZWNJ, ZWS): width 0
- CJK characters (Hiragana, Katakana, CJK Unified): width 2
- Emoji-like symbols: width 2

Grapheme cluster utilities follow UAX #29 with fallback for Racket < 8.12. The implementation handles regional indicator pairs (flag emoji), ZWJ sequences, and variation selectors.

**Impact:** Positive — correct terminal rendering for international text and emoji.

### FINDING-004 (info): Approval channel uses thread-safe module-level box (not parameter)

**Severity:** Info (positive finding)
**Category:** Thread Safety
**Description:** The approval channel correctly uses a module-level box instead of a Racket parameter. This is because parameters are NOT inherited by child threads in Racket — the session thread spawned by `submit-handler.rkt` wouldn't see a parameterized value. Boxes ARE shared across threads (heap-allocated mutable cells).

The channel uses `async-channel` with non-blocking put and blocking get with timeout (default 120s). When no channel is set (non-interactive mode), `approval-await-result` returns `#t` (permissive), allowing tools to proceed without user interaction in CLI/JSON/RPC modes.

**Impact:** Positive — correct thread synchronization pattern for HITL approval.

### FINDING-005 (info): Clipboard supports multiple backends with intelligent detection

**Severity:** Info (positive finding)
**Category:** Feature
**Description:** The clipboard module supports 4 modes:
- `'auto` — system tool first, OSC 52 fallback (also emits OSC 52 after system tool for SSH)
- `'osc52` — OSC 52 escape sequence only (works over SSH)
- `'system` — system clipboard tools only
- `'off` — disabled

Backend detection checks environment variables (WAYLAND_DISPLAY, DISPLAY) and tries appropriate tools (wl-copy, xclip, xsel, pbcopy). Detection is memoized for session lifetime. The paste function also detects and uses appropriate paste tools (wl-paste, xclip -o, xsel, pbpaste).

**Impact:** Positive — robust clipboard support across platforms and SSH sessions.

### FINDING-006 (info): Frame diff algorithm handles variable-length frames correctly

**Severity:** Info (positive finding)
**Category:** Performance
**Description:** The `diff-frames` function computes minimal update commands between two frame states:
- **Same length**: row-by-row diff, only changed rows produce 'write commands
- **Grow**: diff common prefix + append new lines as 'write commands
- **Shrink**: diff common prefix + 'clear-from command for surplus rows
- **First render**: 'full redraw command

This minimizes terminal output by only sending changed lines, improving render performance for the TUI.

**Impact:** Positive — efficient incremental rendering.

### FINDING-007 (info): Event reducer registry provides test isolation and thread safety

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The event reducer registry uses:
- **Thread safety**: `call-with-semaphore` guards all hash operations
- **Test isolation**: `call-with-test-registry` parameterizes a fresh hash for tests
- **Write-once registration**: Re-registering an existing event type is a no-op (prevents accidental handler replacement)
- **Graceful degradation**: Unknown event types return state unchanged

The registry-based design replaces a monolithic case dispatch, making it extensible for new event types.

**Impact:** Positive — extensible, thread-safe, and testable event handling.

### FINDING-008 (info): Scrollback uses atomic writes with max entry limit

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The scrollback module:
- Writes atomically (temp file + rename) to prevent corruption
- Limits to 500 entries (newest kept) to prevent unbounded file growth
- Assigns unique IDs to deserialized entries for render cache tracking
- Uses JSONL format (one JSON object per line) for append-friendly storage

**Impact:** Positive — robust transcript persistence.

## Architecture Summary

The TUI subsystem is organized in layers:

1. **Terminal abstraction** (`terminal.rkt`, `terminal-native.rkt`, `terminal-input.rkt`):
   - Raw terminal I/O (ANSI escape sequences, stty)
   - Screen size queries with caching
   - Key reading and escape-sequence parsing
   - Mouse tracking, bracketed paste, sync mode

2. **State management** (`state-types.rkt`, `state-events.rkt`, `state-ui.rkt`):
   - `ui-state` struct (21 fields, grouped by domain)
   - `streaming-state` sub-struct (8 fields for turn activity)
   - Render cache with automatic pruning (max 100)
   - Event reducer registry (thread-safe, write-once, test-isolated)

3. **Rendering** (`render.rkt`, `render/*`, `cell-buffer.rkt`, `frame-diff.rkt`):
   - Cell buffer (2D array with fg/bg/attributes per cell)
   - Frame diff computation (minimal update commands)
   - Message layout, status line, diff rendering
   - Markdown rendering

4. **Input** (`input.rkt`, `input/*`, `char-width.rkt`):
   - Input state (buffer, cursor, undo/redo, kill-ring)
   - Editing operations, history, completion
   - Mouse event parsing (X10 protocol)
   - IME cursor markers
   - Unicode-aware char width (UAX #11)

5. **Commands** (`command-parse.rkt`, `palette.rkt`, `commands/*`):
   - Slash command parsing with aliases
   - Command palette registry and filtering
   - Extension command integration
   - Command categories (general, session, model)

6. **Interaction** (`selection.rkt`, `clipboard.rkt`, `approval-channel.rkt`, `scrollback.rkt`):
   - Text selection with mouse
   - Multi-backend clipboard (OSC 52 + system tools)
   - HITL approval channel (async-channel based, thread-safe)
   - Scrollback serialization and JSONL persistence

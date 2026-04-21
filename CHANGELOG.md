# Changelog

## v0.14.1 — 2026-04-20

### Exploration & Generation Robustness

- **Wave 0**: Increased HTTP timeout defaults (300→600s request, 30→60s stream) to prevent premature timeouts during long generation
- **Wave 1**: Soft/hard iteration limits — `max-iterations` becomes a soft warning, `max-iterations-hard` (default = soft) is the hard stop. TUI shows `[exploring... iteration N, M remaining before hard stop]`
- **Wave 2**: Context-aware retry messages — auto-retry events now include classified error type (timeout, rate-limit, context-overflow, provider-error) for type-aware TUI display
- **Wave 3**: Exploration progress hints — after 4+ consecutive tool-only turns, shows `[exploring... N tool calls: read, bash, ...]`
- **Wave 4**: Adaptive stream timeout — doubles per-chunk SSE timeout after `stream-secs` total streaming time for long generation pauses
- **Wave 5**: Mid-turn token budget check — emits `context.mid-turn-over-budget` event when context exceeds 90% of `max-context-tokens` during tool execution

### Architecture Boundary Fixes

- **Wave 6**: Lifted TUI mock-provider detection to `provider-factory.rkt` — eliminated `tui→llm` layer violation
- **Wave 7**: Moved `runtime/resource-loader.rkt` → `extensions/resource-discovery.rkt` — eliminated `runtime→extensions` boundary violation
- **Wave 8**: Session-switch dependency injection — replaced direct `extensions/` imports with DI via `dynamic-require` + keyword args
- **Wave 9**: Removed `tui-init.rkt` from arch-boundaries test exceptions — TUI layer now has zero boundary violations

### Metrics
- 314 test files, 5365 tests, 0 failures
- Remaining runtime exceptions: `iteration.rkt` (documented ARCH-01), `package.rkt` (manifest audit)

## v0.14.0 — 2026-04-20

### Context Manager Architecture

Replaces mechanical context truncation with a strategy-driven context assembly engine. The session log is now immutable — the context manager decides what goes into the LLM context window using pluggable strategies:

1. **Pin**: System prompt + first user message (always present)
2. **Summary**: LLM-generated or concatenation summary of excluded entries
3. **Recent**: Last N tokens kept verbatim
4. **Catalog**: One-line-per-entry summary of excluded entries
5. **Budget enforcement**: Total context ≤ token budget

#### New Modules
- `runtime/context-manager.rkt` — Strategy-driven context assembly with configurable budgets, summary generation, catalog creation, and consecutive tool result collapsing
- `tools/builtins/session-recall.rkt` — `session_recall` tool: lets the agent retrieve excluded session entries by ID or range

#### Removed Modules
- `runtime/context-reducer.rkt` — Old pair-aware mechanical trimming, fully replaced by context-manager

#### Features
- **Session Recall Tool** (#1391): Agent can now retrieve excluded context entries via `session_recall(id="...")` or `session_recall(range="from..to")`. Returns formatted message details.
- **LLM Summary Generation** (#1395): Structured summary template with caching. Falls back to concatenation when no LLM provider is available.
- **Summary Integration** (#1396): Excluded entries auto-summarized and injected as `compaction-summary` messages between pinned and recent context.
- **Catalog Token Caps** (#1394): Catalog entries capped at 40 entries / 2K tokens. Consecutive tool results collapsed to single entry.
- **Budget Enforcement**: Catalog dropped first, then summary truncated, recent window last. Pinned items never dropped.

#### Bug Fixes
- **Flaky CI tests fixed**: `test-bump-version.rkt` and `test-ci-local.rkt` now dynamically read current version instead of hardcoding. (#1393)
- **Metrics lint drift**: Resolved stale prose counts causing intermittent CI failures.

### Metrics
- 312 test files, 5,358+ tests passing, 0 failures
- 224+ source modules

---

## v0.13.2 — 2026-04-20

### Bug Fixes
- **Removed context reduction from retry path**: `#:context-reducer` parameter removed from `with-auto-retry`. Retries now always use the same context — no trimming, no reduction. Eliminates P0 class of 400 errors from malformed reduced context after retry trimming. (#1388, PR #1389)

---

## v0.13.1 — 2026-04-20

### Performance
- **TUI transcript O(n²) → O(1)**: Transcript append now uses `cons` instead of `append`, eliminating quadratic slowdown on long sessions. Added `transcript-entries` accessor that reverses on read for backward-compatible oldest-first ordering. (#1386, PR #1387)

### Bug Fixes (from v0.13.0)
- **Settings contract**: Fixed `make-settings` field contracts that rejected valid values. (#1376)
- **Context reducer pair-awareness**: Context reduction now correctly handles paired tool-start/tool-end entries. (#1377)
- **`/retry` + iteration limit**: `/retry` command now correctly updates `last-prompt-box`. Max iterations raised from 10 to 20. (#1378, PR #1383)
- **Newline bleed**: Fixed trailing newline bleed in assistant message rendering. (#1379)
- **Prompt pinning**: First user message now survives context truncation via `pin-first-user` helper. (#1380, PR #1384)
- **Progressive timeout**: Fixed timeout recovery for long-running tool calls. (#1381)
- **O(n²) audit**: Documented all O(n²) hotspots in TUI codebase for future remediation. (#1382, PR #1385)

### Metrics
- 325 test files, 68,903 test lines, 10,786 assertions
- 224 source modules, 41,753 source lines
- 5,330+ tests passing (full suite)

---

## v0.12.0 — 2026-04-19

### Features
- Extension power user API
- Session tree navigation
- SDK foundations

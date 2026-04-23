# Changelog

## v0.18.4 — 2026-04-23

### Sandbox & Safety

**Wave 1 — Dogfooding infrastructure** (PR #1658)
- New `scripts/run-dogfood-session.rkt`: task loader, validator, mock runner
- New `scripts/capture-regression.rkt`: baseline capture and regression comparison
- Dogfood task format spec (JSON) with setup/teardown
- 3 sample tasks: basic-file-ops, planning-workflow, skill-routing
- 11 tests

**Wave 2 — Dogfood execution + analysis** (PR #1659)
- Mock execution trace validation
- Baseline capture and comparison workflow tests
- Regression detection (pass/fail cases)
- Multi-task comparison
- 6 tests

**Wave 3 — Version bump 0.18.0 → 0.18.4** (PR #1660)
- Version bump and CHANGELOG update

## v0.18.0 — 2026-04-23

### Context-Aware Exploration Steering

**Wave 1 — Same-file dedup + raised thresholds** (PR #1611)
- Raise exploration steering thresholds: gentle 5→8, strong 7→12, hard cap 12→20
- Add same-file dedup: reading the same file multiple times counts as 1 exploration step
- New `extract-tool-target-path` and `update-seen-paths` helpers exported for testing
- 18 comprehensive steering tests in `tests/test-steering.rkt`

**Wave 2 — Configurable thresholds** (PR #1612)
- Steering thresholds now configurable via `config.json` under `steering` key
  - `steering.gentle_threshold` (default: 8)
  - `steering.strong_threshold` (default: 12)
  - `steering.hard_cap` (default: 20)
  - `steering.same_file_dedup` (default: true)
- New accessors in `runtime/settings.rkt`: `steering-gentle-threshold`, `steering-strong-threshold`, `steering-hard-cap`, `steering-same-file-dedup?`
- 10 config tests added to `tests/test-steering.rkt`

## v0.17.9 — 2026-04-23

### GSD Planning Workflow + Review Cleanup

Milestone #92 — Post-v0.17.8 review follow-ups and planning prompt augmentation.

**Wave 1 — Review cleanup + test coverage (#1596)**
- Removed all `/tmp/q-cmd-dispatch.log` diagnostic tracing from 4 files.
- Replaced with `log-debug` where useful, removed entirely where not.
- Fixed stale `-> void?` contract comment in `loader.rkt` (now returns `boolean?`).
- Restricted `/plan <text>` submit to `/plan` and `/p` only —
  `/state` and `/handoff` always display artifact regardless of trailing text.
- Added 5 tests for execute-command with/without args.

**Wave 2 — Planning prompt augmentation (#1599)**
- Defined `planning-system-prompt` constant with GSD planning instructions.
- When `/plan <text>` submits, agent prompt is augmented with planning preamble
  instructing it to write a structured plan to `.planning/PLAN.md`.
- Display text shows "Planning: <original>" without full preamble.
- Added test verifying augmented submit text contains `[gsd-planning]` preamble.

**Wave 4 — Fix pre-existing test failures (#1605)**
- Synced all version surfaces: info.rkt, README.md, docs/*.md, wiki-src/ to 0.17.9.
- Added `.planning/` and `.pi/` to `lint-version.rkt` skip list (historical version refs).
- Synced README metrics (source line counts).
- Fixed `test-tui-enter.rkt`: updated expected command return format
  from `(command quit)` to `(command quit "/quit")`.
- All 3 previously-failing tests now pass: 348/348 files, 5629/5629 tests.

## v0.17.8 — 2026-04-23

### Extension Commands & Activation Fix

Milestone #91 — Fix extension activation path, command dispatch plumbing,
and gsd-planning execute-command handler for end-to-end `/activate` → command workflow.

**Wave 1 — Fix project-dir in /activate and /deactivate (#1588)**
- `handle-activate-command`: Changed project-dir source from `(path-only session-dir)`
  to `(current-directory)` — q always starts with cwd set to project root.
- `handle-deactivate-command`: Same fix.
- Removed dead `session-dir` bindings and `not project-dir` branches.
- Tests updated with `(parameterize ([current-directory tmp-dir]) ...)`."

**Wave 2 — Extension command dispatch plumbing (#1589)**
- Added `extension-registry-box` field to `cmd-ctx` (10th) and `tui-ctx` (14th).
- Wired extension-registry from `run-modes.rkt` through `tui-init.rkt` to `tui-ctx`.
- In `process-slash-command`, unknown commands dispatch via `'execute-command`
  extension hook before falling back to "Unknown command" error.
- Updated all test `cmd-ctx` constructors for new field arity.

**Wave 3 — gsd-planning execute-command handler + hot-load (#1590)**
- Added `'execute-command` hook point to `hook-action-schemas` in `util/hook-types.rkt`.
- `gsd-planning.rkt`: Added `handle-execute-command` handler for `/plan`, `/state`,
  `/handoff` — reads artifact content and returns via `hook-amend`.
- `commands.rkt`: Added `try-hot-load-extension` — after `/activate`, loads the
  newly activated extension into the running session registry.
- 4 new tests for execute-command handler.

## v0.17.7 — 2026-04-23

### Review Remediation

Milestone #90 — Security hardening, extension system integrity, and broken
registration fixes from v0.17.6 review.

**Wave 1 — Fix broken extension registrations (#1573)**
- `remote-collab/remote-collab.rkt`: Fixed `ext-register-tool!` from 2-arg
  `(ctx (make-tool ...))` to 5-arg `(ctx name desc schema handler)` form
- `session-export.rkt`: Same arity fix — unwrapped `make-tool` into direct args
- Added tool-registration tests for both extensions

**Wave 2 — Security hardening (#1576)**
- **Shell injection eliminated** in `github-integration.rkt`: Replaced `/bin/sh -c`
  with arg-list `subprocess` pattern (same as `racket-tooling.rkt`). All command
  construction now passes args directly — no shell interpolation.
- **Input validation consistency**: `valid-number?` applied to all issue/PR/milestone
  number params. `valid-state?`/`valid-method?` applied to all state/method branches.
- **Rsync args fixed** in `q-sync.rkt`: `sync-pi-config` and `sync-scripts` now
  concatenate trailing `/` into the path string instead of passing `/` as a separate
  rsync argument (which rsync interprets as "sync root directory").
- **Backup safety** added to `q-sync.rkt`: `--backup --backup-dir=.rsync-backup`
  on all rsync calls.
- **Path traversal fix** in `extension-catalog.rkt`: Added `valid-extension-name?`
  whitelist (`[a-zA-Z0-9_-]+`) before all `build-path` calls in `activate-extension!`
  and `deactivate-extension!`. Same validation wired into TUI `/activate` handler.
- Regression tests for `../../foo`, `foo/bar`, `..`, `.hidden` traversal attempts.

**Wave 3 — Extension system integrity (#1580)**
- **EXTENSIONS_INVENTORY.md**: Rewritten to list all 8 extensions (was 4).
  Corrected tool counts (16), command counts (6), versions, and API versions.
  Removed loadable extensions from "supporting infrastructure" table.
- **Tier validation wired**: `extension-tier-valid?` called during `load-extension!`
  in `loader.rkt`. Default tier is `hooks`; violations are logged as warnings.
- **Catalog type fix**: `list-active-extensions` now uses `#f` for `source-path`
  instead of symbol `'active` (violates struct contract).
- **`/activate --global` UX**: Returns usage error when no name provided after `--global`.
- **`/deactivate` TUI command**: New slash command for symmetry with `/activate`.
  Supports `/deactivate <name>` (project-local) and `/deactivate --global <name>`.

**Tests**: +22 new tests across 4 test files. 347 files, 5631 tests, 0 failures.

---

## v0.17.6 — 2026-04-23

### Extension Discovery & Activation

- **Removed `.pi/extensions/` loading**: TypeScript pi extensions are incompatible
  with q's Racket `dynamic-require` loader. The loader no longer scans
  `<project>/.pi/extensions/`.
- **Added `~/.q/extensions/` global directory**: Extensions placed in
  `~/.q/extensions/` are loaded for all projects. Global extensions load
  first; project-local `<project>/.q/extensions/` overrides global.
- **Extension catalog module** (`runtime/extension-catalog.rkt`): New module
  providing `known-extensions-dir`, `list-known-extensions`,
  `list-active-extensions`, `activate-extension!`, `deactivate-extension!`.
  Supports flat files and subdirectory extensions (e.g., `remote-collab`).
- **`/activate` TUI command**: New slash command for in-session extension
  management:
  - `/activate` — show active + available extensions
  - `/activate <name>` — activate extension in project-local dir
  - `/activate --global <name>` — activate in `~/.q/extensions/`
  - `/activate --available` — list all known extensions from source tree

**Tests**: +18 new tests (4 run-modes, 13 extension-catalog, 5 activate-command,
3 cmd-ctx arity fixes). 347 files, 5609 tests, 0 failures.

---

## v0.17.5 — 2026-04-23

### Audit Remediation

Security and robustness fixes from comprehensive audit of v0.17.0–v0.17.4
(remote pi implementation). 25 findings addressed: 5 CRITICAL, 7 MAJOR, 13 MINOR.

**CRITICAL fixes:**
- **C1**: Fixed `ext-register-tool!` arity — all 7 extensions now use 5-positional-arg
  form instead of wrapping `make-tool` inside a 2-arg call
- **C2**: Replaced `system/exit-code` with `subprocess` in `racket-tooling.rkt`,
  eliminating shell injection via filenames with metacharacters
- **C3–C5**: Added input validation to `github-integration.rkt` — whitelist `state`,
  `method` params; validate `issue_number`, `pr_number`; fixed regex from `#rx` to
  `#px` for `(?:...)` non-capturing groups

**MAJOR fixes:**
- **M2**: `gsd-planning.rkt` rejects path traversal in artifact names (`/`, `..`, null bytes)
- **M3**: `session-export.rkt` HTML-escapes all user content (`&`, `<`, `>`, `"`)
- **M4**: `gh-wave-finish` checks exit codes at every step (PR merge, checkout, pull, close)
- **M5**: Removed hardcoded default host from `q-sync.rkt` — `remote_host` now required
- **M6**: Replaced rsync `--delete` with `--backup --backup-dir=.rsync-backup`
- **M7**: `remote-collab.rkt` validates session names (alphanumeric + hyphens only)

**MINOR fixes:**
- **m1**: Rewrote `find-form-end` with proper depth tracking, string escape handling,
  semicolon comment skipping, and square bracket support

**Regression tests:** 30+ new tests across 6 test files. 168 tests pass.

---

## v0.17.4 — 2026-04-23

### Phase E: Polish

- **ext-package-manager**: Wraps `runtime/package.rkt` as a tool. Actions: list,
  install, remove, info for extension packages.
- **image-input**: Multi-modal image support. Base64 encodes images (PNG, JPEG,
  GIF, WebP) and constructs multi-modal messages for vision-capable LLMs.
- **session-export**: Export session JSONL logs to HTML, JSON, or Markdown.
  Supports output to file or inline text result.

### Testing

- 16 tests across 3 Phase E extensions
- All existing tests continue to pass

---

## v0.17.3 — 2026-04-23

### Phase D: GSD Skills

All 15 GSD skills verified present in `.pi/skills/`:

- **Core skills**: q-gsd-orchestrator, q-gsd-milestone-builder, q-gsd-reviewer,
  q-gsd-project-reviewer, q-gsd-bug-orchestrator, q-gsd-bugfix-builder
- **Builder skills**: q-gsd-foundation-builder, q-gsd-core-builder, q-gsd-tools-builder,
  q-gsd-runtime-builder, q-gsd-interfaces-builder, q-gsd-hardening-builder
- **Support skills**: q-gsd-docs-orchestrator, q-gsd-skillsmith, q-gsd-testing-improver
- **GitHub skills**: q-gsd-github-init, q-gsd-github-issues, q-gsd-github-branches, q-gsd-github-projects

No new Racket code — skills are pure markdown discovered by existing skill system.

---

## v0.17.2 — 2026-04-23

### Phase C: Remote Collaboration Extension

- **C1**: `remote-collab` extension — multi-file extension using subdirectory
  support (B3). `remote-q` tool controls remote q instances via SSH + tmux.
  Actions: status, start, send, capture, wait, interrupt, stop.
  `ssh-helpers.rkt` and `tmux-helpers.rkt` as separate modules.
- **C2**: `q-sync` extension — multi-domain sync tool. Directions: push, pull,
  status, handoff. Domains: planning, pi-config, scripts, git, all.
  Uses `rsync` for file sync, `git` for version control.

### Testing

- 11 tests for remote-collab extension
- 8 tests for q-sync extension
- All existing tests continue to pass

---

## v0.17.1 — 2026-04-23

### Phase B: Self-Editing & Extension Infrastructure

- **B1**: `racket-tooling` extension — 3 tools for structural Racket editing:
  `racket-check` (format, syntax, test, expand, all), `racket-edit` (9 modes:
  replace, form, skeleton, struct-add-field, provide-append, cond-insert-clause,
  match-insert-clause, rewrite-form, constructor-add-arg), `racket-codemod`
  (pattern/template with @@PLACEHOLDER matching). All modes validate with
  `raco fmt` + `raco make`, revert on failure.
- **B2**: `compact-context` extension — agent-invocable context compaction tool
  that reads `.planning/` state and injects it into the compaction context.
- **B3**: Extension subdirectory support — `discover-extensions` now scans
  `extensions/<name>/<name>.rkt` and `extensions/<name>/main.rkt` for
  multi-file extensions, alongside existing flat `.rkt` files.

### Testing

- 16 tests for racket-tooling extension
- 10 tests for compact-context extension
- 3 new tests for extension subdirectory discovery
- All existing tests continue to pass

---

## v0.17.0 — 2026-04-23

### Phase A: Foundation Extensions

- **A1**: `spawn-subagents` tool — parallel batch execution with output aggregation
  (up to 3 concurrent subagent processes, structured results, partial failure handling)
- **A2**: `gsd-planning` extension — registers `planning-read` and `planning-write` tools
  plus `/plan`, `/state`, `/handoff` slash commands
- **A3**: `github-integration` extension — registers 6 GitHub tools:
  `gh-issue`, `gh-pr`, `gh-milestone`, `gh-board`, `gh-wave-start`, `gh-wave-finish`
  Uses `gh` CLI via subprocess with `gh-binary-path` parameter for test injection
- **A4**: `skill-route` tool — skill discovery by description match, full content loading
  Actions: `list`, `match`, `load`. Registered in `registry-defaults.rkt`

### Testing

- 60 tests for github-integration extension (mock `gh` CLI)
- 11 tests for skill-route tool
- All existing tests continue to pass

---

## v0.16.1 — 2026-04-22

### Critical Fixes (from PROJECT_REVIEW_v0.16.0)

- **SEC-07**: Fix `subprocess-result` arity bug — error handler had `#f` nested inside
  `inexact->exact` call instead of being the 6th field (`truncated?`). Any subprocess
  execution failure (command not found, permission denied) would crash with arity error.
  1-line fix: move `#f` to correct position.

### Documentation Fixes

- **D1-D3**: Rewrite wiki Architecture Overview — stale metrics (124→228 modules, 140→349
  test files), removed reference to deleted `runtime/resource-loader.rkt`, corrected provider
  API description from `complete`/`stream-complete` to `make-provider` dispatch protocol.
- **D4**: Add missing ADR index entries for 0008 (safe-mode enforcement),
  0009 (credential redaction), 0010 (streaming port lifecycle).

### Housekeeping

- Bump version references across all docs to 0.16.1

## v0.16.0 — 2026-04-22

### Architecture Hardening & Documentation Refresh

Milestone #81 — 17 issues, 11 PRs merged. Full review findings in
`.planning/REVIEW-v0.15.2.md` (73 findings: 6 CRITICAL, 23 MAJOR, 32 MINOR, 12 NIT).

#### Wave 0 — Housekeeping (#1475, #1477, #1488)
- Version drift sync, STATE.md + SUMMARY.md reconciliation

#### Wave 1 — Shell Injection Fix (#1474)
- FFI `getpid` via isolated submodule (avoids `ffi/unsafe` → `racket/contract` conflict)
- `/proc/<pid>` filesystem check for `pid-alive?` (container-safe)
- `truncated?` field on subprocess results

#### Wave 2 — Azure Hardening (#1479)
- `dynamic-wind` port cleanup for streaming generators
- Shared response parser extracted from Azure-specific code
- Configurable request timeout

#### Wave 3 — Safe-Mode Enforcement (#1482)
- Symlink resolution in path validation
- One-shot lock via box parameter
- `dangerous?` field on tool descriptors

#### Wave 4 — Error Consolidation (#1478, #1489)
- Eliminated duplicate `provider-error` struct definitions
- Unified in `llm/provider-errors.rkt`

#### Wave 5 — Sandbox, Credentials & OAuth (#1485, #1483, #1486)
- SHA-256 HMAC for credential verification
- Opaque credential structs with `gen:equal+hash` + `gen:custom-write`
- OAuth scope separator: `+` → `%20` (spec-compliant)
- OAuth stubs now raise errors instead of silently returning `#f`

#### Wave 6 — CI Pipeline (#1480)
- `scripts/ci-local.rkt`: 10 automated checks (format, compile, imports, security lint)
- `scripts/lint-security.rkt`: hardcoded secret scanner with exemption patterns

#### Wave 7 — Documentation Refresh (#1476, #1484)
- 7 source files updated, 3 ADRs added (0008–0010)
- CHANGELOG backfilled from git history

#### Wave 8a — Iteration.rkt set! Fix (#1481)
- Replaced 3x `set!` with `let-over-cond` binding in exploration escalation

#### Wave 9 — Test Infrastructure (#1487)
- Event bus concurrency test: mutex-protected counter via `call-with-semaphore`
- Subprocess `truncated?` field tests (overflow vs. fits budget)

#### Wave 10 — Port Cleanup & Quality (#1490)
- `dynamic-wind` port cleanup in `anthropic.rkt` and `gemini.rkt` streaming
- Fixed double warning in `load-session-log`
- O(n²) → O(n) `jsonl-read-all-valid-with-count` via `cons`/`reverse`
- Idempotent `cancel-token!` guard

### Metrics
- 332 test files, 5307 tests passing
- 10/10 CI local checks
- 3 new ADRs (0008-safe-mode, 0009-credential-redaction, 0010-streaming-port-lifecycle)

## v0.15.2 — 2026-04-21

### Bug Fixes
- **P1**: Detect silent stream EOF — emit synthetic `model.stream.completed` with
  `finish_reason: "eof"` when API closes without finish chunk (BUG-SILENT-STREAM-EOF)
- **P2**: Strengthen Level 1 exploration steering — "Consider" → "You MUST now"
  (BUG-STEERING-LEVEL1-WEAK)
- **P2**: Detect intent-without-action pattern — if model says "I'll rewrite" but
  no tool call follows, inject steering nudge capped at 1 retry (BUG-INTENT-WITHOUT-ACTION)
- **P0**: Steering messages use `'user` role instead of `'system` (fixes 400 errors)
- **P0**: Context builder preserves tool_call/tool_result pairing during truncation
- **P0**: Tiered context builder preserves system-instruction and first user message
- **P1**: Index rebuild infers missing parentIds from log order (fixes context amnesia)

## v0.15.1 — 2026-04-21

### Trace Logger Hardening
- **[P0]** Fix malformed JSONL: added `sanitize-for-json` to recursively convert
  non-jsexpr values (event structs, procedures) to safe string representations
  before `write-json`, preventing partial writes that corrupt the trace file
- **[P0]** Wrap `write-json` in error handler to skip non-serializable events
  gracefully instead of crashing
- **[P0]** Fix `model` field in `model.request.started` event: use string instead
  of `(object-name provider)` symbol for safe JSON serialization
- Raise circuit breaker threshold from 5 to 100 (sanitization now prevents the
  most common failure mode; high threshold is safety net only)
- 4 new tests: struct sanitization, 100 rapid events, procedure values,
  circuit breaker resilience

### max_tokens Event Timing Fix
- **[P1]** Resolve max-tokens from multiple config paths: top-level,
  `providers.<name>.max-tokens`, `models.default.max-tokens` — not just the
  flat runtime config hash which never contains it
- Import `setting-ref*` for nested config path resolution in iteration.rkt

### Stream Timeout Tuning
- **[P1]** SSE stream timeout formula: `max(120, timeout/4)` → `max(180, timeout/2)`
- For glm-5.1 (request=900s): 225s → 450s, preventing premature SSE timeouts
  during slow model generation

### Exploration Steering Escalation
- **[P2]** 3-level escalation: gentle nudge at 5, strong at 7, hard cap at 12
  consecutive read-only tool calls
- Tool-type-aware counting: counter resets when file writes detected
  (write/edit/replace/create tools)
- Hard cap emits `exploration.hard-cap` event for observability

## v0.15.0 — 2026-04-21

### Request-Cycle Trace Logger Module

Structured diagnostic trace of every LLM request cycle for post-mortem debugging.
Disabled by default — zero overhead when off. Enable via `logging.trace.enabled` in
config.json.

**Core**: New `runtime/trace-logger.rkt` subscribes to event bus, writes `trace.jsonl`
per session with sequence numbers, ISO 8601 timestamps, and full event data.
Flush-on-write for crash safety.

**Enriched events**:
- `model.request.started` now includes `model`, `max_tokens`, `settings`
- `model.stream.completed` now includes `finish_reason` (stop/length/tool_calls)
- `stream-chunk` struct now has 6th field `finish-reason` for actual API value
- New `iteration.decision` event at each loop iteration with termination, consecutive_tools

**Config**: `logging.trace.enabled` (boolean), `logging.trace.max-files` (int, default 10)

**CLI**: `q sessions trace <id>` — formatted, `--json` raw, `--summary` counts

- `runtime/trace-logger.rkt`: New trace logger module (#1452)
- `llm/model.rkt`: Added `finish-reason` field to `stream-chunk` (#1453)
- `llm/stream.rkt`: Pass finish_reason through normalizers (#1453)
- `agent/loop.rkt`: Enriched request/completed events (#1453)
- `runtime/iteration.rkt`: New `iteration.decision` event (#1453)
- `runtime/settings.rkt`: `trace-enabled?`, `trace-max-files` (#1454)
- `wiring/run-modes.rkt`: Wire trace logger into startup (#1454)
- `interfaces/sessions.rkt`: `q sessions trace` command (#1455)

## v0.14.4 — 2026-04-21

### Config Validation + Iteration Budget + Provider Settings Wiring

**P1**: Invalid `config.json` silently fell back to mock provider with only a
WARNING. Now `config-parse-error` in `settings.rkt` detects broken JSON and
`provider-factory.rkt` prints a clear ERROR with file path and fix instructions
before falling back to mock.

**P1**: Slow models (glm-5.1) hit the default `max-iterations=20` from
exploration overhead. Default soft limit raised 20→50. Hard limit now
calculated as `max(soft*1.6, 80)` instead of matching soft limit. After 8+
consecutive tool calls without file writes, a steering message is injected:
"Focus on producing the actual output using the write or edit tool now."

**P2**: Provider settings (e.g. `max-tokens`) from `config.json` never reached
the API request body. Settings are now threaded through `run-provider-turn` →
`run-agent-turn` → `make-model-request` → `openai-build-request-body`.

- `settings.rkt`: `config-parse-error` function for JSON validation (#1444)
- `provider-factory.rkt`: Clear error messages on broken config (#1444)
- `agent-session.rkt`: Default `max-iterations` 20→50 (#1445)
- `iteration.rkt`: Hard limit formula + exploration steering hint (#1445)
- `loop.rkt`: `#:provider-settings` param in `run-agent-turn` (#1446)
- `iteration.rkt`: Config threaded to `run-provider-turn` (#1446)

## v0.14.3 — 2026-04-21

### Second-Prompt Crash + SSE Timeout + Streaming Text Fix

**P0**: Fixed `hash-set` contract violation that crashed any second prompt. Production
runtime uses mutable config hash (`make-hash`), but `hash-set` requires immutable.
Now detects hash mutability and uses `hash-set!` or `hash-set` accordingly.

**P1**: SSE stream timeout now scales with per-model request timeout. Previously
hardcoded at 60s between chunks, causing timeouts on slow models (e.g. glm-5.1
with `request: 900`). Stream timeout = `max(120, request/4)` seconds.

**P2**: Partial streaming text preserved on error. When SSE timeout or other error
fires during model streaming, accumulated grey text is now committed to the
transcript as a partial assistant entry before clearing.

- `agent-session.rkt`: Mutable/immutable config hash detection (#1438)
- `openai-compatible.rkt`: Scaled SSE stream timeouts (#1439)
- `tui/state.rkt`: Partial streaming text preservation (#1440)
- 10 new/updated tests across 3 test files

## v0.14.2 — 2026-04-20

### Retry Robustness & TUI Crash Fix

**Wave 0 (P0 TUI crash)**: Wrapped both TUI runner thread call sites with
`with-handlers exn:fail?` that emit `runtime.error` + `turn.completed` events.
Defense-in-depth `turn.completed` in `agent-session.rkt`. New test file:
`test-tui-error-recovery.rkt` (6 tests).

**Wave 1 (P1 retry budgets)**: Per-type retry budgets (`timeout=2`,
`rate-limit=4`, `provider-error=2`) via `#:per-type-budgets` keyword argument.
`retry-exhausted` struct gains `error-history` field tracking all error types
across retries. Agent session includes `errorHistory` in `runtime.error` payload.
State module renders recovery hints from full error history (mixed-type detection).
4 new tests in `test-auto-retry.rkt`.

**Wave 2 (P2 work preservation)**: `/retry` command enriched with previous
attempt's tool summary. New `get-last-turn-tool-summary` in `tui/state.rkt`.
Modified `/retry` handler to include `[Context from previous attempt: ...]`.
8 new tests in `test-retry-enrichment.rkt`.

**Wave 3 (P2 model timeouts)**: Per-model timeout profiles via
`current-model-timeouts` parameter and `effective-request-timeout-for`.
OpenAI-compatible provider extracts model name from request body and applies
per-model timeout overrides. Config schema:
`{ timeouts: { models: { "glm-5.1": { "request": 900 } } } }`.
10 new tests in `test-model-timeouts.rkt`.

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

### Post-Review Fixes (Waves 10–13)

- **Wave 10**: Removed dead code in `classify-error` (R1) — no-op `when` block. Fixed `rate-limit-error?` pattern — replaced `"too many"` with `"too many requests"` to prevent context-overflow misclassification (R2)
- **Wave 11**: Fixed README v0.14.1 status block — replaced v0.13.x description with accurate v0.14.1 features (D1). Synced metrics (D2)
- **Wave 12**: Added 12 TUI event handler tests covering `iteration.soft-warning`, `exploration.progress`, `context.mid-turn-over-budget`, and `auto-retry.start` with `errorType` (TC1)
- **Wave 13**: Added `session-rebind` to `hook-action-schemas` (H1). Wrapped `dynamic-require` with descriptive error messages in `session-switch.rkt` (SW1). Added argument validation in `resource-discovery.rkt` (RD1)

### Metrics
- 315 test files, 5365 tests, 0 failures
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

## v0.13.0 — 2026-04-20

### Features
- **Session tree navigation**: Navigate between parent/child sessions
- **Context reducer**: Progressive context trimming for long conversations
- **Extension power-user API**: Advanced extension hooks for tool/loop interception
- **Provider improvements**: Better error handling, retry logic, timeout tuning

### Bug Fixes
- Settings contract fixes for `make-settings` field validation
- Context reducer pair-awareness for tool-start/tool-end entries
- `/retry` + iteration limit interaction
- Newline bleed in assistant message rendering
- First user message prompt pinning

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

## v0.11.x — 2026-04-19

### Features
- **Tool scheduler**: Priority-based tool execution with concurrency limits
- **Extension hooks v2**: Lifecycle hooks for tool dispatch, loop iteration, and session events
- **Credential store**: Centralized API key management with environment variable support
- **OAuth framework**: OAuth 2.0 authorization flow (stubs for token exchange/refresh)
- **Safe-mode guard**: One-shot safe-mode lock for restricting dangerous operations

### Infrastructure
- CI local lint suite (8 checks)
- Security lint for hardcoded secrets
- Compatibility matrix documentation

---

## v0.12.0 — 2026-04-19

### Features
- Extension power user API
- Session tree navigation
- SDK foundations

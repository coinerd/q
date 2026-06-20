## 0.99.35

Released: 2026-06-21

### Overview
Racket Abstraction Manual Roadmap. This release applies disciplined
abstraction patterns across 7 production modules, extracting pure
helper functions into dedicated modules to improve testability,
reduce cognitive load, and clarify module boundaries. All extractions
are backward-compatible — public API surfaces are unchanged.

### New Modules

- **W1: `scripts/abstraction-audit.rkt` (392 lines).** Advisory
  scanner that collects abstraction fitness signals (line count,
  export count, parameter/macro usage, I/O mixing, serialization
  hotspots, exception density, dependency fan-out). Supports
  `--strict` mode for CI integration.

- **W2: `tools/builtins/spawn-subagent-helpers.rkt` (139 lines).**
  5 pure functions extracted from spawn-subagent.rkt.

- **W3: `sandbox/subprocess-helpers.rkt` (152 lines).** Subprocess
  result struct, constructors, and classifiers plus secret detection.

- **W4: `agent/event-json-helpers.rkt` (303 lines).** 16 pure
  serializer and deserializer functions for event JSON round-tripping.

- **W5: `extensions/gsd/transition-logic.rkt` (209 lines).** Pure
  state machine logic: valid-transition?, valid-targets,
  find-transition-path, compute-next-gsm-state, check-state-invariants.
  Bugfix in find-transition-path path generation.

- **W6: `runtime/context-assembly/state-aware-helpers.rkt` (182 lines).**
  6 pure functions for state-aware context assembly.

- **W7: `tui/state-events/handler-helpers.rkt` (78 lines).** 5 pure
  functions including deduplicated retry-error-type-label.

- **W8: `llm/anthropic-helpers.rkt` (64 lines).** Narrow extraction
  of 3 pure functions from the HIGH RISK Anthropic provider.

### Documentation

- **W0: Abstraction inventory.** Scored 12 modules across reward/risk
  axes. Reports in `docs/reports/`.

### Bug Fixes

- W5: Fixed `find-transition-path` path generation bug.
- W1: Fixed `#rx` regex bugs (`\\|` literal pipe → `#px` alternation).

### User-Visible Changes

- None. This is an internal refactoring release. All public APIs unchanged.

### Breaking / Behavior Changes

- None. All extractions preserve the full public API surface.

### Migration Notes

- No migration required. All existing imports continue to work unchanged.

### Testing

- 204 new tests across 8 new test files.
- All existing tests pass — zero regressions.

### Operational / Release

- Version bumped to 0.99.35 in util/version.rkt, info.rkt, README.md.
- Metrics synced.

---

## 0.99.29

Released: 2026-07-22

### Overview
Gate Truth Hotfix for v0.99.28. This release closes all six findings
(B-1 through B-6) from the v0.99.28 in-depth audit before the M5/v1.0
milestone. The hotfix focuses on gate truthfulness: ensuring that test
results, audit narratives, and release-gate verdicts are accurate and
verifiable.

### Bug Fixes

- **W0: Skill workflow E2E test hardening (B-1, BLOCKER)**.
  `test-skill-workflow-e2e.rkt` now uses idempotent file writes
  (`#:exists 'truncate/replace`) to prevent failures under `raco test`
  lifecycle re-execution. Added `describe-state` diagnostic helper for
  debugging skill discovery failures with contextual state information.

### Documentation

- **W1: Metrics sync and version bump (B-2, BLOCKER)**.
  README metrics table synced with `--sync-all`. Version bumped to
  0.99.29 in `util/version.rkt`, `info.rkt`, and README badge.

- **W2: Truthful broad-suite triage (B-3, B-6)**.
  `docs/reports/AUDIT-v0.99.29-BROAD-GATE-TRIAGE.md` reports actual
  broad-suite results with honest verdict classification.

- **W3: Corrected v0.99.28 narratives (B-4, B-5)**.
  Errata added to v0.99.28 post-implementation audit, broad-gate triage,
  and MAS completeness audit. Stale head commit references corrected.
  False gate claims removed.

### Breaking / Behavior Changes

- None. All changes are test hardening, documentation corrections, and
  version/metrics metadata.

### Migration Notes

- No API changes.

### Testing

- Skill workflow E2E: 5/5 PASS (verified over 10 consecutive `raco test` runs).
- Full focused matrix (17 files): ALL PASS under `raco test`.
- Zero regressions from v0.99.28.

### Operational / Release

- Broad-gate triage: `docs/reports/AUDIT-v0.99.29-BROAD-GATE-TRIAGE.md`.
- Post-implementation audit: `docs/reports/AUDIT-v0.99.29-POST-IMPLEMENTATION.md`.

### Stats
- 1 hardened test file
- 1 metrics sync
- 4 audit/triage reports created or corrected

## 0.99.28

Released: 2026-07-21

### Overview
M4.5 Audit Remediation + MAS Technical Debt. This release closes remaining
audit blockers found in the v0.99.27 in-depth audit: workflow failure results
discarding partial outputs, YAML frontmatter polluting skill descriptions,
MAS test debt (stale registry defaults, CLI error classification), and
test-runner reporting truthfulness (silent PASS on timeouts/zero-test files).
Also corrects the MAS completeness audit's inaccurate "261 tests" claim.

### Bug Fixes

- **W0: Preserve partial workflow failure results (V27-B1, BLOCKER)**.
  `make-workflow-error-result` now uses `make-tool-result` directly to
  include completed and failed step results in structured details.
  Previously, `make-error-result` discarded all partial outputs.

- **W1: Frontmatter-aware skill descriptions (V27-B2, BLOCKER)**.
  `strip-leading-frontmatter-lines` in `resource-loader.rkt` removes
  leading YAML frontmatter before extracting title/description/content.
  `skill-route list` no longer shows raw `type: mas-workflow` YAML.

- **W2: MAS technical debt — registry defaults, CLI verbose, tool taxonomy**.
  - `test-registry-defaults.rkt`: Updated hardcoded tool count from 17 to 35
    with full browser/memory/spawn tool coverage.
  - `classify-error` in `util/error/error-classify.rkt`: Reordered
    `hash-ref:` session pattern before `contract` pattern. Issues #149, #166
    now classify correctly as `'session` with user-friendly message.
  - `tools/permission-gate.rkt`: Added 8 memory tools to permission sets.
    Read/safe operations auto-approved; destructive (delete/clear) require
    approval.

- **W3: Test-runner reporting truthfulness (V27-B3, MAS-TD-1)**.
  `compute-verdict` in `scripts/run-tests/reporting.rkt` returns
  `'pass`/`'fail`/`'incomplete`/`'inconclusive`. `print-summary` now
  prints an explicit VERDICT line and warns about zero-test files.
  Previously, timeout-heavy runs could appear to pass at a glance.

### Breaking / Behavior Changes

- `classify-error` pattern ordering changed: `hash-ref:` errors that
  were previously classified as `'contract` are now correctly classified
  as `'session`. User-facing error messages for missing-key scenarios
  are now friendlier.

### Migration Notes

- No API changes. All fixes are internal behavior improvements.
- If your code depends on `classify-error` returning `'contract` for
  `hash-ref:` errors, update to expect `'session`.

### Testing

- 19 new tests in `test-run-tests-reporting-truthfulness.rkt` (W3).
- 11 new tests in `test-skill-resource-loader-frontmatter.rkt` (W1).
- `test-registry-defaults.rkt`: 7/7 PASS (was 1/7 FAIL).
- `test-cli.rkt`: 69/69 PASS (was 3/69 FAIL).
- 251 focused tests verified passing across 33 individually-run files.
- Zero direct v0.99.28 regressions.

### Operational / Release

- Broad-gate triage: `docs/reports/AUDIT-v0.99.28-BROAD-GATE-TRIAGE.md`.
- MAS completeness audit corrected: "261 tests" → actual 251.
- Board hygiene report: `docs/reports/AUDIT-v0.99.28-BOARD-HYGIENE.md`.
- v0.99.27 broad-gate triage errata added.

### Stats
- 3 modified production files (workflow-executor, resource-loader,
  error-classify, permission-gate, run-tests reporting)
- 30 new tests across 2 new test files
- 3 new audit/hygiene reports

## 0.99.27

Released: 2026-07-20

### Overview
M4.5 Skill Workflows Remediation. This release fixes production-blocking
issues found in the v0.99.26 in-depth audit: a broken `skill-route workflow`
execution path, a capability taxonomy regression, non-boolean `parallel:`
parsing, partial workflow results dropped on failure, and HITL approval gates
for dangerous workflow steps. Also adds parallel workflow step execution and
truthful broad-gate triage reporting.

### W0: Fix `skill-route workflow` Execution (B-1, BLOCKER)
- **Root cause**: `dynamic-require` used runtime-relative string paths that
  resolved to wrong directories when executed from a non-repo working directory.
- **Fix**: Replaced with `define-runtime-module-path-index` for compile-time-
  resolved module paths that are stable relative to the source module.
- **Test**: Added happy-path E2E test in `tests/test-skill-workflow-e2e.rkt`
  that verifies workflow execution works from a temp directory.

### W1: Restore `skill-route` Read-only Capability Contract (B-2, HIGH)
- **Fix**: Reverted `skill-route` capability from `'subagent` back to
  `'read-only` in `tools/registry-table/skill-tools.rkt`.
- Read-only subagents can discover and load skills again.
- Workflow action remains safe: sub-spawns are individually mediated by
  `run-subagent-with-config` / HITL / capability filtering.

### W2: Parallel Workflow Step Execution (M-1)
- **Feature**: Added parallel workflow step groups via `parallel: true` in
  frontmatter. Steps with `parallel: true` in the same group execute
  concurrently via `spawn-subagents`.
- **Fix**: Added `parse-boolean` coercion in `skills/mas-workflow.rkt` to
  normalize YAML `parallel: true` (string) to Racket `#t`.
- Sequential and parallel steps can be mixed in a single workflow.
- **7 new tests** in `tests/test-workflow-executor.rkt` for parallel execution.

### W3: HITL Approval Gate for Dangerous Workflow Steps
- **Feature**: `execute-step-group` now checks `requires-hitl-approval?` on
  the union of all step capabilities in a group before execution.
- Steps with `shell-exec` or `git-write` capabilities require interactive
  approval. On denial, all steps in the group are marked failed and the
  pipeline stops.
- `step-group-capabilities` aggregates capabilities; `step-group-task-preview`
  builds a short preview for the approval prompt.
- **4 new tests** for approved/denied sequential and parallel dangerous steps.

### W4: Broad-Gate Triage + v0.99.26 Audit Errata (B-3, HIGH)
- Clean-bytecode `raco make main.rkt`: PASS.
- 261 focused tests across 22 files: ALL PASS, zero v0.99.27 regressions.
- Pre-existing failures classified: `test-registry-defaults.rkt` (stale
  hardcoded tool count from v0.86.2), `test-cli.rkt` (Issues #149, #166).
- Hanging tests classified as environment-dependent (TUI/provider/subprocess).
- Added errata to v0.99.26 audit report correcting fast-suite timeout claim.
- New report: `docs/reports/AUDIT-v0.99.27-BROAD-GATE-TRIAGE.md`.

### Stats
- 4 modified production files (skill-router, skill-tools, mas-workflow,
  workflow-executor)
- 11 new tests across 2 test files (workflow-executor, skill-workflow-e2e)
- 2 new audit reports (broad-gate triage, post-implementation)

## 0.99.26

Released: 2026-07-20

### Overview
M4 Skill Workflows (§5.2 Skill-based MAS). This release delivers declarative
multi-agent pipelines packaged as skills. A SKILL.md with `type: mas-workflow`
frontmatter defines an ordered list of agent steps. The skill-router detects
this type and can execute the pipeline, chaining subagent spawns with result
forwarding via `{{result}}` template variables.

Also includes subagent reliability fixes (F-1a/F-1b/F-2) from the v0.99.25
audit: auto-retry for rate-limited subagents, increased default max-turns,
and improved error labeling for bash failures.

### W0: Audit Remediation + Subagent Reliability Fixes
- **E-2**: Exception-safe approval channel cleanup via `dynamic-wind` in
  `tui/tui-init.rkt` — `clear-approval-channel!` now runs on both normal
  exit AND exception paths.
- **E-3/E-4**: CHANGELOG count corrections (44 TUI smoke tests, 8 production
  files).
- **F-1a (CRITICAL)**: Wrapped `provider-send` in `with-auto-retry` inside
  `run-subagent-loop`. Subagents now survive rate-limited and transient
  provider errors instead of dying immediately.
- **F-1b**: Default `max-turns` raised from 5 to 10. Complex analysis tasks
  were exhausting 5 turns on tool calls before producing summaries.
- **F-2**: `ipc-response->tool-result` now handles `status='error` separately,
  showing `"command failed (exit N): <stderr>"` instead of generic
  `"execution plane error"`.
- **9 new tests**: test-approval-channel-teardown.rkt (4), test-subagent-retry.rkt
  (4), test-execution-plane-error-label.rkt (5).

### W1: YAML-Subset Frontmatter Parser
- **`parse-skill-frontmatter-extended`** in `skills/frontmatter.rkt`: Parses
  YAML-subset frontmatter supporting lists (`- item`), nested maps within list
  items, inline arrays (`[a, b, c]`), and quoted values.
- Recursive descent parser with indentation tracking. Backward compatible —
  existing `parse-skill-frontmatter` unchanged.
- **13 new tests** in `tests/test-frontmatter-extended.rkt`.

### W2: MAS Workflow Types + Parser
- **`workflow-step` struct** (role, task, capabilities, parallel?) — a single
  step in a multi-agent pipeline.
- **`mas-workflow` struct** (name, description, steps, variables) — a complete
  parsed workflow.
- **`parse-mas-workflow`**: Frontmatter hash → mas-workflow with validation.
- **`extract-template-variables`**: Finds `{{var}}` patterns in step tasks.
- **`parse-capabilities`**: String list → symbol list.
- **19 new tests** in `tests/test-mas-workflow.rkt`.

### W3: Workflow Executor
- **`execute-workflow`** in `skills/workflow-executor.rkt`: Executes a parsed
  mas-workflow sequentially, chaining results between steps.
- Sequential model: each step receives previous result as `{{result}}`.
- Error stops pipeline: first failure halts remaining steps.
- Reuses `run-subagent-with-config` — no new spawning code.
- HITL approval flows through existing subagent infrastructure.
- **13 new tests** in `tests/test-workflow-executor.rkt`.

### W4: Skill Router Integration + System Prompt
- **`"workflow"` action** in `tool-skill-route`: Loads skill, validates
  `type=mas-workflow`, executes pipeline. Uses `dynamic-require` to break
  circular dependency.
- **`"list"` action** now includes skill type (`standard`/`mas-workflow`).
- **System prompt**: "Available Workflows" section injected when workflow
  skills are present.
- **`skill-tools.rkt`**: Updated tool description and schema with `variables`
  property.
- **4 new tests** in `tests/test-skill-workflow-e2e.rkt`.

### Stats
- 3 new source modules (frontmatter extended, mas-workflow, workflow-executor)
- 5 modified production files (skill-router, skill-tools, run-modes,
  resource-loader, spawn-subagent)
- 1 modified test infrastructure file (scheduler.rkt)
- 49 new tests across 6 test files
- 2 existing test files updated for new defaults

## 0.99.25

Released: 2026-07-19

### Overview
Säule B Completion: HITL (Human-in-the-Loop) Approval TUI Handler. This release
completes the human-approval pillar (§5.3) for multi-agent spawn requests with
dangerous capabilities (shell-exec, git-write). When a subagent requests spawn
with dangerous capabilities, the TUI displays an interactive approval overlay
that blocks until the user explicitly approves (y) or denies (n/Esc). This
replaces the previous always-permissive behavior with a user-controlled gate.

### W0: Approval Channel Infrastructure
- **Async-channel-based blocking mechanism** (`tui/approval-channel.rkt`): Uses
  `racket/async-channel` — the session thread blocks on `approval-await-result`
  while the TUI main thread collects user input. `async-channel-put` is
  non-blocking (unlike `channel-put`), eliminating deadlock risk.
- **Module-level box for thread safety**: Racket parameters are NOT inherited by
  child threads. The approval channel is stored in a module-level `box` (heap-
  allocated mutable cell), shared across all threads.
- **`approval-await-result`**: Blocks with 120s timeout. On timeout → deny (#f).
  On approval → #t. Safe default: both timeout and denial return #f.
- **`spawn-subagent.rkt` integration**: `request-spawn-approval` checks
  `current-approval-channel` — if set (interactive TUI), blocks on
  `approval-await-result`; if #f (non-interactive: CLI, JSON, RPC), falls back
  to the permissive parameter (always #t).
- **12 unit tests** in `tests/test-approval-channel.rkt`.

### W1: TUI Event Reducer + Overlay State
- **`handle-spawn-approval-requested`** handler in
  `tui/state-events/core-handlers.rkt`: Creates `'approval-prompt` overlay with
  styled-line content showing capabilities, task preview, and y/n/Esc prompt.
- Registered as event `"mas.spawn-approval-requested"` (string, not symbol —
  all bus events use string names).
- **Circular dependency fix**: `tui/render/message-layout.rkt` import changed
  from `"../state.rkt"` (which re-exports from state-events → core-handlers,
  creating a cycle) to `"../state-types.rkt"` (only exports struct accessors).
- **10 unit tests** in `tests/test-tui-approval-reducer.rkt`.

### W2: TUI Key Handler + Display
- **`handle-approval-overlay-key`** in `tui/selection.rkt`: Handles
  `#\y`/`#\Y` → approve (`approval-put! #t`) + dismiss overlay,
  `#\n`/`#\N` → deny (`approval-put! #f`) + dismiss overlay,
  `'escape` → cancel/deny (`approval-put! #f`) + dismiss overlay,
  all other keys → `'pass` (overlay stays active).
- Re-exported through `tui/tui-keybindings.rkt` (following tree-overlay pattern).
- Wired into `tui/message-dispatch.rkt` as second intercept:
  tree overlay → approval overlay → normal `handle-key`.
- **10 unit tests** in `tests/test-tui-approval-keyhandler.rkt`.

### W3: Wiring + Integration
- **TUI init wiring**: `run-tui-with-runtime` calls `set-approval-channel!` before
  session creation; `run-tui` (simpler entry point) does the same.
- **TUI teardown wiring**: `clear-approval-channel!` called after loop exits in
  both entry points. Prevents stale channel from leaking to non-TUI modes.
- **Non-TUI modes**: Channel box stays #f (permissive default, unchanged).
- **10 integration tests** in `tests/test-hitl-approval-integration.rkt` covering
  full cross-thread lifecycle: init → spawn → approve/deny → teardown.

### Complete Approval Flow
```
1. TUI init → set-approval-channel! (box set)
2. spawn-subagent → current-approval-channel (reads box)
   → approval-await-result (blocks with 120s timeout)
3. TUI event reducer → creates 'approval-prompt overlay
4. TUI key handler → approval-put! #t/#f + dismiss overlay
5. approval-await-result unblocks → returns result to spawn
6. TUI teardown → clear-approval-channel! (box set to #f)
```

### Testing
- W0: 12 new tests (approval-channel unit tests)
- W1: 10 new tests (approval reducer unit tests)
- W2: 10 new tests (approval key handler unit tests)
- W3: 10 new tests (integration tests)
- 42 new tests total across 4 waves
- All 57 approval-related tests pass (42 new + 15 pre-existing)
- 44 TUI smoke tests pass (no regressions)

### Operational / Release
- Version bumped to 0.99.25.
- 1 new production module (`tui/approval-channel.rkt`).
- 8 production files changed (1 new + 7 modified):
  approval-channel (NEW), spawn-subagent, core-handlers,
  message-layout, selection, tui-keybindings, message-dispatch, tui-init.
- 4 new test files.
- `raco make main.rkt` passes with clean bytecode.

## 0.99.24

Released: 2026-07-18

### Overview
Säule C Completion: Adaptive Verification Production-Ready. This release completes
the adaptive verification pillar (§6.1 complexity heuristic + §6.2 dynamic risk
threshold) to production quality by fixing audit findings, enhancing capability
inference, and adding end-to-end integration tests that verify the full data path.

### Audit Remediation (W0)
- **C-1**: Fixed `test-cli.rkt` regression — 4 test failures from `cli-config` struct
  arity mismatch (17 args, struct now has 20).
- **C-2**: Fixed 40 pre-existing `cli-config` arity failures across 4 additional test
  files (test-cli-interactive.rkt, test-wiring-run-modes.rkt, test-cli-builder.rkt,
  test-wiring-contracts.rkt). All 50 constructor calls now pass 20 args.
- **C-3**: Fixed `get-diff-excerpt` dead code — `file-args` was computed but never
  passed to `system*`. Now uses `git show --stat --oneline HEAD -- <files>` to show
  committed changes at wave-done time (working-tree diff is empty post-commit).
- **C-4**: Corrected v0.99.23 CHANGELOG: acknowledged 4 cli-config arity regressions
  (previously claimed "all existing tests pass").

### Enhanced Capability Inference (W1)
- **FILE-EXTENSION->CAPABILITY table**: 11-entry mapping from file extensions to
  capability symbols (`.rkt`→file-write, `.sh`→shell-exec, `.md`→file-write, etc.).
  Easy to extend without changing inference logic.
- **`infer-capabilities-from-files`** (enhanced): Rewritten to use the extension
  table. Now correctly detects `shell-exec` from `.sh` files (was only
  `.rkt`→file-write).
- **`infer-capabilities-from-tasks`** (NEW): Regex-based heuristic that scans task
  names/actions for `shell`/`bash`/`command`/`exec`→shell-exec and
  `git`/`commit`/`push`/`merge`→git-write.
- **`get-test-summary`** (NEW): Reads cached `.planning/test-results.txt` if available,
  returns descriptive message otherwise (was hardcoded "tests not run").
- **`build-enriched-plan-ctx`** (updated): Combines file-based AND task-based inference
  via `remove-duplicates(append(file-caps, task-caps))`.

### E2E Integration Tests (W2)
- **14 end-to-end tests** verifying the full production data path:
  `gsd-plan`/`gsd-wave` structs → `build-enriched-plan-ctx` → `should-skip-verification?`
  (§6.1) → `effective-risk-threshold` (§6.2).
- Tests use real struct constructors (no mocks), covering: read-only skip, file-write
  non-skip, shell-exec non-skip, large-wave non-skip, empty-wave skip, threshold
  escalation (file-write→medium, shell-exec→low, git-write→low), combined capabilities,
  null plan, missing wave index.

### Testing
- W0: 0 new tests (structural fix — all 50 cli-config calls corrected)
- W1: 11 new tests (26 total enrichment tests, up from 15)
- W2: 14 new E2E integration tests
- 25 new tests total across 3 waves

### Operational / Release
- Version bumped to 0.99.24.
- 2 production files changed (plan-context-builder.rkt, version.rkt + info.rkt + README.md sync).
- 2 new test files (test-adaptive-verification-e2e.rkt + updated test-plan-context-enrichment.rkt).
- All enrichment tests (26), E2E tests (14), and verifier tests pass.
- `raco make main.rkt` passes with clean bytecode.

## 0.99.23

Released: 2026-07-17

### Overview
Two-track release: critical remediation (fix dead plan-context code from v0.99.22) +
Säule B user ergonomics (HITL approval for dangerous spawns + CLI flags for MAS control).

### Remediation (Track 1)
- **B-1**: Fixed empty `plan-context` hash that made §6.1 (complexity heuristic) and
  §6.2 (dynamic risk threshold) dead code in v0.99.22. The verifier gate was receiving
  empty strings for all plan fields, so capability-based verification decisions never
  triggered. New `plan-context-builder.rkt` module now provides real wave data.
- **B-2**: `build-enriched-plan-ctx` populates `files-changed`, `capabilities-used`,
  `diff-excerpt`, `plan-summary`, and `wave-name` from actual plan data.
- **B-3**: Corrected v0.99.22 CHANGELOG file count (2 → 3 production files changed).

### Features (Track 2: Säule B — User Ergonomics)
- **§5.3 HITL Approval for Dangerous Spawns**: Subagent spawns with `shell-exec` or
  `git-write` capabilities now trigger a human-in-the-loop approval gate.
  `requires-hitl-approval?` identifies dangerous capabilities.
  `request-spawn-approval` emits `mas.spawn-approval-requested` events for TUI display.
  Non-interactive mode is permissive (auto-approve) — only interactive/TUI mode blocks.
  Approval denial returns an error result from `run-subagent-with-config`.
- **§5.1 `--agent-pool N` CLI Flag**: Session-wide limit on concurrent subagents.
  New `current-agent-pool-limit` parameter (default 3). `spawn-subagents` respects
  the pool limit. Wired from CLI via `build-runtime-from-cli`.
- **§5.1 `--parallel` CLI Flag**: Enables parallel execution mode. Injects
  partitioning guidance into the system prompt instructing the agent to use
  `spawn_subagents` for task partitioning. Leverages model intelligence — no
  separate partitioning module needed.

### Testing
- W0: 15 new tests for plan-context enrichment
- W1: 15 new tests for HITL spawn approval
- W2: 9 new tests for CLI flags
- All existing MAS tests pass; 4 cli-config arity regressions identified for v0.99.24 fix

### Operational / Release
- Version bumped to 0.99.23.
- 5 production files changed (spawn-subagent.rkt, run-modes.rkt, cli/args.rkt,
  interfaces/cli.rkt, command-handlers.rkt).
- 3 new modules (plan-context-builder.rkt) + test files.
- 39 new tests total across 3 waves.

## 0.99.22

Released: 2026-07-16

### Overview
Three-track release: v0.99.21 audit remediation + Säule A completion (batch capabilities) +
Säule C foundation (adaptive verification). Closes all audit findings from v0.99.21
and introduces complexity-based verifier optimization.

### Remediation (v0.99.21 Audit)
- **A-1**: Fixed `test-subagent-config.rkt` regression — struct arity mismatch from
  the v0.99.21 W2 capability field addition. Test now passes 4/4.
- **A-2**: `spawn-subagents` (batch) now supports per-job `capabilities` filtering.
  Previously only `spawn-subagent` (single) supported capabilities. The batch path
  always gave all tools. Now jobs can specify `capabilities: ["read-only"]` etc.
- **F-5**: Actually updated `.planning/AUDIT-PROCESS-CHECKLIST.md` with clean-bytecode
  `raco make` verification step (was claimed in v0.99.21 but never done).

### Features (Säule C: Adaptive Verification)
- **§6.1 Complexity Heuristic**: The verification gate now skips LLM-based verification
  for trivially simple waves (≤2 files changed, read-only tools only). These waves
  auto-approve with a log message. Non-trivial waves go through full verification
  as before. This reduces verifier latency for simple operations.
- **§6.2 Dynamic Risk Threshold**: The verifier's risk threshold now adjusts dynamically
  based on the wave's capability profile. Shell-exec and git-write waves always get
  the strictest threshold ('low). File-write waves get 'medium. Read-only waves defer
  to the user's configured threshold. This ensures dangerous operations are always
  scrutinized, while simple operations are not over-verified.

### Testing
- W0: 3 new batch-capability tests (total 13 in suite); 1 regression fix
- W1: 16 new tests for complexity heuristic
- W2: 9 new tests for dynamic risk threshold
- All existing MAS tests still pass

### Operational / Release
- Version bumped to 0.99.22.
- 3 production files changed (spawn-subagent.rkt, verifier-gate.rkt, skill-tools.rkt).
- 2 test files changed (1 fix, 1 extend).
- 2 new test files.
- 1 schema update (skill-tools.rkt).

## 0.99.21

Released: 2026-07-15

### Overview
Two-track release: v0.99.20 hotfix (critical build fix) + Säule A agent-driven MAS activation. The hotfix restores the build broken in v0.99.20 (F-1). Säule A makes the primary agent aware of MAS capabilities and able to autonomously delegate to subagents.

### Critical Fixes
- **F-1 (BUILD BREAK)**: Fixed `register-agent!` unbound identifier in `wiring/run-modes.rkt`. The v0.99.20 release could not compile. This hotfix restores the build.
- **F-2**: Wired `mas.verifier.max-rework-iterations` setting to `gsd-max-rework-iterations` parameter. The setting was defined but orphaned in v0.99.20.

### Corrections
- F-3: Documented that rework limit blocks (does not force 'done') — intentional safety design.
- F-4: Corrected v0.99.20 CHANGELOG "8 files fixed" to "7 files fixed + 1 documented obsolete".
- F-5: Updated process checklist to require `raco make` with clean bytecode.

### Features
- **§4.1 System-Prompt MAS Guidance**: The primary agent's system prompt now includes delegation guidance when the blackboard is enabled. The agent knows when to use `spawn-subagent` for parallel work, isolated exploration, second opinions, and high-risk isolation.
- **§4.2 Capability-Aware Spawn**: `spawn-subagent` now accepts an optional `capabilities` argument. When provided, the child agent's tool registry is filtered to only include tools matching the requested capabilities (e.g., `['read-only']` for analyst roles). Backward compatible — omitting `capabilities` provides all child-safe tools.
- **§4.3 Blackboard-Context Injection**: Subagents now receive a compact summary of the parent session's blackboard state as a prefix to their system prompt. This includes recent agent activities, verifier decisions, and wave status.

### Testing
- W0: 4 new tests for rework-limit wiring; `raco make main.rkt` PASSES
- W1: 7 new tests for MAS guidance injection
- W2: 10 new tests for capability-aware filtering
- W3: 6 new tests for blackboard context injection
- All existing MAS tests (135+) still pass

### Operational / Release
- Version bumped to 0.99.21.
- New production file: `agent/mas-guidance.rkt`
- 4 new test files.
- 4 production files changed across W0–W3.

## 0.99.20

Released: 2026-07-14

### Overview

This release addresses four technical debts (§3.1–§3.4) identified in the MAS (Multi-Agent System) Enablement Strategy audit, converting MAS infrastructure from passively wired to actively hardened. The work spans broad test-suite stabilization, extension tool routing, rework-loop protection, and auto-reload watcher wiring.

### Bug Fixes
- **§3.1: Broad-suite stabilization**: Fixed 7 compilation-broken test files and documented 1 obsolete test file (`test-context-assembly-config.rkt` tested a completely refactored struct). Root causes included SDK interface drift, arity mismatches, unbound identifiers, and syntax errors. All 7 fixed files now compile and run.

### Features
- **§3.2: Extension tool routing (delete-lines externalization)**: Externalized the `delete-lines` tool to the worker process sandbox. Added `"delete-lines"` to `externalizable-tool-names` in `registry-table.rkt`. Implemented `execute-delete-lines` in `worker-tools.rkt` with path safety (`path-allowed?`), line range validation, and atomic writes via `call-with-atomic-output-file`. Browser tools (`browser_click`, `browser_type`, `browser_press`) remain in-process pending a proxy architecture (M4/v1.0.0-rc2).
- **§3.3: Rework-loop protection**: Added a rework iteration limit to the GSD state machine. New `gsd-max-rework-iterations` parameter (default 3) blocks the verifying→executing transition when the limit is reached, emitting a `transition-failed` event. The session remains in `verifying` state until manually resolved (via `gsd done` or `gsd reset`). This is intentional — a wave that fails verification 3 times requires human review, not automatic acceptance. The counter resets on fresh plan-written→executing transitions. Configurable via `mas.verifier.max-rework-iterations`.
- **§3.4: Auto-reload watcher wiring**: Connected the registry watcher (fully implemented since v0.99.13 but intentionally unwired) to the runtime lifecycle. When `mas.hot-swap.auto-reload.enabled` is `#t` (default `#f`), the watcher monitors `agent/roles/` for `.rkt` file changes and automatically registers new agent versions via `dynamic-require` + `register-agent!`. The watcher is stopped in `close-session!` (idempotent).

### Breaking / Behavior Changes
- `delete-lines` now executes in the worker process sandbox when externalization is active. Path validation is enforced — only paths within the project root are accepted.
- GSD sessions now have a rework iteration cap (default 3). When the limit is reached, the verifying→executing transition is **blocked** with a `transition-failed` event. The session remains in `verifying` state until manually resolved. This is intentional safety design — a wave that fails verification 3 times requires human review.

### Migration Notes
- No action required for default configurations. All new features are opt-in or use safe defaults.
- To enable auto-reload: set `mas.hot-swap.auto-reload.enabled: true` in config.
- To adjust rework limit: set `mas.verifier.max-rework-iterations` in config.

### Testing
- **W0**: 7 previously compilation-broken test files now compile and run; 1 obsolete test documented.
- **W1**: 5/5 new rework-limit tests pass.
- **W2**: 5/5 new delete-lines worker tests pass.
- **W3**: 5/5 new auto-reload wiring tests pass. 24/24 existing registry/watcher tests pass.
- `raco make main.rkt`: PASS.

### Operational / Release
- Version bumped to 0.99.20. `info.rkt` and `README.md` synced.
- 8 production files changed (1 interface fix in W0, 7 across W1–W3). 3 new test files.

## 0.99.19

Released: 2026-07-13

### Overview

This is a test and documentation only remediation release. It addresses the five findings (A-1 through A-5) from the v0.99.18 post-implementation in-depth audit. Zero production code changes.

### Bug Fixes
- **A-1: Test regressions in config-wiring suite**: Fixed 3 failing assertions in `test-registry-config-wiring.rkt`. The `hot-swap-enabled?` default flipped from `#f` to `#t` in v0.99.18, but 3 tests still asserted `#f`. Updated all 3 to `check-true` with post-flip descriptions. Tests: 8/11 → **11/11**.

### Documentation
- **A-2: Missing W6 audit report**: Created `docs/reports/AUDIT-v0.99.18-POST-IMPLEMENTATION.md` — the persistent audit artifact that was missing from the v0.99.18 milestone. Covers all 7 F-HS findings, CHANGELOG accuracy audit (13/13 verified), architecture assessment. Score: 4.2/5.0 APPROVED.
- **A-3: Stale "pre-flip" descriptions**: Updated header comment, suite name, and CHAR-1 test name in `test-hot-swap-characterization.rkt` to reflect post-Phase-4 state.
- **A-4: Stale comment in deployment gate test**: Updated comment in `test-registry-deployment-gate.rkt` from "default should be #f" to "after explicit set-hot-swap-enabled! #f".
- **A-5: Process checklist improvement**: Added "Before Declaring a Wave Complete" section to `.planning/AUDIT-PROCESS-CHECKLIST.md`, requiring grep for all tests of changed functions before wave completion.

### Breaking / Behavior Changes
- None. This is a test and documentation only release.

### Migration Notes
- None required. No production code changed.

### Testing
- All 11 registry/hot-swap test suites pass: **135/135** (was 132/135).
- W1: Fixed 3 test regressions. Config-wiring: 11/11, characterization: 12/12, deployment gate: 8/8.
- W2: Created missing audit report artifact.
- W3: Process checklist update + version bump.

### Operational / Release
- Version bumped to 0.99.19. `info.rkt` and `README.md` synced.
- Zero production code changes — test and documentation only.

## 0.99.18

Released: 2026-07-13

### Features
- **Hot-swap default-on (MAS Phase 4)**: Agent hot-swap now defaults to enabled (`mas.hot-swap.enabled` defaults to `#t` in `settings-query.rkt`). The dynamic agent loading path in `load-agent-dynamically` is now the primary code path. The runtime registry box (`hot-swap-enabled-box` in `agent/registry.rkt`) still defaults to `#f` — it is bridged at runtime by the wiring layer (`run-modes.rkt`) which reads the settings value. Explicit `mas.hot-swap.enabled: false` in config still disables it.

### Bug Fixes
- **F-HS-01: `mas-envelope` shared module**: Added `'q/util/message/mas-envelope` to `SHARED-MODULES` in `agent/registry.rkt`, allowing dynamically loaded agent modules to require the MAS envelope type.
- **F-HS-03: Agent identity verification**: `load-agent-dynamically` now performs an `agent-role?` check on dynamically loaded factories. If the loaded module does not produce a valid agent role, it falls back to the static factory. This prevents malformed dynamic loads from crashing the agent pipeline.
- **F-HS-05: Dead `decode-mouse-x10` provides**: Removed stale `decode-mouse-x10` re-exports from `tui/tui-render-loop.rkt` and `interfaces/tui.rkt`. The function is defined in `tui/input/state-types.rkt` and remains available via `tui/input.rkt`.
- **F-HS-07: Session teardown clears hot-swap state**: `close-session!` in `runtime/agent-session.rkt` now calls `set-session-active! #f` and `set-hot-swap-enabled! #f`, preventing stale session-active flags from blocking version switches and ensuring clean next-session startup.

### Documentation
- **F-HS-06: Registry watcher documentation**: Added prominent `⚠️ INTENTIONALLY UNWIRED` notice to `agent/registry-watcher.rkt`, clarifying that the module is fully implemented and tested but not connected to runtime startup. The watcher is gated behind `mas.hot-swap.auto-reload.enabled` (default `#f`).

### Breaking / Behavior Changes
- **Hot-swap default-on**: `mas.hot-swap.enabled` now defaults to `#t`. Users who relied on the default-off behavior must explicitly set `mas.hot-swap.enabled: false` in their config to disable it.

### Migration Notes
- To disable hot-swap, add to config: `{"mas": {"hot-swap": {"enabled": false}}}`
- The auto-reload watcher (`registry-watcher.rkt`) remains opt-in behind `mas.hot-swap.auto-reload.enabled: true`.

### Testing
- W0: 12 characterization tests documenting pre-Phase-4 hot-swap behavior (`test-hot-swap-characterization.rkt`).
- W1: Added `mas-envelope` to `SHARED-MODULES`; added 6 identity verification tests (`test-registry-hot-swap.rkt`, 15 total).
- W2: 16 deployment gate tests covering settings parsing, registry gate toggles, supervisor resolution, watcher default-off, session tracking, and E2E integration (`test-hot-swap-deployment-gate.rkt`).
- W3: Flipped `hot-swap-enabled?` default from `#f` to `#t` in `settings-query.rkt`; updated DG-1a test.
- W4: Dead import removal, watcher documentation, session teardown. Added DG-5c test (17 total deployment gate tests).

### Operational / Release
- Version bumped to 0.99.18. `info.rkt` and `README.md` synced.

## 0.99.17

Released: 2026-07-06

### Features
- **Execution plane default-on (MAS Phase 3)**: The execution plane now defaults to enabled (`mas.execution-plane.enabled` defaults to `#t`). Dangerous tools marked `#:dangerous? #t` and `#:externalizable? #t` route through the sandboxed worker process by default. The runtime parameter `current-execution-plane-enabled` still defaults to `#f` — it is activated by the wiring layer (`run-modes.rkt`) after proper worker setup. Explicit `mas.execution-plane.enabled: false` in config still disables it.

### Bug Fixes
- **F-EP-01: `raco test` subprocess compatibility**: Fixed `raco test` custodian interference that caused premature worker subprocess termination. Root cause: worker subprocess drain threads were linked to the test's custodian. Fix: worker subprocesses and drain threads now run under an independent custodian. All gateway IPC tests (19/19) and concurrent IPC tests (12/12) now pass under `raco test`.
- **F-EP-02: Execution plane E2E path resolution**: Fixed `raco test` path resolution using `define-runtime-path` pattern. E2E tests resolve worker-main.rkt to absolute paths at compile time. E2E tests: 7/16 → **16/16** under `raco test`.
- **F-EP-03: Worker-main path resolution**: Same `define-runtime-path` fix for `test-worker-main.rkt`. Worker tests: 17/18 → **18/18** under `raco test`.
- **F-EP-04: Mouse-event contract violation**: Fixed contracts in `tui/input/state-types.rkt` that caused `raco test` contract violations. `decode-mouse-x10` return type corrected from `mouse-event?` to `(or/c list? #f)`. `decode-mouse-message` and `parse-mouse-event` input contracts broadened from `bytes?` to `any/c`. These functions return lists (e.g., `'(mouse click 0 16 16)`), not `mouse-event?` structs.
- **F-EP-05: TUI render loop test pass**: Fixed 11 additional pre-existing `test-interfaces-tui.rkt` failures as a bonus from the contract fix. `test-tui-render-loop.rkt`: 21/22 + 1 error → **25/25**.
- **F-EP-06: Execution plane default flip**: `execution-plane-enabled?` in `settings-query.rkt` now defaults to `#t`. Added 7 deployment gate tests verifying default-on behavior, explicit disable, and timeout defaults.

### Breaking / Behavior Changes
- **Execution plane default-on**: `mas.execution-plane.enabled` now defaults to `#t`. Users who relied on the default-off behavior must explicitly set `mas.execution-plane.enabled: false` in their config to disable it.

### Migration Notes
- To disable the execution plane, add to config: `{"mas": {"execution-plane": {"enabled": false}}}`
- The execution plane only routes tools marked both `#:dangerous? #t` AND `#:externalizable? #t`. Non-dangerous tools are unaffected.

### Testing
- W0: 9 characterization tests for execution plane + gateway IPC.
- W1: F-EP-01 fix. Gateway IPC: 19/19, concurrent IPC: 12/12, characterization: 9/9 under `raco test`.
- W2: F-EP-02/03 fix. E2E: 16/16, worker-main: 18/18 under `raco test`.
- W3: F-EP-04/05 fix. TUI render loop: 25/25, interfaces-tui: 104/106 (2 pre-existing failures).
- W4: F-EP-06 default-on flip. Deployment gate: 7/7. All 125 execution plane tests pass under `raco test`.

### Operational / Release
- Version bumped to 0.99.17. `info.rkt` and `README.md` synced.
- Execution plane is now production-default-on. The worker subprocess runs `racket -tm sandbox/worker-main.rkt` by default.

## 0.99.16

Released: 2026-06-30

### Bug Fixes
- **F-TUI-01: Snapshot clear on resize**: `tui-ctx-resize-ubuf!` now clears `prev-ubuf-box` in addition to `previous-frame-box`. Previously, the delta-render snapshot survived a resize, causing the next incremental render to diff against a stale buffer and produce corrupted display output.
- **F-TUI-02: Periodic full render safety net**: Added `FULL-RENDER-INTERVAL-FRAMES = 300` constant. After 300 consecutive incremental delta renders, a full render is forced to clear any accumulated snapshot drift. The counter resets on full render, forced full render, and resize.
- **F-TUI-03: Delta render row-end clear**: `render-deltas-to-port!` now groups deltas by row and emits `ESC[K` (erase-to-end-of-line) after rows whose last delta has a default/blank cell. This prevents display corruption when row content is shortened and the terminal's actual state has drifted from the tracked snapshot. The `ESC[K` is only emitted when the row was shortened (last delta changed TO a default cell), not when the row ends with non-default content.
- **F-TUI-04: Cursor-blink snapshot consistency**: `render-cursor-blink-frame!` now bypasses the delta renderer and writes the cursor cell directly (position + SGR + character) when a previous snapshot exists. This prevents the F-TUI-03 `ESC[K` emission from corrupting terminal state when the cursor cell toggles to/from a default cell during blink (e.g., cursor past end of text). Falls back to full render when `prev-ubuf` is `#f`.

### Breaking / Behavior Changes
- None. All fixes are internal to the TUI rendering pipeline.

### Migration Notes
- No action required. All fixes are transparent improvements to display stability.

### Testing
- W0: 8 TUI snapshot drift characterization tests establishing bug baselines.
- W1: F-TUI-01 + F-TUI-02 fixes. Updated test #1 from bug-characterization to fix-verification. Added tests 9-13. 13 total tests.
- W2: F-TUI-03 fix. Updated test #7 from `check-false` to `check-true`. Added tests 14-17. 17 total tests.
- W3: F-TUI-04 fix. Exported `write-cell!` for testability. Added tests 18-22. 22 total tests.
- All 22 tests pass. No regressions in existing TUI tests.

### Operational / Release
- Internal rendering pipeline fixes only — no config changes, no API changes.
- `FULL-RENDER-INTERVAL-FRAMES` is a compile-time constant (300 frames ≈ 5 seconds at 60fps).

## 0.99.15

Released: 2026-06-29

### Features
- **Verifier default-on (MAS Phase 2)**: The verifier agent now defaults to enabled (`mas.verifier.enabled` defaults to `#t`). Verification gate runs between executing and idle/done states for GSD `wave-done` commands. Non-GSD sessions are completely unaffected. Safe fallback chain: no provider → auto-approve, error → escalate, timeout → escalate.
- **Conservative risk threshold**: `mas.verifier.risk-threshold` default changed from `'medium` to `'high`, so the verifier gate only escalates genuinely high-risk decisions by default.

### Hot-Swap Bug Fixes (W1)
- **F-11**: `registry-defaults.rkt` now uses `#%variable-reference` + `split-path` for absolute module paths instead of relative strings. Fixed `path-only` unbound error.
- **F-12**: `SHARED-MODULES` populated with `'q/agent/roles/base` and `'q/util/capability` in `registry.rkt`. `namespace-attach-module` wrapped in per-module `with-handlers` for test resilience.
- **F-13**: `set-hot-swap-enabled!` now wired from settings in `run-modes.rkt`.
- **F-14**: `set-session-active!` called at session start in `run-modes.rkt`.
- Contract broadened: `agent-descriptor module-path` from `(or/c #f string?)` to `(or/c #f module-path?)`.
- Registry hot-swap tests: 8/9 → **9/9** (all green from both `q/` and `q/tests/` directories).

### Audit Debt Closure (W2)
- **F-15**: Fixed v0.99.13 CHANGELOG inaccuracies (hot-swap test count, re-export direction).
- **F-16**: Fixed `registry-watcher.rkt` comments (false `filesystem-change-evt` claim, "minor version" → "patch version", removed shadowed local `last`/`caddr` definitions).
- **F-17**: Strengthened E2E-2 token-stripping assertion — bash command dumps env vars, test asserts capability secret does NOT appear in response.
- **F-18**: Added contracts to all 5 `registry-watcher.rkt` provides.
- **F-19**: Renamed E2E-5 for accuracy: "no server running → executor connection fails (fail-closed)".
- **v0.99.14-F-01**: Fixed CHANGELOG "Token budget guard (W3, forthcoming)" → past tense.
- **v0.99.14-F-02**: Fixed CHANGELOG test count "115 existing" → "106 existing".

### Breaking / Behavior Changes
- `mas.verifier.enabled` default changed from `#f` to `#t`. GSD sessions will now run the verification gate by default.
- `mas.verifier.risk-threshold` default changed from `'medium` to `'high`.
- To disable verification: set `mas.verifier.enabled = false` explicitly in config.
- `verifier-risk-threshold` invalid-value fallback changed from `'medium` to `'high`.

### Migration Notes
- No action required for most users — verifier is safe-by-default with auto-approve fallback when no provider is configured.
- To opt out: set `mas.verifier.enabled = false` in your config file.
- The `current-verifier-enabled` runtime parameter default remains `#f` (uninitialized safety); the settings default is wired in during session start.

### Testing
- W0: 7 verifier characterization tests (`test-verifier-deployment-gate.rkt`) — default behavior, explicit on/off, risk threshold, gate with/without provider.
- W1: 8 registry deployment gate tests + hot-swap tests now 9/9 from all directories.
- W2: All audit findings F-15 through F-19 + v0.99.14-F-01/F-02 addressed. Watcher contracts added.
- W3: All verifier tests updated for new defaults — 7/7 deployment gate, 26/26 integration, 20/20 gate, 30/30 core, 25/25 hardening, 17/17 prompt, 30/30 types, 7/7 wiring.
- All 155 existing verifier tests green.
- All 61 existing registry tests green.

### Operational / Release
- Feature gate: `mas.verifier.enabled` (default `#t`, was `#f`).
- Risk threshold: `mas.verifier.risk-threshold` (default `'high`, was `'medium`).
- Safe fallback: no verifier provider → auto-approve (no LLM call).
- Scope: verifier only activates for GSD `wave-done` commands.

## 0.99.14

Released: 2026-06-28

### Features
- **Blackboard default-on (MAS Phase 1)**: The blackboard subsystem now defaults to enabled (`mas.blackboard.enabled` defaults to `#t`). This enables zero-latency event-bus-driven blackboard updates, context injection of wave/task status into the system prompt, and crash recovery from `trace.jsonl` — all by default, with no configuration required.
- **Session lifecycle cleanup**: `close-session!` now calls `stop-blackboard-subscriber!` to prevent event bus subscription leaks when sessions end. The call is idempotent and safe when no subscriber is active.

### Breaking / Behavior Changes
- `mas.blackboard.enabled` default changed from `#f` to `#t`. Sessions will now start the blackboard subscriber and inject context by default.
- To disable: set `mas.blackboard.enabled = false` explicitly in config.

### Migration Notes
- No action required for most users — blackboard is passive and low-overhead.
- To opt out: set `mas.blackboard.enabled = false` in your config file.
- The blackboard subscriber is stopped automatically on session close (W1 wiring).

### Testing
- W0: 6 characterization tests (`test-blackboard-deployment-gate.rkt`) — default behavior, context snippet, subscriber idempotency.
- W1: 3 session lifecycle cleanup tests (`test-blackboard-lifecycle.rkt`) — subscription cleared, idempotent stop, safe no-op.
- All 106 existing blackboard tests green.
- Broad fast gate: zero regressions vs. v0.99.13 baseline.

### Operational / Release
- Feature gate: `mas.blackboard.enabled` (default `#t`, was `#f`).
- Trace logger dependency: crash recovery reads `trace.jsonl` from the session directory if it exists.
- Token budget guard: context injection is capped at 500 characters (implemented in W3).

## 0.99.13

Released: 2026-06-27

### Features
- **Blackboard follower extraction (G-2)**: Extracted `normalize-jsonl-entry`, `rebuild-blackboard-from-log!`, `blackboard-relevant-event?`, and `relevant-event-names` from `blackboard-subscriber.rkt` into a new `blackboard-follower.rkt` module. This separates crash-recovery/JSONL replay from the event-bus subscription lifecycle, enabling standalone log-tailing.
- **Namespace-based hot-swapping (G-3)**: Agent role registry now supports dynamic-require-based loading via `#:module-path` + `#:factory-name` on `register-agent!`. When `hot-swap-enabled?` is `#t`, `make-agent-instance` loads agent modules in a fresh namespace with `namespace-attach-module` for type identity. Default-off: zero behavioral change.
- **Registry watcher (G-4)**: New `registry-watcher.rkt` module provides file-system monitoring of the `agent/roles/` directory. `start-registry-watcher!` starts a polling thread, `stop-registry-watcher!` terminates cleanly. Includes `path->role-name` and `next-version` utilities for automatic role version incrementing.
- **E2E distributed execution tests (G-5)**: 6 comprehensive end-to-end tests covering the full tool→gateway→TLS→executor→result round-trip with real mTLS certificates.
- **Bug fix (F-09)**: `return-identity` helper in remote-executor did not provide early-exit semantics; rewritten with proper `if/cond` control flow.
- **Bug fix (F-10)**: Capability token was not stripped from arguments before tool dispatch; fixed with `hash-remove`.

### Breaking / Behavior Changes
- `agent-descriptor` struct gains `factory-name` field (6 fields, was 5). Code constructing descriptors must add `#f` for the new field.
- `register-agent!` signature extended with optional `#:factory-name` keyword arg (default `#f`).
- `register-default-agents!` now passes `#:module-path` and `#:factory-name` to enable dynamic-require hot-swapping.
- `make-agent-instance` and `make-agent-instance-versioned` now check `hot-swap-enabled?` gate before attempting dynamic-require.
- `activate-agent-version!` now logs a warning when called during an active session.

### Migration Notes
- No configuration changes required. All new features are default-off.
- To enable hot-swapping: set `mas.hot-swap.enabled = true` and `mas.hot-swap.auto-reload.enabled = true`.
- `blackboard-subscriber.rkt` re-exports follower symbols (extracted from it) for backward compatibility.

### Testing
- Blackboard follower: 10/10 tests green.
- Registry hot-swap: 8/9 tests green (1 dynamic-require path failure due to relative module path resolution, fixed in v0.99.15 W1).
- Registry watcher: 9/9 tests green (file detection, modified files, start/stop lifecycle, no-leak).
- E2E distributed execution: 6/6 tests green.
- All existing registry tests: 31 + 11 + 7 + 10 + 2 = 61 tests green.
- Broad fast suite: 916 files, 835 passed, 80 failed (pre-existing debt, zero regressions); 11461 tests, 11339 passed, 122 failed.

### Operational / Release
- Feature gates: `mas.hot-swap.enabled` (default `#f`), `mas.hot-swap.auto-reload.enabled` (default `#f`).
- Dynamic-require fallback: if module loading fails, registry falls back to static factory with a warning log.
- Session safety: `activate-agent-version!` warns when session is active, deferring switch to next session.
- Registry watcher uses custodian-based cleanup to prevent thread leaks.

## 0.99.12

Released: 2026-06-26

### Features
- **Distributed execution via mTLS TCP broker**: high-risk tool execution can now be routed to a remote executor node over mutual TLS, isolating hostile code from the developer machine.
- Certificate infrastructure: `q generate-certificates` command creates a local CA, server cert, and client cert (RSA 4096, 365-day validity).
- Remote IPC client: async TLS client (`remote-ipc.rkt`) with JSON-RPC over TLS, request/response matching by request-id, drain thread for async responses.
- Executor node server: TLS server (`executor-server.rkt`) with per-connection threads, capability token validation, request size limits, and graceful shutdown.
- Risk-based routing integration: tool-gateway now dispatches `'remote` decisions to the actual remote executor when `mas.broker.enabled = true`.
- Connection resilience: circuit breaker (3-state: closed/open/half-open, threshold 5, cooldown 30s), automatic reconnection with exponential backoff, background health check thread.
- Configuration: new `mas.broker.*` settings (`enabled`, `remote-host`, `remote-port`, `cert-dir`, `capability-secret`) — all default-off.

### Breaking / Behavior Changes
- `routing-decision->execution-route` now returns `'remote` for high/critical risk decisions (was `'remote-tagged-but-executed-local`). When broker is disabled (default), remote decisions fall back to local execution.
- `broker-enabled?` now reads `mas.broker.enabled` (was hardcoded `#f`). Default remains `#f`.
- When `mas.broker.enabled = true` but `capability-secret` is not set, q fails fast with an error (fail-closed).

### Migration Notes
- No configuration changes required for local-only operation. The broker is strictly opt-in.
- To enable remote execution: set `mas.broker.enabled = true`, configure `remote-host`, `remote-port`, `cert-dir`, and `capability-secret`.
- Run `q generate-certificates` to create the required mTLS certificates.
- See `docs/distributed-execution-guide.md` for the 5-step deployment guide.

### Testing
- Focused gates pass: TLS contexts 8/8, cert generator 5/5, remote IPC 5/5, remote executor 4/4, executor server 10/10, remote routing 10/10, gateway bridge remote 5/5, routing policy 11/11 + 5/5 integration, remote IPC resilience 11/11, remote executor security 11/11.
- Circuit breaker: 7 unit tests (closed→open, open→reject, cooldown→half-open, half-open recovery) + 4 integration tests.
- Adversarial security: 7 token validation tests + 4 integration tests (circuit storm prevention, prompt injection as data, mTLS rejection).
- Broad fast suite: 913 files, 831 passed, 81 failed (pre-existing debt, no Phase 2 regressions); 11433 tests, 11310 passed, 123 failed.

### Operational / Release
- Certificate generation uses RSA 4096-bit keys with SHA-256 signatures and TLS 1.2 (TLS 1.3 compatible).
- Capability tokens use HMAC-SHA256 with 5-minute TTL and constant-time MAC comparison.
- Circuit breaker protects against connection storms: 5 consecutive failures → circuit open → fast-fail for 30s → half-open trial.
- Zero network surface when disabled: no TCP listeners, no sockets, no cert reads.
- Defense in depth: transport (mTLS) + application (capability tokens) + deployment (config gate) + execution (sandbox).

## 0.99.11

Released: 2026-06-15

### User-Visible Changes
- Hardened MCP Phase 1 security: `tools/call` now routes through governed scheduler execution with real `exec-context`, eliminating the direct `tool-execute` bypass.
- Malformed JSON-RPC `tools/call` params now return `-32602 Invalid params` instead of crashing the handler.
- Internal exception details are no longer exposed to MCP clients in error responses.
- Capability token symbol validation tightened to match string input strictness.
- MCP stdio server now returns JSON-RPC 2.0 spec-compliant `-32700 Parse error` and `-32600 Invalid Request` responses for malformed input (previously silently dropped).
- Broad-suite failure baseline documented in `docs/reports/BROAD-SUITE-DEBT-TRIAGE.md`.

### Breaking / Behavior Changes
- MCP `tools/call` malformed params (non-hash params, non-string name, non-hash arguments) now return JSON-RPC `-32602` instead of throwing uncaught exceptions.
- MCP `tools/call` internal errors return generic `-32603 Internal error` without `data.detail` field.
- Capability symbols containing colons or invalid characters are now rejected at signing time via `capability-input?` contract.
- MCP stdio server now returns JSON-RPC error responses (instead of silently dropping) for invalid JSON (`-32700`) and non-object JSON (`-32600`).

### Migration Notes
- No configuration changes required. MCP remains disabled by default.
- Existing valid capability tokens remain backward-compatible.

### Testing
- Focused gates pass: MCP adapter 31/31, config 17/17, events 12/12, integration 10/10, protocol compliance 7/7, security gates 19/19, capability tokens 22/22, hardening 18/18.
- Broad fast suite: 912 files, 835 passed, 77 failed; 11425 tests, 11305 passed, 120 failed; 5 zero-parsed sentinels (known unrelated pre-existing debt).

### Operational / Release
- Fixed F-01 (CRITICAL): MCP `tools/call` now routes through governed scheduler execution (`run-tool-batch`) via `make-mcp-governed-execute-fn` with a real `exec-context`.
- Fixed F-02 (HIGH): `handle-tools-call` validates params/name/arguments types and returns `-32602` for malformed input.
- Fixed F-03 (MEDIUM): `handle-tools-call-result` no longer exposes internal exception details.
- Fixed F-04 (MEDIUM): Gate evidence files updated from stale `0.94.9` to current truthful `0.99.11` status; committed evidence artifact added to `docs/reports/`.
- Fixed F-05 (LOW): CHANGELOG W6 wording corrected to past tense; C1 claim qualified.
- Fixed F-06 (LOW): `capability-input?` contract tightened for symbol inputs.

---

## 0.99.10

Released: 2026-06-15

### User-Visible Changes
- Hardened MCP Phase 1 server behavior while keeping MCP disabled by default behind `mas.mcp.enabled` and `mas.mcp.server.enabled`.
- Documented local-only Phase 1 transport limits: `stdio` is the supported transport; no network broker, mTLS channel, or remote executor exists in this release.
- Documented capability-token validation APIs and security properties in `docs/mcp-capability-security.md`.

### Breaking / Behavior Changes
- Invalid `mas.mcp.server.transport` values now fall back to `"stdio"` instead of crashing.
- MCP `tools/call` now rejects unknown tools before execution and returns JSON-RPC errors for invalid params/internal failures.
- Empty capability-token/HMAC secrets are rejected.
- Legacy `validate-capability-token` remains backward-compatible; claim-aware validation is available through the new claims APIs.

### Migration Notes
- MCP users must enable both `mas.mcp.enabled` and `mas.mcp.server.enabled` for local server mode.
- Do not expose the Phase 1 MCP server over a network wrapper; remote execution is deferred to Phase 2.
- Use `validate-capability-token-for-agent` when agent scope must be enforced.

### Testing
- Focused remediation gates passed for MCP security/config/protocol/integration/events, routing policy integration, and capability-token hardening.
- Required fast-suite gate was run after each implementation wave; it timed out after 20 minutes with pre-existing broad-suite failures/hangs and is recorded as not green.

### Operational / Release
- Remediated v0.99.9 post-audit blockers C2, H1-H5, and M1-M5 across W1-W4.
- Completed W5 documentation/release-hygiene artifacts and W6 version bump/audit.
- Note: C1 (governed MCP execution) was partially addressed in v0.99.10; the independent post-remediation audit found F-01 (CRITICAL) remained open. C1/F-01 is fully closed in v0.99.11.

---

## [0.98.6] — 2026-06-12

### Browser Audit Final Closure (13 findings from v0.98.3–v0.98.5 audit chain)

#### Correctness & Dedup (W0)
- **F-03 (MODERATE)**: Deduplicated `truncate-observation-text` to delegate to `util/truncation.rkt::truncate-to-n-chars` (#7895)
- **F-04 (MODERATE)**: Removed redundant `'type` key from Gemini image block shape — canonical `{inlineData: {mimeType, data}}` (#7895)
- **F-08 (LOW)**: Applied `truncate-observation-text` to `dom-summary` field in `observation->hash` for defense-in-depth (#7895)
- **NF-11d (LOW)**: Added dom-summary truncation test (#7895)

#### Test Quality & Contracts (W1)
- **F-07 (LOW)**: Added boundary tests for truncation (4000/4001 chars, empty string) (#7900)
- **F-06 (MODERATE)**: Verified NF-03 behavioral test covers dynamic custodian reading (#7900)
- **CON-01/02/03 (LOW)**: Added contracts to browser module public functions (#7900)

#### Code Quality & Hygiene (W2)
- **REP-01 (LOW)**: Replaced `(struct-out browser-adapter)` with explicit provides — no longer exposes internal function slots
- **ARCH-01 (LOW)**: Documented `browser/settings.rkt→runtime/` upward coupling with migration path to v0.99.x
- **ARCH-02/F-09 (LOW)**: Merged stray `require` into main require block in `anthropic.rkt` and `gemini.rkt`

### Critical Bugfix
- **Watchdog→Goal Cancel**: Removed `goal-cancel-box = #t` from watchdog force-clear — watchdog now only clears stale UI state, does NOT kill background goal threads. Fixes autonomous goals being falsely cancelled with "Goal cancelled by user" after 5 minutes. (#W3)

---

## [0.98.5] — 2026-06-11

### Test Foundation

- **BTF-01 (MODERATE)**: Fixed 56/59 browser test failures by updating `browser-settings` constructors to the current 17-field struct arity across `test-browser-service.rkt`, `test-browser-service-adapter.rkt`, and `test-browser-tools.rkt` (#7874)

### Browser Context Overflow

- **NF-11a (LOW)**: `observation->hash` now truncates `text-content` and `visible-text` to 4000 characters before returning tool results, preventing oversized browser observations from consuming context budget (#7878)
- **NF-11b (LOW)**: Fixed double-condition truncation bug in `summarize-tool-result`: any tool result over 8000 characters is now truncated, regardless of line count (#7879)
- **NF-11c (LOW)**: Added regression tests in `tests/test-context-overflow-browser.rkt` covering observation hash truncation and both single-line and multi-line tool-result truncation (#7880)

### Code Quality

- **F-05 (LOW)**: Changed Gemini image-format discriminator from `"inline_data"` to `"inlineData"` for API consistency (#7882)
- **F-08 (INFO)**: Added defensive comment in `playwright-sidecar.rkt` documenting that `cfg` capture in `make-reader-body` is safe-by-design (old reader is killed via custodian before new state is created) (#7882)
- **F-09 (INFO)**: Moved stray `require` after module doc-comments in `llm/anthropic.rkt` and `llm/gemini.rkt` (#7882)

### Test Quality

- **F-06 (LOW)**: Replaced structural `procedure?` check with behavioral test in `tests/test-browser-audit-w1-v0984.rkt`: verifies `start-heartbeat!` reads the custodian from state each iteration by mutating the state's custodian after thread creation (#7886)
- Made `heartbeat-interval-secs` a parameter in `playwright-sidecar.rkt` to enable the behavioral NF-03 test (#7886)
- **GAP-V2 (LOW)**: Verified clarifying comment in `extensions/image-pipeline.rkt` documenting that the resize pipeline is scaffold-only and not yet wired to browser screenshots (#7888)

## [0.98.4] — 2026-06-11

### Critical Fixes

- **NF-01 (CRITICAL)**: `adapter-screenshot` now decodes base64 string to actual bytes before storing in `screenshot-bytes`, fixing 100% crash rate of `browser_screenshot` (#7858)

### Cross-Provider Vision

- **NF-02 (MODERATE)**: Gemini image block keys changed from snake_case `inline_data`/`mime_type` to camelCase `inlineData`/`mimeType` matching the Gemini REST API (#7858)

### Sidecar Lifecycle Hardening

- **NF-03 (MODERATE)**: `start-heartbeat!` reads custodian from state inside loop instead of capturing at thread creation, preventing stale custodian after restart (#7859)
- **NF-04 (LOW)**: `send-command!` removes orphaned async-channel from pending hash before raising on dead sidecar (#7859)
- **NF-05 (LOW)**: `restart-sidecar!` enforces restart semaphore presence, no fallback (#7859)
- **NF-06 (LOW)**: `make-reader-body` enforces pending semaphore, no unsafe fallbacks (#7859)

### Code Quality

- **NF-07 (LOW)**: Removed dead `(require json)` in `workflow.rkt` (#7860)
- **NF-08 (LOW)**: Removed redundant duplicate `require "settings.rkt"` in `service.rkt` (#7860)
- **NF-09 (LOW)**: Removed redundant `(only-in racket/base ...)` import in `playwright-sidecar.rkt` (#7860)
- **NF-10 (INFO)**: Extracted duplicated `parse-data-url` to shared `llm/vision-helpers.rkt` (#7860)

## [0.98.3] — 2026-06-11

### Security & Robustness

- **SEC-09 (CRITICAL)**: Port write errors in `send-command!` now raise `q-browser-error` with category `'sidecar-crash` instead of propagating raw `exn:fail:port` (#7824)
- **SEC-01 (CRITICAL)**: `restart-sidecar!` guarded by `restart-sema` to prevent concurrent restart races (#7824)
- **SEC-04 (CRITICAL)**: Subprocess launch failures in `restart-sidecar!` revert state fields to `#f` and raise structured `q-browser-error` (#7824)
- **SEC-02 (MODERATE)**: `ensure-state` uses double-checked locking with `launch-sema` to prevent TOCTOU orphaned sidecars (#7824)
- **ERR-01 (MODERATE)**: Bare `(error ...)` in adapter dispatch replaced with `raise-browser-error` (#7824)
- **SEC-07 (MODERATE)**: `browser-check-local-app` clamps `timeout_ms` to `[1000, 60000]` (#7826)
- **SEC-10 (MODERATE)**: `restart-sidecar!` readiness probe uses `send-command!` `"ping"` instead of fixed `(sleep 0.5)` (#7826)
- **SEC-15 (MODERATE)**: Mutable `dead?` flag on `playwright-sidecar-state` detects reader EOF before commands are sent (#7826)
- **SEC-05 (MODERATE)**: Heartbeat thread reads `reader-thread` from current state each cycle, avoiding stale closure after restart (#7828)
- **SEC-03 (LOW)**: `browser-session-manager-count` wrapped in `with-session-lock` (#7829)
- **SEC-06 (LOW)**: Workflow cleanup thunk only swallows `session-expired` errors; real errors re-raised (#7829)
- **SEC-08 (LOW)**: Removed semaphore fallback in `send-command!`; missing `'pending-sema` now raises `q-browser-error` (#7829)
- **SEC-13 (LOW)**: DOM traversal in `page-state.js` bails out after 10k elements, returning `scannedCount` (#7829)

### Code Quality

- **DUP-01/02**: Extracted `make-reader-body` and `launch-sidecar-process!` helpers, eliminating ~85 LOC duplication between `launch-sidecar!` and `restart-sidecar!` (#7828)
- **DEAD-01–05**: Removed unused `ready-ch`, `racket/match` imports, `crypto-random-bytes`, and `bytes->hex-string` (#7829)

### Cross-Provider Vision (GAP-V1)

- **Anthropic**: `anthropic-build-request-body` converts OpenAI-format `image_url` content blocks to Anthropic `image` + `source` blocks (#7830)
- **Gemini**: `gemini-build-request-body` converts OpenAI-format `image_url` content blocks to Gemini `inline_data` parts (#7830)

### Deferred

- **GAP-V2**: `extensions/image-pipeline.rkt` resize pipeline not yet wired to browser screenshots; documented for v0.99.x (#7830)

## [0.97.19] - 2026-06-10

### Changed
- F11 (MODERATE): Declare turn-orchestrator as composition root in dependency-policy.rktd
- F12 (MODERATE): Document provider factory registry integration path (architecture note)
- F13 (MODERATE): Add ctx-version field to extension-ctx for schema migration
- F15 (MODERATE): Create docs/config-schema.rktd documenting all configurable keys
- F5 (HIGH): Add session facet structs (provider, tool, identity) + extractors

### Added
- F17a: Provider conformance fitness tests (PC-1, PC-2)
- F17b: Session recovery invariant tests (SR-1)
- F17c: Dependency policy completeness tests (CD-1, CD-2)
- Fitness tests for F6 settings split, F13 ctx-version, F11 composition root

### Skipped
- F7 (CSI extraction): Functions are 1-3 lines, only used in terminal.rkt
- F9a/b (error narrowing): Broad catches are defensive wrappers around user callbacks
- F14 (event wiring table): 6 subscribers in 1 file doesn't justify table abstraction
- F16 (defaults consolidation): Parameters well-scoped to domains

## [0.98.2] — 2026-06-10

### DOM Tagging & Element Targeting

- **q-id injection**: `injectQIds()` annotates interactive elements with unique `q-id` attributes (#7806)
- **Accessibility tree**: `extract()` returns `interactiveElements` array (qId, tag, text, href, etc.) (#7806)
- **Observation parser**: `browser-observation` struct extended with `interactive-elements` field + serialization (#7807)
- **Tool descriptions**: `browser_click/type/extract` descriptions guide LLMs to use `[q-id="N"]` selectors (#7808)
- **Regression tests**: Full round-trip and backward compatibility verified (#7809)


## [0.98.1] — 2026-06-10

### Multimodal Screenshot Pipeline

- **image-part content type**: New `image-part` struct in `content-parts.rkt` with JSON round-trip serialization (#7800)
- **Provider vision API integration**: `build-raw-messages` produces OpenAI content arrays for image+text user messages (#7801)
- **Vision settings**: `vision-enabled?` (default OFF), `vision-detail`, `vision-ephemeral-turns` in `browser-settings` (#7801)
- **Screenshot dual-path**: `browser_screenshot` returns `image-part` when vision enabled, hash otherwise (#7802)
- **Ephemeral vision context**: `strip-image-parts` replaces old screenshots with text summaries to manage context budget (#7803)


## [0.98.0] — 2026-06-10

### Browser Robustness

- **Sidecar auto-recovery**: `send-command-with-recovery!` retries up to 2 times on sidecar crash with automatic restart (#7795)
- **Crypto-quality session IDs**: Session IDs generated with `crypto-random-bytes` instead of `current-milliseconds` + `random` (#7797)
- **Screenshot size enforcement**: `enforce-screenshot-max-bytes` truncates oversized screenshots per `screenshot-max-bytes` setting (#7796)
- **Session manager thread safety**: Semaphore-wrapped mutations prevent concurrent access corruption (#7796)
- **Workflow try/finally**: `browser-check-local-app` uses `dynamic-wind` to guarantee session cleanup on errors (#7797)
- **UUID tests**: 6 new tests for sidecar recovery, UUID format/uniqueness/crypto-quality


## [0.97.18] - 2026-06-10

### Changed
- F3 (MODERATE): Extract ui-state protocol to ui-core/ui-state-protocol.rkt,
  breaking extensions→tui layer dependency. Remove unused tui/state.rkt import
  from widget-lifecycle.rkt.
- F4 (MODERATE): Command-registry types verified already extracted — no change
- F6a (MODERATE): Extract settings loading/merging to runtime/settings-core.rkt
- F6b (MODERATE): Extract settings query functions to runtime/settings-query.rkt
- settings.rkt is now a 28-line facade re-exporting both sub-modules
- Fix: trace-sink.rkt forward reference (contract aliases moved after interface)

### Skipped
- F7 (CSI extraction): Functions are 1-3 lines, only used in terminal.rkt —
  doesn't meet Abstraction Gate
- F14 (event wiring table): 6 subscribers in 1 file — doesn't meet Abstraction Gate
- F16 (defaults consolidation): Parameters well-scoped to domains — doesn't
  meet Abstraction Gate

## [0.97.17] - 2026-06-10

### Changed
- F1 (CRITICAL): pkg/registry.rkt — replace 24 any/c with typed contracts
  (package-index/c, package-entry/c, install-result/c, etc.)
- F2 (CRITICAL): runtime/trace-sink.rkt — add trace-sink-instance/c and
  trace-sink-class/c predicates, document class contract limitations
- F8 (HIGH): tui/tui-init.rkt — tighten entry point contracts with path-string?
  and document opaque runtime/context params
- F10 (MODERATE): llm/model.rkt — message-list/c → (listof hash?),
  tool-list/c → (or/c (listof hash?) #f)
- F21 (LOW): ui-core/dispatch.rkt — document intentional any/c for dispatch context
- F23 (LOW): Add layer-adapters.rkt size budget test (RA-5a ≤120 lines)
- F24 (LOW): Add make-* naming aliases (make-agent-session, make-tui-session,
  make-provider-for-name) alongside create-* originals

### Tests
- New: tests/test-pkg-registry-contracts.rkt (18 contract-blame tests)
- Updated: test-arch-fitness.rkt (+1 RA-5a test, 48 total)

## [0.97.16] - 2026-06-10

### Fixed
- B1: Browser policy now allows common dev ports (3000, 3001, 5173, 8000, 8080, 8443)
  by default instead of blocking all localhost
- B2: TUI busy-state watchdog reduced from 30 min to 5 min (matches 120s tool timeouts)
- B3: Tool execution progress events emitted for long-running tool batches —
  TUI status bar now shows "N tools running" instead of silent hang
- B4: Hash truncation now preserves metadata fields (session-id, status, url) —
  fixes browser_open session-id loss that caused LLM to hallucinate non-existent
  session IDs, breaking all subsequent browser_screenshot/click calls
- B5: Steering queue message now includes "Press Esc to cancel" hint

## [0.97.15] - 2026-06-10

### Fixed
- Bug fix: browser_screenshot blows up context window (4 root causes)
  - summarize-tool-result ignores tool-result-part (truncation bypassed)
  - build-raw-messages missing #:handle-hash? (270K base64 dumped verbatim)
  - result-content->string has no binary/base64 guard
  - context.pressure only emitted once per prompt (stale ctx:1% in TUI)
- M6: check-rollback-triggers imperative set! → pure for/fold
- M10: Extract message->provider-hash to provider-hash-bridge.rkt
- M11: session-index builders mutable make-hash → immutable for/fold
- M11: bm-counter bare set! → boxed counter with semaphore
- L2: settings-cache thread safety documentation
- L3: Remove unused define-tool-schema macro + test (87 LOC deleted)
- L4: Deprecation comment on unused with-output-guard macro
- L9: Add CONSUMERS docs to 18 internal modules
- L10: Add v1.0.0 removal timeline to bump-version.rkt

### Skipped
- M5: Parameter → session-config migration (33 files, zero functional gain)
- M7: Dual working-set unification (low value for hotfix milestone)
- L1: event-bus box → #:mutable (moderate risk, low value)
- L5: with-logged-catch → macro (3 callers, stable code)
- L7: state-types.rkt responsibility split (3h estimate)

## [0.97.14] - 2026-06-10

### Fixed
- Bug fix: ALL tool failures now show actual error in TUI instead of generic "tool failed"
  - Added result-error field to tool-execution-end-event (event layer)
  - Extract error text from tool-result in tool-coordinator (runtime layer)
  - TUI reads result-error from event payload (presentation layer)
  - Added with-handlers to all 8 browser tool handlers (error wrapping)
  - Increased navigate timeout from 10s to 30s (timeout mismatch)
- Bug fix: browser_screenshot blows up context window (4 root causes)
  - summarize-tool-result ignores tool-result-part (truncation bypassed)
  - build-raw-messages missing #:handle-hash? (270K base64 dumped verbatim)
  - result-content->string has no binary/base64 guard
  - context.pressure only emitted once per prompt (stale ctx:1% in TUI)
- M1: Tighten step-interpreter any/c contracts (9 replacements)
- M2: browser/types.rkt struct-out → explicit provides (11 structs)
- M3: gsd/plan-types.rkt struct-out → explicit provides (7 structs)
- M4: all-from-out → explicit provides in 5 facade modules (~12 replacements)
- M9: Parameter naming convention (3 renames to current- prefix)
- M9: MAX-STREAM-CHUNKS converted from make-parameter to define constant
- H6: Deprecated legacy global GSD state-machine API (4 functions)

### Skipped
- M8: field->json-key duplication — Racket phase system requires separate runtime/compile-time implementations

## [0.97.13] - 2026-06-10

### Fixed
- C1: SDK struct-out → explicit provides in sdk-core.rkt, sdk-compat.rkt
- H4: conclusion-graph mutable → immutable (for/fold patterns)
- H5: Embedding cache thread safety (semaphore-guarded struct)
- H1: rollback-actions config struct for snapshot/grouped access
- H3a: Extracted wire-runtime-parameters! from run-modes (eliminates ~60% duplication)
- H3b: Extracted LLM callback factories (make-distill-callback, make-reflection-callback)
- H2a: Migrated ~44 with-handlers silent-swallow sites to with-safe-fallback (logs warnings)

## [0.97.12] - 2026-06-08

### Fixed
- GAP-J: Added field validation to hash->conclusion (id, text, category)
- GAP-J: Added duplicate-ID warning in build-conclusion-graph
- GAP-K: Removed spurious deprecation warning from build-assembled-context/raw
- GAP-L: Excluded 'fact' from high-value-categories for auto-persistence
- GAP-M: Capped current-rollback-action-log at 100 entries
- GAP-N: Added max-tokens enforcement to ws-context closure
- GAP-P: Replaced kill-thread with custodian-shutdown-all in auto-distillation

### Tests
- 5 data validation tests (test-conclusion-graph)

## [0.97.11] - 2026-06-08

### Fixed
- GAP-E: Added session.closed subscriber for belt-and-suspenders conclusion persistence
- GAP-E: Added dedup set (current-mid-session-persisted-ids) to mid-session bridge
- GAP-F: Complete reload-config! — refresh all 10 memory/context parameters on hot-reload
- GAP-H: Replace fragile string-contains? matching in warnings->actions with symbol-based matching
- GAP-I: Extract coerce-task-state helper (was duplicated ×4)

### Tests
- 3 dedup parameter tests (gapf-mid-session-bridge)
- 6 symbol-based rollback matching tests (rollback-actions)

## [0.97.10] - 2026-06-08

### Fixed
- GAP-A: Unified ranking — fallback-select-conclusions now delegates to rank-and-budget
- GAP-B: Removed idle guard — WS evolution correctly runs on idle transitions (any→idle)
- GAP-C: Added 3 missing FSM transitions (impl→verify, verify→debug, debug→verify)
- GAP-D: Replaced magic `(* max-count 200)` with `(current-conclusion-token-budget)` parameter

### Tests
- 3 ranking consistency tests (conclusion-graph)
- 3 idle-reset regression tests (ws-evolution)
- 3 FSM transition tests (ws-evolution)
- 1 unified budget parameter test (state-aware-builder)

## [0.97.9] - 2026-06-08

### Fixed
- GAP-D: Replaced recency-based fallback-select-conclusions with semantic rank-and-budget
- GAP-D: Downgraded cycle detection log from warning to info
- Removed unused fallback-select-conclusions import from state-aware-builder

### Added
- 6 new semantic fallback regression tests

## [0.97.8] - 2026-06-08

### Fixed
- GAP-B: Added 4 missing WS transition rules (planning→implementation/verification/debugging, verification→implementation)
- GAP-C: Relaxed planning inference — now allows writes when meta tool count ≥ write count
- GAP-C: Reordered inference rules so planning check runs before implementation

### Added
- 6 new WS transition regression tests
- 4 new state inference regression tests

## [0.97.7] - 2026-06-08

### Fixed
- GAP-A: Preamble now uses budgeted conclusions capped at 20 instead of arbitrary top-10
- GAP-A: Format string shows total+shown counts for transparency

### Changed
- state-aware-builder.rkt: Raised preamble conclusion cap from 10→20 with improved total/shown count display

### Added
- 6 new regression tests for preamble ranking behavior

## [0.97.6] - 2026-06-08

### Fixed
- F3: Extract content-part->text to module-level in turn-context.rkt for reuse and testability
- F4: model-registry-context-window resolves context window from model config (hash and alist formats)
- F5: Startup log for context-window resolution source and auto-budget observability

### Changed
- wiring/run-modes.rkt: resolves max-context-tokens from model registry before falling back to config default
- model-registry.rkt: new model-registry-context-window function handles both JSON hash and providers.rktd alist entries
- turn-context.rkt: content-part->text extracted from local binding to module-level (F11: documents tool-call-part discard)

### Added
- tests/test-content-part-to-text.rkt -- 9 tests for content-part->text extraction (W0)
- tests/test-gap-e4-dynamic-context-tokens.rkt -- 6 tests for model-registry-context-window (W1)
- tests/test-gapf-mid-session-bridge.rkt -- 4 new bridge action tests (W2, F7)
- tests/test-gapd-memory-injection-default.rkt -- 3 new parameter smoke tests (W2, F8)
- tests/test-gapg-enriched-memory-query.rkt -- test-cases moved inside test-suite form (W2, F12)

## [0.97.5] - 2026-06-08

### Added
- GAP-F: Mid-session conclusion bridge on major forward state transitions
  (exploration→planning→implementation→review)
- GAP-F: current-mid-session-bridge-enabled parameter (default #f, wired for
  self-healing/full profiles via settings)
- GAP-G: Enriched memory query text with state name, active tags, and recent conclusions
- GAP-G: observe-memory-for-context and inject-memory-for-context accept #:tags keyword

### Changed
- state-aware-builder.rkt: builds enriched query before memory observation/injection
- memory-builder.rkt: threads #:tags through to memory-query struct

### Added
- tests/test-gapg-enriched-memory-query.rkt -- 4 tests for query enrichment
- tests/test-gapf-mid-session-bridge.rkt -- 6 tests for transition detection
- tests/test-gapfg-memory-pipeline-integration.rkt -- 5 integration tests

## [0.97.4] - 2026-06-08

### Added
- GAP-D: Default memory injection budget (5% of max-context-tokens) for self-healing and full profiles
- GAP-E: Dynamic conclusion budget from actual max-context-tokens via compute-conclusion-budget
- GAP-E: apply-context-assembly-profile! accepts max-context-tokens parameter (default 128000)

### Changed
- wiring/run-modes.rkt: passes actual max-context-tokens to apply-context-assembly-profile!
- turn-orchestrator.rkt: passes config-max-context-tokens to apply-context-assembly-profile!

### Added
- tests/test-gap-dynamic-budget.rkt -- 6 tests for compute-conclusion-budget
- tests/test-gapd-memory-injection-default.rkt -- 5 tests for injection budget logic
- tests/test-gapde-budget-activation-integration.rkt -- 5 integration tests

## [0.97.3] - 2026-06-08

### Fixed
- GAP-A: Distillation for/list zip truncation causing silent data loss when LLM returns fewer lines than uncovered IDs
- GAP-B: Conclusion IDs now use generate-id instead of reusing WS message IDs, ensuring uniqueness
- GAP-C: Content summaries now include tool-result-part content (string/hash/list) with 500-char cap and error filtering

### Added
- tests/test-gap-ab-distillation-zip.rkt -- 3 tests for zip truncation + unique IDs
- tests/test-gapc-tool-result-summaries.rkt -- 5 tests for tool result summary extraction
- tests/test-gap-distillation-integration.rkt -- 4 integration tests for distillation pipeline

## [0.97.2] - 2026-06-08

### Added
- GAP-5: Conclusion-to-memory bridge enabled in self-healing and full profiles
- GAP-5: Expanded high-value-categories to include 'fact' conclusions
- GAP-6: LLM distillation prompt enrichment with content summaries (max 300 chars)
- GAP-6: Forward content-summaries through auto-distill → distill-with-llm → factory lambda
- New test files: test-gap5-conclusion-bridge.rkt (5 tests), test-gap6-llm-prompt-enrichment.rkt (3 tests)

### Changed
- self-healing and full profiles now persist conclusions to memory on session shutdown
- LLM distillation prompt includes actual content instead of just IDs

## [0.97.1] - 2026-06-08

### Added
- GAP-3: Session-scoped embedding cache (max 500 entries, LRU eviction)
- GAP-3: Batch embedding provider for single API call to compute all embeddings
- GAP-3: cached-embed function to avoid N+1 embedding API calls
- GAP-4: Active tag extraction from working-set message metadata for conclusion ranking
- New test files: test-gap3-embedding-cache.rkt (8 tests), test-gap4-active-tags.rkt (3 tests)

### Changed
- rank-by-relevance now uses batch pre-computation instead of per-item embedding calls
- state-aware-builder extracts file-name tags and passes to rank-and-budget

## [0.97.0] - 2026-06-08

### Fixed
- GAP-2: Tool-result auto-extraction now correctly maps tool-call-id → tool-name instead of using message-id (UUID)
- Fixed stray paren in wiring/run-modes.rkt blocking compilation

### Added
- New test file: tests/test-gap2-tool-extraction.rkt (3 tests)

## [0.96.19] - 2026-06-08

### Added
- GAP-2: Semantic memory retrieval — rank-by-relevance uses embedding cosine similarity when provider configured (NEW: runtime/memory/embeddings.rkt)
- GAP-7: LLM-powered reflection — merge-group-items uses injectable LLM synthesis via current-reflection-lln-fn
- GAP-10: Conclusion-to-memory bridge — high-value decisions/patterns persisted as project-scoped memory on session end (NEW: runtime/memory/conclusion-bridge.rkt)

### Changed
- GAP-9: Sensitivity classification now uses regex patterns for API keys, tokens, passwords instead of substring matching

## [0.96.18] - 2026-06-08

### Fixed
- GAP-4: Graph seed resolution now maps WS message IDs to conclusion IDs via origin-message-ids
- GAP-5: WS evolution only fires on actual state transitions (old ≠ new), eliminating redundant computation

### Changed
- GAP-8: Conclusion token budget is now dynamic — 10% of model context window, capped at 4000 tokens, minimum 500 (was hardcoded 2000)

## [0.96.17] - 2026-06-08

### Added
- GAP-1: LLM distillation wiring — channel-based sync with 5s timeout and deterministic fallback
- GAP-3: Tool result auto-extraction for read/grep/find with content filtering
- GAP-6: Default context-assembly profile changed from `off` to `observe`

### Fixed
- distill-with-llm now correctly receives LLM function result via channel instead of thread descriptor

## [0.96.16] - 2026-06-08

### Changed
- **AX1-1**: Decompose `terminal-input.rkt` (671→498 LOC) — extract Kitty keyboard protocol + SGR mouse decoding into `tui/input/kitty-protocol.rkt`
- **AX1-2**: Decompose `tui-render-loop.rkt` (634→422 LOC) — extract frame-vdom + watchdog into `tui/render-loop/` submodules
- **AX1-3**: Decompose `run-tests.rkt` (1388→370 LOC) — extract 6 sub-modules into `scripts/run-tests/`
- **AX1-5**: Convert 13 `struct-out` to explicit provides across 9 files (33→20 remaining)
- **AX3-3**: Reduce test sleeps >0.5s from 12 to 6 (remaining are intentional timeout tests)
- **AX5-x**: Fix stale version references in README, info.rkt, CHANGELOG
- **AX6-3**: Pre-commit quick mode now runs `raco fmt`/`raco make` on staged files only

### Audit Source
- `.planning/PLAN-v0.96.16-AUDIT-REMEDIATION.md`


## [0.96.15] - 2026-06-09

### Fixed
- **HF1**: Centralize `current-loop-warning-count` mutations into `increment-loop-warning-count!` helper (eliminates dual-mutation maintenance hazard)
- **MF1**: Document Trigger 3+4 overlap in `check-rollback-triggers` (intentional dual-fire behavior)
- **MF2**: Replace 4 `dynamic-require` calls in `state-aware-builder.rkt` with static `only-in` imports (eliminates runtime crash class)
- **MF3**: Add integration tests for counter increment helper and escalation wiring (5 new tests)
- **LF3**: Add post-implementation deviation notes to v0.96.13 PLAN
- **LF4**: Remove redundant `only-in` import of settings in `run-modes.rkt`

### Audit Source
- `.planning/AUDIT-v0.96.14-MEMORY-CONTINUITY-HOTFIX.md`


### Memory Continuity — Post-Audit Hotfix

#### Fixed
- **F1**: Added stuck-detection trigger to `check-rollback-triggers` — fires when ≥6 tool calls with 0 conclusion coverage, producing "stuck" warning → `expand-context` action
- **F2**: Wired exploration-loop detection into rollback pipeline — step-interpreter now increments `current-loop-warning-count` when `detect-exploration-loop` fires, enabling escalation on next `check-rollback-triggers` call
- **F3**: Wired reflection event → `current-reflection-event` parameter — step-interpreter sets the parameter directly after emitting `reflection-suggested` event, enabling preamble reminder consumption
- **F4**: Replaced magic number `2` with named `escalation-threshold` constant in rollback-actions

#### Tests
- 6 new test cases for stuck trigger, exploration-loop wiring, and escalation-threshold constant (27 total)

#### Added
- Config wiring: `reflection-prompt-enabled` and `auto-distillation-enabled` configurable from config.json via new settings accessors (`setting-reflection-prompt-enabled?`, `setting-auto-distillation-enabled?`)
- Both flags enabled in `~/.q/config.json`, `.q/config.json`, `q/.q/config.json`

#### Fixed
- Replaced `dynamic-require` with static `require` imports in step-interpreter.rkt — `dynamic-require` with relative paths resolves against CWD, causing runtime crash when q launched from a different directory

## [0.96.13] - 2026-06-09

### Memory Continuity & Looping Prevention

#### Added
- **WP-1**: Context-aware memory retrieval — query text now derived from task state and recent messages, forwarded to memory search relevance scoring
- **WP-4**: Anti-looping escalation — warning counter with escalation to force-distill after 2+ repeated tool warnings; exploration-loop and stuck-detection triggers
- **WP-3**: Forced reflection on large tool results — reflection-suggested event when results >4000 chars, advisory reminder in state-aware preamble
- **WP-2-light**: Transition detection infrastructure — ws-entry->text helper, warning counter reset on task-state transitions
- All new behaviors default OFF via feature flags (current-reflection-prompt-enabled, current-auto-distillation-enabled?)

#### Changed
- observe-memory-for-context and inject-memory-for-context accept #:query-text parameter
- warnings->actions escalates to force-distill after threshold (case-insensitive pattern matching)
- check-rollback-triggers detects exploration loops and stuck states
- build-state-awareness-preamble includes reflection reminder when events exist

## [0.96.12] - 2026-06-09

### Test Architecture — Post-Audit Hotfix

#### Fixed
- **A1**: Removed 3 dead runner submodules (`scripts/run-tests/{types,classify,parse}.rkt`) — 381 LOC of unreachable code with zero consumers and behavioral drift
- **E2**: Added `run-tests` calls to `test-streaming-tool-events.rkt` and `test-tui-idle-cpu.rkt` — both had `rackunit/text-ui` imported but never invoked
- Fixed stale comment in `test-ui-render-hooks.rkt` (10s → 2s reflecting actual sleep value)
- Added missing `@speed @suite` tags to `test-cursor-debug.rkt` (A2 coverage now 100%)

## [0.96.11] - 2026-06-08

### Test Architecture — Phase 4: Runner Refactor

#### Changed
- **A1**: Extracted `scripts/run-tests/` into sub-modules (types, classify, parse) — modular API for external consumers
- **A2**: Created `scripts/lint-test-tags.rkt` and auto-tagged all 905 test files with `@speed` and `@suite` metadata tags
- **B1/B3**: God test file splitting deferred (low risk/benefit ratio for test-only changes)

## [0.96.10] - 2026-06-08

### Test Architecture — Phase 3: Efficiency

#### Changed
- **E1**: Reduced sleep calls in 4 test files (sleep 30→3, sleep 10→2) — saves ~80s per run
- **E2**: Converted 58 files from `(module+ test ...)` to top-level test forms for unified runner detection
- **E3**: Changed GC from every-batch to every-5-batches in `scripts/run-tests.rkt` for reduced overhead

## [0.96.9] - 2026-06-08

### Test Architecture — Phase 2: Naming & Organization

#### Changed
- **D1**: Renamed 18 test files in `tests/tui/` to follow `test-<module>.rkt` convention
- **D1**: Renamed `tests/interfaces/tui.rkt` → `tests/test-interfaces-tui.rkt`, `tests/bench-streaming-render.rkt` → `tests/test-bench-streaming-render.rkt`
- **D1**: Wrapped bare `check-*` calls in `test-case` blocks across 19 test files
- **B2**: Deduplicated all test-case names across 74 files — every name is now globally unique with module prefix
- **D3**: Updated `docs/TEST_CONVENTIONS.md` with naming, temp file, state isolation, and env guard conventions

## [0.96.8] - 2026-06-08

### Test Architecture — Phase 1: Safety & Cleanup

#### Fixed
- **D2**: Deprecated `fixtures.rkt` `with-temp-dir` (function) in favor of canonical macro in `temp-fs.rkt`
- **C1**: Fixed temp file leaks in 15 test files — wrapped all `make-temporary-file` calls in `with-temp-dir` for guaranteed cleanup
- **C2**: Fixed module-level mutable state in 5 test files — converted `set!` counters to `make-parameter` with per-test reset
- **C3**: Fixed firecrawl env test to use `dynamic-wind` for guaranteed env var restore
## v0.96.5 — Phase 1 Quick Wins (2026-06-08)

### Architecture
- **F7 (dependency cycle) — RESOLVED**: Extracted 4 context-assembly parameters into new `runtime/context-assembly/config.rkt`, breaking the `session-config` → `state-aware-builder` → `memory-builder` cycle.
- **F5/F10 (stability markers) — RESOLVED**: Added `;; STABILITY: public` to 27 hub modules (>15 fan-in).
- **F2 (composition roots) — RESOLVED**: Documented 5 composition root modules with `COMPOSITION ROOT` comments.

### Metrics
| Metric | Before (v0.96.4) | After (v0.96.5) | Change |
|--------|------------------|-----------------|--------|
| Dependency cycles | 1 | 0 | -1 |
| Stability markers | 89 | 116+ | +27 |

## v0.96.4 — Audit Hotfix (2026-06-08)

### Architecture
- **BF1 (dead imports) — RESOLVED**: Removed 4 remaining dead imports from `turn-orchestrator.rkt`: `racket/list`, `cancellation-token-cancelled?`, `tool-result?`, `hook-result-payload`.
- **BF2 (stale comment) — RESOLVED**: Fixed layer-exception comment to reference `layer-adapters.rkt` instead of stale `extensions/hooks.rkt`.

### Metrics
| Metric | Before (v0.96.3) | After (v0.96.4) | Change |
|--------|------------------|-----------------|--------|
| turn-orchestrator dead imports | 4 | 0 | -100% |
| turn-orchestrator import count | 25 | 21 | -16% |

## v0.96.3 — Architecture Audit Hotfix (2026-06-08)

### Architecture
- **AF1 (dead imports) — RESOLVED**: Removed 8 dead import entries from `turn-orchestrator.rkt` (33→25 imports).
- **AF2 (misleading comment) — RESOLVED**: Fixed `format-iso-now` comment in `memory-tools-shared.rkt`.
- **AF3 (behavioral tests) — RESOLVED**: Added 3 behavioral test cases for `symbol->task-state` and `hash->session-config`.
- **AF4 (dead imports) — RESOLVED**: Removed `dispatch-hooks` + `hook-result?` from `extension-setup.rkt`.
- **AF5 (stability markers) — RESOLVED**: Added `STABILITY: public` markers to 4 PUBLIC event definitions.
- **AF6 (dead imports) — RESOLVED**: Removed 10 unused shared imports from `memory-tools.rkt` facade.

### Metrics
| Metric | Before (v0.96.2) | After (v0.96.3) | Change |
|--------|------------------|-----------------|--------|
| turn-orchestrator.rkt imports | 33 | 25 | -24% |
| Behavioral tests for extracted fns | 0 | 3 | +3 |
| Event stability markers | 0 | 4 | +4 |

## v0.96.2 — Module Sizing + Low-Priority Cleanups (2026-06-08)

### Architecture
- **F4 (memory-tools god module) — RESOLVED**: Split 928-line `memory-tools.rkt` into 5 focused modules:
  - `memory-tools-shared.rkt` (316 LOC) — shared helpers + event emitters
  - `memory-tools-store.rkt` (213 LOC) — store/update handlers
  - `memory-tools-query.rkt` (89 LOC) — search/list handlers
  - `memory-tools-manage.rkt` (314 LOC) — delete/clear/consolidate/cleanup
  - `memory-tools.rkt` (130 LOC) — thin registration facade (-86% LOC)
- **F6 (stream-from-provider complexity) — DEFERRED**: Single 200-line function with deeply nested state. Extraction assessed as high-risk, low-reward. Existing tests provide safety net.
- **F7 (wire-session-event-handlers complexity) — DEFERRED**: Similar assessment.
- **F8 (build-runtime-from-cli) — BELOW THRESHOLD**: 166 LOC, under 200-line limit. No action needed.
- **F10 (session config accessors) — ALREADY ADEQUATE**: 24 hash-ref accessors documented and consistent.
- **F11 (format-iso-now duplicate) — DOCUMENTED**: Thin wrapper retained for compatibility.
- **File headers**: Added to 7 files missing them (json-mode, rpc-mode, grep, firecrawl, scrollback, tui-init, frame-diff).

### Metrics
| Metric | Before (v0.96.1) | After (v0.96.2) | Change |
|--------|------------------|-----------------|--------|
| memory-tools.rkt LOC | 928 | 130 | -86% |
| Files without headers | 7 | 0 | -7 |
| God-modules (>500 LOC) | 14 | 13 | -1 |

## v0.96.1 — Representation Hiding + Parameter Reduction (2026-06-08)

### Architecture
- **F2 (Event struct-out over-exposure) — DOCUMENTED**: Classified 60 event types as PUBLIC (4) vs INTERNAL (56). PUBLIC events marked with stability commitment. Assessment: `struct-out` for `#:transparent` immutable structs is idiomatic Racket; explicit provides would be more verbose with no security benefit.
- **F3 (Parameter proliferation) — RESOLVED**: Audited all 129 production parameters. Dead `circuit-breaker-state` removed. Already at ≤130 target. Classification: 42 function-arg candidates, 27 groupable, 36 keep-as-is.

### Metrics
| Metric | Before (v0.96.0) | After (v0.96.1) | Change |
|--------|------------------|-----------------|--------|
| Production parameters | 130 | 129 | -1 |
| Event types classified | 0 | 60 | +60 |
| Dead parameters | 1 | 0 | -1 |

## v0.96.0 — Turn Orchestrator Decomposition (2026-06-08)

### Architecture
- **F1 (God Module) — RESOLVED**: `turn-orchestrator.rkt` reduced from 534 LOC / 42 imports to **286 LOC / 24 imports** (47% LOC reduction, 43% import reduction).
- **F5 (166-line Function) — RESOLVED**: `build-assembled-context` decomposed into 4 clear phases via named helpers.
- Extracted `runtime/context-assembly/turn-context.rkt` (279 LOC): `symbol->task-state`, `assemble-context/pure`, `prepare-turn-context-state`, `emit-context-assembly-events!`, `current-last-task-fsm-state`.
- Extracted `runtime/extension-setup.rkt` (51 LOC): `register-session-extensions!`.
- 18 unused imports cleaned up from turn-orchestrator.rkt.

### Metrics
| Metric | Before (v0.95.21) | After (v0.96.0) | Change |
|--------|-------------------|-----------------|--------|
| turn-orchestrator.rkt LOC | 534 | 286 | -46% |
| turn-orchestrator.rkt imports | 42 | 24 | -43% |
| build-assembled-context LOC | ~169 | ~42 (coordinator) | -75% |
| New modules | 0 | 2 | +2 |

## v0.95.21 — Memory Lifecycle Completion (2026-06-08)

### New Features
- **G1 — Background Reflection Trigger**: Auto-reflection now fires per-turn after auto-extraction when `memory.auto-reflection.enabled` is set to `true`. Non-fatal wrapper `maybe-reflect-session-memories!` catches all exceptions. Defaults to disabled.
- **G2 — User-Scope Config Wiring**: User-scope memory can be enabled via `memory.user-scope.enabled` config key. Policy updated at session startup from config.

### Configuration Keys
- `(memory auto-reflection enabled)` — boolean, default `false`
- `(memory auto-reflection min-items)` — integer >= 2, default `5`
- `(memory user-scope enabled)` — boolean, default `false`

### Verification
- 18 lifecycle tests, 18 policy tests passing
- All new features default off — no behavioral change for existing users

## v0.95.20 — Memory Tools Event Publisher Fix (2026-06-08)

### Bug Fixes
- **Event publisher arity mismatch**: `publish-memory-event!` in `memory-tools.rkt` called the event publisher with 1 arg (combined hash) but `tool-coordinator.rkt` provides a `(event-type payload)` 2-arg lambda. All memory tool events failed with arity mismatch, leaking errors into the TUI prompt area.

### Verification
- 493 focused memory tests passing
- 1 new regression test verifying 2-arg publisher contract
- 5 test publisher lambdas updated from 1-arg to 2-arg

## v0.95.19 — Memory Injection Hotfix (2026-06-07)

### Bug Fixes
- **HF1 (HIGH)**: Thread `#:session-config config` to `build-tiered-context/state-aware` in `turn-orchestrator.rkt` — enables memory injection in production context assembly path
- **HF2 (HIGH)**: Change default `#:scope` from `'session` to `#f` in `observe-memory-for-context`, `inject-memory-for-context`, and `observe-memory-telemetry` — project and user-scoped memories now visible during retrieval
- **LF1 (Low)**: Check `gen:store-memory!` result in `reflect-session-memories!` — failed stores no longer produce phantom reflection items
- **LF2 (Low)**: Thread `session-id`/`project-root` from payload into `decode-mem0-items` — Mem0 external backend uses actual query context instead of hardcoded values

### Verification
- 174+ focused memory tests passing across 10+ test files
- 4 new regression tests (3 previously failing, now green)

## v0.95.18 — Memory Quality Remediation (2026-06-07)

### Regression Harness
- **W0**: Red/regression harness and validation baseline — 9 test files mapping F1-F9 audit findings (4 pass, 5 expected-fail)

### Transport & Extraction Fixes
- **W1**: Mem0 fail-closed retrieval and transport hardening — propagate transport errors before Mem0-specific decode; auth header fix (`Api-key` not `Token`); error sanitization (10 tests)
- **W2**: Blank `[Auto]` auto-distillation and state preamble filter — `make-deterministic-fallback` treats blank/whitespace content as absent; `build-state-awareness-preamble` filters bare `[Auto]` conclusions (14+21 tests)
- **W3**: Validation gate repair and behavioral extraction evidence — structural invariant proving `run-agent-turn` has no LLM-response path bypassing auto-extraction (7 tests)

### Consolidation & Reflection
- **W4**: Consolidation lineage and policy hardening — target-type validation; supersession metadata (`superseded-by`) replaces silent deletion; partial failure reporting (80 tests)
- **W5**: Reflection determinism and conservative grouping — min-group-size raised to 3; shared-tags requires >=2 tags; Jaccard threshold raised to 0.4; fully deterministic IDs (45 tests)

### Release Gate
- **W6**: Version bump, CHANGELOG, docs update, focused memory gate validation

### New Files
- `tests/test-memory-regression.rkt` — F1-F9 regression harness
- `tests/test-memory-consolidation-tool-g3.rkt` — consolidation edge-case tests

## v0.95.17 — Memory Quality (2026-06-06)

### Characterization & Bug Fixes
- **W0**: Characterization tests for remaining gaps — tool-call turn extraction, non-streaming hook, Mem0 transport, consolidation tool (4 test files)
- **W1**: Fix tool-call turn extraction — `maybe-auto-extract-after-response!` now fires for all response types, not just text-only turns
- **W2**: Verified non-streaming extraction path — `run-agent-turn` always delegates to streaming path

### Features
- **W3**: Mem0 HTTP transport — real `http-sendrecv` transport replacing `not-implemented` stub, sanitized error messages (7 tests)
- **W4**: `consolidate-memory` tool — merge multiple items into one with supersedes lineage, optional original deletion (6 tests)
- **W5**: Deterministic memory reflection — group session items by shared tags/Jaccard overlap, produce project-scope reflections (16 tests)

### Integration & Release
- **W6**: Documentation updates, changelog, version bump, focused memory test validation

### New Files
- `runtime/memory/reflection.rkt` — deterministic session-to-project reflection
- `tests/test-memory-tool-turn-extraction-g1.rkt`, `tests/test-memory-nonstreaming-extraction-g1.rkt`
- `tests/test-mem0-http-transport-g2.rkt`, `tests/test-memory-consolidation-tool-g3.rkt`
- `tests/test-memory-reflection-w5.rkt`

## v0.95.12 — Modular Memory System (2026-06-03)

### Memory System Foundation
- **W1**: Protocol and types foundation — `memory-item`, `memory-query`, `memory-result`, `memory-backend` structs with validators (14 tests)
- **W2**: Deterministic in-memory hash backend — `memory-hash-backend` with store/retrieve/update/delete/list (24 tests)
- **W3**: Disabled-by-default config wiring — `config-memory-backend`, `config-memory-enabled?` (9 tests)

### Memory Tools and Events
- **W4**: Explicit memory tools with policy gating — `store_memory`, `search_memory`, `delete_memory` with sensitivity/scope checks (22 tests)
- **W5**: Memory events — `memory.stored`, `memory.retrieved`, `memory.deleted` typed events (13 tests)
- **W9**: Management UX tools — `list_memory` (inspect), `clear_memory` (scoped batch delete with confirmation) (14 tests)

### Context Integration
- **W6**: Observe-only memory context retrieval — `observe-memory-for-context` gathers memory without injection (18 tests)
- **W7**: Bounded memory prompt injection — `inject-memory-for-context` with budget/framing/safety (18 tests)

### Persistence and Backends
- **W8**: Project-local JSONL memory backend — append-only file backend with snapshot loading (14 tests)
- **W10**: Auto-extraction alpha — optional post-response fact extraction, disabled by default, secret pattern blocking (17 tests)
- **W11**: Chained L1/L2 backend with write-through and dedup; external backend adapter skeleton with redaction (26 tests)

### Release Hardening
- **W12**: Release gate tests — validates disabled-by-default, no hidden writes, safe tool responses (14 tests)
- All 14 memory test suites passing (196 total tests)
- Memory is **disabled by default** — no persistent writes unless explicitly configured

### New Files
- `runtime/memory/types.rkt`, `runtime/memory/protocol.rkt`, `runtime/memory/policy.rkt`
- `runtime/memory/backends/memory-hash.rkt`, `runtime/memory/backends/file-jsonl.rkt`
- `runtime/memory/backends/chained.rkt`, `runtime/memory/backends/external-protocol.rkt`
- `runtime/memory/auto-extraction.rkt`
- `runtime/context-assembly/memory-builder.rkt`
- `tools/builtins/memory-tools.rkt`
- `agent/event-structs/memory-events.rkt`
- 14 test files in `tests/test-memory-*.rkt`


## v0.95.16 — Memory Gap Closure

### Auto-Extraction Integration
- **W1**: Config/session-config accessors for auto-extraction
- **W2**: Runtime parameter wiring in build-runtime-from-cli
- **W3**: Post-turn lifecycle hook maybe-auto-extract-after-response! in loop-stream.rkt

### Backend Configuration
- **W4**: Recursive backend factory — chained backends from json config specs
- **W5**: Mem0 external adapter with injectable transport

### Memory Management
- **W6**: Deterministic management.rkt module with dedup, expiry, dry-run

### Context Injection Polish
- **W7**: Tiered injection with type/scope grouping and untrusted header

### Release
- **W8**: Documentation, focused test gate, milestone closure
- 24 files changed, +1860/-78 lines, 79 new tests

## v0.94.9 — GUI-TUI Output Parity (2026-06-03)

### GUI Enhancements
- **W1**: Typed GUI messages — `kind` field on `gui-message` (message, tool-start, tool-end, tool-fail, thinking, system, error)
- **W1**: Extended `gui-state` with `context-info`, `cost`, `model` fields
- **W2**: Tool argument + result display with separate entries and per-kind coloring
- **W3**: Event coverage — thinking, retry, compaction, exploration, goal, session events
- **W4**: Rich status bar — context %, cost, active goal in GUI status
- **W5**: Markdown rendering — headers, lists, numbered lists, inline code parsing

### Tests
- test-gui-typed-messages.rkt: 15 tests
- test-gui-tool-display.rkt: 11 tests
- test-gui-event-coverage.rkt: 18 tests
- test-gui-status-bar.rkt: 8 tests
- test-gui-markdown-render.rkt: 11 tests

### PRs
- #7109 (W1), #7110 (W2), #7111 (W3), #7112 (W4), #7113 (W5)

## v0.93.2 — Browser Feature Audit Closure (2026-06-03)

### Security Fixes (Critical)
- **F1**: All 10 browser tools blocked in safe-mode
- **F2**: Absolute scheme deny-list (file://, data://, javascript://)
- **F3**: IPv6 address classification (blocks loopback, link-local, ULA, mapped)

### Architecture Fixes (High)
- **F4**: Service layer dispatches through adapter interface
- **F5**: load-browser-settings reads from config
- **F6**: Explicit browser tool risk tiers in permission gate
- **F7**: browser-service field on exec-context
- **F8**: Browser lifecycle wired into agent-session

### Completeness Fixes (Medium)
- **F9**: emit-browser-event! publishes real events to bus
- **F10**: Browser events re-exported from event-structs facade
- **F11**: Heartbeat thread in Playwright adapter (30s ping)
- **F17**: Removed unused imports from audit.rkt

### Quality Fixes (Low)
- **F15**: Removed dead viewport schema entries from browser_open
- **F16**: Added browser-url-blocked-error? and browser-adapter-error? predicates

### Documentation
- **F13**: Updated overview.md to v0.93.2 with browser/ layer
- **F14**: Updated README to 27 tools (17 core + 10 browser)

### Tests
- 49 new tests across 6 new test files + 8 existing files updated
- Total browser tests: 282+

## v0.93.1 (2026-06-03)

### Final Audit Closure (Phase 7)

- Post-implementation audit: APPROVED — 0 blocking findings
- Architecture: layer boundaries clean (browser/ imports only agent/event + util)
- Security: default-deny URL policy, dangerous tool classification, safe mode integration
- Tests: 240 browser tests across 14 test files
- Documentation: browser-guide.md + ADR 0020
- **Browser Feature Complete**: 11 source modules, 10 registered tools, Playwright sidecar

## v0.92.2 (2026-06-03)

### Workflow Tool + Documentation (Phase 6)

- Add `browser/workflow.rkt`: `browser_check_local_app` composite tool
  - Open URL → observe → screenshot → close → return composite result
  - Returns: url, title, text, console_errors, loaded_successfully, load_time_ms, screenshot
- Register `browser_check_local_app` as MEDIUM risk in tool registry
- Add `docs/browser-guide.md`: feature overview, architecture, tool reference, security model
- Add `docs/adr/0020-browser-feature-architecture.md`: sidecar rationale, JSONL protocol, adapter contract
- 15 workflow + integration tests
- **240 total browser tests**

## v0.91.3 (2026-06-03)

### Playwright Sidecar + Real Adapter (Phase 5)

- Add `sidecars/playwright/`: Node.js/Playwright sidecar with JSONL stdin/stdout protocol
  - 9 command handlers: navigate, click, type, press, scroll, extract, screenshot, ping, close
  - Page state extraction, screenshot capture with size limits, health check ping/pong
  - Error mapping: Playwright errors → q-browser-error categories (timeout, adapter-error, sidecar-crash, policy-violation)
  - Node.js unit tests for command validation
- Add `browser/adapters/playwright-sidecar.rkt`: Racket adapter for sidecar process lifecycle
  - JSONL IPC with UUID-based request/response and timeout
  - Zombie prevention via custodian management
  - Full browser-adapter interface implementation
- 4 new Racket unit tests + 3 integration tests (skipped without Node.js)
- **225 total browser tests**

## v0.90.2 (2026-06-03)

### Browser Tool Registry Integration (Phase 4)

- Add `tools/builtins/browser-tools.rkt`: 9 tool handlers (browser_open, browser_observe, browser_click, browser_type, browser_press, browser_extract, browser_screenshot, browser_scroll, browser_close)
- Register all 9 browser tools in `tools/registry-table.rkt` with JSON schemas and prompt guidelines
- Risk classification: click/type/press = HIGH risk (requires approval), open = MEDIUM, observe/extract/screenshot/scroll/close = LOW
- 16 new tool handler tests
- **217 total browser tests**

## v0.89.3 (2026-06-03)

### Browser Secure Service + Audit + Mock Adapter (Phase 3)

- Add `browser/adapters/mock.rkt`: deterministic mock adapter with configurable responses, call recording, error simulation
- Add `browser/audit.rkt`: JSONL audit trail for all browser actions
- Add `browser/service.rkt`: `SecureBrowserService` composing policy+session+adapter+audit with open/observe/act/navigate/screenshot/close
- Add `current-browser-service` parameter for runtime integration
- 48 new tests: 16 mock adapter, 6 audit, 26 service
- **185 total browser tests**

## v0.88.3 (2026-06-03)

### Browser Policy Engine + Session Manager (Phase 2)

- Add `browser/policy.rkt`: default-deny URL policy engine with scheme validation, IP classification (RFC 1918, cloud metadata 169.254.169.254, loopback, link-local), domain allowlist, path blocking, action risk classification
- Add `browser/settings.rkt`: 13-field `browser-settings` struct with defaults, `current-browser-settings` parameter
- Add `browser/session.rkt`: session lifecycle manager with create/destroy/get/list, max-sessions (3), max-actions-per-session (100) enforcement
- 74 new tests: 43 policy, 17 settings, 14 session

## v0.87.3 (2026-06-03)

### Browser Domain Model (Phase 1)

- Add `browser/` directory with domain types: `browser-target`, 8 `browser-action` variants, `browser-observation`, `browser-session-info` with JSON roundtrip codecs
- Add `browser/adapter.rkt` with adapter interface (open/close/navigate/observe/act/screenshot) + contract-out
- Add `browser/events.rkt` with 10 typed browser events using `define-typed-event` macro
- Add `browser` layer to `docs/architecture/dependency-policy.rktd` (forbidden from runtime/tui/extensions)
- Add `q-browser-error` branch to error hierarchy with 7 category predicates
- Register 10 `browser.*` event types in `agent/event-json.rkt`
- 63 new tests: 19 types, 13 errors, 11 events, 10 adapter, 10 contracts

## v0.86.5 (2026-06-04)

### Audit Closure (Complete Remediation)

- Fix `turn.completed` → `stream.turn.completed` in 5 test files (F1)
- Fix `enqueue-steering!` to accept string instead of message struct (F4)
- Widen `branch-summary-entry-entry-range` contract in tree-entries.rkt (F3)
- Fix `cli-config` arity 16→17 for context-profile field (F2)
- Update version markers from 0.86.3→0.86.4 across 653 references (F5)
- Add `turn-orchestrator.rkt` risk note to dependency-policy.rktd (F5)
- Fix `record_conclusion` tool name in permission-gate.rkt (F5)
- Fix stale file paths in test-arch-fitness.rkt (F5)
- Fix truncate-string ellipsis mismatch in tui/state test (F6)
- Widen `styled-line?` placeholder to accept struct instances (F9)
- Fix runner `file-has-rackunit-tests?` for self-running rackunit/text-ui files (F10)
- Add `assistant.message.completed` + `stream.turn.completed` to event registry (F11)

## v0.86.4 (2026-06-04)

### Audit Closure

Post-implementation audit closure for v0.86.xx Test Suite Remediation series.

**Test fixes (19 files):**
- Fixed stale module paths in test-skeleton.rkt (auth-store→auth/auth-store, jsonl→json/jsonl)
- Fixed hardcoded version in test-util-reclassification.rkt (0.80.4→0.86.3)
- Updated facade test for explicit provides in event-structs.rkt (test-facade-surface.rkt)
- Fixed stale require path in test-goal-state.rkt (util/message→util/message/message)
- Exported internal accessors for test-gsd-production-session-isolation.rkt
- Updated 5 moved module paths in test-arch-fitness.rkt
- Added risk notes for llm/stream.rkt and tui/commands.rkt in dependency-policy.rktd
- Fixed tool classification for save-conclusion/record-conclusion/set-task-state (permission-gate.rkt)
- Updated 11 docs from version 0.83.9→0.86.3 for lint-doc-freshness
- Widened make-branch-summary-entry contract for opaque entry-range (tree-entries.rkt)
- Fixed classify-error test assertions (test-cli.rkt)
- Fixed loop-state and event-bus args in test-effect-update-fsm-contract.rkt
- Fixed deep-merge-hash test to expect contract violation (test-settings-config-validation.rkt)
- Widened directive-recurse contracts to any/c (directive.rkt)
- Fixed gui-state struct usage in test-gui-subscriber.rkt
- Removed #:no-serialize event from all-known-event-types (event-json.rkt)
- Updated turn.completed→stream.turn.completed in test-iteration-event-golden.rkt
- Fixed enqueue-steering! string contract in test-iteration-steering.rkt

**Source improvements:**
- Added TODO tracking for any/c widenings in turn-model.rkt
- Added doc comments for internal settings accessors
- Updated dependency-policy.rktd with moved paths and new risk notes

## v0.86.3 (2026-06-03)

### Performance & Parallel Safety

**W0 (#6873): Parallel-safety fixes**
- Replaced fixed /tmp/test* paths with unique paths using (random 1000000)
- 6 files fixed: test-iteration, test-iteration-integration, test-iteration-working-set, test-iteration-transitions, test-tool-coordinator-duration, test-arch-01-regression

## v0.86.2 (2026-06-03)

### Mock Updates & Logic Fixes

**W0+W1 (#6870, #6871):**
- test-goal-checks: pipe-to-bash command for safety validation
- test-registry-defaults: tool list updated 14→17
- test-firecrawl: truncate-string format fix
- test-pipeline-smoke: accept stream.turn.completed

## v0.86.1 (2026-06-03)

### Contract & Structural Drift Remediation

**W0 (#6864): cli-config arity fix (9 calls)**
- Updated cli-config calls from 16→17 args (context-profile field) in 2 test files

**W1+W2 (#6865, #6866): Contract drift and event rename fixes**
- turn-model.rkt: widen payload contracts from hash? to any/c
- http-helpers.rkt: widen translate-stop-reason contract to any/c
- context-policy.rkt: fix estimate-message-tokens-cached string estimator
- retry-policy.rkt: relax dict-ref TR boundary to avoid opaque-value error
- tool-coordinator.rkt: guard provider string check before make-minimal-settings
- test-golden-flows.rkt: accept stream.turn.completed alongside turn.completed
- test-iteration.rkt: fix enqueue-steering! to pass strings not messages

**W3+W4 (#6867, #6868): Structural and stale test fixes**
- Fixed 10 test files: preamble changes, struct arity, queue API, event types
- Updated architecture tests: file paths, command list, thresholds

## v0.86.0 (2026-06-03)

### Test Foundation: Module Paths, Exports, False Positives

**W0 (#6859): Module path fixes (12 files)**
- Updated all stale require paths from v0.83-0.85 restructuring
- util/protocol-types.rkt → util/message/protocol-types.rkt (6 files)
- runtime/session-config.rkt → runtime/session/session-config.rkt
- runtime/iteration/ → agent/iteration/ (2 files)
- runtime/context-assembly.rkt → runtime/context/context-assembly.rkt
- util/event.rkt → util/event/event.rkt

**W1 (#6860): Duplicate identifier fixes (3 files)**
- Fixed tool? import conflicts in test-tool-coordinator-phases, test-tool-result-unique
- Fixed truncate-string conflict in tui/builtins.rkt

**W2 (#6861): Missing export fixes (2 source files)**
- Exported gsd-session-ctx-state-box, gsd-session-ctx-sem from session-state.rkt
- Exported q-settings-global, q-settings-project from settings.rkt

**W3 (#6862): False positive investigation (5 files)**
- Confirmed: all 5 files pass individually or hang under raco test (no failure)

## v0.85.3 (2026-06-03)

### God Struct Decomposition

**W0 (#6837): Create lifecycle-state struct**
- Create runtime/session/lifecycle-state.rkt with 9 mutable lifecycle fields
- Add lifecycle field to agent-session struct
- Backward compatible: all existing callers unaffected

**W1 (#6838): Migrate callers, remove old fields**
- Remove 9 lifecycle fields from agent-session (25→16 fields)
- Define compatibility accessors that delegate to lifecycle-state
- agent-session-compacting?, -shutdown-requested?, etc. still work
- Update all test helpers for new 16-field struct layout

**Acceptance:**
- `raco make q/main.rkt` — clean compile
- 90+ session/lifecycle/compaction tests pass
- `agent-session` struct now 16 fields (was 24)

## v0.85.2 (2026-06-03)

### Layer Dependency Cleanup

**W0 (#6833): Create util/types/ forwarding (A1-02)**
- Create util/types/session-config.rkt, session-types.rkt, working-set.rkt
- Update agent/iteration/loop-state.rkt to import from util/types/ instead of runtime/
- Agent layer now has ZERO runtime/ imports

**W1 (#6834): Break wiring↔TUI coupling (A1-03)**
- Create runtime/gsd-query.rkt with current-gsd-mode-query parameter
- Create runtime/command-registry-bridge.rkt re-exporting 5 TUI functions
- Wiring layer now has ZERO tui/ imports

**W2 (#6835): Tighten wiring contracts (C2-03)**
- build-runtime-from-cli: any/c → cli-config? → session-config?
- mode-for-config: any/c → cli-config? → symbol?
- reload-config!: any/c → session-config? → (values session-config? model-registry?)
- Add 7 contract tests for wiring/run-modes

## v0.85.1 (2026-06-03)

### Typed Racket Migration & Facade Assessment

**W0 (#6829): Preflight**
- Audited all 20 #lang typed/racket modules
- Documented 40+ consumers of llm/model.rkt

**W1 (#6830): Revert llm/model.rkt from TR to racket/base (C2-07)**
- Replaced #lang typed/racket with #lang racket/base + explicit contracts
- Removed all TR type annotations (fields were all Any — zero compile-time safety)
- Added contract-out for all constructors and conversion functions
- 9 model tests, 75 conformance tests, 7 provider smoke tests pass

**W2 (#6831): Facade Assessment (C2-08)**
- Assessed tools/tool.rkt facade — KEEP as stable public API
- Verified: zero consumers bypass facade to import sub-modules directly

## v0.85.0 (2026-06-03)

### Test Quality & Quick Wins

**W0 (#6825): Security Test Gaps (T3-01, T3-08)**
- Fix weak path-traversal assertion in test-sandbox-security.rkt
- Add find root guard tests for /run, /snap, symlink-to-/proc
- Add grep missing-path argument error test
- Add ls edge-case tests (broken symlink, invalid sort-by, unreadable dir)

**W1 (#6826): Test Quality Improvements (T3-02, T3-05)**
- Add injectable #:clock parameter to event-bus (default: current-seconds)
- Replace sleep(1.1) with injectable fake clock in event-bus tests
- Enhance provider smoke tests with provider? and export checks

**W2 (#6827): Provide Hygiene (A1-07, A1-08)**
- Replace all-from-out in extensions/gsd/core.rkt with explicit list
- Replace all-defined-out in extensions/gsd/event-structs.rkt with explicit list

## v0.84.4 (2026-06-03)

### Fixed

- **P0**: `find` tool could hang indefinitely when scanning large directory trees — now early-terminates recursion once `max-results` matches are found (BUG-1, fixes `/goal` hang at 93% CPU)
- **P1**: `find` tool now rejects scans from filesystem root (`/`) and system directories (`/proc`, `/sys`, `/dev`, `/run`, `/snap`) — returns clear error instead of walking entire filesystem (BUG-2)
- **P1**: `find` tool now has a hard scan budget of 50,000 entries — prevents unbounded traversal regardless of other parameters (BUG-3)
- **P2**: Tool execution CWD now falls back to `(current-directory)` instead of session directory when no project-dir is configured (BUG-4, `tool-coordinator.rkt`)
- `find` result metadata now includes `scanned` count for observability

## v0.84.3 (2026-06-03)

### Documentation Reconciliation

**W0 (#6805):**
- README accuracy: tool count 14 → 17, add missing subcommands
- Fix subcommands table (q sessions info/delete/trace)
- Trim Status section to 10 entries

**W1 (#6806):**
- Archive pre-v0.70 CHANGELOG entries to CHANGELOG-ARCHIVE.md
- Main CHANGELOG.md ≤ 1500 lines

**W2 (#6807):**
- Add .github/workflows/nightly.yml
- Add docs/getting-started/dev-setup.md
**W0 (#6802):**
- Replace 16 silent `(with-handlers ([exn:fail? void]) ...)` with logging
- Persistence/state errors → `log-warning` (session-lifecycle, session-persistence, lockfile)
- Cleanup/teardown errors → `log-debug` (terminal, clipboard, image-pipeline, streaming-cursor, gsd)
- OAuth errors → `log-warning` with redacted details (oauth, oauth-callback)

**W1 (#6803):**
- spawn-subagent.rkt: per-minute spawn rate limit (30/min)
- safe-mode-state.rkt: `allowed-path?` has `simplify-path` fallback when `resolve-path` fails
- write.rkt + edit.rkt: `log-warning` on path canonicalization failure

## v0.84.1 (2026-06-03)

### Contract Tightening

**W0 (#6799):**
- `build-stream-result` in `agent/loop-stream.rkt`: all params tightened (hash? event-bus? string? loop-state? tool? provider? loop-result?)
- `run-streaming-phase` in `agent/loop-dispatch.rkt`: req model-request?, tools (listof tool?), cancellation-token?, return loop-result?
- `make-tui-ctx` in `tui/context.rkt`: model-registry?, extension-registry?, queue?, box? for optional args

**W1 (#6800):**
- `turn-model.rkt`: extension-registry (or/c extension-registry? #f), payload hash?, result hash?
- `state-types.rkt`: cache key exact-integer?, busy-since (or/c exact-integer? #f)
- `context-files.rkt`: path params use path-string?
- `queue.rkt`: define queue-element? as string?, tighten enqueue/dequeue contracts
- 20 contract tests in `test-contract-tightening.rkt` (all pass)

## v0.84.0 (2026-06-03)

### Facade Purge & Version Sync

- **Remove 87 deprecated facades** (49 runtime + 38 util)
  - All runtime/*.rkt facades deleted; imports updated to canonical sub-package paths (runtime/auth/, runtime/compaction/, runtime/context/, runtime/goal/, runtime/session/, runtime/provider/, etc.)
  - All util/*.rkt facades deleted; imports updated to canonical sub-package paths (util/content/, util/error/, util/event/, util/export/, util/json/, util/message/, util/safe-mode/, util/tool/, etc.)
  - 620 files changed, 1048 insertions(+), 1399 deletions(-)
  - Zero deprecated facades remain in q/

## v0.83.12 (2026-06-03)

### TUI Status Bar Visual Gap

**W0 (#6784):**
- TUI status bar goal-state coverage: `test-tui-goal-status-bar.rkt` with 7 tests
- `handle-stream-turn-completed` preserves active-goal visual state (re-sets busy and status-message)
- `handle-context-built` accepts both `'tokenCount` and `'token-count` payload keys
- `handle-model-stream-completed` tolerates missing/`#f` usage (hash? guard, positive? check)
- `handle-goal-turn-started` sets `busy?` to `#t` and `busy-since` to event timestamp
- `handle-goal-status` sets `ui-state-status-message` in addition to transcript entry

**W1 (#6785):**
- Steering normalization: raw steering strings become `message?` values (role `'user`, meta source `'steering`) before context append
- Bash warning suppression: benign command substitution (e.g., `name=$(basename "$f")`) no longer prints destructive warning or `[CLASSIFIER-DIAG]`
- Regex-only risky commands (e.g., `source /tmp/...`) still warn/block as before

**F8 (post-milestone):**
- `inject-system-instructions` always prepends a system message, even when system instructions are empty
- Empty instructions → `""` content, accepted by OpenAI-compatible providers
- Prevents provider 500 "System message must be at the beginning" when first message was user role
- Consecutive system messages already merged by `merge-consecutive-roles` in `build-raw-messages`

## v0.83.11 (2026-06-03)

### Audit Closure Hotfix

- Fix BLOCKER: `context-event` called with keyword args in `session-lifecycle.rkt` → use `make-context-event`
  - Runtime crash on every prompt in TUI mode; status bar now correctly shows `ctx:NK`
- Fix stale test: `spawn-subagents rejects maxParallel > 3` → `clamps maxParallel > 3`
  - Test now expects success since v0.83.10 T15 changed behavior to clamp

## v0.83.10 (2026-06-03)

### Audit Closure — Runtime Fixes, Workflow Contracts, Metadata Tags

**W0 (#6775):**
- F1: Unique failure log names via `equal-hash-code` on full test path
- F2: `@boundary`, `@isolation`, `@timeout` metadata tags recognized by runner
- F3: `scenario-eof` factory for empty-stream provider testing
- F4: `make-goal-provider-per-turn-cap` for goal loop turn-limit testing

**W1 (#6776):**
- T5: `test-goal-workflow-contract.rkt` — 8 goal lifecycle workflow tests
- T7: Emit `context.built` event so TUI status bar shows token count
- T9: `stream.turn.completed` handler clears busy state in TUI
- T10: `stream_options.include_usage` in OpenAI-compatible streaming requests
- T12: Preserve `session-index` in config when setting working-set
- T13: Use `project-dir` for tool working-directory instead of log path
- T14: Wire session queue into `make-loop-config` + use `enqueue-steering!` in submit-handler
- T15: Clamp `maxParallel` instead of rejecting in `spawn-subagent`

**W2 (#6777):**
- T6: Document 6 metadata tags in `TEST_CONVENTIONS.md` with reference table
- T16: Version bump to v0.83.10

## v0.83.9 (2026-06-03)

### Series Closure, Metrics, and Release Readiness
- **Metrics report**: Before/after metrics for v0.83.xx series (10 milestones, 23 PRs, 176+ tests).
- **Test conventions doc**: `docs/TEST_CONVENTIONS.md` with suite guide, metadata tags, sandbox, scenario harnesses, gate evidence.
- **Series complete**: All 10 milestones (v0.83.0–v0.83.9) delivered.

## v0.83.8 (2026-06-03)

### Process Cleanup, Output Bounds, and Flake Burn-In
- **Unique failure log names**: `make-unique-log-name` uses path hash to avoid basename collisions.
- **Output truncation**: `truncate-test-output` caps test output at 64KB with head+tail+marker.
- **Timeout cleanup tests**: `test-run-tests-timeout-cleanup.rkt` with 7 tests.

## v0.83.7 (2026-06-03)

### Workflow Contract Suite
- **Command surface contracts**: `tests/workflows/test-command-surface-contract.rkt` with 12 tests covering cmd-ctx construction, command parsing, state transitions.
- **Provider/tool workflow contracts**: `tests/workflows/test-provider-tool-workflow-contract.rkt` with 7 tests covering single tool roundtrip, blocked tool, multi-tool ordering, result/response structure, error scenarios.

## v0.83.6 (2026-06-03)

### Goal and TUI Scenario Harnesses
- **Goal scenario harness**: `tests/helpers/goal-scenarios.rkt` with event/status capture, fake run-prompt factories, goal provider factories (no-progress, tool-timeout), shutdown check helpers.
- **Goal loop scenario tests**: `test-goal-loop-scenarios.rkt` with 15 tests covering capture, fake run-prompt, goal-run! integration (shutdown, max-turns cap, event emission, status messages).
- **TUI event pipeline**: `tests/helpers/tui-scenarios.rkt` with synchronized event capture (semaphore-based), channel pairs, frame snapshot.
- **TUI concurrency tests**: `test-tui-event-pipeline-concurrency.rkt` with 8 tests (thread-safe concurrent put!, concurrent put+read, frame snapshot).
- **TUI frame integrity**: `test-tui-frame-integrity.rkt` with 6 tests (row independence, dirty flag determinism, concurrent snapshot creation).

## v0.83.5 (2026-06-03)

### Provider and Tool-Turn Scenario Harnesses
- **Provider scenario DSL**: `tests/helpers/provider-scenarios.rkt` with `scenario-text`, `scenario-tool-call`, `scenario-multi-tool`, `scenario-streaming`, `scenario-error`, `scenario-rate-limit`, `scenario-finish-length`, `make-scenario-provider` with request capture.
- **Tool-turn scenario harness**: `tests/helpers/tool-turn-scenarios.rkt` with `turn-scenario-text/tool-call/multi-tool/blocked-tool/malformed-tool`, mock tool/registry factories, assertion helpers.
- **Shell tokenizer progress tests**: `test-shell-tokenizer-progress.rkt` with 6 tests covering 45+ special character payloads, termination, nested bracket handling, mismatched quote resilience.
- **Tests**: 16 provider scenario tests, 15 tool-turn scenario tests, 6 tokenizer tests.

## v0.83.4 (2026-06-03)

### Suite Taxonomy Migration and No-New-Drift Lints
- **Metadata-backed classification**: `slow-file?`, `tui-file?`, `mutating-file?` now check `@speed`/`@suite`/`@mutates` metadata tags before falling back to heuristics.
- **Metadata cache**: `get-file-metadata` in `scripts/run-tests.rkt` parses file headers once and caches.
- **Test architecture lint**: new checks in `scripts/lint-tests.rkt` for missing metadata on high-risk suites, local `with-temp-dir` definitions, and env mutation without `@mutates`/`@isolation` tags.
- **Tests**: `test-run-tests-metadata-classification.rkt` (9 tests), `test-test-lint.rkt` (7 tests).
- Report-only mode — no enforcement in this milestone.

## v0.83.3 (2026-06-03)

### Canonical Test Sandbox and Isolation Pilot
- **Sandbox helper**: new `tests/helpers/test-sandbox.rkt` with `with-test-sandbox`.
- Creates isolated temp project/session/home/quarantine directories.
- Parameterizes `current-directory` and HOME/XDG env vars; restores on exit.
- Cleans up temp dirs unless `preserve?` is `#t`.
- 14 self-tests covering creation, isolation, cleanup, exception paths.
- **Workflow pilot**: `tests/workflows/test-sandbox-integration.rkt` verifies sandbox coexists with existing workflow fixtures.
- **Fixture adapter**: `with-isolated-temp-dir` in `tests/helpers/fixtures.rkt` wraps sandbox for existing tests.
- No mass migration — all existing tests backward compatible.

## v0.83.2 (2026-06-03)

### Metadata Parser and Fixture Self-Test Inclusion
- **Metadata parser**: new `scripts/test-metadata.rkt` parses `@suite`, `@boundary`, `@speed`, `@mutates`, `@isolation`, `@timeout` annotations from test file headers.
- Report-only: no test selection changes. Validates values, reports warnings.
- `scan-files-metadata` and `metadata-report` for batch scanning.
- **Fixture self-tests**: `tests/**/fixtures/test-*.rkt` now selected by runner (2 files, 27 tests).
- Support modules in `fixtures/` remain excluded.
- `all` suite: 760→762 files, `workflows` suite: 24→26 files.
- 10 new metadata parser tests.

## v0.83.1 (2026-06-03)

### Runner Safety and Gate Evidence v2
- **Strict argument validation**: reject unknown suite names, `--jobs 0`/negative, `--repeat 0`/negative, `--timeout 0`/negative, unknown `--flags`.
- **Gate evidence v2**: structured JSON evidence with version, git SHA, suite, args, counts, inventory hash, timestamp.
- Guards: no evidence on empty results, zero parsed tests, or any failure.
- `run-suite-once` now returns `(values exit-code results)` for evidence capture.
- 17 new runner tests (13 arg validation + 4 gate evidence).

## v0.83.0 (2026-06-03)

### Test Truth Baseline and Inventory
- Add `--inventory` report mode to `scripts/run-tests.rkt`.
- Reports: selected/excluded files per suite, classifier hits, high-risk flags (env/cwd/temp/subprocess/perf/terminal), inventory hash.
- 9 new runner inventory tests.
- No suite selection behavior changed.

## v0.82.6 (2026-06-02)

### Goal Feature Hotfix
- **F-1 CRITICAL**: Wire `agent-session-box` through TUI context pipeline — was always `#f`, so `/goal` always failed.
- **F-2 CRITICAL**: Add `goal-cancel-box` — `/goal clear` now signals running thread to stop.
- **F-5**: Fix `--evaluator` flag parsing — only check next token after `--evaluator`, not entire goal text.
- **F-6**: Inject `GOAL-EVIDENCE-SYSTEM-PROMPT` into every goal turn for evidence-driven behavior.
- **F-3**: Integration tests for session-box wiring and cancel mechanism (5 test cases).
- **F-4**: Add TUI slash commands reference to README.
- Update `cmd-ctx` struct (12→13 fields) and `tui-ctx` struct (19→21 fields).
- Update 10 test files for new cmd-ctx constructor arity.

## v0.82.5 (2026-06-02)

### Feature Flag Activation
- Flip `current-goal-loop-enabled?` default to `#t` — goal loop is now active by default.
- Updated `docs/getting-started/goal.md` with activation note.

## v0.82.4 (2026-06-02)

### GUI Activation + Integration Tests
- Wire GUI `/goal` handler to `goal-run!` (mirrors TUI pattern).
- Feature flag guard with session-null safety.
- 2 new GUI goal tests (feature flag guard + enable path).

## v0.82.3 (2026-06-02)

### System Instruction Injection
- Prepend turn context and evidence instructions to every goal turn.
- System instructions include turn count, max turns, and goal text.

## v0.82.2 (2026-06-02)

### TUI /goal Core Activation
- Wire `goal-run!` in `handle-goal-command` replacing stub message.
- Thread spawning with `exn:fail?` handler and shutdown-check.
- Event bridge adapter for goal-runner → event bus.
- Feature flag guard (`current-goal-loop-enabled?` default `#f`).

## v0.82.1 (2026-06-02)

### Goal Wiring Foundation
- Add `agent-session-box` field to `cmd-ctx` struct for goal-runner access.
- Create `tui/commands/goal-bridge.rkt` with `make-goal-event-bridge` and `make-goal-run-prompt!`.

## v0.82.0 (2026-06-02)

### Goal Feature Foundation
- Fix `extract-transcript-from-result` to handle `loop-result?` struct.
- Add `current-goal-loop-enabled?` feature flag (default `#f`).
- Remove all facade-dependent imports in `runtime/session/`, `runtime/compaction/`, `util/message/`.

## v0.81.5 (2026-06-02)

### runtime/ Safe Sub-Packaging
- Moved 10 compaction files into `runtime/compaction/` sub-package.
- Moved 17 session core files into `runtime/session/` sub-package.
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.4 (2026-06-02)

### runtime/ Safe Sub-Packaging
- Moved 8 goal files into `runtime/goal/` sub-package.
- Moved 8 provider+auth files into `runtime/provider/` and `runtime/auth/`.
- Moved 6 context surface files into `runtime/context/` sub-package.
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.3 (2026-06-02)

### util/ Event + Error + Message Sub-Packages
- Moved 9 event files into `util/event/` sub-package.
- Moved 6 error files into `util/error/` sub-package.
- Moved 3 message files into `util/message/` sub-package (including deprecated protocol-types.rkt).
- All re-export facades at original paths, deprecated for v0.83 removal.

## v0.81.2 (2026-06-02)

### util/ Safe Reclassification
- Moved 20 files into 8 sub-packages with re-export facades.
- `util/export/`, `util/tool/`, `util/json/`, `util/safe-mode/`, `util/path/`, `util/fsm/`, `util/content/`, `util/extension/`
- All facades marked DEPRECATED with target removal in v0.83.

## v0.81.1 (2026-06-02)

### Protocol-Types Migration
- Migrated source imports away from `util/protocol-types.rkt` to focused canonical sub-modules.
- Added source migration guardrail test for non-test facade references.
- Marked `util/protocol-types.rkt` as deprecated with target removal in v0.82+.

## v0.81.0 (2026-06-02)

### SSE Dedup + Contract Tightening
- Azure OpenAI SSE inline loop migrated to shared stream-sse-events
- any/c contracts tightened in stream-runner.rkt (7) and loop-phases.rkt (15)
- with-safe-fallback logging + placeholder test fixes

## v0.80.5 (2026-06-01)

### Architecture Polish
- Protocol-types facade consumer migration (14 files migrated to direct imports)
- Runtime sub-package documentation in dependency-policy.rktd
- T3-1: streaming-message.rkt — replaced 5 boxes with #:mutable struct fields
- T3-9: wave-executor.rkt — replaced statuses-box with #:mutable field
- T3-5: Fixed tui-init.rkt layer violation — moved GSD wiring to wiring/run-modes.rkt
- Session config regression guards (4 tests)
- Struct mutability tests (8 tests)


## v0.79.0 (2026-06-01)

### Final Context Assembly Activation

**6 gaps resolved across 4 milestones.** All feature flags OFF by default.

#### GAP-1: CLI/Config Entry Point for Profile
- Add `setting-context-assembly-profile` to settings.rkt (reads `(context-assembly profile)` from config.json)
- Add `context-assembly-profile` to session-config known-keys
- Add `--context-profile` CLI flag to args.rkt with validation
- CLI flag overrides settings in run-modes.rkt

#### GAP-2: WS Evolution Old-State Tracking
- Add `current-last-task-fsm-state` parameter to track previous task state across turns
- WS evolution now receives real old-state instead of always #f

#### GAP-3: Auto-Distillation Content Summaries
- Auto-distillation accepts optional content-summary hash
- Fallback conclusions use actual file content (truncated to 200 chars) instead of placeholder text
- Turn-orchestrator builds summaries from working-set messages

#### GAP-4: Archived WS Entries Persistence
- Add `current-archive-entry-fn` injectable callback for persisting archived WS entries
- `guarded-set-working-set-evolved!` logs archived entries count + calls callback when wired

#### GAP-5: Idle State Preamble
- `build-state-awareness-preamble` now returns proper preamble for idle state
- Agent sees guidance and conclusions even before first state transition

#### GAP-6: Revert-State Rollback Action
- Add `current-revert-state-fn` injectable callback to rollback-actions.rkt
- `maybe-execute-action` dispatches revert-state when fn is wired, safe #f default
- Wire callback in state-aware-builder.rkt with logging

### Test Summary
- settings accessor: 4 tests
- session-config round-trip: 2 tests
- CLI: 32 tests
- state-aware assembly: 20 tests
- rollback actions: 17 tests
- WS evolution: 12 tests
- session mutation: expanded (archive callback)
- auto-distillation: 12 tests

## v0.78.6 (2026-06-01)

### Post-Audit Remediation

Resolves 8 CRITICAL and 4 WARNING findings from v0.78.xx post-implementation audit.
All feature flags remain OFF by default — no behavior change without explicit activation.

#### C1: WS Evolution Type Mismatch (W0)
- Replaced `evolve-working-set-for-state` with `/result` variant in turn-orchestrator
- Returns `evolution-result?` struct that `guarded-set-working-set-evolved!` expects

#### C2: Recent Tool Calls Not Threaded (W0)
- Added `#:recent-tool-calls` keyword to `assemble-context/pure` contract and signature
- Now forwards `agent-session-recent-tool-calls` from session to `build-tiered-context/state-aware`

#### C3: G3 Auto-Distill Persistence Test (W1)
- Added test verifying auto-distilled conclusions persist through `guarded-set-task-conclusions!`

#### C4: G9 Amnesia Trigger Assertion (W1)
- Fixed weak test — now verifies `task-amnesia-detected` warning fires for repeat tool count > 2

#### C5: G10 FSM Preamble Test (W1)
- Replaced broken `let-values` with proper preamble text extraction
- Verifies `record_conclusion` and `set_task_state` appear in preamble text

#### C6: STATE.md Stale (W2)
- Updated to reflect v0.78.6 and all completed milestones

#### C7: HANDOFF.json Stale (W2)
- Updated with current version, milestone status, and wave PR references

#### C8: PLAN Status Stale (W2)
- Marked PLAN-v0.78.6-AUDIT-REMEDIATION.md as COMPLETE

#### W1: WS Evolution Old-State (W0)
- Changed hardcoded `task-idle` to `#f` (unknown) for inline WS evolution

#### W2: Orphaned Event Subscriber (W0)
- Removed `context.ws-evolve-requested` subscriber from session-events.rkt
- Event was emitted but never consumed; inline evolution is the actual path

#### W3: Weak Integration Assertions (W1)
- Strengthened `check-not-false tc` assertions with meaningful content checks
- Added preamble verification and budget enforcement assertions

#### W4: README Sync (W2)
- Synced README status section to reflect v0.78.6

## v0.78.0 (2026-06-01)

### Comprehensive Context Assembly

Resolves 10 audit gaps (G1-G10) from v0.77.9 deep audit across 5 sub-milestones.
All feature flags remain OFF by default -- no behavior change without explicit activation.

#### G1: Profile Activation Matrix (v0.78.1)
- Rewrote apply-context-assembly-profile! with 5 profiles: off/observe/bounded/self-healing/full
- Each profile is strict superset of previous
- Profile parameter now properly tracked via current-context-assembly-profile
- Moved current-ws-evolution-enabled? to state-aware-builder.rkt to break cycle

#### G2: WS Evolution Wiring (v0.78.2)
- Fixed subscriber race condition: event payloads now include old-state captured BEFORE mutation
- Wired WS evolution into turn-orchestrator context assembly path

#### G3: Auto-Distill Persistence (v0.78.2)
- Auto-distilled conclusions now persist back to session via guarded-set-task-conclusions!

#### G4: Rollout Gate (v0.78.4)
- session-rollout-enabled? bypasses rate check when profile is not off
- Profile-based activation is cleaner than rate-based A/B testing

#### G5: WS Evolution Merge (v0.78.0)
- guarded-set-working-set-evolved! now merges (union by ID) instead of replacing

#### G6: Planning Inference Confidence (v0.78.3)
- Planning heuristic confidence raised from 0.6 to 0.75 (above 0.7 threshold)

#### G7: Preamble Budgeting (v0.78.3)
- build-state-awareness-preamble now receives budgeted conclusions (not raw)

#### G8: Summary Mode Ranking (v0.78.3)
- Summary mode uses rank-and-budget instead of first+last heuristic

#### G9: Repeat Tool Count (v0.78.0)
- repeat-tool-count now computed from recent-tool-calls parameter (was hardcoded 0)

#### G10: FSM Workflow Instructions (v0.78.4)
- Preamble now includes brief FSM workflow guidance (~30 tokens)

#### Integration Tests
- Expanded from 7 to 12 integration tests covering all 10 gaps

## v0.77.10 (2026-06-01)

### Deep Audit Finding Resolution

Resolves 3 minor findings (M1–M3) from deep post-remediation audit of v0.77.9.
All feature flags remain OFF by default — no behavior change without explicit activation.

#### M1: WS Evolution Mutation Wiring
- Session-events subscriber now emits `context.ws-evolve-requested` event on state transitions
- Removed unused `evolve-working-set-for-state` import from session-events.rkt
- Event enables turn-orchestrator to handle WS evolution with WS in scope

#### M2: Real Rollback Action Execution
- `maybe-execute-action` dispatches to injectable callbacks (`current-force-distill-fn`, `current-expand-context-fn`)
- `force-distill` enables auto-distillation flag; `expand-context` doubles conclusion budget
- `current-rollback-action-log` records executed actions for observability
- `'revert-state` remains blocked even when execution enabled

#### M4: Graph Selection Convenience
- Added `graph-select-conclusions` to conclusion-graph.rkt — returns conclusion objects directly
- State-aware-builder uses convenience wrapper instead of manual ID→object hash lookup

#### M5: Integration Tests
- New `test-context-assembly-integration.rkt` with 5 tests exercising full pipeline
- Tests: graph selection with dependencies, bounded profile, rollback callbacks, WS evolution, auto-distill

## v0.77.9 (2026-06-01)

### Post-v0.77.xx Audit Remediation

Fixes 5 CRITICAL + 8 WARNING findings from v0.77.xx post-implementation audit.
v0.77.0 delivered pure library modules; v0.77.9 wires them into the runtime.

#### Runtime Wiring (was dead code in v0.77.0)
- Graph selection: `build-conclusion-graph` + `graph-select-by-seeds` wired into `state-aware-builder.rkt` when `current-graph-conclusion-selection?` is ON
- Budget enforcement: `rank-and-budget` wired into conclusion injection path when `current-conclusion-token-budget` > 0
- Auto-distillation: `auto-distill` wired into `turn-orchestrator.rkt` pre-assembly when `current-auto-distillation-enabled?` is ON
- Rollback actions: `check-rollback-triggers-with-actions` + `maybe-execute-action` wired into `state-aware-builder.rkt` when `current-rollback-action-execution?` is ON
- WS evolution: subscriber on `task.state.transitioned`/`task.state.inferred` events for observability
- Profile activation: `apply-context-assembly-profile!` wired into `build-assembled-context` via `config-context-assembly-profile`

#### Bug Fixes
- Fixed `make-conclusions` arity in E2E test (missing dependencies field)
- Fixed `dynamic-require` in test-record-conclusion.rkt (replaced with direct require)
- Fixed stale version refs (0.74.6 → 0.77.0) across 12 docs/wiki files
- Synced `info.rkt` version 0.76.9 → 0.77.0

#### Tests
- Added multi-node cycle detection test (A→B→C→A)
- Updated budget enforcement test to reflect active trimming
- All 69+ context-assembly tests pass

#### Changed Files
- `runtime/turn-orchestrator.rkt` — auto-distill wiring, profile activation
- `runtime/context-assembly/state-aware-builder.rkt` — graph selection, budget, rollback actions
- `runtime/session-events.rkt` — WS evolution subscriber
- `runtime/session-config.rkt` — config-context-assembly-profile accessor

## v0.77.0 (2026-06-01)

### Advanced Context Assembly Completion

9 milestones (v0.77.0-v0.77.8) turning state-aware context plumbing into a measured, bounded, dependency-aware, self-healing context system. All new behavior is behind feature flags and disabled by default.

#### New Modules
- `runtime/context-assembly/conclusion-graph.rkt` - Pure DAG with cycle detection, topological sort, seed selection
- `runtime/context-assembly/conclusion-ranker.rkt` - Deterministic scoring by state match, category, recency, tags
- `runtime/context-assembly/auto-distillation.rkt` - Uncovered WS entry detection + deterministic/LLM fallback
- `runtime/context-assembly/rollback-actions.rkt` - Bounded action model for self-healing assembly
- `scripts/context-assembly-report.rkt` - Activation summary report

#### Key Changes
- `token-metrics.rkt`: conclusion-budget-remaining telemetry field
- `ws-evolution.rkt`: evolution-result struct with kept/archived/evicted entries
- `session-mutation.rkt`: guarded-set-working-set-evolved!
- `session-events.rkt`: current-ws-evolution-enabled? flag; dependency wiring
- `session-store.rkt`: append-archive-marker! + load-conclusions-archived
- `record-conclusion.rkt`: Optional dependencies parameter
- `task-memory.rkt`: conclusions-for-context with graph + degraded fallback
- `state-aware-builder.rkt`: graph selection flag, hard token budget, triggers-with-actions
- `budgeting.rkt`: conclusion-budget-config (Typed Racket)
- `session-config.rkt`: 5-level graduated activation profiles

#### Feature Flags (all disabled by default)
- current-task-state-aware-assembly? - master switch
- current-ws-evolution-enabled? - WS evolution
- current-graph-conclusion-selection? - graph-based selection
- current-conclusion-token-budget - hard budget (2000)
- current-auto-distillation-enabled? - auto conclusions
- current-rollback-action-execution? - self-healing actions
- current-context-assembly-profile - activation profile (default: 'off)

#### New Tests (69+)
- 12 ws-evolution, 14 conclusion-graph, 8 ranker, 8 auto-distillation
- 10 rollback-actions, 3 session-config profiles, 24 token-metrics, 14 builder

#### Migration
- Fully backward-compatible. No behavior changes without explicit flag activation.
- record_conclusion gains optional dependencies arg; old calls unaffected.


## v0.76.9 (2026-06-01)

### Post-v0.76.8 Audit Hotfix

Fixes 2 CRITICAL + 3 WARNING findings from v0.76.8 audit.

#### Changes
- Fixed VALIDATION gates SG-12 + M7-G4: ADR reference corrected from 0017 to 0019 (was claimed in v0.76.8 but not applied)
- Fixed PLAN-v0.76.xx series header and status
- Fixed STATE.md version (0.76.8) and status (RELEASED)
- Fixed CHANGELOG v0.76.8 date (was 2026-05-30, now 2026-06-01)
- Updated HANDOFF.json current_version to 0.76.8
- Moved 2 test-cases inside their test-suite forms (test-record-conclusion.rkt, test-session-task-state.rkt)

## v0.76.8 (2026-06-01)

### Post-v0.76.7 Audit Closure

1 CRITICAL documentation integrity fix + 8 WARNING fixes + 3 INFO fixes.

#### Changes
- Fixed VALIDATION gates SG-12 + M7-G4: ADR reference corrected from 0017 to 0019
- Fixed STATE.md version (was stale at 0.76.6)
- Fixed PLAN-v0.76.xx series header (main series vs audit closure distinction)
- Removed dead `tc-result` binding in `state-aware-builder.rkt`
- Renamed `check-rollback-triggers` parameters from `#:before-tokens`/`#:after-tokens` to `#:before-messages`/`#:after-messages`
- Added `[DEPRECATED]` prefix to `save-conclusion` tool description
- Fixed `record-conclusion.rkt` event payload to send validated category symbol instead of raw string
- Added error guard around `set-task-state` event emission
- Fixed `when` guard in `test-v0756-audit-closure.rkt` that could swallow test failures

#### Tests Added
- Persistence round-trip test for string `origin-message-ids` (C1 regression guard)
- Assertion for `origin-message-id` in record_conclusion event payload (C2 regression guard)
- Integration test for `tool.set-task-state.completed` → session state change (C3 regression guard)
- Session handler test: `origin-message-ids` populated from event payload

## v0.76.7 (2026-06-01)

### Audit Closure for v0.76.xx Context Assembly Activation

Post-implementation audit fix: 3 CRITICAL + 11 WARNING findings resolved.
No behavior change (feature flag still defaults `#f`).

#### Critical Fixes
- **C1**: `origin-message-ids` deserialization type corruption — kept as strings
  (were being converted to symbols, breaking conclusion-first WS replacement)
- **C2**: `origin-message-id` now wired from `exec-context-call-id` into
  `record_conclusion` event payload; session subscriber reads and persists it
- **C3**: `set-task-state` tool now emits `tool.set-task-state.completed` event;
  session subscriber calls `guarded-set-task-fsm-state!` for explicit transitions

#### Warning Fixes
- **W1**: `save-conclusion` deprecated (emits `tool.deprecated` event)
- **W2**: `record_conclusion` added to inference `'meta` category
- **W5**: Debugging WS changed from `'full` to `'filtered` per decision A7
- **W6**: `check-rollback-triggers` wired into `build-tiered-context/state-aware`
- **W7**: Fixed tautological test assertion in `test-token-metrics.rkt`
- **W8**: Added `'excluded` WS path test for implementation state
- **W9**: Refactored brittle manual constructors in 3 test files to shared fixture
- **W10**: Fixed VALIDATION SG-12 ADR reference (0017 → 0019)
- **W11**: Fixed plan header series description

#### Tests Improved
- test-token-metrics.rkt: preamble verification (non-tautological)
- test-ws-conclusion-fallback.rkt: excluded WS path coverage
- test-session-task-state.rkt, test-v0756-audit-closure.rkt, test-record-conclusion.rkt:
  migrated to shared `make-test-session` fixture


## v0.76.6 (2026-05-31)

### Context Assembly Activation — Production Hardening (M7)

Final milestone: context efficiency dashboard, ADR, cleanup.

#### Changes
- **Dashboard**: `scripts/benchmark/context-efficiency.rkt` — parallel benchmark
  runner with per-scenario metrics, token savings table, and low-coverage flags
- **ADR 0019**: `docs/adr/0019-context-assembly-activation.md` — architecture
  decision record for the full v0.76.xx activation series
- **Cleanup**: removed dead code paths, consolidated imports, final integration
  tests across all 7 milestones

#### Tests Added
- test-v0756-audit-closure.rkt: persistence round-trip, preamble integration,
  FSM validation edge cases
- test-state-aware-assembly.rkt: end-to-end state-dependent assembly


## v0.76.5 (2026-05-31)

### Context Assembly Activation — Conclusion Dependencies (M6)

Sixth milestone: track file dependencies on conclusions for traceability.

#### Changes
- **Dependencies field**: `task-conclusion` struct gains `dependencies` field
  (list of file paths), acyclic by construction (simple tagging, no DAG)
- **Serialization**: `conclusion->hash` / `hash->conclusion` handle the new field
  with backward-compatible deserialization (defaults to `'()`)
- **Record tool**: `record_conclusion` accepts optional `dependencies` parameter

#### Tests Added
- test-conclusion-dependencies.rkt: dependency tracking, serialization round-trip,
  backward compatibility


## v0.76.4 (2026-05-31)

### Context Assembly Activation — Working-Set Evolution (M5)

Fifth milestone: replace working-set entries with conclusions when state
filters or excludes the WS.

#### Changes
- **Conclusion-first replacement**: `ws-entry->conclusion-or-self` replaces
  WS entries that match a conclusion's `origin-message-ids` with compact text
- **State-dependent WS behavior**: `evolve-working-set-for-state` applies
  state-specific WS levels (`'full`, `'filtered`, `'excluded`)
- **WS evolution module**: `runtime/context-assembly/ws-evolution.rkt` —
  per-state WS relevance configuration with conclusion overlay

#### Tests Added
- test-ws-conclusion-fallback.rkt: conclusion-first replacement, fallback,
  mixed WS, token reduction verification
- test-ws-evolution.rkt: per-state WS levels, conclusion overlay


## v0.76.3 (2026-05-31)

### Context Assembly Activation — Graduated Activation (M4)

Fourth milestone: safe rollout infrastructure with per-session config,
A/B comparison harness, and rollback triggers.

#### Changes
- **Per-session config**: `task-state-aware?` field on session config —
  deterministic hash-based rollout (default 0%, no behavior change)
- **A/B harness**: benchmark scenarios comparing state-aware vs. legacy assembly
  with 6 test scenarios across all task states
- **Rollback triggers**: `check-rollback-triggers` — detects excessive savings,
  amnesia-risk, and task-amnesia conditions for safety rollback

#### Tests Added
- test-session-rollout.rkt: rollout hash, per-session config, default safety
- test-rollback-triggers.rkt: all 3 trigger conditions, threshold boundaries


## v0.76.2 (2026-05-31)

### Context Assembly Activation — Measurement Infrastructure (M3)

Third milestone: build instrumentation to prove token savings.
Purely additive — no behavior change.

#### Changes
- **Token metrics**: `runtime/context-assembly/token-metrics.rkt`
  - `context-metrics` struct, `measure-context-size`, `compute-savings`
  - `category-breakdown`: per-tier token counts
  - `measure-context-assembly`: wrapper returning `(values context metrics)`
- **Conclusion coverage**: `compute-conclusion-coverage` — ratio of conclusion
  tool calls to total tool calls
- **Report script**: `scripts/report-context-efficiency.rkt` — text report
  with token savings and low-coverage warning

#### Tests Added
- test-token-metrics.rkt: 24 tests (token counting, savings, coverage,
  assembly wrapper)


## v0.76.1 (2026-05-31)

### Context Assembly Activation — System Prompt Integration (M2)

Second milestone: force the agent to use conclusions by injecting
state-aware instructions into the system prompt.

#### Changes
- **Prompt injection**: `state-guidance-table` updated with action-oriented
  instructions using `record_conclusion` (was: `save_conclusion`)
- **Dynamic preamble**: `build-state-awareness-preamble` now includes
  conclusion count and encouragement when empty
- **Benchmark**: `scripts/benchmark/task-state-aware.rkt` verifies
  instruction presence across all 5 states, token overhead, and
  conclusion count accuracy

#### Tests Added
- test-prompt-injection.rkt: 21 tests (presence, labels, instructions,
  record_conclusion references, conclusion count, no duplicates)


## v0.76.0 (2026-05-31)

### Context Assembly Activation — Gap Closure (M1)

First milestone of the v0.76.xx series: close all v0.75.xx infrastructure gaps
so state-aware context assembly is technically complete.

#### New Features
- **Tools**: `record_conclusion` tool — agent can save distilled insights as
  `task-conclusion` structs with category, tags, and FSM state origin.
  Emits `tool.record_conclusion.completed` event; session layer persists.

#### Architecture
- **Extraction**: `serialization.rkt` reduced from 494 → 173 lines
  - `context-floor.rkt`: core tiered context building (structs, partitioning, hooks)
  - `state-aware-builder.rkt`: state-aware extensions (preamble, conclusion injection)
- **FSM**: `guarded-set-task-fsm-state!` now validates state→state transitions
  (rejects e.g. exploration → verification; allows reset to idle)
- **Inference**: `guarded-set-recent-tool-calls!` validates accumulated tool history
- **Relevance**: `implementation` state hard-excludes `working-set` (was filtered)

#### Tests Added
- test-record-conclusion.rkt: 13 tests (tool validation, event emission, persistence)
- test-task-state.rkt: +7 tests (transition validation, guarded setter)
- test-task-state-inference.rkt: +4 tests (symbol names, batch caps, multi-turn)
- test-state-aware-builder.rkt: 13 tests (feature flag, preamble, conclusions)
- test-state-relevance.rkt: 11 tests (all states × categories)


## v0.75.9 (2026-05-31)

### Post-v0.75.8 Audit Fix

#### Bug Fixes
- **GUI**: Fixed tool result misalignment — parallel tool completions now correctly correlate to their own tool call (was: all → OK hit the last message)
- **Tools**: Fixed `set-task-state` always failing from LLM — `hasheq` symbol keys now accept string args via `string->symbol` conversion
- **Streaming**: Emit `model.stream.completed` on provider stream exceptions — prevents GUI text concatenation on next turn

#### Tests Added
- test-gui-state-sync.rkt: 27 tests (+3: parallel tool correlation, mixed OK/FAIL, orphan completion)
- test-save-conclusion.rkt: 16 tests (+2: string args for set-task-state)

## v0.75.8 (2026-05-31)

### Post-v0.75.7 Audit Fix

#### Bug Fixes
- **GUI**: Fixed tool results showing `→ #f` — now shows `→ OK` or `→ FAIL` with correct event payload keys (`toolName`/`resultSummary`)
- **GUI**: Tool results now inline with tool calls (`Tool: [bash] → OK`) instead of separate messages
- **GSD**: `/plan` wave cleanup now instant — uses `delete-directory/files` instead of per-file iteration
- **GSD**: Plan validator no longer blocks docs-only plans with no file references — downgraded to WARNING

#### Tests Updated
- test-gui-state-sync.rkt: 24 tests (updated tool.execution.completed tests for inline display)
- test-gsd-plan-validator.rkt: 10 tests (updated docs-only plan expectation)
- test-gsd-plan-validator-unit.rkt: 14 tests (updated docs-only plan expectations)

## v0.75.7 (2026-05-30)

### GUI/GSD Workflow Usability Fixes

#### Bug Fixes
- **GUI**: Tool messages now show arguments and results in transcript (was: `Tool: [xxx]`, now: `Tool: [bash] command: ls`)
- **GUI**: Tool execution results now appear in transcript via `tool.execution.completed` handler
- **GUI**: `gui-message` struct extended with `meta` field for structured tool data
- **GSD**: `/plan` now cleans old wave files and emphasizes wave-first ordering in prompt
- **GSD**: `/go` now validates wave doc existence before state transition
- **GSD**: `/go` resilient to relaxed wave index format (`- W0: Title` without `[Status]` brackets)
- **GSD**: `/go` now includes persistent task summary to prevent focus loss during execution
- **Context**: `gsd-progress-message?` extended to pin execution-instruction messages

#### Tests Added
- 13 tests in test-gui-types.rkt (5 new: meta field)
- 25 tests in test-gui-state-sync.rkt (5 new: tool args/results, 20 fixed: struct migration)
- 5 tests in test-gsd-wave-gen-validation.rkt (wave doc validation)
- 5 tests in test-gsd-relaxed-index.rkt (relaxed parser)
- 3 tests in test-gsd-task-focus.rkt (task summary)
- 6 tests in serialization.rkt module+test (execution instruction pinning)


## v0.75.6 (2026-05-30)

### Audit Closure (v0.75.6)

- **Assembly Wiring**: Thread session through `build-assembled-context` for task-state-aware context.
- **Preamble Integration**: Wire `build-state-awareness-preamble` into `build-tiered-context/state-aware`.
- **FSM Validation**: `guarded-set-task-fsm-state!` now rejects invalid state symbols.
- **Unique IDs**: Replace duplicate conclusion message IDs with `generate-id`.
- **Persistence**: Task-state and conclusion round-trip to session log.
- **Inference Accumulation**: `recent-tool-calls` field accumulates last 10 tool calls for better state inference.
- **Dual Input**: Context assembly accepts both fsm-state structs and bare symbols.
- **14 new tests** in `test-v0756-audit-closure.rkt`.

## v0.75.5 (2026-05-30)

### Advanced Context Assembly (v0.75.0-v0.75.5)

- **Task-State FSM**: 6-state FSM with 8 events using `define-fsm-machine`
- **Tools + Session**: `save-conclusion` and `set-task-state` tools, session fields
- **State Inference**: Heuristic tool-call pattern analysis, event-wired
- **State-Aware Assembly**: Relevance matrix, feature-flagged context assembly
- **Working-Set Evolution**: State-transition-driven WS clearing/filtering
- **System Prompt Injection**: State-awareness preamble with guidance + conclusions
- ~106 new tests across 7 test files


## v0.74.8 (2026-05-31)

### Context-Assembly Contract Bugfix + Test Quality Remediation

- **P0 FIX**: `build-tiered-context-with-hooks` contract declared `(or/c hash? #f)` but returned `(or/c hook-result? #f)` — broke all prompt submission with hooks active
- **R1**: Removed trivially-true `check-true #t` assertions in `test-session-persistence.rkt`
- **R2**: Added `current-crash-log-dir` parameter for test isolation — zero `~/.q/` pollution
- **R3**: Added 3 contract-rejection tests for all exported functions
- **R4**: Added 4 direct function tests for `ensure-persisted!` and `buffer-or-append!`
- Test count: 6 → 13 (all meaningful assertions, temp-dir isolated)
- Version bump 0.74.7 → 0.74.8

## v0.74.7 -- 2026-05-31

### Audit Closure (W0 + W1 + W2)
- **W0**: Fixed stale `loop-state.rkt` references (moved from `runtime/` to `agent/` in v0.73.4)
  - Updated `dependency-policy.rktd`: moved exception to `agent` layer, updated sub-modules
  - Updated `test-arch-fitness.rkt`: corrected TR module path
  - Arch-fitness test failures: 2 → 0
- **W1**: Session persistence extraction
  - Extracted `write-crash-log!`, `ensure-persisted!`, `buffer-or-append!` to `runtime/session-persistence.rkt`
  - Added 6 crash-logger tests (was zero)
  - `session-lifecycle.rkt`: 478 → 438 lines (target ≤450)
  - Backward-compatible re-export from `session-lifecycle.rkt`
- **W2**: Arch-fitness tests + version bump
  - Added 6 arch-fitness test-cases (41 → 47)
  - Added agent layer policy tests
  - Added session persistence module verification
  - Added session lifecycle line-budget check
  - Version bump 0.74.6 → 0.74.7


## v0.74.6 -- 2026-05-30

### Architecture Controls & Release (M7)
- **W0**: Activated CI gates
  - Hotspot block-threshold (>20,000 score) now BLOCKING — top files must have risk-notes
  - Added risk-notes for 9 top hotspot files (tui/state-events, tui/terminal-input, llm/gemini, etc.)
  - Cycle-detection tests already blocking
  - Metrics synced to README
- **W1**: Version bump 0.74.5 → 0.74.6, final release

### v0.74.xx Series Summary
7 milestones (v0.74.0–v0.74.6), ~1,000 LOC across:
- Hotspot fitness function with scoring
- Failure-domain classifier (7 error domains)
- Session store cycle elimination (parameter injection)
- Cycle-detection CI gate
- Widened contract reconciliation (~44 contracts tightened)
- Session lifecycle FSM extraction
- GSD boundary clarification + TUI render phase extraction
- Architecture controls activation


## v0.74.5 -- 2026-05-30

### GSD Boundary Clarification + TUI Render Extraction (M6)
- **W0**: Added 8 boundary tests for gsd/core.rkt contract-out API
  - Command dispatch, write guard, status, reset, accessors, contract rejection
- **W1**: GSD state already well-extracted into gsd/session-state.rkt
  - Verified facade pattern: gsd-planning.rkt → gsd/session-state.rkt + gsd/core.rkt
  - Version bump 0.74.4 → 0.74.5


## v0.74.4 -- 2026-05-30

### Session Lifecycle FSM Extraction (M5)
- **W0**: Expanded pure function tests (8 test cases, 19 assertions)
- **W1**: Extracted `session-lifecycle-transitions.rkt` from session-lifecycle
  - `build-user-message`, `compute-parent-id`, `inject-system-instructions`
  - ~40 LOC reduction in session-lifecycle.rkt hotspot
  - Fixed `build-tiered-context-with-hooks` contract: `hash?` → `(or/c hash? #f)`
  - Registered in dependency-policy.rktd
- **W2**: Added 5 fault-injection tests for edge cases
  - Special characters, long strings, malformed entries, message order preservation


## v0.74.3 -- 2026-05-30

### Widened Contract Reconciliation — Investigation Batch (M4)
- Deep caller analysis of 17 investigate-category contracts
- Tightened 14 contracts after confirming actual types:
  - `session-index.rkt` / `mutations.rkt`: branch!, branch-with-summary!,
    append-to-leaf!, lookup-entry, resolve-active-leaf, active-leaf,
    estimate-entry-tokens — all now use `message?` or `(or/c message? #f)`
  - `session-manager.rkt`: sm-fork! kept at `any/c` (returns polymorphic fork result)
  - `settings.rkt`: make-minimal-settings #:provider → `(or/c string? #f)`
  - `context-assembly.rkt`: build-tiered-context-with-hooks 2nd value → `hash?`
  - `llm/stream.rkt`: normalize-openai-chunk(s) → `(or/c stream-chunk? hash?)`
  - `llm/http-helpers.rkt`: translate-stop-reason 2nd arg → `(or/c string? symbol? #f)`
  - `gsd/tool-handlers.rkt`: get-base-dir optional → `(or/c exec-context? #f)`
  - `gsd/wave-docs.rkt`: parse-wave-doc-from-string → specific arg types


## v0.74.2 -- 2026-05-30

### Widened Contract Reconciliation (M3)
- **W0**: Audited 120+ `any/c` positions in contract-out forms across all layers
  - Categorized: ~45 tighten, ~15 investigate, ~55 keep
- **W1**: Tightened ~30 safe contracts across 6 layers:
  - `runtime/`: session-manager (message?), compaction-hooks (procedure?),
    provider-factory (q-settings?), session-controls (model-registry?),
    settings (hash?), iteration/directive (working-set?, list?),
    iteration/fsm-types (fsm-state?, fsm-event?)
  - `tools/`: registry-defaults (tool-registry?), tool-internal (tool?→procedure?),
    tool.rkt (or/c string? hash? list?), builtins/bash+grep+edit+firecrawl+session-recall (tool-result?)
  - `util/`: message-helpers (message?→boolean?), tree-entries (or/c string? #f),
    content-parts (boolean? for is-error)
  - `llm/`: openai-compatible (model-request?, model-response?)
  - `extensions/`: tiers (extension?)
  - `sandbox/`: subprocess (custodian?)
- Reverted 2 auth-store tightenings that broke intentional polymorphic tests


## v0.74.1 -- 2026-05-30

### Session Store Cycle Elimination (M2)
- **W0**: Eliminated `lazy-require` circular dependency between `session-store.rkt`
  and `session-store-tree.rkt` using runtime parameter injection
- **W1**: Added cycle-detection CI gate (DFS-based) to `test-arch-fitness.rkt`
  for runtime, agent, and tools layers
- **W2**: Version bump to 0.74.1


## v0.74.0 -- 2026-05-30

### Fitness Functions & Quick Wins (M1 W0)

- **Fix** `test-registry-snapshot.rkt` — add missing `timeout-seconds` field
- **New** C1 regression test for `extension-registry?` predicate
- **New** `log-stream-setup-timing` unit test (contract accepts `real?`)
- **Tighten** `step-interpreter.rkt` contracts — replace `any/c` with struct predicates
- **Clean** README — remove 214 excess blank lines
- **New** 11 tool security edge-case tests using shell-risk classifier

## v0.73.8 -- 2026-05-30

### Audit Closure (A-7)

- **CRITICAL:** Fix `extension-registry?` contract violation — replaced fake `procedure?` predicate with real re-export from `extensions/api.rkt` (C1)
- **W1:** Activate dead loggers — remove dead `define-logger` from `loop-fsm.rkt`, add 4 `log-q-main-loop-info` calls at FSM transition sites in `main-loop.rkt`
- **W2:** Complete shared timing adoption — fix `timing.rkt` contract to accept `real?`, replace inline timing logs in `anthropic.rkt` and `gemini.rkt`
- **W3:** Add `contract-out` to `step-interpreter.rkt` for `interpret-step`, `handle-stop-action`, `execute-pending-tool-calls`, `sink-append-entries!`

## v0.73.7 -- 2026-05-30

### Extension Tests + Contract Completion (T-6/A-6b)

| Change | Detail |
|--------|--------|
| T-6 | 27 unit tests for GSD command parsing + dispatch |
| T-6 | 14 unit tests for GSD plan validator |
| A-6b | Contract-out added to `sandbox/subprocess.rkt` |
| A-6b | Contract-out added to `sandbox/evaluator.rkt` |

41 new test cases. Sandbox security boundary now has typed contracts.

## v0.73.6 -- 2026-05-30

### Observability Infrastructure (T-3/T-4/T-5)

| Change | Detail |
|--------|--------|
| T-3 | Added `define-logger` to 6 critical modules |
| T-4 | Added trace logging to tool register/unregister |
| T-5 | Reduced flaky `sleep 10` → `sleep 2` in sandbox limits test |

**New loggers:** `q-loop-fsm`, `q-main-loop`, `q-tool-registry`, `q-openai`,
`q-turn-orch`, `q-session-lifecycle`. All additive — no behavior changes.

## v0.73.5 -- 2026-05-30

### Credential Backend Decomposition (A-4)

Decomposed the 650-line `credential-backend.rkt` god module into 7 focused
modules under `runtime/credentials/`:

| Module | Content |
|--------|---------|
| `protocol.rkt` | Struct + generic ops + command runner parameters |
| `file-backend.rkt` | JSON file credential storage |
| `env-backend.rkt` | Environment variable backend (read-only) |
| `memory-backend.rkt` | In-memory backend (testing) |
| `keychain-backend.rkt` | Linux `secret-tool` backend |
| `platform-backends.rkt` | macOS `security` + Windows `cmdkey` + capabilities |
| `chained-backend.rkt` | Chained fallback + policy-aware wrapper |

`credential-backend.rkt` becomes a re-export facade. Zero API changes.

## v0.73.4 -- 2026-05-30

### Iteration Layer Migration (A-1)

**Moved 7 files** from `runtime/iteration/` to `agent/iteration/`:
- `counters.rkt`, `loop-config.rkt`, `loop-phases.rkt`, `loop-state.rkt`
- `main-loop.rkt`, `step-interpreter.rkt`, `tool-turn-bridge.rkt`

**Rationale:** These 7 files have dense coupling to `agent/` (event emission,
queue operations, event bus types) and implement the agent loop's iteration
mechanics. Moving them aligns code location with conceptual ownership.

**~80 import paths** updated across source and test files.
Zero logic changes. All existing tests pass.

## v0.73.3 -- 2026-05-30

### Core Loop Tests: State Machines + Dispatch (T-1b)

| Finding | Change | Tests |
|---------|--------|-------|
| T-1b | Loop FSM state machine tests | test-loop-fsm.rkt (19 cases) |
| T-1b | Loop dispatch surface tests | test-loop-dispatch.rkt (5 cases) |
| T-1b | Tool registry unit tests | test-tool-registry-unit.rkt (14 cases) |

**38 new test cases** across 3 test files. Zero logic changes.

## v0.73.2 -- 2026-05-30

### Tool Builtins + Security Tests (T-2/T-7/S-1/S-2)

| Finding | Change | Tests |
|---------|--------|-------|
| T-7 | FSM unit tests | test-fsm-unit.rkt (8 cases) |
| T-7 | Shell-quote injection tests | test-shell-quote.rkt (16 cases) |
| T-2 | Read builtin tests | test-tool-read-builtin.rkt (7 cases) |
| T-2 | Write + edit builtin tests | test-tool-write-builtin.rkt (5), test-tool-edit-builtin.rkt (7) |
| S-1 | Sandbox limits tests | test-sandbox-limits-unit.rkt (4 cases) |
| S-2 | Image-pipeline temp cleanup reviewed | Already safe — no source change needed |

**47 new test cases** across 6 test files. Zero logic changes.

## v0.73.1 -- 2026-05-30

### Architecture Fixes + Code Quality (A-2/A-3/Q-1/Q-2/Q-3)

| Finding | Change | Files |
|---------|--------|-------|
| A-2 | Extract `tool?` to `util/tool-types.rkt` | util/tool-types.rkt, loop-dispatch.rkt, loop-phases.rkt |
| A-3 | Move `extension-registry?` to `util/extension-types.rkt` | util/extension-types.rkt, loop-state.rkt |
| Q-1 | Adopt `now-epoch-ms`/`now-epoch-secs` across 4 files | util/time.rkt, goal-*.rkt, sdk-core.rkt |
| Q-2 | Extract `with-stream-timing` into `llm/timing.rkt` | llm/timing.rkt, 4 provider files |
| Q-3 | Migrate 2 files to `#lang racket/base` | sdk-public.rkt, tui/state.rkt |

Zero logic changes. Pure mechanical refactoring.

## v0.73.0 — 2026-05-30

### Core Loop Tests: Pure Functions (T-1a)

| Module | Tests | Focus |
|--------|-------|-------|
| `test-iteration-decision.rkt` | 19 | decide-next-action, compute-step-result, step-action?, struct accessors |
| `test-iteration-retry-policy.rkt` | 16 | compute-mid-turn-estimate, detect-exploration-loop, count-occurrences, overflow recovery |
| `test-iteration-counters-unit.rkt` | 8 | compute-next-counters (explore/implement/tool counts), documented check-cancellation gap |

**43 new test cases** across 3 previously untested modules. Zero source changes.

## v0.72.8 — 

### Audit Closure

Close 5 remaining findings from v0.72.xx post-implementation audit.

| Finding | Fix |
|---------|-----|
| AU-2: 6 internal structs #:transparent | Removed #:transparent from directive-recurse, directive-stop, directive-yield, tool-call-actions, subscription, in-memory-session-manager |
| AU-3: turn-reducer any/c contracts | Tightened decide-after-pre-hook and decide-after-msg-hook to (or/c hook-result? #f) |
| AU-4: guarded-set-index! any/c | Tightened to (or/c session-index? #f) |
| AU-5: q-component? placeholder | Documented cycle constraint; placeholder retained with explanation |
| Fix: test-component-model.rkt | Updated component-state-set! → component-state-update (missed in v0.72.7) |

**Files changed:** directive.rkt, tool-coordinator.rkt, event-bus.rkt, in-memory.rkt, turn-reducer.rkt, session-mutation.rkt, state-types.rkt, test-step-directive.rkt, test-turn-reducer.rkt, test-component-model.rkt

## v0.72.7 — 

### Phase 5: Long-Term Patterns + Sweep

TUI reducer pattern + event registry lifecycle + mutable hash audit + naming cleanup + misc fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| W6: TUI slash command mutation | WARNING | Added apply-slash-command return-based wrapper |
| W7: event-reducer test isolation | WARNING | Added current-event-reducers parameter + call-with-test-registry |
| W15: mutable hash thread safety | WARNING | Documented thread-safety guarantees for 3 registries |
| W17: component-state-set! naming | WARNING | Renamed to component-state-update |
| I4: goal-state-total-token-cost location | INFO | Moved from goal-runner.rkt to goal-types.rkt |
| I5: tool-id provide | INFO | Documented as backward-compat |
| I9: compactor current-seconds | INFO | Replaced with now-epoch-ms for consistency |
| I10: gsd/core boundary | INFO | Documented pure/effectful sections |
| I11: scheduler contracts | INFO | Added contract-out for plan-tool-batch, execute-tool-plan |
| I12: field->json-key duplication | INFO | Documented compile-time vs runtime split |

**Files changed:** commands.rkt, state-events.rkt, component.rkt, tui-render-loop.rkt, goal-types.rkt, goal-runner.rkt, define-tool.rkt, compactor.rkt, core.rkt, scheduler.rkt, event-macro.rkt, settings.rkt, model-registry.rkt

## v0.72.6 — 

### Phase 4b: Module Decomposition II

Event-structs explicit provides + event-bus documentation.

| Finding | Severity | Fix |
|---------|----------|-----|
| W12: event-bus layer violation | WARNING | Documented as foundational utility in extensions/api.rkt |
| W11: event-structs all-from-out | WARNING | Replaced with 239 explicit identifier provides |
| I13: context-assembly facade | INFO | Documented sub-module origins for future migration |

**Files changed:** event-structs.rkt, extensions/api.rkt, context-assembly.rkt

## v0.72.5 — 

### Phase 4a: Module Decomposition I

Session-store split + errors split + protocol-types documentation.

| Finding | Severity | Fix |
|---------|----------|-----|
| W3: session-store monolith | WARNING | Split into versioning.rkt + in-memory.rkt sub-modules |
| W14: warn-deprecated! in errors.rkt | WARNING | Extracted to util/deprecation.rkt, re-exported |
| I2: protocol-types transitional | INFO | Documented remaining consumers for future migration |

**Files changed:** session-store/versioning.rkt (new), session-store/in-memory.rkt (new), session-store.rkt (facade), deprecation.rkt (new), errors.rkt, protocol-types.rkt

## v0.72.4 — 

### Phase 3b: Opaque Representations

Struct opacity + goal-state split + mutable-set elimination.

| Finding | Severity | Fix |
|---------|----------|-----|
| W2: goal-state monolith | WARNING | Split into goal-types.rkt + goal-codec.rkt, facade re-exports |
| W1: turn-model #:transparent structs | WARNING | Removed #:transparent from 5 structs |
| I7: append-entries! uses set-box! | INFO | Already pure — no change needed |
| I8: compactor mutable-set | INFO | Replaced with immutable set + for/fold |

**Files changed:** `goal-types.rkt` (new), `goal-codec.rkt` (new), `goal-state.rkt` (facade), `turn-model.rkt`, `compactor.rkt`

## v0.72.3 — 

### Phase 3a: Purity Enforcement

Pure/impure separation in agent loop.

| Finding | Severity | Fix |
|---------|----------|-----|
| C3: run-streaming-phase is impure in pure module | CRITICAL | Extracted to `agent/loop-dispatch.rkt` |
| W5: tool-coordinator lacks purity labels | WARNING | Added EFFECTFUL/PURE labels to phase comments |

**Files changed:** `loop-dispatch.rkt` (new), `loop-phases.rkt`, `loop.rkt`, `tool-coordinator.rkt`

## v0.72.2 — 

### Phase 2b: Boundary Contract Precision

Boundary tightening for tool-registry, settings, and state-types.

| Finding | Severity | Fix |
|---------|----------|-----|
| W10: tool-registry internal fields publicly exported | WARNING | Moved to `module+ internal` |
| W9: settings globals in public API | WARNING | Removed q-settings-global/project from provide |
| W8: state-types uses `any/c` for styled-line/q-component | WARNING | Replaced with real predicates |
| I6: tool-schema naming | INFO | Verified — no rename needed |

**Files changed:** `tool-registry-struct.rkt`, `registry.rkt`, `settings.rkt`, `state-types.rkt`

## v0.72.1 — 

### Phase 2a: Core Contract Precision

Contract tightening for core modules.

| Finding | Severity | Fix |
|---------|----------|-----|
| C1: 28 `any/c` in loop-phases contracts | CRITICAL | Replaced with string?, event-bus?, provider?, loop-state?, procedure?, (listof tool?) |
| W13: turn-reducer lacks contracts | WARNING | Reviewed — contracts already specific where possible |
| I14: session-mutation uses `any/c` for typed fields | INFO | Tightened config→hash?, start-time→exact-nonnegative-integer?, last-compaction-time→(or/c exact-nonnegative-integer? #f) |

**Files changed:** `loop-phases.rkt`, `session-mutation.rkt`

## v0.72.0 — 

### Phase 1: Zero-Cost Fixes

Abstraction quality remediation — zero-risk fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| C2: Raw setter breach — 13 deprecated setters in public API | CRITICAL | Moved to `module+ internal`, only `session-mutation.rkt` can import |
| W4: `epoch-ms` pattern repeated across codebase | WARNING | Extracted `now-epoch-ms` into `util/time.rkt` |
| W16: No effect-base supertype | WARNING | Added `effect-base` struct, `effect?` uses supertype predicate |
| I1: Dead `with-cleanup` macro exported | INFO | Deleted from `errors.rkt` |
| I3: Duplicate bare+only-in import in `goal-runner.rkt` | INFO | Replaced with `except-in` |

**Files changed:** `session-types.rkt`, `session-mutation.rkt`, `session-compaction.rkt`, `effect-types.rkt`, `errors.rkt`, `goal-runner.rkt`, `util/time.rkt` (new)

## v0.71.8 — 

### v0.71.7 Audit Closure

Post-implementation audit closure for the v0.71.7 hotfix. Test gaps + event payload alignment + doc fixes.

| Finding | Severity | Fix |
|---------|----------|-----|
| WARN-1: No test for goal-check-completed event | WARN | Added test via goal-loop-step |
| WARN-2: No test for concurrent goal guard rejection | WARN | Added guard condition verification test |
| INFO-1: string-truncate missing docstring | INFO | Added docstring clarifying output length |
| INFO-2: No payload key assertions in event tests | INFO | Added goal-achieved/goal-started payload checks |
| INFO-3: No test for goal-failed on shutdown-cancel | INFO | Enhanced shutdown test with event tracking |
| INFO-4: Cancelled state not verified in failed event | INFO | Assert goal-state-status in failed payload |
| INFO-5: Event payloads use raw structs | INFO | Aligned 4 event types to hasheq payloads |
| DOC-1: VALIDATION header stale | DOC | Fixed header reference |

**Files changed:** `goal-state.rkt`, `goal-runner.rkt` + 2 test files
**Tests added:** 4 new/enhanced tests

## v0.71.7 — 

### Goal Audit Hotfix

Post-implementation audit hotfix for the `/goal` autonomous loop feature.

| Finding | Severity | Fix |
|---------|----------|-----|
| F1: Type confusion — evaluation-result mixed into checks field | FAIL | Added `evaluations` field to goal-state |
| F2: goal.achieved / goal.check.completed events never emitted | FAIL | Runner now emits both events |
| W1: No shutdown-requested check in runner loop | WARN | Added `#:shutdown-check` parameter |
| W2: No concurrent goal guard — silent overwrite | WARN | TUI rejects `/goal` when active-goal set |
| W3: Shell injection via $(...) only rejected at critical | WARN | Raised to high severity |
| I1: string-truncate duplicated across modules | INFO | Consolidated into goal-state.rkt |
| I2: NO-PROGRESS-THRESHOLD duplicated in runner | INFO | Removed duplicate, uses goal-state export |

**Files changed:** `goal-state.rkt`, `goal-runner.rkt`, `goal-checks.rkt`, `shell-risk.rkt`, `tui/commands.rkt` + 3 test files
**Tests added:** 9 new tests across 3 files
**Breaking changes:** None — new field defaults to empty list


## v0.71.6 — 2026-05-30

### Agent Evaluator + Series Audit

Seventh and final milestone of the `/goal` feature — agent-based evaluation and series closure.

- **Agent evaluator**: New `runtime/goal-agent-evaluator.rkt` with `evaluate-with-agent` — deeper investigation with enhanced system prompt, 2000 token budget, JSON extraction from prose responses.
- **Evaluator mode dispatch**: `goal-runner.rkt` dispatches to `evaluate-with-agent` when `evaluator-mode` is `'agent`. New `--evaluator agent` flag in `/goal` command.
- **Regression verified**: All 9 goal test files pass (state, evaluator, runner, evidence, checks, command, TUI, agent-evaluator, GUI).

#### v0.71.x Series Summary

| Milestone | Theme | New Files | Tests |
|-----------|-------|-----------|-------|
| v0.71.0 | Goal State & Event Foundation | goal-state.rkt + 6 events | 18+ |
| v0.71.1 | Transcript Evaluator + Core Loop | goal-evaluator.rkt, goal-runner.rkt | 22 |
| v0.71.2 | Evidence Discipline + No-Progress | goal-evidence.rkt | 14 |
| v0.71.3 | Deterministic Checks | goal-checks.rkt | 14 |
| v0.71.4 | TUI Integration & Status Display | state-types/event extensions | 11 |
| v0.71.5 | GUI Integration + Docs | gui-types, slash-commands | 5 |
| v0.71.6 | Agent Evaluator + Audit | goal-agent-evaluator.rkt | 7 |

## v0.71.5 — 2026-05-30

### GUI Integration + Release Polish

Sixth milestone of the `/goal` feature — GUI parity and documentation.

- **GUI active-goal**: Added `active-goal` field to `gui-state` struct with `gui-state-set-active-goal` helper.
- **GUI /goal handler**: `/goal clear/status/set` in `gui/slash-commands.rkt`. Added to help text.
- **Documentation**: New `docs/getting-started/goal.md` covering usage, checks, monitoring, limitations, and security.
- **Tests**: 5 GUI tests for struct, setter, hash round-trip.

## v0.71.4 — 2026-05-30

### TUI Integration & Status Display

Fifth milestone of the `/goal` feature — visibility in the TUI.

- **ui-state extension**: New `active-goal` field with `goal-display-info` struct (goal-text, turns-used, max-turns, status).
- **Event reducers**: 6 goal event reducers registered in `state-events.rkt` — `goal.started`, `goal.turn.started`, `goal.evaluated`, `goal.check.completed`, `goal.achieved`, `goal.failed`.
- **Status bar badge**: Goal progress shown in status bar when active: `◎ goal 3/8 · active`. Terminal states: `✓ achieved`, `✗ failed`.
- **Command handler**: `/goal status` shows active goal info. `/goal clear` clears `active-goal` from ui-state.
- **Tests**: 11 TUI tests covering event reducers, display info, badge rendering.

## v0.71.3 — 2026-05-30

### Deterministic Checks

Fourth milestone of the `/goal` feature — user-defined shell commands as verifiable evidence.

- **goal-checks.rkt**: Parse `--check` arguments from `/goal` command. Validate safety via `classify-shell-risks` (blocks critical commands like `rm -rf`). Execute checks via subprocess with timeout + custodian isolation.
- **Evaluator integration**: `evaluate-transcript` gains `#:check-results` parameter. Check results appended to evaluator prompt as deterministic evidence alongside transcript.
- **Runner integration**: `execute-checks-for-goal` runs all checks before LLM evaluation. Results feed directly into evaluator.
- **Command handler**: `/goal "tests pass" --check 'raco test'` parses check arguments, validates safety, shows check summary.
- **Tests**: 14 new tests — parsing, safety, execution, output capture, check-augmented evaluation.

## v0.71.2 — 2026-05-30

### Evidence Discipline + No-Progress Detection

Third milestone of the `/goal` feature — reliable evaluation with evidence requirements.

- **goal-evidence.rkt**: Evidence system prompt injection forces worker agent to produce verifiable evidence (commands, exit codes, file diffs). `evidence-prompt-for-goal` builds continuation prompts with evaluation feedback.
- **No-progress detection**: `detect-no-progress` and `consecutive-same-reason?` detect stall conditions from 3 consecutive same-reason evaluation failures.
- **Runner integration**: Goal runner now uses evidence prompts for continuations and accumulates evaluation history in goal-state checks field. No-progress detection replaces simple counter.
- **Tests**: 17 new tests — evidence prompts (5), no-progress detection (6), runner integration (3), collect-evaluations (1).

## v0.71.1 — 2026-05-30

### Transcript Evaluator + Core Loop

Second milestone of the `/goal` feature — first working autonomous goal loop.

- **goal-evaluator.rkt**: Transcript evaluation via cheap LLM. Sends goal + transcript, parses JSON response `{ok, reason}`. Graceful fallback for non-JSON responses.
- **goal-runner.rkt**: Main goal loop orchestration. Bounded by `max-turns` (default 8). No-progress detection after 3 consecutive same-reason failures. Events emitted at each phase. `goal-run-simulated!` for testing with predefined responses.
- **TUI /goal command**: Added `/goal` and `/g` to command table. Supports `/goal clear`, `/goal status`, `/goal "<description>"`.
- **Tests**: 27 tests — evaluator parsing (5), mock provider evaluation (7), runner continuation (2), simulated runs (4), events (1), single step (2), command parsing (5).

## v0.71.0 — 2026-05-30

### Goal State & Event Foundation

First milestone of the `/goal` autonomous goal-driven loop feature.

- **goal-state.rkt**: Core data model with `goal-state` (11 fields), `goal-check`, `evaluation-result`, and `check-result` structs. Full serialization (hash round-trip) with JSON-safe symbol→string conversion. Contracts on all public constructors.
- **Typed events**: 6 goal events added to `session-events.rkt` — `goal.started`, `goal.turn.started`, `goal.evaluated`, `goal.check.completed`, `goal.achieved`, `goal.failed`.
- **JSONL persistence**: `append-goal-state!` and `load-latest-goal-state` in `session-store.rkt` for session-scoped goal state storage.
- **Context filtering**: `goal-state` kind excluded from context assembly in `session-walk.rkt`.
- **Tests**: 36 tests covering construction, contracts, serialization round-trip, backward compatibility, event construction, persistence, and context filtering.

## v0.70.13 — 2026-05-30

### Audit Hotfix Round 3 + GUI Bug Fixes

- Fix: README v0.70.3 status entry corrected (F1)
- Fix: USER fallback test relative path now works under `raco test` CWD (F2)
- Fix: Slash commands (/help, /status, /clear, etc.) now display immediately
  in GUI transcript — `add-system-msg!` calls `notify-gui!` callback (F3)
- Fix: Text selection in transcript no longer overwritten by streaming output
  or new messages — `insert-message-into-text!` uses explicit position (F4)

## v0.70.12 — 2026-05-30

### Audit Hotfix Round 2 — Closure of v0.70.11 Audit Findings

- Remove duplicate `racket/string` import in credential-backend (F1)
- Add `log-warning` to `drop-old` backpressure handler in trace-sink (F2)
- Fix 9 wrong v0.70.x status descriptions in README (F3)
- Apply `shell-escape` to Windows `cmdkey store!` for defense-in-depth (W1)
- Inline identity function `content-hash` in token-estimate-cache (W2)
- Improve USER fallback test coverage (W3)

## v0.70.11 — 2026-05-30

### Post-Audit Hotfix — Critical/Warning Finding Remediation

- **W0**: Credential backend security fixes
  - New `current-shell-command-runner` parameter for macOS/Windows/capability backends
  - Shell-escape applied to all macOS backend interpolated values (USER, provider, key)
  - Windows backend returns `#f` on load instead of sentinel string
  - Security warning for Windows cmdkey API key exposure
  - File permission race fix: set 0600 before writing content
  - macOS USER env var fallback chain (USER → LOGNAME → who-am-i → "unknown")
  - Fixed mock tests using wrong runner signature
- **W1**: Async sink + trace-logger + token cache fixes
  - `async-session-sink%` flush now propagates to inner sink
  - `trace-logger` closes file handle when async? is true, events routed through sink
  - `token-estimate-cache` uses string identity as key (fixes equal-hash-code collision risk)
  - Removed unused `racket/math` import
- **W2**: Contracts + lint + code quality
  - Added contracts to shell-risk main API functions
  - Fixed tautological string checks in `lint-credential-policy.rkt`
  - `shell-risk-summary` `critical?` now returns boolean
  - `drop-old` policy now logs warning instead of silently falling back to `drop-new`

## v0.70.10 — 2026-05-29

### Series Stabilization + Post-Series Audit

- **W0**: Gate truth lint fixes
  - Fixed `runtime/auth-store.rkt` line-too-long (174→150 chars)
  - Fixed CHANGELOG dash format (–→—) for v0.70.4–v0.70.9
  - Synced README metrics via `metrics.rkt --sync-all`
  - Synced README status block via `sync-readme-status.rkt --sync`
- **W1**: Updated `docs/reports/v0.70.0-audit-baseline.md` with series completion summary
- **W2**: Created `docs/reports/v0.70.10-post-series-audit.md` — full post-series audit report
- **Version bump**: 0.70.10, tag `v0.70.10`

## v0.70.9 — 2026-05-29

### Typed Racket Gradual Migration — Boundary Pilot

- **W0**: Audited 5 candidate modules; selected `util/event-access.rkt` as first target
- **W1**: Converted `util/event-access.rkt` to `#lang typed/racket`
  - Removed manual `racket/contract` boilerplate; TR boundary auto-generates contracts
  - All downstream consumers compile and pass (`agent/event-types.rkt`, `test-stream-error-wrapping.rkt`)
  - Zero contract churn, negligible compile-time impact
- **W2**: Decision — STOP after one successful conversion
  - Second candidate (`util/event-types.rkt`) too small for value
  - Next viable candidate (`util/tool-types.rkt`) re-exported by facade — excluded per risk decision
  - Pilot documented in `docs/reports/TYPED-RACKET-TARGETS-v0.70.9.md`

## v0.70.8 — 2026-05-29

### Large Test File Decomposition

- **W0**: Decomposed `test-cli.rkt` (1202 → 464 lines, 61% reduction)
  - Extracted `test-cli-format.rkt` — format-event-for-terminal (22 tests)
  - Extracted `test-cli-interactive.rkt` — run-cli-interactive prompts, slash commands, error handling (32 tests)
  - Extracted `test-cli-markdown.rkt` — markdown rendering + stream writer (15 tests)
- **W1**: Decomposed `test-gemini.rkt` (1148 → 632 lines, 45% reduction)
  - Extracted `test-gemini-stream.rkt` — stream parsing, single-event parsing, tool-call indices
  - Extracted `test-gemini-provider.rkt` — provider construction, HTTP status, API key validation, security
- **W2**: Verified test runner auto-discovers new subdirectories; version bump

## v0.70.7 — 2026-05-29

### Per-Tool Timeout Override

- **W0**: `timeout-seconds` field added to `tool` struct (10th positional field)
- **W0**: `make-tool` accepts `#:timeout-seconds` keyword; `tool-timeout-seconds` accessor
- **W0**: `tool->jsexpr` serializes `timeoutSeconds` when set
- **W1**: Scheduler `execute-single` injects per-tool timeout into args as `'timeout`
- **W1**: Bash tool respects injected timeout via existing `resolve-exec-limits`
- **W1**: User-provided timeout is not overwritten by tool default

## v0.70.6 — 2026-05-29

### Token Estimation Memoization

- **W0**: `util/token-estimate-cache.rkt` — content-addressed memoization for text token estimation
- **W0**: `cached-estimate-text-tokens` with hit/miss stats and `clear-token-estimate-cache!`
- **W0**: `make-token-estimate-cache` for isolated per-session caches
- **W1**: Replaced ad-hoc cache in `runtime/context-policy.rkt` with `token-estimate-cache.rkt`
- **W1**: Added `token-estimate-cache-hit-stats` for observability
- **W2**: Benchmark shows ~43% speedup for repeated token estimation (100 texts, 10 runs)

## v0.70.5 — 2026-05-29

### Opt-In Async Session/Event Sink Pilot

- **W0**: Session sink ordering characterization tests (file + in-memory sinks)
- **W0**: Hash-chain sequential linking verification
- **W0**: Pending marker cleanup after successful append
- **W1**: `async-session-sink%` class with worker thread and bounded capacity
- **W1**: Opt-in only — default remains sync; no production wiring change
- **W1**: `sink-flush!` blocks until queued writes complete; `sink-close!` stops worker
- **W1**: Integrity proof: `verify-hash-chain` passes after async appends

## v0.70.4 — 2026-05-29

### Async Trace/Event Logging

- **W0**: `async-trace-sink%` class with worker thread and bounded mailbox
- **W0**: Backpressure policies: `block` (default), `drop-new`, `drop-old` (fallback)
- **W0**: `trace-flush!` blocks until worker ack; `trace-close!` terminates worker gracefully
- **W1**: `#:async?` flag in `make-trace-logger` (default `#f` for backward compatibility)
- **W1**: `json-file-trace-sink%` for JSON-formatted trace output
- **W2**: Stress benchmark script `scripts/bench-trace-logger.rkt`

## v0.70.3 — 2026-05-29

### Structured Shell Risk Classification

- **W0**: Conservative shell tokenizer + structured risk classifier (`tools/shell-risk.rkt`)
- **W0**: Risk types: destructive, network-pipe, command-substitution, eval, exec, windows-destructive
- **W1**: Shadow-mode integration in `bash.rkt` — logs diagnostic when regex/classifier disagree
- **W2**: Feature flag `shell-risk-classifier` setting (`regex`, `structured`, `both`)
- **W2**: Default `regex` for backward compatibility

## v0.70.2 — 2026-05-29

### Cross-Platform Credential Backends + Windows Install


## v0.94.8 — Audit remediation, UX hardening, release closure

### Fixed
- **W1 Null-safety**: Guard all callback invocations in `ui-surface.rkt` with `when cb`; fix `ui-callbacks-installed?` to check all 10 fields
- **W2 Overlay + Timeout**: Add `DELTA-SHOW-OVERLAY`/`DELTA-DISMISS-OVERLAY` delta types; enforce render hook timeout via `racket/engine`
- **W3 Lifecycle + Contracts**: Export `current-gui-event-runtime`; add error isolation in `dispatch-gui-hook!`; use action name constants in `ui-delta.rkt`; add contracts on `ui-action->deltas`
- **W4 Schema + Widget**: Consume `ui-action-schema` for payload validation; validate `zone`/`kind` against enumerations in `widget-descriptor-valid?`; add `widget-descriptor-matches-lifecycle?`
- **W5 Layering + Nits**: Remove `tui/state.rkt` import from `ui-surface.rkt` (ARCH-02); add logging to `apply-render-hook-safe`; make `classify-layout-breakpoint` composable; fix `/` query in command filter
- **W6 Test Quality**: Replace manual save/restore with `parameterize`; strengthen parity assertions; add GUI adapter coverage for 7 delta types
- **W7 Integration Gates**: G0-G3 verification — 8 gate tests all pass
- **W9 Regression**: 381+ tests across 44 UI/GUI test files, zero failures

### Changed
- Version bumped to 0.94.8

### Audit Findings Addressed
- C-1: Overlay delta types (W2)
- C-2: Render hook timeout enforcement (W2)
- C-3: Null-safety guards (W1)
- H-1: Export `current-gui-event-runtime` (W3)
- H-2: `ui-make-styled-*` return sensible defaults (W1)
- H-3: Error isolation in `dispatch-gui-hook!` (W3)
- M-1: Contract on `ui-action->deltas` (W3)
- M-2: Consume `ui-action-schema` (W4)
- M-3: Action name constants (W3)
- M-5: Widget zone/kind validation (W4)
- m-1: Hook name validation (W3)
- m-2: `ui-callbacks-installed?` 10-field check (W1)
- m-3: Remove ARCH-02 layering violation (W5)
- m-6: Render hook error logging (W5)
- m-8: Lifecycle token consumer (W4)
- N-1..N-6: Schema comment, member idiom, filter fixes, composable breakpoints
- T-1..T-4: Test isolation, assertion quality, GUI adapter coverage

# Broad Fast-Suite Debt Triage

**Date**: 2026-06-15
**HEAD**: `537c99fe`
**Version**: 0.99.11
**Baseline source**: `docs/reports/v0.99.11-gate-evidence.json`

---

## Status

The broad fast suite (`racket scripts/run-tests.rkt --suite fast`) is **NOT GREEN**.

This is pre-existing unrelated debt — not caused by v0.99.10/v0.99.11 MAS Schritt 6 remediation. The focused MCP/capability/routing gates (160+ tests across 11 files) all pass cleanly.

All focused security gates are GREEN. The broad-suite failures are tracked here so they are visible to anyone cloning the repo.

---

## Current Baseline

```text
Files:     912 total, 835 passed, 77 failed, 0 timeouts
Tests:     11425 total, 11305 passed, 120 failed
Elapsed:   ~4-5 minutes (when completing within timeout)
Exit:      4
```

**Note**: When run under `timeout 1200` (as during the v0.99.10 wave gate), the suite may exit 124 (timeout) because mutation-sensitive tests in the suite call `sync-version --write`, reverting version surfaces and causing cascading failures. After post-suite `sync-version --write` restoration, focused gates pass.

### Zero-Parsed Sentinels (5 files)

These files fail to compile/run at all, producing 0 tests:

| File | Likely Cause |
|------|-------------|
| `tests/mock-worker.rkt` | Helper module not a test file |
| `tests/test-cursor-debug.rkt` | Compile/import error |
| `tests/test-event-ordering.rkt` | Compile/import error |
| `tests/test-vision-helpers.rkt` | Compile/import error |
| `tests/tui/test-error-scenarios.rkt` | Compile/import error |

---

## Failure Taxonomy (5 Clusters)

### Cluster A — Compile/import/zero-test failures (P0)

**~12-15 files.** Syntax errors, missing module paths, stale export names, incorrect runner invocations.

Representative files:
- `tests/test-agent-session-extensions.rkt`
- `tests/test-context-assembly-config.rkt`
- `tests/test-contracts.rkt`
- `tests/test-event-types.rkt`
- `tests/test-extension-tiers.rkt`
- `tests/test-gui-state-sync-w0.rkt`
- `tests/test-llm-error-visibility.rkt`
- `tests/test-runtime-packages.rkt`
- `tests/test-safe-mode.rkt`
- `tests/test-sdk-contracts.rkt`
- `tests/test-session-config-helpers.rkt`
- `tests/test-struct-mutability.rkt`

**Action**: Fix parser/import/export/runner failures before behavioral triage. These hide real signal.

### Cluster B — Constructor/API/schema drift (P1)

**~15-20 files.** Constructor arity mismatches, browser struct drift, event codec gaps.

Representative files:
- `tests/test-cli-builder.rkt`, `tests/test-cli-interactive.rkt`, `tests/test-cli.rkt`
- `tests/test-wiring-contracts.rkt`, `tests/test-wiring-run-modes.rkt`
- `tests/test-browser-adapter.rkt`, `tests/test-browser-audit*.rkt`
- `tests/test-event-json-round-trip.rkt`, `tests/test-event-roundtrip.rkt`
- `tests/test-model-command.rkt`, `tests/test-model-registry.rkt`
- `tests/tui/test-mouse-bridge.rkt`

**Action**: Update constructors/tests/shims by subsystem.

### Cluster C — Context assembly drift (P1)

**~6 files.** Semantic expectation drift from universal pinning and context policy changes.

Representative files:
- `tests/test-agent-session-context.rkt`
- `tests/test-context-assembly-raw.rkt`
- `tests/test-context-assembly-tree.rkt`
- `tests/test-gap5-conclusion-bridge.rkt`
- `tests/test-prompt-injection.rkt`
- `tests/test-working-set.rkt`

**Action**: For each test, decide intended behavior vs bug, update fixtures.

### Cluster D — TUI/tool event reducer drift (P2)

**~10-12 files.** Tool event state transitions and rendering expectations.

Representative files:
- `tests/test-gui-event-subscriber-characterization.rkt`
- `tests/test-tui-event-ordering.rkt`
- `tests/test-tui-render-loop.rkt`
- `tests/test-tui-tool-cycle.rkt`, `tests/test-tui-tool-*.rkt`
- `tests/tui/test-input.rkt`, `tests/tui/test-render.rkt`, `tests/tui/test-state.rkt`
- `tests/test-streaming-tool-bug.rkt`, `tests/test-streaming-transitions.rkt`

**Action**: Normalize event shapes and state updates before rendering assertions.

### Cluster E — Release hygiene/lint (P2)

**~7 files.** Stale version metadata, doc freshness, changelog lint, hotspot budgets.

Representative files:
- `tests/test-ci-local.rkt`
- `tests/test-arch-fitness.rkt`
- `tests/test-hotspot-report.rkt`
- `tests/test-lint-doc-freshness.rkt`
- `tests/test-lint-release-notes.rkt`
- `tests/test-util-reclassification.rkt`
- `tests/test-mutating-tool-taxonomy.rkt`

**Action**: Sync metadata, version refs, and ledgers.

---

## Recommended Recovery Plan

| Wave | Cluster | Priority | Goal | Acceptance |
|------|---------|----------|------|------------|
| A | Compile/import | P0 | Eliminate zero-test and compile/import failures | No 0/0 file failures |
| B | Constructor/schema | P1 | Restore constructor/contract alignment | CLI/browser/event/model clusters pass |
| C | Context semantics | P1 | Decide intended behavior for each test | Context tests pass or bugs filed |
| D | TUI/event-state | P2 | Normalize tool event state transitions | TUI/tool tests pass |
| E | Release hygiene | P2 | Clear lint/metadata noise | CI-local/lint tests pass |

Wave A should be done first — it unblocks trustworthy signal from the other clusters.

---

## Mutation-Sensitive Test Hazard

Several tests in the broad suite call `sync-version.rkt --write` or `bump-version.rkt`, which mutate `info.rkt` and `README.md`. This causes version-surface drift during the suite run. The hazard is:

1. Broad suite starts → mutation-sensitive tests revert version surfaces to old values
2. Version-dependent tests fail because surfaces are now stale
3. After suite exits, `info.rkt`/`README.md` are left in a stale state
4. Post-suite `racket scripts/sync-version.rkt --write` is required to restore correct state

**Mitigation**: These tests should use temporary directories or mocks instead of mutating real repo files. This is tracked as part of Cluster A cleanup.

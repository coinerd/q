# TEST-SUITE LEDGER — v0.99.30 W5

> **Current status addendum — v0.99.34 W2 (2026-06-20)**
>
> This file is historical. It documents how the v0.99.30 W5 known-failure
> ledger was created and how debt was retired in later waves. It is **not** the
> current source of known failures.
>
> The current machine-readable ledger is `tests/test-suite-ledger.json`; as of
> `main@d2565bc8` it contains `"entries": []`. Fresh v0.99.34 W1 local gates
> confirmed:
>
> - fast local: `948/948 files`, `12873/12873 tests`, PASS;
> - broad local with ledger: `1040/1040 files`, `13801/13801 tests`, PASS;
> - ledger summary: `Known=0`, `New=0`, `Unclassified=0`, `Release-blocking=0`.
>
> Historical entries below must not be read as active known failures. If broad
> failures reappear, update `tests/test-suite-ledger.json` and add a new dated
> report/addendum with command evidence.

This report documents the initial known-failure ledger for broad-suite debt.

## Source evidence

- Command: `timeout 900 racket scripts/run-tests.rkt --suite broad --json-out <tmp>`
- Result: runner wrote JSON and human summary, then exited `4` because strict zero-parsed detection flagged `tests/test-benchmarks.rkt`.
- JSON verdict before strict zero-parsed gate: `fail`
- Files: 1041 total, 958 passed, 83 failed, 0 timeouts
- Tests: 12590 total, 12512 passed, 78 failed
- Original categories: ASSERTION_FAILURE=79, ENVIRONMENT_MISSING=1, MODULE_LOAD_FAILURE=2, PASS=957, UNKNOWN_FAILURE=1, ZERO_PARSED=1
- Addendum: `tests/test-tool-edit-builtin.rkt` passed individually (`raco test`) but failed during the broad `--ledger` rerun; it is ledgered as known broad-order/mutation-sensitive debt.
- Ledger entries created: 84

## Policy

- Known failures are matched by both `file` and normalized `category`.
- New or category-changed failures are release-blocking until triaged into the ledger or fixed.
- Known non-release-blocking failures remain visible; they are not converted to PASS.
- Resolved known failures are reported so stale ledger entries can be removed.
- Strict zero-parsed files remain blocking outside the known-failure ledger.

## Owner summary

- architecture: 2
- browser: 7
- core: 20
- event-loop: 9
- extensions: 3
- llm: 7
- release-engineering: 6
- runtime: 7
- sandbox: 2
- security: 2
- tools: 1
- tui: 14
- workflows: 4

## Category summary

- ASSERTION_FAILURE: 80
- ENVIRONMENT_MISSING: 1
- MODULE_LOAD_FAILURE: 2
- UNKNOWN_FAILURE: 1

## Entries

- `tests/test-agent-session-context.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-anthropic.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-arch-fitness.rkt` — ASSERTION_FAILURE — owner `architecture` — issue #8313 — release_blocking=false
- `tests/test-benchmark-scorer.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-browser-adapter.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-audit-w1-v0983.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-audit-w2.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-audit.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-mock-adapter.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-robustness-w1.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-browser-settings.rkt` — ASSERTION_FAILURE — owner `browser` — issue #8313 — release_blocking=false
- `tests/test-bump-version.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-cell-diff-render.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-check-deps.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-ci-local.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-context-assembly-raw.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-context-assembly-tree.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-doctor.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-effect-update-fsm-contract.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-event-json-round-trip.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-event-roundtrip.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-event-types.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-export-formats.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-facade-surface.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-gap5-conclusion-bridge.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-goal-evidence.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-goal-runner.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-gui-event-subscriber-characterization.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-gui-state-sync-w0.rkt` — MODULE_LOAD_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-hook-expansion.rkt` — ASSERTION_FAILURE — owner `extensions` — issue #8313 — release_blocking=false
- `tests/test-hooks-complete.rkt` — ASSERTION_FAILURE — owner `extensions` — issue #8313 — release_blocking=false
- `tests/test-hotspot-report.rkt` — ASSERTION_FAILURE — owner `architecture` — issue #8313 — release_blocking=false
- `tests/test-integration.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-interfaces-tui.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-layout-policy.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-lazy-context-assembly.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-lint-doc-freshness.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-lint-release-notes.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-llm-error-visibility.rkt` — MODULE_LOAD_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-llm-model.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-loop-cancellation.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-loop-edge-cases.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-loop-events.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-main.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-mcp-config.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-memory-policy.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-model-command.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-model-registry.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-pre-commit.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-prompt-injection.rkt` — ASSERTION_FAILURE — owner `security` — issue #8313 — release_blocking=false
- `tests/test-run-tests-arg-validation.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-run-tests.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-safemode-enforcement.rkt` — ASSERTION_FAILURE — owner `security` — issue #8313 — release_blocking=false
- `tests/test-self-hosting-deep.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-self-hosting-workflow.rkt` — ASSERTION_FAILURE — owner `extensions` — issue #8313 — release_blocking=false
- `tests/test-session-config-helpers.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/test-settings.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-skeleton.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-stream-loop-w1.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-streaming-tool-bug.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-streaming-transitions.rkt` — ASSERTION_FAILURE — owner `event-loop` — issue #8313 — release_blocking=false
- `tests/test-struct-mutability.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-subprocess-edge-cases.rkt` — ASSERTION_FAILURE — owner `sandbox` — issue #8313 — release_blocking=false
- `tests/test-subprocess.rkt` — ASSERTION_FAILURE — owner `sandbox` — issue #8313 — release_blocking=false
- `tests/test-summary-integration.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-sync-readme-status.rkt` — ASSERTION_FAILURE — owner `release-engineering` — issue #8313 — release_blocking=false
- `tests/test-tui-event-ordering.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-tui-tool-cycle.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-tui-tool-failure-streaming.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-tui-tool-sequences.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-tui-workflow-harness.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-typed-model.rkt` — ASSERTION_FAILURE — owner `llm` — issue #8313 — release_blocking=false
- `tests/test-ui-actions.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-util-reclassification.rkt` — ASSERTION_FAILURE — owner `core` — issue #8313 — release_blocking=false
- `tests/test-widget-api.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/test-working-set.rkt` — ASSERTION_FAILURE — owner `runtime` — issue #8313 — release_blocking=false
- `tests/tui/test-command-integration.rkt` — ENVIRONMENT_MISSING — owner `tui` — issue #8313 — release_blocking=false
- `tests/tui/test-render.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/tui/test-state.rkt` — ASSERTION_FAILURE — owner `tui` — issue #8313 — release_blocking=false
- `tests/workflows/cli/test-cli-resume.rkt` — ASSERTION_FAILURE — owner `workflows` — issue #8313 — release_blocking=false
- `tests/workflows/cli/test-cli-single-shot.rkt` — ASSERTION_FAILURE — owner `workflows` — issue #8313 — release_blocking=false
- `tests/workflows/parity/test-sdk-cli-parity.rkt` — ASSERTION_FAILURE — owner `workflows` — issue #8313 — release_blocking=false
- `tests/workflows/tools/test-tool-bash-workflow.rkt` — UNKNOWN_FAILURE — owner `workflows` — issue #8313 — release_blocking=false
- `tests/test-tool-edit-builtin.rkt` — ASSERTION_FAILURE — owner `tools` — issue #8313 — release_blocking=false

## W5 acceptance verification

Command:

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --ledger tests/test-suite-ledger.json
```

Observed result:

```text
broad-ledger2-exit=4
Files: 1041 total, 959 passed, 82 failed, 0 timeouts
Category: PASS=958, ASSERTION_FAILURE=78, MODULE_LOAD_FAILURE=2, ENVIRONMENT_MISSING=1, ZERO_PARSED=1, UNKNOWN_FAILURE=1
VERDICT: ❌ FAIL
Known failures: 82
New failures: 0
Unclassified failures: 0
Resolved known failures: 2
Release-blocking known failures: 0
STRICT MODE: files with zero parsed tests: tests/test-benchmarks.rkt
```

Interpretation: W5 ledger classification is functioning; current broad failures are classified as known, no new/unclassified failures remain in this run. The command still exits `4` because strict zero-parsed detection remains correctly blocking for `tests/test-benchmarks.rkt` and is outside the known-failure ledger mechanism.

## W7 deterministic-failure remediation addendum

W7 (#8315) resolved a bounded deterministic batch from the broad ledger:

- `tests/test-agent-session-context.rkt`
- `tests/test-context-assembly-tree.rkt`
- `tests/test-event-roundtrip.rkt`
- `tests/test-gap5-conclusion-bridge.rkt`
- `tests/test-util-reclassification.rkt`

At W7 time, the entries remained in `tests/test-suite-ledger.json` as historical known failures with `issue: "#8315"`; when these files passed, the runner reported them under `Resolved known failures` rather than known/new/unclassified failures. This is now historical: the current ledger has no active entries.

W7 also fixed the strict zero-parsed sentinel for `tests/test-benchmarks.rkt` by adding an explicit RackUnit text-ui runner. Focused runner verification:

```bash
racket scripts/run-tests.rkt --ledger tests/test-suite-ledger.json \
  tests/test-agent-session-context.rkt \
  tests/test-context-assembly-tree.rkt \
  tests/test-event-roundtrip.rkt \
  tests/test-gap5-conclusion-bridge.rkt \
  tests/test-util-reclassification.rkt \
  tests/test-benchmarks.rkt
```

Observed result:

```text
Files: 6 total, 6 passed, 0 failed, 0 timeouts
Tests: 83 total, 83 passed, 0 failed
Category: PASS=6
VERDICT: PASS
Known failures: 0
New failures: 0
Unclassified failures: 0
Resolved known failures: 84
Release-blocking known failures: 0
```

Final W7 broad ledger verification:

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --profile local --ledger tests/test-suite-ledger.json
```

Observed result:

```text
w7-broad-ledger3-exit=1
profile=local
Files: 1042 total, 965 passed, 77 failed, 0 timeouts
Tests: 12617 total, 12547 passed, 70 failed
Category: PASS=965, ASSERTION_FAILURE=73, MODULE_LOAD_FAILURE=2, ENVIRONMENT_MISSING=1, UNKNOWN_FAILURE=1
VERDICT: FAIL
Known failures: 77
New failures: 0
Unclassified failures: 0
Resolved known failures: 7
Release-blocking known failures: 0
```

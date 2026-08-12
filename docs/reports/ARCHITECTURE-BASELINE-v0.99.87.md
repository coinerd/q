# Architecture Baseline

Pinned revision: `274de500e86666b54c10bacda09e99d006bd5501`

History limit: 200 commits

## Summary

- tracked-rkt-files: 2070
- source-modules: 798
- test-files: 1272
- source-lines: 135351
- test-lines: 219199
- provide-specs: 9348
- dependency-edges: 7054
- test-cases: 16822
- checks: 34350
- policy-exceptions: 6
- co-change-threshold: 3
- co-change-pairs-at-threshold: 76
- co-change-pairs-reported: 76

## Published metric reconciliation

| Metric | README | Measured | Status |
|---|---:|---:|---|
| test-files | 1272 | 1272 | MATCH |
| source-modules | 798 | 798 | MATCH |
| source-lines | 135351 | 135351 | MATCH |
| test-lines | 219199 | 219199 | MATCH |
| checks | 34350 | 34350 | MATCH |

### Composition-root fan-out vs policy

| Path | Policy | Measured | Status |
|---|---:|---:|---|
| `runtime/agent-session.rkt` | 33 | 36 | DRIFT |
| `tui/tui-render-loop.rkt` | 29 | 27 | DRIFT |
| `agent/loop.rkt` | 27 | 23 | DRIFT |
| `runtime/session/session-lifecycle.rkt` | 26 | 39 | DRIFT |
| `agent/iteration/main-loop.rkt` | 25 | 20 | DRIFT |
| `runtime/turn-orchestrator.rkt` | 22 | 30 | DRIFT |

### Provide-risk notes vs measured provides

| Path | Recorded | Measured | Status |
|---|---:|---:|---|
| `runtime/agent-session.rkt` | 0 | 46 | DRIFT |
| `runtime/settings.rkt` | 221 | 71 | DRIFT |
| `runtime/session/session-lifecycle.rkt` | 0 | 11 | DRIFT |
| `scripts/run-tests.rkt` | 0 | 78 | DRIFT |
| `llm/openai-compatible.rkt` | 0 | 16 | DRIFT |
| `sandbox/subprocess.rkt` | 0 | 23 | DRIFT |
| `tui/tui-render-loop.rkt` | 0 | 33 | DRIFT |
| `tui/tui-init.rkt` | 0 | 9 | DRIFT |
| `runtime/session/session-store-tree.rkt` | 0 | 8 | DRIFT |
| `extensions/gsd/core.rkt` | 0 | 22 | DRIFT |
| `tui/commands.rkt` | 0 | 18 | DRIFT |
| `tui/state-events.rkt` | 0 | 7 | DRIFT |
| `tui/terminal-input.rkt` | 0 | 48 | DRIFT |
| `interfaces/sessions.rkt` | 0 | 12 | DRIFT |
| `llm/gemini.rkt` | 0 | 8 | DRIFT |
| `tools/scheduler.rkt` | 0 | 48 | DRIFT |
| `runtime/turn-orchestrator.rkt` | 0 | 4 | DRIFT |
| `runtime/tool-coordinator.rkt` | 0 | 15 | DRIFT |
| `llm/stream.rkt` | 0 | 31 | DRIFT |
| `llm/anthropic.rkt` | 0 | 6 | DRIFT |
| `tui/terminal.rkt` | 0 | 83 | DRIFT |
| `tui/state-ui.rkt` | 0 | 50 | DRIFT |
| `wiring/run-modes.rkt` | 0 | 14 | DRIFT |
| `tools/builtins/spawn-subagent.rkt` | 0 | 1 | DRIFT |
| `runtime/context-assembly/state-aware-builder.rkt` | 0 | 11 | DRIFT |
| `extensions/gsd/state-machine.rkt` | 0 | 76 | DRIFT |
| `runtime/settings-query.rkt` | 226 | 53 | DRIFT |
| `tui/state-types.rkt` | 358 | 121 | DRIFT |

Policy exceptions are read from the pinned dependency policy; Part-I claims are reconciled below with live code evidence.

## Top module hotspots

Showing 100 of 2070 tracked Racket modules; raw evidence contains all modules.

| Path | LOC | Provides | Fan-in | Fan-out | Changes | Hotspot |
|---|---:|---:|---:|---:|---:|---:|
| `runtime/session/session-lifecycle.rkt` | 600 | 11 | 9 | 39 | 14 | 8400 |
| `llm/openai-compatible.rkt` | 513 | 16 | 23 | 8 | 12 | 6156 |
| `runtime/context-assembly/turn-context.rkt` | 580 | 5 | 5 | 21 | 10 | 5800 |
| `llm/stream.rkt` | 537 | 31 | 29 | 1 | 10 | 5370 |
| `runtime/context-assembly/state-aware-builder.rkt` | 469 | 11 | 22 | 14 | 10 | 4690 |
| `runtime/context-assembly/rollback-actions.rkt` | 482 | 50 | 21 | 0 | 9 | 4338 |
| `tests/test-rollback-isolation.rkt` | 681 | 0 | 0 | 8 | 6 | 4086 |
| `runtime/turn-orchestrator.rkt` | 432 | 4 | 8 | 30 | 9 | 3888 |
| `runtime/iteration/step-executor.rkt` | 619 | 8 | 7 | 24 | 6 | 3714 |
| `sandbox/worker-tools.rkt` | 695 | 12 | 6 | 7 | 5 | 3475 |
| `tests/test-auto-retry.rkt` | 865 | 0 | 0 | 4 | 4 | 3460 |
| `tests/test-arch-fitness.rkt` | 838 | 0 | 0 | 1 | 4 | 3352 |
| `runtime/auto-retry.rkt` | 541 | 46 | 18 | 2 | 6 | 3246 |
| `tests/test-stream.rkt` | 797 | 0 | 0 | 8 | 4 | 3188 |
| `runtime/agent-session.rkt` | 636 | 46 | 54 | 36 | 5 | 3180 |
| `agent/iteration/main-loop.rkt` | 287 | 2 | 4 | 20 | 10 | 2870 |
| `tests/test-openai-compatible.rkt` | 555 | 0 | 0 | 6 | 5 | 2775 |
| `extensions/gsd/go-orchestrator.rkt` | 426 | 24 | 8 | 7 | 6 | 2556 |
| `tests/test-release-workflow-contract.rkt` | 352 | 0 | 0 | 0 | 7 | 2464 |
| `tests/test-iteration-transitions.rkt` | 366 | 0 | 0 | 14 | 6 | 2196 |
| `runtime/tool-coordinator.rkt` | 546 | 15 | 9 | 20 | 4 | 2184 |
| `llm/gemini.rkt` | 537 | 8 | 10 | 9 | 4 | 2148 |
| `runtime/session/session-events.rkt` | 537 | 6 | 12 | 21 | 4 | 2148 |
| `tools/builtins/spawn-coordinator.rkt` | 509 | 30 | 2 | 33 | 4 | 2036 |
| `tools/builtins/edit.rkt` | 396 | 7 | 15 | 10 | 5 | 1980 |
| `agent/loop.rkt` | 214 | 9 | 12 | 23 | 9 | 1926 |
| `sandbox/subprocess.rkt` | 476 | 23 | 9 | 3 | 4 | 1904 |
| `tests/test-anthropic.rkt` | 946 | 0 | 0 | 6 | 2 | 1892 |
| `tests/test-main.rkt` | 931 | 0 | 0 | 9 | 2 | 1862 |
| `tui/commands.rkt` | 567 | 18 | 8 | 23 | 3 | 1701 |
| `extensions/gsd/command-handlers.rkt` | 531 | 7 | 4 | 25 | 3 | 1593 |
| `tools/scheduler-execution.rkt` | 395 | 14 | 2 | 11 | 4 | 1580 |
| `tests/test-hooks-complete.rkt` | 759 | 0 | 0 | 17 | 2 | 1518 |
| `extensions/gsd/campaign-state.rkt` | 493 | 51 | 8 | 2 | 3 | 1479 |
| `runtime/session/session-config.rkt` | 352 | 49 | 59 | 13 | 4 | 1408 |
| `tools/builtins/spawn-execution-plan.rkt` | 701 | 41 | 8 | 6 | 2 | 1402 |
| `agent/loop-stream.rkt` | 349 | 7 | 11 | 17 | 4 | 1396 |
| `tests/test-session-index.rkt` | 696 | 0 | 0 | 3 | 2 | 1392 |
| `tests/test-memory-continuity-w1.rkt` | 270 | 0 | 0 | 7 | 5 | 1350 |
| `runtime/session/session-types.rkt` | 334 | 59 | 46 | 1 | 4 | 1336 |
| `tui/state-events/core-handlers.rkt` | 438 | 4 | 7 | 9 | 3 | 1314 |
| `tests/test-gsd-go-orchestrator.rkt` | 428 | 0 | 0 | 3 | 3 | 1284 |
| `tests/test-gsd-archive.rkt` | 631 | 0 | 0 | 5 | 2 | 1262 |
| `tests/test-worker-security.rkt` | 406 | 0 | 0 | 4 | 3 | 1218 |
| `runtime/goal/goal-runner.rkt` | 605 | 12 | 9 | 10 | 2 | 1210 |
| `runtime/provider-retry.rkt` | 200 | 2 | 3 | 11 | 6 | 1200 |
| `tests/test-stream-error-wrapping.rkt` | 399 | 0 | 0 | 13 | 3 | 1197 |
| `scripts/lint-release-readiness.rkt` | 389 | 0 | 0 | 0 | 3 | 1167 |
| `agent/stream-runner.rkt` | 289 | 2 | 4 | 15 | 4 | 1156 |
| `tui/commands/runtime-control.rkt` | 385 | 11 | 5 | 11 | 3 | 1155 |
| `runtime/working-set.rkt` | 383 | 28 | 32 | 4 | 3 | 1149 |
| `tests/test-arch-boundaries.rkt` | 362 | 0 | 0 | 1 | 3 | 1086 |
| `runtime/settings-query.rkt` | 542 | 53 | 21 | 2 | 2 | 1084 |
| `tests/test-gsd-planning.rkt` | 1054 | 0 | 0 | 13 | 1 | 1054 |
| `tests/test-release-manifest-traceability.rkt` | 526 | 0 | 0 | 1 | 2 | 1052 |
| `tools/tool.rkt` | 262 | 66 | 246 | 12 | 4 | 1048 |
| `tests/test-rollback-session-ownership.rkt` | 341 | 0 | 0 | 4 | 3 | 1023 |
| `tests/test-state-aware-builder.rkt` | 506 | 0 | 0 | 17 | 2 | 1012 |
| `tui/tui-render-loop.rkt` | 495 | 33 | 10 | 27 | 2 | 990 |
| `wiring/run-modes/cli.rkt` | 465 | 1 | 1 | 45 | 2 | 930 |
| `scripts/abstraction-analysis.rkt` | 449 | 46 | 2 | 0 | 2 | 898 |
| `agent/iteration/loop-config.rkt` | 148 | 24 | 5 | 8 | 6 | 888 |
| `tests/test-session-store.rkt` | 886 | 0 | 0 | 4 | 1 | 886 |
| `runtime/context-assembly/memory-builder.rkt` | 432 | 21 | 15 | 5 | 2 | 864 |
| `runtime/session-index/mutations.rkt` | 288 | 15 | 4 | 11 | 3 | 864 |
| `agent/loop-dispatch.rkt` | 123 | 1 | 3 | 16 | 7 | 861 |
| `tests/test-spawn-batch-timeout.rkt` | 281 | 0 | 0 | 10 | 3 | 843 |
| `gui/slash-commands.rkt` | 397 | 4 | 4 | 13 | 2 | 794 |
| `tests/test-scheduler-hooks.rkt` | 397 | 0 | 0 | 6 | 2 | 794 |
| `tests/test-gsd-campaign-state.rkt` | 390 | 0 | 0 | 2 | 2 | 780 |
| `tests/test-session-config.rkt` | 386 | 0 | 0 | 5 | 2 | 772 |
| `tests/test-ws-evolution.rkt` | 371 | 0 | 0 | 4 | 2 | 742 |
| `tests/test-execution-plane-e2e.rkt` | 361 | 0 | 0 | 9 | 2 | 722 |
| `agent/effect-executor.rkt` | 143 | 2 | 2 | 11 | 5 | 715 |
| `tests/test-run-tests.rkt` | 711 | 0 | 0 | 0 | 1 | 711 |
| `runtime/context-assembly/state-aware-helpers.rkt` | 234 | 8 | 4 | 5 | 3 | 702 |
| `tools/builtins/bash.rkt` | 341 | 17 | 13 | 9 | 2 | 682 |
| `runtime/context-assembly/operational-checkpoint.rkt` | 226 | 27 | 3 | 2 | 3 | 678 |
| `tools/registry-table/skill-tools.rkt` | 339 | 1 | 3 | 9 | 2 | 678 |
| `tests/test-agent-session-basic.rkt` | 677 | 0 | 0 | 16 | 1 | 677 |
| `scripts/run-tests/classify-filters.rkt` | 327 | 23 | 1 | 1 | 2 | 654 |
| `tui/input/state-types.rkt` | 322 | 35 | 5 | 3 | 2 | 644 |
| `tests/test-gemini.rkt` | 641 | 0 | 0 | 6 | 1 | 641 |
| `tests/test-iteration-observability.rkt` | 314 | 0 | 0 | 17 | 2 | 628 |
| `tests/test-scheduler.rkt` | 622 | 0 | 0 | 7 | 1 | 622 |
| `tests/reproducers/reproduce-gsd-go-replanning.rkt` | 206 | 0 | 0 | 0 | 3 | 618 |
| `tests/test-context-assembly-integration.rkt` | 308 | 0 | 0 | 14 | 2 | 616 |
| `extensions/gsd/archive.rkt` | 306 | 9 | 5 | 7 | 2 | 612 |
| `tests/test-fsm-property.rkt` | 304 | 0 | 0 | 2 | 2 | 608 |
| `runtime/context/context-policy.rkt` | 303 | 16 | 12 | 6 | 2 | 606 |
| `tests/test-audit-v09945-w4-mas.rkt` | 587 | 0 | 0 | 7 | 1 | 587 |
| `tests/test-iteration-integration.rkt` | 292 | 0 | 0 | 14 | 2 | 584 |
| `tests/test-audit-v09945-w5-tools.rkt` | 572 | 0 | 0 | 5 | 1 | 572 |
| `tests/test-lint-release-readiness.rkt` | 284 | 0 | 0 | 0 | 2 | 568 |
| `agent/event-structs.rkt` | 563 | 459 | 10 | 10 | 1 | 563 |
| `wiring/run-modes/loop.rkt` | 186 | 2 | 2 | 16 | 3 | 558 |
| `runtime/task-memory/projection.rkt` | 278 | 34 | 5 | 1 | 2 | 556 |
| `tests/test-scheduler-failure-symmetry.rkt` | 278 | 0 | 0 | 4 | 2 | 556 |
| `tui/state-types.rkt` | 546 | 121 | 44 | 3 | 1 | 546 |
| `runtime/session/session-store-goal-task.rkt` | 272 | 12 | 7 | 7 | 2 | 544 |

## Co-change evidence

Release-only commits and exact R100/C100 moves are excluded.

| Count | Path A | Path B |
|---:|---|---|
| 6 | `agent/iteration/loop-config.rkt` | `agent/iteration/main-loop.rkt` |
| 6 | `agent/loop-dispatch.rkt` | `agent/loop.rkt` |
| 5 | `agent/iteration/main-loop.rkt` | `runtime/session/session-lifecycle.rkt` |
| 5 | `agent/iteration/main-loop.rkt` | `tests/test-agent-iteration-di.rkt` |
| 5 | `llm/openai-compatible.rkt` | `llm/stream.rkt` |
| 5 | `runtime/context-assembly/rollback-actions.rkt` | `tests/test-rollback-isolation.rkt` |
| 5 | `runtime/context-assembly/state-aware-builder.rkt` | `runtime/context-assembly/turn-context.rkt` |
| 5 | `runtime/context-assembly/state-aware-builder.rkt` | `tests/test-rollback-isolation.rkt` |
| 4 | `agent/iteration/loop-config.rkt` | `runtime/session/session-lifecycle.rkt` |
| 4 | `agent/iteration/loop-config.rkt` | `tests/helpers/iteration-loop.rkt` |
| 4 | `agent/iteration/loop-config.rkt` | `tests/test-agent-iteration-di.rkt` |
| 4 | `agent/iteration/main-loop.rkt` | `runtime/iteration/step-executor.rkt` |
| 4 | `agent/iteration/main-loop.rkt` | `tests/helpers/iteration-loop.rkt` |
| 4 | `agent/iteration/main-loop.rkt` | `tests/test-iteration-transitions.rkt` |
| 4 | `llm/gemini.rkt` | `llm/openai-compatible.rkt` |
| 4 | `llm/openai-compatible.rkt` | `tests/test-openai-compatible.rkt` |
| 4 | `llm/openai-compatible.rkt` | `tests/test-stream.rkt` |
| 4 | `llm/stream.rkt` | `tests/test-stream.rkt` |
| 4 | `runtime/agent-session.rkt` | `runtime/session/lifecycle-state.rkt` |
| 4 | `runtime/agent-session.rkt` | `runtime/session/session-types.rkt` |
| 4 | `runtime/agent-session.rkt` | `tests/test-agent-session-pure.rkt` |
| 4 | `runtime/agent-session.rkt` | `tests/test-session-fsm.rkt` |
| 4 | `runtime/auto-retry.rkt` | `tests/test-auto-retry.rkt` |
| 4 | `runtime/context-assembly/rollback-actions.rkt` | `runtime/context-assembly/state-aware-builder.rkt` |
| 4 | `runtime/context-assembly/turn-context.rkt` | `runtime/turn-orchestrator.rkt` |
| 4 | `runtime/session/lifecycle-state.rkt` | `runtime/session/session-types.rkt` |
| 4 | `runtime/session/session-lifecycle.rkt` | `tests/helpers/iteration-loop.rkt` |
| 4 | `runtime/session/session-lifecycle.rkt` | `tests/test-agent-iteration-di.rkt` |
| 4 | `runtime/turn-orchestrator.rkt` | `tests/test-turn-context-assembly.rkt` |
| 4 | `tests/helpers/iteration-loop.rkt` | `tests/test-agent-iteration-di.rkt` |
| 4 | `tests/test-agent-session-pure.rkt` | `tests/test-session-fsm.rkt` |
| 3 | `agent/effect-executor.rkt` | `agent/loop-dispatch.rkt` |
| 3 | `agent/iteration/loop-config.rkt` | `agent/iteration/loop-phases.rkt` |
| 3 | `agent/iteration/loop-config.rkt` | `agent/iteration/tool-turn-bridge.rkt` |
| 3 | `agent/iteration/loop-config.rkt` | `runtime/iteration/step-executor.rkt` |
| 3 | `agent/iteration/loop-config.rkt` | `tests/test-arch-boundaries.rkt` |
| 3 | `agent/iteration/loop-config.rkt` | `tests/test-iteration-transitions.rkt` |
| 3 | `agent/iteration/loop-phases.rkt` | `agent/iteration/main-loop.rkt` |
| 3 | `agent/iteration/loop-phases.rkt` | `agent/iteration/tool-turn-bridge.rkt` |
| 3 | `agent/iteration/loop-phases.rkt` | `runtime/iteration/step-executor.rkt` |
| 3 | `agent/iteration/loop-state.rkt` | `agent/iteration/main-loop.rkt` |
| 3 | `agent/iteration/main-loop.rkt` | `agent/iteration/tool-turn-bridge.rkt` |
| 3 | `agent/iteration/main-loop.rkt` | `agent/state.rkt` |
| 3 | `agent/iteration/main-loop.rkt` | `runtime/iteration/decision.rkt` |
| 3 | `agent/iteration/main-loop.rkt` | `runtime/tool-coordinator.rkt` |
| 3 | `agent/iteration/main-loop.rkt` | `tests/test-arch-boundaries.rkt` |
| 3 | `agent/iteration/tool-turn-bridge.rkt` | `runtime/iteration/step-executor.rkt` |
| 3 | `agent/loop-dispatch.rkt` | `runtime/session/session-lifecycle.rkt` |
| 3 | `agent/loop-fsm.rkt` | `agent/loop.rkt` |
| 3 | `agent/loop.rkt` | `runtime/session/session-lifecycle.rkt` |
| 3 | `agent/state.rkt` | `runtime/turn-orchestrator.rkt` |
| 3 | `extensions/gsd/go-orchestrator.rkt` | `tests/test-gsd-go-orchestrator.rkt` |
| 3 | `llm/anthropic.rkt` | `llm/gemini.rkt` |
| 3 | `llm/anthropic.rkt` | `llm/openai-compatible.rkt` |
| 3 | `llm/anthropic.rkt` | `llm/stream.rkt` |
| 3 | `llm/azure-openai.rkt` | `llm/openai-compatible.rkt` |
| 3 | `llm/azure-openai.rkt` | `llm/stream.rkt` |
| 3 | `llm/azure-openai.rkt` | `tests/test-stream.rkt` |
| 3 | `llm/openai-compatible.rkt` | `runtime/session/session-lifecycle.rkt` |
| 3 | `runtime/context-assembly/rollback-actions.rkt` | `runtime/context-assembly/state-aware-helpers.rkt` |
| 3 | `runtime/context-assembly/rollback-actions.rkt` | `runtime/context-assembly/turn-context.rkt` |
| 3 | `runtime/context-assembly/state-aware-builder.rkt` | `runtime/turn-orchestrator.rkt` |
| 3 | `runtime/context-assembly/state-aware-helpers.rkt` | `tests/test-rollback-isolation.rkt` |
| 3 | `runtime/context-assembly/turn-context.rkt` | `runtime/session/session-events.rkt` |
| 3 | `runtime/context-assembly/turn-context.rkt` | `tests/test-rollback-isolation.rkt` |
| 3 | `runtime/iteration/step-executor.rkt` | `tests/test-agent-iteration-di.rkt` |
| 3 | `runtime/iteration/step-executor.rkt` | `tests/test-memory-continuity-w1.rkt` |
| 3 | `runtime/provider-retry.rkt` | `runtime/turn-orchestrator.rkt` |
| 3 | `runtime/session/lifecycle-state.rkt` | `tests/test-agent-session-pure.rkt` |
| 3 | `runtime/session/lifecycle-state.rkt` | `tests/test-session-fsm.rkt` |
| 3 | `runtime/session/session-lifecycle.rkt` | `tests/test-arch-fitness.rkt` |
| 3 | `runtime/session/session-types.rkt` | `tests/test-agent-session-pure.rkt` |
| 3 | `runtime/session/session-types.rkt` | `tests/test-session-fsm.rkt` |
| 3 | `runtime/working-set.rkt` | `tests/test-working-set-budget.rkt` |
| 3 | `sandbox/worker-tools.rkt` | `tests/test-worker-security.rkt` |
| 3 | `tests/test-spawn-batch-timeout.rkt` | `tools/builtins/spawn-coordinator.rkt` |

## Policy exceptions

| Layer | File | Owner | Lifecycle | Revisit | Rationale |
|---|---|---|---|---|---|
| extensions | `context.rkt` | extensions | DATED | 2026-10-01 | imports runtime/session-types.rkt for context assembly (bidirectional — fragile) |
| extensions | `dialog-api.rkt` | tui | DATED | 2026-10-01 | TUI dialog interface |
| extensions | `ext-package-manager.rkt` | extensions | DATED | 2026-10-01 | imports runtime/ for package lifecycle management (bidirectional — fragile) |
| extensions | `ui-surface.rkt` | tui | DATED | 2026-10-01 | TUI UI surface interface |
| extensions | `widget-lifecycle.rkt` | extensions | DATED | 2026-10-01 | Imports tui/component.rkt for q-component? type and make-q-component bridge |
| runtime | `layer-adapters.rkt` | runtime | PERMANENT | — | explicit adapter facade routing tool/extension deps behind contained boundary |

## Largest test files

Showing 100 of 1272 tracked test files; raw evidence contains all tests.

| Path | LOC | Test cases | Checks |
|---|---:|---:|---:|
| `tests/helpers/tmux-q-harness.rkt` | 2008 | 0 | 0 |
| `tests/test-tmux-q-harness.rkt` | 1397 | 116 | 292 |
| `tests/test-interfaces-tui.rkt` | 1133 | 105 | 179 |
| `tests/test-sdk.rkt` | 1082 | 67 | 129 |
| `tests/test-golden-flows.rkt` | 1081 | 55 | 129 |
| `tests/test-gsd-planning.rkt` | 1054 | 94 | 205 |
| `tests/tui/test-state.rkt` | 1052 | 95 | 225 |
| `tests/test-audit-v09945-w8-tui.rkt` | 1036 | 100 | 333 |
| `tests/tui/test-render.rkt` | 1012 | 99 | 193 |
| `tests/test-anthropic.rkt` | 946 | 26 | 162 |
| `tests/test-main.rkt` | 931 | 80 | 127 |
| `tests/test-session-store.rkt` | 886 | 60 | 181 |
| `tests/test-auto-retry.rkt` | 865 | 76 | 189 |
| `tests/test-replay.rkt` | 863 | 24 | 47 |
| `tests/test-arch-fitness.rkt` | 838 | 57 | 81 |
| `tests/test-audit-v09945-w9-extension.rkt` | 802 | 72 | 194 |
| `tests/test-stream.rkt` | 797 | 29 | 130 |
| `tests/tui/test-input.rkt` | 773 | 97 | 226 |
| `tests/test-hooks-complete.rkt` | 759 | 29 | 61 |
| `tests/test-run-tests.rkt` | 711 | 72 | 160 |
| `tests/test-session-index.rkt` | 696 | 46 | 104 |
| `tests/test-rollback-isolation.rkt` | 681 | 57 | 112 |
| `tests/test-agent-session-basic.rkt` | 677 | 18 | 87 |
| `tests/test-compactor.rkt` | 674 | 38 | 123 |
| `tests/test-abstraction-audit.rkt` | 668 | 80 | 172 |
| `tests/test-audit-v09945-w7-creds.rkt` | 654 | 77 | 169 |
| `tests/test-gemini.rkt` | 641 | 4 | 116 |
| `tests/test-gsd-archive.rkt` | 631 | 31 | 70 |
| `tests/test-settings.rkt` | 629 | 73 | 103 |
| `tests/test-scheduler.rkt` | 622 | 29 | 80 |
| `tests/test-credential-backend.rkt` | 611 | 51 | 89 |
| `tests/test-audit-v09945-w6-session.rkt` | 594 | 66 | 159 |
| `tests/test-audit-v09945-w4-mas.rkt` | 587 | 69 | 191 |
| `tests/test-event-types.rkt` | 580 | 44 | 123 |
| `tests/test-memory-file-jsonl-backend.rkt` | 578 | 28 | 68 |
| `tests/test-audit-v09945-w5-tools.rkt` | 572 | 66 | 134 |
| `tests/test-audit-v09945-w3-gsd.rkt` | 563 | 74 | 174 |
| `tests/test-cell-diff-render.rkt` | 560 | 32 | 52 |
| `tests/test-openai-compatible.rkt` | 555 | 32 | 71 |
| `tests/test-audit-v09945-w10-integration.rkt` | 544 | 74 | 151 |
| `tests/test-spawn-subagents-approval.rkt` | 543 | 21 | 92 |
| `tests/test-rpc-mode.rkt` | 540 | 45 | 106 |
| `tests/test-memory-management-tools.rkt` | 529 | 29 | 66 |
| `tests/test-release-manifest-traceability.rkt` | 526 | 70 | 112 |
| `tests/test-integration.rkt` | 521 | 27 | 51 |
| `tests/test-extension-context.rkt` | 517 | 30 | 70 |
| `tests/test-gsd-milestone-truth.rkt` | 512 | 26 | 102 |
| `tests/test-memory-tools.rkt` | 511 | 37 | 73 |
| `tests/test-state-aware-builder.rkt` | 506 | 35 | 60 |
| `tests/test-tool-registry.rkt` | 506 | 50 | 101 |
| `tests/test-cli.rkt` | 497 | 69 | 127 |
| `tests/test-auth-store.rkt` | 496 | 51 | 97 |
| `tests/test-extensions.rkt` | 494 | 40 | 87 |
| `tests/test-release-audit-truth.rkt` | 489 | 70 | 91 |
| `tests/test-github-integration.rkt` | 487 | 5 | 75 |
| `tests/test-provider-conformance.rkt` | 486 | 30 | 62 |
| `tests/test-context-assembly.rkt` | 482 | 36 | 49 |
| `tests/test-resource-loader.rkt` | 480 | 34 | 60 |
| `tests/test-verifier-integration.rkt` | 476 | 26 | 39 |
| `tests/test-terminal-input.rkt` | 465 | 51 | 135 |
| `tests/test-spawn-subagent-terminal-outcomes.rkt` | 455 | 16 | 57 |
| `tests/test-component-model.rkt` | 451 | 49 | 91 |
| `tests/test-goal-state.rkt` | 446 | 22 | 90 |
| `tests/test-milestone-gate.rkt` | 446 | 58 | 91 |
| `tests/test-workflow-executor.rkt` | 446 | 25 | 79 |
| `tests/test-tui-watchdog.rkt` | 445 | 34 | 111 |
| `tests/test-streaming-transitions.rkt` | 439 | 21 | 47 |
| `tests/test-model-registry.rkt` | 429 | 10 | 83 |
| `tests/test-gsd-go-orchestrator.rkt` | 428 | 26 | 52 |
| `tests/test-audit-v09945-w1-memory.rkt` | 424 | 29 | 64 |
| `tests/test-loop.rkt` | 420 | 19 | 54 |
| `tests/test-scheduler-safe-mode.rkt` | 416 | 19 | 44 |
| `tests/test-tui-utf8.rkt` | 412 | 44 | 103 |
| `tests/test-memory-chained-backend.rkt` | 407 | 20 | 49 |
| `tests/test-provider.rkt` | 407 | 28 | 87 |
| `tests/test-audit-v09945-w2-context-assembly.rkt` | 406 | 32 | 63 |
| `tests/test-wave4-sdk-expansion.rkt` | 406 | 36 | 66 |
| `tests/test-worker-security.rkt` | 406 | 32 | 60 |
| `tests/test-arch-parameters.rkt` | 405 | 11 | 20 |
| `tests/test-gsd-state-machine.rkt` | 404 | 43 | 95 |
| `tests/test-event-json-round-trip.rkt` | 403 | 45 | 96 |
| `tests/test-pkg-registry.rkt` | 403 | 32 | 49 |
| `tests/test-event-bus.rkt` | 401 | 32 | 47 |
| `tests/test-gsd-governance-workflow.rkt` | 400 | 26 | 33 |
| `tests/test-stream-error-wrapping.rkt` | 399 | 20 | 50 |
| `tests/test-tui-terminal.rkt` | 398 | 35 | 96 |
| `tests/test-scheduler-hooks.rkt` | 397 | 10 | 35 |
| `tests/test-session-replay.rkt` | 396 | 12 | 42 |
| `tests/test-edit-execution-parity.rkt` | 395 | 21 | 41 |
| `tests/test-session-tree-nav.rkt` | 393 | 30 | 58 |
| `tests/test-gsd-campaign-state.rkt` | 390 | 23 | 64 |
| `tests/test-gsd-core.rkt` | 390 | 43 | 65 |
| `tests/test-session-config.rkt` | 386 | 52 | 115 |
| `tests/test-red-module-first-slice.rkt` | 383 | 66 | 73 |
| `tests/test-memory-context-injection.rkt` | 382 | 22 | 47 |
| `tests/test-gui-rich-transcript.rkt` | 379 | 60 | 118 |
| `tests/test-memory-hash-backend.rkt` | 378 | 34 | 80 |
| `tests/test-tui-approval-reducer.rkt` | 378 | 1 | 41 |
| `tests/test-context-provenance-pipeline.rkt` | 376 | 16 | 42 |
| `tests/test-ws-evolution.rkt` | 371 | 30 | 80 |

## Part-I finding statuses

Allowed: OPEN, PARTIALLY_RESOLVED, RESOLVED, STALE_INCORRECT

| ID | Finding | Status | Evidence |
|---|---|---|---|
| F01_GSD_DOMAIN_EFFECT_SEPARATION | GSD domain logic and external effects remain difficult to separate | OPEN | extensions/gsd/, docs/architecture/dependency-policy.rktd |
| F02_EXTENSION_CONTEXT_SESSION_TYPE | Extension Context imports Runtime session types | STALE_INCORRECT | extensions/context.rkt, util/extension/extension-types.rkt |
| F03_EXTENSION_RUNTIME_SERVICE_COUPLING | Extension Context still imports the concrete Provider Registry service | PARTIALLY_RESOLVED | extensions/context.rkt, runtime/provider/provider-registry.rkt |
| F04_EXTENSION_BOUNDARY_EXCEPTIONS | Extension Runtime and TUI boundary exceptions remain fragile | OPEN | docs/architecture/dependency-policy.rktd |
| F05_PROVIDER_CO_CHANGE_DUPLICATION | Provider transport has shared SSE primitives but adapter-specific normalization; no second evidence justifies a further shared base | PARTIALLY_RESOLVED | llm/stream.rkt, llm/openai-compatible.rkt, llm/gemini.rkt, docs/architecture/dependency-policy.rktd |
| F06_PROVIDER_PROTOCOL_LEAKAGE | Provider-specific stream parsing leaks into Agent Core | RESOLVED | llm/stream.rkt, tests/test-provider-transport-architecture.rkt |
| F07_AGENT_ITERATION_RUNTIME_BOUNDARY | Agent Iteration imports Runtime implementation modules | RESOLVED | docs/architecture/dependency-policy.rktd, tests/test-arch-fitness.rkt |
| F08_SESSION_LIFECYCLE_RESPONSIBILITIES | Session Lifecycle concentrates orchestration and preparation responsibilities | OPEN | runtime/session/session-lifecycle.rkt |
| F09_SETTINGS_QUERY_SURFACE | Settings Query has a high managed surface; policy records 226 provides but source exports count 53 — semantics differ, decide before v0.99.93 | OPEN | runtime/settings-query.rkt, docs/architecture/dependency-policy.rktd, tests/test-architecture-baseline.rkt |
| F10_HIDDEN_PARAMETER_STATE | Parameters contain unclassified hidden cross-turn session state | RESOLVED | docs/architecture/parameter-inventory.rktd, tests/test-arch-parameters.rkt |
| F11_CONTEXT_ASSEMBLY_COMPLEXITY | Context Assembly hidden state is resolved but cognitive complexity remains | PARTIALLY_RESOLVED | runtime/context-assembly/, tests/test-rollback-session-ownership.rkt |
| F12_DOCUMENTATION_ONLY_ENFORCEMENT | Architecture boundaries rely only on documentation | RESOLVED | tests/test-arch-fitness.rkt, docs/architecture/dependency-policy.rktd |

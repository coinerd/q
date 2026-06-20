# Abstraction Fitness Report

**Total modules scanned:** 691

## Largest Modules (top 20)

| Lines | Exports | Path |
|-------|---------|------|
| 741 | 3 | ./tools/builtins/spawn-subagent.rkt |
| 639 | 2 | ./llm/anthropic.rkt |
| 634 | 2 | ./cli/args.rkt |
| 621 | 2 | ./wiring/run-modes.rkt |
| 592 | 1 | ./scripts/run-tests.rkt |
| 562 | 1 | ./agent/event-structs.rkt |
| 558 | 2 | ./runtime/settings-query.rkt |
| 549 | 1 | ./tui/terminal.rkt |
| 548 | 6 | ./runtime/session/session-store.rkt |
| 548 | 3 | ./tui/state-types.rkt |
| 534 | 2 | ./tools/scheduler.rkt |
| 528 | 24 | ./scripts/abstraction-audit.rkt |
| 522 | 3 | ./runtime/agent-session.rkt |
| 520 | 3 | ./browser/adapters/playwright-sidecar.rkt |
| 511 | 1 | ./tui/state-events/core-handlers.rkt |
| 498 | 2 | ./tui/terminal-input.rkt |
| 487 | 4 | ./tui/tui-render-loop.rkt |
| 486 | 2 | ./interfaces/sessions.rkt |
| 477 | 1 | ./tui/commands.rkt |
| 476 | 2 | ./llm/gemini.rkt |

## Parameter Usage

| Count | Path |
|-------|------|
| 12 | ./scripts/run-benchmark.rkt |
| 8 | ./scripts/abstraction-audit.rkt |
| 7 | ./runtime/context-assembly/rollback-actions.rkt |
| 7 | ./util/event/event-macro.rkt |
| 5 | ./agent/verification/verifier-core.rkt |
| 5 | ./runtime/memory/embeddings.rkt |
| 5 | ./sandbox/gateway-bridge.rkt |
| 4 | ./llm/gemini.rkt |
| 4 | ./runtime/context-assembly/config.rkt |
| 4 | ./runtime/memory/service.rkt |
| 4 | ./sandbox/gateway-ipc.rkt |
| 4 | ./sandbox/limits.rkt |
| 4 | ./tools/builtins/spawn-subagent.rkt |
| 3 | ./agent/roles/tool-gateway.rkt |
| 3 | ./agent/verification/verifier-prompt.rkt |
| 3 | ./extensions/gsd/session-state.rkt |
| 3 | ./extensions/gsd/state-machine.rkt |
| 3 | ./pkg/registry.rkt |
| 3 | ./runtime/context-assembly/auto-distillation.rkt |
| 3 | ./runtime/context-assembly/memory-builder.rkt |
| 3 | ./runtime/context-assembly/state-aware-builder.rkt |
| 3 | ./runtime/memory/reflection.rkt |
| 3 | ./runtime/session/session-config.rkt |
| 3 | ./sandbox/evaluator.rkt |
| 3 | ./sandbox/subprocess.rkt |
| 3 | ./scripts/audit-project.rkt |
| 3 | ./scripts/capture-regression.rkt |
| 3 | ./scripts/lint-release-notes.rkt |
| 3 | ./scripts/run-dogfood-session.rkt |
| 3 | ./tools/builtins/bash.rkt |
| 3 | ./util/error/output-guard.rkt |
| 3 | ./util/event/event-bus.rkt |
| 3 | ./util/lockfile.rkt |
| 3 | ./util/safe-mode/safe-mode-state.rkt |
| 2 | ./agent/distributed/remote-ipc.rkt |
| 2 | ./agent/loop-dispatch.rkt |
| 2 | ./browser/adapters/playwright-sidecar.rkt |
| 2 | ./cli/generate-certificates.rkt |
| 2 | ./extensions/mcp-adapter.rkt |
| 2 | ./extensions/ui-surface.rkt |
| 2 | ./gui/lifecycle-hooks.rkt |
| 2 | ./launch.rkt |
| 2 | ./llm/stream.rkt |
| 2 | ./runtime/compaction/ctx-compact.rkt |
| 2 | ./runtime/credentials/protocol.rkt |
| 2 | ./runtime/memory/auto-extraction.rkt |
| 2 | ./runtime/memory/backends/external-protocol.rkt |
| 2 | ./runtime/safe-mode.rkt |
| 2 | ./runtime/session/session-events.rkt |
| 2 | ./runtime/session/session-store-tree.rkt |
| 2 | ./sandbox/worker-dispatch.rkt |
| 2 | ./scripts/ci-local.rkt |
| 2 | ./scripts/run-tests.rkt |
| 2 | ./tools/builtins/write.rkt |
| 2 | ./tui/render-loop/watchdog.rkt |
| 2 | ./tui/renderer.rkt |
| 2 | ./tui/state-events/registry.rkt |
| 2 | ./tui/terminal.rkt |
| 2 | ./util/truncation.rkt |
| 1 | ./agent/blackboard.rkt |
| 1 | ./agent/distributed/remote-executor.rkt |
| 1 | ./agent/event-struct-coverage.rkt |
| 1 | ./agent/iteration/main-loop.rkt |
| 1 | ./agent/iteration/step-interpreter.rkt |
| 1 | ./agent/loop-fsm.rkt |
| 1 | ./agent/loop.rkt |
| 1 | ./agent/registry-watcher.rkt |
| 1 | ./agent/registry.rkt |
| 1 | ./agent/roles/supervisor.rkt |
| 1 | ./agent/state.rkt |
| 1 | ./agent/stream-reducer.rkt |
| 1 | ./agent/verification/verifier-gate.rkt |
| 1 | ./browser/service.rkt |
| 1 | ./browser/settings.rkt |
| 1 | ./extensions/combinators.rkt |
| 1 | ./extensions/github/helpers.rkt |
| 1 | ./extensions/gsd/core.rkt |
| 1 | ./extensions/gsd/events.rkt |
| 1 | ./extensions/gsd/plan-context-builder.rkt |
| 1 | ./extensions/hooks.rkt |
| 1 | ./extensions/loader.rkt |
| 1 | ./extensions/manifest-audit.rkt |
| 1 | ./extensions/quarantine.rkt |
| 1 | ./extensions/racket-tooling-helpers.rkt |
| 1 | ./extensions/remote-collab/ssh-helpers.rkt |
| 1 | ./extensions/widget-lifecycle.rkt |
| 1 | ./gui/main.rkt |
| 1 | ./interfaces/doctor.rkt |
| 1 | ./interfaces/sessions.rkt |
| 1 | ./runtime/auth/oauth.rkt |
| 1 | ./runtime/context-assembly/state-inference.rkt |
| 1 | ./runtime/context-assembly/turn-context.rkt |
| 1 | ./runtime/gsd-query.rkt |
| 1 | ./runtime/memory/conclusion-bridge.rkt |
| 1 | ./runtime/package.rkt |
| 1 | ./runtime/session/session-mutation.rkt |
| 1 | ./runtime/session/session-persistence.rkt |
| 1 | ./sandbox/executor-server.rkt |
| 1 | ./sandbox/worker-tools.rkt |
| 1 | ./scripts/benchmark/executor.rkt |
| 1 | ./scripts/benchmark/scorer.rkt |
| 1 | ./scripts/lint-ivg.rkt |
| 1 | ./scripts/lint-security.rkt |
| 1 | ./scripts/metrics.rkt |
| 1 | ./scripts/run-tests/overhead.rkt |
| 1 | ./tools/builtins/edit.rkt |
| 1 | ./tools/scheduler.rkt |
| 1 | ./tui/approval-channel.rkt |
| 1 | ./tui/clipboard.rkt |
| 1 | ./tui/commands/runtime-control.rkt |
| 1 | ./tui/component.rkt |
| 1 | ./tui/keybindings/binding-resolver.rkt |
| 1 | ./tui/render/status-line.rkt |
| 1 | ./tui/state-ui.rkt |
| 1 | ./tui/terminal-input.rkt |
| 1 | ./tui/theme.rkt |
| 1 | ./tui/tui-render-loop.rkt |
| 1 | ./tui/vdom-bridge.rkt |
| 1 | ./ui-core/ui-actions.rkt |
| 1 | ./util/audit-log.rkt |
| 1 | ./util/capability.rkt |
| 1 | ./util/event/event-migration.rkt |
| 1 | ./util/security/cert-generator.rkt |
| 1 | ./wiring/rpc-ui-adapter.rkt |

## Macro Usage

| Count | Path |
|-------|------|
| 8 | ./scripts/abstraction-audit.rkt |
| 3 | ./extensions/define-extension.rkt |
| 3 | ./util/event/event-macro.rkt |
| 3 | ./util/fsm/fsm.rkt |
| 2 | ./extensions/test-harness.rkt |
| 2 | ./tools/define-tool.rkt |
| 2 | ./util/error/error-helpers.rkt |
| 1 | ./browser/service.rkt |
| 1 | ./browser/session.rkt |
| 1 | ./extensions/github/helpers.rkt |
| 1 | ./runtime/auth/auth-store.rkt |
| 1 | ./tui/terminal.rkt |
| 1 | ./util/error/output-guard.rkt |
| 1 | ./util/telemetry.rkt |

## struct-out Exports

| Count | Path |
|-------|------|
| 10 | ./scripts/abstraction-audit.rkt |
| 7 | ./agent/event-structs/memory-events.rkt |
| 7 | ./agent/event-structs/tool-events.rkt |
| 4 | ./scripts/benchmark/task.rkt |
| 3 | ./runtime/context-assembly/budgeting.rkt |
| 2 | ./benchmarks/metrics.rkt |
| 2 | ./extensions/gsd/transition-logic.rkt |
| 2 | ./gui/gui-types.rkt |
| 2 | ./runtime/context-assembly/rollback-actions.rkt |
| 2 | ./scripts/benchmark/report.rkt |
| 2 | ./scripts/check-imports.rkt |
| 2 | ./skills/resource-loader.rkt |
| 1 | ./agent/streaming-message.rkt |
| 1 | ./benchmarks/tasks.rkt |
| 1 | ./browser/adapter.rkt |
| 1 | ./browser/adapters/mock.rkt |
| 1 | ./browser/adapters/playwright-sidecar.rkt |
| 1 | ./browser/settings.rkt |
| 1 | ./browser/types.rkt |
| 1 | ./extensions/gsd/plan-types.rkt |
| 1 | ./interfaces/cli.rkt |
| 1 | ./interfaces/doctor.rkt |
| 1 | ./interfaces/sdk-compat.rkt |
| 1 | ./interfaces/sdk-core.rkt |
| 1 | ./runtime/goal/goal-types.rkt |
| 1 | ./runtime/memory/types.rkt |
| 1 | ./runtime/session/lifecycle-state.rkt |
| 1 | ./scripts/benchmark/executor.rkt |
| 1 | ./scripts/benchmark/scorer.rkt |
| 1 | ./scripts/run-dogfood-session.rkt |
| 1 | ./scripts/run-tests/overhead.rkt |
| 1 | ./tui/cell-buffer.rkt |
| 1 | ./tui/input.rkt |
| 1 | ./ui-core/event-types.rkt |
| 1 | ./ui-core/ui-delta.rkt |
| 1 | ./ui-core/ui-reducer.rkt |
| 1 | ./util/security/capability-tokens.rkt |

## I/O Mixed With Pure Logic

| Count | Path |
|-------|------|
| 53 | ./sandbox/subprocess.rkt |
| 28 | ./sandbox/worker-tools.rkt |
| 25 | ./sandbox/subprocess-helpers.rkt |
| 25 | ./tui/clipboard.rkt |
| 21 | ./sandbox/gateway-ipc.rkt |
| 20 | ./scripts/run-tests.rkt |
| 18 | ./scripts/abstraction-audit.rkt |
| 18 | ./scripts/audit-project.rkt |
| 16 | ./tools/builtins/bash.rkt |
| 15 | ./browser/adapters/playwright-sidecar.rkt |
| 13 | ./tui/commands/extension.rkt |
| 11 | ./scripts/metrics.rkt |
| 11 | ./scripts/sync-version.rkt |
| 10 | ./runtime/goal/goal-checks.rkt |
| 10 | ./tui/commands/session.rkt |
| 9 | ./extensions/package-audit.rkt |
| 9 | ./runtime/session/session-lifecycle.rkt |
| 9 | ./tui/state-events/core-handlers.rkt |
| 8 | ./extensions/gsd/archive.rkt |
| 8 | ./extensions/gsd/wave-docs.rkt |
| 7 | ./interfaces/sessions.rkt |
| 7 | ./scripts/bump-version.rkt |
| 7 | ./scripts/lint-ivg.rkt |
| 7 | ./tools/builtins/find.rkt |
| 7 | ./tui/commands/runtime-control.rkt |
| 7 | ./util/json/jsonl.rkt |
| 6 | ./extensions/racket-tooling-helpers.rkt |
| 6 | ./runtime/credentials/protocol.rkt |
| 6 | ./scripts/lint-all.rkt |
| 6 | ./scripts/lint-release-readiness.rkt |
| 6 | ./scripts/run-tests/cli.rkt |
| 5 | ./extensions/remote-collab/ssh-helpers.rkt |
| 5 | ./extensions/remote-collab/tmux-helpers.rkt |
| 5 | ./pkg/registry.rkt |
| 5 | ./scripts/hotspot-report.rkt |
| 5 | ./scripts/lint-version.rkt |
| 5 | ./scripts/run-tests/classify.rkt |
| 5 | ./scripts/run-tests/gate-evidence.rkt |
| 5 | ./scripts/sync-readme-status.rkt |
| 5 | ./skills/resource-loader.rkt |
| 5 | ./util/lockfile.rkt |
| 4 | ./extensions/github/helpers.rkt |
| 4 | ./extensions/gsd/core.rkt |
| 4 | ./extensions/gsd/tool-handlers.rkt |
| 4 | ./extensions/q-sync.rkt |
| 4 | ./runtime/memory/backends/file-jsonl-ops.rkt |
| 4 | ./runtime/session/session-store-integrity.rkt |
| 4 | ./scripts/check-protocols.rkt |
| 4 | ./scripts/gen-release-manifest.rkt |
| 4 | ./scripts/lint-doc-freshness.rkt |
| 4 | ./scripts/lint-tests.rkt |
| 4 | ./scripts/run-tests/inventory.rkt |
| 4 | ./tools/builtins/delete-lines.rkt |
| 4 | ./tools/builtins/edit.rkt |
| 4 | ./tools/builtins/grep.rkt |
| 4 | ./tui/commands/branch.rkt |
| 4 | ./tui/terminal-native.rkt |
| 4 | ./wiring/run-modes.rkt |
| 3 | ./agent/mas-guidance.rkt |
| 3 | ./examples/extensions/keyboard-shortcut.rkt |
| 3 | ./extensions/loader.rkt |
| 3 | ./extensions/manifest.rkt |
| 3 | ./extensions/message-inject.rkt |
| 3 | ./interfaces/doctor.rkt |
| 3 | ./runtime/context-assembly/blackboard-context.rkt |
| 3 | ./runtime/context-assembly/serialization.rkt |
| 3 | ./scripts/arch-report.rkt |
| 3 | ./scripts/check-lint-alignment.rkt |
| 3 | ./scripts/lint-contract-changes.rkt |
| 3 | ./scripts/lint-credential-policy.rkt |
| 3 | ./scripts/lint-docs.rkt |
| 3 | ./scripts/lint-test-tags.rkt |
| 3 | ./scripts/milestone-gate.rkt |
| 3 | ./scripts/pre-commit.rkt |
| 3 | ./scripts/run-dogfood-session.rkt |
| 3 | ./scripts/run-tests/reporting.rkt |
| 3 | ./scripts/sdk-gsd-integration-test.rkt |
| 3 | ./tools/registry-table.rkt |
| 3 | ./tui/commands.rkt |
| 3 | ./tui/state-types.rkt |
| 3 | ./tui/theme.rkt |
| 2 | ./agent/loop-messages.rkt |
| 2 | ./agent/registry-watcher.rkt |
| 2 | ./agent/roles/base.rkt |
| 2 | ./agent/verification/verifier-prompt.rkt |
| 2 | ./extensions/quarantine.rkt |
| 2 | ./extensions/session-export.rkt |
| 2 | ./llm/anthropic.rkt |
| 2 | ./runtime/compaction/compactor.rkt |
| 2 | ./runtime/context/context-pinning.rkt |
| 2 | ./runtime/context-assembly/state-aware-builder.rkt |
| 2 | ./runtime/goal/goal-evidence.rkt |
| 2 | ./runtime/project-tree.rkt |
| 2 | ./runtime/session/session-lifecycle-transitions.rkt |
| 2 | ./runtime/session-index/mutations.rkt |
| 2 | ./runtime/session-store/versioning.rkt |
| 2 | ./runtime/settings-core.rkt |
| 2 | ./runtime/trace-sink.rkt |
| 2 | ./sandbox/limits.rkt |
| 2 | ./scripts/benchmark/baseline.rkt |
| 2 | ./scripts/benchmark/fixtures/legacy.rkt |
| 2 | ./scripts/benchmark/scorer.rkt |
| 2 | ./scripts/benchmark/task.rkt |
| 2 | ./scripts/capture-regression.rkt |
| 2 | ./scripts/check-deps.rkt |
| 2 | ./scripts/check-imports.rkt |
| 2 | ./scripts/gen-release-notes.rkt |
| 2 | ./scripts/gen-sdk-catalog.rkt |
| 2 | ./scripts/lint-ci-readiness.rkt |
| 2 | ./scripts/lint-deprecation-deadlines.rkt |
| 2 | ./scripts/lint-security.rkt |
| 2 | ./scripts/lint-widened-ledger.rkt |
| 2 | ./scripts/metrics-report.rkt |
| 2 | ./scripts/run-tests/overhead.rkt |
| 2 | ./scripts/test-gsd-sdk-live.rkt |
| 2 | ./tools/builtins/spawn-subagent.rkt |
| 2 | ./tools/builtins/write.rkt |
| 2 | ./tui/commands/general.rkt |
| 2 | ./tui/keymap.rkt |
| 2 | ./tui/submit-handler.rkt |
| 2 | ./util/audit-log.rkt |
| 2 | ./util/error/error-classify.rkt |
| 2 | ./util/json/checksum.rkt |
| 2 | ./util/json/json-helpers.rkt |
| 2 | ./wiring/mode-helpers.rkt |
| 1 | ./agent/event-struct-coverage.rkt |
| 1 | ./browser/audit.rkt |
| 1 | ./cli/export.rkt |
| 1 | ./cli/init-wizard.rkt |
| 1 | ./cli/replay.rkt |
| 1 | ./examples/extensions/context-enricher.rkt |
| 1 | ./examples/extensions/error-handler.rkt |
| 1 | ./examples/extensions/tool-guard.rkt |
| 1 | ./examples/sdk/03-custom-prompt.rkt |
| 1 | ./extensions/compact-context.rkt |
| 1 | ./extensions/gsd/plan-context-builder.rkt |
| 1 | ./extensions/gsd/prompts.rkt |
| 1 | ./extensions/gsd/wave-executor.rkt |
| 1 | ./extensions/image-input.rkt |
| 1 | ./gui/gui-types.rkt |
| 1 | ./gui/slash-commands.rkt |
| 1 | ./interfaces/gui.rkt |
| 1 | ./llm/gemini.rkt |
| 1 | ./llm/stream.rkt |
| 1 | ./runtime/auth/oauth-callback.rkt |
| 1 | ./runtime/compaction/session-compaction.rkt |
| 1 | ./runtime/compaction/token-compaction.rkt |
| 1 | ./runtime/context/context-fit.rkt |
| 1 | ./runtime/context-assembly/context-floor.rkt |
| 1 | ./runtime/context-assembly/memory-builder.rkt |
| 1 | ./runtime/goal/goal-agent-evaluator.rkt |
| 1 | ./runtime/goal/goal-runner.rkt |
| 1 | ./runtime/memory/auto-extraction.rkt |
| 1 | ./runtime/memory/backends/memory-hash.rkt |
| 1 | ./runtime/memory/search.rkt |
| 1 | ./runtime/provider/provider-schema.rkt |
| 1 | ./runtime/session/session-migration.rkt |
| 1 | ./runtime/session/session-persistence.rkt |
| 1 | ./runtime/session/session-store.rkt |
| 1 | ./runtime/settings-query.rkt |
| 1 | ./runtime/trace-logger.rkt |
| 1 | ./sandbox/gateway-bridge.rkt |
| 1 | ./scripts/archive-planning.rkt |
| 1 | ./scripts/co-change-report.rkt |
| 1 | ./scripts/contract-metrics.rkt |
| 1 | ./scripts/lint-changelog-dates.rkt |
| 1 | ./scripts/lint-format.rkt |
| 1 | ./scripts/lint-pkg-metadata.rkt |
| 1 | ./scripts/lint-release-notes.rkt |
| 1 | ./scripts/run-tests/ledger.rkt |
| 1 | ./scripts/run-tests/profiles.rkt |
| 1 | ./scripts/test-gsd-go-replanning.rkt |
| 1 | ./scripts/test-metadata.rkt |
| 1 | ./scripts/wrap-lines.rkt |
| 1 | ./skills/context-files.rkt |
| 1 | ./skills/mas-workflow.rkt |
| 1 | ./tools/builtins/read.rkt |
| 1 | ./tools/builtins/skill-router.rkt |
| 1 | ./tools/builtins/spawn-subagent-helpers.rkt |
| 1 | ./tools/scheduler.rkt |
| 1 | ./tools/shell-risk.rkt |
| 1 | ./tui/commands/model.rkt |
| 1 | ./tui/scrollback.rkt |
| 1 | ./tui/state-events/goal-handlers.rkt |
| 1 | ./tui/tui-init.rkt |
| 1 | ./tui/tui-render-loop.rkt |
| 1 | ./ui-core/observable-bridge.rkt |
| 1 | ./util/command-types.rkt |
| 1 | ./util/error/error-sanitizer.rkt |
| 1 | ./util/error/errors.rkt |
| 1 | ./util/export/export-html.rkt |
| 1 | ./util/markdown.rkt |
| 1 | ./util/message/message-helpers.rkt |
| 1 | ./util/shell-quote.rkt |
| 1 | ./util/truncation.rkt |

## Serialization Hotspots

| Count | Path |
|-------|------|
| 26 | ./runtime/session/session-store.rkt |
| 24 | ./runtime/goal/goal-codec.rkt |
| 18 | ./browser/types.rkt |
| 17 | ./llm/model.rkt |
| 15 | ./runtime/memory/types.rkt |
| 15 | ./scripts/abstraction-audit.rkt |
| 12 | ./tui/scrollback.rkt |
| 10 | ./gui/gui-types.rkt |
| 10 | ./runtime/memory/backends/file-jsonl-ops.rkt |
| 10 | ./sandbox/ipc-protocol.rkt |
| 9 | ./agent/event-emitter.rkt |
| 9 | ./scripts/benchmark/report.rkt |
| 8 | ./llm/anthropic.rkt |
| 8 | ./llm/openai-compatible.rkt |
| 8 | ./runtime/auth/oauth.rkt |
| 8 | ./runtime/goal/goal-state.rkt |
| 8 | ./runtime/session/session-config.rkt |
| 8 | ./runtime/session-index/mutations.rkt |
| 8 | ./sandbox/worker-dispatch.rkt |
| 8 | ./tools/builtins/browser-tools.rkt |
| 8 | ./tools/tool.rkt |
| 7 | ./interfaces/json-mode.rkt |
| 7 | ./runtime/context-assembly/task-conclusion.rkt |
| 7 | ./runtime/session-store/versioning.rkt |
| 7 | ./util/event/event.rkt |
| 6 | ./agent/event-json.rkt |
| 6 | ./extensions/manifest.rkt |
| 6 | ./runtime/context-assembly/task-memory.rkt |
| 6 | ./runtime/session/session-store-integrity.rkt |
| 6 | ./scripts/audit-project.rkt |
| 6 | ./scripts/run-tests/reporting.rkt |
| 6 | ./util/content/content-parts.rkt |
| 6 | ./util/message/message.rkt |
| 6 | ./wiring/run-modes.rkt |
| 5 | ./extensions/github/handlers/milestone-ops.rkt |
| 5 | ./runtime/session/session-migration.rkt |
| 5 | ./tools/registry.rkt |
| 4 | ./extensions/gsd/events.rkt |
| 4 | ./extensions/session-export.rkt |
| 4 | ./llm/gemini.rkt |
| 4 | ./runtime/memory/backends/mem0-api.rkt |
| 4 | ./sandbox/executor-server.rkt |
| 4 | ./skills/workflow-executor.rkt |
| 4 | ./tools/builtins/skill-router.rkt |
| 4 | ./util/message/mas-envelope.rkt |
| 4 | ./wiring/run-json-rpc.rkt |
| 3 | ./agent/distributed/remote-ipc.rkt |
| 3 | ./extensions/mcp-adapter.rkt |
| 3 | ./extensions/tool-api.rkt |
| 3 | ./llm/http-helpers.rkt |
| 3 | ./llm/openrouter.rkt |
| 3 | ./runtime/agent-session.rkt |
| 3 | ./runtime/context-assembly/state-aware-builder.rkt |
| 3 | ./runtime/session/session-lifecycle.rkt |
| 3 | ./sandbox/gateway-ipc.rkt |
| 3 | ./tools/scheduler.rkt |
| 3 | ./tui/theme.rkt |
| 3 | ./util/event/event-codec.rkt |
| 3 | ./util/event/event-payloads.rkt |
| 3 | ./util/export/export-json.rkt |
| 2 | ./agent/iteration/loop-config.rkt |
| 2 | ./agent/iteration/main-loop.rkt |
| 2 | ./agent/streaming-message.rkt |
| 2 | ./browser/adapters/playwright-sidecar.rkt |
| 2 | ./extensions/github/helpers.rkt |
| 2 | ./extensions/gsd/tool-handlers.rkt |
| 2 | ./gui/state-sync.rkt |
| 2 | ./interfaces/rpc-mode.rkt |
| 2 | ./llm/azure-openai.rkt |
| 2 | ./llm/stream.rkt |
| 2 | ./runtime/compaction/session-compaction.rkt |
| 2 | ./runtime/context-assembly/memory-builder.rkt |
| 2 | ./runtime/tool-coordinator.rkt |
| 2 | ./scripts/run-tests/parse.rkt |
| 2 | ./scripts/run-tests.rkt |
| 2 | ./tools/builtins/firecrawl.rkt |
| 2 | ./ui-core/event-types.rkt |
| 2 | ./util/content/content-helpers.rkt |
| 2 | ./util/event/event-macro.rkt |
| 1 | ./agent/stream-runner.rkt |
| 1 | ./agent/verification/verifier-core.rkt |
| 1 | ./browser/audit.rkt |
| 1 | ./examples/extensions/typed-event-observer.rkt |
| 1 | ./extensions/github/handlers/issue-ops.rkt |
| 1 | ./extensions/github/tool-handlers.rkt |
| 1 | ./extensions/gsd/command-handlers.rkt |
| 1 | ./extensions/racket-tooling/analysis.rkt |
| 1 | ./gui/components/keybindings.rkt |
| 1 | ./interfaces/sessions.rkt |
| 1 | ./pkg/registry.rkt |
| 1 | ./runtime/goal/goal-agent-evaluator.rkt |
| 1 | ./runtime/goal/goal-evaluator.rkt |
| 1 | ./runtime/memory/embeddings.rkt |
| 1 | ./runtime/session/session-metadata.rkt |
| 1 | ./scripts/capture-regression.rkt |
| 1 | ./scripts/metrics-report.rkt |
| 1 | ./scripts/milestone-gate.rkt |
| 1 | ./scripts/run-benchmark.rkt |
| 1 | ./tools/builtins/spawn-subagent.rkt |
| 1 | ./tui/keymap.rkt |
| 1 | ./tui/tui-init.rkt |
| 1 | ./ui-core/dispatch.rkt |
| 1 | ./ui-core/ui-actions.rkt |
| 1 | ./util/event/event-bus.rkt |
| 1 | ./util/json/json-helpers.rkt |
| 1 | ./util/tool/tool-display.rkt |

## Error/Raise Density (top 20)

| Count | Path |
|-------|------|
| 17 | ./extensions/racket-tooling/formatting.rkt |
| 16 | ./util/error/errors.rkt |
| 13 | ./browser/adapters/playwright-sidecar.rkt |
| 12 | ./llm/stream.rkt |
| 12 | ./tools/builtins/firecrawl.rkt |
| 10 | ./llm/http-helpers.rkt |
| 8 | ./scripts/run-tests/cli.rkt |
| 7 | ./tools/tool.rkt |
| 6 | ./agent/registry.rkt |
| 6 | ./runtime/session/session-mutation.rkt |
| 6 | ./scripts/benchmark/task.rkt |
| 5 | ./browser/service.rkt |
| 5 | ./browser/session.rkt |
| 5 | ./extensions/racket-tooling/rewrite.rkt |
| 5 | ./llm/model.rkt |
| 5 | ./tools/builtins/skill-router.rkt |
| 4 | ./agent/distributed/remote-ipc.rkt |
| 4 | ./extensions/manifest.rkt |
| 4 | ./llm/openai-compatible.rkt |
| 4 | ./runtime/extension-catalog.rkt |

## Handler Density (with-handlers, top 20)

| Count | Path |
|-------|------|
| 10 | ./tools/builtins/browser-tools.rkt |
| 9 | ./browser/adapters/playwright-sidecar.rkt |
| 9 | ./tui/tui-init.rkt |
| 7 | ./sandbox/subprocess.rkt |
| 6 | ./sandbox/executor-server.rkt |
| 6 | ./tools/scheduler.rkt |
| 5 | ./agent/distributed/remote-executor.rkt |
| 5 | ./interfaces/doctor.rkt |
| 5 | ./runtime/auth/auth-store.rkt |
| 5 | ./runtime/auth/oauth-callback.rkt |
| 5 | ./sandbox/gateway-bridge.rkt |
| 5 | ./tools/builtins/edit.rkt |
| 5 | ./tools/builtins/firecrawl.rkt |
| 5 | ./tools/builtins/grep.rkt |
| 5 | ./tui/clipboard.rkt |
| 5 | ./tui/commands/extension.rkt |
| 4 | ./agent/distributed/remote-ipc.rkt |
| 4 | ./examples/extensions/error-handler.rkt |
| 4 | ./extensions/loader.rkt |
| 4 | ./gui/main.rkt |

## all-defined-out Usage

- ./browser/events.rkt
- ./extensions/gsd/event-structs.rkt
- ./scripts/abstraction-audit.rkt
- ./scripts/check-imports.rkt

## Advisory

This report is advisory. Use with the Abstraction Instruction Manual
to identify candidates for boundary clarification.

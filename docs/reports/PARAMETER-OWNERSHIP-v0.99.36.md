# Parameter / Dynamic-Context Ownership Map — v0.99.36 W4

**Date:** 2026-06-22
**Wave:** W4 (#8417)
**Risk Assessment:** GREEN (reward 13, risk 4)
**Scope:** Documentation only — no production code changes

## 1. Executive Summary

**Total `make-parameter` sites:** 189 (production code, excluding tests)
**Total `parameterize` sites:** 59 (production code)
**Modules containing parameters:** 78

Parameters in the q-agent codebase fall into five distinct ownership patterns.
This document inventories each pattern, identifies the owning module, and flags
anti-patterns for future cleanup.

## 2. Ownership Pattern Classification

### Pattern A: Feature Flag (toggle)
**Count:** ~40 parameters
**Shape:** `(make-parameter #f)` or `(make-parameter #t)`
**Purpose:** Enable/disable features at runtime, usually via session config.
**Write sites:** `runtime/session/session-config.rkt` via `parameterize`.

| Parameter | Module | Default | Session Config Wired |
|-----------|--------|---------|---------------------|
| `current-task-state-aware-assembly?` | runtime/context-assembly/config.rkt | #f | Yes |
| `current-graph-conclusion-selection?` | runtime/context-assembly/config.rkt | #f | Yes |
| `current-ws-evolution-enabled?` | runtime/context-assembly/config.rkt | #f | Yes |
| `current-auto-distillation-enabled?` | runtime/context-assembly/auto-distillation.rkt | #f | Yes |
| `current-memory-injection-budget` | runtime/context-assembly/memory-builder.rkt | #f | Yes |
| `current-blackboard-injection-enabled` | runtime/context-assembly/state-aware-builder.rkt | #f | Yes |
| `current-goal-loop-enabled?` | runtime/session/session-config.rkt | #t | Yes |
| `current-verifier-enabled` | agent/verification/verifier-core.rkt | #f | Yes |
| `current-auto-extraction-enabled` | runtime/memory/auto-extraction.rkt | #f | Yes |
| `current-auto-reflection-enabled` | runtime/memory/service.rkt | #f | Yes |
| `current-conclusion-to-memory-bridge-enabled` | runtime/memory/conclusion-bridge.rkt | #f | Yes |
| `current-external-backend-enabled` | runtime/memory/backends/external-protocol.rkt | #f | Yes |
| `current-mid-session-bridge-enabled` | runtime/session/session-events.rkt | #f | Yes |
| `current-safe-mode-locked?` | runtime/safe-mode.rkt | #f | Safe-mode subsystem |
| `current-fuzzy-edit-enabled?` | tools/builtins/edit.rkt | #f | No |
| `current-ui-event-actions-enabled?` | ui-core/ui-actions.rkt | #f | No |
| `current-use-registry` | agent/roles/supervisor.rkt | #f | No |
| `current-reflection-prompt-enabled` | agent/iteration/step-interpreter.rkt | #f | No |
| `current-execution-plane-enabled` | sandbox/gateway-bridge.rkt | #f | No |

**Assessment:** Feature-flag parameters are well-structured. The session-config layer
(`runtime/session/session-config.rkt`) acts as the single write authority for most
flags. Unwired flags (e.g., `current-fuzzy-edit-enabled?`) are candidates for
session-config integration if the feature is promoted.

---

### Pattern B: Config Value (limit/threshold/path)
**Count:** ~55 parameters
**Shape:** `(make-parameter <integer>)`, `(make-parameter <path>)`, etc.
**Purpose:** Tunable configuration values, primarily read-only except for
test isolation or session-config wiring.

| Parameter | Module | Default | Notes |
|-----------|--------|---------|-------|
| `current-verifier-risk-threshold` | agent/verification/verifier-core.rkt | 'medium | Dynamic in verifier-gate.rkt |
| `current-verifier-model` | agent/verification/verifier-core.rkt | #f | |
| `current-verifier-provider` | agent/verification/verifier-core.rkt | #f | |
| `current-verifier-timeout-ms` | agent/verification/verifier-core.rkt | 120000 | |
| `current-verifier-max-files-shown` | agent/verification/verifier-prompt.rkt | 30 | |
| `current-verifier-max-diff-chars` | agent/verification/verifier-prompt.rkt | 8000 | |
| `current-verifier-max-file-lines` | agent/verification/verifier-prompt.rkt | 3 | |
| `current-conclusion-token-budget` | runtime/context-assembly/config.rkt | 2000 | |
| `current-memory-max-entry-chars` | runtime/context-assembly/memory-builder.rkt | 200 | |
| `current-memory-retrieval-timeout-ms` | runtime/context-assembly/memory-builder.rkt | 2000 | |
| `current-state-inference-threshold` | runtime/context-assembly/state-inference.rkt | 0.7 | |
| `current-compact-rate-limit` | runtime/compaction/ctx-compact.rkt | 5 | |
| `current-compact-rate-window` | runtime/compaction/ctx-compact.rkt | 60 | |
| `current-hook-timeout-ms` | extensions/hooks.rkt | 500 | |
| `current-extension-startup-timeout` | extensions/loader.rkt | 30 | |
| `current-sandbox-memory-limit` | sandbox/evaluator.rkt | 256 | |
| `current-sandbox-path-limit` | sandbox/evaluator.rkt | #f | |
| `current-max-processes` | sandbox/limits.rkt | 10 | |
| `current-execution-plane-timeout-ms` | sandbox/gateway-bridge.rkt | 120000 | |
| `current-max-write-bytes` | tools/builtins/write.rkt | 1048576 | |
| `cumulative-write-budget` | tools/builtins/write.rkt | 52428800 | |
| `max-parallel-tools` | tools/scheduler.rkt | 8 | |
| `current-agent-pool-limit` | tools/builtins/spawn-subagent.rkt | 3 | |
| `current-busy-watchdog-ms` | tui/render-loop/watchdog.rkt | 300000 | |
| `current-streaming-watchdog-ms` | tui/render-loop/watchdog.rkt | 180000 | |
| `current-audit-log-max-bytes` | util/audit-log.rkt | 10485760 | |
| `current-circuit-breaker-threshold` | util/event/event-bus.rkt | 100 | |
| `current-circuit-breaker-cooldown-secs` | util/event/event-bus.rkt | 60 | |
| `MAX-STREAM-CHUNKS` | agent/stream-reducer.rkt | 10000 | |
| `gsd-max-rework-iterations` | extensions/gsd/state-machine.rkt | 3 | |
| `gsd-wave-gate-interval` | extensions/gsd/state-machine.rkt | 5 | |

**Assessment:** Config-value parameters are healthy. Defaults are explicit and
documented. The verifier-core cluster (8 params) is the densest config cluster
and could benefit from a config struct in future cleanup.

---

### Pattern C: Dependency Injection (callback/function)
**Count:** ~15 parameters
**Shape:** `(make-parameter <default-fn>)` or `(make-parameter #f)`
**Purpose:** Injectable callbacks for testing, alternative implementations.

| Parameter | Module | Default | Pattern |
|-----------|--------|---------|---------|
| `current-llm-distill-fn` | runtime/context-assembly/auto-distillation.rkt | #f | FN injection |
| `current-force-distill-fn` | runtime/context-assembly/rollback-actions.rkt | #f | FN injection |
| `current-expand-context-fn` | runtime/context-assembly/rollback-actions.rkt | #f | FN injection |
| `current-revert-state-fn` | runtime/context-assembly/rollback-actions.rkt | #f | FN injection |
| `current-reflection-llm-fn` | runtime/memory/reflection.rkt | #f | FN injection |
| `current-embedding-provider` | runtime/memory/embeddings.rkt | #f | FN injection |
| `current-batch-embedding-provider` | runtime/memory/embeddings.rkt | #f | FN injection |
| `current-oauth-http-sendrecv` | runtime/auth/oauth.rkt | #f | FN injection |
| `current-tool-executor` | agent/roles/tool-gateway.rkt | default fn | FN injection |
| `current-remote-tool-executor` | agent/roles/tool-gateway.rkt | default fn | FN injection |
| `current-routing-policy` | agent/roles/tool-gateway.rkt | 'local-only | Policy enum |
| `current-mcp-event-sink` | extensions/mcp-adapter.rkt | void | Event sink |
| `current-hook-violation-callback` | extensions/combinators.rkt | #f | Callback |
| `current-external-backend-enabled` | runtime/memory/backends/external-protocol.rkt | #f | Toggle + DI |
| `current-remote-executor` | sandbox/gateway-bridge.rkt | #f | FN injection |

**Assessment:** DI parameters are the most appropriate use of Racket's parameter
mechanism. They enable test isolation without global state leaks. The rollback-actions
cluster (3 callback params) was already partially consolidated into a config struct
(`rollback-actions-config`) in v0.97.13.

---

### Pattern D: Mutable State (anti-pattern)
**Count:** ~8 parameters
**Shape:** `(make-parameter <value>)` where the parameter is mutated directly
(via `(param new-value)`) outside `parameterize`, used as thread-local mutable state.

| Parameter | Module | Pattern | Risk |
|-----------|--------|---------|------|
| `current-loop-warning-count` | rollback-actions.rkt | Incremented/reset directly | **Medium** |
| `current-rollback-action-log` | rollback-actions.rkt | Append via direct mutation | **Medium** |
| `gsd-wave-gate-counter` | state-machine.rkt | Incremented/reset directly | **Low** |
| `current-gemini-tool-id-counter` | llm/gemini.rkt | Incremented via custom fn | **Low** |
| `current-spawn-timestamps` | spawn-subagent.rkt | Wrapped in box, direct mutation | **Low** |
| `current-process-count` | sandbox/limits.rkt | Reads from box, indirect | **Low** |

**Anti-pattern explanation:** Parameters used as mutable state defeat the
thread-safety guarantee of `parameterize`. When code does `(current-loop-warning-count 0)`
outside a `parameterize`, the mutation affects ALL threads sharing the parameter,
not just the calling thread. This is semantically equivalent to `set!` on a global
variable.

**Severity assessment:**
- `current-loop-warning-count` and `current-rollback-action-log` are **Medium** risk
  because they accumulate state across calls within a single turn. If the agent loop
  ever spawns subthreads for parallel context assembly, these mutations would race.
  Current single-threaded execution makes this safe in practice.
- The remaining parameters are **Low** risk because they are scoped to well-understood
  single-threaded call paths (gemini API calls, GSD wave counting, process limits).

**Recommended future cleanup (NOT in W4 scope):**
1. Replace `current-loop-warning-count` with a turn-local counter passed explicitly.
2. Replace `current-rollback-action-log` with a turn-local accumulator.
3. Consider `box` + explicit threading for mutable parameters.

---

### Pattern E: Registry/Cache (collection state)
**Count:** ~10 parameters
**Shape:** `(make-parameter (make-hash))`, `(make-parameter (make-hasheq))`
**Purpose:** Thread-local registries for event types, serializers, schemas.

| Parameter | Module | Default |
|-----------|--------|---------|
| `current-event-field-registry` | util/event/event-macro.rkt | `(make-hasheq)` |
| `current-event-serializer-registry` | util/event/event-macro.rkt | `(make-hash)` |
| `current-event-deserializer-registry` | util/event/event-macro.rkt | `(make-hash)` |
| `current-event-schema-registry` | util/event/event-macro.rkt | `(make-hash)` |
| `current-schema-version` | util/event/event-macro.rkt | 1 |
| `current-event-migration-registry` | util/event/event-migration.rkt | `(make-hash)` |
| `current-event-reducers` | tui/state-events/registry.rkt | #f |
| `current-embedding-cache` | runtime/memory/embeddings.rkt | cache-state |
| `current-mid-session-persisted-ids` | runtime/session/session-events.rkt | `(set)` |

**Assessment:** Registry parameters are correctly scoped. The event-macro cluster
(5 params) is the densest registry cluster. These use `parameterize` for test
isolation (`R-14` in event-macro.rkt).

---

### Pattern F: CLI Flag (command-line parsing)
**Count:** ~30 parameters
**Shape:** `(make-parameter #f)` inside `command-line` forms
**Purpose:** Parsing command-line arguments in scripts.

| Module | Count | Notes |
|--------|-------|-------|
| scripts/run-benchmark.rkt | 12 | Benchmark CLI |
| scripts/run-dogfood-session.rkt | 3 | Dogfood CLI |
| scripts/capture-regression.rkt | 3 | Regression CLI |
| scripts/lint-release-notes.rkt | 3 | Lint CLI |
| cli/generate-certificates.rkt | 2 | Cert CLI |
| scripts/abstraction-audit.rkt | 4 | Audit CLI |

**Assessment:** CLI parameters are correct usage — they exist only in `command-line`
parsing scope and are never exported or shared. No concerns.

---

### Pattern G: Identity/Context (thread-local binding)
**Count:** ~8 parameters
**Shape:** Various
**Purpose:** Thread-local context binding for error recovery, correlation, session.

| Parameter | Module | Default |
|-----------|--------|---------|
| `current-loop-state-for-error-recovery` | agent/state.rkt | #f |
| `current-blackboard` | agent/blackboard.rkt | #f |
| `current-gsd-ctx` | extensions/gsd/session-state.rkt | gsd-default-ctx |
| `current-gsd-correlation-id` | extensions/gsd/events.rkt | #f |
| `current-gsd-session-id` | extensions/gsd/session-state.rkt | "" |
| `current-session-capabilities` | util/capability.rkt | '(any) |
| `current-iteration-fsm-state` | agent/iteration/main-loop.rkt | state-idle |
| `current-turn-fsm-state` | agent/loop-fsm.rkt | turn-state-emit-start |

**Assessment:** Identity parameters are well-structured. They use `parameterize`
correctly to bind context for the duration of a loop turn or session.

## 3. Cross-Cutting Findings

### 3.1 Parameter Density by Subsystem

| Subsystem | Parameter Count | Densest Module |
|-----------|----------------|----------------|
| runtime/context-assembly | 19 | rollback-actions.rkt (6) |
| runtime/memory | 14 | service.rkt (4) |
| agent/verification | 8 | verifier-core.rkt (5) |
| util/event | 9 | event-macro.rkt (5) |
| sandbox | 12 | gateway-bridge.rkt (5) |
| tui | 10 | (distributed) |
| tools/builtins | 9 | spawn-subagent.rkt (3) |
| extensions | 9 | (distributed) |
| runtime/session | 9 | session-config.rkt (3) |
| scripts (CLI) | 21 | run-benchmark.rkt (12) |

### 3.2 Session-Config Ownership Boundary

`runtime/session/session-config.rkt` is the designated write authority for
feature flags. It defines profiles that `parameterize` the relevant parameters.
Modules outside session-config should NOT mutate feature-flag parameters directly
except in tests (via `parameterize`).

**Current boundary discipline: GOOD.** Production code respects this boundary.
The only direct mutations are the anti-pattern parameters from Pattern D.

### 3.3 Thread-Safety Notes

- Racket parameters are thread-local via `parameterize` — each thread sees
  the parameterized value.
- Direct mutation `(param new-value)` is NOT thread-local — it sets the
  parameter's root/default value, visible to all threads.
- **`current-loop-warning-count`** uses direct mutation (`(current-loop-warning-count 0)`)
  in `warnings->actions` and `turn-context.rkt:239`. This is safe only because
  the agent loop is single-threaded for context assembly.
- **`current-rollback-action-log`** uses direct mutation in `log-action!`.
  Same single-threading assumption applies.

### 3.4 Parameter Grouping Opportunities

The following parameter clusters could benefit from grouping into config structs
(similar to the `rollback-actions-config` pattern established in v0.97.13):

1. **Verifier config cluster** (8 params in verifier-core.rkt + verifier-prompt.rkt):
   `current-verifier-enabled`, `current-verifier-model`, `current-verifier-provider`,
   `current-verifier-risk-threshold`, `current-verifier-timeout-ms`,
   `current-verifier-max-files-shown`, `current-verifier-max-diff-chars`,
   `current-verifier-max-file-lines`
   → `verifier-config` struct

2. **Event registry cluster** (5 params in event-macro.rkt):
   → `event-registry-bundle` struct (already partially achieved via parameterize scope)

3. **Embedding config cluster** (4 params in embeddings.rkt):
   → `embedding-config` struct

These are **future cleanup candidates**, NOT W4 scope. Documented here for
planning visibility.

## 4. Anti-Pattern Summary

| Anti-Pattern | Affected Parameters | Risk | Recommendation |
|--------------|---------------------|------|----------------|
| Mutable state via direct param mutation | `current-loop-warning-count`, `current-rollback-action-log` | Medium | Migrate to explicit turn-local state |
| Unwired feature flag | `current-fuzzy-edit-enabled?`, `current-ui-event-actions-enabled?` | Low | Wire to session-config or remove if unused |
| Parameter with no parameterize consumer | ~15 params | Low | Verify intended usage (some are read-only defaults) |

## 5. Recommended Future Actions (Out of W4 Scope)

1. **Medium priority:** Replace `current-loop-warning-count` and
   `current-rollback-action-log` with turn-local accumulators passed explicitly.
   This eliminates the mutable-state anti-pattern.

2. **Low priority:** Group verifier-core parameters into a `verifier-config` struct
   to reduce parameter density.

3. **Low priority:** Audit ~15 parameters with no `parameterize` consumers to
   determine if they should be plain `define` constants instead.

4. **Low priority:** Consider a `current-feature-flags` compound parameter (struct)
   that session-config populates atomically, reducing individual parameter wiring.

## 6. Inventory Statistics

- **Total production make-parameter sites:** 189
- **By pattern:** Feature Flags (~40), Config Values (~55), DI (~15),
  Mutable State (~8), Registry/Cache (~10), CLI Flags (~30), Identity/Context (~8)
- **Total parameterize write sites:** 59
- **Total modules with parameters:** 78
- **Anti-patterns found:** 2 Medium, 13 Low
- **No changes to production code in W4** — documentation only

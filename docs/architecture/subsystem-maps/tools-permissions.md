# Subsystem map — Tool execution, permissions & sandbox

Derived facts: `../subsystem-manifest.rktd` (subsystems `tools`, `sandbox`).
Security model: [security.md](../../security.md), [mcp-capability-security.md](../../mcp-capability-security.md).

## Purpose & entry points

- Tool contract & registry facade: `tools/tool.rkt`; declarative table
  `tools/registry-table.rkt` (+ `tools/registry-table/core-tools.rkt`); execution
  scheduling `tools/scheduler.rkt` + `tools/scheduler-execution.rkt`; builtins under
  `tools/builtins/` (bash: `tools/builtins/bash.rkt`).
- Subprocess isolation: `sandbox/subprocess.rkt` (custodian-bounded) + `sandbox/limits.rkt`.
- Scratch allocation (ONE trusted per-session root): `tools/scratch.rkt`.
- Permissions/approval: `runtime/approval/broker.rkt`, safe mode allowlist
  (`util/safe-mode/`), worker security surface (`tests/test-worker-security.rkt`).
- Worker/executor: `tools/builtins/spawn-subagent.rkt`, worker advertisement
  `runtime/worker-tool-advertisement.rkt`.

## Capability ownership

| Responsibility | Owner |
|---|---|
| Tool contract/registry | `tools/tool.rkt` (stable public API), `tools/registry-table.rkt` |
| Scheduling/isolation | `tools/scheduler.rkt` + `sandbox/subprocess.rkt` |
| Scratch trust root | `tools/scratch.rkt` (session-tokened; the SAME root for structured writes, bash redirection, executor prompt; remote workers receive none — forged `trusted-scratch-root` is refused) |
| Approval decisions | `runtime/approval/broker.rkt` (authorization is never inferred from prose) |
| Safe mode | read-only allowlist by existing policy; scratch is deliberately NOT a safe-mode exception |
| Hook interception | 47 points, `util/hook-types.rkt`; hook-mutated tool arguments are revalidated |

## Behavioral contracts established by tests

- Scratch allocation/refusals: `tests/test-tool-scratch-allocation.rkt`.
- Worker security (trusted IPC channel, redirection sanctions, permission policy):
  `tests/test-worker-security.rkt`.
- Safe mode: `tests/test-browser-safe-mode.rkt` + safe-mode owners; scheduler behavior:
  `tests/test-scheduler-hooks.rkt`; contract tests for builtins under `tests/` (area `tools`).
- Managed-surface changes fail closed (`./qt` refuses unsupported options before selection —
  see [testing.md](../../testing.md)).

## Provider / consumer relationships

- Providers: the agent loop executes tools through the scheduler; extensions register tools
  through the registry; the executor/worker path shares the scratch root via the trusted IPC
  channel only.
- Consumers: bash redirection policy, structured write/edit tools, executor prompts — all
  bind to the scratch root contract; TUI displays approvals via typed events.

## Boundaries, unresolved dependencies, escalation

- `extensions → sandbox` (14 edges) and `tools → runtime` (23) cross layer directions are
  policy-tracked (see `../dependency-policy.rktd`); the capability denylist in the quick
  admission record is a deliberate conservative surface, not bureaucracy.
- **Broader verification required when:** permission/approval semantics, scratch trust roots,
  safe-mode allowlist, subprocess limits, or hook interception change — security/authority
  surfaces always escalate conservatively (per the amended verification model), including the
  security suite (`tests/test-worker-security.rkt` and the CI `security` lane).

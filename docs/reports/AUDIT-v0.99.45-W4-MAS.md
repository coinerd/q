# Audit: MAS Subsystem — v0.99.45 W4

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w4-mas.rkt`
**Tests:** 68 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `util/capability.rkt` — Capability taxonomy and role→capability mapping
  - `util/message/mas-envelope.rkt` — Inter-agent communication envelope
  - `tools/permission-gate.rkt` — Tool approval gates (strict/permissive)
  - `tools/builtins/spawn-subagent-helpers.rkt` — Pure spawn-subagent functions
  - `skills/mas-workflow.rkt` — MAS workflow skill parsing
  - `agent/registry.rkt` — Agent role registry with version management
  - `agent/registry-types.rkt` — Registry data structures
- Special flags: None

## Test Matrix

| # | Test | Expected | Actual | Status |
|---|------|----------|--------|--------|
| 1 | Valid capabilities set | 10 capabilities defined | ✅ Correct | PASS |
| 2 | valid-capability? predicate | Symbols validated, strings/numbers rejected | ✅ Correct | PASS |
| 3 | all-capabilities excludes 'any | 9 real capabilities | ✅ Correct | PASS |
| 4 | Roles defined | 5 roles: supervisor, planner, verifier, tool-gateway, executor | ✅ Correct | PASS |
| 5 | Supervisor capabilities | read-only, plan-write, memory-write, subagent (no shell-exec/file-write) | ✅ Correct | PASS |
| 6 | Verifier read-only | Only read-only capability | ✅ Correct | PASS |
| 7 | Executor capabilities | shell-exec + file-write (no plan-write) | ✅ Correct | PASS |
| 8 | Tool-gateway capabilities | shell-exec, file-write, git-write, network, browser | ✅ Correct | PASS |
| 9 | role-has-capability? | Correct grants including 'any wildcard | ✅ Correct | PASS |
| 10 | Session capability parameter | Default '(any), parameterize works | ✅ Correct | PASS |
| 11 | Envelope create minimal | Auto-generates IDs and deadline | ✅ Correct | PASS |
| 12 | Envelope create full | All fields preserved | ✅ Correct | PASS |
| 13 | Envelope invalid capability | Raises contract error | ✅ Correct | PASS |
| 14 | Envelope invalid source agent | Raises contract error | ✅ Correct | PASS |
| 15 | Envelope invalid target agent | Raises contract error | ✅ Correct | PASS |
| 16 | Envelope invalid risk level | Raises contract error | ✅ Correct | PASS |
| 17 | Envelope serialization roundtrip | Hash→struct preserves all fields | ✅ Correct | PASS |
| 18 | Envelope hash→invalid | Returns #f for non-hash inputs | ✅ Correct | PASS |
| 19 | Envelope unique IDs | Auto-generated IDs are unique | ✅ Correct | PASS |
| 20 | Envelope risk levels | All 4 levels accepted | ✅ Correct | PASS |
| 21 | Envelope auto-deadline | Auto-deadline is ~5 min in future | ✅ Correct | PASS |
| 22 | Permission gate default config | Strict mode by default | ✅ Correct | PASS |
| 23 | PG auto-approved tools | read, grep, ls, find bypass | ✅ Correct | PASS |
| 24 | PG needs-approval tools | edit, write, bash, spawn-subagent blocked | ✅ Correct | PASS |
| 25 | PG strict unknown requires approval | Unknown tools need approval | ✅ Correct | PASS |
| 26 | PG permissive unknown auto-approved | Unknown tools bypass | ✅ Correct | PASS |
| 27 | PG memory tools auto-approved | list/search/store bypass | ✅ Correct | PASS |
| 28 | PG destructive memory needs approval | delete/clear need approval | ✅ Correct | PASS |
| 29 | PG custom approval callback | Callback invoked, returns result | ✅ Correct | PASS |
| 30 | Normalize capabilities none | #f and '() → #f | ✅ Correct | PASS |
| 31 | Normalize single string | "read-only" → '(read-only) | ✅ Correct | PASS |
| 32 | Normalize single invalid | "bogus" → #f | ✅ Correct | PASS |
| 33 | Normalize list | List of strings → validated list | ✅ Correct | PASS |
| 34 | Normalize list with invalid | Invalid entries filtered | ✅ Correct | PASS |
| 35 | Normalize symbols | Symbol list → validated list | ✅ Correct | PASS |
| 36 | Normalize all-invalid | Returns #f | ✅ Correct | PASS |
| 37 | HITL approval required | shell-exec/git-write trigger | ✅ Correct | PASS |
| 38 | HITL approval not required | read-only/file-write bypass | ✅ Correct | PASS |
| 39 | Summary max chars | 4000 char limit | ✅ Correct | PASS |
| 40 | Extract summary short | Short text unchanged | ✅ Correct | PASS |
| 41 | Extract summary truncation | Long text truncated with ellipsis | ✅ Correct | PASS |
| 42 | Extract summary empty | Empty list → "" | ✅ Correct | PASS |
| 43 | Extract summary custom max | Custom max-chars works | ✅ Correct | PASS |
| 44 | WF parse simple | Single-step pipeline parsed | ✅ Correct | PASS |
| 45 | WF parse multi-step | Multi-step pipeline parsed | ✅ Correct | PASS |
| 46 | WF parse missing agents | Returns error | ✅ Correct | PASS |
| 47 | WF parse empty agents | Returns error | ✅ Correct | PASS |
| 48 | WF parse agents not list | Returns error | ✅ Correct | PASS |
| 49 | WF parse missing task | Returns error | ✅ Correct | PASS |
| 50 | WF template variables | {{var}} extracted correctly | ✅ Correct | PASS |
| 51 | WF result chaining detected | {{result}} chaining detected | ✅ Correct | PASS |
| 52 | WF no result chaining | No chaining when absent | ✅ Correct | PASS |
| 53 | WF parallel flag | "true"/"yes" → #t | ✅ Correct | PASS |
| 54 | WF capabilities parsed | String list → symbol list | ✅ Correct | PASS |
| 55 | Registry register+resolve | Register then resolve active | ✅ Correct | PASS |
| 56 | Registry multiple versions | Multiple versions coexist | ✅ Correct | PASS |
| 57 | Registry activate version | Version switch works | ✅ Correct | PASS |
| 58 | Registry idempotent | Duplicate registration skipped | ✅ Correct | PASS |
| 59 | Registry resolve unknown | Unregistered → #f | ✅ Correct | PASS |
| 60 | Registry resolve specific version | Non-active version resolved | ✅ Correct | PASS |
| 61 | Registry make instance | Factory invoked | ✅ Correct | PASS |
| 62 | Registry version pinning | Pin captures active version | ✅ Correct | PASS |
| 63 | Registry registered roles | All roles listed | ✅ Correct | PASS |
| 64 | Registry make with pin | Pinned version resolved | ✅ Correct | PASS |
| 65 | Registry activate unknown error | Unknown version raises error | ✅ Correct | PASS |
| 66 | RT agent-descriptor | Struct creation and accessors | ✅ Correct | PASS |
| 67 | RT version-pin | Struct creation and accessors | ✅ Correct | PASS |
| 68 | RT registry-entry | Struct creation and accessors | ✅ Correct | PASS |

## Findings

### FINDING-001 (info): Capability taxonomy is well-designed with least-privilege defaults

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The MAS capability system follows least-privilege principles:
- **Verifier** has only `read-only` — cannot modify anything during verification
- **Supervisor** has read, plan-write, memory-write, and subagent (delegation) — no direct shell/file access
- **Executor** has shell-exec and file-write but NOT plan-write — cannot tamper with GSD plans
- **Tool-gateway** has all destructive capabilities (shell, file, git, network, browser) — serves as the privileged execution boundary
- The `'any` wildcard is excluded from `all-capabilities()` — it's a backward-compat shim, not a real grant

**Impact:** Positive — strong defense-in-depth posture. Capability violations are caught at the envelope level before execution.

### FINDING-002 (info): MAS envelope provides comprehensive inter-agent message validation

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The `mas-envelope` struct enforces structural integrity for all inter-agent messages:
- **Capability validation**: Rejects unknown capabilities at construction time
- **Agent validation**: Both source and target must be known roles from `ROLE-CAPABILITIES`
- **Risk levels**: 4-tier risk assessment (low/medium/high/critical)
- **Deadline management**: Auto-generates 5-minute deadline from current time
- **Tracing**: Auto-generates unique message-id and trace-id for distributed tracing
- **Serialization round-trip**: `envelope→hash→envelope` preserves all fields via coercion

**Impact:** Positive — ensures all MAS communication is traceable, validated, and time-bounded.

### FINDING-003 (low): Permission gate uses strict-by-default for unknown tools

**Severity:** Low (info)
**Category:** Security
**Description:** The permission gate defaults to `'strict` mode, where unknown tools require explicit approval. This is the safer default — if a new tool is added to the system but not registered in the auto-approved set, it will require approval rather than silently passing through.

However, the static tool sets (`auto-approved-tools`, `needs-approval-tools`) are hardcoded in `make-default-permission-config`. A tool not in either set falls through to the policy-mode check. In strict mode, this means every new tool requires approval until explicitly added to the auto-approved list.

**Impact:** Low — this is the desired behavior for security, but could slow down development of new tools if the lists aren't updated.

**Recommendation:** Consider a configuration-driven tool classification system instead of hardcoded sets.

### FINDING-004 (info): Agent registry has robust version management

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The agent registry provides:
- **Thread-safe operations**: All mutations protected by semaphore
- **Idempotent registration**: Duplicate version registrations are silently skipped
- **Version pinning**: Sessions pin active versions to prevent mid-session inconsistency
- **Session-active tracking**: Hot-swap activation during active sessions logs a warning and defers
- **Dynamic loading fallback**: Falls back to static factory on any dynamic-require error
- **Identity verification**: Dynamically-loaded agents are verified against `agent-role?` predicate

**Impact:** Positive — prevents race conditions and ensures session stability.

## Remediation Items

None required. MAS subsystem is well-structured with clear separation of concerns between capability taxonomy, message envelopes, permission gates, and the stateful registry.

## Architecture Summary

The MAS subsystem is organized in four layers:

1. **Capability layer** (`util/capability.rkt`):
   - 10 valid capabilities, 5 agent roles
   - Least-privilege role→capability mapping
   - Session-level capability parameter for runtime filtering

2. **Communication layer** (`util/message/mas-envelope.rkt`):
   - Structured envelope with tracing, deadline, risk assessment
   - Hash serialization with coercion for schema evolution
   - Validation at construction time (capability, roles, risk levels)

3. **Approval layer** (`tools/permission-gate.rkt`, `tools/builtins/spawn-subagent-helpers.rkt`):
   - Strict/permissive policy modes for unknown tools
   - HITL approval for dangerous capabilities (shell-exec, git-write)
   - Pure helper functions for capability normalization and text extraction

4. **Registry layer** (`agent/registry.rkt`, `agent/registry-types.rkt`):
   - Thread-safe registration with semaphore protection
   - Version management (register, activate, pin, resolve)
   - Dynamic loading via namespace-attach-module for hot-swap
   - Session-aware: defers hot-swap during active sessions

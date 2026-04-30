# GSD Architecture

Internal architecture documentation for the GSD (Get Shit Done) extension rewrite.

## Module Structure

```
extensions/gsd/
├── state-machine.rkt     # Central state machine with explicit transitions
├── runtime-state-types.rkt # Canonical gsd-runtime-state struct
├── command-types.rkt     # gsd-command-result (gsd-ok/gsd-err) structs
├── policy.rkt            # Unified policy engine (gsd-decide-action)
├── events.rkt            # Event telemetry (stable event names + correlation IDs)
├── plan-types.rkt        # gsd-plan, gsd-wave, gsd-task, normalized-plan IR + parsing
├── plan-types-parser.rkt # Markdown parsing for plan types
├── plan-validator.rkt    # Mechanical validation (raw + normalized)
├── wave-executor.rkt     # Wave lifecycle tracking + error recovery
├── wave-docs.rkt         # Wave document generation
├── archive.rkt           # Plan archival (gsd-command-result returns)
├── core.rkt              # Command dispatch, tool guard, write guard, transactions
├── context-bundle.rkt    # Role-specific context assembly
├── steering.rkt          # Mode-aware stall detection
├── prompts.rkt           # Prompt templates for all GSD phases
└── bash-detect.rkt       # Bash file-reading bypass detection

extensions/
├── gsd-planning.rkt          # Legacy extension (unchanged API)
└── gsd-planning-state.rkt    # DEPRECATED shim → pure delegation to gsd/ modules
```

## State Machine

**Module:** `extensions/gsd/state-machine.rkt`

Single global state machine with semaphore-protected transitions.

```
States: idle, exploring, plan-written, executing, verifying

Transitions:
  idle        → exploring
  exploring   → plan-written
  exploring   → idle
  plan-written → executing
  plan-written → exploring
  executing   → verifying
  executing   → exploring
  verifying   → idle
  verifying   → executing
```

All transitions are validated. Invalid transitions return `(err-result reason from to)`.

### API

- `(gsm-current)` — Current state symbol
- `(gsm-transition! target)` — Attempt transition, returns ok-result or err-result
- `(gsm-reset!)` — Reset to idle
- `(gsm-valid-next-states)` — List of valid target states from current
- `(gsm-tool-allowed? tool-name)` — Check tool against per-state blocklist
- `(gsm-snapshot)` — Immutable hash of current state + transition history

## Plan Types

**Module:** `extensions/gsd/plan-types.rkt`

| Struct | Fields |
|--------|--------|
| `gsd-plan` | waves, raw-markdown, metadata, validation-errors |
| `gsd-wave` | index, title, status, root-cause, files, dependencies, verify, notes |
| `gsd-task` | description, file, action-type |
| `validation-result` | valid?, errors, warnings |

Parsing: `parse-waves-from-markdown` extracts waves from PLAN.md headers.

## Plan Validator

**Module:** `extensions/gsd/plan-validator.rkt`

Strict validation that blocks `/go`:

- **Error rules:** no waves, missing wave title, no file references
- **Warning rules:** no verify command, no root-cause documentation
- `valid-plan->go?` — Quick check: valid? and no errors

## Wave Executor

**Module:** `extensions/gsd/wave-executor.rkt`

Mutable executor wrapping a `gsd-plan`:

```
wave-status states: pending → in-progress → completed | failed | skipped
```

Key design decision (DD-5): **Failed waves don't block subsequent waves.** The executor tracks each wave independently and provides `next-pending-wave` to find the next executable wave.

## Write Guard

**Module:** `extensions/gsd/core.rkt`

Returns `policy-decision` (not hasheq). During `executing` mode, blocks modifications to `.planning/PLAN.md` and `.planning/` tree. Uses path normalization via `simple-form-path` to prevent `..` traversal attacks (DD-6). Routes through `gsd-decide-action` in policy.rkt.

## Bash Detection

**Module:** `extensions/gsd/bash-detect.rkt`

Detects bash commands that read file contents (sed, cat, head, tail, awk, python open, etc.), bypassing the read tool's tracking. Safe commands (git, ls, find, raco, make) are excluded.

## Event Telemetry

**Module:** `extensions/gsd/events.rkt`

Stable event taxonomy following `gsd.<category>.<action>` convention:
- `gsd.command.received`, `gsd.command.completed` — command lifecycle
- `gsd.mode.changed` — state transitions
- `gsd.plan.parsed`, `gsd.plan.normalized`, `gsd.plan.validated` — pipeline stages
- `gsd.plan.archived`, `gsd.archive.failed` — archival
- `gsd.transaction.rollback` — transaction failures

Events carry correlation IDs via `current-gsd-correlation-id` parameter.
`emit-gsd-event!` publishes to both the GSD event bus and the session event bus.

## Transaction Wrappers

**Module:** `extensions/gsd/core.rkt` (`with-gsd-transaction`)

Snapshot-rollback semantics for multi-step commands:
- Captures `gsm-snapshot` before execution
- On exception, rolls back to snapshot state
- Used by `cmd-wave-done` and `cmd-done` (archive)

## Archive

**Module:** `extensions/gsd/archive.rkt`

`archive-completed-plan!` validates wave completion, moves plan files to
archive directory, and resets GSD state. Returns `gsd-command-result`
(`gsd-ok` with archive-path/files-archived data, or `gsd-err` with reason).

## Migration Layer (DEPRECATED)

**Module:** `extensions/gsd-planning-state.rkt`

Pure delegation shim that preserves the old public API while delegating to the new modules:

| Old API | New Module |
|---------|------------|
| `(gsd-mode)` | `(gsm-current)` |
| `(set-gsd-mode! v)` | `(gsm-transition! target)` |
| `(reset-all-gsd-state!)` | `(gsm-reset!)` |

Scheduled for removal in a future version.

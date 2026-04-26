# GSD Architecture

Internal architecture documentation for the GSD (Get Shit Done) extension rewrite.

## Module Structure

```
extensions/gsd/
├── state-machine.rkt     # Central state machine with explicit transitions
├── plan-types.rkt        # gsd-plan, gsd-wave, gsd-task structs + parsing
├── plan-validator.rkt    # Mechanical validation before /go
├── wave-executor.rkt     # Wave lifecycle tracking + error recovery
├── core.rkt              # Command dispatch, tool guard, write guard
├── context-bundle.rkt    # Role-specific context assembly
├── steering.rkt          # Mode-aware stall detection
├── prompts.rkt           # Prompt templates for all GSD phases
└── bash-detect.rkt       # Bash file-reading bypass detection

extensions/
├── gsd-planning.rkt          # Legacy extension (unchanged API)
└── gsd-planning-state.rkt    # Thin shim → delegates to gsd/ modules
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

During `executing` mode, the write guard blocks modifications to `.planning/PLAN.md`. Uses path normalization to prevent `..` traversal attacks (DD-6).

## Bash Detection

**Module:** `extensions/gsd/bash-detect.rkt`

Detects bash commands that read file contents (sed, cat, head, tail, awk, python open, etc.), bypassing the read tool's tracking. Safe commands (git, ls, find, raco, make) are excluded.

## Migration Layer

**Module:** `extensions/gsd-planning-state.rkt`

Thin shim that preserves the old public API while delegating to the new modules:

| Old API | New Module |
|---------|------------|
| `(gsd-mode)` → `#f / 'planning / 'plan-written / 'executing` | `(gsm-current)` → `'idle / 'exploring / 'plan-written / 'executing` |
| `(set-gsd-mode! v)` | `(gsm-transition! target)` with multi-step for legacy paths |
| `(reset-all-gsd-state!)` | `(gsm-reset!)` + clear legacy boxes |

Budget, read-count, and wave-tracking functions remain in the shim for backward compatibility. These will be removed in a future version.

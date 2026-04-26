# GSD User Guide

**Get Shit Done** — q's structured workflow for complex tasks.

## Overview

GSD is a plan-driven workflow that guides the agent through exploration, execution, and verification phases. It replaces ad-hoc prompting with a structured approach that:

- Separates planning from execution
- Validates plans before committing
- Handles failures gracefully (skip + continue)
- Provides observability at every step

## Commands

### `/plan [description]`

Start the planning phase. The agent explores the codebase freely to understand the problem, then writes a structured plan to `.planning/PLAN.md`.

```
/plan Fix the login timeout bug in auth.rkt
```

During planning:
- **No tool restrictions** — read, edit, bash are all available
- **No time/call limits** — explore as deeply as needed
- Write the plan when ready using `planning-write` for `PLAN`

### `/go [wave number]`

Begin executing the plan. The agent follows the plan's waves sequentially.

```
/go          # Start from first pending wave
/go 2        # Start from wave 2
```

During execution:
- **Write guard active** — cannot modify `.planning/PLAN.md` (use `/replan`)
- **Tool guard active** — `planning-write` is blocked
- Each wave is tracked: pending → in-progress → completed/failed/skipped

### `/replan`

Return to planning mode from `plan-written` or `executing` state. Use when the plan needs fundamental changes.

```
/replan
```

### `/skip <wave number>`

Skip a failed or unwanted wave during execution. The agent continues to the next wave.

```
/skip 2
```

### `/reset`

Reset GSD to idle state. Clears all mode, budget, and tracking state.

```
/reset
```

### `/gsd`

Show current GSD status: mode, wave progress, and valid next commands.

```
/gsd
```

## Plan Format

Plans are Markdown files in `.planning/PLAN.md`. Each wave has:

```markdown
## Wave 0: Fix the bug

**Root cause:** The timeout handler uses milliseconds instead of seconds.

**Files:** `auth.rkt`, `auth-test.rkt`
**Verify:** `raco test tests/test-auth.rkt`
```

### Validation Rules

Before `/go` is allowed, the plan is validated:

| Rule | Level | Description |
|------|-------|-------------|
| Has waves | **Error** | Plan must contain at least one wave |
| Wave has title | **Error** | Each wave needs a descriptive title |
| Wave has files | **Error** | Each wave must list file references |
| Wave has verify | Warning | Each wave should have a verify command |
| Wave has root-cause | Warning | Each wave should document the root cause |

## State Machine

```
idle → exploring → plan-written → executing → verifying → idle
         ↑              ↑             ↑
         └──────────────┴─────────────┘  (/replan)
```

| State | Write Guard | Tool Guard |
|-------|-------------|------------|
| idle | None | None |
| exploring | None | None |
| plan-written | Blocks edit/write/bash | None |
| executing | Blocks planning-write | Blocks planning-write |
| verifying | Blocks edit/write/bash | Blocks edit/write/bash + planning-write |

## Tips

1. **Write detailed plans** — the more context in each wave, the better the execution
2. **Include verify commands** — automated verification catches regressions
3. **Keep waves small** — 1-3 files per wave is ideal
4. **Skip, don't retry** — if a wave fails, skip it and fix in a later wave
5. **Use /gsd frequently** — check progress and valid next states

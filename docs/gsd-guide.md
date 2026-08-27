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

**Per-wave budget (v1.00.03).** Each wave runs with a bounded runtime budget
(default **3600 s** = 1 hour). Override per campaign, in precedence order:

1. `/go --wave-timeout=SECONDS` flag (e.g. `/go --wave-timeout=7200 3` starts
   wave 3 with a 2-hour budget; the wave number stays the **last** token);
2. `wave-timeout-seconds` key in `~/.q/config.json` (applies to all campaigns
   from that machine);
3. the built-in default (3600 s).

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

Plans are Markdown files in `.planning/PLAN.md`. The supported grammar is the
**plan index**: an index section at the top of `PLAN.md` with one row per wave
(explicit status bracket required), plus one wave doc per row under
`.planning/waves/`:

```markdown
## Waves

- [Inbox] W0: Fix the bug → waves/W0-fix-the-bug.md
- [Inbox] W1: Harden the retry path → waves/W1-harden-retry.md
```

Each wave doc (`waves/W0-....md`) contains the wave body:

```markdown
**Root cause:** The timeout handler uses milliseconds instead of seconds.

**Files:** `auth.rkt`, `auth-test.rkt`
**Verify:** `raco test tests/test-auth.rkt`
```

### Deprecation: inline plan format (BUG-0035)

The legacy **inline** grammar — `## Wave N:` sections written directly in
`PLAN.md` — is deprecated. Status-less relaxed index rows (`- W0: Title`
without a `[Status]` bracket) are deprecated alongside it.

- Since v1.00.21, `/go` and `/gsd status` emit one-line non-fatal deprecation
  warnings when a plan uses inline sections or relaxed rows. Loading and
  execution are unaffected; the warnings name the index skeleton to migrate to.
- **Roadmap:** index+status (`- [Inbox] W0: Title → waves/W0-slug.md`) becomes
  the single supported grammar; inline-format removal is targeted after
  v1.00.20. The removal itself is out of scope for the deprecation change —
  new plans should simply be authored in the index format.

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

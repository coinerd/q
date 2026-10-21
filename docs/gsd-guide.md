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
Production TUI/SDK requests automatically dispatch a fresh independent receipt-pinned
reviewer before delivery; explicit rejection allows one durable implementation retry
by default. Review does not replace operator merge authorization. See the
[automatic review contract and limits](automatic-review.md).

```
/go          # Start from first pending wave
/go 2        # Start from wave 2
```

**Executor scratch directory (sandbox-write-scratch-parity W1).** Each /go
request allocates an opaque session token and the executor prompt designates
`.planning/scratch/<token>/` (relative to the executor's current working
directory; worktree-isolated executors resolve it inside their retained wave
worktree) as the place for diagnostic scripts and probes. Create probe files
with the structured write tool — missing parents are created automatically —
not with shell redirection, which stays restricted to its own sanctioned
scratch root, and never under `/tmp`, which stays denied. Scratch content is
disposable: never reference it as a deliverable or verification receipt, and
never commit it. Untracked files under `.planning/scratch/` are excluded from
delivery evidence and do not block delivery synchronization; every other
dirty state still refuses synchronization.

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
This changes planning mode only: it does not rewrite a frozen campaign contract,
replace receipts or replenish consumed author/evidence allowances.

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
  the single supported grammar; inline-format removal is targeted after v1.00.23.
  The removal itself is out of scope for the deprecation change —
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

### Automatic verification repair

A coordinator-owned declared `Verify` command failure automatically returns to
an executor for the **same wave**, carrying the command, exit status, log path,
and bounded failure output. It preserves existing work and checks all frozen
wave deliverables rather than treating a focused test pass as completion.
Provider retries supplement these diagnostics instead of replacing them; the
latest verifier failure is also persisted for a later resume.

The default allows **three repair attempts**; the internal
`current-gsd-wave-verification-repair-retries` policy parameter can lower this
bound (zero disables repairs). An unchanged committed repair head stops early.
Each repair reruns the complete declared Verify chain. Cancellation, exhausted
budgets, or non-repairable provenance failures stop the campaign without
advancing. A local pass still does not waive protected delivery or independent
review. Completed verification clears the running status, even on rejection.

### Governed supplemental evidence recovery

An exhausted evidence-author allowance stays spent. The typed
`evidence-author-allowance-exhausted` stop is not another automatic retry, and
`/replan`, a new task ID, failure or crash cannot reset it. A supplemental task
requires separate exact-digest operator decisions for task **T**, committed
candidate **K** and receipt supersession **S**. Historical frozen Verify **C0**
stays intact; effective **C1** only appends the approved probe to the existing
focused stage, retaining all five gates. Fresh complete owned GREEN and genuine
baseline-overlay assertion RED/same-candidate GREEN are mandatory. Protected
promotion **P** then links the certified successor receipt to its one-use evidence
registration; only authenticated readiness `ready` admits fresh review. None of
these steps grants merge authorization or delivery.

The scoped worker receives an owner-authenticated task descriptor and replacement prompt
naming exactly its two approved new test/helper paths (relative and absolute), retained
physical working directory, test-author phase and baseline/current source identities.
It authors those files and returns normally. Commits, canonical generation, C1 Verify,
baseline probes, approvals and publication belong to the owner, not the worker. Historical
wave Files and ordinary implementation/Git/shell/scratch instructions confer no authority.
The same contract remains in re-anchors and continuations.

Tool schemas are projected session-locally through a trusted generic runtime policy:
installed permitted read tools plus scoped write/edit, without changing other sessions'
registry. Forged calls still face normal capability/hook admission; final effective
arguments after hook mutation are checked by the same scope policy. Bounded, redacted,
control-character-safe denials distinguish forbidden tools/paths, unsafe paths, invalid
scope and infrastructure failures. They do not unlock owner tools or implementation.
The descriptor grants no launch: existing authority, fences, expiry and cancellation
remain unchanged. This repair cannot retry the spent recovery child or create a grandchild.

The owner CLI is `scripts/gsd-evidence-replan.rkt` with `--verb`
`propose|authorize|inspect|status|revoke`; authorization selects
`--stage task|candidate|supersession`. Proposals and inspection are not grants.
Authority uses the trusted `~/.q/agent/supplemental-evidence` store shared with
`/go`, never project configuration. `/go` does not provision it. The operator
runtime must own trusted HOME and keep author/reviewer tools outside the private
owner/broker boundary; private modes are not a hostile same-user sandbox.

Standalone CLI authorization/revocation refuses (exit 2) without a same-process
interactive broker channel. There is no batch, stdin, TTY or policy auto-approval
route. The trusted keyboard-only TUI handoff supports
`/evidence-replan authorize-task PLAN WAVE TASK-SHA256`; `/go` then requests
candidate and supersession approval separately. To revoke, wait for quiescence
and use `/evidence-replan revoke KIND APPROVAL-SHA256`, where KIND is
`task-approval`, `candidate-approval` or `supersession-approval`. Inspect the exact
approval first; the overlay binds the retained one-target revocation proposal's
full digest. Deny to inspect that proposal, then repeat the revoke command.
Only genuine interactive selection and protected grant consumption durably revoke;
shared/model command dispatch cannot invoke this route. Revocation launches nothing,
resets no allowance, and makes downstream admission/validation/recovery refuse
without redispatch. Fixture coverage is not installation or live authorization;
remaining engineering acceptance gates still apply. See the
[governance contract and frontend limits](automatic-review.md#governed-supplemental-evidence-replan-bug-0088).

Changing models does not revive a spent supplemental launch. An eligible original task
blocked at revision 3 may have one separately broker-approved recovery child, with identical
scope/context/baseline/C0/C1/bounds and explicit parent/pointer lineage. No grandchildren,
allowance reset, old approval reuse, or ordinary-author fallback is permitted. Retain it with
`--verb propose-recovery --campaign-root ROOT --plan PLAN --wave N --sha256 PARENT-T --expires-at UNIX-SECONDS`,
inspect NEW-T, then use `/evidence-replan authorize-recovery PLAN WAVE NEW-T-SHA256` in the owner TUI.
This grants one NEW bounded invocation and installs a permanent claimed pointer migration;
it launches nothing. Invoke `/go` separately after the genuine decision. Provider choice
remains operator-selected, not a quota guarantee. Exact clean source, unrevoked terminal
parent, current failed allocation/fences and no cancellation/budget pause are required;
K, full C1/G1, real R1, S, P/readiness and fresh independent review still follow normally.

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

## Configuration

Stall-threshold tuning is a settings edit, not a PR cycle (BUG-0044).
The stall watchdog that guards each wave's run against mutation stalls
reads its thresholds from the project/global settings file
(`.q/config.json`), keyed under `gsd.stall`:

| Key | Default | Meaning |
|-----|---------|---------|
| `gsd.stall.soft-limit` | 8 | Repeated identical tool calls before a soft re-anchor steering is injected |
| `gsd.stall.hard-limit` | 15 | Repeated identical tool calls before the attempt is failed with an explicit stall cause |
| `gsd.stall.window` | 30 | Lookback window (in tool calls) over which repetition is counted |
| `gsd.stall.backstop` | 300 | Absolute tool-call cap before the attempt is failed, regardless of repetition |

Example:

```json
{
  "gsd": {
    "stall": {
      "soft-limit": 3,
      "hard-limit": 6,
      "window": 20,
      "backstop": 200
    }
  }
}
```

Semantics:

- **Keys absent** → the defaults above (8/15/30/300).
- **Invalid values** (non-positive integers) → silently fall back to the
  defaults with a warning; a typo'd settings file can never crash a
  campaign mid-wave.
- **`#f` disables** a limit (e.g. `"soft-limit": null` disables the
  soft re-anchor).
- **Keyword overrides** on `run-campaign-wave`
  (`#:stall-soft-limit`, `#:stall-hard-limit`, `#:stall-window`,
  `#:stall-backstop`) always win over settings.
- The **effective** thresholds (after override resolution) are logged
  once per wave startup, so operators can see what is active without
  reading source.

## Tips

1. **Write detailed plans** — the more context in each wave, the better the execution
2. **Include verify commands** — automated verification catches regressions
3. **Keep waves small** — 1-3 files per wave is ideal
4. **Skip, don't retry** — if a wave fails, skip it and fix in a later wave
5. **Use /gsd frequently** — check progress and valid next states

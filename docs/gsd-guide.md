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

**Executor scratch directory (one trusted per-session root).** The runtime
allocates exactly ONE owner-only scratch root per session,
`<platform-temp>/q-agent-scratch/<session-token>/` (`/tmp/q-agent-scratch/...`
on POSIX), and the executor prompt advertises that exact absolute path. The
same root is the one non-project location usable by the structured `write` and
`edit` tools (missing parents are created after authorization), by the trusted
worker's allowed roots, and by the bash redirection policy. The allocation
container itself, sibling sessions, repository paths, and arbitrary `/tmp`
paths are outside the sanctioned root; `mkdir` is unnecessary. Direct bash
treats a redirect there as a destructive command: the default `warn` policy
warns and still executes, while destructive blocking (the `block` policy or
safe mode) and the sandbox worker refuse it with the sanctioned path in the
message. Do not rely on warn mode as a write barrier. The token is opaque
(crypto-random hex, or the runtime's per-request token) and never a model
value; retries and worker requests within one session reuse the same root.

Scratch is unavailable in safe mode. Safe mode is read-only: its allowlist
blocks `bash`, `edit`, `write` and `delete-lines`, and a session scratch root
is deliberately not a safe-mode write exception — `read`, `write`, `edit` and
`delete-lines` all refuse it and stay bounded by the project root. Do not plan
scratch work for a `--safe` run.

The trusted scratch root travels only to the local worker. A remote executor
request is built without a trusted scratch root, so remote execution has no
sanctioned scratch destination by omission; there is no fallback to the
allocation container, `/tmp`, or a coordinator-local path.

The /go anchor prompt submitted for a fresh session deliberately names no
scratch path: it is a display/legacy fallback for an unbound session, and the
campaign interpreter advertises the exact per-attempt root in each wave prompt
it actually runs (see [architecture](gsd-architecture.md#scratch)).

Scratch is disposable: never reference it as a deliverable, approval, or
verification receipt, never commit it, and never delete another session's
scratch directory. Because it lives outside the checkout it never appears in
delivery evidence. A leftover `.planning/scratch/<token>/` directory from an
older session is no longer created, advertised or evidence-exempt by the
executor, and the delivery tooling no longer grants it any tolerance: an
untracked in-tree path is ordinary working-tree dirt and blocks
synchronization until it is moved or removed by its owner.

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

Each wave doc (`waves/W0-....md`) is one full document stored VERBATIM by
`planning-write` — the writer adds nothing. It MUST begin with the two-line
machine-managed header: exactly one `# Wave <N>` H1 followed by exactly one
`Status: Inbox` line (substitute the wave's own index in the H1: `# Wave 0`
for W0, `# Wave 2` for W2). No second `Status:` line may appear anywhere in
the document. The remainder is the wave body:

```markdown
# Wave 0
Status: Inbox

## Root Cause
The timeout handler uses milliseconds instead of seconds.

## Files
- File: auth.rkt
- File: tests/test-auth.rkt

## Action
Convert the timeout budget to seconds and update the unit test.

## Verify
raco test tests/test-auth.rkt

## Done
- Timeout budget expressed in seconds; unit test updated and passing
```

### Required wave-doc contract (one source)

The machine header and the required sections are ONE contract owned by
`extensions/gsd/wave-docs.rkt` (`WAVE-DOC-*` constants); the planning prompt
template is generated from those constants and `/go`'s wave-doc lint validates
against the same values. A planning prompt and the validator therefore cannot
disagree about the format.

| Part | Requirement | Violation |
|------|-------------|-----------|
| Header | exactly one `# Wave <N>` H1, then exactly one `Status: <status>` line | `status-header` |
| Second status | no second `Status:` line anywhere in the body | `duplicate-status` |
| `## Files` | at least one `- File: <path>` bullet (singular, no prose) | `files` |
| `## Verify` | non-empty (see the grammar below) | `verify` |
| `## Done` | non-empty | `done` |

Header statuses are the canonical vocabulary (plus documented legacy aliases
`COMPLETED`, `PENDING`, `NOT STARTED`, `IN PROGRESS`). An unknown status is a
typed rejection (`exn:fail:wave-status`) naming the value and the accepted
spellings; it is never silently treated as `pending`. Writers emit canonical
spellings only.

### Verify grammar (`## Verify`)

`## Verify` is read by ONE grammar (`verify-command-grammar.rkt`, re-exported
by `verify-command-format.rkt`) shared by live execution, the frozen offline
binding and the review-proof retention layer. It is deliberately not a shell
parser, and `set -e` does not validate a pipeline: an ambiguous declaration
refuses instead of guessing.

Accepted declaration shapes:

- **Backticked bullets** (`- ` / `* ` / `+ ` lines). Only backticked spans are
  gates; any prose outside them is ignored. One rejected span voids the whole
  declaration — there is no partial green.
- **Single-line shell contract** (no newline). The text is passed through
  byte-for-byte as one stage (legacy behavior), including `&&` chains that the
  conservative splitter can parse.
- **Fenced or unfenced multiline.** Stages are split on newlines and `&&` in
  document order and executed as one fail-fast `&&` chain (`a && b && c`), so a
  terminal zero exit cannot mask an earlier failing stage. A non-command line
  in an unfenced section is a named rejection, never a launch through `/bin/sh`.

A span is executable only when its first token names an accepted tool
(`racket`, `raco`, `bash`, `sh`, `python`/`python3`/`pytest`, `npm`/`pnpm`/`yarn`,
`make`, `git`, `cd`, `env`, `test`, `printf`), or it starts with `./` or
`<project-base>`, or it uses the POSIX `[ ... ]` test form. Inside an accepted
span the payload is not reinterpreted, so `bash -c '...'` is the supported way
to express a compound check.

Named rejections and refusals (each carries the offending span or the
structural reason, and a safe next action):

| Reason | Kind | Meaning |
|--------|------|---------|
| `unsupported-command` | rejection | first token is outside the accepted vocabulary (`ls`, `sed`, `cat`, `echo`, `node`, ...) |
| `grep-exit-semantics` | rejection | bare `grep`/`egrep`/`fgrep`/`rg` (exit 0/1/2 cannot distinguish no-match from error) |
| `inverted-command` | rejection | `! cmd` inverts an arbitrary exit status |
| `bash-only-construct` | rejection | `[[ ... ]]` under `/bin/sh` semantics |
| `ambiguous-shell-span` | rejection | semicolon, pipe, `&`, `$`, `<`, `>` inside a span the grammar cannot verify |
| `prose-in-unfenced-multiline` | rejection | an English line in an unfenced multiline section |
| `empty-declaration` | refusal | nothing declared |
| `no-executable-command` | refusal | only prose / no backticked gate |
| `ambiguous-grammar` | refusal | dangling `&&`, unbalanced or quoted delimiters, continuations, subshells, keywords |

The examples below are the contract's own regression set: the docs-contract
test (`tests/test-gsd-docs-contracts.rkt`) parses this block and checks every
line against the shared parser. Format: `<ACCEPT|REJECT reason> | <declaration>`
where the declaration uses the two characters `\n` for a line break.

<!-- verify-grammar-examples:start -->
```text
ACCEPT | raco test tests/test-auth.rkt
ACCEPT | raco make a.rkt && raco test tests/a.rkt
ACCEPT | ./scripts/check.sh
ACCEPT | bash -c 'test "$(grep -c TOKEN file)" -eq 0'
ACCEPT | - `raco make a.rkt` compiles cleanly
ACCEPT | - `racket tests/a.rkt` passes
REJECT unsupported-command | - `ls -l` lists the directory
REJECT grep-exit-semantics | - `grep TOKEN file` finds the token
REJECT inverted-command | - `! grep -q TOKEN file` proves absence
REJECT bash-only-construct | - `[[ -f a.rkt ]]` exists
REJECT ambiguous-shell-span | - `for f in tests/*.rkt; do racket $f; done` green
REJECT prose-in-unfenced-multiline | racket tests/a.rkt\nbad line here
REJECT empty-declaration |
REJECT no-executable-command | - all focused tests pass
REJECT ambiguous-grammar | raco make a.rkt &&
```
<!-- verify-grammar-examples:end -->

Never rely on `sed -i`, `echo >`, or shell pipelines inside `## Verify`: they
are named rejections, and the coordinator runs the declaration through the
owned verification lane, not your interactive shell.


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
`propose|propose-recovery|propose-disposition|authorize|inspect|status|revoke`; authorization selects
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
scope/context/baseline/C0/C1/bounds and explicit parent/pointer lineage. Outside the fixed exception below, no grandchildren,
allowance reset, old approval reuse, or ordinary-author fallback is permitted. Retain it with
`--verb propose-recovery --campaign-root ROOT --plan PLAN --wave N --sha256 PARENT-T --expires-at UNIX-SECONDS`,
inspect NEW-T, then use `/evidence-replan authorize-recovery PLAN WAVE NEW-T-SHA256` in the owner TUI.
This grants one NEW bounded invocation and installs a permanent claimed pointer migration;
it launches nothing. Invoke `/go` separately after the genuine decision. Provider choice
remains operator-selected, not a quota guarantee. Exact clean source, unrevoked terminal
parent, current failed allocation/fences and no cancellation/budget pause are required;
K, full C1/G1, real R1, S, P/readiness and fresh independent review still follow normally.

### Sole fixed disposition exception

The immutable amendment permits exactly one additional v2→v3 task for campaign
`79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e`, wave 1,
with spent parent `77538def70739c9260a0920705a7174baf2f326dbbffb1e560d4b57c2fccc629`
and original root `30be1b8ecd87ee21246d19473a338a571f3e0ef13dea7fe214824163280ae31c`.
The parent terminal BODY digest is
`209accd0ca0f80a7f6d3fc562baeedafcb96396d98019a1ae861f160dfa5ca17`;
the COMPLETE-record digest is
`7412b190a534cd291eb676a10bcbee75196f3f5fd3b42f2c04d5924cec94ae40`.
These are distinct namespaces, not interchangeable evidence or authority.

Use `--verb propose-disposition --campaign-root ROOT --plan PLAN --wave 1 --sha256 PARENT-T --expires-at UNIX-SECONDS`
to retain an inert proposal, inspect its full digest, then use the keyboard-only
`/evidence-replan authorize-disposition PLAN 1 NEW-T-SHA256`.
A fresh genuine broker T decision is mandatory; chat, amendment identity, proposal
retention and false/denied grants authorize nothing. Installation launches no author.
Do not submit `/go` automatically. K and S require separate later broker decisions;
no old grant reuse, auto-authorization, retry, budget reset or ordinary fallback exists.
One permanent claim admits only one winner, including after expiry, revocation or failure.
The ordinary v1→v2 rules are unchanged; no generic v3 successor is permitted.

#### Amendment-2 (exhausted fixed disposition)

Amendment-1 above is exhausted and retained only for provenance; its permanent claim
is never rewritten. The exhausted amendment-2 ID is
`645a6a88de529fa239edbda468b8c12e64df1c0c1fbff859db4a9f284e242505`.
It binds spent v3 proposal
`f4238535e291e23062926f698c922da75fd3caadcd2de160e8b3ca68b33ba764` / approval
`f8ea96b875e8b05fc2f5800674270c6dddf93b4e256fa6b8fc1ed650fcd5b816`, blocked/rev3,
launch consumed, allocation attempt-5 / attempt-fence-8 / coordinator-fence-8.
The terminal BODY digest is
`b60c5f76528c76ff66ce98943633c93fed570e80b60a99192150d178c86d8259`; COMPLETE digest is
`0c9e45d27cfb3934c29dc9bdc44686b62a6011ddf21c08d8c507beaf88e6e5c0`.
Root/v2 ancestry, snapshot, receipt, clean source and owned proof port remain unchanged.

`propose-disposition` now targets only this v3 parent; the keyboard authorization
route is unchanged and still requires fresh genuine exact-T consent before a separate
`/go`. The task remains kind/v3 with unchanged recovery ancestry and bounds
1/14400/600/86400; only the disposition binding and engine provenance change.
A separate amendment-keyed permanent claim permits one winner and one dispatch,
not a retry, generic chain, fourth-generation schema or fourth amendment.
Old amendment proposals refuse; K and S remain separate decisions, with full
C1/G1, baseline RED/candidate GREEN R1 and P/registration/readiness mandatory.
BUG-0089 must be merged and installed before any live author attempt.

#### Amendment-3 (active fixed late disposition)

Amendment-2 is exhausted and retained only for provenance; its permanent claim is
never rewritten. The active amendment-3 ID is derived by the policy module from its
public fields (it is never hardcoded). It binds spent v3 proposal
`15e68c8b1ddd822a969b18ebfb7e22c425a0f74e0a68ec48a2b712d1cd05e49d` / approval
`ccf20bc98f7688b4bb5fa69226c54a876038c88fd28a8fe476dcad9ae7e1f9ac`, its consumed
attempt-6 / attempt-fence-9 / coordinator-fence-9 lineage, and the spent
amendment-2 parent pair as immutable prior ancestry. The live attempt-6 operation
expired after reaching `verifying`, so the owner-deadline terminal writer sealed it
`blocked` at revision 6 with exactly the verifying-prefix facts
`allocation dispatch-id candidate-ref candidate-approval-ref verify-job block-reason`.
The terminal BODY digest is
`758d653547879fe5204785af73acf1f7850810ad61e72bc9664532fd3d7068b1`; the COMPLETE
canonical-record digest is
`837710077fec71c156ac65ee86075d2c1d847270617056cbf34a1a7e1a7160c4`.

Admission is generation-fixed: only this fixed third amendment may accept that
late blocked/revision-6 shape, while amendment-1/2 retain the blocked/revision-3
launch-spent shape. It is not a generic late-terminal recovery, and no fourth
successor (`successor-permitted` #f), retry, reallocation, budget reset or second
dispatch is authorized. The owner disposition route additionally admits a wave
parked at `awaiting-delivery` only when that exact authenticated late terminal and
the amendment's own current attempt/fences match; every cancellation, budget-pause,
snapshot, receipt, clean-source and owned-proof check still applies, and admission
never mutates the live wave status.

## Campaign lifecycle

This is the ONE campaign-lifecycle diagram. `/go` evaluates it once per pass:
one decision, one effect, one fenced commit. ADR-0011 and
`gsd-architecture.md` link here instead of repeating it.

```
          .planning/PLAN.md + wave docs
                     │  bound once: manifest + snapshot digest
                     ▼
            durable campaign record ────► decide-campaign-action  (pure, one action)
                     ▲                                 │
                     │                                 ├─ complete  all waves done/deferred, delivery proven
                     │                                 ├─ launch    next actionable wave (lease + attempt fence)
                     │                                 ├─ resume    one named lane effect, same attempt/fence
                     │                                 ├─ finalize  authenticated merge proof ⇒ wave DONE
                     │                                 ├─ block     typed denial (no fallback dispatch)
                     │                                 └─ cancel    durable cancellation
                     │                                 ▼
                     └──── fenced commit + projections (PLAN.md row / wave header / STATE.md)
```

The campaign record is authoritative; `PLAN.md`, wave headers and `STATE.md`
are projections. One pass yields exactly one of `complete`, `launch`,
`resume`, `finalize`, `block` or `cancel`. Decisions choose behaviour from
typed kinds, statuses, wave indices and one route symbol — never from a
human-readable message.

### Recovery table

| Condition | What q does | What you may safely do |
|-----------|-------------|------------------------|
| Status-only edit to a live wave/PLAN row | Non-semantic: normalization excludes status markers, generated headers/prefixes and the `## Last Failure` span (its heading through the line before the next `#`/`##` heading); the SAME bound campaign resumes | Nothing. Run `/go` again. |
| Structural edit (wave rows, titles, declared paths, plan body, `FILES`, constraints) | Refuses closed with the stored/recomputed/bound/live hashes and paths; never rebuilds or re-binds the snapshot | Stop editing, then create a corrected plan/scope as a NEW campaign. Never hand-edit durable campaign state to make it match. |
| Missing/malformed frozen scope or `FILES` list | Refuses with the missing or non-literal scope class | Create a corrected new scope (new campaign) or use the existing explicit drift procedure. Deleting campaign state is not a repair. |
| Malformed `## Verify` in a frozen wave | Named reason with the offending span, identical for live and frozen consumers | Correct the wave doc in a NEW scope; frozen evidence is never rewritten. |
| Lease held by another live session | No launch. The OS advisory lock is authoritative; a lock FILE on disk alone proves nothing | Wait for the owner, or stop that session. There is no force-release command, by design. |
| Interrupted attempt (crash, kill, timeout) | Replay from the durable journal/fences; an in-flight author, review, verification or supplemental attempt is never duplicated | Run `/go` again. Do not delete journal, receipt or fence files. |
| Retained supplemental evidence blocks the wave | The secondary/idempotent seal runs first, then a typed denial; no ordinary author/coordinator fallback | Complete the owner T/K/S decisions (`/evidence-replan …`) or leave it retained. Never invent approvals. |
| Review rejection | One durable retry with the findings by default; the new attempt gets a fresh session, attempt and fence | Let the retry run, or `/replan`. Do not rewrite review evidence. |
| Delivery/merge proof missing or stale | `block`/`cancel` with the concrete reason; the wave is never marked DONE | Complete external delivery (merge authorization is operator-owned), then `/go`. |
| Cancelled campaign | `cancel`: terminal, nothing is dispatched | Start a new campaign. `/reset` only changes planning mode; it does not rewrite durable state. |
| Interrupted `/done` archive | Archiving only renames: files already moved are complete under `.planning/archive/<slug>/`, the rest are still complete in `.planning/`, no temporary or owned staging artifact is created, and unknown directories are untouched | Re-run `/done` (or `/done --force`) once the obstruction is cleared; do not hand-recreate or delete moved files. |
| Unknown empty `.planning/<dir>` after `/done` | Preserved, never deleted: only the known `waves` directory or a directory an authentic ownership marker proves q created may be cleaned, and only when nothing else is left in it | Keep it if you want it. Only q-created leftovers are ever removed, so an unknown directory is yours. |

The `## Last Failure` exclusion is span-exact: it starts at the `## Last
Failure` heading and ends immediately before the next `#` or `##` heading (the
hashes followed by a space or tab), or end of file. A `###` subheading does not
end it. Everything inside that span — including `###` lines — is generated
failure context, not authored input, so it is invisible to drift detection. Do
not put authored content you need compared inside the span, and do not rely on
a `###` heading to cut the exclusion short.

## Mode machine

The planning/execution mode guards are a separate machine from the campaign
lifecycle above (see the diagram there for campaign routing):
`idle → exploring → plan-written → executing → verifying → idle`, with
`/replan` returning to `exploring`.

| Mode | Write Guard | Tool Guard |
|------|-------------|------------|
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

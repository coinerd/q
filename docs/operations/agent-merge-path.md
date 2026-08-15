# Agent Merge Path — Landing Agent-Authored PRs onto Protected `main`

Status: canonical (BUG-0010)
Audience: any autonomous agent (or human) that must land a fix/document wave on
protected `main`. This is the **standing, versioned procedure** — no wave may improvise
its own landing chain.

The procedure is implemented by the **github extension** (`extensions/github/`, tool
reference: [README](../../extensions/github/README.md)). All three steps are invoked as
extension tools; there is no manual `gh`/`git` ad hoc chain.

---

## The Procedure at a Glance

```
gh-wave-start {issue_number}
      │  create/reuse  feature/issue-<N>-wave  from main
      ▼
[ implement ]  edit files in the working tree (on the wave branch)
      ▼
gh-wave-finish {issue_number, files[], commit_msg, pr_title, pr_body}
      │  commit files → push branch → open PR (body: "Closes #N")
      │  best-effort inline squash-merge (usually skipped: checks still pending)
      ▼
[ wait ]  required checks green on the PR
      ▼
gh-pr {action: "merge", method: "squash", number: <PR>}
      │  land on main — the ONLY way main advances in this procedure
      ▼
[ post ]  PR ↔ issue ↔ commit linkage verified; landing SHA recorded in the registry
```

Golden rule: **`main` only ever advances via the squash-merged PR.** The procedure never
pushes to `main` directly; the only `git push` is the wave branch in step 2.

---

## Step 1 — Start the wave: `gh-wave-start`

```json
{"issue_number": 9325}
```

Behavior (from `extensions/github/tool-handlers.rkt`):

- Derives the canonical branch name from the wave template: `feature/issue-<N>-wave`.
- If the branch already exists locally, it is **reused** (re-running after an
  interruption is safe and expected); otherwise it is created from the current `main`
  and checked out.
- Milestone/labels are whatever the wave issue already carries — no side effects.

Prerequisite: local `main` freshly pulled (`git checkout main && git pull`) so the wave
branches from the true tip.

## Step 2 — Implement and open the PR: `gh-wave-finish`

Write/edit the wave's files in the working tree (you are on the wave branch), then:

```json
{
  "issue_number": 9325,
  "files": ["docs/operations/agent-merge-path.md", "extensions/github/README.md"],
  "commit_msg": "<conventional commit subject>\n\n<body>",
  "pr_title": "<conventional commit subject>",
  "pr_body": "Closes #9325\n\n## What\n...\n\n## Verification\n..."
}
```

Behavior:

1. Stages exactly `files[]` (explicit allow-list — nothing else is committed), commits
   with `commit_msg`, pushes the branch (`git push -u origin feature/issue-<N>-wave`).
2. Opens one PR: title `pr_title`, body `pr_body`, base `main`.
3. Attempts an inline squash-merge immediately. On a fresh PR the required checks are
   still pending, so **this best-effort merge is expected to be skipped** — landing
   happens in step 3. (Known quirk: the handler closes the wave issue regardless;
   harmless — see Failure Modes.)
4. Checks out `main` and pulls it clean.

**The PR body MUST contain `Closes #<issue>`** (or `Fixes`/`Resolves`). This is what
links the PR to the issue, auto-closes it on merge, and lets `project-automation.yml`
move the board card (see "Automation gate" below).

## Step 3 — Land: `gh-pr` squash merge

Wait until the PR's required checks are green (12 checks — see table below), then:

```json
{"action": "merge", "method": "squash", "number": 9333}
```

This runs the equivalent of `gh pr merge <N> --squash` and lands the changes on
protected `main` as a single squash commit. If the merge is rejected because `main`
moved (strict up-to-date rule), see Failure Modes.

---

## Post-Conditions (verify after every landing)

| # | Post-condition | How to verify | Feeds |
|---|----------------|---------------|-------|
| 1 | PR is linked to the issue and the issue is closed | PR body contains `Closes #N`; issue state `CLOSED` after merge | audit trail, board |
| 2 | Registry row updated | `.planning/bugs/BUG-NNNN-*.md`: `Status: fixed`, `Fixed in: <milestone> (#<PR>, <wave>)`; mirrored in `.planning/bugs/INDEX.md` | BUG-0008 release-readiness gate (W2) |
| 3 | Landing SHA recorded | squash-commit SHA on `main` noted in the bug file / release tracker | readiness gate, traceability |
| 4 | `main` advanced by squash merge only | `git log main` shows the `(#<PR>)` squash commit; no direct-push commits | audit trail |

The landing SHA matters because the release-readiness gate (see
[release-runbook.md](release-runbook.md)) requires every registry bug fixed **in the
milestone** before a tag is cut — the recorded SHA is the evidence of what landed and
when.

---

## Prerequisites

### Token scopes

| Scope | Required for | Notes |
|-------|--------------|-------|
| `repo` | everything (issues, PRs, contents, push of the wave branch) | the standing credential for this procedure |
| `workflow` | **only** if the wave's commit touches files under `.github/workflows/` | GitHub rejects pushes containing workflow files without it; most waves (docs/code) never need it |

Authentication uses the standard `gh` CLI credential resolved by the extension
(`GITHUB_TOKEN`); auth-path issues were BUG-0006 (fixed) and are out of scope here.

### How branch protection admits this path

`main` is protected with:

- **12 required status checks** (strict, `enforce_admins`): `lint`, `security`,
  `release-dry-run`, `smoke (ubuntu-latest)`, `test (0/1/2)`, `test-aggregate`,
  `test-platform`, `workflows (0/1)`, `workflows-aggregate`.
- **No merge queue.** Admission is simply: all checks green + branch up to date.
- The merge is performed by the same identity that opened the PR; no human approval
  requirement is configured, so the path works unattended.

Therefore the procedure needs **no exception, allow-list, or administrator override** —
it waits for the same checks any human PR waits for.

### Automation gate

`.github/workflows/project-automation.yml` does **NOT gate merges** — it is a read-only
board-sync workflow ("q Roadmap & Delivery") that runs after PR/issue events and maps
`fixes|closes|resolves #N` in a PR body onto board Done. It is `continue-on-error` and
cannot block a landing. Conclusion for this procedure: **no additional automation gate
is required**; the only contract with it is the `Closes #N` line in the PR body.

---

## Audit Trail

Every wave leaves a three-way linkage, all on GitHub:

```
wave issue #N  ←──(PR body "Closes #N")──  PR #M  ──(squash)──►  main commit <sha> "(#M)"
```

- **Issue ↔ PR:** the `Closes #N` line (GitHub cross-reference + auto-close; board card
  moves to Done).
- **PR ↔ commit:** the squash commit subject ends with `(#M)`.
- **Landing SHA:** recorded locally in the bug registry (post-condition 3) as the
  readiness-gate evidence.

Registry files under `.planning/` are intentionally not part of the PR (`.planning/` is
gitignored); the registry update is a local post-condition, not a commit.

---

## Failure Modes

| Symptom | Cause | Recovery |
|---------|-------|----------|
| Push rejected at `gh-wave-finish`, message mentions `workflow` | commit includes `.github/workflows/**` and the token lacks the `workflow` scope | add the scope, or split workflow files into a separate wave that has it |
| Inline squash-merge at finish time skipped | required checks still pending (expected on a fresh PR) | not an error — wait for green, then step 3 |
| Issue closed by `gh-wave-finish` although PR is still open | handler closes the wave issue unconditionally (known quirk) | benign — the PR body's `Closes #N` keeps the linkage; on merge the states converge to closed/merged |
| `gh-pr` merge rejected: required checks not green | step 3 ran too early | wait (`gh pr checks <N> --watch`), retry the same call |
| `gh-pr` merge rejected: branch out of date (strict rule) | `main` moved since the branch was cut | `git rebase main` on the wave branch → `git push --force-with-lease` → checks re-run → retry step 3 |
| `gh-wave-start` finds an existing branch | previous run of the same wave | intended — reuse it, continue at step 2 |

---

## Retry, resume, and idempotence (BUG-0011)

Transient failures no longer stall a wave on a human `/retry`, and an interrupted run can
be resumed without re-executing completed steps.

**Auto-retry (turn/LLM/tool).** Failures classified as *transient* — provider 5xx, network
timeouts, SSE stalls/reconnectable provider errors — are retried automatically at the
agent-loop level with the same policy shape the LLM layer already uses: bounded
max-attempts with exponential backoff (`q/runtime/auto-retry.rkt`; classification in
`q/llm/provider-errors.rkt`). When the budget is exhausted the error surfaces exactly as
before. Non-transient errors (bad arguments, auth, 4xx policy) fail immediately — no
behavior change.

**Durable checkpoints.** `gh-wave-start` / `gh-wave-finish` record each completed step as
`- [x] <step>` under a `### <wave-id>` block in `.planning/STATE.md`. Steps are appended
in execution order; a resumed run reads the checklist and **skips** steps already checked,
instead of re-executing them.

**Idempotent milestone actions.**
- `gh-pr create` / PR creation inside `gh-wave-finish` is lookup-first: if an open PR
  already exists for the head branch it is returned, never duplicated. Re-running the
  command for the same head branch succeeds both times with a single PR.
- `gh-wave-finish` is a no-op success when (a) the step is already checked in
  `STATE.md`, or (b) the exact change is already committed (tree/content check on the
  declared `files` list) — it will not double-commit.

**Operational consequence:** after any interruption, re-run the wave commands; completed
steps report "already recorded / already committed" and the run proceeds from the first
incomplete step. No operator double-checking of remote state is required.

---

## Dogfooding

This document itself landed via this exact procedure: `gh-wave-start` on issue #9325 →
`gh-wave-finish` (this file + the extension README) → `gh-pr` squash merge. Its squash
commit on `main` is the audit record, and subsequent waves (W5+) are expected to replay
the identical steps with zero improvisation — that repetition is the acceptance test for
BUG-0010.

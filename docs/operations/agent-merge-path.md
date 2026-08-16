# Agent Merge Path — Landing Agent-Authored PRs onto Protected `main`

Status: canonical (BUG-0010; hotfix #9346 quarantine update)
Audience: any autonomous agent or human that must land a change on protected `main`.

The GitHub extension provides issue management and wave setup. Finalization is an
**external authenticated PR workflow**: the in-product `gh-wave-finish` tool is
quarantined and cannot publish or land changes.

---

## Procedure at a Glance

```
gh-wave-start {issue_number}
      │  create/reuse feature/issue-<N>-wave from main
      ▼
[ implement ]  edit and verify the wave in the working tree
      ▼
external authenticated PR workflow
      │  commit intended files → push wave branch → open linked PR
      │  wait for required checks → squash-merge through branch protection
      ▼
[ post ]  verify PR ↔ issue ↔ commit linkage and record the landing SHA
```

Golden rule: **`main` advances only through a protected, squash-merged PR.** Never push
directly to `main`.

## Step 1 — Start the wave: `gh-wave-start`

```json
{"issue_number": 9325}
```

The tool derives `feature/issue-<N>-wave`, updates local `main`, and creates the wave
branch. Start with a clean working tree. If setup cannot update `main` or create the
branch, it fails rather than continuing on an uncertain base.

## Step 2 — Implement and verify

Make only the intended changes on the wave branch. Run the relevant formatter, compiler,
and focused tests before handing the branch to the publishing workflow.

## Step 3 — Publish and land externally

Use the approved external workflow with an authenticated GitHub identity. That workflow
must:

1. verify the intended file set and repository root;
2. create the commit and push only the wave branch;
3. open a PR against `main` whose body contains `Closes #<issue-number>` (or the
   equivalent `Fixes`/`Resolves` keyword);
4. wait until all required checks pass and the branch is up to date;
5. squash-merge the PR through GitHub branch protection; and
6. report the PR number and landing SHA for post-condition checks.

Authentication, mutation, retry, and audit controls belong to that external workflow.
Do not improvise a local direct-push chain.

### `gh-wave-finish` quarantine contract

`gh-wave-finish` remains registered for compatibility, but it is not a publishing step.
Its schema accepts exactly these required arguments and no extras:

```json
{
  "issue_number": 9325,
  "files": ["path/to/file.rkt"],
  "commit_msg": "fix: example"
}
```

- `issue_number` must be a positive integer (numeric strings and zero are invalid).
- `files` must be a non-empty list of safe relative paths.
- `commit_msg` must be a non-empty string.

After validating those arguments, the handler **always fails before any filesystem,
git, planning, or GitHub mutation** and directs the caller to the external authenticated
PR workflow. A quarantine error is expected and is not evidence that any publishing
step completed.

---

## Post-Conditions

| # | Post-condition | Evidence |
|---|----------------|----------|
| 1 | PR is linked to the issue and the issue closes on merge | PR body contains `Closes #N`; merged PR and closed issue on GitHub |
| 2 | Registry row is updated | Bug entry records fixed status, milestone, PR, and wave |
| 3 | Landing SHA is recorded | Squash commit SHA from protected `main` |
| 4 | `main` advanced only through the PR | Git history shows the GitHub squash commit; no direct-push commit |

The three-way audit linkage is:

```
wave issue #N ←── PR body "Closes #N" ── PR #M ── squash ──► main commit <sha>
```

## Authentication and Branch Protection

The external workflow must use an authenticated GitHub credential with repository
access. It also needs workflow-file permission when a commit changes
`.github/workflows/**`.

`main` remains protected by required status checks and strict up-to-date enforcement.
The external workflow must satisfy those controls; quarantine is not an exception or
an administrator bypass.

## Failure and Recovery

| Symptom | Recovery |
|---------|----------|
| `gh-wave-finish` returns a quarantine error | Expected. Confirm it made no mutation, then use the external authenticated PR workflow. |
| Required arguments are rejected | Supply a positive integer `issue_number`, non-empty safe relative `files`, and non-empty `commit_msg`; the valid call will still fail closed. |
| Push is rejected for workflow scope | Use an external credential with workflow-file permission, or split the workflow change according to policy. |
| Required checks are pending or failing | Wait or fix the branch externally; do not merge until checks pass. |
| Branch is out of date | Update the wave branch in the external workflow, push it, and let checks rerun. |
| Publishing is interrupted | Resume using the external workflow's authenticated state and audit record; do not treat an in-product quarantine response as a checkpoint. |

## Automation Gate

`.github/workflows/project-automation.yml` synchronizes issue/PR state with the project
board and does not replace branch protection. The `Closes #N` line remains required for
issue linkage and board automation.

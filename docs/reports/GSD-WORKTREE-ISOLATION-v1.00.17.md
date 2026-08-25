# GSD Worktree Isolation — Design Record v1.00.17

- **Date:** v1.00.17, Wave W6 (#9512 part a)
- **Scope:** `q/extensions/gsd/wave-executor.rkt` (worktree lifecycle),
  `q/extensions/gsd/go-orchestrator.rkt` (executor integration),
  `q/tests/test-gsd-wave-worktree.rkt` (Layer 1/2 lifecycle tests)
- **Predecessor:** `.planning/waves/W6-worktree-execution.md`; root-cause
  analysis W4 ("done" waves existed only as uncommitted working-tree
  mutations; concurrent campaigns contaminate each other's diffs and gates).

## 1. Problem

Wave executors ran directly in the shared project checkout:

1. "done" waves existed only as uncommitted working-tree mutations;
2. later waves ran baselines against a tree they did not own;
3. concurrent campaigns contaminate each other's diffs and gates.

## 2. Design

Each wave *attempt* runs inside its own git worktree on a fresh campaign
branch. The shared checkout is never mutated by an attempt.

### 2.1 Naming and placement (HARD constraint)

For wave attempt *N* of campaign *C* (plan-id = 64-hex SHA-256):

- Worktree path: `<project-parent>/wt-campaign-<hash8>-w<N>`
- Branch: `campaign/<hash8>/w<N>` where `<hash8>` = first 8 hex chars of the
  campaign plan-id, downcased.
- Base ref: the **current** `origin/main` (override for tests:
  `#:base-ref`).

**Placement constraint (hard):** the worktree MUST be a **sibling of the
project root** — never `/tmp`, never inside the repo. This exact path shape
is load-bearing: executor scripts resolve `Q-DIR` as `<cwd>/../q`, so a
worktree at `<parent>/wt-…` keeps that resolution identical to the real
checkout (CI parity). The path is derived *by construction* from the repo
root (`wave-worktree-dir` = `(build-path (path-only repo) name)`) — no other
location input exists, so no `/tmp` default can regress.

Repo-root resolution (`find-repo-root`) supports both observed layouts, with
base-dir precedence: `<base>/.git`, then `<base>/q/.git` (the two-tier
project layout).

### 2.2 Lifecycle

```
campaign start (run-campaign!)
  └─ reclaim-orphaned-worktrees! (scoped to campaign, best-effort, logged)

wave attempt (run-campaign-wave, else-branch)
  ├─ make-wave-worktree!        (also reclaims this campaign's stale
  │                              worktrees first — crash-idempotent)
  ├─ executor session           (current-directory = worktree path)
  │    .planning/ resolves to the REAL project root
  └─ cleanup-wave-worktree!     (after terminal outcome: merged or abandoned)
```

- `make-wave-worktree!` → `git worktree add -b campaign/<h8>/w<N> <dir>
  origin/main`. Raises `exn:fail` with captured git stderr when git fails.
- `cleanup-wave-worktree!` → `git worktree remove --force` + `git branch -D`.
  **Best-effort:** every failure is caught and logged; it returns a status
  hash (`'ok?`, `'worktree-removed?`, `'branch-removed?`, `'errors`) and
  NEVER raises, so it can never mask the wave outcome.
- `reclaim-orphaned-worktrees!` → enumerates `git worktree list --porcelain`,
  matches the sibling directory-name shape `wt-campaign-<hash8>-w<N>`, and
  removes worktree + branch. Scoped (`#:campaign-id`) reclaims only that
  campaign; unscoped reclaims every campaign worktree (full campaign wipe).
  Unrelated worktrees (any other name) are never touched.

### 2.3 Executor integration (go-orchestrator.rkt)

When isolation is enabled, `run-campaign-wave` wraps the executor
invocation (`run-with-timeout-retry`) so that:

- `current-directory` is **parameterized to the worktree path** — sessions
  spawned by the host runner factory inherit it as their cwd;
- **base-dir — and therefore `.planning/` — stays the REAL project root.**
  Canonical campaign state (record, projections, leases) is shared, never
  per-worktree. Executor prompts already receive base-dir separately.
- Worktree creation failure **falls back to the shared checkout** with a
  logged warning (campaign liveness over isolation purity);
- Cleanup runs in the dynamic-wind post-thunk, i.e. after the terminal
  outcome (any result, timeout, or exception) and never masks it.

`run-campaign!` passes `#:isolate?` through and reclaims orphans at campaign
start (after acquiring the lease, before wave selection).

### 2.4 Feature flag

- Setting: `gsd.worktree-isolation`, parameter `current-gsd-worktree-isolation`.
- **Default OFF** until the integration bake (W8) flips it ON after
  verification. `worktree-isolation-enabled?` reads the parameter;
  the `#:isolate?` keyword (`run-campaign!`, `run-campaign-wave`) overrides
  for tests and for callers that need to force either mode.

## 3. Failure modes

| Failure | Behavior |
|---|---|
| `git worktree add` fails (bad ref, disk, git missing) | `make-wave-worktree!` raises `exn:fail` with captured stderr; orchestrator logs a warning and runs the attempt in the shared checkout (legacy path) |
| Cleanup fails (locked worktree, branch checked out elsewhere) | Logged inside the status hash `'errors`; never raises; wave outcome preserved |
| Attempt crashes between add and cleanup | Worktree + branch orphaned; reclaimed on the NEXT campaign start (`run-campaign!`) or by the next `make-wave-worktree!` for the same campaign (reclaim-then-add is idempotent) |
| Stale worktree dir already exists for the same campaign+wave | Reclaimed (removed + branch deleted) before the fresh add |
| Another campaign's / unrelated worktree present | Never touched by a scoped reclaim; only an explicit unscoped reclaim removes other campaigns' worktrees |

## 4. Verification

`cd <project root> && racket q/tests/test-gsd-wave-worktree.rkt` — Layer 1
(pure: naming, sibling-placement construction, command shape, flag default)
runs always; Layer 2 (real throwaway git sandbox) covers: creation on a
fresh branch off origin/main; sibling placement on the real filesystem;
executor cwd = worktree while `.planning/` resolves to project root; writes
never leak to the shared checkout; cleanup removes worktree AND branch,
is idempotent, never raises; scoped/unscoped orphan reclaim with unrelated
worktrees surviving; git-failure surfacing (`exn:fail?`,
`exn:fail:contract?` for argument errors).

## 5. Open items (W8 bake)

- Verifier side: the delivery verifier still inspects the shared checkout;
  with isolation ON its evidence (branch, changed files, verify command)
  must come from the worktree/branch — flip the flag default ON only after
  that integration is verified end-to-end.

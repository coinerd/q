# GitHub Extension (`extensions/github/`)

GitHub integration tools for the q agent, built on the `gh` CLI + git. All handlers
live in `tool-handlers.rkt` (with per-domain modules under `handlers/`) and are plain
functions over argument hashes (they can be invoked standalone from Racket as well as
through the q tool layer).

## Tools

### `gh-issue` — issue lifecycle
Create, read, and manage tracker issues (one wave = one issue).

| action | parameters | purpose |
|--------|-----------|---------|
| `create` | `title`, `body`, `labels[]?`, `milestone?` | file the wave issue |
| `get` | `number` | read state/body (e.g. to check auto-close) |
| `update` | `number`, `title?`, `body?`, `state?` | edit or close |
| `comment` | `number`, `body` | annotate |

### `gh-pr` — pull requests and **landing on protected `main`**
| action | parameters | purpose |
|--------|-----------|---------|
| `create` | `title`, `body`, `head?`, `base?` | open a PR |
| `get` | `number` | read state/checks |
| `list` | — | open PRs |
| `merge` | `number`, `method` (`merge`/`squash`/`rebase`) | **land the wave — canonical method: `squash`** |
| `comment` | `number`, `body` | annotate |

### `gh-milestone` — release milestone hygiene
| action | parameters | purpose |
|--------|-----------|---------|
| `create` | `title`, `description?`, `due-on?` | open the release milestone |
| `list` | — | enumerate (used by readiness checks) |
| `close` | `number` | close after release |

### `gh-board` — project board
| action | parameters | purpose |
|--------|-----------|---------|
| `list-items` | `project?`, `column?` | read "q Roadmap & Delivery" items |
| `move-card` | `issue-number`, `column` | manual card moves (the `project-automation.yml` sync usually covers this) |

### `gh-wave-start` — begin a wave
**Parameters:** `issue_number`

Derives and checks out `feature/issue-<N>-wave` (created from `main`, or reused if it
already exists — restart-safe). Step 1 of the merge path.

### `gh-wave-finish` — quarantined (fail closed)
**Required parameters:** `issue_number` (positive integer), `files[]` (non-empty safe
relative paths), `commit_msg` (non-empty string). No additional parameters are accepted.

This in-product tool validates those required arguments and then **always returns an
error before any repository, git, planning, or GitHub mutation**. It cannot commit,
push, open, merge, or close a PR or issue. Use the external authenticated PR workflow
to publish and land wave changes.

## Where each tool fits in the merge path

The canonical landing procedure is documented in
[docs/operations/agent-merge-path.md](../../docs/operations/agent-merge-path.md):

```
gh-issue create            →  file the wave issue (pre-wave)
gh-wave-start              →  branch feature/issue-<N>-wave
[ implement ]              →  edit files in the working tree
external authenticated PR  →  commit, push, open PR, wait for checks, and merge
gh-issue get               →  verify auto-close (post-condition)
```

Do not call `gh-wave-finish` as a publishing step: its quarantine guarantees that it
fails before mutation. See the operations guide for the external workflow boundary.

`gh-milestone` and `gh-board` support the surrounding release workflow (milestone
hygiene for the readiness gate, board visibility) rather than the landing itself.

## Files

- `tool-handlers.rkt` — entry point; the six `handle-gh-*` tool handlers.
- `tool-schemas.rkt` — argument schemas for the six tools.
- `helpers.rkt` — shared `gh`/git plumbing (e.g. `gh-success-json`).
- `handlers/` — per-domain implementation modules (e.g. `pr-ops.rkt` for `gh-pr`).
- `README.md` — this file.

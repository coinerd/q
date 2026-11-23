# CL5 W2 — offline coverage-gap audit

Read-only, deterministic audit of the `./qt quick` changed-file selector's ownership coverage.
Tool: `scripts/cl5/ownership-audit.rkt`. Tests: `tests/test-cl5-ownership-audit.rkt`.
Issue #9982. No policy change, no execution of the audited tests, no GitHub Actions.

## How to reproduce

```
racket -e '(require (file "scripts/cl5/ownership-audit.rkt")) (run-audit (current-directory))'
# or with machine-readable output:
racket -e '(require (file "scripts/cl5/ownership-audit.rkt")) (run-audit (current-directory) #:json-out "out.json")'
```

Inputs are the repository's existing metadata only — the audited owner table
(`current-admitted-owners`), the generated `@covers` manifest (`load-coverage-manifest`), and the
impact graph (`build-dependency-graph`). No second source of truth is introduced.

## Result at `a7e14a25`

Rows are the **complete production-module universe**, not only modules that appear in the manifest. A
module with no declared detector is `unverified` — the broadest risk category — so it can no longer be
structurally zero (review #7088040257).

| Class | Count | Meaning |
|---|---|---|
| audited owners (selectable universe) | 23 | what `./qt quick` can run |
| declared `@covers` tests | 226 | what the repository claims detects what |
| **production modules (denominator)** | **813** | every `.rkt`/`.ss`/`.rktl` outside `tests/`, `scripts/`, `docs/`, `artifacts/`, `.github/` |
| declared-covered production modules | 170 | at least one declared detector (directly or transitively) |
| **confirmed gaps** | **146** | declared detectors exist, **none is directly selectable** |
| potential gaps | 17 | audited detector exists, ≥ 3 production dependents, no consumer escalation |
| covered | 7 | audited detector, low fan-in |
| **unverified** | **643** | **no declared detector at all** — the tool cannot say who detects this module |
| declared `@covers` targets outside production | 32 | docs/scripts/tests targets, listed rather than silently dropped |
| audited owners without `@covers` | 2 | always included on every change (fail-open noise) |
| graph parse failures | 0 | dependency metadata is parseable |
| manifest drift | 0 missing, 0 stale, 0 extra | the manifest is fresh, so this is not staleness |

Definitions are deliberate: a **confirmed gap** means *no directly selectable declared detector* — a
transitive consumer test may still run, but that is not detection of this module's own behaviour.
`unverified` means *no declaration exists*, not that the module is untested.

### Confirmed gaps by risk area (21 at risk ≥ 5)

| Risk | Confirmed gaps | Examples |
|---|---|---|
| authorization | 10 | `runtime/credentials/protocol.rkt`, `runtime/context-selection/authority.rkt`, `extensions/gsd/policy.rkt`, `runtime/memory/policy.rkt` |
| security | 3 | `sandbox/subprocess.rkt`, `extensions/gsd/review-audit-format.rkt` |
| persistence | 4 | `runtime/session/session-store.rkt`, `runtime/session/session-store-goal-task.rkt`, `extensions/gsd/plan-snapshot.rkt` |
| recovery | 4 | `extensions/gsd/delivery-recovery.rkt`, `runtime/task-memory/replay.rkt` |
| provider | 6 | `llm/model.rkt`, `runtime/context-assembly/*` |
| ordering | 4 | `extensions/gsd/events.rkt`, `runtime/session/session-events.rkt` |
| shared state | 6 | `extensions/gsd/campaign-state.rkt`, `tui/state.rkt` |
| none of the above | 109 | ordinary modules |

### Unverified modules by risk area (643 total)

| Risk (path heuristic) | Count |
|---|---|
| authorization | 32 |
| security | 17 |
| persistence | 10 |
| recovery | 6 |
| provider | 62 |
| ordering | 44 |
| shared state | 34 |
| none of the above | 438 |

The risk classifier is a **path-substring heuristic for prioritisation only**; it is not an audited
security classification. The 21 risk≥5 confirmed gaps must be reviewed manually before any ownership
repair, and the 49 authorization/security `unverified` modules are the first manual-review queue.

112 confirmed gaps have exactly **one** declared cover. Two gaps exist because their only cover, the
archival audit owner `tests/test-audit-v09945-w3-gsd.rkt`, was retired from the selectable table in
`9f292394` (#9970).

### Top potential gaps (audited detector, high fan-in, no consumer escalation)

`util/error/errors.rkt` (44 dependents), `runtime/context-assembly/task-conclusion.rkt` (15),
`agent/state.rkt` (13), `runtime/working-set.rkt` (12), `agent/queue.rkt` (9), `browser/types.rkt`
(9), and 11 more.

## What the audit does and does not prove

Proven (mechanically, from the repository's own metadata):

- a changed module in the confirmed-gap class has declared detectors that `./qt quick` cannot run,
  so a change to it can produce a selection with no detector for that module (146 modules, 21 at
  risk≥5);
- 643 production modules declare no detector at all, so ownership evidence for them is simply absent;
- the escalation rule cannot see this state (it inspects declared covers, not selectable ones);
- audited owners without `@covers` are included on every change, adding unrelated tests.

Not proven:

- that `@covers` entries are behaviourally accurate (they are declarations, never execution-verified
  by this audit);
- that each confirmed-gap module would actually regress undetected (that needs the corpus/CI
  experiment, currently blocked by the GitHub Actions billing restriction);
- that quick is used as a verdict anywhere in production policy today — the code documents it as a
  reproduction aid, which is exactly the assumption CL5 is testing.

## Verification

`tests/test-cl5-ownership-audit.rkt` pins the classification rule, the risk ordering, the cover
inversion, the full-denominator arithmetic (`unverified + declared-covered = production`), and both
repository-level controls:

- **negative control**: `extensions/gsd/wave-status.rkt` is a confirmed gap whose covering tests
  include `tests/test-gsd-wave-status.rkt` (D5); the guard escalates on it with `unadmitted-cover`
  and names that test;
- **positive control**: `extensions/gsd/campaign-decision.rkt` is not a confirmed gap and its
  audited detector is present.

A passing synthetic control is not proof of comprehensive defect detection; it only shows the guard
refuses to claim a green when selectable ownership is missing.

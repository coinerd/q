# Release tracker (v1.00.01)

Milestone #882 (open, 8 open issues), referred to here as (v1.00.01).

Registry source of truth: `.planning/bugs/` (local, gitignored — this file is the
in-repo mirror of the tracker mapping so release tooling does not depend on the
local planning tree).

| BUG-ID | Issue | Title (short) | Severity | Priority | Registry file |
|---|---|---|---|---|---|
| BUG-0007 | [#9322](https://github.com/coinerd/q/issues/9322) | Release pipeline no fail-fast before full suite | medium | medium | `BUG-0007-release-no-fail-fast.md` |
| BUG-0008 | [#9323](https://github.com/coinerd/q/issues/9323) | No release-readiness gate | high | high | `BUG-0008-no-release-readiness-gate.md` |
| BUG-0009 | [#9324](https://github.com/coinerd/q/issues/9324) | Pinned test expectations drift | medium | medium | `BUG-0009-pinned-expectations-drift.md` |
| BUG-0010 | [#9325](https://github.com/coinerd/q/issues/9325) | No standing agent merge path | high | high | `BUG-0010-no-standing-agent-merge-path.md` |
| BUG-0011 | [#9326](https://github.com/coinerd/q/issues/9326) | No auto-retry / idempotent resume | medium | medium | `BUG-0011-no-auto-retry-no-resume.md` |
| BUG-0012 | [#9327](https://github.com/coinerd/q/issues/9327) | Wave plans lack preconditions | medium | medium | `BUG-0012-wave-plans-no-preconditions.md` |
| BUG-0013 | [#9328](https://github.com/coinerd/q/issues/9328) | Planning artifacts no concurrency protocol | low | low | `BUG-0013-planning-artifacts-no-concurrency.md` |
| BUG-0014 | [#9329](https://github.com/coinerd/q/issues/9329) | Manual release close-out | low | low | `BUG-0014-manual-release-closeout.md` |

All 8 registry rows are `in-progress` as of W0 (v1.00.01).

## Notes

- This PR lands via the agent merge path (branch off `origin/main`, squash merge)
  — dogfooding the procedure that BUG-0010 / issue #9325 will codify in W4,
  targeting (v1.00.01).
- BUG-0008's readiness gate and BUG-0014's close-out report consume this table;
  keep it in sync when issue numbers or statuses change for (v1.00.01).

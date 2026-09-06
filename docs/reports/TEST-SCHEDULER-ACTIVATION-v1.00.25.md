# Test Scheduler Activation States — v1.00.25

**Status:** milestone #890 closed at overall-verdict `hold`; no scheduler lever activated.

This report states, per lane, `activated (evidence link)` or `not activated (named reason)`.
Every claim is bound by SHA-256 to the cohort, decision, or report artifact that carries the
underlying evidence. Checksums are quoted verbatim from the delivered `SHA256SUMS` manifests and
can be re-verified with `cd q && sha256sum -c` against `artifacts/ci-baseline/v1.00.25-{c1,c2,prepared-env}/SHA256SUMS`.

## Evidence index

| artifact | path | sha256 |
|---|---|---|
| C1 paired shadow cohort raw attempts | `artifacts/ci-baseline/v1.00.25-c1/cohort.json` | `1315f37224781751ad45fe3c538a62ff80f5cbed71c81af8f119dcaa95bbac24` |
| C1 machine report | `artifacts/ci-baseline/v1.00.25-c1/report.json` | `6e0a2e99ba893dfc95d019333917408587b7a3226811dd0f2c1b1d7a97a44f93` |
| C1 human report | `artifacts/ci-baseline/v1.00.25-c1/report.md` | `ced7ed780878bc42a74f37bfe351fe83ad8742c2f37471acac292938a6980231` |
| C1 reviewed decision | `artifacts/ci-baseline/v1.00.25-c1/decision.md` | `67638641fd644a02bda41a4f27bf56281f01df9cfe8850f6303d29fb0c437fea` |
| C2 post-promotion cohort raw attempts | `artifacts/ci-baseline/v1.00.25-c2/cohort.json` | `edb8dda3ed9be345701955f095210e099aa10554803ffcd8b832f1f3a0b5cee5` |
| C2 machine report | `artifacts/ci-baseline/v1.00.25-c2/report.json` | `02481e3651b07712ed1b8249e4e6ded83dce363279fde771cbf674ce43b81f74` |
| C2 human report | `artifacts/ci-baseline/v1.00.25-c2/report.md` | `5098e37e6ed3b71f4bc5175441079a9461518f0a59fcf15e01e54fcb3d504d50` |
| C2 reviewed decision | `artifacts/ci-baseline/v1.00.25-c2/decision.md` | `572eaea038ce6af80b7ab03d38f0dec484770691f6e1b231b00b8283b70db5eb` |
| W5 prepared-environment report | `artifacts/ci-baseline/v1.00.25-prepared-env/report.json` | `a5f7129dd2efdf233422d4ca639dfdfc7e64049360e2192ab3b6d850fc040a7c` |

Wave-level honesty records: `docs/reports/gsd-wave-evidence/v1.00.25-w{0,1,2,3,4,5,6}.rktd`
(schema 2, each content-digest bound), with review and validation records under
`docs/reports/gsd-wave-reviews/` and `docs/reports/gsd-wave-validation/`.

## Per-lane activation states

| lane | lever | state | evidence |
|---|---|---|---|
| fast lanes (required CI shard) | queue scheduling via `TEST_RUNNER_SCHEDULER` | **not activated** (named reason below) | W2 hold record `docs/reports/gsd-wave-evidence/v1.00.25-w2.rktd`; C1 decision `67638641fd…437fea` |
| fast lanes (within-shard ordering) | LPT ordering via `FAST_SHARD_ORDERING` / `--ordering` | **not activated** (named reason below) | W3 hold record `docs/reports/gsd-wave-evidence/v1.00.25-w3.rktd`; C1 decision `67638641fd…437fea` |
| security lane | queue scheduling via `TEST_RUNNER_SCHEDULER` | **not activated** (named reason below) | W4 decision record `docs/reports/gsd-wave-evidence/v1.00.25-w4.rktd` (`w4-security-queue-v1`, verdict `hold`); C1 decision `67638641fd…437fea` |
| all lanes | prepared-environment activation | **not activated — decision reserved** (evidence integrated and passing) | W5 record `docs/reports/gsd-wave-evidence/v1.00.25-w5.rktd`; report `a5f7129dd2…040a7c` |
| grouped / tier-overlap scheduling | broad grouped-mode activation | **not activated — withheld** (out of v1.00.25 scope by plan) | roadmap v1.00.26/v1.00.27 reservation in `.planning/STATE.md` |
| any lane | 2× performance claim | **not made** (withheld) | C1 decision `67638641fd…437fea`; C2 decision `572eaea038…70db5eb` |

## Named reasons (verbatim from the gates)

### fast-queue — not activated

C1 closed at overall-verdict `hold`: the `fast/queue/fifo` shadow configuration recorded **0
attempts / 0 samples** across all 20 eligible cohort SHAs, so the promotion gate (paired
out-of-sample evidence) was not evaluable. Per the roadmap, a missed gate produces hold, never a
revised target. `TEST_RUNNER_SCHEDULER` was never set, `.github/workflows/ci.yml` carries no
scheduler token, and the required fast shard keeps its strict command line verbatim; guard pins in
`tests/test-run-tests-profiles.rkt` and `tests/test-scheduler-shadow-workflow.rkt` make any silent
future activation a red CI run. Rollback state: main as-is (nothing to roll back). Baseline on the
default batch scheduler: 20 samples, p50 228.5815 s / p95 235.9965 s.

### fast-LPT — not activated

C1 closed with `fast-LPT . hold` (0 attempts / 0 samples; no predicted improvement over
round-robin). `FAST_SHARD_ORDERING` was never added and the runner's default ordering stays fifo;
manual `--ordering lpt` remains available for shadow configurations only. Guard pins in
`tests/test-runner-scheduler-characterization.rkt` (eight W3 pins) cover the unset default, the
manual override seam, and the distinct named fallback reasons (missing / stale / malformed /
wrong-inventory evidence), plus byte-identical consecutive shard-plan generation
(digest `08e418bbe13082e9c776b0835ae3f67cb92eaa4aba8765631af496499f375279` on both runs).

### security-queue — not activated

Decision `w4-security-queue-v1` recorded verdict `hold` with `gate-evaluable . #f`: the
`security/queue/fifo` shadow leg has zero recorded attempts in
`artifacts/ci-baseline/v1.00.25-c1/cohort.json` (`1315f372…bbac24`) and no security/batch paired
baseline leg exists, so permission/isolation/sandbox semantics cannot be evidence-compared. The
only ci.yml delta in W4 is a comment-only hold notice; the security lane's single
`--suite security` run command is pinned verbatim in `tests/test-run-tests-profiles.rkt`.

### prepared-environment — evidence integrated, activation reserved

W5 integrated the restore evidence and passed its gate honestly: 15/15 verified restores
(rate 1.0 ≥ 95 %, zero rebuilds, zero fallbacks, restore times 338–635 ms, warm window
2026-09-05T10:42:45Z → 2026-09-06T00:25:41Z), with fresh setup+execution remeasures
(5 samples, 249.578–275.619 s; historical 488.0 s / 627.0 s cited as history only). The activation
decision itself is explicitly **reserved** for a later reviewed decision record — evidence passing
is not activation. Report bound at `a5f7129dd2…040a7c`.

## Post-promotion cohort C2 outcome (W6)

C2 measured 20 new eligible PR head SHAs on the required CI lane itself (no shadow duplication) and
returned verdict **`target unachieved`**: p50 253.5 s vs the ≤ 115 s target, p95 276.0 s vs the
≤ 135 s target, with full attempt honesty (20 attempts, 20 successes, 0 failures / cancellations /
reruns, 8 named exclusions). A timing miss alone implies no queue rollback (nothing was activated);
the named next lever — trimming batch shard fan-out and reusing the prepared-environment cache,
then re-running the cohort on new SHAs — is queued for a separate reviewed decision in the reserved
v1.00.26/v1.00.27 series, with no target revised inside this milestone. Decision bound at
`572eaea038…70db5eb`.

## Rollback contract

No lever in this milestone was ever set, so main already is the rollback state. Had a promotion
ever landed, rollback is one command per lever (e.g.
`gh api -X DELETE repos/coinerd/q/actions/variables/TEST_RUNNER_SCHEDULER`), retained unchanged
from the v1.00.23 foundation series.

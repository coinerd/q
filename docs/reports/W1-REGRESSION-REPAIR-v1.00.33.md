# W1 — Regression Repair Evidence (milestone #900, issue #9949)

Date: 2026-11-19 · Baseline: `0d3ee03b` · Branch: `feat/9949-w1-ci-regression`
Worktree: `/home/user/src/q-agent/wt-900-w1` · Runner version: 1.00.32 (info.rkt)
Status: **repair implemented and locally verified; the authoritative
exact-SHA full-regression evidence is NOT produced by this wave** (§5 lists
exactly what is still mandatory after merge).

## 1. Corrected invocation (full-regression)

Incident chain (confirmed):

- Run `43708558326` (Nov 15, "green"): executed only `mutation-pilot`; every
  real regression job was skipped. Green-without-execution.
- Run `44286441007` (Nov 18): all six Linux shards failed with
  `qt: default does not accept CI-surface option --suite (only ./qt full / ./qt --lane NAME do)`;
  the macOS platform job was cancelled (cause still unverified, §4); the
  aggregation failed.

Root cause: `.github/workflows/full-regression.yml` invoked bare
`./qt --suite ...`. A bare `./qt` whose first argument starts with `-` and
without `--lane` dispatches to the **default PR lane** (`./qt` dispatcher:
`(or (not command) (string-prefix? command "-"))` → `run-lane-command
'default`), and the default lane refuses every CI-surface flag fail-closed.

Repair (the explicit supported full/CI surface; **no `--lane`** — a lane is a
suite union and the CI matrix owns the shard via `--shard-*`):

```
STRICT_TEST_RUNNER=1 ./qt full \
  --suite "$SUITE" \
  --jobs "$JOBS" \
  --shard-index <matrix.shard> --shard-total 6 \
  --profile "$PROFILE" \
  "${LEDGER_ARGS[@]}" \
  --json-out shard-results/shard-<n>.json \
  $EXTRA_ARGS
```

Pinned by the new `tests/test-full-regression-invocation.rkt` (11 test
cases, `@suite workflows`): the shard step must contain
`STRICT_TEST_RUNNER=1 ./qt full --suite "$SUITE"`, must not contain the bare
spelling or `--lane`, the whole option combination must stay present, the
ledger guard must stay, and **no workflow file** may contain the bare
`./qt --suite` invocation (executable text only; comments are stripped).

## 2. Validation commands and results (local, all executed)

| Command | Result |
|---|---|
| `./qt --suite all --explain` | exit 1, `qt: default does not accept CI-surface option --suite ...` — the incident reproduced exactly |
| `./qt full --suite all --jobs 4 --shard-index 0 --shard-total 6 --profile ci --explain` | exit 0 — 263 files, `shard 0 of 6`, suite `all` |
| same + `--ledger <existing>` (`--shard-index 5`) | exit 0 — 262 files, `shard 5 of 6` |
| `--ledger` with a non-existent path | fails closed: `qt: full-plan: --ledger file does not exist` (the workflow guards with `if [ -f .github/known-failures.json ]`) |
| `./qt full --file tests/test-skeleton.rkt --profile ci --json-out <tmp>` | exit 0 — valid W0-schema JSON (`run_summary.file_count=1 pass=1`) |
| `./qt full --suite all --json-out x --explain` | refused: `--explain cannot be combined with --json-out` |
| `./qt full --lane fast --jobs 4 --explain` | exit 0 — 1329 files (the compat-lane surface below) |
| `./qt full --lane fast --suite all --explain` | refused: `--lane cannot be combined with --suite` |
| `./qt --suite all` + `--lane` + `--suite` in-process spec (`resolve-qt-ci-plan`) | accepted; plan carries suite/index/total/profile/ledger/json-out |

`--explain` probes execute nothing; the single executed probe was a one-file
explicit reproduction into a temp path.

## 3. Cross-version cadence change (per-push → weekly/manual)

- **Removed** ci.yml `test-cross-version`: a push-only full re-run of the
  1329-file fast suite under Racket 8.11 (~3666 s job wall — the largest
  single main-CI job; `docs/reports/TEST-WORK-MASS-v1.00.32.md`). The stale
  v1.00.11 "W0 KNOWN-RED" comment left with the job; the version-indexed
  schema-revision-2 lock accepts 8.11 today (pinned required-green by
  `tests/ci/verify-lock-selection-test.rkt`).
- **Retained** in full-regression.yml as `cross-version-compat`:
  - schedule: new weekly cron `40 2 * * 6` (Saturday 02:40 UTC) — the job's
    `if:` matches `github.event.schedule == '40 2 * * 6'`, so the nightly
    cron leaves it skipped; plus every `workflow_dispatch`.
  - identity: `racket-version: "8.11"` via the shared setup-racket action.
  - real invocation: `STRICT_TEST_RUNNER=1 ./qt full --lane fast --jobs 4`.
  - evidence: `results-cross-version-811` (JSON + log), `retention-days: 30`,
    `if: always()`.
  - deliberately **not** a `summarize` producer: the definitive
    `run-summary.json` status remains exactly the six Linux shards +
    workflows + platform lanes.
- `scripts/required-pr-checks.policy` untouched and byte-identical (13
  contexts; `test-cross-version` was never one of them).

⚠️ **Residual risk (outside W1's allowed file scope):**
`scripts/gsd-gates/gate-claims.rkt` → `ci-required-jobs` (the 16-entry
milestone-closure list, distinct from the 13-context PR policy) still names
`test-cross-version`. Until a scripts-owner change removes that entry,
`scripts/milestone-gate.rkt` will classify the latest main CI run as
`ci_required_job_missing` at milestone closure. This is a one-line follow-up
for a wave allowed to touch `scripts/`.

## 4. Nightly investigation (run `44289721382`) — demonstrated and repaired

Evidence: job `134751478032` logs, retrieved read-only (GitHub API):

1. `2026-11-18T10:58:11.842Z Unknown JSON field: "runAttempt"` — the consume
   step called `gh run list --json databaseId,runAttempt`; gh's JSON field is
   **`attempt`** (REST: `run_attempt`). gh exited non-zero →
   `resolve: gh run list unavailable (fallback: run the suite)` →
   `resolve: no ci.yml fast-suite proof bundle ...` — proof reuse has never
   engaged on this run.
2. The designed fallback ran `./qt --lane fast` and was killed at exactly
   15 minutes: `##[error]The action 'Run full test suite' has timed out after
   15 minutes.` The fallback budget (900 s) is below the measured fallback
   execution — the unsharded fast suite measured 2141–3666 s wall at
   `--jobs 4` (`docs/reports/TEST-WORK-MASS-v1.00.32.md`); this step runs the
   runner default `--jobs 2`, i.e. slower. **Every** fallback was therefore
   guaranteed red.

Repairs (`.github/workflows/nightly.yml`), neither weakens any control:

- `--json databaseId,attempt` + `.[0].attempt // empty` — the fail-closed
  reuse validator becomes actually reachable; freshness, validator steps 1-15,
  and the fail-safe fallback semantics are untouched.
- fallback budget `timeout-minutes: 15` → `120`, with the budget arithmetic
  recorded in the workflow. The timeout is an execution budget, not an
  evidence control: the suite still runs completely and fail-closed on ANY
  consume rejection.
- Pins updated in `tests/test-proof-bundle-consume.rkt` (the `attempt`
  spelling is now required; the stale spelling and `timeout-minutes: 15` are
  now forbidden).

Not repaired (no demonstrated cause available): the macOS **platform
cancellation** in run `44286441007` — its cause needs that job's own runner
logs (infrastructure termination vs. explicit cancel). The nightly workflow
checks were left intact otherwise.

## 5. Mandatory exact-SHA evidence still required AFTER merge

None of this was executed by W1; W1 makes no claim about a full-regression
run. Required post-merge, retained, at the exact merge-head SHA:

1. **Authoritative full-regression result**: a scheduled or freshly
   dispatched `full-regression` run at the merge head with all **six Linux
   shards** actually executed (jobs `test shard N/6` success; per-shard JSON +
   `test-output.log` retained), suite `all`, profile `ci`.
2. **Nine migrated deep owners with genuine positive executed counts** in the
   aggregated shard evidence, no silent skip: `test-gsd-auto-review-production.rkt`,
   `test-gsd-auto-review-loop.rkt`, `test-gsd-generated-metadata-correction.rkt`,
   `test-gsd-supplemental-evidence-probe.rkt`, `test-gsd-evidence-repair-production.rkt`,
   `test-gsd-evidence-probe.rkt`, `test-qt-interface.rkt`,
   `test-gsd-lost-author-finish.rkt`, `test-gsd-delivery-recovery.rkt`
   (all nine verified present in this worktree; the in-process `--explain`
   probe shows three of them selected in shard 0 alone).
3. **Complete aggregation**: `run-summary.json` with `status=pass`, every
   `required_lanes` entry pass (`linux-shards` 6/6, `workflows`, `platform`),
   no timeout/skip folded into pass.
4. **Red-path proof**: a relevant injected defect on an isolated review
   branch producing an observable red destination result through this
   workflow, then reverted — both run URLs retained (per-family independent
   failure coverage).
5. **Compat-cadence evidence**: one green scheduled Saturday
   `cross-version-compat` run and one green manual dispatch, with
   `results-cross-version-811` artifacts retained.
6. **Nightly repair evidence**: one scheduled nightly run at a SHA with a
   green ci.yml proof bundle (decision `reusable`, `zero-tests-run: true`) or
   a fallback run completing green inside the 120-minute budget, with the
   `reuse-decision-fast` artifact retained.

## 6. Gates executed in this worktree (all green)

| Gate | Result |
|---|---|
| `racket scripts/run-tests.rkt tests/test-full-regression-invocation.rkt` | PASS — 11/11 |
| `racket scripts/run-tests.rkt tests/test-proof-bundle-consume.rkt` | PASS — 25/25 |
| `racket scripts/run-tests.rkt tests/test-ci-workflow.rkt tests/test-ci-workflows.rkt tests/test-workflow-purge-contract.rkt` | PASS — 39/39 |
| `racket scripts/run-tests.rkt tests/test-workflow-invocation-contract.rkt tests/ci/verify-lock-selection-test.rkt` | PASS — 14/14 |
| `racket scripts/ci/validate-workflows.rkt` | 16 workflow/action files valid |
| `./qt quick` | passed: 28 files, 325 tests |
| `./qt default --explain` | exit 0 (1379-file required-offline plan resolved) |
| `racket scripts/lint-all.rkt` | 23 passed, 2 warnings |
| `racket scripts/lint-format.rkt` | 0 errors / 2651 files |
| `racket scripts/lint-tests.rkt` | 0 errors (386 warnings, 110 infos — pre-existing) |
| `racket scripts/lint-docs.rkt` | PASSED, 0 warnings |
| `racket scripts/metrics.rkt --lint` | all 5 static metrics match README |
| `sha256sum -c artifacts/tier-ownership/v1.00.32-w0/SHA256SUMS` | OK |
| `racket scripts/tier-ownership-matrix.rkt check` | green (1170 rows) |
| `git diff --exit-code -- scripts/required-pr-checks.policy` | empty |
| `git diff --exit-code -- extensions scripts runtime agent llm tools cli tui .planning` | empty |

Artifacts regenerated in-commit after the new test file:
ownership map (`reports/test-ownership-map.{json,md}` + inventory),
`artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` + fresh
`SHA256SUMS`, `tests/tier-ownership-matrix.json` (1170 rows),
`tests/.coverage-manifest.json` (205 entries, unchanged content),
README metrics synced with `git add -A` before `metrics.rkt --sync-all`.

## 7. Residual risks / honest limits

- `ci-required-jobs` staleness (§3) — blocks milestone-closure classification
  until a scripts-owner follow-up; W1 cannot touch `scripts/`.
- `cross-version-compat` may red the workflow-run conclusion while
  `run-summary.json` is pass; triage consumers must keep reading
  `run-summary.json` (as `docs/operations/test-regression-triage.md` already
  prescribes) rather than the run conclusion.
- The new contract test costs ~112 s (five `qt` dispatcher probes each load
  the real selection over ~1500 files). It lives in the `workflows` suite;
  measured against that lane's history (~330 s shard wall, 15-minute job
  budget) this fits, but it is the lane's new heaviest file.
- Platform cancellation cause remains unverified (§4).

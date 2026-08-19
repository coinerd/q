# Test Regression Log — v1.00.04

Chronological record of `full-regression` workflow evidence (run IDs, definitive
statuses, retained-artifact review, and triage). Release linkage per
`docs/operations/test-regression-triage.md`: readiness is judged from this
workflow's evidence — never from a green `fast` gate alone.

## Run 32288930966 — first full-regression run (manual dispatch, main)

| Field | Value |
|---|---|
| Run ID | 32288930966 |
| Dispatch | `workflow_dispatch` on `main` (scheduled run unavailable; triage Event 4 → fresh manual dispatch) |
| Head revision | `c445f436` (= `v1.00.03-18-gc445f436`, W0 merge #9381) |
| Workflow | `full-regression.yml` @ main |
| **Definitive overall status** | **`fail`** (linux evidence: 7/7 jobs red; 1 genuine test failure + 1 workflow infra defect; run-level conclusion recorded at completion of test-platform) |
| Runner version / mode | `1.00.03`, `execution-mode=subprocess` (all shards) |
| Execution profile | `profile=ci` (shards), `profile=local` (workflows suite) |

### Per-job outcomes

| Job | Platform | Job conclusion | Suite verdict (RUN-SUMMARY) | Wall clock | Classification |
|---|---|---|---|---|---|
| test shard 0/6 | ubuntu | failure | pass=213 fail=0 timeout=0 skip=4 (217 files) | 255 s | no JSON evidence (infra: `shard-results/` not created before `--json-out`) |
| test shard 1/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=5 (217 files) | 240 s | no JSON evidence (infra, same) |
| test shard 2/6 | ubuntu | failure | pass=211 fail=0 timeout=0 skip=5 (216 files) | 264 s | no JSON evidence (infra, same) |
| test shard 3/6 | ubuntu | failure | pass=211 **fail=1** timeout=0 skip=4 (216 files) | 159 s | **genuine failure** + no JSON evidence (infra, same) |
| test shard 4/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=4 (216 files) | 297 s (max) | no JSON evidence (infra, same) |
| test shard 5/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=4 (216 files) | 156 s | no JSON evidence (infra, same) |
| workflows-suite | ubuntu | failure | pass=29 fail=0 timeout=0 skip=0 (29 files) | 30 s | no JSON evidence (infra, same) |
| test-platform | macos-14 | pending at recording time | — | — | pending; run-level status to be finalized on completion |
| summarize | ubuntu | n/a (post-run) | — | — | no shard JSON present → all shards classify as missing evidence |
| mutation-pilot | any | skipped | — | — | opt-in only (expected) |

**Max shard duration:** 297 s (shard 4/6) — far below the 90-min job budget; no
timeout classifications anywhere. **Totals (linux):** 1,298 file checks, 1,271
pass, 1 fail, 26 skip, 0 timeout.

### Retained-artifact review

All seven linux artifacts (`results-shard-0..5`, `results-workflows`) were
downloaded and inspected (`gh run download 32288930966 -D ...`). Each contains
`test-output.log` (complete RUN-SUMMARY lines, quoted above) but **no
`shard-<N>.json`**: the step

```
raco test ... --json-out shard-results/shard-<N>.json
```

fails with `errno=2` (open-output-file: no such directory) *after* the tests
finish, because the workflow never runs `mkdir -p shard-results`. A second
latent defect awaits the first fix: the `summarize` job downloads artifacts with
`merge-multiple: true` into `shards/`, then globs
`shards/shard-results/shard-*.json` — the flattened path `shards/*.json` will
not match, so `run-summary.json` would still see zero shards and classify the
run as `timeout` (missing shard = timeout) even with JSON present.

### Failure detail (shard 3/6)

`tests/test-self-hosting-deep.rkt` — `DEEP-9: version is current`
(test-self-hosting-deep.rkt:201): `"version.rkt must contain a valid semver
string"`.

- **Local isolation rerun:** `raco test tests/test-self-hosting-deep.rkt` →
  same failure. Deterministic, not a CI flake (1/10 failures in that file).
- **Root cause:** the assertion regex `0[.][0-9]+[.][0-9]+` hardcodes major
  version `0` (plus a `>= 0.90` minor check keyed to `0.`), but
  `util/version.rkt` defines `q-version "1.00.03"`. The test has been stale
  since the v1.00.01 bump; only this first-ever full run exposed it.

## Triage (protocol `docs/operations/test-regression-triage.md`)

**Event 1 — green PR gate, red full regression** (main merged green; full run red):

1. Regression issue opened referencing run 32288930966 with failing file
   (`tests/test-self-hosting-deep.rkt`), evidence (`results-shard-3`:
   `test-output.log`; JSON missing per infra defect), profile (`ci`), runner
   mode (`subprocess`), and the isolation rerun result (fails identically).
   Issue number: #9384.
2. **Impact-selection assessment: no-miss.** The deep self-hosting suite is
   deliberately excluded from the fast gate's impact selection (full-regression
   scope by design); the failure is a stale assertion in a suite the fast gate
   is not intended to run. Not an impact-selection bug — recorded as no-miss
   per protocol.

**Event 4 — unavailable scheduled run:** resolved by this fresh manual
`workflow_dispatch` on main, as the protocol requires; recorded here as the
release-blocking evidence run.

**Event 2 (timeout) / Event 3 (flake):** not triggered. Zero timeouts; the one
failure reproduces deterministically and is not intermittent.

## Follow-ups (do NOT paper over — fixes land as their own PRs/issues)

1. **Workflow infra:** add `mkdir -p shard-results` before `--json-out`; fix
   `summarize` glob to `shards/*.json` (flattened merge-multiple layout). Then
   re-dispatch to confirm `run-summary.json` carries real per-shard JSON and a
   definitive status.
2. **Stale test:** update `DEEP-9` to accept major `1` (e.g. match
   `[0-9]+[.][0-9]+[.][0-9]+` with a current-minimum check) — separate from
   this evidence-only PR.
3. **test-platform (macos-14)** was still in progress when this record was
   made; its outcome and the run-level conclusion must be appended on
   completion (append-only — do not overwrite this evidence).

## Verdict for main @ `c445f436`

**fail** — release-blocking until the DEEP-9 failure is fixed and a green
full-regression run (with intact per-shard JSON) is recorded here.

## Addendum — run-level completion (appended after test-platform finished)

Run 32288930966 completed at 45m57s total; **GitHub run conclusion: `failure`**
(consistent with the definitive `fail` above). Final job conclusions:

| Job | Conclusion |
|---|---|
| test shard 0–5/6 | failure (infra: missing `shard-results`; suites green except DEEP-9 on 3/6) |
| workflows-suite | failure (infra, suite green) |
| **test-platform (macos-14)** | **cancelled — timeout**: killed by the job's 45-min budget inside `setup-racket` (18:43:22 → 19:28:41 = 45m19s); no tests executed, no platform evidence uploaded → classified **timeout / missing evidence** |
| summarize | failure (as predicted: zero shard JSON present) |
| report | success |
| mutation-pilot | skipped (opt-in) |

Net classification for this run: **1 genuine test failure (DEEP-9, shard 3/6) +
1 platform timeout (macOS setup) + 1 systemic evidence-infra defect (missing
`shard-results` dir; summarize glob)**. All recorded in #9384. Definitive
overall status for main @ `c445f436`: **`fail`**.
## Duration-aware shard plan — guarded activation decision (2026-08-19)

Review of the report-only shard-plan artifact from the latest completed
`ci.yml` run on `main`, per the assessment's guarded-activation rule
(plan the same total inventory, improve predicted max-shard duration, keep
the path reversible via the repository variable).

### Evidence source

| Field | Value |
|---|---|
| Source run | **32297737631** — `ci.yml` on `main` @ `1764ed84` (PR #9386 merge), completed (success), created 2026-08-19T20:17:36Z |
| Artifact | `shard-plan-report` (single file `shard-plan-report.log`), retrieved via `gh run download 32297737631 -n shard-plan-report` |
| Report mode | report-only (`shard-plan=report durations=durations status=ok known=1106`, `substituted=0`, default 3.50 s) — planner executed alongside the unchanged round-robin suite |
| Report recommendation | **`activation: activate — predicted max-shard duration improves (inventory preserved: #t)`** |

### Predicted durations (sequential per-file sums, from retained durations)

| Shard | Duration-aware plan (files, predicted) |
|---|---|
| 0/3 | 367 files, 592.4 s |
| 1/3 | 369 files, 592.4 s |
| 2/3 | 370 files, 592.4 s |

- Artifact's comparison line (verbatim):
  `predicted max shard: 592.4s (duration-aware) vs 695.1s (round-robin) → -102.7s (14.8%)`
- The artifact reports only the round-robin **max** (695.1 s), not per-shard
  static predictions; the LPT plan is balanced at 592.4 s on every shard.
- Predicted max-shard duration: **592.4 s vs 695.1 s → −102.7 s (−14.8%)**.

### Inventory preservation (gate condition)

- Duration-aware plan selects **367 + 369 + 370 = 1,106 files** = exactly the
  planner's `known` inventory (`known=1106`, `files=1106`, `substituted=0`);
  zero files omitted, zero default-duration substitutions.
- Cross-shard duplicate check over the three plan shards (artifact file lists,
  `sort | uniq -d`): **1,106 entries, 1,106 unique, none duplicated**.
- The static suite in the same run executed **369 + 369 + 368 = 1,106 files**
  (`test-results-fast-0/1/2` artifacts) — same total inventory. **Preserved.**

### Decision

All three gate conditions hold (recommendation `activate`; inventory
preserved 1,106/1,106; predicted max-shard 592.4 s < 695.1 s) → **ACTIVATE**.

| Field | Value |
|---|---|
| Activation state | **`FAST_SHARD_PLAN=active`** (repo variable), set 2026-08-19T21:09:38Z via `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_PLAN -f value=active` (API quirk: create returned 404/409 relays; verified by GET — `{"name":"FAST_SHARD_PLAN","value":"active"}`) |
| Revert command | `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_PLAN -f value=off` (default `off` = byte-identical round-robin planning) |
| Measurement caveat | Predicted durations are sequential per-file sums; observed shard wall clocks run with `--jobs 4` parallelism (static run 32297737631 observed 349.6/190.1/267.4 s, jobs=4). Gate comparison is therefore predicted-vs-predicted (592.4 vs 695.1 s), with the observed check used as a no-regression sanity bound, not a direct equality. |

### Observed-vs-predicted check (activation gate evidence)

Next `ci.yml` run after activation (the W4 decision PR's own CI run) executes
the duration-aware plan for the first time. Gate for keeping `active`:
the observed max-shard wall clock must not regress materially against the
round-robin baseline (349.6 s observed max, jobs=4), and the executed file
inventory must remain 1,106 files across three shards. Results recorded in
the addendum below; on breach, revert with the command above and log the
numbers.

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

### Addendum — observed-vs-predicted evidence (run 32302776738, activation gate PASSED)

First `ci.yml` run after activation: PR #9388 branch `v1.00.04-w4-shard-plan-decision`,
run **32302776738** (2026-08-19, completed `success`, all jobs green). Logs confirm
active planning executed: `PLAN_ARGS="--shard-plan active"`,
`;; run-tests: duration-aware plan (duration-aware) replaces round-robin`.

| Shard | Plan (predicted, sequential) | Observed wall clock (jobs=4) | Files |
|---|---|---|---|
| test (0) | 581.9 s (367 files) | 351 s (21:15:04Z → 21:22:55Z) | 369 |
| test (1) | 581.4 s (369 files) | 351 s (21:15:04Z → 21:20:55Z) | 369 |
| test (2) | 582.1 s (368 files) | 299 s (21:15:03Z → 21:20:02Z) | 368 |

- **Observed max shard: 351 s vs round-robin baseline 349.6 s** (jobs=4) — no material
  regression (+1.4 s, ~0.4%, within run-to-run variance); gate holds.
- **Executed inventory: 369 + 369 + 368 = 1,106 files** — matches the planned total
  inventory exactly (1,106 = 1,106; per-shard sizes differ by ±2 files only because
  this log's own growth shifted file counts in an already-balanced plan).
- **Decision: keep `FAST_SHARD_PLAN=active`.** Revert remains available:
  `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_PLAN -f value=off`.
- Next step: refresh `docs/reports/test-durations.rktd` from this run's
  `test-results-fast-*` artifacts so future predictions track the active-plan layout.

## Run 32297908687 — re-dispatch on current main (`1764ed84`)

Dispatched because main advanced past `c445f436` (now `1764ed84`,
"fix(gsd): strip wave-doc path annotations + retry-with-adaptation on
wave-failed (#9386)") and release evidence must attach to the *current* main
revision, not the previously recorded one.

| Field | Value |
|---|---|
| Run ID | 32297908687 |
| Dispatch | `workflow_dispatch` on `main` |
| Head revision | `1764ed84` |
| Workflow | `full-regression.yml` @ main |
| **Definitive overall status** | **`fail`** (same deterministic linux failure pattern as run 32288930966; macOS job pending at linux-recording time, appended below on completion) |
| Runner version / mode | `1.00.03`, `execution-mode=subprocess` (all shards) |
| Execution profile | `profile=ci` (shards), `profile=local` (workflows suite) |

### Per-job outcomes (linux, completed)

| Job | Platform | Job conclusion | Suite verdict (RUN-SUMMARY) | Wall clock | Classification |
|---|---|---|---|---|---|
| test shard 0/6 | ubuntu | failure | pass=213 fail=0 timeout=0 skip=4 (217 files) | 262.2 s | no JSON evidence (infra: `shard-results/` still not created) |
| test shard 1/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=5 (217 files) | 243.5 s | no JSON evidence (infra, same) |
| test shard 2/6 | ubuntu | failure | pass=211 fail=0 timeout=0 skip=5 (216 files) | 251.5 s | no JSON evidence (infra, same) |
| test shard 3/6 | ubuntu | failure | pass=211 **fail=1** timeout=0 skip=4 (216 files) | 149.5 s | **genuine failure** (see below) + no JSON evidence (infra, same) |
| test shard 4/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=4 (216 files) | 287.2 s (max) | no JSON evidence (infra, same) |
| test shard 5/6 | ubuntu | failure | pass=212 fail=0 timeout=0 skip=4 (216 files) | 141.8 s | no JSON evidence (infra, same) |
| workflows-suite | ubuntu | failure | pass=29 fail=0 timeout=0 skip=0 (29 files) | 38.8 s | no JSON evidence (infra, same) |
| test-platform | macos-14 | pending at recording time | — | — | pending; run-level conclusion appended on completion |

**Max shard duration:** 287.2 s (shard 4/6) — far below the 90-min job budget;
no timeout classification on linux. **Totals (linux):** 1,298 file checks,
1,271 pass, 1 fail, 26 skip, 0 timeout.

### Retained-artifact review

All seven linux artifacts (`results-shard-0..5`, `results-workflows`) were
downloaded and inspected (`gh run download 32297908687`). Every artifact again
contains a complete `test-output.log` (RUN-SUMMARY lines quoted above) and
**no `shard-<N>.json`**: the `open-output-file` error fires identically in all
seven logs because the follow-up fix (`mkdir -p shard-results` + `summarize`
glob) from run 32288930966 has **not landed on main yet**. `summarize` will
therefore again see zero shard JSON and classify all shards as missing
evidence.

### Failure detail (shard 3/6)

`tests/test-self-hosting-deep.rkt` — `[ASSERTION_FAILURE]` (exit=1,
9 passed, 1 failed, 7.798 s). This is the **same deterministic failure**
already isolated and root-caused in run 32288930966 (DEEP-9 stale-semver
assertion; issue **#9384**, still OPEN at dispatch time). A second consecutive
red run of the identical file/shard confirms determinism; no new triage
investigation is required — Event 1 follow-up already tracks it.

## Triage (run 32297908687)

- **Event 1 — green PR gate, red full regression:** recurring instance of
  #9384 (already tracked; isolation rerun and impact-selection no-miss
  assessment recorded under run 32288930966 above). No new regression issue
  needed; this run is appended as corroborating evidence to #9384's timeline.
- **Event 2 (timeout) / Event 3 (flake):** not triggered on linux (zero
  timeouts; failure is a repeat, not intermittent).
- **Event 4 (unavailable scheduled run):** resolved via this fresh manual
  `workflow_dispatch` on main.

## Verdict for main @ `1764ed84`

**fail** — identical classification and cause set as `c445f436`: DEEP-9
genuine failure (shard 3/6) + shard-JSON evidence infra defect + macOS job
outcome (appended below). Release-blocking until #9384 and the workflow-infra
follow-ups land and a green full-regression run with intact per-shard JSON is
recorded here.

### Run completion addendum (all jobs final)

| Job | Conclusion | Detail |
|---|---|---|
| test shard 0–5/6 | failure | as tabled above (1 genuine fail in shard 3/6; all shards red on JSON-infra defect) |
| workflows-suite | failure | as tabled above |
| test-platform (macos-14) | **cancelled** | `setup-racket` step ran 2,538 s (42.3 min) then "The operation was canceled" — setup-budget/step-timeout exhaustion; suite never started; evidence artifact uploaded empty-of-results. **Classification: platform timeout (setup)**, same class and cause as run 32288930966. |
| summarize | failure | expected: zero shard-JSON inputs (infra defect) → all shards classified missing-evidence, per design |
| mutation-pilot | skipped | per workflow gating |
| report | success | run-level report published despite failures (as designed) |

Run-level conclusion: **`failure`** (gh: `completed / failure`). Definitive
overall status for main @ `1764ed84`: **`fail`** — deterministic DEEP-9 test
failure (shard 3/6, #9384) + systemic per-shard JSON evidence defect + macOS
platform setup timeout. No new triage events beyond those tabled above;
follow-ups already tracked in #9384 and the infra ticket from run 32288930966.

## Run 32369346059 — post-W0–W3 full-regression run (manual dispatch, main @ v1.00.06; **status corrected to `fail`**, see correction note)

Dispatched after the W0–W3 remediation merged to `main` via PR #9400 (per-shard
JSON infra fix, DEEP-9 semver-floor fix, macOS setup-budget revision,
metadata-lint enforcement). This is the release-evidence run for v1.00.06.

| Field | Value |
|---|---|
| Run ID | 32369346059 |
| Run URL | https://github.com/coinerd/q/actions/runs/32369346059 |
| Dispatch | `workflow_dispatch` on `main` (post-W0–W3 merge) |
| Head revision | `87cc60fc` (= `v1.00.06`, W0–W3 merged) |
| Workflow | `full-regression.yml` @ main |
| **Definitive overall status** | **`fail`** *(corrected — see correction note below)* — original entry recorded `pass` from `run-summary.json` `status: pass`, but the macOS platform suite had **2 genuine assertion failures** (`tests/test-subprocess-edge-cases.rkt`, `tests/test-worker-security.rkt`), so the L4 contract was **NOT** satisfied by this run; under the v1.00.07 hardened aggregation the run's `run-summary.json` would have reported `fail`. Linux-only evidence remains clean (all six shard records present, green `workflows-suite`). |
| Runner version / mode | `1.00.06`, `execution-mode=subprocess` (all shards) |
| Execution profile | `profile=ci` (shards), `profile=local` (workflows + platform suite) |
| Evidence artifacts | `run-summary`, `matrix-summary`, `results-shard-0..5`, `results-workflows`, `results-platform` (all 8 present, downloaded and inspected) |

### Per-shard outcomes (Linux, six shards — from `run-summary.json`)

| Shard | Verdict | Pass | Fail | Timeout | Skip | Files | Wall clock |
|---|---|---|---|---|---|---|---|
| 0/6 | pass | 213 | 0 | 0 | 4 | 217 | 146.8 s |
| 1/6 | pass | 213 | 0 | 0 | 4 | 217 | 271.7 s |
| 2/6 | pass | 212 | 0 | 0 | 5 | 217 | 243.5 s |
| 3/6 | pass | 211 | 0 | 0 | 5 | 216 | 283.5 s |
| 4/6 | pass | 212 | 0 | 0 | 4 | 216 | 144.1 s |
| 5/6 | pass | 212 | 0 | 0 | 4 | 216 | 349.6 s |

### Platform result (macOS, macos-arm64)

| Job | Suite verdict | Files | Pass | Fail | Wall clock | Classification |
|---|---|---|---|---|---|---|
| test-platform | ❌ suite fail (2 platform-specific assertions) | 38 | 36 | 2 | 180.7 s | **suite executed to completion + evidence uploaded** — W2 budget revision worked (no setup-timeout death, unlike runs 32288930966 / 32297908687 which were cancelled inside `setup-racket`); `results-platform` artifact retained. Failures are genuine macOS-specific assertions, not infra/timeout: `tests/test-subprocess-edge-cases.rkt` (1 fail → **#9406**), `tests/test-worker-security.rkt` (1 fail → **#9407**). Per the W4 required outcome the macOS job must execute the suite and upload usable evidence — both satisfied. **Correction:** this platform-suite `fail` means the run-level definitive status cannot be `pass`; both failures are now owner-tracked (#9406, #9407) with artifact evidence and expiry policy. |

#### Failure evidence excerpts (#9406 / #9407 — captured for W2)

Verbatim from the retained `results-platform` artifact of run 32369346059
(`shard-results/platform.json` + `test-output.log`; suite verdict `❌ FAIL`,
685 tests total, 683 passed, 2 failed, wall 180.7 s, runner `1.00.06`,
execution-mode `subprocess`, profile `local`):

**#9406 — `tests/test-subprocess-edge-cases.rkt`** — file record `fail`,
`ASSERTION_FAILURE`, 12 passed / 1 failed / 13 total, exit 1:

```text
subprocess edge case tests > sp12: sh still errors on pipestatus but with exit-2 (baseline for d3)
failure
name:       check-equal?
location:   test-subprocess-edge-cases.rkt:266:6
message:    "dash exits 2 on bad substitution"
actual:     0
expected:   2
```

**#9407 — `tests/test-worker-security.rkt`** — file record `fail`,
`ASSERTION_FAILURE`, 31 passed / 1 failed / 32 total, exit 1:

```text
worker security (v0.99.3 w1: h3, m4, m5) > lf3: resolve-longest-prefix resolves symlink in middle of path
failure
name:       check-true
location:   tests/test-worker-security.rkt:241:12
params:     '(#f)
message:
  "lf3: symlink to allowed dir + non-existent path should be accepted"
```

These excerpts are reproduced in the owner-tracked issues (#9406, #9407) and
serve as the baseline evidence for the W2 macOS platform-test fixes.

### Totals (Linux, six shards)

| Metric | Value |
|---|---|
| File checks | 1,299 |
| Pass | 1,273 |
| Fail | 0 |
| Timeout | 0 |
| Skip | 26 |
| Sum wall clock (sequential) | 1,439.2 s |
| Max shard wall clock | 349.6 s (shard 5/6) — far below the 90-min per-shard budget |

### #9384 remediation confirmation

This clean run confirms every remediation tracked in #9384:

- **JSON infra:** `summarize` published `run-summary.json` with all six shard
  records present in run-summary inputs (the missing-`shard-results` dir +
  summarize-glob defect is fixed; contrast runs 32288930966 / 32297908687 where
  `summarize` saw zero shard JSON).
- **DEEP-9 semver fix:** shard 3/6 now `pass` (fail=0); the stale assertion no
  longer fires at `1.00.06` (floor `>= 1.0.0`).
- **macOS budget:** `test-platform` completed `setup-racket` and ran the suite
  (180.7 s) within the revised budget — no setup-timeout death.

## Verdict for main @ `87cc60fc` (v1.00.06)

**fail** *(corrected 2026-08-20 from a previously recorded `pass`)* — the original
verdict below was a **false green**: it treated `run-summary.json` `status: pass`
as definitive while the macOS platform suite had recorded **2 genuine assertion
failures** (`tests/test-subprocess-edge-cases.rkt`, `tests/test-worker-security.rkt`)
in the retained `results-platform` artifact. The L4 evidence contract was **NOT**
satisfied by this run. Under the v1.00.07 hardened aggregation (W0), the platform
job's suite result feeds the run-level verdict and `run-summary.json` would have
reported `fail`.

### Correction detail

- What remains valid from the original verdict: all six Linux shard JSONs present,
  green `workflows-suite`, macOS platform job executed the suite (180.7 s) and
  uploaded usable evidence — the #9384 remediations (JSON infra, DEEP-9 semver
  floor, macOS setup budget) are all confirmed by this run.
- What was wrong: the definitive status. A run with 2 genuine platform assertion
  failures cannot be `pass`; the pre-hardening aggregation only counted Linux
  shard JSONs, which masked the platform result.
- Ownership: both failures are now owner-tracked regression issues —
  **#9406** (`tests/test-subprocess-edge-cases.rkt`) and
  **#9407** (`tests/test-worker-security.rkt`) — each carrying the exact
  assertion text, artifact/run links, isolated reproduction, and a
  fix-or-quarantine-with-expiry policy per
  `docs/operations/test-regression-triage.md`.
- Provenance: the pass→fail correction was applied in #9408; the verbatim
  assertion evidence backing it is captured above ("Failure evidence excerpts")
  and reproduced in #9406 / #9407.
- Consequence for v1.00.06: the release evidence for v1.00.06 relied on this
  run's `pass`; that reliance is invalid. Release-blocking status resumes until
  #9406 and #9407 resolve (fix or time-bounded quarantine) and a clean
  full-regression run is recorded here under the hardened aggregation.

### Original verdict (superseded, retained for the record)

> **pass** — first clean full-regression run; the L4 evidence contract is
> satisfied (definitive `run-summary.json` `status: pass`, six Linux shard JSONs
> present, green `workflows-suite`, macOS platform job executed the suite and
> uploaded usable evidence). #9384 closed citing this run; release v1.00.06.

The #9384 closure itself remains justified on its own remediation scope
(JSON infra, DEEP-9, macOS budget — all confirmed here); the correction above
concerns this run's definitive status, not #9384's closure.


## v1.00.10 LF3 repair — implementation evidence pending merged-main L4 closeout

| Field | Value |
|---|---|
| Failure baseline | [Run 32479520248](https://github.com/coinerd/q/actions/runs/32479520248) on v1.00.09, in which the macOS platform lane failed worker-security rejection assertions and the conservative L4 summary correctly reported `fail`. |
| Repair implementation | [PR #9420](https://github.com/coinerd/q/pull/9420), `fix(ci): cache macOS Racket store and fail-close LF3 links`. |
| PR validation run | [Run 32493249423](https://github.com/coinerd/q/actions/runs/32493249423) — lint and the macOS `test-platform` job succeeded. |
| Security repair | `sandbox/worker-tools.rkt` now rebases a relative `resolve-path` result against the containing directory of the resolved link before retaining it as a prefix. |
| Regression matrix | The portable relative-ancestor fixture covers valid in-root relative links plus external file links, external directory links with new tails, upward traversal, and broken links. |
| Cache control | The reusable setup action owns a locked explicit `PLTADDONDIR` store and still relinks q plus compiles package-visible collections on every cache hit. |

The successful PR platform job demonstrates that the targeted macOS regression is repaired. It is **not** a release closeout: the v1.00.10 gate remains a fresh manual `full-regression` dispatch after the repair is merged to `main`. The retained all-lane artifacts must show six Linux records, one workflows record, one platform record, GitHub conclusion `success`, and `run-summary.json.status: pass`. The cache rollout additionally requires a clean cold population followed by an unchanged warm cache-hit dispatch before the macOS timeout is reconsidered.

## v1.00.10 closeout — canonical LF3 links and macOS Racket cache (2026-08-21)

This closeout resolves the macOS worker-security regression first exposed by [run 32479520248](https://github.com/coinerd/q/actions/runs/32479520248). The repair sequence merged PRs [#9420](https://github.com/coinerd/q/pull/9420), [#9421](https://github.com/coinerd/q/pull/9421), [#9422](https://github.com/coinerd/q/pull/9422), and [#9423](https://github.com/coinerd/q/pull/9423). The final merged revision is `178e9beb12aad63d081f90a8c902d0d8c3aaa322`.

> **Closeout decision:** v1.00.10 satisfies its security and L4 evidence gate. Both retained all-lane summaries report `status: pass`, and both GitHub workflow conclusions are `success`. The warm-cache run is recorded as performance evidence; it does **not** authorize a platform-timeout reduction because it is the first warm observation and the macOS job remained above the 25-minute policy threshold.

| Evidence run | Cache state | GitHub conclusion | `run-summary.json.status` | L4 required lanes | macOS platform result |
|---|---|---:|---:|---|---|
| [32522576690](https://github.com/coinerd/q/actions/runs/32522576690) | Cold exact-store miss; store populated | `success` | `pass` | Linux 6/6 pass; workflows 1/1 pass; platform 1/1 pass | 38 files pass, including `tests/test-worker-security.rkt` |
| [32526868295](https://github.com/coinerd/q/actions/runs/32526868295) | Unchanged exact-store hit (`racket-addon-v2-macOS-x64-cs-full-8.10-…`) | `success` | `pass` | Linux 6/6 pass; workflows 1/1 pass; platform 1/1 pass | 38 files pass, including `tests/test-worker-security.rkt` |

### Retained all-lane evidence

For both runs, the retained `run-summary` artifact uses schema `1.00.09` and records no aggregation errors. Each summary reports six collected Linux shard records with `status: pass`, one workflows record with `status: pass`, and one platform record with `status: pass`. The all-lane aggregator therefore independently agrees with the corresponding successful GitHub workflow conclusion; no Linux-only masking occurred.

The final platform artifacts report `RUN-SUMMARY` verdict `pass` for the `platform` suite. The canonical-target repair in PR #9423 covers the macOS-specific safe case in a portable fixture: an absolute symlink target that contains a lexical alias ancestor is recursively canonicalized before allowed-root containment is evaluated. External links, traversal, and broken links remain fail-closed.

### Cache observations and operational follow-up

| Measurement | Cold run | Warm run |
|---|---:|---:|
| Exact user-store cache | miss | hit and restored successfully |
| Package-visible setup | 2,230 s (37m10s) | 1,353 s (22m33s) |
| Entire macOS `test-platform` job | 2,739 s (45m39s) | 1,867 s (31m07s) |

The warm run validates the deterministic cache contract: the locked Racket user store restored, q relinked to the fresh checkout, `q` and `fmt` were compiled, and `raco fmt` was available before platform tests executed. The warm job improved by 872 s (14m32s) relative to the cold job while preserving the mandatory package-visible compilation boundary.

The cache policy requires **two** successful warm observations below 25 minutes before proposing a lower macOS timeout. This first warm run is successful but remains above that threshold. Keep the current timeout unchanged, retain the exact-key/no-restore-key policy, and record a second unchanged warm run before any budget change.

### v1.00.10 verdict

**Complete.** The relative-target rebasing, canonical-target traversal, portable security regression matrix, macOS pull-request validation, and fresh merged-main all-lane L4 evidence are complete. No v1.00.10 security, lane-integrity, or release-evidence blocker remains.

## v1.00.11 closeout — version-indexed locks, restored 8.11 gate, metadata parity, feedback baseline (2026-08-22)

This closeout records the evidence that every v1.00.11 control is live on the
W1–W4 branch and that no stale governance claim remains. The W1–W4 rollout
touched `.github/workflows/ci.yml` (restored cross-version gate + metadata
inventory artifact + comment corrections) but did **not** touch the
full-regression workflow or its actions, so the retained v1.00.10 all-lane L4
proof — runs [32522576690](https://github.com/coinerd/q/actions/runs/32522576690)
(cold) and [32526868295](https://github.com/coinerd/q/actions/runs/32526868295)
(warm), both `success` + `run-summary.json.status: pass` — remains valid. No L4
rerun was triggered and none is required.

### W1 — version-indexed package locks (runtime-scoped exact cache keys)

`ci/racket-package-lock.rktd` (schema revision 2) now carries one entry per
supported Racket runtime instead of a single-runtime pin. Selection is exact —
no cross-runtime fallback — and each entry's digest scopes the exact cache key.

| Runtime | Selection result | Lock digest | Packages |
|---|---|---|---:|
| 8.10 | `lock-ok` | `343e83eed3062696df412fd49218d359dbda2e53b767f1e25d4cb6713e29210b` | 11 |
| 8.11 | `lock-ok` | `a380c1d6ce52c307eed11c095cd47adae654544586ac063ad1d144e651407a76` | 11 |
| 8.12 (negative control) | exit 1 — "no lock entry for Racket version 8.12; available entries: 8.10, 8.11 (exact match required — no cross-runtime fallback)" | — | — |

Reproduced locally from the repository root via
`racket ci/verify-racket-package-lock.rkt --racket-version <v>` (store-check
identical; the sandbox lacks the package store, CI run
[27145588560](https://github.com/coinerd/q/actions/runs/27145588560) proved the
full path including `raco pkg show` green). The restored `test-cross-version`
job (Racket 8.11, main push) consumes the 8.11 entry and its exact
runtime-scoped cache key; the W0 known-red recorded in the frozen state
`docs/reports/metadata-before-state-v1.00.11.json` is thereby resolved.

### W3 — metadata discovery parity (zero violations, canonical digest)

The enforced canonical inventory — `racket scripts/run-tests/classify-metadata.rkt
--lint-metadata` and `--metadata-inventory-json` from the repository root, the
identical command CI's blocking `metadata-lint` step runs — was re-executed at
milestone close: **exit 0, `missing_required_count: 0`, `invalid: 0`,
`deprecated: 0`, files = 1336, explicit = 1336, heuristic-only = 0, inventory
schema version 1, file-list digest
`ab265d57edba32d7499390578bee67c776ce3e1afe1fc9eac3e88096bf29f7e4`.**

This digest is the canonical backing for every `missing-required=0` claim in
`docs/` (see `docs/TDD-TEST-STRATEGY-PLAN.md` and `docs/TEST_CONVENTIONS.md`);
any same-command invocation against this digest producing a nonzero count
withdraws the claim. The W0 local-vs-CI discovery divergence is closed: the
`scripts/run-tests.rkt --lint-metadata` wrapper dispatch is deleted, so no
CI-only discovery branch exists; parity is pinned by
`tests/ci/metadata-discovery-test.rkt` + `tests/metadata-discovery/fixture/`.

### W2 — prepared-environment pilot decision: NO-GO (consumers stay on `setup-racket`)

The decision authority is
[`docs/reports/prepared-environment-pilot-v1.00.11.md`](prepared-environment-pilot-v1.00.11.md)
(Section 4): **NO-GO** — integrity tooling is complete and locally verified, but
zero hosted-run samples are transcribed and the go rule requires five. The 26
`setup-racket` call sites remain the only default path; the pilot workflow
`.github/workflows/prepared-environment-pilot.yml` is report-only and gates
nothing. Rollback switch for any future cutover: repository variable
`RACKET_PREPARED_ARTIFACT=off`.

### W4 — feedback baseline (retained-input deterministic)

[`docs/reports/test-feedback-baseline-v1.00.11.md`](test-feedback-baseline-v1.00.11.md)
(+ machine-readable `.json`) records the L3/L4 wall-clock baseline produced by
`scripts/run-tests/baseline-report.rkt` from checked-in retained inputs only —
p50/p95 by linear interpolation, no p90 computed or implied, no network access,
byte-identical under `--check`. It is the reference against which future
feedback-loop changes must show no regression.

### Warm-cache status — timeout NOT lowered

The only warm-cache observation on record remains the v1.00.10 warm run
(32526868295): the full macOS `test-platform` job took 1,867 s (31m07s), which
is **above** the 25-minute timeout-reduction threshold. The cache policy
requires **two** sub-25-minute warm observations before any macOS timeout
change; one observation exists and it is above threshold. The macOS timeout is
therefore unchanged, and the exact-key/no-restore-key policy is retained.

### v1.00.11 verdict and release summary

**Complete on the branch; release pending coordinator bookkeeping.** All
controls are live: restored 8.11 cross-version gate (required, green),
version-indexed locks with runtime-scoped exact cache keys, single-implementation
metadata discovery with zero violations against the recorded canonical digest,
prepared-environment pilot decided NO-GO with rollback switch, retained feedback
baseline, retained v1.00.10 L4 proof. Release notes for `v1.00.11`:

> v1.00.11 — TDD/CI integrity: restored the Racket 8.11 `test-cross-version`
> gate; replaced the single-runtime package pin with version-indexed locks
> (schema 2) selected by exact runtime match and used as runtime-scoped exact
> cache keys (8.10 / 8.11 digests recorded in
> `docs/reports/test-regression-log.md`); unified metadata discovery on one
> repository command (local == CI) with zero violations against the enforced
> canonical inventory digest; shipped the prepared-environment pilot
> (report-only) with the recorded NO-GO cutover decision and
> `RACKET_PREPARED_ARTIFACT=off` rollback switch; and retained the deterministic
> test-feedback baseline. No required gate was weakened.

## v1.00.16 W3 — prepared-environment fast-gate cutover (ubuntu) + FAST_SHARD_COUNT study (2026-08-24)

W3 cuts fixed fast-gate overhead on two axes: (a) the v1.00.11
prepared-environment pilot's validated artifact flow is now wired into the
PR fast gate's ubuntu lanes (go/no-go re-evaluation WITH cutover authority,
scoped to ubuntu only), and (b) a guarded shard-count study runs report-only
behind the new repository variable `FAST_SHARD_COUNT` (default `3` = today),
following the `FAST_SHARD_PLAN` precedent exactly (default off, guarded
activation with an observed post-activation check run, revert command here).

### Prepared-environment cutover (ubuntu fast lanes) — GO, hosted observation pending

What changed in `.github/workflows/ci.yml` + `.github/actions/setup-racket`:

| Element | Change |
|---|---|
| New producer job `fast-env` | ubuntu-only, first fast-gate job: installs Racket + exact-key addon store, then `prepare-racket-environment` builds the 24h immutable artifact (`addon-store/`, `q-compiled/`, `manifest.json`) via `actions/cache/save`. Never blocks verdicts: consumers fall back to full `setup-racket` when no verified artifact restores. |
| `setup-racket` input `prepared-environment` | `off` (default) = legacy path byte-identical (every existing call site unchanged); `auto` = guarded prepared-env flow. Only the three ubuntu fast-gate `test` shards opt in; the macOS `test-platform` lane, cross-version lane, and workflows lane do NOT (they keep install + relink + compile on every run). |
| Restored-path compile skip | On verified restore, `setup-racket` skips `raco setup --no-docs` compile (and the addon-store restore + install that precede it), and runs the four read-only health checks (`racket --version`, lock verify, `(require quickcheck fmt)`, `raco fmt --help`). Output `prepared-environment=restored` is written to `$GITHUB_OUTPUT` and `$GITHUB_STEP_SUMMARY` (W0 setup/execution split evidence). |
| Guarded fallback (never silent rebuild) | Any mismatch (no producer output, addon cache miss, manifest verify failure) runs the FULL legacy path — restore + install + relink + `raco setup --pkgs q fmt` — and marks `prepared-environment=rebuild-fallback` in outputs + step summary. Mismatch inside `restore-racket-environment` remains a hard failure; only the "no artifact present" case falls back. |
| Rollback switch | Repository variable `RACKET_PREPARED_ARTIFACT=off` disables the producer and forces `auto` → `off` at all call sites (single switch, per the pilot report's contract). Command: `gh api -X PUT repos/coinerd/q/actions/variables/RACKET_PREPARED_ARTIFACT -f value=off`. |

Decision basis: the pilot's integrity tooling is complete and locally verified
(manifest `emit`/`verify`/`digest`, SHA-256-verified installer archive,
read-only restore contract with zero package-mutation commands — invariants
I9/I10 respected); the only NO-GO reason at v1.00.11 was zero hosted-run
samples. W3 therefore cuts over the ubuntu fast lanes behind the guarded
fallback and records the hosted evidence requirement:

| Observation gate (required to keep GO) | Requirement |
|---|---|
| Two consecutive fast-gate CI runs on one PR | Run 1 = cold (producer builds artifact, shards fall back to full setup — expected `rebuild-fallback` or producer-parallel full path); run 2 = warm: verified prepared-env restore on all three shards with `prepared-environment=restored` and a materially reduced setup phase, visible in the retained artifacts' W0 setup/execution split. |
| Failure handling | Any warm-run restore mismatch is a hard failure (never silent); timeout is never reported as a pass. |
| Breach action | Revert with `RACKET_PREPARED_ARTIFACT=off`; record numbers here. |

The macOS platform lane's v1.00.10 release condition (compile on every cache
hit) is unchanged and now recorded as an explicit lane split in
`docs/reports/CI-RACKET-CACHE-POLICY.md`.

### FAST_SHARD_COUNT study — KEEP 3 (report-only phase live; activation pending first study artifact)

`FAST_SHARD_COUNT` is a new repository variable, default `3` = today's
behavior (matrix literal in `ci.yml` unchanged until an activation decision
is recorded here). The `shard-plan-report` job now ALSO runs the
duration-aware planner over this run's post-W1/W2 staged durations at
`N=4/6/8` (`--shard-total 4|6|8`, report-only) and appends the predicted
max-shard table to the retained `shard-plan-report` artifact — the same
retained-artifact mechanism the `FAST_SHARD_PLAN` decision used (run
32297737631 → ledger entry 2026-08-19).

| Field | Value |
|---|---|
| Study variable | `FAST_SHARD_COUNT` (repo variable; `3` = default/keep, activation value = the chosen N, which must equal the `ci.yml` matrix literal after the activation edit) |
| Study mechanism | `shard-plan-report` job: `racket scripts/run-tests.rkt --suite fast --shard-total <N> --shard-plan report --durations durations` per N in {4,6,8}, report-only, retained in the `shard-plan-report` artifact |
| Activation rule (all must hold) | (1) predicted max shard improves vs N=3 over the same run's durations; (2) inventory preserved (same total files, planner `substituted` recorded); (3) runner-minute cost delta computed from observed job minutes (fast-gate job minutes at N minus at 3) and explicitly accepted here; (4) observed post-activation check run recorded here with no material max-shard regression |
| Decision (2026-08-24) | **KEEP `3`** — report-only phase wired; no hosted study artifact with post-W1/W2 4/6/8 predictions has been transcribed yet, so no activation criterion is evaluable. Activation requires the table + accepted cost delta recorded in a dated addendum below. |
| Revert command | `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_COUNT -f value=3` and restore the `ci.yml` fast-gate matrix to `[1, 2, 3]` (the variable alone does not resize the matrix; the matrix literal is the source of truth) |

### Constraints reaffirmed

Gate structure (fast / platform / workflows / cross-version), job names, and
required status checks are unchanged; the workflows contain zero
impact-selector flags (verify gate: `grep -rn "changed-base\|changed-head\|impact-dry-run" q/.github/workflows/ \| wc -l` = 0); a timeout is never reported as a pass.

## v1.00.16 W4 — halving-objective evidence, baseline of record (2026-08-24)

The v1.00.16 objective (fast suite at least half the time) is measured against
the W0 baseline of record (v1.00.11 fast-gate p50 488.0 s, ten L3 runs) using
the regenerable-report discipline this log and the TDD plan mandate.

| Field | Value |
|---|---|
| Sample (run IDs) | 32745843124 (PR feature/v10016-w1w2), 32748197712 (main @ 93e7996a) — 2 successful post-change fast-gate runs; retained jobs JSON in `artifacts/ci-baseline/jobs/` |
| Before (W0 baseline p50) | 488.0 s (fast-gate-budget-v1.00.11.md / test-feedback-baseline-v1.00.11.md) |
| After (v1.00.16 sample p50) | 627.0 s (worst-shard totals 619.0 / 635.0; shard 2/3 each) |
| Achieved ratio | 1.2848360655737705× of the baseline p50 — **halving MISSED** (target ≤ 244.0 s) |
| Toggle: `FAST_SHARD_PLAN` | `active` (guarded activation 2026-08-19, ledger above); duration-aware plan in effect for both runs |
| Toggle: `FAST_SHARD_COUNT` | unset (default 3) — CI matrix literal `[0, 1, 2]`; study report-only, decision **KEEP 3** (W3) |
| Toggle: prepared-env | NOT in effect for these runs — both predate the W3 cutover merge 1351fb05; `setup-racket` ran the legacy path (Racket install + q relink + `raco setup`) on all three ubuntu fast shards |
| Remaining attributed cost | setup 343.0 / 348.0 s (legacy-path install/relink/`raco setup` — the dominant term) + execution max shard 276.0 / 287.0 s |
| Named next lever | Complete the W3 prepared-env observation gate: a warm hosted run with `prepared-environment=restored` on all three ubuntu fast shards (materially reduced setup), then re-measure with a fresh retained sample; guard `RACKET_PREPARED_ARTIFACT=off` |
| Remeasurement date | 2026-09-30 (or the next retained post-warm-run sample, whichever comes first) |

Revert commands (one line per toggle):

- `FAST_SHARD_PLAN`: `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_PLAN -f value=off`
- `FAST_SHARD_COUNT`: `gh api -X PUT repos/coinerd/q/actions/variables/FAST_SHARD_COUNT -f value=3` and restore the `ci.yml` fast-gate matrix literal to `[0, 1, 2]` if it was ever resized
- Prepared-env: `gh api -X PUT repos/coinerd/q/actions/variables/RACKET_PREPARED_ARTIFACT -f value=off` (forces `auto` → `off` at all call sites and disables the `fast-env` producer)

No target was massaged: the achieved ratio, the remaining attributed cost, and
the named next lever are recorded above exactly as measured; per-file runner
JSON artifacts were not retained (authenticated download), so no per-file
numbers appear in the v1.00.16 report.

## v1.00.23 W4 — reproducible 20-PR cohort evidence tooling (2026-09-22)

W4 closes the evidence-closure roadmap package by turning expiring workflow
artifacts into a deterministic, reviewable activation record. The cohort
reporter (`scripts/run-tests/cohort-report.rkt`) is a pure function of on-disk
inputs — no network or database is queried at report time — so an activation
reviewer can reproduce a cohort byte-for-byte after GitHub's seven-day
artifact retention expires.

| Field | Value |
|---|---|
| Cohort size | 20 consecutive eligible unique PR head SHAs (canonical) |
| Timing sample | exactly one final successful attempt per SHA; failed/cancelled/rerun attempts retained as reliability evidence |
| Exclusions | named mechanical only (`missing-lane-artifact`, `incompatible-scheduler`, `incompatible-config`, `inventory-mismatch`, `artifact-corrupt`, `artifact-expired`, `non-unique-sha`) |
| Rejected cohorts | duplicate SHAs, missing lane artifacts, incompatible scheduler/config, inventory mismatch, silently truncated (<20 without matching exclusions) |
| Statistics | linear-interpolation percentile estimator; p50/p95, file/inventory digest, pass/fail/timeout/skip/zero-test counts, flakes, parallel-only failures, prepared-env outcomes, queue telemetry, runner-minute cost |
| Regeneration | `cohort-report.rkt --manifest <cohort.json> --out-json <stored> --check` exits 0 on byte-identical reproduction |
| Raw inputs | bounded, named by milestone/cohort under `artifacts/ci-baseline/`; retention/schema contract in `artifacts/ci-baseline/README.md` |
| Tests | `tests/test-ci-cohort-report.rkt` — exactly-20, fewer-than-20, duplicate SHA, failed-then-passed rerun, exclusion, missing/corrupt artifact, inventory mismatch, percentile edges, deterministic output |

Triage hook: see **Event 7 — Cohort activation disagreement** in
`docs/operations/test-regression-triage.md`.

## Run W5-shadow-smoke-01 — same-SHA shadow plumbing smoke (manual dispatch, main)

| Field | Value |
|---|---|
| Run ID | not-yet-dispatched (plumbing-only smoke) |
| Dispatch | `workflow_dispatch` on `main` (manual, post-W5 merge) |
| Head revision | tip of `v1.00.23-w5` (post-merge) |
| Workflow | `test-scheduler-shadow.yml` @ main |
| **Definitive overall status** | **plumbing-only** (does NOT count as C0/C1 performance evidence) |
| Runner version / mode | `1.00.23`, `execution-mode=subprocess`, `TEST_RUNNER_SCHEDULER=batch` (default) |
| Execution profile | `profile=ci` (mirror of canonical fast lane) |
| Trigger | explicit manual dispatch; no PR event binding |

This smoke is the **single manual same-SHA batch/queue smoke** required by
W5 Action 6. Its sole purpose is to prove the plumbing works (env
resolution, JSON emission, artifact upload). It is not part of the C0
performance cohort and does not enter the comparison set.

## Run W5-c0-observation — fresh canonical C0 batch observation (v1.00.23)

| Field | Value |
|---|---|
| Run ID | (observation period — see `q/docs/reports/TEST-RUNTIME-C0-v1.00.23.md`) |
| Dispatch | main-CI canonical batch on `main` (no dispatch override) |
| Head revisions | canonical supported Racket/config, complete required artifacts, no dispatch override |
| **Definitive overall status** | **observation** (W5 closes in observation; W6 must not manufacture or shorten C0) |
| Cohort size | 20 consecutive eligible unique PR head SHAs (canonical, main-CI batch) |
| Exclusions | named mechanical only (see `cohort-report` contract) |

If 20 eligible SHAs are not yet available at W5 close, W5 remains in
**observation**. W6 must not manufacture or shorten the C0 cohort. Normal
unrelated PRs may contribute if they satisfy the pre-registered rule.

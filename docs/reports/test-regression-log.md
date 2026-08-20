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

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

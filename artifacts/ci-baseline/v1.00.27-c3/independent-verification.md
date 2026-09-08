# Independent Verification — Cohort v1.00.27-c3 (v1.00.27 §8 W5)

This is a **second-channel review** performed by an agent process
(`independent-verifier`) that is **independent of the implementing executor
session**. The implementing session's artifacts were read; the verdicts were
re-derived from primary data (per-SHA samples in `cohort.json`), not accepted
from `report.json` or `decision.md`.

- Review date: **2026-09-19**
- Git root under review: `/home/user/src/q-agent/q`
- Artifacts reviewed: `artifacts/ci-baseline/v1.00.27-c3/{cohort.json,report.json,decision.md}`

## Reviewed-input digests

| file | sha256 |
|---|---|
| `cohort.json` | **UNVERIFIABLE IN THIS CHANNEL** (see Limitations) |
| `report.json` | **UNVERIFIABLE IN THIS CHANNEL** (see Limitations) |

The verification tool channel available to this agent exposes only
read/write/edit/grep/find/ls primitives — no shell execution. `sha256sum`
could not be run, so no digest value is asserted. Both artifacts were read in
full from disk; the reviewed content is what appears in the tables below.

## Method

- **Script path:** `/home/user/src/q-agent/tmp/iv-c3-rederive.rkt` (throwaway,
  helper `(define (file->json p) (with-input-from-file p read-json))`,
  `require racket/file json`). The script encodes the checks below, parses
  both artifacts with `read-json`, collects the final attempt per SHA
  (`run-id = _final_run_id`), and applies the fixed thresholds.
- **Execution limitation (disclosed, honest):** this tool channel cannot
  spawn processes, so the script was staged but **not executed**. Every
  numeric re-derivation reported below was therefore computed by hand from
  the per-SHA samples in `cohort.json`, using the identical formula the
  script encodes: exact linear interpolation on the sorted sample vector with
  rank `r = q·(n−1)`, `lo = ⌊r⌋`, `hi = ⌈r⌉`, value `x_lo + (r − lo)·(x_hi − x_lo)`,
  rounded to 2 decimals.
- **Re-derived:** all 7 row observed values; all 7 verdicts against FIXED
  roadmap §8 thresholds; sample counts; SHA count/uniqueness; attempt
  reliability counts; prepared-env record counts; guard-reference resolution
  (paths resolved against the git root, parentheticals stripped).
- **Taken from artifacts:** report-versus-decision consistency of wording;
  guard semantic quality (existence of referenced files confirmed, content
  semantics not independently re-proven); the C2 reliability baseline record
  in `cohort.json` (`reliability-baseline`, not recomputed here).
- **Raw samples present:** cohort.json carries per-SHA per-attempt timing
  fields (`fast-execution-seconds`, `pr-elapsed-seconds`,
  `security-runner-seconds`, `workflows-runner-seconds`), so observed values
  were recomputed from raw samples, not merely checked for `samples: 20 /
  expected: 20` (which was checked and holds for every timing row).

## SHA accounting (re-derived)

- cohort.json `shas[]` entries: **20**; unique SHAs: **20**; no duplicates,
  none dropped. `expected-count: 20` (cohort.json and report gate) — match.
- `exclusions`: **12** entries, all `lane-run-failed`; the 12 excluded SHAs
  are disjoint from the 20 included SHAs, so exclusions do not reduce the
  included set below 20. `window-eligible-count: 20` — consistent.
- report gate `unique-head-shas: 20`, `expected-count: 20`,
  `cohort-status: closed` — consistent with cohort.json.

## Per-row table

| row | recomputed/checked observed | fixed threshold | re-derived verdict | report verdict | agree? | guards provided? | guard refs resolve? |
|---|---|---|---|---|---|---|---|
| fast-p50 | 267.50 (from 20 samples; interp rank 9.5 → (265+270)/2) | ≤ 115.0 | target not achieved | target not achieved | YES | YES (4/4) | YES |
| fast-p95 | 285.95 (rank 18.05 → 278 + 0.05·(284−278)) | ≤ 135.0 | target not achieved | target not achieved | YES | YES (4/4) | YES |
| pr-ci-p50 | 1043.50 (rank 9.5 → (1039+1048)/2) | ≤ 588.0 | target not achieved | target not achieved | YES | YES (3/3) | YES |
| pr-ci-p95 | 1175.65 (rank 18.05 → 1144 + 0.05·(1177−1144)) | ≤ 735.0 | target not achieved | target not achieved | YES | YES (3/3) | YES |
| security-runner-p50 | 685.50 (rank 9.5 → (684+687)/2) | ≤ 240.0 | target not achieved | target not achieved | YES | YES (3/3) | YES |
| workflows-runner-p50 | 695.50 (rank 9.5 → (694+697)/2) | ≤ 220.0 | target not achieved | target not achieved | YES | YES (3/3) | YES |
| prepared-env-verified-restores | 100.00 (24 verified / 24 records, 0 fallback) | ≥ 95.0 | pass | pass | YES | YES (2/2) | YES |

Sample-count integrity: every timing row has `samples: 20 / expected: 20`;
the prepared-env row has `samples: 24 / expected: 20` — the 24 is the record
count over 24 prepared-env records (3 test shards × 8 prepared-env-cutover
SHAs) and is the correct basis for a rate; the rate (24/24 = 100.0) is what
was recomputed and compared to the fixed ≥ 95.0 threshold.

## Reliability re-derivation

From cohort.json `shas[].attempts[]` (counted across all included SHAs):
**21 attempts** (one SHA, `829b6f76…`, carries 2 attempts; the other 19 carry 1),
**21 successes**, **0 failures**, **0 reruns**, **0 cancelled** (no attempt
record carries a cancelled/failed result; the final attempt per SHA matches
`_final_run_id` in all 20 cases). This matches the report
`final-claim-gate.reliability` (21/21/0/0/0) exactly. The recorded C2
baseline (`0 failures, 0 reruns, 0 cancels`) is therefore also met →
`reliability-non-regression: true` is supported. The reliability guard entry
is provided on every row.

## Prepared-env re-derivation

cohort.json `prepared-env-restore-stats`: total 24, verified 24, fallback 0,
records-observed 24, rate 100.0. Independent recount from the per-SHA data:
8 SHAs carry `prepared-env: "match"` with `prepared-env-record-outcomes:
["verified"]` (3 shards each → 24 records); each of those 8 SHAs' final
attempt carries `prepared-env-report-job: "success"` (8 of 8). 12 SHAs carry
`prepared-env: null` with no report-job field and are correctly **not**
counted as records (pre-cutover runs, outside the stated prepared-env
contract), leaving 24 counted records / 24 verified / 0 fallback / rate
100.0. Matches the report prepared-env block (24/24/0, rate 100.0,
`cohort-status` consistent, window `run_started_at > 2026-08-31T17:14:49Z`).
Rate 100.0 ≥ 95.0 fixed threshold → pass, agrees with report.

## Guard references

All guard entries marked `provided: true` resolve to existing files at the
git root (parentheticals stripped; second path of the two-file
`prepared-env-no-bypass` reference checked separately — both exist):

- `artifacts/ci-baseline/v1.00.27-c3/cohort.json` — exists ✓
- `artifacts/ci-baseline/v1.00.25-c2/cohort.json` — exists ✓
- `docs/reports/gsd-wave-evidence/v1.00.27-w2.rktd` — exists ✓
  (used by `semantic-gate-equivalence` and `shared-state-permission-isolation`)
- `docs/reports/gsd-wave-evidence/v1.00.27-w3.rktd` — exists ✓
- `docs/reports/gsd-wave-evidence/v1.00.27-w4.rktd` — exists ✓
- `.github/workflows/ci.yml` — exists ✓ and does contain the guarded
  `prepared-environment: auto` input and the required `prepared-env-report`
  job (inspected directly)

No row is missing guards; every `pass` row (prepared-env-verified-restores)
has all 2 of its guards provided. No guard-less pass exists.

## decision.md consistency

`decision.md` per-row table (observed values, thresholds, verdicts) matches
`report.json` `final-claim-gate.rows` verbatim and matches the re-derivation
above. Its overall verdict `target not achieved` ("six of the seven §8 rows
miss their fixed thresholds… the prepared-environment row passes") is
consistent with the re-derived row verdicts. Queue disposition (no rollback
on a timing miss with zero reliability regression) matches the stated
decision rule.

## Overall

**AGREE — decision.md "target not achieved" is supported by independent re-derivation.**

## Discrepancies

None in the artifacts. One verification-channel limitation, recorded for
honesty: the throwaway Racket script
(`/home/user/src/q-agent/tmp/iv-c3-rederive.rkt`) could not be executed and
`sha256sum` could not be run because this tool channel has no process-spawn
primitive; percentiles were recomputed by hand from the same raw samples with
the same `q·(n−1)` interpolation the script encodes, and the sha256 digests
are recorded as UNVERIFIABLE rather than fabricated. If a digest check is
mandatory for the audit trail, a channel with shell access must re-run it;
nothing in this re-derivation depends on the digests being wrong.

## Scratch-file cleanup note

Task instruction 6 (delete the scratch script) could not be performed: this
tool channel has no delete/process primitive. The scratch file
`/home/user/src/q-agent/tmp/iv-c3-rederive.rkt` has been overwritten with a
tombstone stub recording exactly this limitation; it is outside the repo
tree (`q/`) and writes no other files.

---

(signed) independent-verifier agent process, second channel; not the implementing executor session

# Baseline recovery cohort — v1.00.30 R2

**Status: `COHORT OPEN`. R2 was not evaluated.**

This record is the W5 output required by
`.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md` for a period in which genuine
eligible PR traffic did not accumulate. It is deliberately a *negative*
result: it reports that the population never opened, and it reports no
performance statistics.

## Verdict vocabulary

The plan fixes a closed vocabulary, and the labels are not interchangeable:

| label | defined as |
|---|---|
| `ACHIEVED` | R0–R4 pass |
| `RECOVERED — PRIOR TARGET MET, NORTH-STAR NOT ACHIEVED` | R0–R3 pass, R4 fails |
| `RECOVERED — BASELINE ONLY, PERFORMANCE OBJECTIVE NOT ACHIEVED` | R0–R2 pass, R3 fails |
| `NOT ACHIEVED — RECOVERY INCOMPLETE` | **R2 fails** |
| `BLOCKED — SAFETY/INTEGRITY` | any safety failure |
| `COHORT OPEN` | insufficient evidence, **not a terminal success** |

`NOT ACHIEVED — RECOVERY INCOMPLETE` is **not** applied here. That label is
reserved for an R2 that was evaluated and failed. R2 was never evaluated, so
using it would assert a measurement that does not exist. The correct state is
`COHORT OPEN`.

Likewise, W7 is the wave that produces the single final verdict, and **W7 was
never reached**. No v1.00.30 terminal verdict has been issued, because issuing
one would require a wave that did not run.

## Window

The cohort window opens strictly after the W4 final activation merge:

- commit `cc25b3663362d032b9a23a9d22f9dd5edbe637d8` — *"W4 delivery: campaign/v1.00.30-w4 (#9752)"*
- committer date `2026-09-25T04:10:14+02:00`, i.e. **`2026-09-25T02:10:14Z` UTC**

The commit carries a `+02:00` offset. Taking its local wall-clock time as if it
were UTC shifts the window two hours later and **silently drops PRs #9753 and
#9754** from the candidate set. #9754 is a genuine, execution-affecting PR and
would otherwise have been invisible. The cutoff is therefore recorded in UTC,
with both representations retained in `cohort.json`.

Related defect: `artifacts/ci-recovery/v1.00.30-w4/activation.json` still reads
`"activation-merge-sha": "PENDING (filled at protected merge)"`. The merge
exists and is verified, so that field was never backfilled.

## Candidates

Six PRs were created strictly after the window opened.

| PR | head | created (UTC) | exec files | doc files | eligible |
|---|---|---|---|---|---|
| #9753 | `839de8b61` | 2026-09-25T02:15:13Z | 0 | 3 | **no** |
| #9754 | `49d9bb088` | 2026-09-25T03:18:34Z | 1 | 0 | yes |
| #9755 | `435d59031` | 2026-09-25T18:01:31Z | 69 | 6 | yes |
| #9756 | `ee31a8a67` | 2026-09-27T08:40:30Z | 0 | 3 | **no** |
| #9757 | `c5375e605` | 2026-09-27T18:21:13Z | 1 | 0 | yes |
| #9758 | `5dfd34344` | 2026-09-27T21:56:37Z | 3 | 0 | yes |

Exclusions, with reason: #9753 and #9756 are binding-only publications touching
solely `docs/reports/gsd-wave-*.rktd`. The plan states *"Docs-only/no-execution-change
exempt PRs do NOT fill performance cohorts"* — such PRs take the trusted
reporter's `NOT_APPLICABLE` result instead.

## Result

```
required eligible heads        20
unique eligible heads           4
shortfall                      16
r2_evaluated                  false
status                 COHORT OPEN
```

`4 ≠ 20`. The cohort never opened.

**No samples were manufactured.** The plan is explicit: *"Wait for genuine
traffic; don't generate empty commits to get 20 samples"* and *"No
dummy/empty commits or manufactured PRs solely to satisfy sample count; if
development produces fewer observations, hold."* Fabricating sixteen PRs would
produce an R2 whose number is a property of those PRs rather than of the
codebase, and would be indistinguishable from a genuine recovery on the page.

## Comparability caveats

The four eligible heads are honest but are **not** a representative latency
sample, and the record says so rather than leaning on them:

- **#9754** is a W4-era test-harness fix created 68 minutes after activation.
  Strictly inside the window and execution-affecting, so it is counted, but it is
  campaign maintenance rather than feature traffic.
- **#9755** and **#9758** are campaign implementation work (69 and 3 changed
  files). Their runtime is dominated by their own change size, not by
  repository-wide cost.
- **#9757** changes only `.github/workflows/release.yml`; it exercises the CI
  definition, not the codebase.

## Why no statistics are reported

`report.json` carries `measured: false` and no p50/p95, trigger/queue time, max
fast-shard time, or runner-minute figures. Those quantities have no valid
population. The `820.5` / `850.3` and `2558` / `3023` baselines appear in
`report.json` explicitly marked **for context only** — they are not achieved
values and were not recomputed.

R2's threshold (closed-cohort p50 ≤ 820.5 s) is recorded as
`evaluated: false`. It was neither met nor missed; it was never tested.

## Consequences

- **W5 is not complete.** The plan forbids marking it complete while a cohort is
  open. It stays halted at `COHORT OPEN`.
- **W6 is forbidden.** *"W6 is forbidden otherwise."*
- **W7 and W8 are not authorized**, and no terminal verdict exists.
- R0 and R1 remain genuinely delivered; that work stands on its own records.

The plan's pre-registered policy for this situation is to check `COHORT OPEN`
every 7 calendar days and, at 30 days without enough records, *"stop automated
retries and issue a coordinator resumption/amendment request."* *"Deadlines
never authorize a pass."*

This milestone is being closed for real, so that policy is now exhausted:
resumption requires a separately reviewed new plan or genuine traffic, decided
by an operator.

## Provenance

- Generator: `raw/build-cohort.py` — deterministic, re-runnable, no hand-entered values.
- Raw captures: `raw/post-w4-pulls-raw.json`, `raw/pr-<n>-files.json`, `raw/pr-<n>-runs.json`.
- Checksums: `SHA256SUMS` over this directory, verified with `sha256sum -c`.
- Regenerating from the same raw captures reproduces both JSON files byte-for-byte.

## Why there is no schema-2 evidence/review/validation trio here

The W5 file list names `docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.30-w5.rktd`.
Those records are **not** created, deliberately.

A schema-2 trio is the wave *delivery* instrument. Its `content-digest` is not a
value a human writes: `scripts/gsd-evidence-bind.rkt` computes
`excluded-evidence-digest` as a `git diff` digest at a specific head, and
`bind-records!` is documented as *"the only sanctioned way a content-digest
comes into existence."* A record is only bound when a head exists that is being
delivered.

W5 is not being delivered. Its cohort never opened, so the wave cannot be
marked complete (*"Never mark this wave complete while a cohort is open"*), and
writing a trio — even one labelled `COHORT OPEN` — would place a delivery
instrument on disk for a merge that is not authorized. Hand-writing a digest
would be fabrication; letting `bind-records!` write one would imply a delivery
gate that must not be passed.

What is committed instead is the observation output itself: the cohort record,
the report, the raw captures, their checksums, and this report. That is what W5
actually produced.


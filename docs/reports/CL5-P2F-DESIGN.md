# CL5 P2-F — CI-matched measurement feasibility (pre-registration)

Status: **design frozen before data collection.** No measurement has been run under this
protocol. Review of this document happens before any workflow dispatch.

## 1. Decision and scope

P2-E's six-hour local measurement is **deferred**, not invalidated. PR #10016 established that this
workstation is 2–4× slower than the CI runner on the files that could be compared directly, so a
local baseline cannot participate in a CI-efficiency claim. P3 collection continues unchanged. P1,
W3.3, production selection and the 40 % claim remain held.

P2-F asks one question only:

> Can a controlled, evaluation-only job on the production CI host class produce **paired,
> comparable measurements** of a frozen workload, with recorded provenance and no change to
> production test selection?

A GO authorizes the *design* of expensive paired comprehensive measurements. It does not authorize
running them, does not produce a baseline, and does not support any speedup claim. A NO-GO is a
feasibility finding, not a negative efficiency result.

Per the owner's rule, this is the only infrastructure change being made: it removes the demonstrated
blocker — "there is no measurement environment representative of the optimization target". No other
experiment machinery is authorized in this wave.

## 2. The demonstrated blocker

CI per-file timings below are from the green main run `45861840409` (commit `61af492c`,
`test-results-fast-*` artifacts). Local timings are from the 1 400-file single-invocation reference
surface at `--jobs 4` on this workstation (recorded in `.planning/PLAN-CL5-P2-P3.md`; the raw JSON was
deleted during disk recovery, so these are retained as rounded diagnostic values, **not** as
acceptance inputs):

| File | CI (s) | local (s) | local/CI |
|---|---:|---:|---:|
| `test-gsd-delivery-coordinator.rkt` | 29.9 | 120.2 (timeout) | ≥ 4.0 |
| `test-gsd-go-orchestrator.rkt` | 32.2 | 79.2 | 2.5 |
| `test-gsd-delivery-binding.rkt` | 55.5 | 124.9 | 2.2 |
| `test-milestone-gate.rkt` | 55.9 | 95.3 | 1.7 |
| `test-qt-lanes.rkt` | 56.7 | 110.1 | 1.9 |

Consequences already observed:
- locally calibrated limits are conservative for CI on these files, but that is **not** a guarantee —
  individual CI workloads can behave differently; `test-gsd-delivery-binding.rkt` and
  `test-gsd-go-orchestrator.rkt` were well under their local limits in the reference run yet sat at
  180 s and 120 s in CI shards (`45969460888`, attempts 1–2);
- nested measurement runners inside a 4-job shard made co-scheduled heavy files time out twice
  (`45969460888` attempts 1–2), so instrumentation must be isolated in its own job;
- a flag can be correctly documented and completely inert (`--at-real-path`, PR #10016, review
  round 3), so wiring is asserted by a test rather than by prose.

## 3. Host definition

Production CI is defined by `.github/workflows/ci.yml`: `runs-on: ubuntu-latest`, with the provenance
block pinning `runner_image: ubuntu-24.04`, `architecture: x86_64`, `racket_version: 8.10`,
locale `C.UTF-8`, timezone UTC; the fast gate runs `--jobs 4` across three shards. Racket is installed
by `./.github/actions/setup-racket` with `racket-version: '8.10'`.

The pilot therefore runs on GitHub-hosted `ubuntu-latest` at `workflow_dispatch`. This is the same
**class** of host, not a reserved physical machine: CPU model and shared-host contention can differ
between runs. That is precisely why the pilot compares distributions rather than single timings.

Every run records: `uname -a`, `/etc/os-release`, `nproc`, CPU model, memory, cgroup CPU quota
(`/sys/fs/cgroup/cpu.max`), load average before and after, Racket version and executable digest, the
prepared-environment state and its restore/fallback timing, the exact command argv, and the wall time
of each invocation. All of it is uploaded as an artifact alongside the per-file `--json-out`.

## 4. Frozen workload

Pinned test tree: `e56b3f82` (the main head when this design was frozen). The dispatch SHA must equal
`github.sha` and the checked-out HEAD. The workflow verifies that the pinned SHA is an ancestor and
that `git diff --name-only e56b3f82 HEAD` contains **only** these four paths: the workflow file and the
three `docs/reports/CL5-P2F-*` files. Every test file, runner module and production CI definition is
therefore byte-identical to the frozen tree; any other difference aborts before Racket is installed.

Amendment after design approval (demonstrated blocker): the first deployment put the workflow only on
a feature branch, and dispatch by workflow filename returned **404 Not Found** — GitHub resolves the
workflow against the default branch. The workflow is therefore merged to `main` as a dispatch-only,
inert file. It has no push/PR trigger, is not a required check, and changes no test selection; the
tree check above is what preserves pinning. The earlier "not merged to `main`" sentence was wrong and
is corrected here before any data was collected.

Pilot subset (eight files, exact list also in `CL5-P2F-WORKLOAD.json`):

| File | CI elapsed (ms) at `45861840409` |
|---|---:|
| `tests/test-gsd-evidence-repair-production-contract.rkt` | 131 906 |
| `tests/test-full-regression-invocation.rkt` | 120 390 |
| `tests/test-gsd-delivery-merge.rkt` | 88 002 |
| `tests/test-gsd-auto-review-loop-contract.rkt` | 85 687 |
| `tests/test-gsd-supplemental-disposition-store.rkt` | 79 339 |
| `tests/test-gsd-delivery-read-ladder.rkt` | 76 426 |
| `tests/test-milestone-gate.rkt` | 55 927 |
| `tests/test-gsd-go-orchestrator.rkt` | 32 234 |

These include long-running GSD tests, the four files where CI and local differed most, and files at
both ends of the duration range. All eight passed in that CI run, so none of the reference values is
censored.

Pilot command, identical for every run:

```
racket scripts/run-tests/runner.rkt --jobs 4 --json-out cl5-p2f/run-<i>.json <the eight files>
```

The workflow verifies the SHA-256 of `CL5-P2F-WORKLOAD.json`
(`3e93bd21ec840814bdd6d8c26a3ca9314602e4e9e64ef9987e46975bfd380dba`) and derives the file list from
it; a changed workload under the same path check cannot silently pass.

It uses the same runner and job count as production, but **not** the shard flags; per-file elapsed is
the comparable quantity, not total CI wall. Two further differences are recorded rather than hidden:
(a) the isolated job has no same-run prepared-environment producer, so `setup-racket` is called with
`prepared-environment: off` and its own outputs (`prepared-env-state`, cache state, setup duration)
are recorded — this affects preparation overhead, not the runner's per-file elapsed; (b) a production
shard co-schedules ~452 files while this pilot co-schedules 8, so contention density differs. If A4
fails on this surface, host comparability is **not established** — no post-hoc redefinition of the
surface.

One warm-up invocation (recorded, never a sample), then five measured invocations. The eight files
sum to 670 s of CI elapsed; with no overlap at all that is 4 020 s (67 min) for six invocations, so
the job timeout is **120 min**. A job timeout is an infrastructure failure and is reported with both
attempts; it is never converted into a sample. No nested runners, no shards, no retries inside the
job, no modification of test files, timeouts, selectors or production CI configuration. The workflow
is stored as a reviewed template under `docs/reports/` and deployed as a dispatch-only file at
`.github/workflows/cl5-p2f-pilot.yml`; it has no push/PR trigger (see the amendment in §4).

The eventual comprehensive workload is defined here but not executed: enumerate the complete union of
the eight CI producers at one pinned revision with `./qt --lane … --explain`, record its digest, count
**1 492 execution slots and 1 397 unique files if that revision still matches**, and keep the counts
distinct. Its wall time is a *single-invocation comprehensive reference* at `--jobs 4`, explicitly not
the eight-producer CI schedule; `Σ producer wall` is a separate resource-cost measure.

## 5. Pre-registered acceptance criteria

All six must pass for GO. Every test below is decided from the uploaded artifacts alone.

**A1 — host.** OS Ubuntu 24.04, x86_64, Racket 8.10, job count 4, and all metadata fields present.
An absent or mismatched field is NO-GO; "it should be 24.04" is not evidence.

**A2 — integrity.** The warm-up plus five measured artifacts exist and parse; each measured artifact
contains exactly the eight paths; every file status is `pass` in every measured run; zero timeouts,
zero failures, zero missing results. A censored or missing run is NO-GO — never silently dropped.

**A3 — stability.** Across the five measured runs, `(max total wall − min total wall) / median total
wall ≤ 0.15`. Above that threshold the host is not stable enough for paired measurement: NO-GO.
No extra runs are added after seeing the data, so the threshold cannot be tuned post hoc.

**A4 — comparability.** For each of the eight files, `median(pilot elapsed) / CI elapsed` lies in
`[0.5, 2.0]`; the pooled median of those eight ratios lies in `[0.67, 1.5]`. With five samples the
pilot p95 equals the maximum, so it is a lower bound and is reported but not used as a gate. A single
file outside its interval is a NO-GO with that file named — no averaging away an outlier.

**A5 — isolation.** The job contains exactly one runner process per invocation, no `./qt` shards and
no nested runner. The only `.github/` addition is the dispatch-only pilot workflow itself: it has no
push/PR trigger, is not a required check, and selects no tests. The four-path tree diff pins the
tested tree; the dispatch SHA, workflow SHA and pinned SHA are recorded as distinct fields. A
violation is NO-GO regardless of timings.

**A6 — provenance.** Cache state, preparation overhead, load and exact argv are recorded for each
run; warm-up is excluded from all statistics. Missing provenance is NO-GO.

## 6. Analysis and reporting

After dispatch, the artifacts are downloaded and reduced to `CL5-P2F-PILOT.json`
(`cl5-p2f-pilot/1`) with: host metadata, per-run wall, per-file elapsed and status per run, per-file
min/median/max/spread, the eight ratios, all six criteria with their observed values, and the run
URLs. The record must be independently re-derivable from the artifacts; a verifier flag is acceptable
only if it re-reads them rather than trusting a self-declared verdict. Analysis also checks that
`provenance.txt:workflow_sha` equals the reviewed deployment commit (the head of PR #10018 at merge):
the tree pin lists the workflow itself, so its identity must be checked from the recorded SHA rather
than from that self-referential list.

The report states explicitly: which host was used, which SHAs, what was and was not reproduced from
production CI, whether the eight-file pilot can support a paired selected/comprehensive experiment,
and what remains unmeasured. No baseline, no selected-arm result, no efficiency or 40 % claim.

## 7. Authorized and not authorized

Authorized: this pre-registration; the inert workflow template and its dispatch-only deployment; one dispatch of the eight-file pilot
(and one repeat only if the run itself failed for infrastructure reasons, with both runs reported);
artifact analysis; independent review of both the design and the result.

Not authorized: restarting P2-E locally; any comprehensive-cost run; a selected arm;
changes to production test selection, CI definition, timeout policy, or P3 eligibility; P1; W3.3;
any efficiency claim.

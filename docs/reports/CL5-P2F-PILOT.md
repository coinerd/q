# CL5 P2-F — CI-matched measurement feasibility: pilot result

Status: **NO-GO** under the pre-registered criteria in `CL5-P2F-DESIGN.md`. This is a feasibility
finding, **not** an efficiency result, a baseline, or a negative measurement.

## What was attempted

Two dispatches of the frozen eight-file workload on GitHub-hosted `ubuntu-latest`, from the reviewed
dispatch-only workflow at `main` head `5f4e3940`, pinning the tested tree to `e56b3f82` by a
seven-path diff. The design allowed one dispatch and one repeat for an infrastructure failure; that
budget is now exhausted. Both runs are reported; no third dispatch has been made.

| Run | Outcome | Evidence |
|---|---|---|
| [`46026806520`](https://github.com/coinerd/q/actions/runs/46026806520) | infrastructure failure before setup | depth-1 checkout could not resolve the pinned SHA (`fatal: Not a valid commit name e56b3f82…`, exit 128); zero artifacts |
| [`46032975158`](https://github.com/coinerd/q/actions/runs/46032975158) | failed at warm-up | tree pin, setup, workload materialisation and provenance succeeded; warm-up file `test-gsd-delivery-merge.rkt` failed; the job stopped before the five measured runs |

The raw artifact of run 2 is copied verbatim under `docs/reports/CL5-P2F-PILOT-EVIDENCE/` with SHA-256
digests in `CL5-P2F-PILOT.json`, so the findings can be re-derived without trusting the report.

## Criteria

| Criterion | Result | Evidence |
|---|---|---|
| A1 host | **FAIL** | measured host is **Ubuntu 26.04.1 LTS**; the pre-registered requirement was 24.04 |
| A2 integrity | **FAIL** | zero measured observations; one file failed in the warm-up |
| A3 stability | not evaluated | no measured wall timings |
| A4 comparability | not evaluated | no per-file distributions |
| A5 isolation | pass | dispatch SHA = workflow SHA; seven-path pin held; one runner per invocation; no nested runner or test-selection change |
| A6 provenance | pass | environment, provenance, file list, exact argv and load recorded; warm-up excluded |

## Finding 1: the host contract is asserted, not measured

`ci.yml` pins `runner_image: ubuntu-24.04` and `os_version: 24.04`, and the production proof bundle
for green run `45861840409` repeats those strings. The pilot's own `/etc/os-release` says
**Ubuntu 26.04.1 LTS** on the same `ubuntu-latest` label. Nothing in the production bundle records a
measured OS, so the discrepancy cannot be resolved retroactively: either `ubuntu-latest` moved after
that run, or the field has always been a hard-coded constant.

What *is* matched: x86_64, 4 CPUs, Racket 8.10 with the **identical executable SHA-256**
(`7674f178…`) to the production bundle, locale and package cache state. The machine is an Azure
`AMD EPYC 9V74` with 15 GiB.

Because A1 was fixed before data, it cannot be relaxed now. Accepting 26.04 would be post-hoc
threshold tuning. The correct next step is a separately reviewed change that records the **measured**
OS in the production CI evidence, establishes which host class the optimization target actually
uses, and re-pre-registers the criterion before any further measurement.

## Finding 2: a known flaky test blocks the workload

The warm-up failure was `tests/test-gsd-delivery-merge.rkt`:
`exn:fail: force: reentrant promise 'delay-ffi-obj'`. The same file and exception failed CI run
`46012479816` attempt 1 and passed attempt 2 (and the pilot workflow's own first attempt passed its
workflow checks after a full rerun). It is a pre-existing flake, not a property of the measured host,
but it stopped the protocol before any sample.

Fixing that test is a separate workstream with its own review; it changes a measured file and would
require re-freezing the workload. It is **not** done here.

## What P2-F did establish

- The evaluation-only workflow can be dispatched from `main`, pins the tested tree, records host and
  cache provenance, and runs the frozen workload with one runner and no nested processes.
- Three deployment blockers were found and fixed under review (branch-only workflow → 404;
  shallow checkout → unresolved SHA; CI declaration guards rejecting `python3` and a locally required
  parent-relative CLI module).
- The intended optimization target is not yet reproducible: the OS class is unverified and a known
  test flake can abort the workload. **No expensive comprehensive measurement is authorized.**

## Proposal for the owner (requires a new decision)

1. Authorize a separately reviewed fix that makes production CI record the **measured** OS/runner
   image, then re-pre-register A1 against that evidence.
2. Authorize a separately reviewed repair of the `delay-ffi-obj` reentrancy flake, or a protocol
   amendment that permits a bounded, declared retry for that specific known flake.
3. Only then re-run the eight-file pilot with a fresh dispatch budget.

Until then: P2-E remains deferred, P3 collection continues unchanged, and P1 / W3.3 / production
selection / the 40 % claim remain held.

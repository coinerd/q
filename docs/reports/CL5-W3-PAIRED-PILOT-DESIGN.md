# CL5 W3 paired-measurement feasibility — pre-registration (rev 1)

Status: **frozen before any paired-pilot data collection**. This is a methodological pilot only.
It does not continue P2-F, does not reuse P2-F's A4 threshold, and does not authorize the W3.2
campaign, production selection or an efficiency claim.

## 1. Decision and scope

The revised P2-F pilot established that the GitHub-hosted runner is stable enough for controlled
measurement (five measured runs, wall spread `0.004717`), but it failed the pre-registered
requirement to reproduce absolute timings from a different CI run. The owner accepted that NO-GO
and removed historical timing equivalence as a prerequisite.

This pilot asks only:

> Can selected and comprehensive verification be measured on the same runner, in the same session,
> under alternating order, with complete provenance and no cross-run timing assumption?

The quantity of interest is `S = 1 - T_selected/T_comprehensive`, where both terms are measured
here. `S` is reported per pair with no GO threshold. A positive value is not an efficiency result
and a negative value does not refute contract-aware selection; this pilot validates the measurement
method only.

## 2. Frozen case and reduced surface

- Pinned revision: `ecb92a5e846a48ee710d9e5b9bed9ca0b0631d5b`.
- Frozen workload: `docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json`, SHA-256
  `f4f557ef3a646c0977bb52683670586d848f02212b6d5aa837a831d6841cf0d8`; the workflow verifies this
  digest before deriving either surface.
- Case: D5, changed file `extensions/gsd/wave-status.rkt` (a frozen W3 case with a real
  contract-selection decision, not chosen for favorable performance).
- Decision step: `scripts/cl5/paired-pilot-selection.rkt` calls the evaluation-only
  `contract-escalation.rkt` on the pinned tree. Expected decision: `escalate` with
  `no-consumer-detector`; full required set 23 tests; decision digest recorded in the workload.
- Full selected arm would be `required ∪ comprehensive`. That is too large for a small pilot, so
  **both arms are projected onto the same frozen reduced surface**:
  - comprehensive reduced surface: the eight files in `CL5-W3-PAIRED-PILOT-WORKLOAD.json`;
  - selected projected surface: `required ∩ comprehensive_reduced_files` = two files:
    `tests/test-gsd-auto-review-loop-contract.rkt` and `tests/test-gsd-go-orchestrator.rkt`.
- This projection is a methodological device. It must never be reported as production-equivalent
  CI, a W3.2 trial, or evidence about the 40 % claim.

## 3. Controls and execution

- One dispatch-only workflow, no push/PR trigger, no test selection in production CI.
- Host is validated before any runner invocation: Ubuntu 26.04.1, image `ubuntu26:20261122.9.1`,
  x86_64/X64, AMD EPYC class, 4 cores, memory in `[15 500 000, 17 500 000] kB`, Racket 8.10 and
  executable SHA-256 `7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c`.
- Package cache warm, prepared environment explicitly off, `--jobs 4` for both arms.
- One warm-up invocation per arm (never a sample), then three measured pairs in frozen order:
  `selected → comprehensive`, `comprehensive → selected`, `selected → comprehensive`.
- Selection overhead is measured around the decision computation and added to the selected arm's
  wall for each pair. Runner wall and overhead are both recorded separately.
- No repeat is authorized for this pilot. A behavioral failure, host mismatch, missing artifact or
  degraded observation stops the experiment and is reported as `unavailable`; it is never retried
  to obtain a favorable result.

## 4. Pre-registered criteria

All must pass for the method to be usable; none produces a performance GO.

- **M1 — host.** Every required host field is present and matches; image is recorded, not inferred
  from `ubuntu-latest`.
- **M2 — integrity.** All warm-ups and six measured invocations exit 0; the selected arm contains
  exactly the two projected files and the comprehensive arm exactly the eight frozen files; every
  file status is `pass`; zero timeouts, failures or missing results.
- **M3 — pairing.** One warm-up per arm plus three measured pairs in the frozen order; every pair
  records both walls, both selection overheads, test counts and statuses. Warm-ups excluded from
  statistics.
- **M4 — isolation/provenance.** One runner process per invocation, no shards or nested runner;
  exact argv, cache state, preparation, load, host identity and decision digest recorded.
- **M5 — analysis.** Compute `S` per pair from the recorded values. Missing, censored or failed
  observations remain `unavailable`, never dropped. No threshold is applied to `S`; no GO is
  derived from this pilot.

## 5. Analysis and reporting

Download the artifact, reduce to a new `cl5-w3-paired-pilot/1` record with all individual values,
and independently re-derive M1–M5 from the artifacts. The report must state: host, image, revision,
case, decision digest, projected surfaces, all warm-ups and pairs, all selection overheads, all
failures/unavailable observations, and the explicit limitation that the reduced surface is not
production-equivalent. No W3.2 expansion is authorized by this report; independent review is
required before any expansion.

## 6. Authorized and not authorized

Authorized: this pre-registration; the frozen workload; the dispatch-only workflow; the decision
script and focused tests; one pilot run; artifact analysis; independent review before dispatch and
after results.

Not authorized: P2-F retry or threshold change; P2-E baseline; W3.2 campaign; W3.3 mutation
execution; production selector/CI/admission/scheduler change; timeout change; generic retry;
production selection; any efficiency or 40 % claim.

## 7. Revision 2 — workflow execution correction only

Attempt 1 (run `46123138578`) is retained as **unavailable**: the job failed before the first runner
invocation because `run_arm()` read an undeclared third shell parameter under `set -u`. No tests
ran, no timings were collected, and none of M1–M5 was evaluated. It is not excluded from the record
and is not a continuation point.

The owner authorized one fresh dispatch after a workflow-execution-only correction. Revision 2
changes only:

- removal of the unused `out="$3"` binding from `run_arm()` in the deployed workflow and its
  reviewed template;
- a lightweight local preflight in `tests/test-cl5-w3-paired-pilot.rkt` that extracts the deployed
  shell function, executes it under `set -euo pipefail` with stubbed `racket`/`date`/`cat`, verifies
  every scheduled call and its output contract, and demonstrates that the historical `out="$3"`
  form fails under `set -u`;
- this amendment recording the new attempt identity and preserving the original registration.

Revision 2 does **not** change the workload digest, pinned revision, case, decision script, host
requirements, preparation, cache assumption, concurrency, warm-up count, alternating order,
timing calculations, selection-overhead treatment, M1–M5 criteria, or the no-repeat rule. Attempt 2
is a distinct dispatch with its own SHA and run identifier. Success remains feasibility evidence
only; it is not a 40 % claim, a W3.2 campaign, or proof of general safety.

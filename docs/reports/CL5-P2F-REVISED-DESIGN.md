# CL5 P2-F revised feasibility pilot — pre-registration (rev 2)

Status: **frozen before any revised-pilot dispatch**. This document supersedes §3 and §5 of
`CL5-P2F-DESIGN.md` for the revised pilot only. The original pilot record and its exhausted dispatch
budget remain unchanged in `CL5-P2F-PILOT.json`.

## 1. Scope and decision point

This experiment answers exactly three questions:

1. Can an environment matching *measured production CI* be established and verified before any
   measurement?
2. Can the frozen eight-file workload execute reliably after the concurrency repair?
3. Can it produce complete, reproducible observations without setup failures, unrelated test
   failures, or changing host conditions?

A PASS authorizes consideration of a matched-host baseline only. It does not establish a speedup,
does not authorize the selected arm, and does not support the 40 % claim. A FAIL is a feasibility
finding; it must not be reclassified as an efficiency result.

## 2. Evidence that unblocked this revision

- **Observed production environment (PR #10023, merge `03fc798334f1d2c025ffb6b4fb3ffe0c63dbce41`).**
  Production push run `46063005906` recorded Ubuntu 26.04.1 LTS on image `ubuntu26:20261122.9.1`.
  Verbatim evidence: `.planning/evidence/CL5-PRODUCTION-CI-OBSERVED-03fc7983.json` (SHA-256
  `743b3490991433f7d7f5ae70fb5788303920dd456499549333093cb8b5f66dbb`). The earlier `24.04`
  assertion was false. This revision is based on measured evidence, not on where the old pilot ran.
- **Concurrency repair (PR #10022, merge `dd5c9995fc13bbc1253995768b2d78c4531e0a60`).**
  `racket/os`'s unsynchronized `delay-ffi-obj` promise was replaced on the staging-marker path by
  q's eager `util/lockfile` binding. No generic retry was added.
- **Residual known defect.** A different `malformed binding datum` failure in
  `tests/test-gsd-delivery-merge.rkt` is reproducible locally at base and head. It must be recorded
  as a behavioral failure if it appears here; it is not infrastructure and must not be retried to
  obtain favorable data.

## 3. Frozen revision, workload and command

- Pinned test tree: **`dd5c9995fc13bbc1253995768b2d78c4531e0a60`** (the first `main` after both
  blocking repairs). The dispatch `source-sha` must equal `github.sha` and checked-out `HEAD`.
- The diff from the pinned tree to the dispatch head must contain exactly the paths listed in
  `allowed_tree_diff_paths` in `CL5-P2F-REVISED-WORKLOAD.json`. Any other difference aborts before
  Racket is installed.
- Frozen workload file: `docs/reports/CL5-P2F-REVISED-WORKLOAD.json`
  (schema `cl5-p2f-workload/2`, SHA-256
  `cdba4c1a62ac9e3c572a03fb3eb8b495eea74ce9e86b6e4e35d132e1e10abe70`). The workflow verifies its digest, schema,
  pinned SHA, file count and command before reading the eight paths from it.
- The eight test files are unchanged from the original pilot. CI reference elapsed values are taken
  from production run `46067550544` at the pinned tree, not from a local run and not from
  a retry. All eight must have passed there; a censored or missing reference aborts analysis.
- Exact command for every invocation:

  ```
  racket scripts/run-tests/runner.rkt --jobs 4 --json-out cl5-p2f/run-<i>.json <the eight files>
  ```

- One warm-up (`i = 0`, never a sample), then five measured invocations (`i = 1..5`). Job timeout
  remains 120 minutes. No nested runners, no `./qt` shards, no test/timeout/selector changes, no
  in-job retries.

## 4. Host identity: measured, not assumed

Matching the `ubuntu-latest` label is necessary but **not sufficient**. The workflow writes the
observed environment before measurement and rejects any mismatch in the `Validate measured host`
step. Required values, all taken from the production observation:

| Field | Required value |
|---|---|
| OS release ID / version / pretty name | `ubuntu` / `26.04` / `Ubuntu 26.04.1 LTS` |
| Runner image | `ImageOS=ubuntu26`, `ImageVersion=20261122.9.1` |
| Architecture | `uname -m = x86_64` and `RUNNER_ARCH = X64` |
| CPU class | `model name` matches `AMD EPYC 9V…` (production 9V45; 9V74 observed in the old pilot is the same class) |
| Core allocation | `nproc = 4`; if `cpu.max` is present and not `max`, quota/period ≥ 4 |
| Memory | `/proc/meminfo` `MemTotal` in `[15 500 000, 17 500 000] kB` (production 16 366 632 kB) |
| Racket | version `8.10`, executable SHA-256 `7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c` |

The workflow records every field, plus kernel, runner name, load before/after, prepared-environment
state, cache state, setup duration, exact argv and wall time. A missing or malformed field is a
NO-GO. A host mismatch is a non-transient environment failure: stop and report; it does **not**
consume the repeat slot unless the owner authorizes a new configuration.

## 5. Acceptance criteria (unchanged thresholds, updated host basis)

All must pass, decided only from the uploaded artifacts.

- **A1 — host identity.** Every required field in §4 matches and is present.
- **A2 — integrity.** Warm-up plus five measured artifacts exist and parse; each measured artifact
  contains exactly the eight frozen paths; every status is `pass`; zero timeouts, failures or
  missing results. A behavioral failure stops the experiment and is never retried.
- **A3 — stability.** `(max total wall − min total wall) / median total wall ≤ 0.15` over the five
  measured runs. No extra runs after seeing data.
- **A4 — comparability.** For each file, `median(pilot elapsed) / CI reference elapsed ∈ [0.5, 2.0]`;
  pooled median ratio ∈ `[0.67, 1.5]`. A single file outside its interval is a NO-GO.
- **A5 — isolation.** One runner process per invocation, no shards or nested runner; the only
  `.github/` addition is this dispatch-only pilot workflow (no push/PR trigger, no test selection).
  The dispatch SHA, workflow SHA and pinned SHA are recorded as distinct fields.
- **A6 — provenance.** Cache state, preparation overhead, load, exact argv and host observations are
  recorded per run; warm-up excluded from all statistics.
- **Budget compliance** is an execution gate, not a new performance threshold (§6).

## 6. Dispatch budget

Budget ID `CL5-P2F-REV1`: one initial dispatch and at most one repeat. The workflow requires the
budget ID and an attempt number (`1` or `2`) as dispatch inputs; both are recorded in provenance.

A repeat is allowed **only** for a qualifying infrastructure failure before any `runner.rkt`
invocation: checkout failure, setup failure, network/cache failure, job timeout before measurement,
or a transient Actions infrastructure error, with the failing run ID and exact signature recorded.
A host mismatch is not treated as transient. A warm-up or measured behavioral failure — including
`malformed binding datum` — stops the experiment with no repeat. Both attempts remain in the
experimental record; the original exhausted P2-F budget remains separate and unchanged.

## 7. Analysis and reporting

Download the artifact, reduce to a new `cl5-p2f-pilot-rev1/1` record, and independently re-derive
all six criteria from the artifacts. The report must state: host and image actually observed, all
three SHAs, which production conditions were and were not reproduced, all attempts (including
failures), per-run and per-file timings, and any residual defect by its exact signature. No baseline,
selected-arm, speedup or 40 % claim.

Independent read-only review is required **before dispatch** of this pre-registration and **after**
the artifact reduction. Delegated reviews use `deepseek/deepseek-flash` with `thinking: high`.

## 8. Authorized and not authorized

Authorized: this pre-registration; the revised workload; the dispatch-only workflow amendment; one
bounded pilot under the budget above; artifact analysis; two independent reviews.

Not authorized: production test selection or CI policy changes; timeout changes; generic retries;
P2-E baseline; selected arm; P1; W3.3; production selection; any efficiency claim.

# CL5 Wave 2 — Option A pilot: D5 with exact-SHA CI ground truth (#9982)

Approved mechanism: a defect (or clean control) state is committed to a uniquely named scratch
branch, CI is triggered with `workflow_dispatch` on that branch, and the run is located **by head
SHA** — never by branch name, never through a pull request's synthetic merge commit.

Revision: `89dac02e17683f1f964b0499ddbff851fc5e2d64` (main after the W1 merge).
Case: **D5 / F-VS-1** — unknown wave status silently read as `pending` instead of raising
`exn:fail:wave-status`.
Host: 4-vCPU Linux container (driver), GitHub-hosted CI runners (ground truth).

## Result: `detected`

| State | Scratch branch | Exact head SHA | CI run | Verdict |
|---|---|---|---|---|
| defect (injection applied) | `cl5-w2/d5-defect-1795364830` | `48c08c1d950e846b54765288f9d5c81336a3843d` | 45043919793 | **fail** |
| control (clean revision) | `cl5-w2/d5-control-1795364830` | `cc6be2df8421e1f48a91fe6767476ad26d97e23f` | 45046666272 | **pass** |
| pair classification | — | — | — | **detected** |

Detecting jobs in the defect run (from the job/step payload, not from log grepping):

| Job | Failed step |
|---|---|
| `test (0)` | Run test shard 0/3 |
| `test (1)` | Run test shard 1/3 |
| `test (2)` | Run test shard 2/3 |
| `test-aggregate` | Gate on the required producers (fail closed on skipped too) |
| `lint-quality` | Run all lint checks |

The control run passed with no failed job, so the defect verdict is attributable to the injection
and not to the environment.

## Evidence rules applied (and tested)

- A run that is not `completed` is **unavailable** — never pass, never failure.
- `cancelled`, `timed_out`, `action_required` and `stale` runs are **unavailable**.
- A **skipped required producer** (`test (0..2)`, `test-aggregate`, `security`, `test-platform`,
  `smoke`, two `workflows` shards) makes the run **unavailable**, because the comprehensive surface
  did not execute. A skipped non-required job (e.g. `gsd-governance`, `release-readiness`) does not.
- A pair is `detected` only when the defect state fails **and** the control passes; a red control
  or any unavailable state yields `unavailable`.

Seven unit tests in `tests/test-cl5-ci-evidence.rkt` pin these rules; the pilot JSON is
`docs/reports/CL5-W2-PILOT-89dac02e.json`.

## What this pilot does and does not establish

**Does:** the CI-backed mechanism produces authoritative, exact-SHA, per-case evidence with the
detecting jobs named; a clean control pass separates the defect from the environment; scratch
branches are cleaned up after the verdict.

**Does not:**

- It is **not** a performance comparison. The selected-set timing must be measured on the same
  host class as the comparison it feeds; a local 25 s selected run is not comparable to a
  12–15 min CI run. The preregistered 40 % savings threshold needs a like-for-like execution
  surface, so the speed half stays open.
- It covers **one** corpus case. The registry currently implements D5 and F-PE-1; a GO decision
  needs the full executable corpus (D5, D6, D8, D9, D10, D11, D12 and the F-* injections where a
  fixture exists).
- It does not authorize any test-selection, scheduler or CI policy change.

## Cost (measured)

| Step | Wall time |
|---|---|
| push + dispatch (defect) | < 1 min |
| CI defect run (exact SHA) | ~14 min |
| push + dispatch (control) | < 1 min |
| CI control run (exact SHA) | ~12 min |
| **total pilot** | **~27 min** |

That is ~1/4 of a single local `./qt full` run on this host (≥ 90 min, timed out twice) and is
reproducible from the committed JSON.

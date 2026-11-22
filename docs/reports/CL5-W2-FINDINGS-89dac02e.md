# CL5 Wave 2 — first measurement attempt: findings and protocol amendment (#9982)

Revision measured: `89dac02e17683f1f964b0499ddbff851fc5e2d64` (main, after W1 merge).
Host: 4-vCPU Linux container, no CI runners attached.
Case attempted: **D5 / F-VS-1** (unknown wave status silently read as `pending`).

Status: **no valid measurement obtained.** The first attempt exposed two harness defects
(fixed below) and one environment limit that invalidates the per-case cost assumption in the
frozen protocol. No result from this attempt is used anywhere.

## 1. Harness defects found and fixed

| Defect | Effect | Fix |
|---|---|---|
| `./qt` was resolved against the *driver's* working directory | the selector ran the W2 branch's `qt` while measuring the trial worktree, so its own `scripts/cl5/*.rkt` triggered a `runner-helper-change` escalation and widened selection to 23 files | `resolve-command-path` resolves `./x` and `../x` inside the measured worktree; PATH lookup for bare names; absolute paths unchanged |
| a timed-out run was classified as evidence | `./qt full` hit the 5400 s timeout and was recorded as a failing comprehensive run, which would read as `missed` (a false green) | `classify-run` returns `unavailable` whenever either run timed out; timeouts can never become `detected`/`missed`/`true-negative` |

Both are covered by tests (`tests/test-cl5-selection.rkt`): timeout combinations, and relative
executable resolution against the worktree.

## 2. Environment limit: comprehensive ground truth does not fit the protocol's cost model

Measured on this host at `89dac02e`:

| Run | Observation |
|---|---|
| selector plan (`./qt quick --changed HEAD --explain`) | ~2 s |
| selected set (`./qt quick --changed HEAD`) | 25.2 s, 23 files (with the escalation defect above) |
| comprehensive (`./qt full`) | **did not finish within 5400 s**, twice (defect state and control) |

The corpus requires comprehensive verification as ground truth for every case (n = 1 per case
per selector version). At ≥ 90 min per run on this host, one case costs ≥ 3 h for defect plus
control; the 11 executable cases would cost ≈ 33 h and the fault-injection candidates more. That
is not a bounded wave, and it cannot be run in a single session.

The frozen CL0 report's local-lane figures (§9) were measured for a different purpose and do not
bound `./qt full` on a 4-vCPU host. This is now a measured, recorded discrepancy, not an
assumption.

## 3. Proposed amendment (requires owner approval before any case is measured)

The corpus's *rule* — comprehensive verification is ground truth, a selector that is never
compared against the full lane has produced no evidence — is not weakened by choosing a different
execution surface for the same lane. Three options, cheapest first:

| Option | Ground truth | Cost per case | Deviation to record |
|---|---|---|---|
| A. CI lanes | push the defect state to a scratch branch and read the CI shard verdicts (the same required contexts used for merges) | ~12–15 min of CI wall time | ground truth comes from CI, not a local `./qt full`; recorded per case with the run id |
| B. Affected lanes only | `./qt --lane fast` plus the owning shard on the local host | ~10–25 min | narrower than comprehensive; only acceptable if the corpus case names its lanes, and it must be labeled a partial comparison |
| C. Bounded local scope | 3 representative cases (D5, D8, D9) with `./qt full`, 4 h timeout, run unattended | ~8 h per case | fewer cases than the corpus defines; partial corpus coverage |

Recommendation: **A** for all executable cases (it uses the authoritative lane and the existing
required contexts, and produces exact-SHA evidence), with **C** only if CI capacity is unavailable.
Option B alone is not sufficient for the GO/REVISE/REJECT decision because it cannot show what the
selector missed.

## 4. What is already established

- The driver, the frozen case registry and the classifier are written, tested and committed; they
  run the shipped selector rather than reimplementing it.
- Applying and restoring a case is byte-identical, and the measured worktree is left clean.
- The admission-refresh step required after touching an admitted owner's closure is performed and
  its derived files are restored; this cost is recorded rather than hidden.
- The `./qt full` duration on this host (≥ 90 min, twice) is measured.

No W2 result exists yet, and W3 remains on hold.

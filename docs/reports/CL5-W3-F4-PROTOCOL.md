# CL5 W3 F4 — D5 replay under the contract-aware decision layer (pre-registered protocol)

Status: **protocol fixed before the replay was run.** This document states the inputs, the
expected decision, the evidence rules and the stopping conditions; the F4 record
(`CL5-W3-F4-REPLAY-89dac02e.json`) is produced from it and may not be adjusted afterwards to fit a
better result.

- Owner authorisation: GO for F4 only, as an evaluation-only, independently reviewed change.
- Non-goals: no production selector/CI/scheduler/admission change; no W3.2/W3.3; no edit of the
  frozen W3 protocol, corpus, W3.1 record or its CI cache.

## 1. Inputs (all pinned)

| Input | Value |
|---|---|
| Evaluated revision | `89dac02e17683f1f964b0499ddbff851fc5e2d64` (the W3.1 D5 revision) |
| Revision tree (control) | `aaf6097317a8489de7947b16f4f79b425c08547f` |
| Defect tree (reconstructed) | `24f225d459cf90bd24561ba4fcd76a243fa421a4` |
| Case | D5, changed file `extensions/gsd/wave-status.rkt` (frozen W3 corpus) |
| Deployed (historical) selector | `./qt quick --changed HEAD` on the defect tree |
| Contract layer | `c98997dd3860d2321287a101623736b50237ed51` (F3 merge), a **separately pinned** worktree |
| Contract module | `scripts/cl5/contract-escalation.rkt`, blob SHA recorded (expected to equal `4725b614`'s) |
| Comprehensive enumeration | the F2 producer registry at the evaluated revision (fast 0–2, platform, security, workflows 0–1, smoke = the declared command), not the 4725b614 union |
| Historical comprehensive run | `45043919793`, head `48c08c1d950e846b54765288f9d5c81336a3843d` (tree `24f225d4…`), conclusion failure |

The replay **refuses** to continue if the reconstructed revision or defect tree differs from the
expected SHAs: a reconstruction that is not bit-identical is not the historical case.

The 1 396-file union measured at `4725b614` is **not** used. The comprehensive set is re-enumerated at
`89dac02e` with the F2-corrected registry, because test sets are revision-specific and W3.1's smoke
enumeration overcounted (19 inferred files vs 1 declared command file).

## 2. Expected decision (hypothesis, not evidence)

Because the evaluated revision predates the contract layer, the decision is an **explicit
counterfactual**: layer code from `c98997dd`, data (graph, coverage manifest, changed files) from
`89dac02e`. The expected decision, derived from the F3 pinned result at `4725b614`, is:

- verdict `escalate`, instruction `run-comprehensive`;
- `required` contains the contract's own test `tests/test-gsd-wave-status.rkt`;
- `required ⊆ selected`, with `selected = required ∪ comprehensive` at `89dac02e`.

F4 must **demonstrate** this, not assume it. A different verdict (for example `covered`) is a
legitimate outcome and would be reported as such.

## 3. Evidence rules

1. **Decision evidence** — the recorded contract block: verdict, escalation codes, required set,
   selected set, instruction, layer/tree provenance and the counterfactual label.
2. **Historical detection evidence** — the failed per-file results in run `45043919793`'s own
   `test-results` artifacts (F1/F2 machinery), read only for the tree that run produced
   (`24f225d4…`). No inference from job names, and no reuse of a different run.
3. **Behavioural evidence tied to the tree** — the executed set is the **required files that the
   historical run itself failed** (`required ∩ failed-files(run)`), executed on the reconstructed
   defect tree and on the control tree in disposable worktrees, one file at a time, recording each
   file's exit status. A detector is *caused by the defect* iff it **fails on the defect tree and
   passes on the control tree**. Set-level verdicts alone are not used, because unrelated
   environment failures can appear in either arm.

   *Protocol amendment, recorded before any replay record existed.* The first attempt executed the
   whole required set (23 files) and exceeded the 50-minute time box on a hanging GSD test without
   producing a record. The set was therefore narrowed to the pre-registered rule above. The rule is
   chosen from **historical failure evidence**, not from any observed defect-tree result (no record
   existed when it was fixed), and it is re-derivable by the verifier. Executing the narrowed set is
   still sufficient: the escalated instruction runs `required ∪ comprehensive`, so a required file
   that fails on the defect tree makes that superset fail too.
4. Executing the required set is sufficient to speak about the escalated instruction: the
   instruction runs `required ∪ comprehensive`, so a required file that fails on the defect tree
   makes that superset fail as well. The full comprehensive set is not re-executed locally; its
   historical result is the frozen CI evidence above.

## 4. Claim matrix (fixed in advance)

| Condition | Claim |
|---|---|
| Trees not reproduced, layer unavailable/invalid, or decision not `escalate → run-comprehensive` with the contract test in `required` | **no claim** (`refused` / `no-escalation`) |
| Decision criteria hold, but no required file is *caused by the defect* (execution passes, is unavailable, or the same files fail on the control tree) | **decision-level success only** |
| Decision criteria hold and a required file *is* caused by the defect, but it is not among the historical run's failed files | **prevented — unattributed** (the detector is not the historical one) |
| Decision criteria hold, a required file is caused by the defect, and that file is among the historical run's failed files | **prevented — attributed** |

"Prevented" here means: the contract-aware instruction would have run a test that this exact tree
fails and the control tree passes, so the defect could not have been reported as a green run. It does
not generalise beyond D5, and it says nothing about cost, the 40 % hypothesis, or other defects.

## 5. Stopping conditions

The replay stops and reports failure (rather than retrying or substituting) if: either worktree is
dirty or at a different revision; either reconstructed tree differs from its expected SHA; the layer
worktree is dirty or not at the pinned revision; the contract module is modified relative to its
revision; or the historical run's artifacts are missing/expired. A timeout of a required test is
recorded as `unavailable` for that file and weakens the claim to decision-level unless another file
is attributed.

## 6. Reproduction

```
racket scripts/cl5/w3-dry-run.rkt --replay \
  --worktree ../wt-9982-f4-defect --control-worktree ../wt-9982-f4-control \
  --revision 89dac02e17683f1f964b0499ddbff851fc5e2d64 --case D5 \
  --contract-worktree ../wt-9982-f4-layer --contract-revision c98997dd3860d2321287a101623736b50237ed51 \
  --ci-cache docs/reports/CL5-W3-CI-CACHE-89dac02e.json \
  --expect-defect-tree 24f225d459cf90bd24561ba4fcd76a243fa421a4 \
  --expect-revision-tree aaf6097317a8489de7947b16f4f79b425c08547f \
  --repo-root .. --out docs/reports/CL5-W3-F4-REPLAY-89dac02e.json

racket scripts/cl5/w3-dry-run.rkt --verify-replay docs/reports/CL5-W3-F4-REPLAY-89dac02e.json
```

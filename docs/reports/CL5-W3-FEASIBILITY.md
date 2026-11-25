# CL5 W3.2-A — feasibility audit: the frozen corpus cannot support the W3a measurement

Status: **finding, awaiting independent review.** No trial was run, no case was classified, no
metric was produced. The frozen protocol and corpus were not edited. This document proposes a
correction; it does not apply one.

## What was asked and what the measurement needs

W3.2's task is to measure, on the frozen 928-case historical corpus, how often contract-aware
selection avoids comprehensive testing and what that saves (M3, M5, M6). Per the frozen protocol
(`docs/reports/CL5-W3-DESIGN.md`), each case needs, **at the case tree**:

1. the shipped selection engine — `./qt`, `scripts/testing/quick-audit.rkt`,
   `scripts/testing/full-plan.rkt` — to enumerate the selected set and its cost;
2. the coverage manifest (`tests/.coverage-manifest.json`) for the decision layer to be meaningful;
3. a completed, successful comprehensive CI run for the tree (population rule).

## Findings

### A1 — the engine does not exist at 922 of 928 case trees (99.4 %)

| Stratum | Cases | Version-bump-only | Engine present | Measurable (engine, non-bump) |
|---|---|---|---|---|
| S1 | 307 | 19 | 3 | 3 |
| S2 | 59 | 0 | **0** | **0** |
| S3 | 562 | 128 | 3 | 3 |
| **total** | **928** | **147** | **6** | **6** |

The selection engine was introduced by `707bf3bf` (2026-11-15, #9913). The corpus window is the
1 500 commits before `431b491d` (2026-11-23); 922 of its cases predate the engine. The protocol's
population rule only required a green comprehensive CI run, which says nothing about whether the
tools the measurement invokes exist at that tree.

All six engine-bearing cases have a successful push `CI` run for their head SHA (`2ca8d0bb`,
`5a6dd759`, `707bf3bf`, `ec88d463`, `f264b06d`, `fde734da`), so all six pass the protocol's
population rule. **Eligible cases: 6** — and the gate still fails on stratum coverage
(S2 = 0, S3 = 3).

*Correction during review:* an earlier draft of this report said 5 eligible because the CI lookup
fetched a single page of 10 runs; `707bf3bf` is a busy commit whose push `CI` run sat behind ten
`workflow_run` entries. The lookup now paginates (`all-runs`), the audit JSON is regenerated, and a
test pins the page-2 case. The population conclusion is unchanged.

### A2 — the engine-era history is 66 commits, so no re-frozen historical corpus can meet the minima either

`git rev-list --no-merges --count 707bf3bf..431b491d` = **60** inside the frozen window (67 to
`HEAD` when this report was last revised; the projection does not depend on the exact tip). At the frozen corpus's S2 share (59/928 ≈ 6 %), a re-freeze over
the whole engine era would yield roughly 3–4 S2 cases, against the pre-registered minimum of 20 per
stratum for S2 and S3. The pre-registered population minima therefore cannot be met by *any* slice
of this repository's history: the engine has not existed long enough.

### A3 — a matched-host comprehensive run does not complete in the time box

On this host, `./qt full` on an engine-bearing corpus case tree (`2ca8d0bb`, 1 679 test files) was killed
by a 3 600 s timeout (elapsed 3 733 s, `exit=124`, still inside `run-files`). The protocol is explicit:
if a matched-host `C_comp` cannot be completed, **M6 is `unavailable` and no GO is possible**, and
CI-derived timings must not be substituted. This is independent of population: the cost arm has no
working measurement surface on this host.

### A4 — the decision layer is also mostly unmeasurable on the frozen corpus

777 of 928 case trees have no `tests/.coverage-manifest.json`. The layer fails closed
(`attribution-unavailable` → escalate), so "escalation rate on history" would report an artefact of
missing metadata, not of contract coverage.

## Consequence under the frozen protocol

- W3a cannot be executed on the frozen corpus: no selected sets, no C_sel, no C_comp, no M3/M5/M6.
- The pilot cannot be multi-strata: S2 has **zero** measurable cases.
- No GO is possible (it never was from W3a alone, and M6 is unavailable besides).
- **No metric is reported here.** Reporting an escalation rate over a corpus where the engine is
  absent would be a fabricated number.

## Proposed correction (for review; not applied)

The corpus and protocol stay frozen. A protocol revision 3 is proposed, with these pre-registered
elements:

- **P1 — measure efficiency on the mutant population.** W3b's mutants are constructed on
  engine-era base revisions, where the engine, manifest and contract layer all exist by
  construction; the corpus is stratified and ≥ 60 cases. Run M3, M5 and (subject to P2) M6 on the
  same mutants that carry the safety measurement. Historical W3a is recorded as *not measurable* on
  this repository rather than dropped.
- **P2 — resolve the comprehensive-cost surface first.** M6 needs a matched-host `C_comp`. Either
  (i) a bounded official comprehensive definition executed locally under a declared budget, or
  (ii) a controlled-host measurement that is *not* a CI duration harvested from historical runs.
  Both are protocol changes with their own review; without one, M6 stays `unavailable` and the GO
  condition cannot be met regardless of population.
- **P3 — prospective corpus only if history is required.** A sound historical study would need a
  prospective window (record the next N merges with the engine present), which costs calendar time
  and must be pre-registered before the first merge is recorded.
- Any accepted correction re-runs this audit on the candidate population before freezing; the audit
  tool is deterministic and part of this change.

## What was not done

No trial, no case classification, no CI-duration substitution, no corpus or protocol edit, no
production selector/CI/scheduler/admission change, no safety or efficiency claim.

## Reproduction

```
racket scripts/cl5/w3-feasibility.rkt \
  --corpus docs/reports/CL5-W3-CORPUS-431b491d.json \
  --repo-root . --workspace .. --ci \
  --out docs/reports/CL5-W3-FEASIBILITY-431b491d.json

# A3 probe (destroys nothing outside the disposable worktree):
git worktree add --detach /tmp/wt-ccomp 2ca8d0bb19db34d6d7f632c2e10efc844ecb3b47
cd /tmp/wt-ccomp && time timeout 3600 ./qt full    # exit=124 after 3733 s
```

Machine-readable result: `docs/reports/CL5-W3-FEASIBILITY-431b491d.json`
(`population-feasible? = false`, reasons S2 = 0 and S3 = 3 against a minimum of 20; `ci-eligible = 6`).

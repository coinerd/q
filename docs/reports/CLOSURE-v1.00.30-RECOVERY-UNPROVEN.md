# CLOSURE — v1.00.30 Recovery Unproven (Closed Without Performance Verdict)

**Status:** CLOSED WITHOUT PERFORMANCE VERDICT — administrative closure only. Successor contract: `q/docs/reports/PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md` (co-located with this record in the delivery repository, so the wave has a real deliverable; the byte-identical staged source remains at `.planning/SUCCESSOR-PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md`).
**Administrative record:** audit commit `61b352a184a48186548aed5f18fd74104935bd84` ("docs(ci-recovery): record the v1.00.30 R2 cohort as COHORT OPEN", #9759).
**Delivery history (2026-09-28, recorded for audit):** the first attempt failed `git status` with `fatal: not a git repository`, and the second passed its declared verify gates while still failing delivery with `no wave target files changed` — both wave targets were then declared under `.planning/`, which sits OUTSIDE the `q/` delivery repository, so the coordinator's git step had nothing to stage. The wave was nevertheless advanced to `done` with an empty delivery branch and empty head SHA and a `delivery-pending` receipt; that false completion was reverted to `interrupted` and the wave re-scoped into `q/docs/reports/`. The successor contract was also written by a coordinator-side shell `cp` that bypassed the structured write hooks for `.planning/PLAN-*`; the copy was byte-identical to its staged source, and the bypass is recorded as a guard defect, not endorsed.
**Frozen predecessors (untouched by this record):** `.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md`, `.planning/STATE-v1.00.30-PR-CI-RECOVERY.md`, `.planning/VALIDATION-v1.00.30-PR-CI-RECOVERY.md`, `.planning/V1.00.30-W5-BLOCK.md`, and the evidence bindings `q/docs/reports/gsd-wave-validation/v1.00.30-w0.rktd` … `v1.00.30-w4.rktd`.

## Verdict

**v1.00.30 is CLOSED WITHOUT PERFORMANCE VERDICT — RECOVERY UNPROVEN. Not an R2 pass; not an empirically measured R2 failure.**

Phrasing note: an earlier revision of W0's verify gates forbade the literal substring `R2 pass` while simultaneously mandating a verdict sentence that contains it, so the two gates were mutually exclusive and a prior attempt reworded this verdict to dodge them. The gates have since been corrected on two counts. First, the verdict is back to the mandated wording. Second, the negative check no longer relies on `grep -L`, which was measured on this host (GNU grep 3.11) to exit 0 whether or not the file matches, and so could never fail; it is now an inverted `grep -q` inside a `bash -c` span, which is a real gate. The meaning here is unchanged and remains exact: no R2 result of any kind was computed for v1.00.30, in either direction.

## Diagnostic evidence — an alarm, not a verdict

Four required PR-CI runs, all on the contained global root-off topology:

| Workflow run | Wall time (s) | Slowest `fast-test` step (s) | Root state |
|---|---|---|---|
| 36089836057 | 1174 | 541 | global-off |
| 36301381207 | 1237 | 563 | global-off |
| 36340329485 | 1225 | 564 | global-off |
| 36353531203 | 1191 | 559 | global-off |

Cohort statistics: p50 1208.0 s, p95 1235.2 s, mean 1206.75 s. The p50 sits 387.5 s (47.2%) above the 820.5 s reference. In every run the `test-aggregate` workflow finished last (273–291 s), including 252–271 s inside its own cold `setup-racket`. The containment fallback bounded the eager gate at 63.712–75.680 s after purging 2,393–2,394 bytecode files.

**Classification: diagnostic alarm, not a verdict.** Four diagnostic heads are neither the >=20-head closed cohort the frozen acceptance contract requires nor a controlled benchmark. No success or failure of R2 is computable from this data; R2 was correctly not computed, and W5 recorded the cohort as COHORT OPEN.

## Four-claim ladder

safety delivered ≠ root activated ≠ root measured ≠ organic-PR evidence ≠ performance success

1. **Safety delivered** — yes: the W0–W4 infrastructure below is real and verified.
2. **Root activated** — no: every diagnostic run above is global root-off with eager fallback.
3. **Root measured** — no: an unactivated root has no A/B/C benchmark.
4. **Organic-PR evidence** — no: four diagnostic heads only; the cohort is still COHORT OPEN at audit commit `61b352a184a48186548aed5f18fd74104935bd84`.
5. **Performance success** — therefore unclaimable, and equally unrefuted. The 47.2% p50 gap is an alarm about the contained topology, not a measured verdict about the root.

## Delivered-but-reclassified inventory

What v1.00.30 actually delivered, reclassified from the recovery narrative to infrastructure status:

| Wave | What was delivered | Reclassification |
|---|---|---|
| W0 | Exit-truth repair (BUG-0073): the aggregate verifies shard records, run-SHA binding, result consistency | Truthfulness foundation; not a performance result |
| W1 | Guard evaluator | An evaluator, not a deployed guard |
| W2 | Safe containment: global root-off / eager fallback | Containment only; fallback bounded the eager gate at 63.712–75.680 s after purging 2,393–2,394 bytecode files |
| W3 | Trusted-root prototype: immutable external compiled root + single eager fallback | Prototype; never activated |
| W4 | Guarded producer/consumer + local drill | Activation machinery, drilled locally only |
| W5 | Honest four-head observation: COHORT OPEN, R2 correctly not computed | Observation, not a cohort result |
| W6–W8 | Never ran | No v1.00.30 release exists |

## Administrative closure is not acceptance evidence

Closing the v1.00.30 milestone and its issues is administrative state — recorded at audit commit `61b352a184a48186548aed5f18fd74104935bd84` — not acceptance-criteria evidence. It computes no verdict, supersedes no frozen artifact, and reopens nothing. The frozen acceptance contract in `PLAN-v1.00.30-PR-CI-RECOVERY.md` remains the historical definition of what R2/R3/R4 would have required; none of those gates was ever evaluated against a closed cohort, and this record reopens none of them.

**Successor:** `q/docs/reports/PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md` (co-located with this record) — measured recovery through ordered gates, with no auto-authorization of root-on, release, or a live cohort.

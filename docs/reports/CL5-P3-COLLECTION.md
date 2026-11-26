# CL5 P3 — prospective collection: first collection run

Status: **collection started (owner-authorised), no analysis performed.** The population is nowhere
near the pre-registered minimum, and nothing here is evidence of efficiency. Cohort `P` is empty by
construction: no landed change exists after `P3_START_SHA` yet.

Pre-registration: `CL5-P3-PREREG.md` (rev 2, with the two pre-collection amendments: the observation
unit is a *landed change* because this repository squash-merges, and E7 requires the change to be a
corpus-classifier case).

## What was collected

| Cohort | Window | Landed changes | Eligible | Unavailable | S1 | S2 | S3 |
|---|---|---|---|---|---|---|---|
| **H** (engine-era historical) | `707bf3bf..d67edc7c` | 68 | **5** | 63 | 3 | **0** | 2 |
| **P** (prospective) | `d67edc7c..main` | 0 | 0 | 0 | 0 | 0 | 0 |
| total | | 68 | 5 | 63 | 3 | 0 | 2 |

Exclusions, with their reasons (never dropped):

| Reason | Count | Meaning |
|---|---|---|
| E7 | 50 | the corpus classifier excludes the change: it touches no production module (`scripts/`, `tests/`, `docs/` are outside the production universe), so it has no stratum |
| E5 | 13 | no successful comprehensive CI run for that landed change's own SHA with every required producer green |
| — | 5 | eligible |

E5 is not noise: several engine-era commits have no `CI` push run (they were branch-only or their run
was cancelled), and the F1 rule forbids reading an infrastructure failure as a detection. Recording
them as `unavailable` is the fail-closed outcome the pre-registration requires.

## The five eligible cases (cohort H)

| Landed change | Stratum | Changed files | Unique files in surface | Producer slots |
|---|---|---|---|---|
| `ec88d463` | S3 | 14 | 1 385 | 1 480 |
| `f264b06d` | S3 | 12 | 1 378 | 1 473 |
| `5a6dd759` | S1 | 4 | 1 378 | 1 473 |
| `2ca8d0bb` | S1 | 10 | 1 377 | 1 472 |
| `fde734da` | S1 | 15 | 1 372 | 1 466 |

Their surface shapes (1 372–1 385 unique files) are consistent with the 1 397-file surface measured at
`dad0a9b9`, and every one was computed from the case tree's own lane plans, not from today's.

## Why this is not a population yet

The pre-registered minimum is **20 per stratum**. Cohort H supplies 3 / 0 / 2, cohort P supplies
nothing. Under §6 of the pre-registration these cases authorise no analysis, no metric and no GO, and
the S2 gap cannot be closed from history: the frozen corpus showed S2 is rare (59 of 928 cases had
engine-era ancestry at all) and the engine-era window is 68 landed changes.

This is the expected starting state, not a failure: the point of P3 is that the population accumulates
*prospectively*, where every landed change has the engine, the manifest and a CI run by construction.

## Collection cost and mechanics (for the record)

- 68 landed changes took ~2 h 45 m wall on this host. Per-case cost is dominated by the ~10
  engine-bearing changes (7 `--explain` invocations each, plus one classifier run with the case tree
  as root); the other ~58 cost seconds each (git probes only).
- Each case is appended to `CL5-P3-CASES.jsonl` **as it is collected**, so an interrupted run resumes
  from the file and never rewrites a line (`append-cases!` refuses an existing `landed_sha`).
- No test was executed during collection: `--explain` only. No CI was triggered, no workflow changed.

## Next collection steps (mechanical only)

1. Re-run cohort P after merges land: `racket scripts/cl5/p3-collect.rkt --from d67edc7c --to main
   --cohort P --workspace .. --out docs/reports/CL5-P3-CASES.jsonl`.
2. Do **not** re-check an `unavailable` case: an E5 case whose CI run appears later stays recorded as
   E5-unavailable, because a re-run appends nothing for a sha already present. Relaxing that would be a
   pre-registration revision (and would invite post-hoc selection), not a collection convenience.
3. Nothing else. Collection stays a data-gathering step until the minimums are met.

## Reproduction

```bash
# cohort H (engine-era historical)
racket scripts/cl5/p3-collect.rkt --from 707bf3bf --to d67edc7c --cohort H \
  --workspace .. --out docs/reports/CL5-P3-CASES.jsonl
# cohort P (prospective; empty until merges land)
racket scripts/cl5/p3-collect.rkt --from d67edc7c --to main --cohort P \
  --workspace .. --out docs/reports/CL5-P3-CASES.jsonl
```

Both are idempotent: re-running appends only shas not already in the file.

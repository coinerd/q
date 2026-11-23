# CL5 W3.0 — historical corpus: extraction and frozen manifest (#9982)

Status: **W3.0 complete, drafted for review.** The corpus is frozen in
`CL5-W3-CORPUS-431b491d.json`; no trial has been run.

Protocol: `docs/reports/CL5-W3-DESIGN.md` (merged `431b491d`).
Extractor: `scripts/cl5/w3-corpus.rkt` (tests: `tests/test-cl5-w3-corpus.rkt`, 16 green).

## Gate status

| Gate (protocol W3.0) | Requirement | Result |
|---|---|---|
| manifest committed before the first trial | required | yes, this PR |
| keyed by full commit SHA, tree SHA resolved at trial time | required | yes |
| exclusions listed and counted | required | yes |
| S2 ≥ 20 cases | required | **59** |
| S3 ≥ 20 cases | required | **562** |

## Window and revision

| Field | Value |
|---|---|
| revision | `431b491d0485…` (`origin/main` at extraction) |
| window | `git log -n1500` from that revision |
| scanned | 1 500 commits |
| cases | **928** (S1 307, S2 59, S3 562) |
| excluded | 570 non-production, 2 revert-commit |
| coverage manifest | `loaded` (170 declared-covered modules of 813 production modules) |

The window is a contiguous slice of `main` history, not a sample: 1 500 is the
pre-registered bound that first satisfies the S2/S3 minimums, and the manifest
records the bound so the selection is reproducible.

## Exclusions (counted, not dropped)

| Reason | Count | Rule |
|---|---|---|
| `non-production` | 570 | no changed file in the production universe (docs, tests, artifacts, metadata, CI config) |
| `revert-commit` | 2 | subject is a revert |
| `merge-commit` | 0 | in this window `git log` linearises merges; the rule stays in force |
| `formatting-only` | 0 | no whitespace/binary-only production changes in this window |

## Stratification

Cone definition used by the extractor (recorded in the manifest as
`cone-definition`):

1. the module's declared owner detector, when the coverage manifest declares one; else
2. the highest-fan-in transitive dependency with at least 3 consumers (the shared
   contract it consumes); else
3. the module itself.

S1 = one production module; S2 = ≥ 2 modules sharing one cone; S3 = ≥ 2 cones.
S2 is therefore graph-derived rather than manifest-derived, so it does not depend
on the coverage manifest being complete — which matters because only 170 of 813
production modules are declared-covered.

## What this corpus can and cannot show

- **Can**: escalation rate, escalation reasons, selection size, and the cost model
  (M3, M4a, M4b, M5, M6) on real change sets, per stratum.
- **Cannot**: any safety statement. Every case's defect tree is a commit tree that
  already passed comprehensive CI, so it cannot produce a `detected` case except
  through flakiness (quarantined as `unavailable`). Safety evidence is the mutant
  corpus (W3.3).

This is the protocol's design decision, not a limitation discovered late; the
extractor's header states it so no later reading can treat W3a as a safety result.

## Next gates

| Wave | Gate |
|---|---|
| W3.1 | one dry case end-to-end on a scratch branch: provenance and classification must match the D5 behaviour from W2 |
| W3.2 | S2 then S3 trials; every case classified, none silently dropped |
| W3.3 | mutant manifest freeze: ≥ 60 verified comprehensive-failing mutants, ≥ 20 per stratum, ≥ 12 per synthetic class, mixed-hunk rule stated |

No production selection, scheduler, admission or CI-policy change is part of
any of these waves.

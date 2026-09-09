# TEST-CONSOLIDATION v1.00.28 — W5: Semantic redundancy and shared-stimulus consolidation

## Scope

Family: gsd-delivery-verifier (`extensions/gsd/delivery-verifier.rkt`), covered by
`tests/test-gsd-delivery-verifier.rkt` (24 tests) and the adjacent adapter seam
`tests/test-gsd-github-port.rkt` (19 tests, covers `extensions/gsd/github-port.rkt`).

## Candidate detection

Candidate group **G1** was recorded from the W0 census as a family pair (the github port is
the delivery-verification side-effect seam of the verifier) and written to
`artifacts/test-consolidation/v1.00.28-w5/candidates.json` with evidence links **before** any
change was made. The static detector (`scripts/run-tests/redundancy-candidates.rkt`, re-run
during review) reports **no** same-covers group and no shared-fixture group joining the two
suites: they carry distinct `@covers` keys and distinct stimuli (fake delivery-verification
child process with exit status + branch state vs fake github adapter). The candidate was
evaluated and resolved to keep-both rather than merged.

## Policy decision (preference order applied)

1. Share setup without merging tests — not applicable across the pair: each suite pins its own
   module-local stimulus (a shared fixture builder would couple the unit suite to the port
   adapter); within each suite setup is already module-local.
2. Merge complementary assertions with same stimulus — not applicable: the unit suite asserts
   sync gate decisions over a fake child process; the port suite asserts the adapter seam over
   a fake github adapter. Stimulus is adjacent, not identical (different module boundaries,
   different `@covers` keys).
3. **Keep both when detection power is ambiguous** — APPLIED. Merging would reduce the
   independent detection of exit-status forgery (unit) vs wire-format drift (port).
4. Delete with adequacy evidence — not applicable.

Result: no test deleted, no test merged. Counts unchanged: before 43, after 43
(24 + 19 test cases).

## Adequacy evidence

`artifacts/test-consolidation/v1.00.28-w5/adequacy.json`
(schema `test-consolidation-adequacy/1`), validated fail-closed by
`racket scripts/run-tests/mutation-pilot.rkt --check` → **OK, evidence complete**.

Manual micro-mutations on `extensions/gsd/delivery-verifier.rkt` (all killed by the
consolidated suite; no previously killed mutant survives):

| id | mutation | before | after |
|----|----------|--------|-------|
| M1 | branch gate `(string=? branch expected)` → `(string=? branch branch)` | killed | killed (2 failures) |
| M2 | sync approval gate `(eqv? exit-code 0)` → `(eqv? exit-code 7)` | killed | killed (14 failures) |
| M2-async | async wait path gate `(equal? exit-code 0)` → `(equal? exit-code 7)` | not covered by this suite | surviving (out of scope; no test moved) |

## Failure-history review

`ANALYSIS-v1.00.04-W0-DELIVERY-VERIFIER-COMMITTED-FALSE-NEGATIVE.md` reviewed: the family
exists precisely because a committed false negative slipped through; both candidate suites
guard that behavior. No consolidation was applied that could regress it.

## Checksums

`artifacts/test-consolidation/v1.00.28-w5/SHA256SUMS` pins this report, `candidates.json`,
and `adequacy.json`.

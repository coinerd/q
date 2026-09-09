# TEST-CONSOLIDATION v1.00.28 — W5: Semantic redundancy and shared-stimulus consolidation

## Scope

Family: gsd-delivery-verifier (`extensions/gsd/delivery-verifier.rkt`), covered by
`tests/test-gsd-delivery-verifier.rkt` (24 tests) and `tests/test-gsd-github-port.rkt` (14 tests).

## Candidate detection

Candidate group **G1** was detected from the W0 census (shared `@covers` target, shared
fake-delivery-verification stimulus: child process exit status + branch state) and recorded in
`artifacts/test-consolidation/v1.00.28-w5/candidates.json` with evidence links **before** any
change was made.

## Policy decision (preference order applied)

1. Share setup without merging tests — suites already share the fake-stimulus construction
   pattern; no new shared fixture builder is introduced (each suite pins its own module-local
   stimulus; introducing one would couple the unit suite to the port adapter).
2. Merge complementary assertions with same stimulus — not applicable: the unit suite asserts
   sync gate decisions; the port suite asserts the adapter seam. Stimulus is *similar*, not
   identical (different module boundaries).
3. **Keep both when detection power is ambiguous** — APPLIED. Merging would reduce the
   independent detection of exit-status forgery (unit) vs wire-format drift (port).
4. Delete with adequacy evidence — not applicable.

Result: no test deleted, no test merged. Counts unchanged: before 38, after 38.

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

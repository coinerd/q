# Test Consolidation Policy

Status: **Active** (introduced by W9 — Adequacy Pilot & Consolidation Policy)
Owner: q maintainers
Related tooling: `scripts/run-tests/mutation-pilot.rkt` (bounded adequacy pilot)
Related workflow: `.github/workflows/full-regression.yml` (`mutation-pilot` opt-in scheduled job)
Governance: canonical test strategy is documented in
[`docs/TDD-TEST-STRATEGY-PLAN.md`](../TDD-TEST-STRATEGY-PLAN.md) (adopted in v1.00.04;
supersedes PR #9348) — Phase 5 defines the adequacy evidence this
policy requires before any consolidation.

## Purpose

Test consolidation (merging or removing tests) trades detection power for maintainability.
This document is the contract that makes that trade explicit and reviewable. It exists so
that a consolidation PR can never be justified by line/branch coverage equivalence alone:
coverage measures *execution*, not *assertion*. Mutation-style adequacy evidence measures
whether the suite still *fails* when behaviour changes.

## Hard rules

1. **No test may be deleted or merged without an explicit consolidation rationale that
   cites this policy.** A coverage-only justification ("both tests execute the same
   lines / branches") is explicitly insufficient and must be rejected in review.
2. **Behaviour-named tests are the last candidates.** A test whose name encodes a
   behavioural contract (e.g. `retry-stops-after-max-attempts`, `timeout-is-never-a-pass`)
   is presumed to carry detection power beyond its assertions' surface; it may only be
   consolidated when the surviving test provably fails under every mutant the deleted
   test killed.
3. **Zero silent drops.** If a consolidation PR changes the number of tests, the PR
   description must contain a before/after count and a per-consolidated-test mapping to
   the surviving assertion location.

## When a test is a consolidation candidate

All four conditions must hold, with links in the PR:

1. **Duplication**: two or more tests duplicate a behavioural contract (same stimulus,
   same expected observable outcome), not merely the same code path.
2. **Scope**: the tests declare identical explicit `@covers` metadata (see
   `docs/TEST_CONVENTIONS.md` and the W1 metadata schema). Tests with differing or
   absent `@covers` scope are out of scope for consolidation until metadata is fixed.
3. **History**: neither test has a distinct failure history — check the quarantine
   ledger and triage records (`docs/operations/test-regression-triage.md`); a test that
   has uniquely caught a regression in the last 6 months is not a duplicate.
4. **Readability & owner approval**: the consolidated test must be *easier* to read than
   the pair it replaces (shared setup, complementary assertions), and a module owner
   must approve the readability trade-off in the PR review.

## Evidence required in the consolidation PR

| Evidence | How produced | Pass criterion |
|---|---|---|
| Preserved-behaviours doc | Manual section in the PR description listing each behaviour covered before and the surviving assertion that covers it after | Every prior behaviour has a named surviving assertion |
| Green suites | Full pipeline: L1 (impact-selected unit-fast), L2 (unit-all), L3 (shards), L4 (full regression workflow) | All green; no quarantine additions |
| Adequacy evidence | `racket scripts/run-tests/mutation-pilot.rkt --modules <module(s) under the consolidated tests> --budget <seconds>` run before and after the consolidation | **No mutant that was killed before survives after** (detection score may not decrease for the scope) |

If mutation tooling cannot run for the scope (e.g. non-pure module, budget exceeded),
equivalent adequacy evidence is required: a documented reviewed micro-mutation review
where a reviewer manually injects the operator-swap / boundary / boolean mutations listed
in the pilot's mutation set and confirms the suite fails. "Cannot run" is never a reason
to skip the evidence — it is a reason to not consolidate.

## Preference order for consolidation

When duplicates are found, prefer in this order:

1. **Consolidate shared setup** (fixtures, server bootstrap, temp trees) into helpers —
   zero assertion risk.
2. **Merge complementary assertions** (same stimulus, different aspects) into one test
   with labelled assertion phases.
3. **Keep both, add cross-reference** when detection power is ambiguous.
4. **Delete a duplicate test** only with full evidence per the table above.

## Bounded adequacy pilot (the evidence generator)

`scripts/run-tests/mutation-pilot.rkt` applies a reviewed set of manual micro-mutations
(operator swaps, boundary off-by-one, boolean flips, removed checks) to an explicitly
nominated small set of pure, high-value modules and runs the impact-selected tests
against each mutant under a hard wall-clock and mutant-count budget. It aborts and
reports partial results rather than exceeding budget; timeouts are never passes.
Runs are opt-in (scheduled weekly, low priority) via the `mutation-pilot` job in
`.github/workflows/full-regression.yml`; it never blocks PRs or releases.

### Tool decision record

No free/open-source, Racket-compatible mutation tool met the maintenance and
compatibility criteria for the supported Racket version; the pilot therefore implements
reviewed manual micro-mutations applied via the existing racket-edit tooling, as
sanctioned by the plan constraint. Re-evaluate this decision when a candidate tool
appears (record date and criteria).

### Baseline findings (first pilot run, 2026-08-17)

- Scope: `runtime/memory/policy.rkt` (nominated pure, high-value module from the L1
  inventory), L1/L2 test selection via the impact tooling.
- Result: planned 18 mutants, **killed 4, survived 14, detection score 4/18 (0.222)**.
- Full artifact: `artifacts/mutation-pilot/run-5/findings.md` (JSON:
  `artifacts/mutation-pilot/run-5/mutation-pilot.json`).
- Interpretation: a genuine adequacy gap — boundary/off-by-one and boolean-flip
  micro-mutations in the policy module survive the current suite. These survivors are
  the actionable gap list: tests asserting exact boundary values (`<=` vs `<`) and
  boolean policy branches are the highest-value additions before any consolidation in
  this scope is even considered.
- Consequence under this policy: **no consolidation of tests covering
  `runtime/memory/policy.rkt` is approvable** until the detection score for that scope
  improves to the agreed bar (target: no boundary/boolean survivor class).

## Review checklist (copy into the consolidation PR)

- [ ] Candidate tests listed with `@covers` scope (identical)
- [ ] Failure history checked (quarantine ledger + triage records)
- [ ] Preserved-behaviours table present
- [ ] L1/L2/L3/L4 green (links)
- [ ] Mutation pilot (or equivalent manual adequacy review) run before AND after;
      detection score not decreased; no previously-killed mutant survives
- [ ] Owner approval of readability trade-off
- [ ] Before/after test counts match the mapping

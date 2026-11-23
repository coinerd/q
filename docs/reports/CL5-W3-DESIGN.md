# CL5 Wave 3 — measurement design: does contract-aware selection protect regressions? (#9982)

Status: **design only**. No trials have been run, no production selection/CI/admission/scheduler
behaviour is changed by this wave, and the 40 % claim remains a hypothesis.

Review gate: this design must be reviewed before any execution (W3.2 onwards).

## Why W3 exists

W2 measured one case (D5) end-to-end and the answer was `missed`: the shipped selector passed a tree
that comprehensive CI failed. The follow-up work added a fail-closed decision layer
(`scripts/cl5/contract-escalation.rkt`, merged `4725b614`) and a guard
(`scripts/cl5/selection-guard.rkt`), but neither proves anything about **real change sets**. Both
prove structural properties: consumers accounted for, detectors declared and present, metadata
parseable, instructions not reduced. Whether the selected tests actually *detect* the regressions
comprehensive testing detects is empirical.

W3 is that measurement. It answers two questions and nothing else:

| Id | Question | Formal target |
|---|---|---|
| **Q-S** (safety) | Would the selected tests have detected the regressions comprehensive testing detected? | `missed = 0` over the corpus; any miss is a REJECT for that configuration |
| **Q-E** (efficiency) | How much testing could realistically be avoided, including the cost of unnecessary escalations? | selected/comprehensive cost ratio on a matched host, escalation cost included |

## Corpus frame (pre-registered before any trial)

**Population.** Commits on `main` history that were CI-validated (the commit's own tree has a
completed, successful comprehensive run) and that change at least one module in the production
universe (`scripts/cl5/ownership-audit.rkt` production set).

**Stratification.** W3 requires a *multi-component* corpus, because D5 was single-module and that is
where structural reasoning is easiest:

| Stratum | Inclusion | Minimum cases |
|---|---|---|
| S1 single-module | exactly one production module, one contract cone | 0 (W2 covers it) |
| S2 multi-module | ≥ 2 production modules in the same contract cone | ≥ 20 |
| S3 cross-contract | ≥ 2 production modules spanning ≥ 2 contract cones, or contract + non-contract | ≥ 20 |

**Exclusions** (recorded, not silently dropped): merge commits, revert commits, docs-only,
metadata-only (derived artifacts, manifests, metrics), formatting-only diffs, commits whose parent
tree has no successful comprehensive run (baseline unknown ⇒ case is `unavailable`, not a pass), and
commits whose diff touches only test files.

**No post-hoc selection.** The case list is materialised as a JSON manifest, committed before the
first trial, and every subsequent run reports against that manifest. Adding a case requires a new
manifest revision and is disclosed in the report.

## Case construction

For a corpus commit `C` with parent `P`:

| Surface | Tree | Meaning |
|---|---|---|
| defect | tree of `C` (parent + `C`'s diff) | the historical change, replayed as the "injected defect" |
| control | tree of `P` | must pass comprehensively; a control that fails makes the case `unavailable` |

Both surfaces are keyed by **tree SHA**, never by commit or PR merge commit. The local selected run
executes against the defect tree before any commit (W2 driver behaviour: staged refresh, measured on
an isolated tree, provenance assertion, cleanup in `dynamic-wind`). The comprehensive run is located
by exact head SHA via `workflow_dispatch` on a uniquely named scratch branch (Option A), classified
from job/step payloads, not logs.

Detection requires **both** surfaces to fail on the defect tree and **both** controls to pass:

- comprehensive passes defect → not a regression; case does not count (and is reported as such);
- comprehensive fails, selected passes → **`missed`** (Q-S failure);
- selected fails, comprehensive passes → `unavailable` (inconsistent surfaces; investigate, do not
  score as a win);
- any incomplete/cancelled/timeout/missing required producer → `unavailable`, never a pass.

## Fail-closed evidence rules (inherited, unchanged)

- Unavailable ≠ PASS. Cancelled, skipped, timed-out, action-required, incomplete, or a missing
  required producer is `unavailable`.
- A skipped required producer makes the run `unavailable`.
- Provenance: the committed tree must change exactly the registered mutation; otherwise the driver
  raises before dispatch and no evidence is recorded.
- Timeouts are never detection evidence.
- Append-only checkpoint records per case; the report is generated from records, never hand-edited.

## Metrics (pre-registered; reported with counts and intervals)

| Id | Metric | Definition |
|---|---|---|
| M1 | detection parity | cases where selected failed among comprehensive-detected cases |
| M2 | misses | `comprehensive-detected ∧ selected-passed`, with the exact test list comprehensive failed |
| M3 | escalation rate | cases where the decision layer escalates (`run-comprehensive`/`run-all`) |
| M4 | unnecessary escalation | escalated cases whose selected set would also have detected the regression |
| M5 | selection size | selected test files executed / comprehensive test files, per case |
| M6 | cost ratio | wall-clock selected / comprehensive **on the same host**, with escalation cost amortised by M3 |
| M7 | decision-layer agreement | does `contract-escalation` escalate exactly the cases where the selector misses? (the value of the decision layer as a filter) |

M6 is the only metric that may speak to the 40 % threshold, and only on a matched host. Local ~25 s
versus CI 12–15 min is **not** a cost measurement and must never be reported as one.

## Statistical discipline

- Pre-register N. With `missed = 0` in N detected cases, the rule-of-three upper 95 % bound is
  ≈ `3/N`; the report must state it. 0/20 ⇒ ≤ 14 %; 0/60 ⇒ ≤ 5 %. A zero-miss claim from a small
  corpus is a bound, not proof of parity.
- Cases are not independent samples of "the distribution of regressions"; they are a purposive
  historical corpus. Report counts, not p-values, and never extrapolate a rate to all changes.
- Any change to the corpus, the metrics, or the decision thresholds after the first trial invalidates
  the comparison and is disclosed in the report.

## Decision rule (pre-registered, same three outcomes as W2)

| Outcome | Conditions |
|---|---|
| **GO** (advisory only) | M2 = 0 on S2 ∪ S3 with N ≥ 60 detected cases (upper bound ≤ 5 %), and M6 ≤ 0.60 on a matched host including escalation cost |
| **REVISE** | M2 = 0 but M6 > 0.60, or escalation cost (M3 × comprehensive) eats the saving |
| **REJECT** | any M2 > 0, or any case where the decision layer reported `covered` while comprehensive failed |

GO never enables production selection by itself: it authorises an advisory rollout proposal, which
needs its own review. No CI policy change is ever in scope for W3.

## W3b — mutation complement (why history alone is insufficient)

Historical changes under-sample regressions: most merged commits were correct, so comprehensive may
pass on many of them and there is nothing to detect. W3b therefore adds deliberate mutants:

- **Target set**: contract modules and their declared consumer cones (the D5 vocabulary, `errors.rkt`
  after it is fully declared, and the two eligible ownership-repair modules).
- **Mutants**: small, behaviour-changing edits (guard inversion, boundary shift, error-path removal,
  vocabulary keyword change) applied one at a time to a pinned revision.
- **Requirement**: for every mutant where comprehensive fails, the selected set must also fail; the
  `missed` mutants are listed with the detector that *should* have caught them.
- **Value**: this is the only measurement that speaks to *behavioural* completeness of a contract's
  detector set, which the structural validator explicitly does not claim.

W3b runs only after W3a (historical) is reported, and reuses the same evidence machinery.

## Reuse (no new framework)

| Need | Existing component |
|---|---|
| case execution, staging, provenance | `scripts/cl5/selection-experiment.rkt` (W2 driver) |
| comprehensive ground truth | `scripts/cl5/ci-evidence.rkt` (Option A, exact SHA) |
| grading | `scripts/cl5/oracle.rkt`, `task-spec.rkt` |
| decision-layer agreement (M7) | `scripts/cl5/contract-escalation.rkt` (evaluation only, not wired) |
| audit/guard context | `scripts/cl5/ownership-audit.rkt`, `selection-guard.rkt` |

No test-runner, scheduler, admission, or CI-policy change is part of W3. If a metric needs a new
execution surface, that is a separate issue with its own review.

## Threats to validity (must appear in the final report)

- Flakiness and nondeterminism inflate "detected"; every comprehensive-fail must be attributable to a
  named job, and repeated failures are recorded.
- Non-hermetic tests (network, time, environment) can fail either surface; such cases are
  `unavailable`, not evidence.
- History bias: merged commits passed CI, so the corpus skews to non-regressions; that is why W3b
  exists.
- Cache and warm-start effects on M6; only same-host, same-machine comparisons count.
- Escalation cost is real: a high M3 can erase M5's saving, which is why M6 includes it.
- CI capacity/billing can make the corpus partly `unavailable`; unavailable cases are reported, and a
  corpus whose detected cases fall below the pre-registered N yields no decision.

## Artifacts

- `docs/reports/CL5-W3-CORPUS-<manifest-sha>.json` — frozen corpus manifest (pre-registered).
- `docs/reports/CL5-W3-VALIDATION-<sha>.md` and `.json` — final report, generated from per-case
  records; one record per case including surface verdicts, tree SHAs, run ids, and classification.
- Per-case checkpoints are append-only and never rewritten.

## Wave plan (execution gated on this design's review)

| Wave | Work | Gate |
|---|---|---|
| W3.0 | corpus extraction + manifest freeze (S2/S3 ≥ 40 cases) | manifest reviewed; exclusions listed |
| W3.1 | single dry case end-to-end on a scratch branch | provenance + classification match W2's D5 behaviour |
| W3.2 | S2 trials | all cases classified; no `unavailable` silently dropped |
| W3.3 | S3 trials | same |
| W3.4 | M7 decision-layer agreement + report | M1–M7 reported with intervals; decision stated |
| W3b | mutation complement | mutant detection table |

## Non-goals (explicit)

- No production test selection, no `./qt` behaviour change, no CI policy change, no scheduler or
  admission change.
- No public API change, no new universal framework, no new authoritative CI policy.
- No 40 % claim, no GO decision, and no production selector promotion from this design.

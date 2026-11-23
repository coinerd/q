# CL5 Wave 3 — measurement design: does contract-aware selection protect regressions? (#9982)

Status: **design only, revision 2**. No trials have been run, no production selection/CI/admission or
scheduler behaviour is changed by this wave, and the 40 % claim remains a hypothesis.

Revision 2 incorporates the independent design review: the historical corpus **cannot** supply safety
evidence (a merged commit's tree is by definition one that passed comprehensive CI, so replaying it
can only "fail" through flakiness, which is quarantined as `unavailable`). Safety evidence therefore
moves to a pre-registered mutant corpus, and the decision gate moves after it.

Review gate: this revision must be re-reviewed before any execution (W3.0 onwards).

## Why W3 exists

W2 measured one case (D5) end-to-end and the answer was `missed`: the shipped selector passed a tree
that comprehensive CI failed. The contract layer merged afterwards (`scripts/cl5/contract-escalation.rkt`,
`4725b614`) decides fail-closed between "run the reduced required set" and "run comprehensive /
run-all", but proves only structural properties: consumers accounted for, detectors declared and
present, metadata parseable, instructions never reduced. Whether selected tests *detect* the
regressions comprehensive detects is empirical.

W3 has two questions with **different evidence sources**, and they are not interchangeable:

| Id | Question | Evidence source |
|---|---|---|
| **Q-S** (safety) | Would the reduced selected set have detected the regressions comprehensive detects? | **mutant corpus** (W3b) — history cannot answer this |
| **Q-E** (efficiency) | How much testing is realistically avoided, including the cost of escalations? | **historical corpus** (W3a) |

## Why the historical corpus is efficiency-only

Population of the historical corpus = commits on `main` whose own tree has a completed, successful
comprehensive run (otherwise the control baseline is unknown). Its defect tree **is** that green tree.
Therefore comprehensive can only fail on it via flakiness, environment drift or nondeterminism — all
of which this design quarantines as `unavailable`, never as detection. The expected number of
`detected` cases is ~0 **by construction**, not merely skewed.

Consequences, stated explicitly so no later reading can treat W3a as a safety result:

- W3a reports **M3, M4a, M4b, M5, M6 and escalation behaviour**. It reports `out-of-scope` and
  `unavailable` counts.
- W3a **cannot** report M1/M2 or contribute to the GO safety denominator.
- W3a **is** still valuable: the escalation rate on real change sets, and the cost model, are exactly
  what Q-E needs, and D5's `missed` result proves the shipped selector does miss real regressions —
  which is why safety must be measured where regressions are guaranteed to exist.

An `out-of-scope`-dominated corpus is therefore the **expected** outcome of W3a, reported as a finding
about how rarely merged changes are regressions, not hidden as a null result.

## W3a — historical corpus (efficiency and escalation behaviour)

### Population and strata

| Stratum | Inclusion | Minimum cases |
|---|---|---|
| S1 single-module | exactly one production module in one contract cone | 0 (W2 covers it) |
| S2 multi-module | ≥ 2 production modules in the same contract cone | ≥ 20 |
| S3 cross-contract | ≥ 2 production modules spanning ≥ 2 contract cones, or contract + non-contract | ≥ 20 |

**Exclusions** (recorded, never silently dropped): merge commits, revert commits, docs-only,
metadata-only (derived artifacts, manifests, metrics), formatting-only diffs, test-only diffs, and
commits whose parent tree has no successful comprehensive run (baseline unknown ⇒ `unavailable`).

**Pre-registered detected minimums.** W3a reports per-stratum `out-of-scope`/`unavailable`/detected
counts; there is **no** detected-case minimum here because none is attainable (see above). W3b carries
the detected-case minimums.

**No post-hoc selection.** The case list is materialised as a JSON manifest, committed before the
first trial; adding a case requires a new manifest revision, disclosed in the report.

### Case construction

For a corpus commit `C` with parent `P`:

| Surface | Tree | Meaning |
|---|---|---|
| defect | tree of `C` | the historical change, replayed |
| control | tree of `P` | must pass comprehensively; otherwise the case is `unavailable` |

Both surfaces are keyed by **tree SHA**, never by commit or PR merge commit. The reduced selected set
is executed on the defect tree **unconditionally**, before any commit, even when the decision layer
escalates — this shadow execution is what makes `safe-but-useless` (M4b) computable. The
comprehensive run is located by exact head SHA via `workflow_dispatch` on a uniquely named scratch
branch (Option A), classified from job/step payloads, never from logs.

### W3a reporting

Outcome counts, M3, M4a, M4b, M5, M6 (if and only if a matched-host comprehensive baseline exists),
plus the escalation reasons (`unmapped-contract`, `partial-consumers`, `no-consumer-detector`, …)
aggregated per stratum. **No safety claim of any kind.**

## W3b — mutant corpus (the sole safety evidence)

History cannot supply regressions, so they are constructed. W3b applies deliberate, behaviour-changing
edits to pinned revisions and measures which surface detects them.

### Mutant sources

| Source | Definition | Why |
|---|---|---|
| M-synthetic | one mutant per class per frozen contract cone: guard inversion, boundary shift, error-path removal, vocabulary keyword change | deterministic, covers the vocabulary/contract axis D5 failed on |
| M-reintroduced | reverse-apply the **production-code only** part of a bug-fix commit to that fix's tree | real historical regressions with known causes, not invented ones |

`M-reintroduced` construction is explicit: take fix commit `F` (identified by a fix reference and a
test change in the same commit), take tree of `F`, and reverse-apply only the non-test hunks of `F`'s
diff to the production files. Reversing the test hunks would delete the detector and make the mutant
undetectable by construction.

### Mutant manifest (frozen before any selection run)

- Committed JSON manifest with: mutant id, source, base revision, affected module(s), the exact
  reverse/forward patch, expected detector(s), and the stratification (S2/S3 cone).
- **Pre-registered count: N ≥ 60 comprehensive-failing mutants**, with **≥ 20 per stratum** and
  **≥ 12 per synthetic class**. Mutants where comprehensive passes are reported as
  `comprehensive-inadequate` — a finding about comprehensive testing, not about selection — and do
  not count toward N.
- Selection is frozen for the mutation phase: every comprehensive-failing mutant in the manifest is
  run; there is no per-mutant cherry-picking, and any revision of the manifest is disclosed with a new
  hash.

### W3b requirement

For every comprehensive-failing mutant, the reduced selected set is executed. `missed` mutants (reduced
set passes, comprehensive fails) are listed with the detector that should have caught them. W3b
supplies the M1/M2/GO denominator.

## Outcome taxonomy (mutually exclusive; reduced set always executed)

Every case is assigned exactly one label. The decision layer's instruction is recorded separately from
the shadow execution of the reduced set, so escalation and detection cannot be confused:

| Outcome | Definition |
|---|---|
| **correct-reduced** | decision `run-required` ∧ comprehensive fails ∧ reduced set fails |
| **correct-escalation** | decision `run-comprehensive`/`run-all` ∧ comprehensive fails ∧ reduced set **passes** |
| **unsafe-reduced** | decision `run-required` ∧ reduced set passes ∧ comprehensive fails — **Q-S failure** |
| **safe-but-useless** | decision escalates ∧ comprehensive fails ∧ reduced set **also fails** |
| **comprehensive-inadequate** | comprehensive passes on a case the manifest declares a regression (W3b mutants only) |
| **out-of-scope** | comprehensive passes on the defect tree (W3a history: the expected majority) |
| **unavailable** | incomplete/cancelled/timeout/missing producer/inconsistent surfaces/control red |

`correct-escalation` and `safe-but-useless` are disjoint by construction (reduced set fails vs passes).
`safe-but-useless` is the counterweight that makes "escalate everything" visible as a costly strategy
rather than a safe one.

## Metrics

| Id | Metric | Definition | Source |
|---|---|---|---|
| M1 | detection parity | comprehensive-failing cases where the reduced set also failed | W3b |
| M2 | misses | `unsafe-reduced` cases, with the comprehensive-failing test list | W3b |
| M3 | escalation rate | escalations / all classified cases | W3a + W3b |
| M4a | escalation rate on detected cases | `correct-escalation` / (`correct-reduced` + `correct-escalation` + `unsafe-reduced`) | W3b |
| M4b | unnecessary escalation | `safe-but-useless` / detected cases | W3b |
| M5 | selection size | reduced test files executed / comprehensive test files, per case | W3a + W3b |
| M6 | decision-layer cost ratio | `(M3 · C_comp + (1 − M3) · C_sel) / C_comp`, i.e. `(1 − M3) · (C_sel / C_comp) + M3` — see cost model below | W3a |
| M7 | decision-layer agreement | does the contract layer escalate exactly the `unsafe-reduced` cases (and not the `correct-reduced` ones)? reported as a confusion matrix | W3b |
| M8 | reduced-path share | (`correct-reduced` + `unsafe-reduced`) / detected cases; a GO needs this non-trivial, so "always comprehensive" cannot pass on safety alone | W3b |

### Cost model (M6, pre-registered)

Per change set, the strategy either runs the reduced set or, when the decision layer escalates, the
comprehensive suite. Nothing runs both (shadow execution is a measurement device, not the deployed
strategy). With `C_sel` = median wall-clock of the reduced set on the matched host and `C_comp` = median
wall-clock of `./qt full` on the matched host:

```
M6 = (1 − M3) · (C_sel / C_comp) + M3
```

Both arms are measured **on the same host, same revision, same harness invocation**, three runs,
median. `C_sel`/`C_comp` from different machines is not a measurement and must never be reported.

**Matched host.** Same physical/virtual machine, same revision, same warm/cold state, sequential runs.
If a matched-host `C_comp` cannot be completed (W2 measured that a local comprehensive run does not
finish within the harness timeout on the dev host), M6 is **`unavailable`** and **no GO is possible**;
CI-derived timings must not be substituted. Resolving this needs a bounded or CI-hosted selected-run
surface, which is a separate issue with its own review — not part of W3.

## Statistical discipline

- Pre-register N. With `missed = 0` in N detected cases, the rule-of-three upper 95 % bound is
  ≈ `3/N`: 0/20 ⇒ ≤ 14 %, 0/60 ⇒ ≤ 5 %. Report the bound; never claim the miss rate is zero.
- The detected denominator is `unsafe-reduced + correct-reduced + correct-escalation +
  safe-but-useless` from **W3b only**; `out-of-scope`, `comprehensive-inadequate` and `unavailable`
  never enter it.
- Report per stratum: ≥ 20 detected mutants in S2 and ≥ 20 in S3, or the pooled claim is not made.
- Cases are a purposive corpus, not a random sample of "regressions". Report counts, bounds and the
  confusion matrix; no p-values, no extrapolation to all changes.
- Any change to corpus, mutant manifest, metrics or thresholds after the first trial invalidates the
  comparison and is disclosed.

## Decision rule (pre-registered)

| Outcome | Conditions |
|---|---|
| **GO** (advisory only) | `M2 = 0` on the W3b corpus with N ≥ 60 detected mutants (upper bound ≤ 5 %) and ≥ 20 per stratum, **and** `M6 ≤ 0.60` on a matched host, **and** `M8 ≥ 0.50` |
| **REVISE** | `M2 = 0` but `M6 > 0.60`, `M6 = unavailable`, or `M8 < 0.50` |
| **REJECT** | any `unsafe-reduced` mutant, or any case where the decision layer reported `covered` while comprehensive failed |

The decision gate is **after** W3b. W3a cannot produce a GO: it supplies the escalation behaviour and
the cost term only.

GO never enables production selection by itself: it authorises an advisory rollout proposal, which
needs its own review. No CI policy change is ever in scope.

## Wave plan (execution gated on this revision's review)

| Wave | Work | Gate |
|---|---|---|
| W3.0 | historical corpus extraction + manifest freeze (S2/S3 ≥ 40 cases) | manifest reviewed; exclusions listed |
| W3.1 | single dry case end-to-end on a scratch branch | provenance + classification match W2's D5 behaviour |
| W3.2 | S2 then S3 historical trials (efficiency/escalation) | all cases classified; none silently dropped |
| W3.3 | mutant manifest freeze: ≥ 60 comprehensive-failing mutants, ≥ 20 per stratum | manifest hash committed before any selection run |
| W3.4 | mutant trials (the safety measurement) + M7 confusion matrix | M1/M2/M8 reported with bounds; `missed` mutants listed |
| W3.5 | report + decision | M1–M8 with intervals and the outcome table; decision stated |

## Threats to validity (must appear in the final report)

- Flakiness and nondeterminism inflate "detected"; every comprehensive failure must be attributable to
  a named job, and repeated failures are recorded.
- Non-hermetic tests (network, time, environment) can fail either surface; such cases are `unavailable`.
- **Parity, not correctness.** The experiment measures agreement with comprehensive testing. A
  regression invisible to comprehensive (including a commit that weakened tests) is undetectable by
  construction — `comprehensive-inadequate` quantifies part of this, but the limit stands.
- History bias: merged commits passed CI, so W3a is structurally efficiency-only (see above).
- Mutant bias: synthetic mutants are chosen by class; `M-reintroduced` mutants depend on the density of
  fix commits in the window. Both are frozen in advance and reported with their construction counts.
- Cache and warm-start effects on M6; only same-host, same-state comparisons count.
- Escalation cost is real and is inside M6 by construction; a high M3 shows up as M6 → 1.
- CI capacity/billing can make cases `unavailable`; a corpus below the pre-registered N yields no GO.

## Artifacts

- `docs/reports/CL5-W3-CORPUS-<manifest-sha>.json` — frozen historical corpus (pre-registered).
- `docs/reports/CL5-W3-MUTANTS-<manifest-sha>.json` — frozen mutant corpus (pre-registered, W3.3).
- `docs/reports/CL5-W3-VALIDATION-<sha>.md` / `.json` — final report generated from per-case records,
  carrying the outcome table with counts.
- Per-case checkpoints are append-only and never rewritten.

## Reuse (no new framework)

| Need | Existing component |
|---|---|
| case execution, staging, provenance, shadow selected run | `scripts/cl5/selection-experiment.rkt` |
| comprehensive ground truth by exact SHA | `scripts/cl5/ci-evidence.rkt` |
| grading | `scripts/cl5/oracle.rkt`, `task-spec.rkt` |
| decision layer (M7) | `scripts/cl5/contract-escalation.rkt` (evaluation only, never wired) |
| cones, fan-in, detector attribution | `scripts/cl5/ownership-audit.rkt`, `selection-guard.rkt` |

No test-runner, scheduler, admission, or CI-policy change is part of W3. A new execution surface
(e.g. a bounded comprehensive run or CI-hosted selected run) would be a separate issue with its own
review.

## Non-goals (explicit)

- No production test selection, no `./qt` behaviour change, no CI policy change, no scheduler or
  admission change.
- No public API change, no new universal framework, no new authoritative CI policy.
- No 40 % claim, no GO decision, and no production selector promotion from this design.

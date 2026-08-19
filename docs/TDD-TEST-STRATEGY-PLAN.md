# TDD and Test-Suite Improvement Plan

**Status:** Adopted — canonical governance record. The strategy below was
implemented through the test-suite campaign begun in v1.00.04 (waves W0–W5);
the operational state on `main` is recorded in
[Adoption status](#adoption-status--operational-state-after-the-waves-in-v10004).

**Supersedes:** PR #9348 (`docs/tdd-test-strategy-plan`), which carried the
original proposed plan and was never merged; that PR is closed as superseded by
this canonical copy on the default branch, updated to the current operational
state.

**Author:** Manus AI (original assessment); operational-state updates by the
test-suite campaign run in v1.00.04.

**Repository baseline assessed:** `main` at `dc4a0cf9` on 2026-08-16

## Adoption status — operational state after the waves in v1.00.04

The plan text below is the governance baseline: the feedback model, work
packages, risk table, and decision gates remain the contract this repository is
measured against. This section records how much of that contract is operational
on `main` as delivered in v1.00.04, with evidence pointers. Deviations are
stated explicitly; the plan text remains the target.

### Impact selection — implemented, fail-open, local opt-in only

The change-impact selector (`--changed-base` / `--changed-head`, with
`--impact-dry-run`, `--explain`, `--json-out`, and `--prioritize impact`) is
implemented in `scripts/run-tests/` and documented as the L1/L2 local loop in
`docs/TEST_CONVENTIONS.md` (W0, PR #9381). Selection is conservative and
fail-open: dynamic `require`, macros, `#lang reader`/generated code,
configuration/workflow/package changes, fixture/helper/runner changes, and
graph uncertainty escalate to a declared broad fallback (`fast` plus the area
suites of the changed files), never a silently smaller set.

It is deliberately **not a CI gate**: the former `test-impact` GitHub job was
removed because it exceeded the repository's 30-minute job runtime limit, and it
must not be reintroduced into `.github/workflows/`. The authoritative PR
protection remains the static gates in `.github/workflows/ci.yml` — the
three-shard `fast` suite on every PR/push plus the platform, security, smoke,
and workflow gates. The non-CI boundary is enforced by grepping
`.github/workflows/` for the selector flags (zero matches expected). The
rollout stage reached is the local developer default (2C-equivalent); the 2B
shadow-CI stage was replaced by the fail-open fallback design plus unchanged L3
gates, because the shadow job could not fit the job runtime budget.

### `@covers` manifest — pilot-area expansion complete (W1)

`tests/.coverage-manifest.json` now maps **94 test files** covering **92
production modules** in the reviewed pilot areas (runtime, tools, providers,
extensions, TUI), up from the 10-file / 20-module baseline. Every entry is
`source: metadata` — reviewed `@covers` tags, not name inference — regenerated
via `racket scripts/run-tests.rkt --generate-covers-manifest` (landed via the
W0–W2 campaign merges, #9376/#9381/#9383). No repository-wide mapping quota was
introduced; unmapped sources keep the fail-open fallback.

### Metadata lint — report-only in CI, enforcement in v1.00.05 (W2)

`.github/workflows/ci.yml` runs a `metadata-lint` step in report-only mode
(`continue-on-error: true`); missing mandatory tags are visible in the run
log/summary without gating merges (PR #9383). Current coverage: `@speed`
1,298/1,298 files, `@boundary` 1,280/1,298, `@area` ownership map complete.
Enforcement flips to blocking in v1.00.05 per the schedule recorded in
`docs/TEST_CONVENTIONS.md`.

### Full regression — nightly + manual dispatch, first-run evidence recorded (W3)

`.github/workflows/full-regression.yml` runs the broad suite on a nightly
schedule (02:30 UTC), a weekly Sunday 03:30 UTC mutation-pilot-only trigger,
and manual `workflow_dispatch`. It uses a static six-shard matrix with an
explicit 90-minute per-shard timeout — a timeout is never reported as success —
and retains `run-summary.json`, `matrix-summary.json`, per-shard JSON, and
failure logs. First-run evidence is recorded in
`docs/reports/test-regression-log.md`: run 32288930966 (manual dispatch;
definitive verdict `fail`: one genuine DEEP-9 failure, one macOS platform
timeout, one evidence-infra defect — triaged under issue #9384) and the
re-dispatch run 32297908687 on `main` @ `1764ed84` (PRs #9385, #9387). The
triage protocol is `docs/operations/test-regression-triage.md`; its
release-linkage rule (release readiness requires full-regression evidence,
never a green `fast` run alone) is binding.

### Shard plan — guarded `FAST_SHARD_PLAN` activation: ACTIVE (W4)

Duration-aware LPT shard planning ships with a guarded activation path via the
`FAST_SHARD_PLAN` repository variable (default `off` = byte-identical
round-robin planning). The report-only artifact from `ci.yml` run 32297737631
recommended activation with inventory preserved (1,106/1,106 files, zero
cross-shard duplicates), and the predicted max-shard duration improved:
592.4 s (duration-aware) vs 695.1 s (round-robin), −102.7 s (−14.8%).
Decision: **ACTIVATE** — `FAST_SHARD_PLAN=active` was set on 2026-08-19. The
activation-gate observed check (first post-activation run 32302776738) passed:
observed max shard 351 s vs the 349.6 s round-robin baseline (+0.4%, within
run-to-run variance) with the full 1,106-file inventory executed. The complete
decision record, the revert command (`gh api -X PUT
repos/coinerd/q/actions/variables/FAST_SHARD_PLAN -f value=off`), and the
measurement caveats live in `docs/reports/test-regression-log.md` (W4 section,
PR #9388).

### Mutation pilot — bounded, survivor-driven strengthening (W5)

The bounded adequacy pilot (`scripts/run-tests/mutation-pilot.rkt`) runs only as
an opt-in weekly scheduled job or a manual dispatch input — never in PR gates —
currently scoped to `runtime/memory/policy.rkt`. The first recorded run (run-5)
killed only 4 of 18 mutants; per the consolidation policy those survivors drove
new boundary and boolean-polarity tests
(`tests/memory/policy-boundary-test.rkt`), raising the kill rate to **16/18**,
with the two remaining survivors documented as equivalent mutants (runs 6–8,
including a clean-cache re-verification; PR #9389). Consolidation in any scope
remains blocked without the full evidence set required by
`docs/operations/test-consolidation-policy.md`.

### Delivered vs. deferred

**Operational:** the local impact loop with a fail-open selector; the expanded
`@covers` manifest; report-only metadata lint with a dated enforcement flip;
nightly/manual full regression with retained evidence and a triage protocol;
the active duration-aware shard plan; and the bounded mutation pilot with
survivor-driven test strengthening.

**Deferred (still governed by the plan text below):** Phase 0 rolling trend
baselines beyond the retained run summaries and per-run JSON artifacts; the
remaining metadata gap (18 files without `@boundary`); confirmation of the
L0–L2 latency SLOs against measured baselines; and any test-suite
consolidation, which stays blocked pending adequacy evidence per the
consolidation policy.

## Executive Summary

q already has substantial test infrastructure: declarative test metadata, a custom runner with subprocess and grouped modes, profile-aware skips, sharding, and separate CI gates for fast, platform, workflow, and smoke-related checks. The immediate objective is therefore **not to reduce coverage or remove tests**. It is to restore a consistently fast, trustworthy TDD feedback loop by making test ownership, selection, ordering, isolation, and regression cadence explicit.

The recommended end state is a layered model in which a developer runs the current test and a small impact-selected set while editing; pull requests receive a fast, deterministic impact signal before the existing broader suites; and the complete regression suite runs on a scheduled and manually dispatchable cadence. No existing required broad PR gate should be removed until the new selection mechanism has demonstrated that it is conservative and has produced no selection misses during a shadow period.

> **Principle:** The full test suite remains the safety net. It should not be the default inner-loop command.

The plan prefers repository-owned Racket scripts and GitHub Actions configuration over a new external test-management service. Any optional mutation-testing tool must be free and open source, compatible with the supported Racket version, and confined to an experimental scheduled job until it proves useful.

## Current-State Assessment

The repository has 1,286 files named `test-*.rkt`. The present metadata is uneven: 823 carry `@suite`, 1,255 carry `@speed`, but only 29 carry `@boundary`, 16 carry `@mutates`, and 12 carry `@isolation`. The runner therefore still relies materially on filename and path heuristics. This is a workable transition state, but it is not yet a reliable basis for conservative change-impact selection.

| Existing capability | Evidence | Assessment | Planning implication |
|---|---|---|---|
| Layered suites | `smoke`, `fast`, `unit-fast`, `slow`, `runtime`, `extensions`, `workflows`, `platform`, and other suite names are already supported.[1] | **Strong foundation.** The suite taxonomy mostly exists. | Preserve the taxonomy and make its ownership, boundary, and entry criteria complete and verifiable. |
| Local execution modes | The runner selects grouped in-process execution for `unit-fast` and subprocess isolation otherwise.[2] | **Useful but underexploited.** In-process eligibility depends on test shape and the `unit-fast` selector depends on metadata. | Increase the stock of truly isolated unit tests and make the fast local command discoverable. |
| Parallelism and sharding | The runner supports job parallelism and round-robin shards; CI uses three `fast` shards and two workflow shards.[1] [3] | **Implemented, but static.** Current shards do not use duration history or changed-code risk. | First eliminate avoidable test coupling; then balance shards using measured duration. |
| Isolation controls | Tests can declare mutations, isolation, timeouts, and profile requirements; the runner serializes mutation-sensitive files ahead of parallel work.[2] [4] | **Correct direction, incomplete classification.** Three environment/harness failures remain documented for parallel mode.[1] | Fix root causes and require explicit side-effect metadata instead of widening serial execution. |
| Machine-readable outputs | The runner can emit per-file JSON and gate evidence.[4] | **Strong measurement hook.** | Turn these outputs into a retained performance, flake, and selection-quality baseline. |
| CI policy | Every PR runs lint, all `fast` test shards, the platform subset, workflow shards, and smoke-related checks.[3] | **Safe but broad.** The pre-submit selection is static rather than change-aware. | Add an impact-selected early signal in shadow mode before altering any required test gate. |
| Full-regression cadence | The CI trigger is push, pull request, and manual dispatch; there is no scheduled trigger in the current workflow.[3] | **Gap.** Slow and full tests do not have an explicit periodic policy in this workflow. | Add a separate scheduled full-regression workflow with clear ownership and escalation. |

The current runner applies static filters and path/name heuristics; it does not consume `git diff`, a module dependency graph, changed-code coverage, or test history to decide which tests to execute. Its default result order follows the selected file list, aside from serializing mutation-sensitive files before parallel batches.[2] [4] This is the main gap between the current state and the supplied recommendations.

## Target Feedback Model

The following table defines the desired execution contracts. Time budgets are initial targets, not claims about current duration; Phase 0 establishes the baseline and adjusts targets only with evidence.

| Level | Trigger and purpose | Selection rule | Initial target | Required safety rule |
|---|---|---|---|---|
| **L0 — current behavior** | During the red-green-refactor loop | The test file currently being changed or added | p90 at or below 5 seconds | Developers run it before widening scope. |
| **L1 — direct unit impact** | Before commit and on a PR’s first test job | Direct tests for changed modules plus required local contract tests | p90 at or below 30 seconds | Any unmapped source file escalates rather than silently selecting zero tests. |
| **L2 — transitive impact** | Before push or as the next PR test job | L1 plus tests for dependent modules and boundary contracts | p90 at or below 120 seconds | Dynamic-load, generated-source, configuration, test-infrastructure, or graph uncertainty escalates to the broader tier. |
| **L3 — PR regression** | Required pull-request protection | Existing `fast`, `platform`, and `workflows` coverage, initially unchanged | Maintain or improve current p50/p95 duration | This gate remains required throughout the shadow and rollout periods. |
| **L4 — full regression** | Scheduled, manually dispatchable, and release readiness | `all`/broad suite with profile-aware execution, plus the required platform and integration variants | Reliable completion with published trend data | Failure creates or updates a visible issue; a timeout is never reported as a pass. |
| **L5 — adequacy experiments** | Scheduled, opt-in, and targeted | Mutation or manual micro-mutation analysis for a small changed/high-risk module set | Bounded compute budget | It informs consolidation; it never blocks a normal developer loop initially. |

This follows the established regression-testing distinction between an inexpensive pre-submit selected set and a broader post-submit set ordered to reveal failures sooner.[6] The taxonomy is intentionally compatible with q’s existing suites rather than replacing them.

## Work Packages and Sequence

### Phase 0 — Establish a Trustworthy Baseline and Guardrails

The first change should be measurement, not optimization. Amend the test runner’s JSON result schema, or add a small repository-owned summarizer, so every CI test job exposes file count, pass/fail/timeout/skip counts, wall-clock duration, per-file duration, execution mode, shard, profile, metadata completeness, and runner version. Upload the raw JSON as a CI artifact and publish a concise job summary. Store a rolling, reviewable baseline in `docs/reports/` or a versioned JSON file only if it is generated deterministically; avoid a database or external service.

For ten successful PR runs and at least two full-regression attempts, record the p50 and p95 duration of each current suite, the slowest test files, retry rate, zero-test detections, profile skips, and the known parallel-only failures. Treat the existing failure ledger as a classification aid, not an exemption from fixing failures. The documented `test-interfaces-tui.rkt`, `test-settings.rkt`, and `test-run-tests-ledger.rkt` parallel-mode problems should each receive a reproducible root-cause issue and an isolation/fixture remedy.[1]

| Deliverable | Acceptance condition | Owner boundary |
|---|---|---|
| Test-performance report | A CI summary and artifact are produced for every test job without changing pass/fail semantics. | Test infrastructure |
| Metadata quality report | The report lists missing, invalid, and heuristic-only metadata by test file and module area. | Test infrastructure with area maintainers |
| Baseline decision record | It states actual suite timings, flake rate, and the chosen initial L0–L4 SLOs. | Maintainers |
| Parallel-failure remediation backlog | Each documented parallel-only failure has a reproduction command, cause hypothesis, and exit criterion. | Owning subsystem |

### Phase 1 — Refactor Test Architecture and Complete Classification

Treat test code as production code. Preserve the existing `tests/helpers`, fixtures, scenario harnesses, and module-correspondence convention, while refactoring only when a specific smell is observed: duplicated fixture construction, mixed unrelated behaviours, hidden shared state, implementation-coupled assertions, or integration work embedded in a unit test. Extract builders and fakes when they improve readability; do not create a generic helper framework merely to reduce line count.

Make the existing metadata declarative contract complete. New tests must carry `@suite`, `@speed`, `@boundary`, `@mutates`, `@isolation`, `@timeout`, and `@requires` when applicable. Existing tests should be migrated module area by module area, starting with runtime, provider, session, tools, extensions, TUI, and workflows. Introduce a test-metadata lint that rejects invalid values and reports missing required tags. Resolve the current vocabulary ambiguity in one documented schema: the runner recognizes both `process` and `subprocess` as isolation signals, while the conventions document only `process`.[1] [2]

The initial completion target is 95% explicit classification for runnable tests and 100% classification for any test selected by L1 or L2. The exceptions must be named and owned rather than silently classified by filename. Require `module+ test` or an explicitly documented equivalent for tests intended for grouped in-process execution, after verifying that the conversion preserves RackUnit discovery and failure reporting.

| Change | Repository locations | Definition of done |
|---|---|---|
| Versioned metadata schema and lint | `docs/TEST_CONVENTIONS.md`, `scripts/run-tests/`, focused runner tests | One vocabulary, deterministic validation, and no heuristic-only L1/L2 candidate. |
| Area-by-area test refactoring | `tests/` plus corresponding production modules | Tests expose one behaviour or a coherent scenario, isolate mutable state, and reuse only domain-specific helpers. |
| Explicit unit eligibility | Test headers and test module forms | `unit-fast` inventory shows only safe grouped/in-process candidates and rejects hidden mutation. |
| Test ownership map | Generated report with source area, test suite, boundary, and owning path | Every production area has an accountable test destination. |

### Phase 2 — Introduce Conservative Change-Impact Selection

Build a repository-owned impact manifest before attempting automatic selection. The manifest must map each test file to the production modules and contracts it directly validates, using explicit `@covers` metadata or a checked-in/generated mapping with reviewable provenance. Do not infer a binding solely from names, because q has many similarly named cross-cutting modules. Add a Racket script that reads `git diff --name-only <base>...<head>`, normalizes changed source, test, configuration, and fixture paths, then produces an explainable selected-test list with a reason for every inclusion and every escalation.

The implementation should then add a source-module dependency graph. A short design spike must compare compiler-derived Racket dependency information with a syntax-based `require` extractor and choose the more conservative, deterministic option. Dynamic `require`, extension loading, generated code, macros, configuration schema, package manifests, runner changes, helper changes, and graph parse failures are **escape hatches**: they must expand selection to a declared broad suite, not risk an omission.

Start with a manual command such as `racket scripts/run-tests.rkt --changed-base <ref> --changed-head <ref>`, retaining the existing explicit-file mode for the current test. It must support `--inventory`-style dry runs, emit JSON evidence, and provide an `--explain` view that shows changed file, direct mapping, dependency path, selected test, and fallback reason. Add an informational `test-impact` PR job; do not make it required and do not remove the three `fast` shards during the shadow period.

| Rollout stage | Selection policy | Evidence required to progress |
|---|---|---|
| **2A — manual and inventory only** | Show selection and fallback explanations; do not execute automatically. | Reviewed mappings for two high-change areas and no unexplained empty selection. |
| **2B — shadow execution** | Run L1/L2 on every PR alongside unchanged L3 gates. | At least 20 merged PRs or four weeks, whichever yields more evidence; zero confirmed relevant test omission; every fallback is explainable. |
| **2C — developer default** | Document L1/L2 commands as the normal local loop and make `test-impact` the earliest PR signal. | p90 meets the agreed budget and developers can reproduce CI selection locally. |
| **2D — policy review** | Decide whether any full L3 work may move from every PR to post-merge/scheduled execution. | Maintainer-approved risk review, dependable L4 completion, and a documented rollback to current CI behaviour. |

This conservative rollout directly reflects regression-test-selection practice: select tests before submission, widen to dependent-module testing afterward, and use inexpensive signals rather than assuming code coverage is required.[6]

### Phase 3 — Prioritize and Balance the Tests That Still Run

Selection reduces the candidate set; prioritization improves time to first useful failure within that set. Extend the runner with an optional, deterministic priority key. The initial order should be explicit current-test files, direct `@covers` tests, transitive dependents, changed-boundary contract tests, recently failed tests, and finally all remaining selected tests. A test’s historical failure weight must decay and be explainable; a stale failure must not permanently dominate execution. Preserve serial execution for mutation-sensitive tests, but prioritize within the serial and parallel partitions independently.

Use the Phase 0 per-file duration data to replace round-robin CI sharding with deterministic duration-aware bin packing. The planner must respect serial groups, profile skips, and any explicit co-location/anti-co-location constraints. Validate it first in report-only mode against the last retained CI artifacts, then enable it only if its predicted maximum shard duration improves over round robin without reducing test inventory. Test-case prioritization is valuable because it advances likely failures, not because it allows a selected test to be discarded.[7]

| Feature | Behaviour | Guardrail |
|---|---|---|
| `--prioritize impact` | Emits a stable order and reason per test. | Stable ties are ordered by repository path; a priority change never changes selection. |
| Duration-aware shard plan | Allocates files to minimize the predicted slowest shard. | Serial/mutation-sensitive groups remain intact and the planner can fall back to round robin. |
| Failure-history input | Uses only retained CI JSON results and bounded recency. | Missing or corrupt history produces a deterministic neutral order. |
| First-failure reporting | CI summary surfaces the first failing selected test with its selection reason. | All test results and logs remain available; summary is not the source of truth. |

### Phase 4 — Make Full Regression Explicit, Reliable, and Observable

Add a separate `full-regression.yml` workflow triggered by a nightly schedule and `workflow_dispatch`. It should run the complete `all`/broad suite using the current profile-aware runner, appropriate sharding, platform variants where feasible, and the workflow suite. Keep release readiness on the same full-regression evidence or require a fresh manual dispatch; release status must never be inferred from a successful `fast` run alone.

The workflow needs an explicit failure protocol. A failure should identify the test file, log, execution profile, runner mode, and whether an isolation rerun changed the result. A timeout or profile skip should be reported distinctly from a passing test. Repeated nondeterminism must become an owned issue with a quarantine expiry date, not a permanent entry in a known-failure ledger. The existing ledger’s JSON source-of-truth and profile-aware skip semantics provide the appropriate foundation.[1] [4]

| Operational event | Required response |
|---|---|
| Full regression fails after a green PR | Open or update a regression issue within one working day, attach evidence, and assess whether impact selection missed a relevant test. |
| Full regression times out | Preserve shard/test progress, classify the bottleneck, and set a bounded remediation experiment; do not relabel the run as successful. |
| Flake recurs | Add a minimal reproduction, fix isolation or environmental dependency, and run the test repeatedly before removing the flake marker. |
| Scheduled regression is unavailable | Escalate release readiness to a manual full run rather than silently proceeding. |

### Phase 5 — Assess Adequacy Before Any Test-Suite Reduction

Test-suite reduction is deliberately last. After Phases 0–4 provide reliable impact and duration data, identify candidates only where two or more tests duplicate a behavioural contract, cover the same explicit `@covers` scope, have no distinct failure history, and are readable enough to consolidate without obscuring the intent. Prefer consolidating shared setup or complementary assertions over deleting a named behaviour test.

Run a bounded mutation-analysis spike on a small number of pure, high-value modules, beginning with a pilot selected from the L1 inventory. The pilot may use an open-source Racket-compatible tool or a small reviewed set of manual micro-mutations if no mature tool meets the compatibility and maintenance criteria. Mutate only changed or nominated modules, cap runtime, retain every mutant/result artifact, and keep the work out of normal PR gates. Mutation testing measures whether tests detect small artificial faults, but its traditional computational cost makes targeted use preferable to universal execution.[8]

A test may be deleted only when the consolidation pull request documents the preserved behaviours, the affected L1/L2/L3/L4 suites pass, mutation or equivalent adequacy evidence shows no lost detection for the relevant scope, and an owner approves the readability trade-off. Line or branch coverage equivalence alone is insufficient evidence for deletion.

## Governance, Risks, and Non-Goals

The plan does not propose a wholesale folder reorganization, a new commercial testing service, or immediate removal of PR regression tests. It also does not subsume the separately open cross-frontend end-to-end event-order work in issue #9345; that work should adopt the metadata and feedback-level contract once established.[5]

| Risk | Prevention | Rollback trigger |
|---|---|---|
| Impact selector omits a relevant test | Conservative escape hatches, shadow runs, full L3 retained, nightly L4. | Any confirmed omission returns selection to report-only mode for the affected area. |
| Metadata becomes bureaucracy | Generate reports, migrate by ownership area, and use lint only after examples and helpers are in place. | High invalid-tag rate or developer friction pauses enforcement and simplifies schema. |
| Faster parallelism increases flakes | Fix state, filesystem, environment, and repo-surface coupling before adding workers. | Flake rate rises above baseline for two consecutive reporting windows. |
| History-based prioritization obscures failures | Stable and explainable priority reasons; neutral fallback; retain all logs. | Priority cannot be reproduced from artifact data. |
| Mutation analysis consumes CI capacity | Scheduled, bounded pilot only; no PR blocking. | Runtime budget exceeded or findings do not change testing decisions. |
| Full regression becomes ignored noise | Clear ownership, triage SLO, visible dashboards, and release dependency. | Two untriaged failures or unavailable runs in a rolling month. |

## Decision Gates and Success Measures

The maintainers should review the following scorecard after each work package rather than treating implementation as automatically successful.

| Measure | Baseline source | Initial success condition |
|---|---|---|
| Metadata completeness | Phase 0 inventory | At least 95% explicit classification overall; 100% for L1/L2 candidates. |
| Local feedback latency | Developer/CI JSON for L0 and L1 | L0 p90 at or below 5 seconds and L1 p90 at or below 30 seconds, or an evidence-backed adjusted target. |
| Impact selection correctness | Shadow PR comparison with unchanged L3 | Zero confirmed relevant omissions during the shadow criterion. |
| Explainability | Selector JSON and CLI output | Every selected test and every fallback includes a machine-readable reason. |
| CI time to first relevant failure | PR test artifacts | Improves from the Phase 0 baseline without reducing L3 inventory. |
| CI wall-clock balance | Per-shard duration | Predicted and observed slowest-shard time improves or remains neutral after shard-planner rollout. |
| Parallel reliability | Repeated suite execution | No increase from baseline in parallel-only failures; each known failure has a tracked remedy. |
| Full-regression health | Scheduled workflow history | Full regression is run on schedule, reports a definitive status, and meets the agreed triage process. |
| Test consolidation safety | Consolidation PR evidence | No deletions based solely on coverage; adequacy evidence retained. |

## Implementation Order

The work should be delivered as small, independently reviewable pull requests in the following order. Each row is a dependency boundary, not a time estimate.

| Order | Pull-request scope | Depends on | Review focus |
|---|---|---|---|
| 1 | Per-file JSON retention and CI performance summary | None | Does not alter test result semantics. |
| 2 | Metadata-schema normalization and lint in report-only mode | 1 | Backward compatibility, accurate inventory, documented isolation values. |
| 3 | Fix or isolate the three documented parallel failures | 1–2 | Reproducibility and removal of hidden shared state. |
| 4 | Area-by-area metadata completion and `unit-fast` eligibility audit | 2 | Test ownership, behaviour clarity, safe grouped mode. |
| 5 | `@covers` manifest, impact inventory, and `--explain` | 2 and 4 for pilot areas | Conservative mapping and fail-open escalation. |
| 6 | Shadow `test-impact` CI job | 5 | Selection evidence while L3 remains unchanged. |
| 7 | Local L1/L2 documentation and deterministic prioritization | 5–6 | Reproducible commands and selection/order separation. |
| 8 | Duration-aware shard planning in report-only then active mode | 1 and 3 | Determinism, isolation constraints, and measured balance. |
| 9 | Scheduled/manual full-regression workflow and triage protocol | 1 and 3 | Definitive status, artifacts, release linkage. |
| 10 | Targeted mutation/adequacy pilot and carefully evidenced consolidations | 4–9 | Bounded compute, useful findings, and no coverage-only deletions. |

## References

[1]: https://github.com/coinerd/q/blob/main/docs/TEST_CONVENTIONS.md "q test conventions"
[2]: https://github.com/coinerd/q/tree/main/scripts/run-tests "q test-runner implementation"
[3]: https://github.com/coinerd/q/blob/main/.github/workflows/ci.yml "q continuous-integration workflow"
[4]: https://github.com/coinerd/q/blob/main/scripts/run-tests/runner.rkt "q test execution orchestration"
[5]: https://github.com/coinerd/q/issues/9345 "Cross-frontend end-to-end event-order tests"
[6]: https://research.google/pubs/techniques-for-improving-regression-testing-in-continuous-integration-development-environments/ "Elbaum, Rothermel, and Penix, Techniques for improving regression testing in continuous integration development environments"
[7]: https://digitalcommons.unl.edu/csearticles/9/ "Rothermel et al., Prioritizing Test Cases for Regression Testing"
[8]: https://research.google/pubs/state-of-mutation-testing-at-google/ "Petrovic and Ivankovic, State of Mutation Testing at Google"

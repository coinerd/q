# v1.00.09 Plan — TDD L4 Status Integrity and Cross-Platform Evidence Closure

**Status:** Proposed

**Owner:** q maintainers

**Prepared from:** `main` at `2640112391ef2d3ec7f14b6986f77d0c56173fd6` (v1.00.07)

**Author:** Manus AI

## Purpose

This milestone completes the remaining operational gap in the adopted TDD and test-suite improvement plan: **the L4 full-regression evidence artifact must be at least as conservative as the GitHub workflow that produces it.** The milestone does not restore the removed `test-impact` GitHub job. Change-impact selection remains a local, opt-in, fail-open L1/L2 workflow because its former CI implementation exceeded GitHub’s 30-minute job runtime limit.[1]

The immediate trigger is the latest scheduled full-regression run, [32443625177][2], on the current head revision. Its six Linux shards and workflows suite passed, but the macOS platform suite failed `tests/test-worker-security.rkt`; the overall GitHub workflow correctly concluded `failure`. The generated L4 `run-summary.json` nevertheless reported `pass`, because the aggregation procedure uses only the six Linux shard records. The earlier `v1.00.06` false-green evidence was corrected in the regression log, and the original two macOS failures were tracked under issues #9406 and #9407.[3] [4] [5]

> **Release rule:** v1.00.09 is not complete until a manual full-regression run on the release candidate is green in both places: the GitHub workflow conclusion and the retained `run-summary.json` artifact. A green Linux-only aggregate is insufficient.

## Scope and Non-Goals

| In scope | Explicitly out of scope |
|---|---|
| Conservative aggregation of all required L4 job outcomes and evidence artifacts. | Reintroducing `test-impact` to PR or scheduled GitHub CI. |
| Root-causing and fixing the remaining macOS LF3 assertion without a silent skip. | Repository-wide expansion of the `@covers` manifest. |
| Deterministic, tested release-evidence validation and clear triage outputs. | Test-suite deletion or consolidation. |
| A fresh fully-green cross-platform full-regression run and evidence-log update. | Adding commercial or nonessential third-party dependencies. |

The implementation must use the existing Racket runtime, the repository’s test runner, GitHub Actions, and existing `jq`/artifact facilities only. No new external dependency is needed for the core logic.

## Exit Criteria

| ID | Required outcome | Objective evidence |
|---|---|---|
| **E1** | The definitive L4 summary represents all required execution lanes: six Linux shards, workflows suite, and macOS platform suite. | `run-summary.json` contains a per-lane status section and its top-level status is computed from all required lanes. |
| **E2** | A failed, cancelled, timed-out, skipped unexpectedly, missing, malformed, or unreadable required lane can never yield summary status `pass`. | Automated status-contract tests cover every listed adverse state; workflow output exposes the same non-pass class. |
| **E3** | The remaining `test-worker-security.rkt` LF3 macOS assertion passes through a semantics-preserving fix. | Targeted test passes on macOS arm64 and Linux; adversarial inside-root/outside-root/broken-link cases remain covered. |
| **E4** | The full-regression workflow produces a truly green release-evidence run. | A manual run on the v1.00.09 candidate has GitHub conclusion `success`, all required jobs green, and a `run-summary.json` top-level `pass`. |
| **E5** | The plan and triage record make the result auditable. | The regression log links the run and artifacts; the canonical TDD plan is updated only after E1–E4 are verified. |

## Wave Plan

### W0 — Define and Test the L4 Status Contract

Extract the status decision from the inline workflow shell into a small, deterministic repository-owned aggregation helper, preferably a Racket program under `scripts/run-tests/` using only the standard `json` library. The helper shall accept a directory or explicit paths for six shard records, one workflows record, one platform record, and the three upstream GitHub job conclusions. It shall emit both `matrix-summary.json` and a new schema-versioned `run-summary.json`.

The schema must retain the current shard totals and add a `required_lanes` object containing the lane name, expected record count, collected record count, upstream job conclusion, record verdict, evidence classification, and final lane status. The top-level status must use a conservative precedence order: **`timeout`** for cancelled, missing, unreadable, malformed, or timeout evidence; otherwise **`fail`** for any failed required job or assertion failure; otherwise **`pass`** only if every required lane is present and passing. An unexpected `skipped` status is a failure unless the workflow’s mutation-only schedule explicitly excludes the entire L4 aggregate.

Add focused fixture tests for: complete pass; one failed Linux shard; one failed workflows suite; one failed platform suite; missing platform artifact; malformed platform JSON; cancelled platform job; and an empty evidence set. Each test must prove that `pass` is impossible in every adverse case. Retain the `if: always()` artifact upload behavior so red evidence remains available for triage.

**Acceptance gate:** locally executed helper tests pass, including an assertion that reproduces run 32369346059/32443625177’s former false-green shape and now resolves to `fail`.

### W1 — Wire All Lane Evidence into `full-regression.yml`

Replace the existing Linux-shard-only aggregation step with the W0 helper. Keep the six-shard Linux matrix, workflows suite, and macOS platform suite as explicit required lanes. Pass `needs.test.result`, `needs.workflows-suite.result`, and `needs.test-platform.result` into the summarizer as machine-readable inputs, rather than relying on job ordering alone. Download and validate the existing `results-shard-*`, `results-workflows`, and `results-platform` artifacts separately enough to prevent filename flattening from obscuring their lane provenance.

The workflow summary must show a concise required-lane table and state why a non-pass result occurred. The report job must publish the same top-level status. Release-readiness documentation must name the versioned summary schema and require agreement between the run conclusion and summary status. Update the triage procedure so a summary/workflow disagreement is a dedicated **evidence-integrity event** and is release-blocking.

**Acceptance gate:** a repository-level workflow fixture or testable command verifies all status mappings; review of the changed workflow confirms every required lane is fed into the status function; a deliberately failed platform fixture produces `fail`, not `pass`.

### W2 — Resolve LF3 on macOS Without Weakening Path Safety

Issue #9407’s original fixture remains red in the current scheduled run despite the v1.00.07 change. Diagnose the behavior on macOS arm64 before changing expectations: record `simplify-path`, `resolve-path`, link-target, and parent-prefix behavior for the exact temporary-tree fixture, while avoiding permanent debug output or platform special-casing. Determine whether APFS case behavior, link target representation, or longest-prefix reconstruction is the remaining source of `path-allowed?` returning `#f`.

Implement the smallest semantics-preserving correction to `resolve-longest-prefix` or its fixture construction. The solution must preserve all three security properties: a symlink wholly inside an allowed root with an unresolved tail is accepted; an allowed-root symlink to an outside-root target is rejected; and broken links are rejected. Add regression tests that use explicit temporary paths and separate the existing-prefix and non-existent-tail conditions. The cross-platform contract must pass on macOS arm64 and Linux without an unbounded skip. If a temporary quarantine is unavoidable, it must follow the existing policy: issue number, owner, expiry, rationale, and it must still force the L4 summary to `fail`.

**Acceptance gate:** targeted LF3 tests pass on macOS arm64 and Linux; no safety-rejection test is weakened; issue #9407 is closed only with linked artifact evidence.

### W3 — Candidate Evidence Run and Governance Closure

After W0–W2 merge, dispatch `full-regression.yml` manually from the v1.00.09 release-candidate commit. Download and inspect the three lane classes of evidence and the new summary artifact. Confirm six Linux shard records, one workflows record, and one macOS platform record; all must be present, parseable, and passing. Confirm that the GitHub workflow conclusion and `run-summary.json.status` are both `success`/`pass`.

Append one compact, factual entry to `docs/reports/test-regression-log.md`, including the run URL, head revision, all required-lane totals, artifact names, and the agreement check. Update `docs/TDD-TEST-STRATEGY-PLAN.md` to mark L4 fully addressed only after this evidence exists. Close or annotate the related platform issues with the replacement evidence, not merely with the merge of a proposed fix.

**Acceptance gate:** E1–E5 are all satisfied; an independent reviewer can reproduce the conclusion from retained artifacts without relying on prose assertions.

## Verification Matrix

| Layer | Command or evidence | Required result |
|---|---|---|
| Unit/status contract | New focused aggregation-helper tests | Every adverse required-lane state is non-pass; complete evidence is pass. |
| Targeted security | `raco test tests/test-worker-security.rkt` | LF3 and existing path-safety assertions pass. |
| Platform suite | `STRICT_TEST_RUNNER=1 racket scripts/run-tests.rkt --suite platform --jobs 4 --profile local` on macOS arm64 | Zero failures and zero timeouts; `results-platform` JSON is retained. |
| Linux regression lanes | Manual `full-regression.yml` dispatch | Six shard JSON records, workflows record, and platform record are all present and passing. |
| Cross-status integrity | Compare GitHub run conclusion with `run-summary.json.status` | Exact agreement; no `pass` summary for a red workflow. |
| Release governance | Updated regression log, triage protocol, and canonical plan | Links, head SHA, artifact evidence, and stated verdict agree. |

## Sequencing, Risk, and Rollback

| Risk | Control | Rollback or containment |
|---|---|---|
| A workflow-only fix masks an untested status edge case. | W0 makes decision logic testable before workflow wiring. | Revert the helper/workflow pair; preserve red artifacts and retain the previous explicit release block. |
| A macOS-specific fix opens a symlink escape on Linux. | Pair acceptance behavior with outside-root and broken-link rejection tests on both platforms. | Revert the resolver change; do not use a silent platform skip. |
| Artifact path layout changes again. | Preserve lane names at download time and validate expected records by schema and lane, not path glob alone. | Treat the lane as missing evidence and return `timeout`, never `pass`. |
| Long macOS provisioning erodes release feedback. | Preserve the explicit budget and record setup/runtime separately in the candidate evidence. | Classify a budget exhaustion as `timeout`; triage it as a release blocker. |

## References

[1]: https://github.com/coinerd/q/blob/main/docs/TDD-TEST-STRATEGY-PLAN.md "Adopted TDD and test-suite improvement plan"
[2]: https://github.com/coinerd/q/actions/runs/32443625177 "Latest scheduled full-regression run at v1.00.07"
[3]: https://github.com/coinerd/q/blob/main/docs/reports/test-regression-log.md "Full-regression decision and correction log"
[4]: https://github.com/coinerd/q/issues/9406 "macOS subprocess-edge-case regression"
[5]: https://github.com/coinerd/q/issues/9407 "macOS LF3 worker-security regression"
[6]: https://github.com/coinerd/q/blob/main/.github/workflows/full-regression.yml "Full-regression workflow to be hardened"
[7]: https://github.com/coinerd/q/blob/main/docs/operations/test-regression-triage.md "Regression triage protocol"

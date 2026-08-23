# v1.00.09 Plan — TDD L4 Status Integrity and macOS Canonical-Path Closure

**Status:** Proposed revision incorporating the 2026-08-21 LF3 analysis

**Owner:** q maintainers

**Prepared from:** `main` at `2640112391ef2d3ec7f14b6986f77d0c56173fd6` (v1.00.07)

**Author:** Manus AI

## Purpose

This milestone completes the remaining operational gap in the adopted TDD and test-suite improvement plan: **the L4 full-regression evidence artifact must be at least as conservative as the GitHub workflow that produces it.** The milestone does not restore the removed `test-impact` GitHub job. Change-impact selection remains a local, opt-in, fail-open L1/L2 workflow because its former CI implementation exceeded GitHub’s 30-minute job runtime limit.[1]

The immediate trigger is scheduled full-regression run [32443625177][2]. Its six Linux shards and workflows suite passed, but the macOS platform suite failed `tests/test-worker-security.rkt`; the overall GitHub workflow correctly concluded `failure`. The generated L4 `run-summary.json` nevertheless reported `pass`, because the aggregation procedure uses only the six Linux shard records.[2] [3]

The LF3 investigation materially narrows the macOS work. The likely cause is **asymmetric canonicalization**, not merely APFS casing: `resolve-longest-prefix` canonicalizes each existing component of the requested path, while the allowed root is canonicalized only at its final component. A symlinked or aliased ancestor can therefore leave a lexical allowed-root string and a physical resolved-request string that identify the same tree but fail a string-prefix comparison. The current v1.00.07 lowercasing branch did not fix the test and must not be relied on for authorization, because Apple supports both case-sensitive and case-insensitive APFS volumes.[4] [5] [6]

> **Release rule:** v1.00.09 is not complete until a manual full-regression run on the release candidate is green in both places: the GitHub workflow conclusion and the retained `run-summary.json` artifact. A green Linux-only aggregate is insufficient.

## Scope and Non-Goals

| In scope | Explicitly out of scope |
|---|---|
| Conservative aggregation of all required L4 job outcomes and evidence artifacts. | Reintroducing `test-impact` to PR or scheduled GitHub CI. |
| Symmetric root/request canonicalization for LF3, with inside-root, outside-root, and broken-link coverage. | Repository-wide expansion of the `@covers` manifest. |
| Deterministic, tested release-evidence validation and clear triage outputs. | Test-suite deletion or consolidation. |
| A fresh fully-green cross-platform full-regression run and evidence-log update. | Adding commercial or nonessential third-party dependencies. |

The implementation must use the existing Racket runtime, test runner, GitHub Actions, and existing `jq`/artifact facilities only. No new external dependency is required for the core logic.

## Exit Criteria

| ID | Required outcome | Objective evidence |
|---|---|---|
| **E1** | The definitive L4 summary represents all required lanes: six Linux shards, workflows suite, and macOS platform suite. | `run-summary.json` contains a per-lane status section and its top-level status is computed from all required lanes. |
| **E2** | A failed, cancelled, timed-out, unexpectedly skipped, missing, malformed, or unreadable required lane can never yield summary status `pass`. | Status-contract tests cover every adverse state; workflow and summary expose the same non-pass class. |
| **E3** | Candidate paths and allowed roots are canonicalized through the same longest-existing-prefix semantics before authorization. | A platform-neutral ancestor-alias test passes; an alias-root escape and a broken-link test fail closed. |
| **E4** | The LF3 fixture passes on macOS arm64 and Linux without weakening path safety or using a silent skip. | Targeted security tests and the macOS platform suite are green; the renewed #9407 evidence is linked. |
| **E5** | The release-candidate full regression is genuinely green. | GitHub conclusion is `success`; every required lane passes; `run-summary.json.status` is `pass`; both values agree. |
| **E6** | The result is auditable and accurately governed. | Regression log, triage protocol, canonical TDD plan, issues, head SHA, and retained artifacts agree. |

## Wave Plan

### W0 — Define and Test the L4 Status Contract

Extract the status decision from the inline workflow shell into a small, deterministic repository-owned aggregation helper, preferably a Racket program under `scripts/run-tests/` using only the standard `json` library. The helper shall accept six shard records, one workflows record, one platform record, and the three upstream GitHub job conclusions. It shall emit both `matrix-summary.json` and a schema-versioned `run-summary.json`.

The schema must retain shard totals and add a `required_lanes` object containing each lane’s expected record count, collected record count, upstream job conclusion, record verdict, evidence classification, and final lane status. The top-level status precedence is: **`timeout`** for cancelled, missing, unreadable, malformed, or timeout evidence; otherwise **`fail`** for any failed required job or assertion failure; otherwise **`pass`** only if every required lane is present and passing. An unexpected `skipped` state is a failure unless the mutation-only schedule excludes the entire L4 aggregate.

Add focused fixtures for: complete pass; a failed Linux shard; a failed workflows suite; a failed platform suite; missing platform artifact; malformed platform JSON; cancelled platform job; unexpected skipped platform job; and an empty evidence set. Every adverse state must prove `pass` impossible. Retain `if: always()` artifact upload so red evidence remains available for triage.

**Acceptance gate:** helper tests pass, including a fixture shaped like runs 32369346059 and 32443625177 that now resolves to `fail`, never `pass`.

### W1 — Wire All Required Lane Evidence into `full-regression.yml`

Replace the Linux-shard-only aggregation step with the W0 helper. Keep the six-shard Linux matrix, workflows suite, and macOS platform suite as explicit required lanes. Pass `needs.test.result`, `needs.workflows-suite.result`, and `needs.test-platform.result` into the helper as machine-readable inputs. Download `results-shard-*`, `results-workflows`, and `results-platform` with retained lane provenance so artifact flattening cannot obscure source lane identity.

The workflow summary must show a required-lane table, each lane’s evidence classification, and the causal reason for any non-pass. The report job must publish the same top-level status. Update release-readiness and triage documentation so any workflow/summary disagreement is an **evidence-integrity event**, immediately release-blocking.

**Acceptance gate:** a testable workflow fixture verifies all status mappings; a deliberately failed platform fixture produces `fail`; a missing platform artifact produces `timeout`; `pass` requires every required lane to pass.

### W2 — Fix LF3 Through Symmetric Canonicalization, Not macOS Case Folding

Reopen or supersede issue #9407 because the original fixture still fails in run 32443625177 after its closure. The issue must carry the latest platform artifact, head SHA, exact assertion, owner, and this milestone’s completion condition.

First, add a platform-neutral reproducer before changing production code. It must create a `real-parent` directory and an `alias-parent` symbolic link to it, set the allowed root through `alias-parent/allowed`, create an in-root `mid-link` to `real-parent/allowed/sub`, and request the non-existent tail `mid-link/deep/file.txt`. The current asymmetric implementation should reject this case; the corrected implementation must accept it. This fixture converts the likely macOS temporary-directory topology into a deterministic cross-platform contract.

Then introduce a single canonicalization boundary in `sandbox/worker-tools.rkt`. Both a candidate request and a configured allowed root must be converted to complete paths and processed through the same `resolve-longest-prefix` semantics before containment is evaluated. Require the canonical root to be an existing directory; invalid or unresolvable configured roots fail closed. Compare canonical paths with component boundaries, not raw lexical request strings.

Remove the unconditional `(system-type) = 'macosx` lowercasing branch. Apple supports case-sensitive APFS volumes, so OS identity is insufficient evidence that differently cased paths are the same resource. Do not replace the removal with a macOS-only exception. If case behavior must ever be treated specially, it requires filesystem capability detection plus a separate security review.

Preserve these non-negotiable properties: an in-root symlink with a non-existent tail is accepted; an allowed-root symlink to an outside-root target is rejected; a broken final or intermediate link is rejected; direct in-root non-existent writes remain accepted; and paths separated only by case are not conflated on an explicitly case-sensitive filesystem.

Before merge, use a failure-only diagnostic test parameter or assertion message to capture the temporary directory, lexical root, canonical root, lexical request, resolved candidate, and final containment decision in the macOS job. Do not leave unconditional filesystem-path logging in CI artifacts.

**Acceptance gate:** the ancestor-alias acceptance test passes on Linux and macOS; the alias-root escape and broken-link tests fail closed on both; `tests/test-worker-security.rkt` and the full macOS platform suite pass; #9407 is closed only with replacement run evidence.

### W3 — Candidate Evidence Run and Governance Closure

After W0–W2 merge, dispatch `full-regression.yml` manually from the v1.00.09 release-candidate commit. Download and inspect evidence from all required lanes: six Linux shard records, one workflows record, and one macOS platform record. Every artifact must be present, parseable, and passing. Confirm exact agreement between the GitHub workflow conclusion (`success`) and `run-summary.json.status` (`pass`).

Append a compact factual entry to `docs/reports/test-regression-log.md` with the run URL, head revision, all lane totals, artifact names, schema version, and agreement check. Update `docs/TDD-TEST-STRATEGY-PLAN.md` to mark L4 complete only after E1–E5 are verified. Update the #9407 disposition with the replacement evidence; do not close it merely because a proposed code change merged.

**Acceptance gate:** E1–E6 are satisfied; an independent reviewer can reproduce the conclusion from retained artifacts instead of prose assertions.

## Verification Matrix

| Layer | Command or evidence | Required result |
|---|---|---|
| Status contract | New focused aggregation-helper tests | Every adverse lane state is non-pass; complete evidence is pass. |
| Ancestor-alias contract | New public `path-allowed?` regression fixture | An allowed root named through a symlinked ancestor accepts its own in-root tail. |
| Security negatives | New/retained outside-root and broken-link tests | Every escape/broken-link path is rejected on Linux and macOS. |
| Targeted security | `raco test tests/test-worker-security.rkt` | LF3 and existing path-safety assertions pass. |
| Platform suite | `STRICT_TEST_RUNNER=1 racket scripts/run-tests.rkt --suite platform --jobs 4 --profile local` on macOS arm64 | Zero failures and zero timeouts; `results-platform` JSON is retained. |
| Linux regression lanes | Manual `full-regression.yml` dispatch | Six shard JSON records and workflows record are present and passing. |
| Cross-status integrity | Compare workflow conclusion and `run-summary.json.status` | Exact agreement; a red workflow can never have a pass summary. |
| Release governance | Updated log, triage protocol, canonical plan, and #9407 | Links, head SHA, artifacts, and stated verdict agree. |

## Sequencing, Risk, and Rollback

| Risk | Control | Rollback or containment |
|---|---|---|
| A workflow-only change masks a status edge case. | W0 makes status logic testable before workflow wiring. | Revert helper/workflow pair; preserve red artifacts and explicit release block. |
| A canonicalization change permits a symlink escape. | Pair every acceptance case with outside-root and broken-link rejection tests on Linux and macOS. | Revert resolver change; never use a silent platform skip. |
| Global lowercasing conflates names on supported case-sensitive APFS. | Remove the OS-wide case-fold branch; test case-distinct names when capability permits. | Preserve canonical component comparison; do not restore blanket lowercasing. |
| Artifact layout changes again. | Preserve lane provenance and validate schema plus lane identity, not a flattened glob alone. | Treat lane as missing evidence and return `timeout`, never `pass`. |
| Long macOS provisioning erodes feedback. | Preserve explicit budget and record setup/runtime in candidate evidence. | Classify budget exhaustion as `timeout`; triage as release-blocking. |

## References

[1]: https://github.com/coinerd/q/blob/main/docs/TDD-TEST-STRATEGY-PLAN.md "Adopted TDD and test-suite improvement plan"
[2]: https://github.com/coinerd/q/actions/runs/32443625177 "Latest scheduled full-regression run at v1.00.07"
[3]: https://github.com/coinerd/q/blob/main/docs/reports/test-regression-log.md "Full-regression decision and correction log"
[4]: https://github.com/coinerd/q/issues/9407 "macOS LF3 worker-security regression"
[5]: https://github.com/coinerd/q/blob/main/sandbox/worker-tools.rkt "Current LF3 resolver and authorization check"
[6]: https://support.apple.com/lv-lv/guide/disk-utility/dsku19ed921c/mac "Apple Disk Utility: APFS formats, including case-sensitive APFS"
[7]: https://docs.racket-lang.org/reference/Manipulating_Paths.html "Racket path semantics: resolve-path and simplify-path"
[8]: https://docs.racket-lang.org/reference/Filesystem.html "Racket filesystem predicates and symbolic links"
[9]: https://github.com/coinerd/q/blob/main/.github/workflows/full-regression.yml "Full-regression workflow to be hardened"
[10]: https://github.com/coinerd/q/blob/main/docs/operations/test-regression-triage.md "Regression triage protocol"

# CL5 W3.1 — single-case dry run (machinery validation, #9982)

Status: **W3.1 complete — machinery validated on one frozen case. This is not a measurement
result.** No efficiency claim, no safety claim, no GO/REVISE/REJECT decision, and no change to
production selection, scheduler, admission or CI policy.

- Protocol (frozen): `docs/reports/CL5-W3-DESIGN.md` (merged `431b491d`).
- Corpus (frozen, untouched): `docs/reports/CL5-W3-CORPUS-431b491d.json` (merged `68058717`).
- Dry case: **D5** from `docs/reports/CL0-SELECTIVE-VERIFICATION-CORPUS-003e425d.md`, the same
  case W2 measured end-to-end (`docs/reports/CL5-W2-E2E-89dac02e.md`).
- Measured arm: `89dac02e17683f1f964b0499ddbff851fc5e2d64`.
- Driver: `scripts/cl5/w3-dry-run.rkt`; tests: `tests/test-cl5-w3-dry-run.rkt`.
- Evidence (generated, not hand-edited): `docs/reports/CL5-W3-DRYRUN-89dac02e.json`.
- Frozen CI verdict inputs (tree SHA → verdict, from the two W2 runs):
  `docs/reports/CL5-W3-CI-CACHE-89dac02e.json`.

## Why this case and this revision

W3.1's gate is "provenance + classification must match W2's D5 behaviour". The only revision where
that comparison is a checksum rather than a new experiment is the W2 revision, whose two exact-SHA
CI runs already exist and are keyed by tree SHA. Re-running the case at `89dac02e` therefore tests
the W3 machinery against a **known answer**: the reduced set must pass, comprehensive must fail, and
the classification must be W2's `missed`.

The current revision is **not** usable for this gate yet: its comprehensive control is not green
(§ Finding F1/F2). That is recorded here, not worked around.

## Gate table

| W3.1 requirement | Evidence | Result |
|---|---|---|
| exact reproducibility: tree SHA, contract revision, selection input | `provenance` + `selection-input` + `contract-layer` blocks; committed tree re-derived from the mutation | **PASS** |
| explicit test attribution (selected and comprehensive enumerated, not inferred) | selected: shipped `--explain` plan (3 files); comprehensive: 8 CI producers' `--explain` plans (1 394 files) | **PASS** |
| classification follows the frozen W3 protocol and matches W2 D5 | W2 `missed`; W3 `unsafe-reduced`; both re-derived by `verify-record` | **PASS** |
| reliable timing: same host, no CI substitution | strategy run 9.08 s on this host; `C_comp` recorded `unavailable`, M6 `unavailable`; CI-source timing **refused** by the driver | **PASS (bounded)** |
| independent reproducibility | `racket scripts/cl5/w3-dry-run.rkt --verify <json>`; independent agent re-derivation (§ below) | **PASS** |

## Provenance chain

| Surface | Value | How it was obtained |
|---|---|---|
| arm revision | `89dac02e17683f1f964b0499ddbff851fc5e2d64` | worktree preflight: HEAD must equal it and be clean |
| revision tree | `aaf6097317a8489de7947b16f4f79b425c08547f` | `git rev-parse <rev>^{tree}` |
| declared mutation | `extensions/gsd/wave-status.rkt` only | frozen case registry (`scripts/cl5/selection-cases.rkt`) |
| measured tree | `6fe78f9b62128c71a6efe37d542faa5c431a155f` | `git write-tree` in the state the selected run executed in |
| committed tree (CI surface) | `24f225d459cf90bd24561ba4fcd76a243fa421a4` | same, after restoring the derived admission record |
| committed changes | `extensions/gsd/wave-status.rkt` | `git diff-tree revision-tree committed-tree` |
| contract layer | **absent at this revision** | `git cat-file -e <rev>:scripts/cl5/contract-escalation.rkt` fails |
| selection input | `./qt quick --changed HEAD --explain` | shipped tool, parsed; digest `154d561195728243b7a3bc776144354371bef1fe` (sha1 of the sorted list) |

The measured tree differs from the committed tree because the shipped selector refuses a stale
audited admission record: the selected run necessarily executes against a refreshed
`tests/.quick-admission.rktd`. The record states both trees and that difference; the committed tree
— the one CI sees — carries only the registered mutation. Provenance assertion
(`committed changes == declared changes`) is `true`; the driver raises before any CI lookup if it is
not.

## Explicit enumerations (no filename inference)

**Selected set (3 files, executed exactly as enumerated):**

```
tests/test-agent-memory-decoupling.rkt
tests/test-agent-session-config.rkt
tests/test-gsd-campaign-decision-pure.rkt
```

The driver executes that list file by file (`./qt full --file …`) **and** runs the deployed strategy
once (`./qt quick --changed HEAD`); it raises if the strategy's own plan is not the same set. The
shipped plan reported no escalation (`changed escalations: none`).

**Comprehensive set (1 394 unique files)** — union of the required CI producers' own plans, from the
same lane/shard specs `ci.yml` passes:

| Producer | Files | Digest (sha1, first 10) |
|---|---|---|
| `test (0)` — `--lane fast --shard-index 0 --shard-total 3` | 448 | `c9f734fdf0` |
| `test (1)` — shard 1 | 448 | `d4ae2dbb38` |
| `test (2)` — shard 2 | 448 | `c400dd7e40` |
| `test-platform` — `--lane platform` | 38 | `639f9a8d28` |
| `security` — `--lane security` | 64 | `fcd0b709fe` |
| `workflows (0)` — `--lane workflows --shard-index 0 --shard-total 2` | 19 | `d1de5c7192` |
| `workflows (1)` — shard 1 | 18 | `dd192419fa` |
| `smoke (ubuntu-latest)` — `--lane smoke` | 19 | `3141e1a4dc` |
| **union** | **1 394** | `a7ec26d2377c4d98785bce013a1473205912f3a5` |

Per-producer counts are re-checked against the plan header, and shards of one lane must be disjoint
(an overlap means the plan is not an explicit partition, and the driver refuses). Cross-lane overlap
(1 502 producer-entries → 1 394 unique files) is expected and is *not* deduplicated silently: the
record keeps every producer's digest alongside the union.

## Execution and classification

| Axis | Defect | Clean control |
|---|---|---|
| enumerated execution (3 files) | **pass** (21.9 s total, per-file) | pass (same 3 files) |
| deployed strategy (`./qt quick --changed HEAD`) | **pass** (9.08 s) | not run: zero selection on a clean tree is a failure by design |
| comprehensive (exact-SHA CI, reused by tree key) | **fail** — run `45043919793`, head `48c08c1d…`, tree `24f225d4…` | **pass** — run `45063731406`, head `642c84d9…`, tree `aaf60973…` |
| failed jobs (defect) | `lint-quality`, `test (0)`, `test (1)`, `test (2)`, `test-aggregate` | — |
| behavioural detectors (defect) | `test (0)`, `test (1)`, `test (2)`, `test-aggregate` | — |

Both CI runs are `workflow_dispatch` runs located by head SHA; every required producer is present and
`security` concluded `success` in both, so the frozen completeness rules pass. `lint-quality` also
failed in the defect run. The record keeps both views explicit: `detecting` is every failed job (what
`classify-ci-run` reports), and `behavioural-detecting` is the subset that is a behavioural detector.
`lint-quality` is derived-metadata enforcement (stale source-line metrics), so counting it as a
behavioural detection would inflate M1; omitting it from the failed-job list would silently diverge
from the frozen classifier. Neither is done.

Classification (frozen taxonomy, no decision layer at this revision → instruction `run-required`):

- **W2: `missed`** — reduced set passes, comprehensive fails.
- **W3: `unsafe-reduced`** — same shadow facts, decision layer absent so the deployed strategy is the
  reduced set.

The two labels are the same event in two vocabularies; `verify-record` re-derives both from the
recorded verdicts and raises if either label is edited.

## Timing (what is measured, what is refused)

| Quantity | Value |
|---|---|
| host | `vps2402959.fastwebserver.de`, 4 CPUs, Racket 8.10, Linux |
| `C_sel` (deployed strategy, one process) | **9.08 s** |
| per-file enumerated execution (diagnostic only) | 21.9 s — three separate process startups; **not** `C_sel` |
| `C_comp` (local `./qt full`, same host) | **unavailable** — W2 measured that a local comprehensive run does not finish within the harness bound (two attempts ≥ 90 min); it is not re-attempted here |
| `M6` | **unavailable** (needs corpus-level M3 and a matched-host `C_comp`) |
| CI timing substituted? | **no** — `timing-record` raises on `#:comprehensive-source 'ci` |
| `same-host` | `unavailable` (no comprehensive timing exists to compare against) |

So the dry run validates the *timing mechanism* (one host, one method, explicit refusal of CI
durations) and honestly reports that the 40 % cost hypothesis is still unmeasured. `./qt full`'s
1597-file local suite is not the CI surface either: the CI comprehensive surface is the producer
union above (1 394 files). Neither is claimed as `C_comp`.

## Contract-layer revision (companion, offline — not part of the dry-run classification)

At `89dac02e` the contract layer does not exist, which is why the dry-run instruction is
`run-required`. At the merged revision `4725b614` (file last changed in `4725b614`, present in
`68058717`) the same changed-file set evaluates offline as:

```
verdict: escalate
escalations: ((no-consumer-detector extensions/gsd/wave-status.rkt))
required: 23 tests, including tests/test-gsd-wave-status.rkt (the corpus detector)
instruction: run-all (without a comprehensive list supplied)
```

This is context for W3.2, not a W3.1 result: the driver **refuses** to classify a case at a revision
where the layer is present, because it does not evaluate the decision layer itself. W3.2 must supply
the instruction explicitly and record it with the contract revision. The companion also shows the
layer would have changed this case's W3 label (`correct-escalation`, since escalation means the
deployed strategy runs comprehensive) while the shadow facts — and therefore the W2 `missed`
observation — are unchanged.

## Independent reproducibility

1. The record self-verifies before it is written (any mismatch aborts the run).
2. Anyone can re-check a committed record without executing anything:

   ```
   racket scripts/cl5/w3-dry-run.rkt --verify docs/reports/CL5-W3-DRYRUN-89dac02e.json
   ```

   It re-derives: enumeration digests from the explicit file lists; the executed set and the deployed
   strategy's set against the enumeration; the provenance assertion; the W2 and W3 labels from the
   recorded verdicts; and the timing-substitution rule. Editing a label, a file list or a digest
   raises.
3. The underlying CI verdicts are public and keyed by tree SHA, not by this record:
   `gh run view 45043919793` / `45063731406` (both `workflow_dispatch`, heads `48c08c1d…` /
   `642c84d9…`).
4. The driver recomputes the committed tree from the frozen mutation on every run; a tree-SHA drift
   would change the CI cache key and the record would fall to `unavailable` instead of reusing a
   verdict for a different tree. Two independent driver invocations produced the same measured tree
   (`6fe78f9b…`) and the same committed tree (`24f225d4…`).
5. An independent agent reproduced this record read-only: recomputed both sha1 digests, re-derived the
   provenance chain with `git diff-tree` (defect tree differs from the revision tree only in
   `extensions/gsd/wave-status.rkt`), re-derived `missed`/`unsafe-reduced` from the recorded verdicts
   without reading the classification block, confirmed the contract layer is absent at the revision,
   ran `--verify`, and checked the timing field separation. **Result: PASS on all five gate rows**;
   its only caveat (the `detecting` list omitted `lint-quality` relative to `classify-ci-run`) is
   fixed in the record by the `detecting` / `behavioural-detecting` split above.

## Findings

### F1 — a setup-action network timeout is classified as a behavioural detection (methodology defect)

The `security` producer at the current revision failed **before running its lane**. Run
`45280459705` (push, head `68058717`), job `security` (`138065565620`, 22:35:33Z–22:42:51Z):

| Step | Conclusion |
|---|---|
| `Run ./.github/actions/setup-racket` | **failure** — `connect ETIMEDOUT 165.124.149.122:443` after 424 s |
| `Clear stale bytecode` | skipped |
| `Run security test suite (parallel)` | **skipped** |
| `Test results summary` | success (empty) |

`classify-ci-run` (`scripts/cl5/ci-evidence.rkt`) classifies any completed run with a failed job as
`fail`, so this run would be recorded as a **behavioural detection attributed to `security`** — and,
read as a security result, as a "security failure". Both readings are wrong: the security lane never
executed. This is exactly the class of error W3 separates `detected` from: a green/red *test*
outcome may only come from an executed lane.

Recorded honestly:

- the **code** is not red because of this job — all 17 other jobs were success or by-design skip;
- the **security check** is **not** a pass — it did not run;
- the correct classification under the protocol's own intent ("the comprehensive surface did not
  actually execute → `unavailable`") is **`unavailable`**, not `fail`.

Proposed mechanism (needs its own review before adoption; the frozen protocol and corpus are
**not** changed by this report): a required producer counts as `unavailable` when its failing step is
an infrastructure/setup step and the lane-executing step is `skipped`/absent. The rule must be
pre-registered before W3.2 and applied to all cases, not case-by-case.

This finding does **not** affect the dry case: both CI runs used above have `security` `success`.

### F2 — W3.2 cannot start at the current revision

At `68058717` the comprehensive control for the unmodified revision tree is run `45280459705`, which
is not `pass` (F1) and not `unavailable` under the current classifier (`fail`). Under the frozen
completeness rules a control that is not green makes the case `unavailable`; under F1's proposed rule
it is also `unavailable`. Either way, **no current-revision case can be classified** until a
comprehensive run exists in which every required producer actually executes. That is a CI-state
blocker, not a harness defect.

### F3 — the dry-run driver does not evaluate a present contract layer

Recorded above: it refuses instead of inventing an instruction. W3.2 must wire
`contract-escalation.rkt` (which already returns `required`/`escalations`/`instruction`) and record
its revision. Until then, a `present`-layer case has no W3 label from this driver.

### F4 — enumeration and timing must not be conflated

The first version of this driver timed the per-file enumerated execution (21.9 s) as `C_sel`; that
measures three process startups, not the deployed strategy (9.08 s). The record now separates
`selected` (enumerated, diagnostic) from `strategy` (deployed, timed) and refuses to time the former
as the latter. This matters for M6, where `C_sel` must be the strategy actually deployed.

## What W3.1 establishes and what it does not

**Establishes:** one frozen case can be run end-to-end with exact tree/revision/input provenance,
explicit selected and comprehensive enumerations, the frozen taxonomy, honest single-host timing, and
a record that an independent agent can re-derive.

**Does not establish:** any miss rate, any cost saving, any 40 % claim, any parity or safety result,
and no authorization for W3.2. W3.2 stays gated on (a) reviewer acceptance of this dry run, (b) a
comprehensive control that actually executes at the measured revision, and (c) a decision on F1's
infrastructure-failure rule before the first W3.2 case is classified.

# CL5 W3 F4 — D5 replay under the contract-aware decision layer

Status: **delivered, awaiting independent review.** Evaluation-only. The protocol
(`CL5-W3-F4-PROTOCOL.md`) was fixed before the recorded run; the one amendment (the size of the
executed set) is documented there and was made before any record existed.

## Result: the escalated set contains the historical detectors and they fail on the defect tree

| Claim level | Outcome |
|---|---|
| Decision | **escalate → `run-comprehensive`** (counterfactual: layer from `c98997dd`, data from `89dac02e`) |
| Escalation reason | `no-consumer-detector` — `extensions/gsd/projection-kernel.rkt` is a declared consumer with no `@covers` detector |
| Required set | 23 tests, including `tests/test-gsd-wave-status.rkt` (the contract's own owner test) |
| Selected set | 1 391 = required ∪ comprehensive (comprehensive re-enumerated at `89dac02e`: 1 389) |
| Historical selector | `./qt quick --changed HEAD` → 3 tests, **pass** on the defect tree (the W3.1 `missed`) |
| Historical CI evidence | run `45043919793` (tree `24f225d4…`) failed 11 test files |
| Required ∩ historical failures | **3 files**: `test-gsd-wave-status.rkt`, `test-gsd-wave-docs.rkt`, `test-gsd-campaign-state.rkt` |
| Behavioural replay (defect / control) | all three: **fail** on the defect tree, **pass** on the control tree |
| **Classification** | **prevented — attributed** |

"Prevented" means: the contract-aware instruction runs `required ∪ comprehensive`; three of those
required files fail on this exact defect tree and pass on the unchanged revision tree, so the defect
could not have been reported as a green run. It is *attributed* because those three are the files the
historical CI run actually failed. This is one case at one revision; it does not generalise, and it
says nothing about cost or the 40 % hypothesis.

## Reproduction and provenance

- Record: `CL5-W3-F4-REPLAY-89dac02e.json`; verifier: `racket scripts/cl5/w3-dry-run.rkt
  --verify-replay docs/reports/CL5-W3-F4-REPLAY-89dac02e.json` (re-derives the decision, the trees,
  the executed set, the overlap, the attribution and the claim from the record).
- Trees are exact: revision tree `aaf60973…`, reconstructed defect tree `24f225d4…` — the driver
  **refuses** to run if either differs from the frozen W3.1 values.
- The contract layer is a separate worktree pinned at `c98997dd…` (F3 merge). Because the evaluated
  revision predates the layer, the record is labelled `state = counterfactual`,
  `mode = counterfactual`, `counterfactual? = #t`, `contract-revision ≠ evaluated-revision`, and the
  layer module's blob SHA is recorded. It cannot be read as a historical execution.
- The frozen W3.1 record, corpus and CI cache were not edited; W3.1 still verifies byte-identically.

## Comprehensive set: re-enumerated, not inherited

The 1 396-file union measured at `4725b614` was **not** used. The comprehensive set was enumerated at
`89dac02e` with the F2 registry (fast shards 0–2, platform, security, workflows 0–1, smoke as the
workflow-declared command):

| Producer | Files |
|---|---|
| test (0) / (1) / (2) | 448 / 448 / 448 |
| test-platform | 38 |
| security | 64 |
| workflows (0) / (1) | 19 / 18 |
| smoke | 1 (declared command) |
| **union** | **1 389** (digest `17dde18f…`) |

This differs from W3.1's 1 394 because W3.1 enumerated smoke as `./qt --lane smoke` (19 inferred
files) while CI runs one declared command file: 5 of those 18 phantom files were not covered by any
other lane, so the union drops by 5 (13 were already selected elsewhere). The driver now shares the
single F2 producer registry, so future W3 runs cannot repeat the overcount.

**Fidelity note:** two of the 23 required tests (`tests/test-gsd-auto-review-loop.rkt`,
`tests/test-gsd-lost-author-finish.rkt`) exist but are not members of the CI comprehensive set at this
revision. An escalated instruction therefore runs 2 tests beyond what CI's producers execute. This
does not affect the attribution (the three detectors are inside the CI surface), but it is recorded
rather than smoothed over.

## Evidence detail

Defect-tree / control-tree execution of the three attributed files (one `./qt full --file` per file):

| File | Defect exit | Control exit | Defect s | Control s |
|---|---|---|---|---|
| `tests/test-gsd-wave-status.rkt` | 1 (fail) | 0 (pass) | 6.5 | 6.7 |
| `tests/test-gsd-wave-docs.rkt` | 1 (fail) | 0 (pass) | 7.6 | 8.3 |
| `tests/test-gsd-campaign-state.rkt` | 1 (fail) | 0 (pass) | 7.1 | 7.7 |

The historical failure list also contains `test-gsd-command-handlers-unit.rkt`,
`test-gsd-plan-types.rkt` and six `test-qt*` files that are not in the contract's required set (no
`@covers` edge reaches them); they are reported, not claimed.

## Limits

- One case, one revision, one defect: no population-level safety claim, no cost/performance claim,
  no 40 % claim.
- The claim rests on the contract layer's declared consumer set being sound for this case (the
  registry's `wave-status` entry) and on the coverage manifest at `89dac02e`; a wrong declaration
  would make the escalation look better than it is. F3's fail-closed checks reduce, but do not
  remove, that dependency.
- The executed evidence covers the required set (the historical detectors), not the full 1 391-test
  selected set; executing the superset would also fail on these three files, which is the logical
  basis of the claim.
- The replay does not exercise a production selector: none of this changes `./qt`, CI, admission or
  scheduling.

## Non-claims

No W3.2/W3.3 authorisation, no generalisation to other defects, no claim that contract-aware
selection is *always* safer, and no retroactive reinterpretation of W3.1 or the frozen corpus.

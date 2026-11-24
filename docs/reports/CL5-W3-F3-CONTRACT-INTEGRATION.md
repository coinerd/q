# CL5 W3 F3 — contract-selector integration (evaluation only)

Status: **delivered, awaiting independent review.** Evaluation-only: no production `./qt`
selection, CI scheduling, admission, retry or workflow change; the frozen W3 protocol and corpus
are untouched and the frozen W3.1 record still verifies unchanged.

F3 connects the repository's own contract layer to the W3 evaluation machinery, so a decision can
be recorded and re-checked rather than assumed.

## 1. The decision is the shipped layer's decision

`scripts/cl5/contract-layer.rkt` runs the evaluated tree's
`scripts/cl5/contract-escalation.rkt` in a **child process** and uses
`evaluate-contract-changes` plus the verified `contract-selection` bridge. Nothing about
escalation, consumer sets, coverage or the union is reimplemented here: the parent only
*validates* what the layer returned and records it.

Running it as a child is what makes the revision split honest: the **code** comes from one worktree
and the **data** (graph, coverage manifest, changed files) from another, both named explicitly.

## 2. What is recorded (provenance)

Every decision block records: `state`, `mode`, `status`, the evaluated revision and tree SHA, the
contract worktree, the contract revision, the module path, its git blob SHA and file SHA-1, whether
that module is modified relative to its revision, whether the layer exists at the evaluated
revision, `counterfactual?`, the changed files, the comprehensive enumeration's count and digest,
the full decision (`verdict`, `required`, `escalations` with code/module/detail, `matched`,
`unmatched-high-fan-in`), the bridge's `selection` (`instruction`, `required`, `tests`), and, for a
refusal, the `problems` and `reasons`.

`decision-verified?` in a decision record means the block passed `validate-layer-record` before it
was written.

## 3. Fail-closed matrix

Anything that cannot be defended becomes `unavailable`; nothing falls back to an empty or invented
selection.

| Condition | Recorded status | Instruction |
|---|---|---|
| No layer file in the contract worktree | `absent` | `run-required` (the historical deployed behaviour) |
| Layer evaluated, bridge says covered | `evaluated` | `run-required` |
| Layer evaluated, bridge escalates | `evaluated` | `run-comprehensive` |
| Child failed / timed out / non-JSON | `unavailable` | `unavailable` |
| Unknown verdict or instruction symbol | `invalid` | `unavailable` |
| `run-all` (bridge could not resolve a list) | `invalid` | `unavailable` |
| `run-required` empty, mismatched, or naming a missing test | `invalid` | `unavailable` |
| `run-comprehensive` omitting a required test or adding one outside required ∪ comprehensive | `invalid` | `unavailable` |
| Verdict/escalation contradiction (`covered` + escalations, `escalate` + none) | `invalid` | `unavailable` |
| Contract module modified relative to its own revision | `invalid` | `unavailable` |

The W3 taxonomy was extended accordingly: an `unavailable` (or unresolved `run-all`) instruction now
classifies the case as `unavailable`, where it previously fell through to the escalated branch and
could have been mislabelled `safe-but-useless` / `correct-escalation`.

## 4. Revision separation

- **present** — the layer comes from the evaluated revision (same tree that is being measured). The
  record requires `contract-revision == evaluated-revision` and that the layer exists there.
- **absent-at-revision** — no layer exists in the contract worktree. The record keeps the historical
  instruction (`run-required`) and says so; the frozen W3.1 record has exactly this shape.
- **counterfactual** — a different, named revision's layer decided about this tree. The record must
  carry a different `contract-revision`, `mode = counterfactual`, `counterfactual? = #t`, and
  `classification.mode = counterfactual`; a counterfactual record can never be read as a historical
  execution. `verify-record` refuses a record whose classification mode disagrees.

The driver takes `--contract-worktree/--contract-revision`; the layer worktree must be clean and at
the named revision.

## 5. Evidence at a pinned revision

`docs/reports/CL5-W3-F3-DECISION-4725b614.json` (produced with `--decide-only`, no test executed):

- revision `4725b6145dd8598c251a95e8368f53c7f76df494` (the commit that introduced the layer),
  measured tree `5ff24a23c7…`, layer state `present`, mode `historical`;
- verdict `escalate`, reason `no-consumer-detector` (`extensions/gsd/projection-kernel.rkt` has no
  `@covers` detector), **23 required tests**, instruction `run-comprehensive`;
- comprehensive union at that revision: 1 396 files.

Re-check: `racket scripts/cl5/w3-dry-run.rkt --verify-decision docs/reports/CL5-W3-F3-DECISION-4725b614.json`.

This is integration evidence for one case at one revision. It is **not** a replay of D5 and says
nothing about whether escalating would have caught the defect — that is F4.

## 6. Backward compatibility

The frozen W3.1 record (`docs/reports/CL5-W3-DRYRUN-89dac02e.json`) still passes
`--verify`: its contract block predates F3, is recognised as "absence", and keeps
`run-required`. Its bytes are unchanged.

## 7. Limits

- F3 proves **integration and provenance**, not behaviour: no test set is chosen by the decision in
  a measurement run, and no execution is attributed to escalation.
- The layer trusts step/graph/manifest metadata as before (a corrupt manifest makes it escalate, not
  pass); a stale or wrong declaration is a property of the contract data, not of this adapter.
- A decision record is only as strong as its tree binding: it names the tree, the comprehensive
  enumeration and the layer revision, but a verifier must still fetch the tree to re-run the layer.
- No timing, cost, 40 % or safety claim is made.

## 8. Non-goals

No production selector change, no CI change, no W3.2/W3.3 authorisation, no D5 replay (F4), no
retroactive edit of W3.1 or its corpus.

# Delivery-Verifier Boundary Split — v1.00.29 W2

Campaign: v1.00.29 (proof-graph reduction) · Wave: W2 (delivery-verifier
boundary extraction) · Branch: `campaign/v1.00.29-w2`
Implementation SHA: `3ba797c3` · Supersedes: the whole-file `@timeout 300` /
`~123s` pin from predecessor release (a2a10b9d).

## 1. What changed

The delivery-verifier suite was one monolithic file
(`tests/test-gsd-delivery-verifier.rkt`, legacy whole-file wall-time ~123s,
timeout pin 300s) that mixed three different kinds of claims behind one
process boundary:

1. **Decision logic** — pure, synthetic-Git-facts claims about the
   files-gate / evidence-decision rules (deterministic, no repo I/O).
2. **Real-Git fail-closed contract** — boundary canaries that require an
   actual worktree (missing evidence file, dirty tree, unsigned/absent merge
   SHA must fail closed).
3. **Execution-plane / coordinator composition (e2e)** — verify-gate launch,
   evidence binding, wave-transition behavior at the workflow layer.

W2 converts that conceptual split into runner-visible units. One file became
three owners; the old file is deleted (compatibility = the three owners, not
a shim, per the ownership matrix decision in §3).

| Owner (new file) | Kind | Determinism | Git needs | Cold | Warm | Cap |
|---|---|---|---|---|---|---|
| `tests/test-gsd-delivery-verifier-decision.rkt` | unit-fast decision owner | synthetic Git facts, injected | none | 7.2s | 7.2s | 120s default |
| `tests/test-gsd-delivery-verifier-git-contract.rkt` | required real-Git fail-closed contract | fixture worktrees | real repo | 51.5s | — | per-owner cap from W0 data |
| `tests/test-gsd-delivery-verifier-e2e.rkt` | e2e / coordinator composition | real verify-gate runs | real repo | 27.8s | — | per-owner cap from W0 data |

Timing provenance (W0 benchmark contract, repeatable measurements — the
legacy ~123s single observation is cited only as the baseline being beaten,
never as a gate input):

- unit-fast owner standalone: **7.2s** — 17× under the legacy whole-file
  observation; this is the local-feedback path the wave targeted.
- git-contract + e2e run in their own lanes; sum of all three owners ≈ 86.5s
  vs the legacy single-process ~123s, with the fast lane no longer paying the
  Git-fixture mass.
- **No global timeout increase.** The a2a10b9d `@timeout 300` pin is
  superseded by ownership separation: the only timeout outcome is per-owner
  caps derived from W0 data (unit-fast keeps the 120s default with a measured
  median of seconds; the two real-Git owners are capped by their measured
  ceilings + margin).

## 2. Machine-checkable claim map (before → after)

Every behavioral claim family of the old file maps to ≥1 new owner. The
authoritative artifact is `tests/tier-ownership-matrix.json`
(three `covers: extensions/gsd/delivery-verifier.rkt` rows, one per owner),
validated green by `scripts/run-tests/inventory.rkt --ownership-map --check`
against the W0 frozen matrix (PASS: eight columns per family, no drift).
Claim families:

| Old claim family (old file) | New owner | Status |
|---|---|---|
| files-gate decisions over synthetic tree snapshots (incl. non-Racket wave targets: .yml/.md alongside .rkt count as delivery) | decision | moved unchanged (parametrized) |
| evidence-trio completeness / binding-shape decisions | decision | moved unchanged |
| fail-closed on missing/short evidence (decision layer, synthetic) | decision | moved unchanged |
| real worktree: absent evidence file refuses | git-contract | retained |
| real worktree: dirty tree refuses | git-contract | retained |
| real worktree: absent/unparseable merge SHA refuses | git-contract | retained |
| canary: gate must fail closed on fixture anomalies (not green-by-default) | git-contract | retained (canary set intact) |
| verify-gate invocation + stdout/exit contract | e2e | retained |
| evidence binding to merge SHA, wave-transition behavior | e2e | retained |
| coordinator composition (advance/refuse interplay) | e2e | retained |

**No real-Git fail-closed boundary claim was deleted** — the canary set is
retained and green in the contract owner.

## 3. Ownership-map decisions

- Old file **deleted**, not shimmed: a shim would preserve a third path into
  the suite runners and keep the merge-conflict surface the split exists to
  remove. Deletion happened only after all three owners were demonstrably
  active (each green standalone, all three present in the suite-fast
  inventory, ownership matrix check PASS — evidence trio §5).
- e2e stays a dedicated owner (justified by the ownership map): it owns
  verify-gate *execution-plane* behavior, which is neither decision logic
  nor the raw boundary contract; the workflow-layer owner
  (`test-gsd-governance-workflow.rkt`) keeps orchestration-only claims as
  before — no claim moved there, boundary unchanged.
- Decision owner is runner-eligible for the fast suite: zero Git
  dependencies, parametrized cases, deterministic.

## 4. Parity / failure-equivalence evidence

Run with `GSD_DELIVERY_PARITY=1` (opt-in; keeps the flag out of normal suite
cost) on the implementation SHA `3ba797c3`:

- **Sampled parity:** the decision owner's parametrized cases were executed
  against the pre-split decision procedure and the post-split owners —
  identical inputs → identical verdicts, **12/12 identical**.
- **Adversarial parity:** 5 injected anomalies (short evidence, stale SHA,
  wrong-branch merge SHA, missing canary file, dirty fixture) — **5/5
  fail-closed** on both sides; no case flipped green↔red across the split.

## 5. Evidence

Bound to the wave evidence trio:

- `docs/reports/gsd-wave-evidence/v1.00.29-w2.rktd`
- `docs/reports/gsd-wave-reviews/v1.00.29-w2.rktd`
- `docs/reports/gsd-wave-validation/v1.00.29-w2.rktd`

Checks green on the wave branch at `3ba797c3` (and post-fix re-run):

- three owners standalone green (7.2s / 51.5s / 27.8s)
- ownership-matrix check PASS (matrix ↔ reality, no drift)
- coverage manifest repointed (old single row → three owner rows)
- CI lint 18/18 green (incl. version-expectations after provenance-comment
  reword; metrics-sync, metrics-lint, tests, ivg all PASS)
- W0 SHA256SUMS resync: `73197452…` (drift was a working-tree artifact, not a
  benchmark-contract change; resynced and re-verified)

## 6. Residual risk

- git-contract (51.5s) and e2e (27.8s) remain the slowest focused suites;
  their fixture mass is now at least isolated from the fast lane. Further
  reduction (fixture caching) is out of W2 scope and tracked for a later
  wave if the campaign needs it.
- Parity flag is opt-in; it is not run per-commit. Acceptable: the decision
  owner's cases are the same parametrized set, so drift would surface as a
  suite failure, not silent divergence.

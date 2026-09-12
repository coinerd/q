# W8: Selector Shadow Cohort — CLOSED-SKIPPED (governance gate unmet)

Status: **CLOSED-SKIPPED** (first-class outcome per the wave contract)
Date: 2026-09-12
Campaign: v1.00.29 — Proof-Graph Reduction (plan `6ae3ac5125fd82a7f22e5d7250c5fb25c506441ec928cbf148a85e1edb5c940f`)

## Skip decision

The W8 wave contract is conditional: *"If and only if W7 governance approved
the amendment"* may the selector pilot run. The hard gate is
**W7 strategy amendment reviewed and merged** (W7 wave doc: "The strategy
amendment gate decides W8: no pilot until the amendment is reviewed and
merged").

State of the gate at W8 entry (2026-09-12):

- W7 delivered `docs/reports/SELECTOR-CI-GOVERNANCE-v1.00.29.md` with the
  amendment **PROPOSED** (§8) and recorded the gate state verbatim:
  **"OPEN, PENDING AMENDMENT REVIEW"** (§9; evidence trio).
- No independent review of the amendment has occurred, and the canonical
  strategy (`docs/TDD-TEST-STRATEGY-PLAN.md`) is **unchanged** — the proposal
  was deliberately not merged by the campaign itself.
- Self-approval of the campaign's own strategy amendment inside the same
  autonomous run would violate the governance discipline this campaign
  enforces (§11.1 no target massaging; amendments need independent review).

**Decision: W8 is CLOSED-SKIPPED.** The pilot does not run. No selector
workflow file is landed (`.github/workflows/**` remains free of any
test-impact execution job — the W7 hard prohibition stands). The amendment
remains open for review in a future campaign; if approved there, a future W8
equivalent may run per `SELECTOR-CI-GOVERNANCE-v1.00.29.md`.

## Consequences (recorded per wave contract)

1. **W9 scope note (binding):** W9 proceeds on **non-selector proof reuse
   only** — i.e. the `q.proof-bundle/1` producer/validator mechanism from W5
   (§9 threat-model-validated, fail-closed) and same-SHA reuse provenance.
   No change-impact selection may narrow any required proof, cohort, or gate
   anywhere in v1.00.29.
2. **No cohort state exists**: no `artifacts/proof-graph/v1.00.29-w8/`
   directory, no eligibility ledger, no omission records, no
   `selector-shadow.yml`. The promotion target ("zero confirmed relevant
   omissions over max(20 PRs, 4 weeks)") remains **unclaimed and unstarted**.
3. **Security/platform/cross-version/release claims** remain outside any
   selector-removal consideration — restated as a design constraint for any
   future pilot (per the wave contract and governance doc §4).
4. **Scope note for W10/W11:** the final cohort rebalance and the bake must
   not reference selector-based reductions. All reduction claims in this
   campaign derive from duplicate-proof reuse (W5/W9) and prepared-env
   verified-restore savings (W4/W6).

## Verification (skip path)

Per the wave contract, the skip path is "trivial doc-only PR still passes
all gates":

- `racket scripts/run-tests.rkt --suite fast` green (unsharded, coordinator
  lane — see wave validation record)
- `racket scripts/metrics.rkt --lint` green (docs-only change; counts
  unchanged)
- `grep -rin "test-impact\|impact-select" .github/workflows/` → no matches
  (prohibition still mechanical; recorded in the evidence file)

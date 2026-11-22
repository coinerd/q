# CL5 W2 — remediation design: ownership, contract escalation, fail-closed hybrid

Design-only document at the review boundary. No production selector, admission, CI or scheduling
change is proposed for merge here. Issue #9982. Prototype: `scripts/cl5/selection-guard.rkt`
(isolated, not wired anywhere).

## Problem statement (measured, not hypothetical)

`./qt quick` selects from a 23-owner audited universe while the repository declares 226 `@covers`
tests over 202 modules. The selector never checks that a changed module's declared detectors are
*selectable*, and its `unmapped-source` escalation inspects declared covers instead of selectable
ones. Result: 176 modules can change, run no detector for themselves, and report green (D5 is one of
them; see `CL5-W2-D5-ROOT-CAUSE.md` and `CL5-W2-COVERAGE-GAP-AUDIT.md`).

The remediation must keep `quick` fast for its intended use (a reproduction aid for pure unit
decisions), and must make a green *honest*: either a real detector ran, or the tool refuses to claim.

## Option A — Ownership repair

**Mechanism.** Every verified `@covers` relationship for an eligible test is added to the audited
owner table, with a drift check (owner table ⊇ declared covers for admitted-eligible tests). Eligibility
is the existing admission criteria: `@boundary unit`, `@speed fast`, capability scan clean over the
reviewed closure.

**Effect on D5.** `tests/test-gsd-wave-status.rkt` (2-file closure, zero scan hits, `@boundary unit`,
`@speed fast`) becomes an owner; the change selects it and the defect is caught — *if* the test
actually exercises the rejected-status path.

**Evaluation.**

| Dimension | Assessment |
|---|---|
| correctness | Good for modules whose pure-unit tests are eligible; does nothing for integration-level tests |
| maintainability | The table grows move-only: 226 candidates vs 23 today; each addition is a reviewed diff |
| agent cognitive load | High: the table becomes a second, large mapping to keep in sync |
| false-green risk | Reduced but not removed: eligibility is capability-based, not coverage-completeness-based; a module test can be admitted and still not detect the change |
| unnecessary testing | Rises: `quick` would run far more pure-unit tests per change |
| execution cost | Rises proportionally; the fast loop target (warm p90 ≤ 90 s) is at risk |
| complexity | Low to implement, high to sustain |

Blocking issue: it cannot repair integration-level detectors (the majority of the 176 gaps) without
abandoning the pure-unit admission contract that makes `quick` trustworthy and fast.

## Option B — Contract-aware escalation

**Mechanism.** Declare, per shared behavioural contract (vocabulary owners, policy/authority modules,
event/protocol surfaces), the consumer tests and boundary tests that must run when that contract
module changes; widen the selection to them, or escalate to the applicable comprehensive lane.

**Effect on D5.** `wave-status.rkt` is declared a vocabulary-owner contract with a consumer set
(the corpus documents exactly this), so the change escalates to the consumers/covering lane instead
of selecting three files.

**Evaluation.**

| Dimension | Assessment |
|---|---|
| correctness | Highest for shared contracts: it targets the failure mode (fan-in semantics) rather than file-level attribution |
| maintainability | Requires a curated contract map; drift is silent unless validated against the graph |
| agent cognitive load | Moderate: one concept ("contract") the agent already reasons about |
| false-green risk | Low if validation is fail-closed (an unmapped contract escalates) |
| unnecessary testing | Bounded to affected consumers; better than A for fan-in modules |
| execution cost | Higher per change, but only for contract modules |
| complexity | Medium: needs contract metadata + validation + a lane definition |

Blocking issue: it is a new behavioural taxonomy. Without an offline validator it becomes another
hand-maintained list, i.e. the same failure mode as the owner table, one level up.

## Option C — Fail-closed hybrid (recommended)

**Mechanism.** Keep the impact/ownership selection as the *cheap* path, and make the *claim* fail
closed:

1. compute the selected set from audited owners (today's rule);
2. for every changed module, verify that at least one **selectable** detector exists
   (`declared ∩ owners ≠ ∅`), and that the admission record is fresh;
3. if not — or if the selection is empty — do not report green: report `escalate` with the named
   uncovered modules and unadmitted detectors, and route to the comprehensive lane;
4. audited owners without `@covers` stop being unconditionally included; they run only when their
   module is in the change's impact cone (noise removal is optional but cheap).

This is implemented as a pure decision function (`guard-decision`) over the same data the selector
already loads; the prototype in `scripts/cl5/selection-guard.rkt` is 1 screen of rules and is not
wired into `./qt`.

**Effect on D5.** `wave-status.rkt` → `escalate` with
`unadmitted-cover: tests/test-gsd-wave-status.rkt, tests/test-audit-v09945-w3-gsd.rkt` (verified by
test). A clean change to an audited module (`campaign-decision.rkt`) → `selected`, no escalation.

**Evaluation.**

| Dimension | Assessment |
|---|---|
| correctness | Best available without an execution oracle: a green now implies a selectable detector existed; otherwise the tool refuses |
| maintainability | One rule, one file, no new taxonomy; drift in the owner table shows up as escalations, not as silent greens |
| agent cognitive load | Low: "green means a detector ran; otherwise you get a lane escalation" |
| false-green risk | Structurally removed for the uncovered case; residual risk only from detectors that run but do not detect |
| unnecessary testing | Only what today's selector already runs, plus escalations where coverage is genuinely missing |
| execution cost | Unchanged for covered changes; escalated changes pay the comprehensive lane (correctly) |
| complexity | Lowest: pure function + validation, testable offline |

**Recommendation: C as the decision layer, A as an optional cost optimisation, B as a follow-up for
fan-in contracts.**

Rationale: C converts the observed failure (silent green with no detector) into an explicit,
testable refusal without touching the fast path, without a new taxonomy, and without needing CI.
A can then be applied incrementally to *covered* modules to reduce escalations where an eligible
pure-unit detector exists (start with the 22 risk≥5 gaps and the two retired-cover modules). B is the
right long-term answer for shared contracts, but it needs its own validated contract map and should
not gate the safety fix.

## Sequencing (proposed, bounded)

1. **Milestone 1 — visible gaps (offline).** Publish the audit + drift check as a read-only report
   generator with tests (this PR). No behaviour change.
2. **Milestone 2 — review: decision layer.** Adopt the guard as an *advisory* signal in the CL5
   harness only (still not in `./qt`), measure how often it escalates on the frozen corpus.
3. **Milestone 3 — ownership repair pilot.** Add the eligible pure-unit detectors for the 22 risk≥5
   confirmed gaps, one reviewed diff with drift validation; require the audit to drop those rows.
4. **Milestone 4 — contract escalation design.** Only after 1–3: define contract ownership for the
   fan-in modules (B), with a validator and a lane mapping.
5. **Milestone 5 — policy decision.** Resume CL5 W2 only when a green cannot be emitted without a
   selectable detector, and matched-host cost evidence is available.

## Preservation and non-goals

- Frozen CL0 corpus and thresholds unchanged.
- Required CI contexts, branch protections, production selector, admission/security enforcement and
  the W1 harness unchanged.
- No new CI dispatches while GitHub Actions is under the billing restriction; the 40 % target stays
  unproven and is not claimed.
- The guard prototype is isolated; enabling it in `./qt` requires its own reviewed PR with the
  audit's row count as the acceptance signal.

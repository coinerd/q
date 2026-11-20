# Subsystem map — GSD campaign execution & delivery

Derived facts: `../subsystem-manifest.rktd` (subsystem `extensions`; machine-checked effects in
`gsd-inventory`). Internal architecture: [gsd-architecture.md](../../gsd-architecture.md).
Lifecycle diagram: [gsd-guide.md](../../gsd-guide.md#campaign-lifecycle).

## Purpose & entry points

- Extension entry: `extensions/gsd-planning.rkt`; command surface `extensions/gsd/command-parser.rkt` → `extensions/gsd/command-handlers.rkt`; dispatch/transactions `extensions/gsd/core.rkt`.
- `/go` interpreter: `extensions/gsd/go-orchestrator.rkt` — the ONLY effect interpreter (lease, fence, commit; one decision per pass).

## Capability ownership (single-owner table)

| Responsibility | Owner |
|---|---|
| Pure `/go` routing decision | `extensions/gsd/campaign-decision.rkt` (`decide-campaign-action`; typed kind/route/code; message is operator-only, never parsed) |
| Durable campaign state write | `extensions/gsd/campaign-state.rkt` + `extensions/gsd/campaign-repository.rkt` (fenced, journaled) — only writers |
| OS lease | `extensions/gsd/campaign-lease.rkt` |
| PLAN/wave projections | `extensions/gsd/projection-kernel.rkt` + `extensions/gsd/projection-effects.rkt` |
| Wave-status vocabulary | `extensions/gsd/wave-status.rkt` (unknown status = typed rejection, never silent `pending`) |
| Wave-doc contract | `extensions/gsd/wave-docs.rkt` (lint + template from one contract) |
| Verify grammar | `extensions/gsd/verify-command-grammar.rkt` (+ `extensions/gsd/verify-command-format.rkt` layout) |
| Pure delivery-stop classification | `extensions/gsd/delivery-outcome.rkt` |
| Delivery evidence sink | `extensions/gsd/delivery-journal.rkt` (87 direct dependents) + `extensions/gsd/delivery-receipt.rkt` |
| Delivery routing/coordination | `extensions/gsd/delivery-coordinator.rkt`, `extensions/gsd/delivery-finalize.rkt`, `extensions/gsd/delivery-verifier.rkt`, `extensions/gsd/delivery-handoff.rkt`, `extensions/gsd/delivery-publication.rkt`; recovery via `extensions/gsd/delivery-recovery.rkt` + `extensions/gsd/reconciliation-checkpoint.rkt` |
| Planning-artifact ownership predicate | `extensions/gsd/planning-artifact-ownership.rkt` (consumed by every destructive sweep) |

## Behavioral contracts established by tests

- Decision matrix (kind/route/code precedence; stale-readback and cancellation precede pending;
  finalize requires authenticated SHA; deferred is disjoint): `tests/test-gsd-campaign-decision.rkt`
  (decision matrix through the interpreter's view types) + `tests/test-gsd-campaign-decision-pure.rkt`
  (purity isolation; fixture decision for all-done waves = `complete` / `none` / `campaign-complete`).
- Interpreter: `tests/test-gsd-go-orchestrator.rkt` (81 cases); wave-status vocabulary:
  `tests/test-gsd-wave-status.rkt`, `tests/test-go-orchestrator-wave-status.rkt`.
- Delivery stop classification: `tests/test-gsd-delivery-stop-decision.rkt`
  (`@covers extensions/gsd/delivery-outcome.rkt`).
- Composition: `tests/test-gsd-governance-workflow.rkt`, recovery:
  `tests/test-gsd-delivery-recovery.rkt`, `tests/test-gsd-reconciliation-checkpoint.rkt`.

## Provider / consumer relationships

- **Providers (views in):** `extensions/gsd/delivery-handoff.rkt` produces the authenticated delivery view;
  `extensions/gsd/delivery-finalize.rkt` produces the lane view; the decision trusts producer authenticity
  (awaiting SHAs are binding facts, not re-verified by the decision).
- **Consumers (decisions out):** `extensions/gsd/go-orchestrator.rkt` dispatches on `campaign-decision-kind`;
  `extensions/gsd/delivery-finalize.rkt`/`extensions/gsd/delivery-handoff.rkt` consume typed decisions downstream.
- **Boundary rule:** decisions never inspect human-readable text; changing a view struct field
  moves semantics without breaking compilation — provider + consumer conformance tests are
  required for view changes.

## Boundaries, unresolved dependencies, escalation

- Effects per module are machine-checked against the
  [responsibility inventory](../../../extensions/gsd/responsibility-inventory.rkt)
  (`tests/test-gsd-responsibility-inventory.rkt`).
- Static graph is clean here; the unresolved areas of this subsystem live in the
  [supplemental-evidence map](gsd-supplemental-evidence.md) (dynamic requires).
- **Broader verification required when:** durable-state schema/writer changes
  (campaign-repository), lease/fencing changes, view-struct field changes (provider + consumer
  + composition), or anything touching refusal ordering / cancellation semantics.
- Everyday loop: edit → admission refresh if a closure file changed →
  `./qt quick tests/test-gsd-campaign-decision-pure.rkt` → impact via `./qt quick --changed`.

# Subsystem map — GSD supplemental evidence (CL4 pilot candidate)

Derived facts: `../subsystem-manifest.rktd` (subsystem `extensions`, `gsd-inventory` rows;
`dynamic-require-modules` marks). Parent workflow context: [gsd-campaign.md](gsd-campaign.md).

## Purpose & entry points

The governed supplemental T/K/S evidence lane: discovering, authoring, checkpointing,
disposition and publication of supplemental evidence during campaign execution. Reached only
through campaign routing (the `supplemental-*` resume routes of
`extensions/gsd/campaign-decision.rkt` interpreted by `extensions/gsd/go-orchestrator.rkt`).

## Capability ownership

| Responsibility | Owner |
|---|---|
| Coordination (observe/decide/coordinate today — the CL4 pilot concentration) | `extensions/gsd/supplemental-evidence-controller.rkt` (1826 LOC, fan-in 29, **widest effect set in the inventory**: fs-write/read/rename/delete, mkdir, sha256, git, parameterize, path-ops, dynamic-require) |
| Evidence authority | `extensions/gsd/supplemental-evidence-authority.rkt` — adapters consume decisions; they never invent permission |
| Evidence journal | `extensions/gsd/supplemental-evidence-journal.rkt` (fan-in 38) — durable evidence writes |
| Repair (format/pure part) | `extensions/gsd/evidence-repair-format.rkt` (pure), controller `extensions/gsd/evidence-repair-controller.rkt`, ledger `extensions/gsd/evidence-repair-ledger.rkt` |
| Discovery / probes | `extensions/gsd/evidence-discovery.rkt`, `extensions/gsd/evidence-probe.rkt`, `extensions/gsd/evidence-readiness.rkt` |
| Disposition claim | `extensions/gsd/supplemental-disposition-claim.rkt` + disposition stores |
| Operator-facing text | owned by the respective review/proof modules, never by the controller |

## Behavioral contracts established by tests

34 `tests/test-gsd-supplemental-*` and related owners pin the governed flows, including
entry (`tests/test-gsd-supplemental-campaign-entry.rkt`), advance failure
(`tests/test-gsd-supplemental-advance-failure.rkt`), disposition store/campaign
(`tests/test-gsd-supplemental-disposition-store.rkt`,
`tests/test-gsd-supplemental-disposition-campaign.rkt`), preserved integration
(`tests/test-gsd-supplemental-preserved-integration.rkt`), publication
(`tests/test-gsd-supplemental-evidence-publication.rkt`), terminal observation
(`tests/test-gsd-supplemental-terminal-observation.rkt`).

## Provider / consumer relationships

- Providers: campaign decision (`supplemental-author`, `supplemental-checkpoint`,
  `supplemental-retained` routes), journal facts, authority facts.
- Consumers: `extensions/gsd/go-orchestrator.rkt` (route execution), `extensions/gsd/delivery-verifier.rkt`
  (evidence counting), review/proof modules.
- `tests/helpers/supplemental-go-fixture.rkt` and
  `tests/helpers/supplemental-promotion-fixture.rkt` are the shared composition fixtures.

## Unresolved dependencies (marked, not inferred)

**8 `dynamic-require` call sites** in `extensions/gsd/supplemental-evidence-controller.rkt` (plus 4 in
`extensions/gsd/evidence-discovery.rkt`, 3 in `extensions/gsd/evidence-probe.rkt`): the static
graph does NOT show these edges. Any map-based selection must treat this component's true
runtime topology as unresolved — conservative escalation applies.

## Broader verification required when

- Journal schema, disposition-store format, or authority interpretation changes.
- Controller routing changes (owner + route + recovery paths, plus failure/restart scenarios).
- Anything touching the dynamic dispatch targets.
- CL4 note: the proposed pilot is an observe→decide split INSIDE this component (pure decision
  over explicit views, controller remains interpreter, journal/stores stay the only writers).
  Per #9981 acceptance, the pilot must also demonstrate adequate contract coverage and safe
  verification selection — this extraction alone does not prove component-local verification.

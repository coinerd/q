# Subsystem Maps — repository index (CL1, #9978)

Canonical, current-truth entry points into q's subsystems for humans and agents.
These maps are the **ownership and verification layer** on top of
[the architecture overview](../overview.md) (structure/layers) and
[the GSD architecture guide](../../gsd-architecture.md) (GSD internals); they do not replace
either. Every concrete module and test named here is checked to exist by
`tests/test-subsystem-maps.rkt`.

## Repository map (first cut: where does a task live?)

| Task touches… | Subsystem map | Authoritative docs | First tests to run |
|---|---|---|---|
| Campaign execution, `/go`, waves, delivery, review | [gsd-campaign.md](gsd-campaign.md) | [gsd-architecture.md](../../gsd-architecture.md), [gsd-guide.md](../../gsd-guide.md) | `tests/test-gsd-campaign-decision.rkt`, `tests/test-gsd-go-orchestrator.rkt` |
| Supplemental evidence (T/K/S lane) | [gsd-supplemental-evidence.md](gsd-supplemental-evidence.md) | [gsd-architecture.md](../../gsd-architecture.md) | supplemental owners (see map) |
| Session lifecycle, agent loop, context assembly, streaming | [agent-session.md](agent-session.md) | [overview.md](../overview.md) (data flows), [session-tree.md](../../session-tree.md) | `tests/test-agent-state.rkt`, session-store owners |
| Provider adapters, retry, streaming transport | [provider.md](provider.md) | [provider-retry.md](../../provider-retry.md) | `tests/test-retry-error-classification.rkt`, provider golden matrix |
| Tool execution, permissions, scratch, safe mode | [tools-permissions.md](tools-permissions.md) | [security.md](../../security.md), [memory-activation.md](../../memory-activation.md) | `tests/test-tool-scratch-allocation.rkt`, `tests/test-worker-security.rkt` |
| TUI rendering / commands (incl. Unicode width helpers in `tui/char-width.rkt`) | (map not in pilot set — see overview.md TUI stack) | overview.md | `tests/test-ansi.rkt`, `tests/tui/test-char-width.rkt` |
| Everyday verification workflow | — | [../../testing.md](../../testing.md) | `./qt quick <owner>` |

Machine-readable manifest (derived, not hand-maintained):
[`../subsystem-manifest.rktd`](../subsystem-manifest.rktd) — one record per top-level
subsystem: production module count, provide totals, audited quick owners with capability
status, `@covers` mappings, `dynamic-require` marks, GSD inventory effects, dependency-policy
exceptions, outbound cross-subsystem edges.

## How these maps stay fresh

1. **Derived data is generated, never edited:**

   ```sh
   racket scripts/subsystem-manifest.rkt --out docs/architecture/subsystem-manifest.rktd
   ```

   The command reads only existing metadata (`tests/.coverage-manifest.json`,
   `tests/.quick-admission.rktd`, `reports/test-ownership-map.json`,
   `extensions/gsd/responsibility-inventory.rkt`,
   `docs/architecture/dependency-policy.rktd`, and the working-tree source graph).
   `tests/test-subsystem-maps.rkt` fails when the committed manifest drifts from
   regeneration — the same regenerate-and-commit-in-the-same-change pattern as the README
   metrics and the tier-ownership matrix. No new mandatory process: the existing checks
   carry the freshness.

2. **Curated maps may only name real files.** The same pin test asserts every
   backticked `.rkt` path in these maps exists. Owner claims come from `@covers`
   metadata, module header annotations, and the measured graph — when ownership
   moves intentionally (e.g. a CL4 extraction), update the map in the same commit
   as the move and the pin test enforces the new reality.

3. **Historical evidence stays opt-in.** CL0 baseline and prior assessments live in
   `docs/reports/` and `docs/architecture/*v*.rktd`; they are history, not current
   truth, and are referenced from the maps only as provenance.

## Verification framing (per the amended initiative)

A change confined to one component should run that component's owner tests plus the
affected **behavioral boundary** contracts — not unrelated internals. The maps mark, per
component: the single writer of durable state, the pure decision owner, the effect
interpreter, provider/consumer relationships, and the conditions that require broader
verification (shared dependency, security/authority surface, unresolved dynamic impact,
verification-infrastructure changes). Selection is evidence for CL4/CL5 shadow-mode
evaluation; today's required PR verification remains exactly as configured in CI.

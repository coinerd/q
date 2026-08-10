# State: v0.99.88 — Extension Host/Runtime Service Boundary

**Campaign status:** ACTIVE — W4 implemented; review + gates done; release pending
**Plan-ID / Hash:** generated at campaign start
**Baseline SHA:** `0e78c51f` (v0.99.87 release)
**Current main:** `5131ca3d` (W3 merge; W4 branch `feature/v09988-w4-tui-exceptions-release`)
**Current wave:** W4 — Decide TUI exceptions and release (#9225)
**Next:** independent review; PR/CI/merge; tag + milestone #875 close

| Wave | Status | PR | Merge SHA | Review | Evidence |
|---|---|---|---|---|---|
| W0 | ✅ DONE | #9249 | `61fc7057` | ✅ APPROVED (2 passes) | characterization 12/12; focused 257/257; Fast 15369 |
| W1 | ✅ DONE | #9250 | `8214d7cd` | ✅ APPROVED | protocol 11/11; focused 76/76; Arch 231/231; Fast 15382 |
| W2 | ✅ DONE | #9251 | `0fb0779d` | ✅ APPROVED | dual-run 8/8; focused 150/150; Arch 232/232; Broad 1241/17657; Fast 15390 |
| W3 | ✅ DONE | #9252 | `5131ca3d` | ✅ APPROVED | E1–E7 9/9; package 51/51; Security 63/702; Arch 21/235; Fast 1056/15403 |
| W4 | REVIEW | — | — | PENDING | policy 96/96; fitness 12/12; Arch 21/237; Security 63/702; Fast 1056/15405; Broad 1234/17672; Smoke 19/306; release dry-run 5/5 |

## Finding state

MA-02 CLOSED (W2); MA-03 CLOSED (W2); MA-04 CLOSED (W3);
MA-05 CLOSED (W4: dialog-api/ui-surface/widget-lifecycle converted to
evidence-backed permanent pair waivers; none expired; no abstract UI
framework introduced).

## W4 decision summary

Roadmap W4 ("Nur entfernen, wenn ein bereits vorhandenes neutrales
UI-Protokoll genügt; kein abstraktes UI-Framework bauen. Verbleibende
intentionale UI-Bridges als eng begrenzte, dauerhafte Waiver mit
Consumer-Beleg dokumentieren"): no neutral UI protocol exists outside
TUI/ui-core for ui-state, ui-action events, or q-component, so none of the
three can be removed without building an abstract UI framework (prohibited).
All three are intentional UI/TUI bridges with consumer evidence:

- `dialog-api.rkt` (ui, dest `ui-core/ui-state-protocol.rkt`) — extension
  dialog primitives; framework infra (extension-catalog infra list);
  documented in api-stability.md.
- `ui-surface.rkt` (ui, dest `ui-core/ui-actions.rkt`) — callback registry
  bridge; production consumers tui/tui-init.rkt + gui/main.rkt
  (install-ui-callbacks!) + widget-api + custom-ui-api.
- `widget-lifecycle.rkt` (tui, dest `tui/component.rkt`) — lifecycle widget
  → q-component bridge; shipped extension feature (#5253/#5254).

Each converted to `(permanent-waiver . #t)` + `(waiver-justification . ...)`
keeping pair-precise `boundary` + `destinations`. Release: v0.99.88
(version bump, CHANGELOG, README metrics synced).

## Projection consistency

Tracked scaffolds live in `q/docs/planning/`; executor mirror in `.planning/`.
STATE/VALIDATION/wave files advance together on every transition.

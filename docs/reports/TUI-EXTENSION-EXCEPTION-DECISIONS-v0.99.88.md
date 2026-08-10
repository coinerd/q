# TUI Extension Exception Decisions — v0.99.88 W4

**Wave:** v0.99.88 W4 · **Issue:** #9225 · **Finding:** MA-05 (closure)
**Decision:** all three remaining extension exceptions are intentional
UI/TUI bridges → **evidence-backed permanent pair waivers** (roadmap W4:
"Nur entfernen, wenn ein bereits vorhandenes neutrales UI-Protokoll genügt;
kein abstraktes UI-Framework bauen. Verbleibende intentionale UI-Bridges als
eng begrenzte, dauerhafte Waiver mit Consumer-Beleg dokumentieren").
No exception is removed because no neutral UI protocol exists outside
TUI/ui-core for the bridged types (ui-state protocol, ui action events,
q-component); building one would be an abstract UI framework, which the
roadmap prohibits. None are expired (permanent waivers never expire;
revisit-by dates removed).

## 1. Result

`docs/architecture/dependency-policy.rktd` converts the three dated
extension exceptions (revisit-by 2026-10-01) to permanent pair waivers that
keep `boundary` + `destinations` (pair-precision preserved — the extension
fitness checker requires destinations regardless of lifecycle). All five old
extension exceptions are now terminal:

| Exception | v0.99.87 | Decision (v0.99.88) |
|---|---|---|
| `extensions/context.rkt` (runtime) | dated | REMOVED (W2, MA-02/MA-03) |
| `extensions/ext-package-manager.rkt` (runtime) | dated | REMOVED (W3, MA-04) |
| `extensions/dialog-api.rkt` (ui) | dated | PERMANENT pair waiver (W4, MA-05) |
| `extensions/ui-surface.rkt` (ui) | dated | PERMANENT pair waiver (W4, MA-05) |
| `extensions/widget-lifecycle.rkt` (tui) | dated | PERMANENT pair waiver (W4, MA-05) |

None expired: permanent waivers carry no revisit-by; the fitness checker's
expiry rule applies only to dated entries.

## 2. Per-exception evidence (individually decided)

### 2.1 `dialog-api.rkt` — PERMANENT pair waiver (boundary ui)

- **Purpose**: extension dialog primitives — `ctx-notify` / `ctx-confirm` /
  `ctx-select` (#721–724) plus notification/select result structs.
- **Import target**: `ui-core/ui-state-protocol.rkt` (F3 extraction) —
  re-exports `ui-state`, `ui-state-status-message`, `set-status-message` from
  `tui/state-types.rkt`. dialog-api imports **no tui/ module directly**; the
  crossing is into the shared ui-core protocol layer (TUI-adjacent, not tui/).
- **Consumers**: extension framework infrastructure (listed in
  `runtime/extension-catalog.rkt` infra-names, i.e., part of the extension
  framework set); documented in `docs/api-stability.md`; characterized by
  `tests/test-dialog-api.rkt`.
- **Removal analysis**: no neutral ui-state protocol exists outside
  tui/state-types; ui-core/ui-state-protocol is the intended seam for
  non-TUI modules. Moving ui-state to util/ or inventing a new protocol
  would be an abstract UI framework (prohibited) and would ripple into every
  TUI module. → waiver with consumer evidence.

### 2.2 `ui-surface.rkt` — PERMANENT pair waiver (boundary ui)

- **Purpose**: parameter-based callback registry (M-08) so extensions call
  UI operations (footer/header/status/widget) without importing TUI
  internals (ARCH-02 break); event-publishing dual-path via ui-core actions.
- **Import target**: `ui-core/ui-actions.rkt` (F4) — shared action event
  names/emitters. ui-surface imports **no tui/ module directly**; the
  crossing is into the shared ui-core action layer.
- **Consumers**: strong production evidence — `tui/tui-init.rkt` and
  `gui/main.rkt` statically require it and install callbacks
  (`install-ui-callbacks!`); `extensions/widget-api.rkt` and
  `extensions/custom-ui-api.rkt` depend on it; characterized by
  `tests/test-ui-surface-actions.rkt`, `test-ui-surface-characterization.rkt`,
  `test-ui-surface-null-safety.rkt`.
- **Removal analysis**: this IS the intentional bridge that keeps
  extensions↔TUI decoupled; removing it would force extensions to import
  tui/ directly (worse). ui-core/ui-actions re-exports event types and
  `runtime/settings-query.rkt`, so no neutral home exists without an abstract
  UI framework. → waiver with strong consumer evidence.

### 2.3 `widget-lifecycle.rkt` — PERMANENT pair waiver (boundary tui)

- **Purpose**: lifecycle widget protocol (#5253/#5254) — mount/render/input/
  unmount phases, thread-safe registry, focus management, and
  `widget->component` bridge into the q-component model.
- **Import target**: `tui/component.rkt` for `q-component?` and
  `make-q-component` (the bridge). This is the **only remaining direct
  tui/ import** from an extension.
- **Consumers**: shipped extension feature listed as an available extension
  (non-infra, activatable via /activate); characterized by
  `tests/test-widget-lifecycle.rkt`.
- **Removal analysis**: the q-component protocol lives in the TUI component
  model (`tui/component.rkt`) with no neutral equivalent; widget->component
  deliberately bridges lifecycle widgets into that model. Extracting
  q-component to a neutral layer would be an abstract UI framework
  (prohibited). → waiver with consumer evidence.

## 3. Policy changes

- Three dated entries → permanent format:
  `(permanent-waiver . #t)` + `(waiver-justification . "...")` +
  `(boundary . ui|ui|tui)` + `(destinations . (...))` (pair-precise).
- Schema comment updated: extension exceptions ALWAYS keep
  boundary + destinations (fitness checker requirement); runtime/agent
  checkers allow waivers without destinations.
- `test-extension-exception-fitness.rkt`: added W4 gate
  "All extension exceptions are evidence-backed permanent pair waivers"
  (each must be permanent, justification non-empty, no revisit-by,
  pair-precise destinations, boundary classified); added positive probe for
  the permanent waiver format; count/membership/boundary-classification
  tests unchanged (still 3; runtime `'()`, tui widget-lifecycle,
  ui dialog-api + ui-surface).

## 4. Gates

| Gate | Result |
|---|---|
| Policy/boundary focused (5 files) | ✅ 96/96 |
| Exception fitness | ✅ 12/12 |
| Arch | (run) |
| Fast | (run) |
| Broad | (run) |
| Security | (run) |
| Smoke | (run) |
| Release dry-run | (run) |
| lint-format | (run) |

## 5. MA-05 closure proof

"Each of dialog-api, ui-surface, and widget-lifecycle is removed or receives
an evidence-backed permanent pair waiver" → each receives an evidence-backed
permanent pair waiver (evidence per §2; justification text stored in
dependency-policy.rktd; consumer evidence pinned by the fitness gate).
None expired (revisit-by removed). Roadmap acceptance "alle fünf alten
Exceptions geschlossen oder neu evidenzbasiert entschieden; keine abgelaufene
Ausnahme" satisfied: context.rkt + ext-package-manager removed (W2/W3),
dialog-api/ui-surface/widget-lifecycle permanent (W4).

## 6. Release

v0.99.88 released with this wave: version bump + CHANGELOG + README metrics
(§ gates + tag + milestone #875 close).

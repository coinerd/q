# ADR-0017: GUI Architecture

## Status
Proposed

## Context

v0.63.0 introduces a native GUI mode alongside the existing TUI. The GUI must:
- Share the same agent runtime (event-bus, session store, tool system)
- Not require modifications to existing TUI modules
- Work with or without gui-easy-lib installed
- Use the same state reducer (`apply-event-to-state`)

## Decision

Add two new layers on top of the existing 10-layer architecture:

### Layer 10: ui-core/ (UI Abstraction)
Backend-agnostic modules shared between TUI and GUI:
- `observable-bridge.rkt` — Thread-safe bridge from event-bus to GUI state boxes
- `dispatch.rkt` — UI action dispatch (submit, cancel, scroll, command)
- `theme-protocol.rkt` — Shared color/font theme struct
- `layout-protocol.rkt` — Shared layout geometry struct

### Layer 11: gui/ (GUI Backend)
gui-easy-specific rendering and interaction:
- `main.rkt` — Entry point, display check, window creation
- `app.rkt` — Top-level application layout
- `views/` — Status bar, input area, transcript, sidebar
- `components/` — Message entries, code blocks, progress bars
- `extension-slots/` — Extension widget zones, custom renderers

### Data Flow
```
User Input → dispatch-*! → event-bus → state reducer → UI update
                                             ↓
                                 observable-bridge → GUI state box → render
```

### Key Constraints
1. Zero changes to `tui/` modules
2. GUI modules use `(gui-available?)` guard for headless CI
3. `gui-easy-lib` is an optional dependency in info.rkt
4. All GUI tests skip gracefully with `(exit 0)` when no display

## Consequences

### Positive
- TUI and GUI share state management logic
- GUI can be developed incrementally without breaking TUI
- Headless CI continues to work
- Extensions work in both modes via shared dispatch layer

### Negative
- Two rendering backends to maintain
- Optional dependency management complexity
- GUI code path not testable in CI without display server

## Hooks Added

8 new GUI-specific hook action schemas:
- `gui.window.opened` — Window created
- `gui.window.closed` — Window destroyed
- `gui.theme.changed` — Theme updated
- `gui.layout.changed` — Layout resized
- `gui.focus.changed` — Component focus changed
- `gui.input.submit` — Input submitted
- `gui.command.execute` — Slash command
- `gui.scroll.request` — Scroll request

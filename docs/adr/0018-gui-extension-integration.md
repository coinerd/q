# ADR-0018: GUI Extension Integration Architecture

**Status**: Accepted
**Date**: 2026-05-25
**Supersedes**: ADR-0017 (GUI Architecture)

## Context

ADR-0017 established the GUI architecture with Layer 10 (ui-core/) and Layer 11 (gui/) on top of the existing TUI stack. We now need to define how extensions integrate with the GUI — registering custom widgets, renderers, and handling lifecycle events.

## Decision

### Extension Slots Architecture

1. **Widget Zones** (`gui/extension-slots/widget-zone.rkt`): Named regions (sidebar, toolbar, status) where extensions register widgets. Each zone is a mutable box of widget hashes, with semaphore-protected register/unregister operations.

2. **Custom Renderers** (`gui/extension-slots/custom-renderer.rkt`): Registry mapping content types (code, image, table) to rendering functions. Extensions register renderers for custom content; unregistered types fall through to default rendering.

3. **Extension Bridge** (`gui/extension-slots/extension-bridge.rkt`): Central coordinator connecting widget zones, renderer registry, and extension lifecycle. Extensions are loaded/unloaded through the bridge, which manages cleanup.

### View Components

4. **Overlay Views** (`gui/views/overlay.rkt`): Alert, confirm, and prompt dialogs rendered on top of the main view. Theme-aware with severity coloring.

5. **Sidebar** (`gui/views/sidebar.rkt`): Collapsible sidebar with sections and items. Width collapses to 2 characters when collapsed.

6. **Toolbar** (`gui/views/toolbar.rkt`): Horizontal toolbar with buttons and separators. Supports top/bottom positioning.

7. **Code Blocks** (`gui/views/code-block.rkt`): Fenced code block rendering with language detection, line numbers, and copy metadata.

8. **Error Boundary** (`gui/views/error-boundary.rkt`): Catches rendering errors and displays fallback views instead of crashing.

### Theme Management

9. **Theme Manager** (`gui/theme-manager.rkt`): Manages multiple themes (default, light, dark, custom) with runtime switching and per-color customization.

### Design Principles

- **Backend-agnostic**: All views produce hash descriptors, not platform-specific output
- **Zero TUI changes**: All gui/ modules are independent of tui/ modules
- **Headless-safe**: No dependency on gui-easy or racket/gui for core logic
- **Theme propagation**: All views receive `ui-theme` from the shared protocol

## Module Inventory (v0.63.0–v0.67.0)

| Layer | Module | Purpose |
|-------|--------|---------|
| L10 | `ui-core/theme-protocol.rkt` | Shared theme struct + accessors |
| L10 | `ui-core/layout-protocol.rkt` | GUI layout dimensions |
| L10 | `ui-core/observable-bridge.rkt` | Event bus → GUI state bridge |
| L10 | `ui-core/dispatch.rkt` | UI event dispatch |
| L11 | `gui/app.rkt` | Top-level app state |
| L11 | `gui/main.rkt` | GUI entry point (guarded) |
| L11 | `gui/theme-manager.rkt` | Multi-theme management |
| L11 | `gui/views/status.rkt` | Status bar view |
| L11 | `gui/views/input.rkt` | Input area view |
| L11 | `gui/views/transcript.rkt` | Transcript/scroll view |
| L11 | `gui/views/message-entry.rkt` | Message entry styling |
| L11 | `gui/views/overlay.rkt` | Alert/confirm/prompt dialogs |
| L11 | `gui/views/sidebar.rkt` | Collapsible sidebar |
| L11 | `gui/views/toolbar.rkt` | Toolbar with buttons |
| L11 | `gui/views/code-block.rkt` | Code block rendering |
| L11 | `gui/views/error-boundary.rkt` | Error fallback views |
| L11 | `gui/extension-slots/widget-zone.rkt` | Widget zone management |
| L11 | `gui/extension-slots/custom-renderer.rkt` | Custom renderer registry |
| L11 | `gui/extension-slots/extension-bridge.rkt` | Extension lifecycle bridge |

## Consequences

- Extensions can register custom views without modifying core code
- All 18 GUI modules are testable in headless environments
- Theme changes propagate to all views automatically
- Error boundaries prevent rendering failures from crashing the GUI

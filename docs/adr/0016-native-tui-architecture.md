# ADR-0016: Native TUI Architecture

## Status
Accepted

## Context
The q TUI originally depended on two external Racket packages: `tui-term` (terminal I/O, input handling) and `tui-ubuf` (cell buffer, screen rendering). These dependencies created several problems:

1. **Dynamic loading fragility**: The code used `dynamic-require` with fallback paths, making error handling complex and debugging difficult
2. **External dependency risk**: Package availability and compatibility were outside our control
3. **Feature limitations**: The external packages didn't support advanced features needed for q's TUI (incremental rendering, virtual DOM)
4. **Testing difficulty**: Mocking external packages required complex workarounds

The existing codebase already contained ~80% native fallback implementations handling most functionality. The native terminal input module (`terminal-input.rkt`, 668 lines) provided raw mode, ANSI decoding, UTF-8, mouse events, and bracketed paste.

## Decision
Replace all external TUI dependencies with a native Racket architecture consisting of six layers:

### Layer 1: Native Terminal I/O (`tui/terminal-native.rkt`)
- Direct ANSI escape sequences for terminal control
- Native `stty` commands for raw/cooked mode
- Vector-based mouse messages: `#(tmousemsg kind x y left? middle? right?)`
- No dynamic-require of external packages

### Layer 2: Cell Buffer (`tui/cell-buffer.rkt`)
- Flat vector storage: 7-element cell vectors `#(char fg bg bold underline italic blink)`
- Index formula: `(+ (* row cols) col)` for O(1) access
- Mutable struct: `(cell-buffer cols rows cells dirty?)`
- Default cell: `#\space`, fg=7, bg=0, all flags #f

### Layer 3: Cell-Level Incremental Rendering (`tui/cell-diff.rkt`, `tui/cell-diff-render.rkt`)
- XOR-based row hashing for fast dirty-row detection
- Delta struct: `(cell-delta col row old-cell new-cell)`
- Smart rendering: auto-selects full vs incremental based on change threshold (50%)
- SGR output: `\x1b[0;38;5;FG;48;5;BGm` with bold/underline extensions

### Layer 4: Virtual DOM (`tui/vdom.rkt`, `tui/vdom-layout.rkt`, `tui/vdom-render.rkt`)
- Five vnode types: `vtext` (styled text), `vhbox` (horizontal), `vvbox` (vertical), `vfill` (padding), `voverlay` (layered)
- Layout engine converts vnodes to styled-line list (reuses existing `styled-line` format)
- Style model: `(listof symbol)` matching existing `styled-segment` conventions
- Full pipeline: `vnode → vdom-layout → styled-lines → cell-buffer`

### Layer 5: Component System Extension (`tui/vdom-components.rkt`)
- `q-component` extended with `vdom?` flag
- Typed components: Header, Transcript, StatusBar, InputBox, Overlay
- Frame composition: `make-vdom-frame-component` combines all zones
- Backward compatible: existing styled-line components work unchanged

### Layer 6: Bridge (`tui/vdom-bridge.rkt`)
- `render-vdom-frame!` — renders vnodes through the existing render loop
- `styled-line->vnode` / `styled-lines->vnode` — migration helpers
- `use-vdom-render?` parameter — toggles between vdom and direct rendering

## Consequences

### Positive
- **Zero external TUI dependencies**: Entire TUI stack is native Racket
- **Better performance**: Cell-level incremental rendering (was row-level)
- **Virtual DOM foundation**: Enables future GUI backend (M7 deferred)
- **Cleaner code**: No `dynamic-require`, no dual code paths
- **Easier testing**: All modules testable without external package mocking
- **Smaller attack surface**: No dynamic loading of external packages

### Negative
- **Maintenance burden**: We own the terminal I/O layer (ANSI escape handling, mouse protocols)
- **Terminal compatibility**: Must handle terminal differences ourselves (was handled by tui-term)
- **No GUI backend yet**: Virtual DOM is ready but GUI rendering is deferred

### Neutral
- File count increased: 8 new modules, ~1000 lines of new code
- Aliases retained: `make-ubuf` → `make-cell-buffer` for backward compat during transition

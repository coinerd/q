# TUI Overview

The terminal UI provides a rich interactive interface for q with scrolling output, syntax highlighting, and keyboard shortcuts.

## Layout

```
┌─────────────────────────────────────────────────────────────┐
│ q v0.7.9 │ gpt-5.4 │ session: a7f3c2e1          [● active] │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  You: Explain the architecture of this project              │
│                                                             │
│  [tool:read] Reading BLUEPRINT/00-overview.md …             │
│  [tool:read] Reading agent/loop.rkt …                       │
│                                                             │
│  Agent: This project is **q**, a coding agent in Racket.    │
│  The architecture follows a layered design:                 │
│                                                             │
│    1. Types layer — canonical structs                       │
│    2. Core layer — agent loop, event bus, LLM provider      │
│    3. Tools layer — registry with read/write/edit/bash      │
│    4. Runtime — session, compactor, extensions              │
│    5. Interfaces — CLI, JSON, TUI                           │
│                                                             │
│  You: Add a test for the event bus                          │
│                                                             │
│  [tool:write] Writing tests/test-event-bus.rkt              │
│  [tool:bash]  raco test tests/test-event-bus.rkt            │
│  ✓ 5 tests passed in 0.03s                                  │
│                                                             │
│  Agent: Done. Five tests covering subscribe, publish,        │
│  wildcard, fan-out, and error isolation.                    │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ > _                                                          │
└─────────────────────────────────────────────────────────────┘
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Enter` | Send prompt (or insert newline if cursor is mid-line) |
| `Ctrl+Enter` | Insert newline (multi-line prompt) |
| `Ctrl+S` | Save session |
| `Ctrl+C` | Cancel current agent turn |
| `Ctrl+L` | Clear transcript area |
| `Ctrl+D` | Quit |
| `Up` / `Down` | Scroll transcript |
| `PgUp` / `PgDn` | Scroll transcript by page |
| `Tab` | Autocomplete `/` commands |
| `Alt+←` / `Alt+→` | Switch between branched sessions |

## Mouse Support

- **Click + drag** in the transcript area to select text
- **Scroll wheel** scrolls the transcript
- **Right-click** on a `[tool:…]` badge to expand/collapse full tool output

## Transcript Features

- **Tool badges** like `[tool:read]` are collapsible — click or press `Enter` to expand the full output
- **Test results** show a green `✓` or red `✗` with a summary line
- **Code blocks** are syntax-highlighted using ANSI colours
- **Long responses** stream in word-by-word with a blinking cursor indicator

## Status Bar

The top bar shows:

```
q v0.7.9 │ gpt-5.4 │ session: a7f3c2e1 │ [● active]
```

- **Version** — current q version
- **Model** — active LLM model
- **Session** — current session ID (short form)
- **Status indicator** — `● active`, `○ idle`, or `◐ thinking…`

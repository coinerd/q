# Demo Transcripts

Realistic transcripts showing q in action across its different interfaces.

## CLI Demos

| Demo | Description |
|------|-------------|
| [Basic CLI Session](cli-basic.md) | Starting a session, asking questions, running tools, saving |
| [Session Resume](cli-session-resume.md) | Resuming a previous session with context |
| [Session Branching](session-branching.md) | Branching a session to try an alternative approach |

## Interface Demos

| Demo | Description |
|------|-------------|
| [JSON Mode](json-mode.md) | Machine-readable JSON protocol for editor/CMS integration |
| [TUI Overview](tui-overview.md) | Terminal UI layout, keybindings, and interaction patterns |

## Running Demos Yourself

All commands work with `raco q` (installed) or `racket main.rkt` (from source tree).

```bash
# Basic CLI
raco q --model gpt-5.4

# JSON mode (pipe-friendly)
raco q --json

# Resume a session
raco q --resume <session-id>
```

See the [main README](../../README.md) for installation and configuration.

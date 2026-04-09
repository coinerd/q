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

```bash
# Basic CLI
racket main.rkt --model gpt-5.4

# JSON mode (pipe-friendly)
racket main.rkt --json

# Resume a session
racket main.rkt --resume <session-id>
```

See the [main README](../../README.md) for installation and configuration.

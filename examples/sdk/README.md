# Q SDK Examples

Graduated examples demonstrating programmatic usage of the Q agent SDK.

## Examples

| # | File | Description |
|---|------|-------------|
| 01 | `01-minimal.rkt` | Simplest session with defaults |
| 02 | `02-custom-model.rkt` | Select model and thinking level |
| 03 | `03-custom-prompt.rkt` | Modify system prompt |
| 04 | `04-tools.rkt` | Custom tool registration |
| 05 | `05-extensions.rkt` | Subscribe to events |
| 06 | `06-sessions.rkt` | In-memory sessions, resume, list |
| 07 | `07-tree.rkt` | Branch, navigate, tree-info |
| 08 | `08-full-control.rkt` | Full dependency injection |

## Usage

```bash
# From the q/ directory:
racket examples/sdk/01-minimal.rkt
```

## Requirements

- Racket 8.x+
- Q agent library (same repository)

## Progression

The examples go from minimal (01) to full control (08), progressively
introducing more SDK features:

1. **01-03**: Core setup (provider, model, prompts)
2. **04-05**: Tools and events
3. **06-07**: Session management and tree operations
4. **08**: Full DI of all subsystems

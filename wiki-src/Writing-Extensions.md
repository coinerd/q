# Writing Extensions

q extensions hook into the agent lifecycle to add custom behavior — tool interception, response post-processing, session logging, and more.

## Extension Anatomy

Every extension has:

1. **Manifest** (`extension.json`) — Declares the extension's name, version, entry point, hooks, and minimum q version.
2. **Entry module** — A Racket module that registers hooks via the extension API.

### Minimal Example

**`extension.json`:**
```json
{
  "name": "my-extension",
  "version": "X.Y.Z",
  "entry": "main.rkt",
  "min-q-version": "0.X.Y",
  "hooks": ["on-tool-call", "on-response"]
}
```

**`main.rkt`:**
```racket
#lang racket/base

(require q/extensions/api)

(define (on-tool-call ctx tool-name args)
  (printf "[my-extension] Tool called: ~a~n" tool-name)
  args)  ; return args unchanged

(define (on-response ctx response)
  response)  ; return response unchanged

(provide on-tool-call on-response)
```

## Available Hooks

| Hook | Signature | Purpose |
|------|-----------|---------|
| `on-session-start` | `(ctx)` | Called when a new session begins |
| `on-tool-call` | `(ctx tool-name args)` | Intercept/modify tool arguments before execution |
| `on-tool-result` | `(ctx tool-name result)` | Inspect/modify tool results after execution |
| `on-response` | `(ctx response)` | Post-process the LLM response |
| `on-session-end` | `(ctx)` | Called when a session terminates |

## Security Model

- Extensions are loaded from `~/.q/extensions/` (user scope) or `.q/extensions/` (project scope).
- Manifests are validated for required fields and schema compliance.
- File integrity hashes (SHA256) are verified on load.
- Extensions can be quarantined if they cause repeated errors.

## Tiers

Extensions run in one of three tiers:

| Tier | Capabilities |
|------|-------------|
| **Observer** | Read-only hooks, no mutation |
| **Modifier** | Can transform tool args/results and responses |
| **Authority** | Full access including tool registration |

## Best Practices

- Keep extensions stateless where possible.
- Use narrow hook subscriptions — only declare hooks you actually use.
- Return values unchanged unless you intentionally modify them.
- Log errors gracefully; don't crash the agent loop.
- Test with `q/extensions/test-harness.rkt`.

## Reference

- [Extension API source](https://github.com/coinerd/q/blob/main/q/extensions/api.rkt)
- [Manifest spec](https://github.com/coinerd/q/blob/main/q/extensions/manifest.rkt)
- [Test harness](https://github.com/coinerd/q/blob/main/q/extensions/test-harness.rkt)

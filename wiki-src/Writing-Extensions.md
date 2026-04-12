# Writing Extensions

This page explains how to think about q extensions and where they fit in the architecture.

## Principles

- Prefer narrow responsibilities
- Keep extension behavior observable
- Avoid leaking interface-specific concerns into the core
- Link back to canonical implementation and contract docs

## Getting started

See the extension API documentation in the main repository:
- [extensions/api.rkt](https://github.com/coinerd/q/blob/main/q/extensions/api.rkt)
- [Extension manifest spec](https://github.com/coinerd/q/blob/main/q/extensions/manifest.rkt)

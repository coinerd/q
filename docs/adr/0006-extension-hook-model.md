# ADR-0006: Extension Hook Model

## Status
Accepted

## Context
Extensions need well-defined interception points to modify agent behavior—adding
context to prompts, validating tool calls, post-processing responses—without
modifying core code. A free-form monkey-patching approach makes the system
unpredictable and hard to debug.

## Decision
q exposes a hook system with named dispatch points (e.g., `pre-tool-call`,
`post-response`, `session-start`). Extensions register handler functions for
specific hooks. Each handler returns one of:

- **pass** — continue without modification.
- **block** — abort the action with an optional error message.
- **modify** — transform the data and continue.

Hooks execute in registration order. The core invokes hooks at clearly
documented points in the agent loop.

## Consequences
**Easier:** Powerful extensibility without forking core. Extensions are
self-contained and discoverable. Hook behavior is auditable.

**Harder:** Hook ordering matters and can cause subtle bugs if extensions
conflict. Error isolation—ensuring one extension's crash doesn't bring down
the agent—requires defensive coding in the hook dispatcher.

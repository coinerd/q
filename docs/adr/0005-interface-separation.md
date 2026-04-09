# ADR-0005: Interface Separation

## Status
Accepted

## Context
q serves multiple user interfaces: an interactive CLI, a terminal UI (TUI), a
JSON streaming mode for programmatic access, and an SDK for embedding. Each
interface has different display capabilities and interaction patterns. Embedding
UI logic in the core agent loop would make it impossible to reuse core logic
across interfaces.

## Decision
The core is strictly UI-independent. Interfaces are pure event consumers: they
subscribe to the event bus and render events in their own format (terminal
markup, JSON lines, API responses). No UI code exists in the core module.

Shared state (session ID, model name, configuration) is passed explicitly
through the session and context, never through global UI variables.

## Consequences
**Easier:** Each interface evolves independently. Adding a new interface (e.g.,
a web API) requires only a new event consumer. Testing the core without any UI
attached is straightforward.

**Harder:** Interfaces must handle all relevant event types to avoid silent data
loss. The event contract between core and interfaces must be stable and
well-documented.

# ADR-0003: Event Bus Architecture

## Status
Accepted

## Context
Multiple consumers need to react to agent events: the TUI updates its display,
the session logger persists events, extensions run hooks, and the JSON mode
interface streams output. Without a central dispatch mechanism, producers become
coupled to every consumer, making the system brittle and hard to extend.

## Decision
q uses a central event bus with publish/subscribe semantics. All significant
agent actions—message sent, tool invoked, session started, error raised—emit
typed events to the bus. Consumers subscribe to event types they care about.

Events are dispatched synchronously in subscription order within a single agent
turn, ensuring deterministic behavior for testing and replay.

## Consequences
**Easier:** Loose coupling between layers—new consumers can be added without
modifying producers. Testability—events can be captured and asserted. Extensibility
—extensions subscribe to events they need.

**Harder:** Event ordering across async boundaries requires care. Debugging event
chains can be non-obvious since the flow is implicit. Subscribers that block or
fail must not stall the entire bus.

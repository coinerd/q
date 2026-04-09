# ADR-0001: Small Trusted Core

## Status
Accepted

## Context
Coding agents operate with significant power—they read files, execute commands, and
modify source code. This power demands strong safety guarantees. A large monolithic
core is difficult to audit: the more code that runs in the trusted path, the harder
it is to reason about correctness and security.

## Decision
q adopts a minimal trusted core with well-defined extension points. The core is
responsible for exactly three things:

1. **Message routing** — dispatching requests between the user, the LLM, and tools.
2. **Event dispatch** — emitting typed events to all subscribers.
3. **Session lifecycle** — creating, persisting, and closing sessions.

Everything else—tools, LLM providers, UI frontends—is pluggable outside the core.

## Consequences
**Easier:** Auditing core behavior, reasoning about safety-critical paths, testing
the core in isolation.

**Harder:** Extension quality varies since extensions are not part of the trusted
core. The boundary between core and extensions must be clearly documented and
enforced.

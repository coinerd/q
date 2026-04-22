# ADR-0008: Safe-Mode Enforcement

## Status
Accepted

## Context
q tools can execute arbitrary shell commands and modify files. In production
use, operators need a way to lock the agent into a read-only or restricted mode
that cannot be reversed during a session — for example, during code review,
CI runs, or when auditing a third-party extension.

The tool struct already has a `dangerous?` flag, but there was no mechanism to
prevent dangerous tools from executing at runtime once safe mode is activated.

## Decision
Add a one-shot safe-mode lock backed by a guarded parameter:

1. **`safe-mode-locked?`** — A parameter wrapping a one-shot box. Once
   `lock-safe-mode!` is called, no further changes to safe-mode state are
   possible for the remainder of the Racket process.
2. **`dangerous?` field** — The `tool` struct carries a `dangerous?` boolean.
   When safe mode is active, the tool scheduler refuses to dispatch tools
   marked as dangerous.
3. **Irreversible** — Safe-mode lock is one-shot: `lock-safe-mode!` can be
   called exactly once. Attempts to re-lock or unlock raise an error.

This is enforced at the tool scheduler level, before tool dispatch.

## Consequences
**Easier:** Operators can confidently restrict agent behavior. Extensions cannot
bypass the lock. The scheduler check is a single boolean test.

**Harder:** Extensions that need to perform dangerous operations (e.g., file
writes) must either run before safe-mode is locked or use the extension hook
system to negotiate elevated access.

**Risks:** If safe mode is locked too early (e.g., during initialization),
legitimate setup tools may fail. The lock should be applied after bootstrap.

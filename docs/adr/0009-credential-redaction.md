# ADR-0009: Credential Redaction

## Status
Accepted

## Context
q stores API keys and OAuth tokens for LLM providers. These credentials appear
in several places: environment variables, config files, session logs, and
in-memory structs. If credential structs use `#:transparent`, Racket's default
printer will display the full API key in error messages, REPL output, and debug
logs — a security risk.

## Decision
Credential structs use opaque printing with redaction:

1. **`credential` struct** — No `#:transparent`. Custom `gen:custom-write`
   masks the API key to `****`. Implements `gen:equal+hash` for test comparison
   without exposing the key in printed output.
2. **`redacted-credential` struct** — Opaque. Stores only the last-4 chars of
   the key for display purposes.
3. **No raw key logging** — The auth store never writes raw keys to session
   logs or trace files. Only `redacted-credential` values appear in events.

## Consequences
**Easier:** API keys are never accidentally exposed in logs, error reports, or
debug output. Tests can still compare credentials via `equal?`.

**Harder:** Debugging credential issues requires explicit accessor calls rather
than printing the struct. The `gen:equal+hash` implementation must stay in sync
with the struct fields.

**Risks:** If a future developer adds `#:transparent` back for "convenience",
the protection is lost. A security lint rule (`scripts/lint-security.rkt`)
catches this pattern.

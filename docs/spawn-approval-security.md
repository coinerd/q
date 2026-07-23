<!-- verified-against: 0.99.59 -->
# Subagent Spawn Approval Security

Dangerous subagent delegation (`shell-exec` or `git-write`) is fail-closed. In CLI, JSON, RPC, SDK, or any other headless mode without an attached interactive approval frontend, both `spawn-subagent` and `spawn-subagents` deny dangerous plans unconditionally. A publisher, callback, Boolean parameter, or process-mode switch cannot grant authority. Safe delegated plans continue to run headlessly.

## Exact-plan commitment

Single and batch delegation use the same immutable execution-plan type. Its SHA-256 commitment binds execution-effective values, including task and role digests, model, effective capabilities and tools, turn limit, safe mode, working directory, parent session/call correlation, provider binding, child identities, and batch ordering/concurrency.

The frontend-neutral broker in `runtime/approval/broker.rkt` owns:

1. registration of the exact commitment and immutable redacted presentation;
2. authoritative presentation lookup;
3. a decision carrying the same request ID and commitment digest;
4. an opaque one-use grant;
5. exact-digest grant consumption immediately before execution.

A missing, malformed, wrong, stale, replayed, or teardown-revoked digest cannot authorize execution. TUI events carry correlation and digests only; the TUI renders the broker-owned presentation rather than trusting event-carried preview text.

## Secret handling

Raw task, role, model, credential, and provider objects are absent from request events, overlays, terminal events, and grants. Presentations use credential redaction, terminal-control removal, whitespace normalization, and bounded previews. The private in-memory execution snapshot is not a frontend/event payload.

## Breaking approval API migration

The following legacy or mutable-authority APIs were removed rather than deprecated:

- `approval-await-result`
- `approval-put!`
- `approval-channel-ch`
- `register-approval-request!`
- `current-spawn-approval-result`
- `set-headless-approval-mode!`
- `headless-approval-mode?`

Callers must not implement replacement Boolean approval callbacks. Interactive frontends attach a broker channel lease, display only the authoritative request view, and submit `approval-decide!` with both request ID and commitment digest. Authenticated headless automation is outside this release and requires a separate capability-bounded design.

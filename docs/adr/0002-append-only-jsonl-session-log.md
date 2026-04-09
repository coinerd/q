# ADR-0002: Append-Only JSONL Session Log

## Status
Accepted

## Context
q needs durable session history that survives crashes and can be replayed for
debugging or auditing. Traditional database-backed session stores add complexity
and failure modes. Mutable log formats make crash recovery fragile—partially
written records can corrupt the entire log.

## Decision
Sessions are stored as append-only JSONL files. Each agent event (user message,
assistant response, tool call, tool result, system event) is serialized as a
single JSON object on one line. No record is ever mutated in place.

Session files are named by session ID and written to a configurable directory.

## Consequences
**Easier:** Crash recovery—just read up to the last valid line. Auditing—`grep`
or `jq` work directly. Replay—replay events in order to reconstruct state.
Implementation—no transaction or locking logic needed.

**Harder:** Files grow without bound; a compaction mechanism is required for
long-running sessions. Random access to a specific event requires scanning from
the start or maintaining a separate index.

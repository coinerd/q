# Basic CLI Session

A walkthrough of a typical q CLI session from start to finish.

```bash
$ raco q --model gpt-5.4
q version 0.11.2
Model:   gpt-5.4
Session: a7f3c2e1

> Explain the architecture of this project

[tool:read] Reading BLUEPRINT/00-overview.md … (1.2 KB)
[tool:read] Reading BLUEPRINT/01-types.md … (3.4 KB)
[tool:read] Reading agent/loop.rkt … (5.1 KB)

This project is **q**, a coding agent written in Racket. The architecture
follows a layered design:

1. **Agent layer** (`agent/`) — canonical types, event bus, agent loop,
   queue, and state.
2. **LLM layer** (`llm/`) — provider abstraction, OpenAI, Anthropic, and
   streaming.
3. **Tools layer** (`tools/`) — a registry with 10 built-in tools (read,
   write, edit, bash, grep, find, ls, date, firecrawl).
4. **Runtime layer** (`runtime/`) — session management, resource
   tracking, compaction, safe-mode, and extension loading.
5. **Interfaces** (`interfaces/`) — CLI, JSON mode, RPC, TUI, and SDK
   front-ends that consume events from the bus.

Events flow outward: the agent loop emits events to the bus, and
interfaces subscribe to whichever events they care about.

> Add a test for the event bus

[tool:read] Reading agent/event-bus.rkt … (4.7 KB)
[tool:write] Writing tests/test-event-bus.rkt … (2.1 KB)
[tool:bash] Running: raco test tests/test-event-bus.rkt
  5 tests passed in 0.03s

Done. I created `tests/test-event-bus.rkt` with five tests:

- `subscribe-and-publish` — a handler receives a published event
- `unsubscribe` — unsubscribed handler is not called
- `wildcard-subscribe` — wildcard catches all event types
- `multiple-subscribers` — event fans out to all subscribers
- `error-isolation` — one failing handler does not block others

All five pass. The test covers the public contract in `agent/event-bus.rkt`.

> /save
Session saved to ~/.q/sessions/a7f3c2e1.jsonl (7 events, 3.4 KB)

> /quit
Goodbye.
```

### Key points

| Command | Effect |
|---------|--------|
| `> text` | Send a user prompt; the agent reads files, writes code, runs tests |
| `/save` | Persist session to JSONL |
| `/quit` | Exit q |

Tool calls are shown inline as `[tool:read]`, `[tool:write]`, `[tool:bash]` so you always know what the agent is doing.

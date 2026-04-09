# Session Resume

Resuming a previous session with full conversation context.

```bash
$ racket main.rkt --resume a7f3c2e1
Resuming session a7f3c2e1 …
Replay: 7 events loaded (3 tool calls, 2 responses)
Model:   gpt-5.4
Session: a7f3c2e1 (resumed)

> What did we work on last time?

In this session we covered two things:

1. **Architecture overview** — I read the BLUEPRINT and core modules and
   explained the layered design (types → core → tools → runtime →
   interfaces).

2. **Event bus tests** — I added `tests/core/event-bus-test.rkt` with
   five tests covering subscribe/publish, unsubscription, wildcards,
   fan-out, and error isolation. All five passed.

The session has one saved snapshot at `~/.q/sessions/a7f3c2e1.jsonl`.

> Now add the same level of test coverage for the tool registry

[tool:read] Reading src/tools/registry.rkt … (6.2 KB)
[tool:read] Reading tests/tools/registry-test.rkt … (not found)
[tool:write] Writing tests/tools/registry-test.rkt … (2.8 KB)
[tool:bash] Running: raco test tests/tools/registry-test.rkt
  7 tests passed in 0.04s

Created `tests/tools/registry-test.rkt` with seven tests:

- `register-and-lookup` — round-trip a tool through the registry
- `duplicate-register` — registering twice overwrites the first entry
- `lookup-missing` — returns `#f` for unknown tool names
- `list-tools` — lists all registered tool names
- `invoke-tool` — calls a registered tool with args
- `invoke-error` — tool errors are caught and returned as error results
- `builtin-tools` — the four built-in tools are present after init

All seven pass.

> /save
Session saved to ~/.q/sessions/a7f3c2e1.jsonl (16 events, 7.9 KB)

> /quit
Goodbye.
```

### Key points

| Flag | Effect |
|------|--------|
| `--resume <id>` | Replay a saved session and continue where you left off |

The agent replays all previous events into its context before answering, so it "remembers" everything from the earlier conversation.

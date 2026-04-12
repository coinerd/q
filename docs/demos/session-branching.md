# Session Branching

Branching a session to explore an alternative approach without losing the original history.

```bash
$ racket main.rkt --model gpt-5.4
q version 0.7.1
Session: a7f3c2e1

> Refactor the event bus to use channels instead of locks

[tool:read] Reading agent/event-bus.rkt … (4.7 KB)
[tool:write] Writing agent/event-bus.rkt … (5.3 KB)
[tool:bash] Running: raco test tests/test-event-bus.rkt
  3 passed, 2 FAILED
  FAIL: subscribe-and-publish — handler not called
  FAIL: multiple-subscribers — only first subscriber received event

The channel-based approach is causing delivery issues. The lock-based
design was correct — let me try a different strategy.

> /branch
Branched session def456 from a7f3c2e1 at event 7
Switched to session def456

> Actually, keep the lock-based design but add a dispatch queue

[tool:read] Reading agent/event-bus.rkt … (4.7 KB)
[tool:edit] Editing agent/event-bus.rkt — adding dispatch-queue helper
[tool:bash] Running: raco test tests/test-event-bus.rkt
  5 tests passed in 0.03s

The dispatch queue works. Instead of calling handlers inside the lock,
we now collect the matching handlers under the lock, then invoke them
outside it. This keeps thread safety while avoiding deadlock potential.

> /save
Session def456 saved to ~/.q/sessions/def456.jsonl

> /switch a7f3c2e1
Switched to session a7f3c2e1

> /status
Session:  a7f3c2e1
Branches: def456 (at event 7)
Events:   7 (3 tool calls, 2 responses)
Status:   The original channel-based refactor attempt

> /quit
Goodbye.
```

### Key points

| Command | Effect |
|---------|--------|
| `/branch` | Fork a new session from the current point; original history is untouched |
| `/switch <id>` | Jump to a different session in the same process |
| `/status` | Show current session info and any branches |

Branching is useful when you want to try a risky change without losing a working conversation. The parent session remains at its last save point, and you can switch back and forth freely.

# JSON Mode

Machine-readable newline-delimited JSON protocol for editor and CMS integration.

```bash
$ racket main.rkt --json --model gpt-5.4
{"type":"ready","session":"b9d4e8a2","model":"gpt-5.4"}
{"type":"prompt","content":"Explain event-bus.rkt"}
{"type":"tool_call","id":"tc_01","tool":"read","args":{"path":"src/core/event-bus.rkt"}}
{"type":"tool_result","id":"tc_01","content":"#lang racket\n\n(provide make-event-bus ...)\n\n(struct event-bus (subscribers lock) ...)\n\n(define (bus-publish bus type payload)\n  ...)\n\n(define (bus-subscribe bus type handler)\n  ...)"}
{"type":"response","content":"The event bus is a lightweight pub/sub system...\n\n- `make-event-bus` creates a new bus with an empty subscriber table.\n- `bus-subscribe` registers a handler for a specific event type (or `#f` for wildcard).\n- `bus-publish` dispatches an event to all matching subscribers.\n\nSubscribers are invoked synchronously inside a thread-safe lock. Errors in one handler are isolated so they don't block other subscribers."}
{"type":"prompt","content":"Write a test for wildcard subscriptions"}
{"type":"tool_call","id":"tc_02","tool":"write","args":{"path":"tests/core/event-bus-wildcard-test.rkt","content":"#lang racket\n..."}}
{"type":"tool_call","id":"tc_03","tool":"bash","args":{"command":"raco test tests/core/event-bus-wildcard-test.rkt"}}
{"type":"tool_result","id":"tc_02","content":"wrote 1.4 KB"}
{"type":"tool_result","id":"tc_03","content":"2 tests passed in 0.02s"}
{"type":"response","content":"Created `tests/core/event-bus-wildcard-test.rkt` — 2 tests, both passing."}
{"type":"prompt","content":"/save"}
{"type":"session_saved","session":"b9d4e8a2","path":"/home/user/.q/sessions/b9d4e8a2.jsonl","events":10,"bytes":4210}
{"type":"prompt","content":"/quit"}
{"type":"bye"}
```

### Event types

| Event | Direction | Description |
|-------|-----------|-------------|
| `ready` | server → client | Session is initialised |
| `prompt` | client → server | User sends a message or `/command` |
| `tool_call` | server → client | Agent invokes a tool |
| `tool_result` | server → client | Tool output |
| `response` | server → client | Agent's text response |
| `session_saved` | server → client | Confirmation of `/save` |
| `bye` | server → client | Session ended (`/quit`) |

Every line is a complete JSON object. There are no multi-line payloads — newlines inside content fields are escaped as `\n`.

### Usage patterns

```bash
# Pipe a prompt and capture the response
echo '{"type":"prompt","content":"What does agent.rkt do?"}' \
  | racket main.rkt --json --model gpt-5.4 \
  | jq -c 'select(.type=="response")'

# Drive q from an editor plugin over stdio
```

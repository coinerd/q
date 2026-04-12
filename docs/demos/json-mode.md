# JSON Mode

Machine-readable newline-delimited JSON protocol for editor and CMS integration.

```bash
$ raco q --json --model gpt-5.4
{"type":"ready","session":"b9d4e8a2","model":"gpt-5.4"}
{"type":"prompt","content":"Explain event-bus.rkt"}
{"type":"tool_call","id":"tc_01","tool":"read","args":{"path":"agent/event-bus.rkt"}}
{"type":"tool_result","id":"tc_01","content":"#lang racket/base\n\n(provide make-event-bus event-bus? subscribe! unsubscribe! publish! ...)\n\n(struct event-bus (subscribers semaphore) ...)\n\n(define (publish! bus type payload)\n  ...)\n\n(define (subscribe! bus type handler)\n  ...)"}
{"type":"response","content":"The event bus is a lightweight pub/sub system...\n\n- `make-event-bus` creates a new bus with an empty subscriber table.\n- `subscribe!` registers a handler for a specific event type (or `#f` for wildcard).\n- `publish!` dispatches an event to all matching subscribers.\n\nSubscribers are invoked synchronously inside a thread-safe lock. Errors in one handler are isolated so they don't block other subscribers."}
{"type":"prompt","content":"Write a test for wildcard subscriptions"}
{"type":"tool_call","id":"tc_02","tool":"write","args":{"path":"tests/test-event-bus-wildcard.rkt","content":"#lang racket\n..."}}
{"type":"tool_call","id":"tc_03","tool":"bash","args":{"command":"raco test tests/test-event-bus-wildcard.rkt"}}
{"type":"tool_result","id":"tc_02","content":"wrote 1.4 KB"}
{"type":"tool_result","id":"tc_03","content":"2 tests passed in 0.02s"}
{"type":"response","content":"Created `tests/test-event-bus-wildcard.rkt` — 2 tests, both passing."}
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
echo '{"type":"prompt","content":"What does loop.rkt do?"}' \
  | raco q --json --model gpt-5.4 \
  | jq -c 'select(.type=="response")'

# Drive q from an editor plugin over stdio
```

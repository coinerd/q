# Q Event Taxonomy Reference

Complete reference for all event types in Q v0.12.0.

## Base Types

### `event` (protocol-level)
All events on the bus are wrapped in an `event` struct:

| Field | Type | Description |
|-------|------|-------------|
| `version` | integer | Protocol version (currently 1) |
| `ev` | string | Event name (e.g., "turn.started") |
| `time` | real | Unix timestamp |
| `session-id` | string | Session identifier |
| `turn-id` | string | Turn identifier |
| `payload` | any | Event-specific data (hash or typed-event struct) |

### `typed-event` (structural base)
Typed event structs inherit from `typed-event`:

| Field | Type | Description |
|-------|------|-------------|
| `type` | string | Event category name |
| `timestamp` | real | Unix timestamp |
| `session-id` | string | Session identifier |
| `turn-id` | string | Turn identifier |

## Turn Lifecycle

### `turn-start-event`
Emitted when an agent turn begins.

| Field | Type | Description |
|-------|------|-------------|
| `model` | string | Model name |
| `provider` | string | Provider name |

### `turn-end-event`
Emitted when a turn completes.

| Field | Type | Description |
|-------|------|-------------|
| `termination` | symbol | `'completed`, `'cancelled`, `'tool-calls-pending` |

## Message Streaming

### `message-start-event`
Emitted when a message begins streaming.

| Field | Type | Description |
|-------|------|-------------|
| `role` | symbol | `'assistant`, `'user` |
| `model` | string | Model name |

### `message-update-event`
Emitted for each streaming chunk.

| Field | Type | Description |
|-------|------|-------------|
| `content` | symbol | Content type |
| `delta` | string | Text delta |

### `message-end-event`
Emitted when a message completes streaming.

| Field | Type | Description |
|-------|------|-------------|
| `role` | symbol | Message role |
| `reason` | string | Stop reason |

## Tool Execution

### `tool-execution-start-event`
Emitted before a tool begins execution.

| Field | Type | Description |
|-------|------|-------------|
| `tool-name` | string | Tool name |
| `tool-call-id` | string | Unique call ID |

### `tool-execution-update-event`
Emitted during tool execution with progress.

| Field | Type | Description |
|-------|------|-------------|
| `tool-name` | string | Tool name |
| `progress` | string | Progress info |

### `tool-execution-end-event`
Emitted when a tool completes.

| Field | Type | Description |
|-------|------|-------------|
| `tool-name` | string | Tool name |
| `duration-ms` | real | Execution duration |
| `result-summary` | any | Result data |

## Per-Tool Typed Events

All per-tool events inherit from `tool-call-event`:

| Field | Type | Description |
|-------|------|-------------|
| `tool-name` | string | Tool name |
| `arguments` | any | Tool arguments |
| `tool-call-id` | string | Call ID |

### `bash-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `command` | string | Shell command |
| `timeout` | real | Timeout in seconds |
| `cwd` | string | Working directory |

### `edit-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `path` | string | File path |
| `edits` | list | Edit operations |

### `write-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `path` | string | File path |
| `content` | string | File content |

### `read-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `path` | string | File path |
| `offset` | integer | Start line |
| `limit` | integer | Max lines |

### `grep-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `pattern` | string | Search pattern |
| `path` | string | Search directory |
| `glob` | string | File glob |

### `find-tool-call-event`
| Field | Type | Description |
|-------|------|-------------|
| `pattern` | string | File pattern |
| `path` | string | Search directory |

### `custom-tool-call-event`
No additional fields. Use `tool-call-event-tool-name` and `tool-call-event-arguments` from parent.

## Provider Events

### `provider-request-event`
| Field | Type | Description |
|-------|------|-------------|
| `model` | string | Requested model |
| `messages` | integer | Message count |

### `provider-response-event`
| Field | Type | Description |
|-------|------|-------------|
| `model` | string | Responding model |
| `status` | symbol | Response status |

## Session Events

### `session-start-event`
| Field | Type | Description |
|-------|------|-------------|
| `session-id` | string | Session ID |
| `model` | string | Model name |

### `session-shutdown-event`
| Field | Type | Description |
|-------|------|-------------|
| `session-id` | string | Session ID |
| `reason` | symbol | Shutdown reason |

## Tree Events

### `tree-navigation-entry`
| Field | Type | Description |
|-------|------|-------------|
| `entry-id` | string | Entry ID |
| `parent-entry-id` | string | Source entry |
| `target-entry-id` | string | Target entry |

### `branch-entry`
| Field | Type | Description |
|-------|------|-------------|
| `entry-id` | string | Branch ID |
| `parent-entry-id` | string | Parent entry |
| `branch-name` | string | Branch name |

### `branch-summary-entry`
| Field | Type | Description |
|-------|------|-------------|
| `text` | string | Summary text |
| `entry-range` | list | Start/end entries |
| `token-count` | integer | Token count |

## Hook Points (47 total)

Key hook points where extensions can intercept:

| Hook | When | Can Block? |
|------|------|-----------|
| `agent-start` | Before LLM call | Yes |
| `agent-end` | After LLM call | No |
| `message-start` | Before message stream | Yes |
| `message-update` | During streaming | Yes |
| `message-end` | After message complete | No |
| `tool-call` | Before tool execution | Yes |
| `tool-result` | After tool execution | No |
| `session-start` | Session opened | No |
| `session-shutdown` | Session closed | No |
| `session-before-fork` | Before fork | Yes |
| `session-rebind` | Session switch | No |
| `context-reduce` | Before compaction | Yes |
| `resources-discover` | Extension resource scan | No |
| `input` | User input received | No |

For the complete list, see `util/hook-types.rkt`.

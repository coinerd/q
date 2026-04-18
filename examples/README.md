# Extension Examples

A gallery of canonical extension examples for the q agent platform.
Each example demonstrates specific extension API patterns.

## Getting Started

Extensions are Racket modules that provide a `the-extension` binding.
They register hook handlers for named dispatch points in the agent loop.

```racket
#lang racket/base

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "my-extension"
             "1.0.0"
             "1"
             (hasheq 'turn-start
                     (lambda (payload)
                       (hook-pass payload)))))
```

## Examples Index

| # | File | Demonstrates |
|---|------|-------------|
| 1 | [hello-world.rkt](extensions/hello-world.rkt) | Minimal extension, turn-start hook |
| 2 | [custom-tool.rkt](extensions/custom-tool.rkt) | registerTools hook, JSON schema, tool execution |
| 3 | [custom-command.rkt](extensions/custom-command.rkt) | input hook, slash commands, argument parsing |
| 4 | [context-enricher.rkt](extensions/context-enricher.rkt) | context hook, context assembly |
| 5 | [tool-guard.rkt](extensions/tool-guard.rkt) | tool-call hook (critical), blocking tools |
| 6 | [response-logger.rkt](extensions/response-logger.rkt) | turn-end hook, observation patterns |
| 7 | [session-lifecycle.rkt](extensions/session-lifecycle.rkt) | before-agent-start, session-before-switch hooks |
| 8 | [streaming-observer.rkt](extensions/streaming-observer.rkt) | tool.execution.start/end streaming hooks |
| 9 | [provider-hook.rkt](extensions/provider-hook.rkt) | before-provider-request hook |
| 10 | [multi-hook.rkt](extensions/multi-hook.rkt) | Multiple hooks in one extension |
| 11 | [define-extension-dsl.rkt](extensions/define-extension-dsl.rkt) | define-q-extension macro DSL |
| 12 | [error-handler.rkt](extensions/error-handler.rkt) | Error resilience, critical vs advisory hooks |

## Hook Reference

### Hook Actions
Every hook handler must return one of:
- `(hook-pass payload)` — continue without modification
- `(hook-amend new-payload)` — replace the payload and continue
- `(hook-block reason)` — stop dispatch and abort the action

### Critical vs Advisory Hooks
- **Critical** hooks default to `block` on handler error (safety-first)
- **Advisory** hooks default to `pass` on handler error (liveness-first)
- Critical hooks: `tool-call`, `session-before-switch`, `session-before-fork`, `session-before-compact`, `input`

### Available Hook Points

| Hook | Actions | Description |
|------|---------|-------------|
| `turn-start` | pass, amend, block | Before each agent turn |
| `turn-end` | pass, amend | After each agent turn |
| `tool-call` | pass, amend, block | Before tool execution (critical) |
| `tool-result` | pass, amend, block | After tool execution |
| `input` | pass, amend, block | User input interception (critical) |
| `context` | pass, amend, block | After context assembly |
| `before-provider-request` | pass, amend, block | Before LLM API call |
| `register-tools` | pass, amend | Register extension tools |
| `session-before-switch` | pass, block | Before session resume (critical) |
| `before-agent-start` | pass, amend, block | Before agent loop starts |
| `tool.execution.start` | pass | Tool batch starts (observation) |
| `tool.execution.end` | pass | Tool batch completes (observation) |

## Running Tests

```bash
racket examples/tests/test-examples.rkt
```

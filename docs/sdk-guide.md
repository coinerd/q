# Q SDK Guide

Programmatic usage of the Q agent as an embeddable library.

## Quick Start

```racket
#lang racket

(require "interfaces/sdk.rkt")

;; Create a session with defaults
(define rt (create-agent-session
            #:provider (hasheq 'type 'test)))

;; Get session info
(session-info rt)
;; => #hasheq((session-id . "...") (active? . #t) ...)
```

## Configuration Options

### Provider Selection

```racket
(create-agent-session
 #:provider (hasheq 'type 'openai
                    'api-key "sk-..."
                    'model "gpt-4"))
```

### Custom Model and Thinking Level

```racket
(create-agent-session
 #:provider (hasheq 'type 'test)
 #:model-name "claude-sonnet-4-20250514"
 #:thinking-level 'high)
```

Options: `'off`, `'low`, `'medium`, `'high`

### System Instructions

```racket
(create-agent-session
 #:provider (hasheq 'type 'test)
 #:system-instructions
 '("You are a helpful coding assistant."
   "Always write tests first."))
```

### Custom Tool Registry

```racket
(require "tools/tool.rkt")

(define registry (make-tool-registry))
(register-tool! registry
                (hash 'name 'my-tool
                      'description "A custom tool"
                      'parameters '()))

(create-agent-session
 #:provider (hasheq 'type 'test)
 #:tool-registry registry)
```

## Event Subscription

```racket
;; Subscribe to all events
(define sub-id
  (subscribe-events! rt
                     (lambda (event)
                       (printf "Event: ~a\n" (event-ev event)))))

;; Unsubscribe
(unsubscribe! (runtime-config-event-bus (rt-cfg rt)) sub-id)
```

## Session Management

### In-Memory Sessions

```racket
(require "runtime/session-manager.rkt")

(define mgr (make-in-memory-session-manager))

(create-agent-session
 #:provider (hasheq 'type 'test)
 #:session-manager mgr)

;; List sessions
(sm-list mgr) ;; => '("session-1" "session-2")

;; Load session entries
(sm-load mgr "session-1")

;; Fork a session
(sm-fork! mgr "session-1" "fork-1" "entry-id")
```

### Persistent Sessions

```racket
(define mgr (make-persistent-session-manager "/path/to/sessions"))

(sm-append! mgr "my-session"
            (make-message "m1" #f 'user 'message '() 0 (hasheq)))
```

## Session Tree Operations

### Branch

```racket
(q:session-branch rt)
;; => #hasheq((branch-id . "...") (parent-entry-id . "...") (branch-name . "unnamed"))

(q:session-branch rt "some-entry-id" "my-branch")
;; => #hasheq((branch-id . "...") (parent-entry-id . "...") (branch-name . "my-branch"))
```

### Navigate

```racket
(q:session-navigate rt "target-entry-id")
;; => #hasheq((navigation-id . "...") (from-entry-id . "...") (target-entry-id . "..."))
```

### Tree Info

```racket
(q:session-tree-info rt)
;; => #hasheq((total-entries . 5) (branch-count . 2) (navigation-count . 1)
;;            (summary-count . 0) (leaf-ids . '("id1" "id2")))
```

## Full Dependency Injection

```racket
(define bus (make-event-bus))
(define registry (make-tool-registry))
(define mgr (make-in-memory-session-manager))

(create-agent-session
 #:provider (hasheq 'type 'test)
 #:event-bus bus
 #:tool-registry registry
 #:session-manager mgr
 #:max-iterations 20
 #:system-instructions '("You are an expert coder.")
 #:token-budget-threshold 80000)
```

## SDK API Reference

| Function | Description |
|----------|-------------|
| `create-agent-session` | Create session with full config |
| `run-prompt!` | Send prompt and get response |
| `session-info` | Get session metadata |
| `subscribe-events!` | Subscribe to event bus |
| `unsubscribe!` | Remove subscription |
| `interrupt!` | Cancel current operation |
| `fork-session!` | Fork at entry |
| `q:session-branch` | Create branch |
| `q:session-navigate` | Navigate to entry |
| `q:session-tree-info` | Tree metadata |

## GSD Planning via SDK

The SDK provides convenience functions for GSD (Goal-driven Structured Development) planning workflows. These wrap the `/plan` and `/go` slash commands into simple function calls.

### Quick Start

```racket
(require "interfaces/sdk.rkt")

;; 1. Create a runtime with extensions
(define rt (make-runtime #:provider my-provider
                          #:session-dir "/tmp/my-session"
                          #:auto-load-extensions? #t
                          #:project-dir "/path/to/project"))

;; 2. Open session (extensions pre-registered automatically)
(define rt-open (open-session rt))

;; 3. Plan a task
(define-values (rt2 result) (q:plan rt-open "build a hello world module"))

;; 4. Execute the plan
(define-values (rt3 result2) (q:go rt2))
```

### GSD Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `q:plan` | `(runtime? string?) → (values runtime? any/c)` | Dispatch `/plan` task, run planning prompt |
| `q:go` | `(runtime? [wave #f]) → (values runtime? any/c)` | Execute plan, optionally starting at a specific wave |
| `q:gsd-status` | `→ (or/c 'no-active-session hash?)` | Get current GSD state snapshot |
| `q:reset-gsd!` | `→ void?` | Reset all GSD planning state |

### Extension Pre-Registration

As of v0.20.5, `open-session` automatically registers extension tools (like `planning-read` and `planning-write`) before the first `run-prompt!` call. This means:

- Extension tools appear in `list-tools` immediately after `open-session`
- GSD event bus is initialized and ready to emit events
- No need to call `run-prompt!` once just to trigger tool registration

### Events

GSD operations emit events to the runtime's event bus:

- `gsd.mode.changed` — emitted when GSD mode transitions (planning → plan-written → executing)

Subscribe with:

```racket
(subscribe-events! rt
  (lambda (event)
    (printf "GSD event: ~a~n" (event-ev event))))
```

## Examples

See `examples/sdk/` for 8 graduated examples from minimal to full control.

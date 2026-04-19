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

## Examples

See `examples/sdk/` for 8 graduated examples from minimal to full control.

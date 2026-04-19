# Q Session Tree Guide

The session tree model enables branching, navigation, and summarization
of agent sessions. This allows exploration of alternative approaches,
backtracking, and efficient context management.

## Concepts

### Tree Structure
Each session is a tree of entries:
- **Branch entries** mark fork points where the session diverged
- **Navigation entries** record movements between branches
- **Summary entries** store condensed branch content

### Entry Types

| Type | Description |
|------|-------------|
| `message` | User/assistant messages |
| `tool-call` | Tool invocations and results |
| `branch-entry` | Branch creation marker |
| `tree-navigation-entry` | Branch navigation record |
| `branch-summary-entry` | Condensed branch content |

## SDK Operations

### Branch a Session

Create a new branch at the current position:

```racket
(require "interfaces/sdk.rkt")

;; Branch at current position
(q:session-branch rt)
;; => #hasheq((branch-id . "branch-1700000000")
;;            (parent-entry-id . "session-123")
;;            (branch-name . "unnamed"))

;; Branch at specific entry with name
(q:session-branch rt "entry-456" "exploration-branch")
```

### Navigate Between Branches

Move to a different entry in the tree:

```racket
(q:session-navigate rt "target-entry-id")
;; => #hasheq((navigation-id . "nav-1700000001")
;;            (from-entry-id . "session-123")
;;            (target-entry-id . "target-entry-id"))
```

### Query Tree Structure

Get metadata about the session tree:

```racket
(q:session-tree-info rt)
;; => #hasheq((total-entries . 42)
;;            (branch-count . 3)
;;            (navigation-count . 5)
;;            (summary-count . 1)
;;            (leaf-ids . ("id1" "id2" "id3")))
```

## Low-Level Operations

### Append Tree Entries

```racket
(require "runtime/session-store.rkt"
         "util/protocol-types.rkt")

;; Create a branch entry
(define branch (make-branch-entry "b1" "parent-id" "my-branch"))
(append-tree-entry! "/path/to/session.jsonl" branch)

;; Create a navigation entry
(define nav (make-tree-navigation-entry "n1" "from-id" "to-id"))
(append-tree-entry! "/path/to/session.jsonl" nav
                    #:before-hook (lambda (e) (printf "Before: ~a\n" e))
                    #:after-hook (lambda (e) (printf "After: ~a\n" e)))
```

### Load Tree Structure

```racket
(define tree (load-tree "/path/to/session.jsonl"))
;; tree is a hash with tree metadata

(tree-info tree)
;; => #hasheq((total-entries . 42) (branch-count . 3) ...)
```

### Fork a Session

```racket
(fork-session! "/path/to/source.jsonl" "entry-id" "/path/to/fork.jsonl")
;; Copies root→entry path to a new session file
```

## Session Manager Integration

The unified session manager supports tree operations:

```racket
(require "runtime/session-manager.rkt")

;; In-memory tree operations
(define mgr (make-in-memory-session-manager))
(sm-append! mgr "s1" (make-branch-entry "b1" "root" "branch-a"))
(sm-fork! mgr "s1" "s2" "b1")

;; Persistent tree operations
(define pmgr (make-persistent-session-manager "/sessions"))
(sm-append! pmgr "s1" (make-message "m1" #f 'user 'message '() 0 (hasheq)))
```

## Hook Events

Tree operations emit events when hooks are provided:

| Event | When |
|-------|------|
| `session-before-tree` | Before tree entry appended |
| `session-tree` | After tree entry appended |

Wire to dispatch-hooks at the orchestration layer:

```racket
(append-tree-entry! log-path entry
  #:before-hook (lambda (e)
    (dispatch-hooks 'session-before-tree e ext-reg #:ctx ctx))
  #:after-hook (lambda (e)
    (dispatch-hooks 'session-tree e ext-reg #:ctx ctx)))
```

## Use Cases

### 1. Exploration Branching
Branch before trying a risky refactoring approach. If it doesn't work,
navigate back and try a different approach.

### 2. Checkpoint + Summarize
Branch at a stable point, summarize the branch, then continue. The
summary replaces the full context in future turns.

### 3. A/B Testing Prompts
Branch the session, apply different system prompts to each branch,
and compare results.

## Examples

See `examples/sdk/07-tree.rkt` for a complete tree operations example.

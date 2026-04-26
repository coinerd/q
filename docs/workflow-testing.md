# Workflow Testing Guide

This guide covers q's workflow-level integration tests that verify
complete agent behaviors end-to-end.

## Test Structure

```
tests/workflows/
├── fixtures/          # Shared test infrastructure
│   ├── mock-provider.rkt
│   ├── workflow-runner.rkt
│   ├── temp-project.rkt
│   └── event-recorder.rkt
├── gsd/               # GSD planning workflow tests
│   ├── test-planning-workflow.rkt
│   ├── test-bug-analysis-workflow.rkt
│   ├── test-milestone-delivery-workflow.rkt
│   └── test-self-hosting-validation.rkt
├── extensions/        # Extension integration tests
│   ├── test-extension-round-trip.rkt
│   ├── test-extension-execution-hardening.rkt
│   ├── test-sdk-extension-loading.rkt
│   └── test-extension-hooks.rkt
├── safety/            # Safety boundary tests
│   └── test-safe-mode-boundary.rkt
└── dogfood/           # Self-hosting dogfood tests
    ├── tasks/         # Dogfood task definitions
    ├── test-dogfood-infrastructure.rkt
    └── test-dogfood-execution-analysis.rkt
```

## Key Concepts

### Mock Provider

`make-scripted-provider` creates a deterministic provider that returns
pre-scripted responses. Each response is a hash with:

- `content` — The text response
- `tool_calls` — Optional list of tool call specifications

```racket
(make-scripted-provider
 (list (hasheq 'content "I'll read the file."
               'tool_calls
               (list (hasheq 'id "tc-1"
                             'type "function"
                             'function
                             (hasheq 'name "read"
                                     'arguments "{\"path\":\"test.txt\"}"))))
       (hasheq 'content "Done reading."))
 #:name "my-test")
```

### Workflow Runner

`run-workflow` creates a full SDK runtime, runs a prompt, and returns
a `workflow-result` struct:

```racket
(struct workflow-result
  (output         ; loop-result? from SDK
   events         ; event-recorder? with captured events
   session-log    ; path? to session.jsonl
   session-id     ; string?
   project-dir    ; path? (or #f)
   session-dir    ; path? to session base dir
   runtime        ; sdk:runtime?
   side-effects)  ; (listof string)
  #:transparent)
```

### Extension Loading in Tests

```racket
(define ext-reg (make-extension-registry))
(define bus (make-event-bus))
(load-extension! ext-reg "/path/to/extension.rkt" #:event-bus bus)
```

Use relative paths from the test file:
```racket
(define ext-path
  (build-path (or (current-load-relative-directory) (current-directory))
              "../../../extensions/gsd-planning.rkt"))
```

### Event Recording

```racket
(define bus (make-event-bus))
(define recorder (make-event-recorder bus))
;; ... run workflow ...
(define events (recorded-events recorder))
```

## Mock Provider Patterns

### Single-turn response
```racket
(make-scripted-provider
 (list (hasheq 'content "Done."))
 #:name "simple")
```

### Tool call then response
```racket
(make-scripted-provider
 (list (hasheq 'content "Reading..."
               'tool_calls (list tool-call-spec))
       (hasheq 'content "Result: ..."))
 #:name "tool-user")
```

## Test Conventions

1. Use `with-temp-dir` for file system operations
2. Use `parameterize ([current-directory dir])` when testing planning tools
3. Always check `workflow-result?` before accessing fields
4. Use `(check-true ...)` for boolean assertions
5. Keep tests independent — no shared mutable state between test-cases

## Version 0.20.1

This documentation reflects q 0.20.1.

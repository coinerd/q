# Self-Hosting Guide

q is a self-hosting agent: it uses its own GSD (Get Stuff Done) planning workflow
to develop and maintain itself. This guide explains how q dogfoods its own tools.

## Overview

The self-hosting cycle consists of:

1. **Plan** — Create `.planning/PLAN.md` with atomic waves
2. **Implement** — Execute each wave as a focused task
3. **Review** — Verify implementation with `q-gsd-reviewer`
4. **Release** — Bump version, tag, create GitHub release

## GSD Planning Artifacts

All planning state lives in `.planning/` at the project root:

| File | Purpose |
|------|---------|
| `PLAN.md` | Current wave plan with tasks and verification |
| `STATE.md` | Current project status and progress |
| `VALIDATION.md` | Verification mapping and test status |
| `SUMMARY.md` | Concise progress summary |
| `HANDOFF.json` | Machine-readable handoff state for context switches |

### Creating Artifacts

Use the `planning-write` tool (provided by the GSD extension):

```
planning-write: action=write, name=PLAN, content="..."
```

### Reading Artifacts

```
planning-read: action=read, name=PLAN
```

## Workflow Testing

q includes a workflow test harness at `tests/workflows/` that tests
complete agent workflows end-to-end.

### Fixtures

- `tests/workflows/fixtures/workflow-runner.rkt` — Full SDK runtime wrapper
- `tests/workflows/fixtures/mock-provider.rkt` — Deterministic mock provider
- `tests/workflows/fixtures/temp-project.rkt` — Temporary project directories
- `tests/workflows/fixtures/event-recorder.rkt` — Event bus recording

### Writing Workflow Tests

```racket
(require "../fixtures/mock-provider.rkt"
         "../fixtures/workflow-runner.rkt")

(define provider
  (make-scripted-provider
   (list (hasheq 'content "Response text"
                 'tool_calls (list ...)))
   #:name "test-provider"))

(define result
  (run-workflow provider "Prompt text"
                #:extension-registry ext-reg
                #:max-iterations 5))

(check-true (workflow-result? result))
```

## Dogfood Infrastructure

### Task Format

Dogfood tasks are JSON files in `tests/workflows/dogfood/tasks/`:

```json
{
  "name": "task-name",
  "description": "What this task tests",
  "prompt": "The prompt to send to the agent",
  "expected_tools": ["read", "write"],
  "expected_outcome": "Description of expected result",
  "max_iterations": 5,
  "setup": {},
  "teardown": ["file.txt"]
}
```

### Running Dogfood Sessions

```bash
# Mock execution (no real LLM)
racket scripts/run-dogfood-session.rkt --mock tests/workflows/dogfood/tasks/basic-file-ops.json

# Capture regression baseline
racket scripts/capture-regression.rkt baseline <trace.jsonl>

# Compare against baseline
racket scripts/capture-regression.rkt compare <trace.jsonl> <baseline.json>
```

### Scripts

| Script | Purpose |
|--------|---------|
| `scripts/run-dogfood-session.rkt` | Execute dogfood tasks with mock or live provider |
| `scripts/capture-regression.rkt` | Capture baselines and detect regressions |
| `scripts/analyze-trace.rkt` | Extract metrics from session traces |
| `scripts/sync-version.rkt` | Sync version across source files |
| `scripts/lint-version.rkt` | Verify version consistency |

## Extensions

q extensions provide additional tools and commands. The GSD planning extension
is the primary self-hosting extension:

- **gsd-planning.rkt** — Planning artifact read/write tools
- **context.rkt** — Context window management
- **dynamic-tools.rkt** — Dynamic tool registration
- **hooks.rkt** — Lifecycle event hooks

### Loading Extensions

```racket
(define ext-reg (make-extension-registry))
(define bus (make-event-bus))
(load-extension! ext-reg "path/to/extension.rkt" #:event-bus bus)
```

## Version 0.20.4

This documentation reflects q 0.20.4.

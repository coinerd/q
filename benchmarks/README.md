# q Benchmark Harness

Lightweight benchmark harness for measuring q's coding-agent task performance.

## What It Measures

The benchmark harness evaluates q's agent loop across five core coding-agent capabilities:

| Task | Capability | Description |
|------|-----------|-------------|
| `explain-code` | Comprehension | Explain a module in natural language |
| `write-test` | Test generation | Generate a rackunit test for an existing function |
| `edit-function` | Code editing | Add a new function to an existing module |
| `search-codebase` | Navigation | Find files importing from a given module |
| `session-resume` | Continuity | Resume a session and summarize prior context |

For each task, the harness captures:
- **Status**: `completed` or `error`
- **Elapsed time**: wall-clock milliseconds
- **Turns**: number of agent turn iterations
- **Tool calls**: tool invocations during execution
- **Validation**: whether the output passes task-specific checks

## Running Benchmarks

### Mock mode (default)

No API keys required. Uses deterministic mock responses that exercise the full SDK pipeline (runtime → session → agent loop → event tracking):

```bash
racket q/benchmarks/run.rkt
```

### Verbose mode

Shows per-task progress:

```bash
racket q/benchmarks/run.rkt --verbose
```

### Real provider mode

Routes prompts through a live LLM provider (requires provider configuration):

```bash
racket q/benchmarks/run.rkt --real
```

## Example Output

```
q benchmark harness
Mode: mock (deterministic, no API calls)

=== Benchmark Results ===
Task                   Status    Time       Turns  Valid
---------------------------------------------------------
explain-code           completed 42ms       1      pass
write-test             completed 38ms       1      pass
edit-function          completed 35ms       1      pass
search-codebase        completed 36ms       1      pass
session-resume         completed 31ms       1      pass

Total: 5 tasks, 5 passed, 0 failed, 0 errors
Total time: 182ms
```

## Architecture

```
q/benchmarks/
├── run.rkt      — Entry point, CLI, report formatting
├── tasks.rkt    — Task definitions and validation predicates
├── metrics.rkt  — Metrics and result data structures
└── README.md    — This file
```

The harness uses q's public SDK API (`interfaces/sdk.rkt`) rather than internal modules, ensuring benchmarks reflect the real integration surface.

### Mock Provider Strategy

Each task gets a tailored mock response that produces plausible output for its category. This means:
- The full SDK pipeline runs (session creation, prompt dispatch, event tracking)
- Validation predicates check against realistic response shapes
- Results are deterministic and reproducible
- No network calls or API costs

## Adding New Benchmark Tasks

1. Add a `benchmark-task` entry in `tasks.rkt`:

```racket
(benchmark-task
 'your-task-name
 "Short description of what it tests"
 "The prompt to send to the agent"
 your-validation-predicate)
```

2. Add a matching mock response in `run.rkt`'s `make-benchmark-mock-provider` (under the `case` form).

3. Write a validation predicate — a function `(string? -> boolean?)` that checks whether the output looks correct. Use helpers like `contains-any` or write custom logic.

4. Run: `racket q/benchmarks/run.rkt --verbose` to verify.

## Methodology and Limitations

### What this measures
- **End-to-end SDK latency**: session creation + prompt dispatch + event overhead
- **Event pipeline correctness**: turn/tool-call/token tracking via event bus
- **Validation coverage**: output quality checks per task category

### What this does NOT measure (v1)
- **Real LLM quality**: mock responses are canned, not generated
- **Tool execution**: mock mode doesn't run actual file edits or searches
- **Concurrency**: tasks run sequentially
- **Memory usage**: not tracked in v1
- **Streaming latency**: mock provider delivers instantly

### Future improvements
- `--real` mode integration with provider factory (requires config)
- Token-accurate mock responses for cost estimation
- Statistical analysis (mean, p95, stddev over N runs)
- JSON output format for CI integration

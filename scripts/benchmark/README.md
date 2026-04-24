# q Benchmark Suite

Automated benchmarking for the q coding agent. Measures agent performance
across structured tasks with reproducible scoring.

## Usage

### Full suite (live)

```bash
racket scripts/run-benchmark.rkt --live \
  --task-dir scripts/benchmark/tasks/ \
  --output-dir traces/ \
  --json \
  --provider openai
```

### Single task

```bash
racket scripts/run-benchmark.rkt --live \
  --task-dir scripts/benchmark/tasks/ \
  --task edit-refactor \
  --provider anthropic
```

### Mock mode (no API calls)

Runs tasks against a deterministic stub provider — useful for CI smoke
tests and local development without burning tokens.

```bash
racket scripts/run-benchmark.rkt \
  --task-dir scripts/benchmark/tasks/ \
  --output-dir traces/ \
  --json
```

### CI

Benchmarks run automatically on version tags (`v*`) via the
`.github/workflows/benchmark.yml` workflow. Use **workflow_dispatch** to
trigger manual runs with a chosen provider.

## Scoring Dimensions

Each task is scored 0–10 on five independent axes:

| Dimension | Description |
|---|---|
| **Correctness** | Does the output satisfy the task specification? |
| **Tool Use** | Are tools invoked efficiently and appropriately? |
| **Reasoning** | Is the chain-of-thought clear and logically sound? |
| **Efficiency** | Token usage and step count relative to an optimal solution |
| **Robustness** | Handling of edge cases, errors, and ambiguous inputs |

A weighted composite score (0–100) is computed per task and aggregated
across the suite.

## Example Output

```
Task: edit-refactor
  Correctness:   9/10
  Tool Use:      8/10
  Reasoning:     9/10
  Efficiency:    7/10
  Robustness:    8/10
  Composite:     82/100

Task: multi-file-feature
  Correctness:   8/10
  Tool Use:      9/10
  Reasoning:     8/10
  Efficiency:    6/10
  Robustness:    7/10
  Composite:     76/100

=== Suite Summary ===
Tasks:     2
Average:   79.0
Duration:  47s
```

JSON output is written to `traces/` alongside a Markdown summary for
the GitHub Step Summary panel.

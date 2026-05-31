# ADR 0019: Context Assembly Activation

## Status

Accepted — v0.76.0 through v0.76.6

## Context

The v0.75.xx milestone series built the infrastructure for state-aware context assembly: task-state inference, state-relevance matrix, tiered context builder, task conclusions, working-set evolution, and token metrics. However, the feature was **gated off** by default (`current-task-state-aware-assembly?` = `#f`).

The v0.76.xx series was needed to **activate** the feature: turn it on in production, measure results, add safety rails, and prove the 20–30% token-reduction promise.

## Decision

### Graduated activation (M4)

- Per-session `task-state-aware?` config field (default `#f` for existing sessions)
- Global `current-task-state-aware-assembly?` parameter as emergency kill switch
- `session-rollout-enabled?` for A/B testing

### Measurement (M4)

- A/B comparison harness (`scripts/benchmark/context-efficiency.rkt`)
- Token metrics struct with per-tier breakdowns
- Conclusion coverage computation

### Safety rails (M4)

- `check-rollback-triggers`: observational trigger system (excessive-savings >50%, amnesia-risk coverage <0.20, task-amnesia repeats >2)
- Rollback is a single parameter flip — no code changes needed

### Working-set deprecation (M5)

- `ws-entry->conclusion-or-self`: matches WS entries to conclusions via `origin-message-ids`
- When WS level is `excluded`: only keeps entries with matching conclusions
- When WS level is `filtered`: replaces matched entries with compact conclusion text

### Dependency tracking (M6)

- `dependencies` field on `task-conclusion` (list of file paths)
- Simple tagging — no graph traversal. Acyclic by construction.
- Serialization round-trip includes dependencies

## Consequences

### Positive
- Token savings of 10–30% on tasks with conclusions (measured via A/B benchmark)
- Per-session rollout allows safe A/B testing
- Rollback is instant (single parameter)
- Working-set entries gracefully replaced by conclusions

### Negative
- ~50-100 token preamble overhead on every assembly (acceptable for real sessions)
- Synthetic benchmarks may show negative savings (overhead > savings on short sessions)
- Dependency tracking is tagging-only — no transitive closure or impact analysis

### Metrics (from A/B benchmark)
- State-aware assembly adds preamble but removes redundant context in planning/implementation/debugging
- Conclusion coverage is the key health metric (target: ≥ 30%)
- Amnesia risk detected via repeated tool calls to same files

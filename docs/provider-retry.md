# Provider Retry Configuration

q automatically retries transient provider errors (timeouts, rate limits, 5xx
server errors) with exponential backoff and full jitter.

## Circuit Breaker & Cumulative Ceiling

Two networking-hardening features protect against DeepSeek SSE stall patterns:

### Circuit Breaker (PN-4)

When a provider returns HTTP 200 with SSE headers but sends **zero chunks**
before the stream timeout fires (a "held request"), the retry layer classifies
this as non-retryable and skips all remaining retries.

The circuit breaker inspects the `exn:fail:network:timeout:stream` metadata:

| Condition | Classification | Retries? |
|-----------|---------------|----------|
| `received-any-data?=#f` AND `phase='initial` | `'held-request` | **No** — circuit breaker fires |
| `received-any-data?=#t` (any data seen) | normal stall | **Yes** — full retry budget |
| `phase='thinking` or `'content` | mid-stream stall | **Yes** — full retry budget |

An `auto-retry.start` telemetry event with `errorType: "circuit-breaker"` is emitted
when the breaker fires.

### Cumulative Ceiling (PN-7)

Each retry attempt resets its own timeout clock. Without a cumulative bound,
a turn with 3 sub-ceiling timeouts can run 6+ minutes. The cumulative ceiling
bounds the **total wall-clock** across all retry attempts.

#### Configuration

```json
{
  "providers": {
    "deepseek": {
      "retry-ceiling-secs": 300
    }
  }
}
```

| Setting | Default | Description |
|---------|---------|-------------|
| `providers.<name>.retry-ceiling-secs` | `300` (5 min) | Maximum cumulative wall-clock across all retries for a single turn. |

When the cumulative elapsed time exceeds the ceiling, the turn fails immediately
with a `retry-exhausted` exception naming the ceiling.

## Retry Behavior Summary

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max-retries` | 2 | Maximum retry attempts per turn |
| `base-delay-ms` | 1000 | Base delay for exponential backoff |
| `rate-limit-base-delay-ms` | 10000 | Base delay for rate-limit (429) errors |
| `max-delay-ms` | 60000 | Maximum delay cap |
| `retry-ceiling-secs` | 300 | Cumulative wall-clock ceiling (W2) |

Backoff uses exponential growth (`base * 2^attempt`) with full jitter
(random value in `[0, exponential-cap]`), capped at `max-delay-ms`.

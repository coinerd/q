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
| `received-any-data?=#t`, `<100 chars`, consecutive | `'minimal-output` | **No** — progressive breaker after 2 stalls |
| `received-any-data?=#t` (substantial output) | normal stall | **Yes** — full retry budget |
| `phase='thinking` or `'content` | mid-stream stall | **Yes** — full retry budget |

An `auto-retry.start` telemetry event with `errorType: "circuit-breaker"` is emitted
when the breaker fires.

### Progressive Stall Circuit Breaker (NR-1)

A provider that sends **some** data but fewer than `stall-min-output-chars`
characters (default 100) before stalling is in a "minimal-output" state. This
indicates a sick provider that starts responding but cannot sustain output.

The progressive breaker tracks consecutive minimal-output stalls across retry
attempts within the same turn. After `stall-max-consecutive` (default 2)
consecutive stalls, remaining retries are skipped.

| Setting | Default | Description |
|---------|---------|-------------|
| `stall-min-output-chars` | 100 | Chars below which a stall is classified "minimal-output" |
| `stall-max-consecutive` | 2 | Consecutive minimal-output stalls before circuit break |

Non-stall errors (rate-limit, auth, etc.) reset the consecutive counter.

### Provider Health Gate (NR-3)

A sliding-window failure tracker records provider failures across turns within
the same session. When the failure count exceeds a threshold within the window,
retries are skipped and the turn fails fast with a clear diagnostic.

| Setting | Default | Description |
|---------|---------|-------------|
| `providers.<name>.health-window-secs` | 60 | Sliding window for failure counting |
| `providers.<name>.health-failure-threshold` | 3 | Failures in window before provider marked unhealthy |

A successful provider response resets the failure window. The health gate does
NOT block the first attempt — it only gates retries.

A `provider.health-gate` telemetry event is emitted when the gate denies a retry,
including the failure count, window, threshold, and decision.

### Partial Result Recovery (NR-4)

When a provider stall produces partial output before timing out, q preserves
the partial text in the transcript **always**. Optionally, the partial text can
be fed as continuation context for the retry attempt.

#### Transcript Preservation (always on)

When `stream-from-provider` catches a mid-stream error with accumulated text,
the partial text is persisted as an assistant message with `'partial #t` in
the loop state. This happens regardless of the `partial-recovery` setting.

#### Continuation Injection (opt-in)

Set `partial-recovery = #t` to prepend partial text as a continuation prompt
on retry:

```
[Previous partial response (provider stalled):
{partial text}
]

Continue from where you left off.
```

The `partial-recovery-min-chars` threshold (default 200) prevents using tiny
fragments that wouldn't be useful for continuation.

| Setting | Default | Description |
|---------|---------|-------------|
| `providers.<name>.partial-recovery` | `#f` | Enable partial text continuation injection |
| `providers.<name>.partial-recovery-min-chars` | 200 | Minimum partial text length to qualify for recovery |

A `provider.partial-recovery` telemetry event is emitted when partial text is
injected, including `partialChars` and `minChars`.

### Cumulative Ceiling (PN-7)

Each retry attempt resets its own timeout clock. Without a cumulative bound,
a turn with 3 sub-ceiling timeouts can run 6+ minutes. The cumulative ceiling
bounds the **total wall-clock** across all retry attempts.

#### Configuration

```json
{
  "providers": {
    "deepseek": {
      "retry-ceiling-secs": 900
    }
  }
}
```

| Setting | Default | Description |
|---------|---------|-------------|
| `providers.<name>.retry-ceiling-secs` | `900` (15 min) | Maximum cumulative wall-clock across all retries for a single turn. |

When the cumulative elapsed time exceeds the ceiling, the turn fails immediately
with a `retry-exhausted` exception naming the ceiling.

### Adaptive Retry (PN-6)

On the second retry for a timeout or structured network error, q makes the next
request smaller instead of resending an identical overloaded payload:

- removes the oldest complete user/assistant history pair;
- preserves every system message and the current user request;
- keeps at least one complete user/assistant history pair;
- reduces `max-tokens` by 25% when it is configured.

If the context is already at the minimum floor, q leaves both context and
`max-tokens` unchanged. Authentication, bad-request, rate-limit, and other
non-network error classes do not trigger adaptive reduction.

Each adaptive decision emits `provider.adaptive-retry` with the retry attempt,
error class, original/reduced message counts and token estimates,
original/reduced `max-tokens`, and `floorReached`.

## Retry Behavior Summary

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max-retries` | 2 | Maximum retry attempts per turn |
| `base-delay-ms` | 1000 | Base delay for exponential backoff |
| `rate-limit-base-delay-ms` | 10000 | Base delay for rate-limit (429) errors |
| `max-delay-ms` | 60000 | Maximum delay cap |
| `retry-ceiling-secs` | 900 | Cumulative wall-clock ceiling (W2) |

Backoff uses exponential growth (`base * 2^attempt`) with full jitter
(random value in `[0, exponential-cap]`), capped at `max-delay-ms`.

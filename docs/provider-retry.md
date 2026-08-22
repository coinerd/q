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

## Streaming Timeout Matrix

Streaming requests run under three independent stall windows, resolved per
request by `sse-phase-timeout-secs` in `llm/stream.rkt` and wired into the
openai-compatible adapter since v1.00.12 (SS-1..SS-3):

| Phase | Window | Formula | Purpose |
|-------|--------|---------|---------|
| `initial` | ≤ 120 s | `min(request-timeout, 120)` | Dead-peer detection: a silent HTTP 200 is a held request, retried early instead of hanging |
| `thinking` | ≤ 300 s | `min(request-timeout, min(or sse-read 120, 300))` | Silent reasoning window; honors the model's `timeouts.models.<model>.sse-read` override up to `max-thinking-gap-secs` |
| `content` | 60 s | `http-stream-timeout-default` | Per-chunk gap once content flows; fixed — overrides must not widen it |

Key invariant: the raw `sse-read` configuration feeds **only** the thinking
window. It was previously wired into all three phases — a bug fixed in v1.00.12
— letting a deepseek `sse-read=600` turn a mid-content stall into a
~10-minute hang. Root-cause analysis:

```
.planning/ANALYSIS-v1.00.08-deepseek-10min-sse-stall.md
```

Slow-reasoning models keep their windows: kimi/glm with `sse-read=300` still
get the full 300 s reasoning gap.

Every stream timeout raises `exn:fail:network:timeout:stream` whose message now
carries a stable diagnostic suffix (SS-5):

```
... waiting for SSE chunk [phase=<initial|thinking|content> data-received=<yes|no> chars=<n>]
```

The struct fields (`phase`, `received-any-data?`, `content-chars`) remain the
machine source of truth for the retry layer's held-request/minimal-output
classification (see Circuit Breaker above); the suffix exists for logs and
human triage.

Time-to-first-byte (TTFB) behavior: a request that produces **no** chunk within
the initial window is classified `held-request` (`data-received=no`,
`phase=initial`) and is not retried — the circuit breaker fires immediately,
so worst-case dead-peer recovery is ~2 minutes plus backoff rather than the
full request timeout.

Regression matrix: `tests/test-sse-phase-timeout-bounds.rkt` locks the resolver
semantics and the message suffix.

### Scope and handoff (v1.00.13)

**v1.00.12** is explicitly a **containment** milestone: only the openai-compatible
adapter uses the resolver. The anthropic, azure-openai, and gemini adapters and
their eager-body read paths still apply timeouts independently. Unifying all
provider request/stream lifecycle policies behind one shared module is the
**Request Lifecycle Policy Unification** wave in v1.00.13 (SS-6 deferral;
handoff contract:

```
.planning/PLAN-v1.00.12-SSE-STALL-DETECTION-BOUNDS-REVISED.md
```

§5.1).

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

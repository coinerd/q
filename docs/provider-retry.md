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

## Resolved Request-Network Policy (v1.00.13)

Every provider request — streaming or non-streaming, on every adapter —
consumes ONE resolved `request-network-policy` produced by
`llm/request-policy.rkt` (`resolve-request-network-policy-for-model`).
Adapters translate wire formats only; they never interpret raw timeout
configuration and never author generic lifecycle constants
(`tests/test-request-policy-architecture.rkt` enforces this; the
cross-adapter harness `tests/test-provider-network-policy-conformance.rkt`
proves all four adapters pass identical values into the shared mechanism).

| Field | Value | Kind | Purpose |
|-------|-------|------|---------|
| `request-budget-secs` | per-model `request` | budget | Whole-request wall clock (unchanged meaning) |
| `connect-ttfb-secs` | `min(request, 120)` | bound | connect+TLS+status+headers; an established-but-silent peer can never consume the request budget (phase `'connect/ttfb` on timeout) |
| `initial-idle-secs` | `min(request, 120)` | bound | Dead-peer detection before the first byte; never widened by config |
| `thinking-idle-secs` | `min(request, min(or override 120, 300))` | bound | Silent reasoning window |
| `content-idle-secs` | 60 | bound | Per-chunk gap once content flows; never widened by config |
| `stream-total-secs` | `max(600, 2×request)` | budget | Total stream wall clock — a **deadline**, not an inactivity detector |
| `body-read-budget-secs` | explicit `body-read` > legacy `sse-read` > 120 | budget | Eager/non-streaming full-body read |

**Deadline vs inactivity.** `stream-total-secs` and `request-budget-secs`
bound how long a *healthy* stream may run; the idle bounds
(initial/thinking/content/connect) detect *liveness*. Widening a budget can
never hide a stall: every blocking read is additionally capped at the
remaining total budget (`min(phase-idle, remaining-total)`), so a read can
never overshoot the deadline by a full phase window.

### Semantic config keys (new) and `sse-read` deprecation

```
timeouts.models.<model>.request        # unchanged
timeouts.models.<model>.thinking-idle  # NEW: silent-reasoning window (cap 300)
timeouts.models.<model>.body-read      # NEW: eager full-body read budget
timeouts.models.<model>.sse-read       # DEPRECATED (still honored, see below)
```

Legacy `sse-read` compatibility (one resolver owns this mapping, nothing
else reads it):

- `thinking-idle`: explicit `thinking-idle` > legacy `sse-read` > 120, capped at 300 and by the request budget.
- `body-read`: explicit `body-read` > legacy `sse-read` > 120 fallback.
- Legacy `sse-read` **never** influences connect/TTFB, initial, or content.

Migration examples: a DeepSeek config with only `request: 900,
sse-read: 600` resolves to thinking 300 (capped), initial 120, content 60,
total 1800, body-read 600 — the slow-body allowance is preserved without
letting the safety detectors widen. A Kimi config with `sse-read: 300`
keeps its 300 s thinking window and 300 s body-read. Explicit
`thinking-idle`/`body-read` keys always win over the legacy alias.
`wiring/mode-helpers.rkt` logs a deprecation warning when it sees
`sse-read`; removal is planned after v1.00.16.

### Structured failure context and Retry-After

HTTP status and retry-relevant response headers survive the request boundary
in a structured failure context (`kind`, `http-status`, redacted
`response-headers`, parsed `retry-after-ms`). Sensitive headers
(Authorization, Set-Cookie, provider tokens) are dropped — only
`Retry-After` and `x-ratelimit-*` are retained.

`Retry-After` is read from the actual response header (delta-seconds and
HTTP-date forms; the clock is injectable for deterministic tests) and
consumed by auto-retry as structured metadata. No retry decision parses
human exception text (the pre-v1.00.16 path fed the whole message to a
string parser).

### Heartbeat-aware held-request classification

The circuit breaker now consults heartbeat metadata: a stream that received
SSE comments (`: ...` keep-alives) proved the peer is alive
(live-but-no-content) and is **not** classified as a held request, while a
zero-liveness silent 200 still trips the breaker immediately. Heartbeat-only
streams remain bounded by the total deadline and the empty/comment flood
ceiling (100 consecutive empty lines).

| Condition | Classification | Retries? |
|-----------|---------------|----------|
| zero data, zero heartbeats, `phase=initial` | `'held-request` | **No** — circuit breaker fires |
| heartbeat-only, `phase=initial` | live-but-no-content | **Yes** — bounded by total deadline + flood ceiling |
| `phase='connect/ttfb` | silent connection head | **Yes** — bounded at ≤ 120 s, far below the request budget |
| data received, `<100 chars`, consecutive | `'minimal-output` | **No** — progressive breaker after 2 stalls |
| `phase='thinking` or `'content` | mid-stream stall | **Yes** — full retry budget |

Every stream timeout raises `exn:fail:network:timeout:stream` whose message
carries the stable diagnostic suffix:

```
... waiting for SSE chunk [phase=<initial|thinking|content> data-received=<yes|no> chars=<n>]
```

The struct fields remain the machine source of truth; the suffix is rendered
for logs and human triage.

### Architecture (ownership direction)

```
raw config → llm/request-policy.rkt (resolver + invariants)
           → provider adapter (wire format/auth only)
           → request lifecycle (HTTP ownership/headers/deadlines)
           → stream/body mechanism (enforces resolved bounds)
           → structured outcome/failure
           → retry policy / TUI presentation
```

Regression locks: `tests/test-request-network-policy.rkt` (resolver matrix),
`tests/test-provider-network-policy-conformance.rkt` (cross-adapter parity),
`tests/test-request-policy-architecture.rkt` (ownership rules R1–R5),
`tests/test-network-failure-context.rkt` (structured failures),
`tests/test-provider-response-cleanup.rkt` (close-once lifecycle),
`tests/test-stream-liveness-classification.rkt` (liveness matrix),
`tests/test-sse-phase-timeout-bounds.rkt` (phase-bound semantics).


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

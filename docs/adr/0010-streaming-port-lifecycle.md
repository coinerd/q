# ADR-0010: Streaming Port Lifecycle

## Status
Accepted

## Context
q's LLM providers consume HTTP response ports for streaming (SSE). If a
streaming request is interrupted — by timeout, cancellation, or an exception —
the response port may be leaked. Accumulated leaked ports waste file
descriptors and can eventually exhaust system resources.

In the Azure OpenAI provider specifically, the `http-sendrecv` call opens three
ports (input, output, response) that all need guaranteed cleanup.

## Decision
Separate eager request setup from lazy generator ownership:

1. **`call-with-request-timeout`** wraps the HTTP request with a deadline and
   runs resource cleanup before interrupting its worker.
2. **`dynamic-wind` is limited to non-suspending acquisition/handoff code.** It
   closes a response port if setup is interrupted before ownership transfers.
3. **A will-backed generator owner** keeps the port open across yields and
   closes it on normal completion, failure, cancellation, or garbage
   collection after consumer abandonment.

Do **not** wrap a generator body containing `yield` in a `dynamic-wind` whose
after-thunk closes the port. A yield exits the dynamic extent, so that pattern
closes a still-live response port after the first chunk.

## Consequences
**Easier:** Timeout and abandonment paths have explicit ownership. A lazy
stream remains usable across multiple yields while still receiving eventual
cleanup if its consumer disappears.

**Harder:** Acquisition, ownership transfer, and generator finalization use
different mechanisms. Finalizer actions must be exception-isolated so one
faulty custom port cannot disable later cleanup.

**Risks:** Will execution is asynchronous. Correctness must not depend on
immediate finalization; explicit normal/error cleanup remains the primary
path.

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
Use `dynamic-wind` for guaranteed port cleanup in all streaming providers:

1. **`call-with-request-timeout`** wraps the HTTP request with a deadline.
2. **`dynamic-wind`** ensures the response port is always closed — even on
   timeout, exception, or custodian shutdown.
3. **Port cleanup in thunk** — The streaming generator reads from the port
   inside the dynamic-wind body. The post-thunk closes the port.

This pattern is applied consistently across all providers: OpenAI, Azure
OpenAI, Anthropic, and Gemini.

## Consequences
**Easier:** No port leaks regardless of failure mode. Timeout behavior is
predictable. The pattern is the same across all providers.

**Harder:** Slightly more nesting in the streaming code. Developers must
remember to use `dynamic-wind` rather than simple `with-handlers`.

**Risks:** If the port is closed before the generator finishes reading, the
generator must handle `read-string` errors gracefully.

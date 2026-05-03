# Exn:fail Migration Analysis (v0.28.19 W1)

## Audit Results

### Audited Sites
- **runtime/**: ~20 `exn:fail?` catch sites in credential-backend, oauth, session-lifecycle, session-events, auto-retry, extension-catalog
- **llm/**: ~12 `exn:fail?` catch sites in anthropic, azure-openai, gemini, stream, http-helpers, openai-compatible

### Conclusion
All sites are legitimate broad catches for:
1. **File I/O fallbacks** (credential-backend, oauth) — file not found, parse errors
2. **Port cleanup** (LLM providers) — `with-handlers void` on port close
3. **Stream error handling** (stream.rkt) — channel put/get error propagation
4. **Network fallbacks** (http-helpers) — connection errors, timeouts

### Decision: No Migration
These are infrastructure error boundaries, not application logic. Domain error types
(`provider-error`, `tool-error`, `session-error`) don't apply to file I/O and port
cleanup contexts. Broad `exn:fail?` is the correct catch level here.

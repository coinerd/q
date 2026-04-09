# ADR-0004: Provider Abstraction

## Status
Accepted

## Context
q must support multiple LLM backends—OpenAI-compatible APIs, Anthropic, and local
models—without coupling the core agent loop to any single provider. Hardcoding a
provider makes it impossible to switch models, compare outputs, or support
air-gapped environments with local inference.

## Decision
q defines a generic provider interface with streaming support. The interface
exposes a single primary operation: send a message list and configuration,
receive a streaming response of tokens or tool-call requests.

Provider-specific adapters implement this interface, translating between q's
canonical message format and each provider's API. Configuration (API keys,
endpoints, model names) is injected, not hardcoded.

## Consequences
**Easier:** Adding a new provider requires only a new adapter module. Switching
providers at runtime is straightforward. Testing the core loop with a mock
provider is trivial.

**Harder:** Provider-specific features (e.g., Anthropic's thinking mode,
OpenAI's function calling) must be carefully abstracted or exposed as optional
extensions without polluting the core interface.

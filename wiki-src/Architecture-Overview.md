# Architecture Overview

q is organized as a layered system.

## Layers

1. **Interfaces** — CLI, TUI, JSON mode, RPC/SDK
2. **Tools & Extensions** — Built-in tools, extension API, sandbox
3. **Runtime** — Session lifecycle, settings, provider factory, compaction
4. **Agent Core** — Event bus, agent loop, iteration manager
5. **LLM** — Provider protocol, OpenAI/Anthropic/Gemini adapters, streaming

See the canonical architecture description in the [repository README](https://github.com/coinerd/q/blob/main/README.md) and [docs/](https://github.com/coinerd/q/tree/main/docs).

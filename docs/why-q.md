# Why q?

A candid look at who q is for, what it prioritizes, and where it's intentionally narrow.

## Who q is for

- Developers who want a **local-first, auditable** coding agent runtime
- People who value **provider neutrality** and extensibility
- Users comfortable with **terminal workflows** and the Racket ecosystem
- Researchers and tinkerers who want to **understand and modify** their agent

## Key priorities

These are the architectural decisions that shape every part of q:

1. **Local-first** — Runs on your machine. No cloud dependency for the runtime itself. Your code and prompts never leave your network unless you configure a remote provider.

2. **Auditability** — Every action logged in append-only JSONL. Full session replay. You can reconstruct exactly what happened and why.

3. **Provider neutrality** — OpenAI, Anthropic, local models. Switch providers without changing your workflow. The agent core doesn't know or care which LLM backend you use.

4. **Extensibility** — Hook system, tool registry, and extension points throughout. Add new tools, providers, or behaviors without touching the core loop.

5. **Transparency** — Small trusted core. All behavior inspectable. The entire agent loop is readable in a handful of modules, not buried in abstraction layers.

## Where q is intentionally narrower

q doesn't try to be everything. These are deliberate scope boundaries:

- **No built-in cloud sync or collaboration** — Single-machine, single-user.
- **No GUI beyond the terminal** (TUI) — If you need rich visual tooling, q isn't there yet.
- **No model training or fine-tuning** — q is a runtime, not a training pipeline.
- **Single-user, single-session focus** — No multi-user or concurrent-session architecture.
- **Racket ecosystem** — Not Python, not Node.js. This is a deliberate choice for a small, coherent language with excellent macro and module systems, but it means fewer existing libraries and a smaller talent pool.

## Current maturity (v0.3.1)

Be realistic about where things stand:

- **Core architecture** — Stable and tested. 2343+ tests passing. The agent loop, session management, and provider abstraction are production-quality.
- **Interfaces** — Working CLI, TUI, JSON mode, and SDK interfaces. All functional, all terminal-based.
- **Provider support** — OpenAI and Anthropic providers are solid. Local model support works via compatible endpoints (Ollama, llama.cpp, etc.).
- **Extension system** — Functional but young. The hooks and tool registry work; the ecosystem of third-party extensions doesn't exist yet.
- **Packaging** — No published packages. Install from source. This will change, but for now, `git clone` and `raco pkg install` are the path.

## When to choose q

Pick q when:

- You want **full control** over your coding agent's behavior — not just configuration, but actual code-level control.
- You need to **audit every action** the agent takes, for compliance, security review, or personal understanding.
- You want to run **local or private models** without sending your codebase to cloud APIs.
- You're **building on top of** a coding agent and need a stable, extensible runtime as a foundation.
- You value a **small, understandable codebase** that you can read end-to-end and modify without fear.

## When to look elsewhere

Don't pick q if:

- You need a **polished, productized experience** out of the box. q works, but it's a developer tool, not a consumer product.
- Your team needs **real-time collaboration** features. q is single-user by design.
- You're **not comfortable with Racket** or terminal-centric workflows. There's no VS Code extension or web dashboard.
- You need mature **IDE integrations** (VS Code, JetBrains). The interfaces exist in code but aren't wired to editor plugins yet.
- You want a **large community and ecosystem**. q is early. The community is small.

---

*This document reflects q v0.3.1. Maturity and capabilities will evolve. Revisit this page as the project grows.*

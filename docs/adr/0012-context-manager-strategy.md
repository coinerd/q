# ADR-0012: Context Assembly Architecture

Date: 2024-11 (v0.23.6)
## Status
Accepted

## Context
The context assembly pipeline was split across two modules: `context-builder.rkt` (production tree-walk) and `context-manager.rkt` (dead code, never called outside tests). Logic was duplicated: token estimation, first-user pinning, and pair-preserving budget fitting existed in both modules with divergent implementations. LLM summarization in context-manager was a stub that always concatenated.

## Decision
Unified the context assembly into a three-module architecture:

1. **`runtime/context-policy.rkt`** — Shared policy functions:
   - `estimate-message-tokens` (canonical implementation)
   - `ensure-first-user-pinned` (first user message always included)
   - `build-pair-index` + `requires-pair-inclusion?` + `fit-messages-pair-preserving`
   - Re-exports `estimate-text-tokens` from token-budget
   - Both context-builder and context-manager import from this single source

2. **`runtime/context-manager.rkt`** — Production pipeline:
   - `assemble-context`: Full pipeline (tree-walk → pin → budget → summarize → catalog → reassemble)
   - LLM summarization via compactor's `llm-summarize` when provider given
   - Summary caching for excluded entries
   - Catalog generation for observability
   - Wired into `session-lifecycle.rkt` as the production context builder

3. **`runtime/context-builder.rkt`** — Legacy tree-walk (fallback, no summarization):
   - Used when no provider is available
   - Imports shared logic from context-policy.rkt
   - Will be removed in a future version

## Consequences
- Single source of truth for token estimation, pinning, and pair-preserving budget fitting
- LLM summarization integrated into the production pipeline via compactor's proven `llm-summarize`
- context-manager is no longer dead code — it's the primary production pipeline
- context-builder serves as a fallback when no provider is configured
- New strategies (e.g., relevance-based inclusion) can be added to context-manager without touching context-policy

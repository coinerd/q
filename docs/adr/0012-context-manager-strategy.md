# ADR-0012: Context Manager Strategy Engine

Date: 2024-11 (v0.22.2)
## Status
Accepted

## Context
The context manager needed to decide which messages to truncate when the conversation exceeded the LLM's token budget. Initially, truncation was a simple FIFO approach that could break tool-call/result pairing and lose important context like system instructions.

## Decision
Implemented a strategy engine with multiple truncation strategies:
- Pair-preserving truncation: Keeps tool-call/result pairs together
- System-instruction preservation: Never truncates system-instruction messages
- Budget-aware: Estimates token usage and truncates to stay within limits

## Consequences
- Tool-call/result pairs are always kept together, preventing LLM confusion
- System instructions survive truncation even under heavy budget pressure
- New strategies can be added without modifying existing code
- The `test-context-pair-preserving.rkt` test suite validates all invariants

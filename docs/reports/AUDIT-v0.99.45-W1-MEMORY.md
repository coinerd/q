# Audit: Memory Subsystem — v0.99.45 W1

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w1-memory.rkt`
**Tests:** 29 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Memory backends tested: `memory-hash`, `file-jsonl`, `chained`
- Session dir: Temporary directories per test
- Special flags: None

## Test Matrix

| # | Test | Expected | Actual | Status |
|---|------|----------|--------|--------|
| 1 | Store/retrieve lifecycle | Item stored, retrieved by text query | ✅ Correct | PASS |
| 2 | Episodic memory type | Store and retrieve episodic events | ✅ Correct | PASS |
| 3 | Procedural memory type | Store and retrieve procedural knowledge | ✅ Correct | PASS |
| 4 | file-jsonl persistence | Items survive "restart" (new backend) | ✅ Correct | PASS |
| 5 | Multiple items in file-jsonl | All items persist | ✅ Correct | PASS |
| 6 | Scope isolation (session vs project) | Session items not visible cross-session | ✅ Correct | PASS |
| 7 | Sensitivity gate: API keys | Content with `sk-` pattern blocked | ✅ Correct | PASS |
| 8 | Sensitivity gate: passwords | Content with `password=` blocked | ✅ Correct | PASS |
| 9 | Sensitivity gate: bearer tokens | Content with `Bearer` blocked | ✅ Correct | PASS |
| 10 | Sensitivity gate: AWS keys | Content with `AKIA` blocked | ✅ Correct | PASS |
| 11 | Sensitivity gate: normal content | Non-secret content allowed | ✅ Correct | PASS |
| 12 | Sensitivity gate: secret scope | `secret` sensitivity blocked by default | ✅ Correct | PASS |
| 13 | Backend switching | Hash items not in file-jsonl | ✅ Correct | PASS |
| 14 | Management: list | All stored items returned | ✅ Correct | PASS |
| 15 | Management: delete | Item removed, others remain | ✅ Correct | PASS |
| 16 | Management: delete nonexistent | Returns not-found error | ✅ Correct | PASS |
| 17 | Content deduplication | Duplicate content returns idempotent | ✅ Correct | PASS |
| 18 | Idempotent re-store | Same exact item returns idempotent | ✅ Correct | PASS |
| 19 | Policy: content length limit | Content over limit blocked | ✅ Correct | PASS |
| 20 | Policy: retrieve budget | Over-budget request rejected | ✅ Correct | PASS |
| 21 | Policy: user scope disabled | User scope blocked by default | ✅ Correct | PASS |
| 22 | Chained backend store/retrieve | L1+L2 store, retrieve, write-through | ✅ Correct | PASS |
| 23 | Update content | Content updated and retrievable | ✅ Correct | PASS |
| 24 | Update nonexistent | Returns not-found error | ✅ Correct | PASS |
| 25 | Redaction: API keys | `sk-` pattern replaced with `[REDACTED]` | ✅ Correct | PASS |
| 26 | Redacted snippet truncation | Snippet respects max length | ✅ Correct | PASS |
| 27 | Retrieval respects limit | Returns at most N items | ✅ Correct | PASS |
| 28 | Empty text query | Returns all items (no text filter) | ✅ Correct | PASS |
| 29 | Type filtering | Only items of specified type returned | ✅ Correct | PASS |

## Findings

No critical or high-severity findings. Memory subsystem behavior matches documented expectations across all tested dimensions.

**Observations:**
- **FINDING-001** (info): Content deduplication uses token overlap, not exact match. Items with same content but different IDs are detected as duplicates via the `content-duplicate?` function. This is by design (prevents semantic duplication).
- **FINDING-002** (info): Sensitivity gating has comprehensive pattern coverage: API keys, passwords, bearer tokens, AWS keys, GitHub tokens, private keys, and multi-line `.env` patterns. No gaps found.
- **FINDING-003** (info): file-jsonl backend uses incremental loading with file-size cache. Restart creates a fresh backend that correctly reads persisted items from disk.

## Remediation Items

None required. Memory subsystem is functioning correctly.

## Trace Evidence Summary

- Events: 29 test assertions, all passing
- Key phases: store → retrieve → persist → scope-filter → sensitivity-gate → management
- Anomalies: None detected

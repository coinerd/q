# Audit: Context Assembly Subsystem — v0.99.45 W2

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w2-context-assembly.rkt`
**Tests:** 32 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested: `budgeting.rkt`, `selection.rkt`, `context-policy.rkt`, `context-pinning.rkt`, `context-floor.rkt`
- Message types: System, user, assistant, tool, compaction-summary, GSD-pinned
- Special flags: None

## Test Matrix

| # | Test | Expected | Actual | Status |
|---|------|----------|--------|--------|
| 1 | Config defaults (30k tokens, 40 catalog entries) | Correct defaults | ✅ Correct | PASS |
| 2 | Config custom params | All custom values applied | ✅ Correct | PASS |
| 3 | Token estimate: short text | > 0 tokens | ✅ Correct | PASS |
| 4 | Token estimate: long vs short | Long > short | ✅ Correct | PASS |
| 5 | Token estimate proportional | 200 chars > 100 chars | ✅ Correct | PASS |
| 6 | Select: fits within budget | Budget 50/10 = max 5 msgs | ✅ Correct | PASS |
| 7 | Select: all fit when budget large | All 2 msgs selected | ✅ Correct | PASS |
| 8 | Select: nothing fits when budget small | 0 selected, all excluded | ✅ Correct | PASS |
| 9 | Select: pinned always included | Pinned in selected list | ✅ Correct | PASS |
| 10 | Tiered basic partitioning | System/user in tier-a | ✅ Correct | PASS |
| 11 | Tiered: small conv all preserved | All messages kept | ✅ Correct | PASS |
| 12 | Tiered: user messages in tier-a | All user msgs in tier-a | ✅ Correct | PASS |
| 13 | Tiered: GSD progress pinned | GSD msg in tier-a | ✅ Correct | PASS |
| 14 | Tiered: compaction summary pinned | Summary in tier-a | ✅ Correct | PASS |
| 15 | Universal user pinning | All 5 user msgs pinned | ✅ Correct | PASS |
| 16 | User pinning via partition | System + users pinned | ✅ Correct | PASS |
| 17 | Working-set pinning | WS target pinned | ✅ Correct | PASS |
| 18 | Dynamic tier-b sizing | min(50, max(20, N/10)) | ✅ Correct | PASS |
| 19 | Dynamic tier-c sizing | min(12, max(4, N/50)) | ✅ Correct | PASS |
| 20 | Tier merge: windowed drop | 30 asst → 25 kept | ✅ Correct | PASS |
| 21 | Tier merge: pinned preserved | All user msgs survive | ✅ Correct | PASS |
| 22 | Payload round-trip | Messages preserved | ✅ Correct | PASS |
| 23 | GSD detection: meta flag | gsd-pin meta detected | ✅ Correct | PASS |
| 24 | GSD detection: wave complete text | Lowercase "wave" detected | ✅ Correct | PASS |
| 25 | GSD detection: PLAN.md text | Detected | ✅ Correct | PASS |
| 26 | GSD detection: STATE.md text | Detected | ✅ Correct | PASS |
| 27 | GSD detection: negative | Regular msg not detected | ✅ Correct | PASS |
| 28 | Budget overflow: pinned exceeds | All removable excluded | ✅ Correct | PASS |
| 29 | Empty message list | Returns empty tiered-context | ✅ Correct | PASS |
| 30 | Single message | System in tier-a only | ✅ Correct | PASS |
| 31 | Select empty lists | 0 selected, 0 excluded | ✅ Correct | PASS |
| 32 | Large conversation (302 msgs) | Windowed selection | ✅ Correct | PASS |

## Findings

### FINDING-001 (low): Tiered context is windowed, not full-preservation

**Severity:** Low (info)
**Category:** docs
**Description:** The tiered context builder (`build-tiered-context`) uses a windowed selection strategy. It does NOT preserve all messages in large conversations. Instead, it keeps:
- **Tier-A:** All system instructions, ALL user messages, GSD progress messages, compaction summaries, working-set messages
- **Tier-B:** Most recent N regular messages (dynamic: `min(50, max(20, count/10))`)
- **Tier-C:** Oldest M regular messages (dynamic: `min(12, max(4, count/50))`)

Middle messages between Tier-B and Tier-C are dropped. This is by design for token budget management. The `build-assembled-context/raw` path (token-budgeted) also drops messages that don't fit.

**Impact:** In a conversation with 300+ assistant messages, ~250+ middle messages are dropped from the tiered context window. User messages and system instructions are always preserved.

**Recommendation:** Document this behavior clearly in user-facing documentation.

### FINDING-002 (low): GSD progress regex is case-sensitive

**Severity:** Low (info)
**Category:** docs
**Description:** The `gsd-progress-message?` function uses `(regexp ...)` which creates a case-sensitive regex. The pattern expects lowercase `"wave [0-9]+"`, so `"Wave 0 marked complete"` (capitalized) would NOT be detected by the text pattern. However, the `gsd-pin` meta flag takes precedence and works regardless of text case.

**Impact:** Low — the `gsd-pin` meta flag is the primary detection mechanism. Text-based detection is a fallback for messages without the meta flag.

**Recommendation:** Consider using `pregexp` with `#px"(?i)wave"` for case-insensitive matching, or document that GSD progress text must be lowercase.

## Remediation Items

None required (informational findings only). Context assembly subsystem is functioning correctly.

## Architecture Summary

The context assembly pipeline has two main paths:

1. **Token-budgeted assembly** (`build-assembled-context/raw`):
   - Partitions messages into pinned (system, ALL user, compaction, working-set) and removable
   - Fits removable messages into remaining token budget using pair-preserving fit with importance rescue
   - Optionally generates summary for excluded messages
   - Optionally generates catalog of excluded entries

2. **Tiered assembly** (`build-tiered-context`):
   - Partitions into 3 tiers (A: always-kept, B: recent window, C: oldest window)
   - Used for pre-LLM context preparation with hook dispatch support
   - Dynamic sizing based on conversation length

Both paths ensure user messages and system instructions are always preserved.

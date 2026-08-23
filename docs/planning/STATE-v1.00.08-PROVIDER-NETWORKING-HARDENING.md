# STATE v1.00.08 — Provider Networking Hardening

**Milestone:** v1.00.08  
**Plan:** `PLAN-v1.00.08-PROVIDER-NETWORKING-HARDENING.md`  
**Status:** COMPLETE — implemented, tested, released as v1.00.08  
**Depends on:** v1.00.07 (macOS platform fixes + full-regression dispatch)  
**Target:** DeepSeek v4 Flash SSE stall resilience + provider-agnostic hardening

## Wave Status

| Wave | Title | Defects | Status | Branch | PR | Gate |
|------|-------|---------|--------|--------|-----|------|
| W0 | Port Closure & Generator Finalization | PN-1, PN-3 | DONE (pre-existing impl) | main | #9416 | fast |
| W1 | SSE Heartbeat Tracking & Liveness Metadata | PN-2b | DONE (pre-existing impl) | main | #9416 | fast |
| W2 | Circuit Breaker & Cumulative Ceiling Config | PN-4, PN-7 | DONE | main | #9416 | fast+broad |
| W3 | Adaptive Retry | PN-6 | DONE (pre-existing impl) | main | #9416 | fast |
| W4 | Integration & Release | — | DONE | main | #9416 | broad/full + release |

## Defect Status

| ID | Defect | Severity | Wave | Test Created | Implemented | Verified |
|----|--------|----------|------|--------------|-------------|----------|
| PN-1 | Port not closed on timeout | CRITICAL | W0 | ✅ (test-stream.rkt) | ✅ (pre-existing) | ✅ |
| PN-2b | No SSE heartbeat tracking | MEDIUM | W1 | ✅ (test-stream-heartbeat-metadata.rkt) | ✅ (pre-existing) | ✅ |
| PN-3 | SSE generator leaks ports | HIGH | W0 | ✅ (test-stream.rkt) | ✅ (pre-existing) | ✅ |
| PN-4 | No circuit breaker for held requests | HIGH | W2 | ✅ (test-auto-retry.rkt) | ✅ (pre-existing) | ✅ |
| PN-6 | Retry sends identical request | MEDIUM | W3 | ✅ (test-adaptive-retry.rkt) | ✅ (pre-existing) | ✅ |
| PN-7 | Cumulative ceiling not configurable | MEDIUM | W2 | ✅ (test-provider-retry-ceiling-config.rkt) | ✅ (pre-existing + extracted resolve-retry-ceiling-secs) | ✅ |

## Notes

- Defects PN-1..PN-7 were implemented in the v0.99.81-v0.99.84 era; v1.00.08 added the missing PN-7 settings-based config-override test (`test-provider-retry-ceiling-config.rkt`) and fixed documentation drift (`docs/provider-retry.md` ceiling default 300→900).
- `resolve-retry-ceiling-secs` extracted from inline settings resolution in `runtime/turn-orchestrator.rkt` (pure refactor, exported).
- Fast gate: 1108 files / 16185 tests, 0 failures. tui/arch/workflows gates green.
- Release: v1.00.08 tagged and published via release workflow.

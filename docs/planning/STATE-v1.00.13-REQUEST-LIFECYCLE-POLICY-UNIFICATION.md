# STATE v1.00.13 — Request Lifecycle Policy Unification

Status: In Progress (W0–W4 implemented; W5 release in flight)
Milestone: GitHub #886 — waves #9454 (W0), #9461 (W1), #9466 (W2), #9473 (W3), #9478 (W4), #9483 (W5)
Plan: `PLAN-v1.00.13-REQUEST-LIFECYCLE-POLICY-UNIFICATION.md`

## Wave ledger

| Wave | Issue | PR | Merge SHA | Status |
|------|-------|-----|-----------|--------|
| Pre: CI cold-runner repair + fixture exclusion | — | #9488 | 63d8856e | merged (unblocked all gates) |
| W0 red contracts | #9454 (+9455–9460) | #9489 | e2e9a51d | merged |
| W1 policy module | #9461 (+9462–9465) | #9490 | 1e64cab3 | merged |
| W2 mandatory consumption | #9466 (+9467–9472) | #9491 | 7e3fa110 | merged |
| W3 lifecycle ownership | #9473 (+9474–9477) | #9492 | (pending CI) | in flight |
| W4 liveness + deadlines | #9478 (+9479–9482) | (next) | — | implemented, gates running |
| W5 docs + release | #9483 (+9484–9487) | (next) | — | in flight |

## Defect ledger

| ID | Status | Where fixed |
|----|--------|-------------|
| RL-1 single policy owner | ✅ | W1 `llm/request-policy.rkt`; arch gate R1–R5 |
| RL-2 sse-read overload | ✅ | W1 resolver: sse-read → thinking-idle/body-read only |
| RL-3 mandatory consumption | ✅ | W2 all four adapters; conformance harness |
| RL-4 connect/TTFB bound | ✅ | W4 `provider-sendrecv/ttfb-bounded`, phase 'connect/ttfb |
| RL-5 headers survive | ✅ | W3 structured failure context (redacted) |
| RL-6 non-streaming cleanup | ✅ | W3 close-once lifecycle; matrix tests |
| RL-7 structured retry metadata | ✅ | W3 `structured-retry-after-ms`; message parsing retired |
| RL-8 heartbeat classification | ✅ | W4 heartbeat-aware `held-request?` |
| RL-9 total deadline owned centrally | ✅ | W1 formula in policy; W4 hard remaining-budget reads |
| RL-10 conformance + architecture gate | ✅ | W0 harness (green since W2) + R1–R5 (allowlist empty) |

## Behavior changes vs v1.00.12 (all intentional, recorded in W2/W4 reports)

1. anthropic/azure/gemini streaming: thinking window 60 → `min(request, min(or override 120, 300))`; total 600 → `max(600, 2×request)` when request > 300.
2. All adapters (incl. openai eager): non-streaming body read honors legacy `sse-read` (or explicit `body-read`) instead of the flat 120 s fallback.
3. Connect+TTFB on every path bounded at `min(request, 120)` with structured phase.
4. Heartbeat-only initial stalls no longer trip the held-request circuit breaker.
5. Blocking stream reads capped at `min(phase-idle, remaining-total)`.
6. Retry delays derive from the `Retry-After` header via structured context (HTTP-date + delta-seconds), never from message text.

## Non-goals honored

TCP keepalive (NP-5) untouched; provider wire formats untouched beyond policy plumbing; retry-count policy, circuit-breaker thresholds, TUI unchanged; legacy `sse-read` not removed (deprecated only).

## Rollback points

Each wave is one squash commit on main (see ledger). W1/W2 are pure relocation (revert restores v1.00.12 adapter semantics); W3 is independent of phase-bound policy; W4's behavior changes are isolated in one commit.

# VALIDATION v1.00.13 — Request Lifecycle Policy Unification

Verification map for PLAN-v1.00.13 §9 hooks. All suites executed via
`racket scripts/run-tests.rkt` (runner-version 1.00.13 on the release tree).

## §4 Definition-of-Done ↔ Evidence

| DoD | Hook | Evidence |
|-----|------|----------|
| 1 one policy owner | unit: resolver matrix | `tests/test-request-network-policy.rkt` green (W1, #9490); stream.rkt mechanism-only (arch R3) |
| 2 semantic config | compat precedence + caps | policy suite + `tests/test-request-policy-migration.rkt` (DeepSeek/Kimi fixtures, explicit-wins) |
| 3 policy mandatory (4 adapters) | cross-adapter fixture at mechanism boundary | `tests/test-provider-network-policy-conformance.rkt` F1/F2 green (W2, #9491): identical mechanism args for openai/anthropic/azure/gemini |
| 4 conformance tested | same | as above; observer seam `current-request-mechanism-observer` |
| 5 architecture gate | only policy module reads raw config | `tests/test-request-policy-architecture.rkt` R1–R5 with EMPTY allowlist (since W2) |
| 6 connect/TTFB bounded | established mock, no headers → bounded failure + cleanup | `tests/test-stream-liveness-classification.rkt` "silent peer bounded" (elapsed < 5 s vs 900 s budget; phase `'connect/ttfb`) |
| 7 headers survive | 429 fixture preserves Retry-After | `tests/test-network-failure-context.rkt` (headers in context, redaction excludes Authorization/Cookie) |
| 8 Retry-After works | delta-seconds + HTTP-date | `parse-retry-after-header` unit cases with injectable clock (W3, #9492) |
| 9 non-streaming cleanup | close count exactly one | `tests/test-provider-response-cleanup.rkt` matrix: success/status-raise/read-timeout/request-timeout/cancel + GC-pressure idempotency |
| 10 structured failure path | no retry decision parses text | `structured-retry-after-ms` (message mentioning Retry-After without context → #f); auto-retry consumes context only |
| 11 heartbeat-aware liveness | heartbeat-only ≠ held | `tests/test-stream-liveness-classification.rkt` (zero-liveness still held; heartbeat-only retryable; flood ceiling unchanged in `test-stream.rkt`) |
| 12 total deadline owned | per-read wait ≤ remaining budget | `test-stream-liveness-classification.rkt` "total deadline" (60 s phase window, 0.5 s total → raise ≤ 5 s) |
| 13 gates on final SHA | focused/fast/broad/arch/security/lint | see Gate ledger below (CI on release PR) |
| 14 tagged release | v1.00.13 + assets | release PR (#9494) → tag → release workflow (recorded post-release) |

## Gate ledger

| Gate | Scope | Result |
|------|-------|--------|
| focused (18 suites) | policy/conformance/context/cleanup/liveness/stream/bounds/retry/4 adapters | 567+ tests PASS (W2-W4 runs) |
| fast | 1116 files | PASS (W3 run; W4 additions re-gated individually + CI test shards) |
| broad (full) | 1309 files | 1299 pass; arch-fitness fixed via risk-note; tmux-campaign requires pushed SHA (GitHub attestation) — validated by CI test shards on the release branch |
| security | 64 files | PASS (W4) |
| arch | 31 files | PASS after risk-note registration |
| architecture (policy) | R1–R5, empty allowlist | PASS |
| CI (12 required checks) | lint/test×3/security/smoke/platform/workflows/release-dry-run | green on W0/W1/W2/W3 merges; W4/W5 in flight |

## Ownership graph (before → after)

Before (v1.00.12): `llm/stream.rkt` owned params + constants + resolver;
openai-compatible alone consumed it and authored `(max 600 2×req)`;
anthropic/azure/gemini used raw defaults; http-helpers owned no lifecycle;
headers discarded; retry parsed message text; held-request ignored heartbeats.

After (v1.00.13): `llm/request-policy.rkt` owns all translation (params,
constants, resolver, legacy mapping); all four adapters consume one resolved
policy (stream + eager); `llm/http-helpers.rkt` owns the request lifecycle
(headers → structured context with redaction, close-once ports, connect/TTFB
bound); `llm/stream.rkt` is pure mechanism enforcing resolved bounds with
hard remaining-budget reads; auto-retry consumes structured metadata only.

## Parity changes (intentional, W2-recorded)

1. anthropic/azure/gemini thinking 60 → `min(request, min(or override 120, 300))`
2. anthropic/azure/gemini total 600 → `max(600, 2×request)` when request > 300
3. eager body reads: legacy `sse-read` budget (or explicit `body-read`) replaces flat 120 fallback
4. connect/TTFB ≤ 120 s on every path (was: unbounded up to request budget)
5. heartbeat-only stalls retryable (were: held-request non-retryable)

## Rollback points

W0 #9489 (e2e9a51d) → W1 #9490 (1e64cab3) → W2 #9491 (7e3fa110) →
W3 #9492 (e172472c) → W4 #9493 (pending) → W5 release PR. Each wave is one
squash commit; W1/W2 revert restores v1.00.12 semantics; W4 isolates the
behavior changes; W5 is docs/version only.

## Post-release smoke (pending tag)

Re-run DeepSeek/Kimi migration fixtures against the released SHA; watch
telemetry for adapter-specific timeout divergence (none expected: conformance
pinned).

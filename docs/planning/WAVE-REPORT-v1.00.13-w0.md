# W0 Ownership Map + Total-Duration Characterization (v1.00.13)

Evidence base: `q/` @ `e3983d4a` (main after v1.00.12 release + v1.00.11 CI merge).
Method: direct source reads of every consumer of the plan's W0 inventory symbol
list. No production files were modified in W0.

## 1. Symbol-by-symbol ownership inventory

| Symbol | Defined in | Consumed by | Semantics owned where? |
|---|---|---|---|
| `effective-sse-read-timeout-for` | `llm/stream.rkt` | `llm/openai-compatible.rkt` (stream: thinking override), `llm/anthropic/sse.rkt` (non-streaming body read), tests | **distributed** — two adapters translate raw config independently (RL-1/RL-2) |
| `effective-request-timeout-for` | `llm/stream.rkt` | `llm/openai-compatible.rkt` (`do-http-request`, `openai-stream-request`); indirectly `llm/http-helpers.rkt` via `current-http-request-timeout` default | adapter-side budget resolution (RL-1) |
| `current-model-timeouts` / `current-model-sse-read-timeouts` | `llm/stream.rkt` | wired by `wiring/mode-helpers.rkt` (`wire-timeouts!`, `timeouts.models.<m>.request` / `.sse-read`); read by the accessors above | raw config plumbing — OK; translation is the leak |
| `held-request-detect-secs` | — (not present under this name) | the equivalent fixed 120 s initial bound lives inside `sse-phase-timeout-secs` (`llm/stream.rkt`) | mechanism module owns policy constant (RL-1) |
| `http-stream-timeout-default` (60) | `llm/stream.rkt` | default `#:stream-timeout` of `stream-sse-events`; content phase value in `sse-phase-timeout-secs`; referenced by openai-compatible comments | mechanism module (fine as the constant, wrong that policy resolution lives there) |
| `max-total-timeout` (kwarg) | `stream-sse-events` param | **only `llm/openai-compatible.rkt`** passes it: `(max 600 (* 2 stream-timeout))`; anthropic/azure/gemini rely on the 600 default | adapter-authored total policy (RL-9) |
| `stream-sse-events` timeout kwargs | `llm/stream.rkt` | openai: all four passed from `sse-phase-timeout-secs` + `max-total`; anthropic (`llm/anthropic/sse.rkt`), gemini, azure: **zero kwargs** → defaults initial=120, thinking=stream=60, content=60, total=600 | mixed ownership (RL-3) |
| `make-provider-http-request` | `llm/http-helpers.rkt` | openai (send), anthropic (send + kimi eager), azure (send), gemini (send) | body-read timeout passed only by anthropic (`#:read-timeout` = legacy sse-read); others fall back to 120; **no port cleanup on any failure path** (RL-6) |
| `http-sendrecv` response headers (`rh`) | used in all four stream paths + helpers | **discarded everywhere** (status line parsed; headers bound then dropped; `check-provider-status!` never sees them) | RL-5 confirmed |
| `parse-retry-after` | `runtime/auto-retry.rkt` | `with-auto-retry` — applied to `(exn-message exn)`, i.e. human exception text | RL-7 confirmed |
| `received-heartbeats?` | `exn:fail:network:timeout:stream` field (stream.rkt) | recorded by `stream-sse-events`; **ignored** by `held-request?` (auto-retry) | RL-8 confirmed |
| `sse-phase-timeout-secs` (v1.00.12 resolver) | `llm/stream.rkt` | only `llm/openai-compatible.rkt` | resolver in mechanism module; 3 of 4 adapters bypass it (SS-6 parity deferral, confirmed in `STATE-v1.00.12` line 28) |

## 2. Adapter-by-adapter timeout behavior (today)

| Path | initial | thinking | content | total | body-read (non-streaming) |
|---|---|---|---|---|---|
| openai-compatible stream | min(req,120) | min(req, min(sse-read\|120, 300)) | 60 | **(max 600, 2×req)** | 120 fallback (no read-timeout passed) |
| anthropic stream | 120 (default) | 60 (default) | 60 | 600 (default) | legacy sse-read via `#:read-timeout` |
| azure stream | 120 | 60 | 60 | 600 | 120 fallback |
| gemini stream | 120 | 60 | 60 | 600 | 120 fallback |

Non-streaming request budget: openai uses per-model `effective-request-timeout-for`;
anthropic/azure/gemini use `current-http-request-timeout` default 600 (make-provider-http-request).

## 3. Total-duration characterization → frozen W1 formula

Two formulas exist today:
- openai-compatible: `(max 600 (* 2 request))` — intentional since v0.45.12 L1
  ("2x the request timeout as the wall-clock cap, 600 s floor"), i.e. for the
  default 600 s request budget the total is 1200 s; for GLM-5.1 (900 s) it is 1800 s.
- anthropic/azure/gemini: flat 600 default of `stream-sse-events` — older,
  pre-dates per-model budgets; NOT documented as intentional anywhere.

**Frozen decision (W1):** `stream-total-secs = (max 600 (* 2 request-budget))`.
Rationale: it is the only *documented intentional* formula; it is a total
wall-clock budget (not an inactivity detector — per-phase windows stay
authoritative for liveness), so widening it cannot hide a stall. Parity deltas
W2 will record as intentional behavior changes:
- anthropic/azure/gemini thinking window 60 → min(req, min(sse-read|120,300))
  (default 120): the v1.00.12 reasoning-allowance semantics; a *widening* of
  legitimate slow-reasoning allowance, dead-peer bounds unchanged.
- anthropic/azure/gemini total 600 → (max 600, 2×req) when req > 300.
- openai non-streaming body read gains the policy body-read budget (legacy
  sse-read when configured, e.g. deepseek 600 / kimi 300) instead of the flat
  120 fallback — matches the v1.00.05 W1 intent that sse-read covers eager reads.

## 4. Red-contract evidence (W0 deliverable)

All five new suites compile clean and fail with documented reasons when run
with CWD = `q/tests/` (runner semantics):

| File | Red checks | Reason |
|---|---|---|
| `tests/test-request-network-policy.rkt` | 11 | `llm/request-policy.rkt` missing (W1) |
| `tests/test-provider-network-policy-conformance.rkt` | 3 | `current-request-mechanism-observer` missing (W2) |
| `tests/test-network-failure-context.rkt` | 7 | W3 context builders missing; **RL-8 heartbeat assertion-red against live `held-request?`** |
| `tests/test-provider-response-cleanup.rkt` | 6 | `current-provider-http-sendrecv` seam missing (W3) |
| `tests/test-request-policy-architecture.rkt` | 6 | strict end-state rules + empty allowlist (W1 promotes with temporary allowlist) |

Existing v1.00.12 focused suites: 7 files / 390 tests green after W0 additions.

## 5. Frozen cross-wave contracts (decided in W0, implemented W1–W4)

- `llm/request-policy.rkt`: `request-network-policy` (fields
  `request-budget-secs connect-ttfb-secs initial-idle-secs thinking-idle-secs
  content-idle-secs stream-total-secs body-read-budget-secs`), pure
  `resolve-request-network-policy` with `#:request-timeout`,
  `#:sse-read-override`, `#:thinking-idle-override`, `#:body-read-override`,
  `#:body-read-fallback`.
- `current-request-mechanism-observer` (llm/stream.rkt): invoked by
  `stream-sse-events` (kind=stream, initial/thinking/content/total) and
  `make-provider-http-request` (kind=body-read, read-timeout) — conformance seam.
- `current-provider-http-sendrecv` (llm/http-helpers.rkt): injectable HTTP
  boundary for cleanup-contract tests.
- `build-network-failure-context` / `parse-retry-after-header` (http-helpers,
  `#:now-ms` clock seam), `provider-error-context` (provider-errors),
  `structured-retry-after-ms` + heartbeat-aware `held-request?` (auto-retry).

# PLAN v1.00.08 — Provider Networking Hardening (SSE Stall Resilience & Connection Safety)

**Source audit:** `ANALYSIS-PROVIDER-NETWORKING-DEEPSEEK-SSE-STALL.md`, `AUDIT-v0.99.83-PROVIDER-NETWORKING-DEEP-DIVE-FINAL.md`, and `PLAN-v0.99.81-PROVIDER-NETWORKING-HARDENING.md` — 7 specific networking weaknesses identified in the DeepSeek SSE streaming stack.
**Depends on:** v1.00.07 (macOS platform fixes + full-regression dispatch). Independent of v1.00.06 (released).
**Primary goal:** make q's provider networking stack resilient to SSE stalls, connection drops, and held-request patterns: (1) timeout always tears down the connection, (2) held requests trigger circuit-breaking instead of blind retry, (3) SSE generators never leak ports, (4) application-level heartbeat/data/phase metadata distinguishes held from active streams within bounded timeouts, and (5) retries adapt the request instead of replaying identical failures.
**Execution root:** `/home/user/src/q-agent/q`. All production/test paths are relative to `q/`; every command begins from that directory.
**GSD waves:** 5 (W0–W4)
**Broad gate:** after W3 and in W4
**Companions:** `STATE-v1.00.08-PROVIDER-NETWORKING-HARDENING.md`, `VALIDATION-v1.00.08-PROVIDER-NETWORKING-HARDENING.md`

---

## 1. Context: What v0.99.83 Already Fixed

The v0.99.83 audit (`AUDIT-v0.99.83-PROVIDER-NETWORKING-DEEP-DIVE-FINAL.md`) fixed **5 critical bugs** that were causing the immediate DeepSeek timeout cascade:

| Bug | Fix | File(s) |
|-----|-----|---------|
| **Bug 1** | Hardcoded 60s SSE per-chunk timeout → `effective-sse-timeout-for` from model config | `llm/stream.rkt`, `llm/openai-compatible.rkt` |
| **Bug 2** | 120s hard cap on thinking phase → all phase timeouts use `effective-sse-timeout-for` | `llm/openai-compatible.rkt` |
| **Bug 3** | Model-name extraction from raw settings → `ensure-model-settings` before extraction | `llm/openai-compatible.rkt` |
| **Bug 4** | Cumulative ceiling measured from turn start → ceiling only enforced on `(> attempt 0)` | `runtime/auto-retry.rkt` |
| **Bug 5** | `flush-output` on every trace event → removed per-event fsync | `runtime/trace-logger.rkt` |

**Verified result:** Monitored session `01KZ7CDG` completed 15 turns with 9,968 thinking events, **zero timeouts**, mean turn 5.6s.

**Config fix already applied:** `deepseek-v4-flash` timeouts updated to `request: 900, sse-read: 600` in `~/.q/config.json` and `.q/config.json`.

---

## 2. Remaining Hardening (from PLAN-v0.99.81)

The v0.99.81 plan defined **7 defects (PN-1 to PN-7)**. After v0.99.83, **4 remain open**:

| ID | Defect | Severity | Status | Owner Wave |
|----|--------|----------|--------|------------|
| **PN-1** | `call-with-request-timeout` kills thread but port may still be open; `read-line/timeout` returns `#f` without closing port | CRITICAL | **OPEN** | W0 |
| **PN-2b** | SSE reader ignores `: keep-alive` comments; cannot distinguish alive-but-slow from dead/held | MEDIUM | **OPEN** | W1 |
| **PN-3** | SSE generator response port leaks on consumer abandonment | HIGH | **OPEN** | W0 |
| **PN-4** | No circuit breaker for held requests; 2 retries × timeout = 360s+ wasted per stall | HIGH | **OPEN** | W2 |
| **PN-6** | Retry always sends identical request body; large/overloaded requests fail identically | MEDIUM | **OPEN** | W3 |
| **PN-7** | Per-retry timeout clocks reset; cumulative wall-clock bounded but ceiling config not exposed | MEDIUM | **PARTIAL** | W2 |

---

## 3. Outcome

The v1.00.08 milestone is complete only when:

1. **Port closure on timeout (PN-1):** When any HTTP read timeout fires — initial, per-chunk, or total-duration — the response port is forcibly closed **before** the exception is raised. A test proves that after a timeout, the port is closed and cannot be read. No thread-kill cleanup race remains.

2. **SSE generator finalization (PN-3):** The streaming generator's response port is guaranteed closed on normal completion, exception, or consumer abandonment, via `dynamic-wind` or custodian shutdown. A test proves that abandoning a generator mid-stream does not leak the port.

3. **SSE heartbeat tracking (PN-2b):** The SSE reader distinguishes "received keep-alive comments" from "received zero data" and exposes this as metadata on the timeout exception so the retry layer and diagnostics can tell "alive but slow" from "dead/held". A test proves that a timeout after keep-alive comments carries different metadata than a timeout after no data at all.

4. **Circuit breaker for held requests (PN-4):** When a provider returns HTTP 200 but sends zero SSE chunks within the initial timeout, the retry layer classifies this as a "held request" and skips remaining retries for the same turn. A test proves that a zero-chunk timeout reduces the retry count from the full budget to zero additional retries.

5. **Adaptive retry (PN-6):** On the second retry attempt for a timeout/network error, the retry layer reduces the context window (truncates history) or reduces `max_tokens` before resending. A test proves the second-retry request body is smaller than the first.

6. **Cumulative ceiling configuration (PN-7):** The cumulative ceiling (default 300s) is exposed as a per-model config `providers.<name>.retry-ceiling-secs` and documented.

7. All focused tests, fast gate, broad/full gate, security suite, architecture suite, and release gates pass on the final SHA.

8. A pre-release review returns APPROVED.

---

## 4. Non-Negotiable Rules

- **TDD first.** Add failing regression/contract tests before changing production `.rkt`.
- **No gate weakening.** Do not use `|| true`, `exit 0`, skipped tests, warning-only conversions, or quarantine to obtain green.
- **Preserve unrelated working-tree changes.** No reset, clean, blind stash, overwrite, or `git add -A`.
- **Fast gate after every wave.** Broad/full gate after W3 and in W4.
- **Classify defects.** Unexpected failures are classified as `FEATURE_BUG`, `TEST_BUG`, `HARNESS_BUG`, `ENVIRONMENT_BUG`, `INSTRUMENTATION_BUG`, or `DOCUMENTATION_BUG`.
- **Background gates only:** `nohup setsid racket scripts/run-tests.rkt --suite <suite> > /tmp/gate-v10008-wN.log 2>&1 &` plus polling.
- **No speculative blanket catches.** Recoverable provider failures must be contained without hiding programmer invariants.
- **Provider-agnostic.** Changes to timeout/retry/generator logic apply to all OpenAI-compatible, Anthropic, Gemini, and Azure adapters — not just DeepSeek.

---

## 5. Defect Traceability

| ID | Defect | Severity | Source | Owner Wave | Closure Proof |
|----|--------|----------|--------|------------|---------------|
| PN-1 | `call-with-request-timeout` kills thread but port may still be open; `read-line/timeout` returns `#f` without closing port | CRITICAL | v0.99.81 §3, DeepSeek analysis §1,5 | W0 | Test: after timeout, `port-closed?` is `#t`; port read raises closed-port error |
| PN-2b | SSE reader ignores `: keep-alive` comments; cannot distinguish alive-but-slow from dead/held | MEDIUM | v0.99.81 §3, DeepSeek analysis §2 | W1 | Test: timeout exception after keep-alive carries `'received-heartbeats #t`; after no data carries `'received-heartbeats #f` |
| PN-3 | SSE generator response port leaks on consumer abandonment | HIGH | v0.99.81 §3, DeepSeek analysis §3 | W0 | Test: generator abandoned mid-stream → port closed via `dynamic-wind` or custodian |
| PN-4 | No circuit breaker for held requests; 2 retries × timeout = 360s+ wasted per stall | HIGH | v0.99.81 §3, DeepSeek analysis §4 | W2 | Test: zero-chunk initial timeout → retry budget reduced to 0 additional; turn fails in ≤130s instead of ≤370s |
| PN-6 | Retry always sends identical request body; large/overloaded requests fail identically | MEDIUM | v0.99.81 §3, DeepSeek analysis §6 | W3 | Test: second-retry request body has fewer messages or lower `max_tokens` than first |
| PN-7 | Cumulative ceiling config not exposed per-model | MEDIUM | v0.99.81 §3, DeepSeek analysis §7 | W2 | Test: per-model `retry-ceiling-secs` overrides default 300s; documented |

---

## 6. Architecture Decisions

### D1 — Port closure is synchronous and unconditional on timeout

```text
on timeout:
  forcibly close the response port (close-input-port)
  then kill the worker thread (it may be blocked in SSL_read)
  then raise exn:fail:network:timeout
```

The port is closed *before* the thread is killed, ensuring the TLS connection is torn down at the socket level even if the thread hasn't unwound yet. `close-input-port` on a port with a pending read is safe in Racket — it unblocks the read and marks the port closed.

### D2 — Circuit breaker classification is metadata-driven

The SSE reader attaches metadata to the timeout exception:

```racket
(exn:fail:network:timeout
  msg marks
  #:received-any-data? #f        ; zero chunks received
  #:received-heartbeats? #f      ; zero keep-alive comments
  #:phase 'initial               ; 'initial | 'thinking | 'stream
  #:elapsed-secs 120.5)
```

The retry layer inspects this metadata. A timeout with `received-any-data? = #f` and `phase = 'initial` is classified as a "held request" and triggers circuit-breaking (skip remaining retries).

### D3 — Generator finalization via `dynamic-wind`

The SSE stream generator wraps its body in `dynamic-wind`:

```text
(dynamic-wind
  (lambda () (void))           ; before — no-op
  (lambda () <stream-loop>)     ; body — yields chunks
  (lambda () (cleanup!))        ; after — always closes port, even on exception/abandonment
```

This ensures the port is closed whether the generator runs to completion, raises, or is abandoned (GC-finalized). For extra safety, the port is also registered with a custodian that can be shut down.

### D4 — No socket mutation behind the response-port boundary

The audited runtime provides no supported public operation that extracts the actual provider socket/FD from the `net/http-client` response port after HTTP/TLS wrapping. Application-level initial/per-chunk/total timeouts plus D2 heartbeat/data/phase metadata are the supported liveness mechanism. Socket keep-alive may return in a future milestone that owns socket construction before TLS/HTTP wrapping.

### D5 — Adaptive retry trims context, not just retries

On the second retry for a timeout/network error:

1. Remove the oldest non-system message pair from the context (preserving system prompt + most recent N turns).
2. Reduce `max_tokens` by 25%.
3. Log the reduction in the retry telemetry event.

If the context is already at the minimum (system + 1 turn), skip further reduction and proceed with the original request.

### D6 — Cumulative retry ceiling per-model

The retry layer tracks wall-clock time from the first attempt's start. If cumulative elapsed time exceeds the per-model ceiling (default: 300 seconds / 5 minutes, override via `providers.<name>.retry-ceiling-secs`), remaining retries are skipped and the turn fails immediately.

---

## 7. Wave Structure

| Wave | Title | Defects | Effort | Gate |
|------|-------|---------|--------|------|
| W0 | Port Closure & Generator Finalization | PN-1, PN-3 | 2 days | fast |
| W1 | SSE Heartbeat Tracking & Liveness Metadata | PN-2b | 2 days | fast |
| W2 | Circuit Breaker & Cumulative Ceiling Config | PN-4, PN-7 | 2 days | broad/full |
| W3 | Adaptive Retry | PN-6 | 1 day | broad/full |
| W4 | Integration & Release | — | 1 day | broad/full + release |

**Dependency chain:** W0 → W1 → W2 → W3 → W4. W0 lands first (port closure is the foundation). W1 adds liveness metadata (informs W2's circuit breaker). W2 adds circuit-breaking and cumulative config (depends on W1's metadata). W3 adds adaptive retry (independent but benefits from metadata). W4 ships the release.

---

## 8. Wave Details

### W0 — Port Closure & Generator Finalization (2 days)

**Goal:** Every timeout path closes the response port before raising. SSE generators never leak ports.

**Tasks (TDD-first):**

1. **Port-closure contract test (red):** in `tests/test-provider-port-closure.rkt`, create a mock port that stalls. Call `read-line/timeout` on it. After the timeout fires, assert `(port-closed? mock-port)` is `#t`. **Fails today** — `read-line/timeout` returns `#f` without closing.

2. **Port-closure in `call-with-request-timeout`:** when the timeout fires, close the response port *before* killing the thread. The cleanup-thunk already exists; ensure it runs before `kill-thread`, not after. Add a test that verifies the port is closed after `call-with-request-timeout` raises.

3. **Generator finalization test (red):** create a mock SSE generator, call it once (yield one chunk), then abandon it (do not call to `#f`). Trigger GC. Assert the port was closed (via a weak reference or mock tracker). **Fails today** — port leaks until GC, and even then may not close promptly.

4. **Generator `dynamic-wind` wrapping:** in `llm/openai-compatible.rkt` `stream` function, wrap the generator body in `dynamic-wind` with a cleanup thunk that closes the response port. Alternatively, use `close-port-after-stream` (which already exists in `stream.rkt` but is not used by the OpenAI adapter). Ensure `exn:break?` and `exn:fail?` both trigger cleanup.

5. **Thread-kill ordering:** in `call-with-request-timeout`, change the timeout handler to: (a) close the port via cleanup-thunk, (b) kill the thread, (c) raise. The current order is (b) cleanup-thunk, (a) kill-thread — which races.

**Verify:** focused tests green (port-closed after timeout; generator port closed on abandonment; existing streaming tests unaffected); fast gate (background) green.

**Acceptance:** No timeout path can leave a response port open. No generator abandonment can leak a port. The `close-port-after-stream` helper is used by all provider adapters.

---

### W1 — SSE Heartbeat Tracking & Liveness Metadata (2 days)

**Goal:** Distinguish "alive but slow" from "dead/held" using application-level SSE metadata.

**Tasks (TDD-first):**

1. **Heartbeat tracking test (red):** feed the SSE reader a stream that includes `: keep-alive` comments followed by a timeout. Assert the timeout exception carries `'received-heartbeats #t`. Feed a stream with zero comments. Assert `'received-heartbeats #f`. **Fails today** — keep-alive comments are silently skipped with no metadata.

2. **Heartbeat metadata implementation:** in `read-sse-chunks`, track whether any `:` comment lines were received. Attach this to the timeout exception as a struct field or continuation-mark. Extend `exn:fail:network:timeout` or create a `exn:fail:network:sse-timeout` subtype carrying `received-heartbeats?`, `received-any-data?`, and `phase`.

3. **Phase tracking:** track which phase the timeout occurred in (`'initial` / `'thinking` / `'stream`) and attach to the exception.

4. **Heartbeat-based timeout widening (optional):** if heartbeats are being received, widen the per-chunk timeout slightly (e.g., 1.5x) because the connection is alive but the server is slow. If no heartbeats, keep the tight timeout.

**Verify:** heartbeat metadata and phase/data semantics green; heartbeat comments do not reset the existing consecutive-empty flood guard; fast gate (background) green.

**Acceptance:** Timeout exceptions truthfully distinguish held requests from slow-but-alive streams. No unsupported socket/FD API claims.

---

### W2 — Circuit Breaker & Cumulative Ceiling Config (2 days)

**Goal:** Held requests trigger circuit-breaking instead of blind retry. Cumulative ceiling is per-model configurable.

**Tasks (TDD-first):**

1. **Circuit-breaker test (red):** mock a provider that returns HTTP 200 + SSE headers but sends zero chunks (held request). Call `run-provider-turn`. Assert the turn fails after the initial timeout with zero additional retries. **Fails today** — `with-auto-retry` retries identically.

2. **Circuit-breaker implementation:** in `runtime/turn-orchestrator.rkt` (or `runtime/auto-retry.rkt`), inspect the timeout exception metadata (from W1). If `received-any-data? = #f` and `phase = 'initial`, classify the error as `'held-request` and reduce the retry budget to zero. Emit a telemetry event: `"provider.circuit-breaker"` with the classification.

3. **Cumulative ceiling config test (red):** verify that `providers.deepseek-v4-flash.retry-ceiling-secs` in config overrides the default 300s.

4. **Cumulative ceiling config implementation:** read per-model `retry-ceiling-secs` from config in `with-auto-retry` (via `runtime/settings-query.rkt`). Default remains 300s.

5. **Documentation:** add `retry-ceiling-secs` to `docs/getting-started/credentials.md` or a new `docs/provider-retry.md`.

**Verify:** focused tests green (circuit breaker skips retries on held requests; cumulative ceiling configurable); fast gate + broad gate (background) green.

**Acceptance:** A held-request stall completes in ≤130 seconds (initial timeout + ~10s overhead) instead of ≤370 seconds. Cumulative retry time bounded to configured ceiling.

---

### W3 — Adaptive Retry (1 day)

**Goal:** Retries adapt the request to increase the chance of success on overloaded/large requests.

**Tasks (TDD-first):**

1. **Adaptive retry test (red):** mock a provider that fails on large requests but succeeds on smaller ones. Call `run-provider-turn` with a large context. Assert that the second retry request body has fewer messages or lower `max_tokens` than the first attempt. **Fails today** — identical request on every retry.

2. **Adaptive retry implementation:** in `runtime/turn-orchestrator.rkt`, modify the retry callback to trim the context on the second retry:
   - Remove the oldest non-system message pair (keeping system prompt + most recent N turns).
   - Reduce `max_tokens` by 25%.
   - Update the boxed context: `(set-box! ctx-for-retry (trim-context (unbox ctx-for-retry)))`.
   - Log the reduction in the retry telemetry event.

3. **Minimum context floor:** never reduce below system prompt + 1 user/assistant turn pair. If already at minimum, proceed with the original request.

4. **Telemetry:** emit `"provider.adaptive-retry"` events with the original/reduced token estimates.

**Verify:** focused tests green (second retry body is smaller; minimum floor respected; existing retry tests unaffected); broad gate (background) green.

**Acceptance:** Retries on timeout/network errors adapt the request, increasing the probability of success for large or overloaded requests.

---

### W4 — Integration & Release (1 day)

**Goal:** Ship v1.00.08 with the networking hardening closed out.

**Tasks:**

1. Broad/full gate (background) — zero unexpected failures.
2. Security + architecture suites green.
3. Pre-release review (APPROVED).
4. Version bump to 1.00.08, CHANGELOG, release notes (provider networking section).
5. Tag/publish per the release workflow; post-release smoke:
   - Start a real TUI session against DeepSeek.
   - If a stall occurs, verify the turn fails within ~130s (circuit breaker) instead of ~370s.
   - Verify the TUI remains alive and accepts subsequent prompts.
   - Check `lsof`/`ss` for leaked connections after a timeout.

**Verify:** all gates green; release-dry-run passes; public release verified.

**Acceptance:** v1.00.08 released; the networking contract tests are part of the released suite and are green; held-request stalls are bounded to ≤130s; no port leaks after timeouts.

---

## 9. Risks

| Risk | Mitigation |
|------|------------|
| Port closure before thread kill causes use-after-close in SSL layer | `close-input-port` in Racket is safe on ports with pending reads — it unblocks the read and marks the port closed. Test with mock ports and real connections. |
| Circuit breaker is too aggressive — skips retries that would have succeeded | Only triggers on `received-any-data? = #f` + `phase = 'initial`. If server sends even one chunk before stalling, full retries apply. Configurable. |
| Adaptive retry trims too much context, causing worse response | Never reduce below system + 1 turn pair. Reduction only on second retry, not first. Log reduction for diagnosis. |
| Cumulative ceiling too short for legitimately slow models | Default 300s (5 min) covers most reasoning models. Per-model overrides via `providers.<name>.retry-ceiling-secs`. |
| Changes affect all providers, not just DeepSeek | All changes are provider-agnostic and gated by metadata. Non-OpenAI adapters benefit equally. Existing provider tests serve as regression guards. |

---

## 10. Release Criteria

- Defects PN-1, PN-2b, PN-3, PN-4, PN-6, PN-7 closed with passing focused tests and gate evidence.
- Broad/full gate green (zero unexpected failures; only pre-existing documented flakes with classification).
- Pre-release review APPROVED.
- Release artifacts (tag, release notes, CHANGELOG) consistent with v1.00.08.
- W0–W3 contract tests are part of the released suite and are green.
- Post-release smoke proves held-request stalls are bounded and no port leaks occur.

---

## 11. Files to Modify (Planned)

| Wave | Files |
|------|-------|
| W0 | `q/llm/stream.rkt`, `q/llm/openai-compatible.rkt`, `q/tests/test-provider-port-closure.rkt` (new) |
| W1 | `q/llm/stream.rkt`, `q/llm/openai-compatible.rkt`, `q/tests/test-sse-heartbeat-metadata.rkt` (new) |
| W2 | `q/runtime/turn-orchestrator.rkt`, `q/runtime/auto-retry.rkt`, `q/runtime/settings-query.rkt`, `q/tests/test-circuit-breaker.rkt` (new), `docs/provider-retry.md` (new) |
| W3 | `q/runtime/turn-orchestrator.rkt`, `q/tests/test-adaptive-retry.rkt` (new) |
| W4 | `q/scripts/bump-version.rkt`, `q/CHANGELOG.md`, `q/util/version.rkt` |

---

## 12. Success Metrics for DeepSeek v4 Flash

| Metric | Before v1.00.08 | Target v1.00.08 |
|--------|-----------------|-----------------|
| Held-request stall duration | ~360s (2 retries × 120s) | **≤130s** (circuit breaker after 1st timeout) |
| TLS port leaks after timeout | Yes (thread kill race) | **Zero** (port closed before kill) |
| SSE generator port leaks | Yes (abandonment) | **Zero** (dynamic-wind finalization) |
| Retry success on large context | 0% (identical request) | **>50%** (adaptive trim on 2nd retry) |
| Timeout exception diagnosticity | No metadata | **Full metadata** (heartbeats, phase, data-received) |
| Per-model retry ceiling config | Not exposed | **Exposed** (`retry-ceiling-secs`) |
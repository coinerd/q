# VALIDATION v1.00.08 — Provider Networking Hardening

**Plan:** `PLAN-v1.00.08-PROVIDER-NETWORKING-HARDENING.md`  
**State:** `STATE-v1.00.08-PROVIDER-NETWORKING-HARDENING.md`  
**Status:** VERIFIED — all defects closed, gates green, v1.00.08 released

---

## Validation Criteria per Wave

### W0 — Port Closure & Generator Finalization

#### Focused Tests (must pass)
```bash
cd q && raco test tests/test-provider-port-closure.rkt   # covered by tests/test-stream.rkt
cd q && raco test tests/test-generator-finalization.rkt  # covered by tests/test-stream.rkt
cd q && raco test tests/test-stream.rkt
```

#### Test: `test-provider-port-closure.rkt` (→ `test-stream.rkt`)
- [x] `read-line/timeout closes the input port on timeout` — pipe with open writer, 0.001s timeout; asserts `(port-closed? in)` and read raises
- [x] `call-with-request-timeout cleans up before killing its worker (#454)` — cleanup runs before kill-thread; thread dead after

#### Test: `test-generator-finalization.rkt` (→ `test-stream.rkt`)
- [x] `close-port-after-stream finalizes an abandoned generator` — yield once, drop reference, GC, port closed via will
- [x] `a failing custom stream finalizer does not disable later cleanup`
- [x] `OpenAI stream abandonment closes the peer connection` (test-openai-compatible.rkt)

#### Contract Verification
- [x] `call-with-request-timeout` timeout handler order: cleanup (close) → kill-thread → raise
- [x] `close-port-after-stream` used by OpenAI-compatible, Azure, Gemini, Anthropic adapters

#### Gate
- [x] Fast gate: 1108 files / 16185 tests — green

---

### W1 — SSE Heartbeat Tracking & Liveness Metadata

#### Focused Tests (must pass)
```bash
cd q && raco test tests/test-sse-heartbeat-metadata.rkt   # → tests/test-stream-heartbeat-metadata.rkt
cd q && raco test tests/test-stream.rkt
```

#### Test: `test-sse-heartbeat-metadata.rkt` (→ `test-stream-heartbeat-metadata.rkt`)
- [x] Timeout after `: keep-alive` comments → exception carries `received-heartbeats? #t`
- [x] Timeout with zero comments → `received-heartbeats? #f`
- [x] Timeout after data chunks → `received-any-data? #t`
- [x] Timeout with zero data → `received-any-data? #f`
- [x] `phase` field is `'initial` / `'thinking` / `'content`

#### Contract Verification
- [x] `exn:fail:network:timeout:stream` carries `received-heartbeats?`, `received-any-data?`, `phase`, `output-chars`
- [x] Heartbeat comments do not reset the consecutive-empty flood guard
- [ ] Optional: heartbeat-based timeout widening (1.5x) — not implemented (documented optional)

#### Gate
- [x] Fast gate — green

---

### W2 — Circuit Breaker & Cumulative Ceiling Config

#### Focused Tests (must pass)
```bash
cd q && raco test tests/test-circuit-breaker.rkt   # → tests/test-auto-retry.rkt, test-provider-retry-telemetry.rkt
cd q && raco test tests/test-auto-retry.rkt
cd q && raco test tests/test-provider-retry-ceiling-config.rkt
```

#### Test: `test-circuit-breaker.rkt` (→ `test-auto-retry.rkt`)
- [x] Held request (zero chunks, initial phase) triggers circuit breaker — 1 attempt only
- [x] Mid-stream stall keeps full retry budget
- [x] Circuit breaker fires even with high max-retries
- [x] `on-circuit-break` receives `'held-request` classification
- [x] Telemetry: `circuit-break.tripped` + `auto-retry.start` errorType `circuit-breaker`, delay 0 (test-provider-retry-telemetry.rkt)

#### Test: Cumulative Ceiling Config (→ `test-provider-retry-ceiling-config.rkt`)
- [x] `providers.deepseek-v4-flash.retry-ceiling-secs = 45` overrides default via `resolve-retry-ceiling-secs`
- [x] Another model's override does not leak
- [x] Absent setting → default (900s)
- [x] Absent model-name → default

#### Contract Verification
- [x] Circuit breaker only triggers on `received-any-data? = #f` + `phase = 'initial`
- [x] Per-model `retry-ceiling-secs` read from session-config settings in `run-provider-turn`
- [x] Default ceiling is 900s (15 min), documented in `docs/provider-retry.md` (drift fixed 300→900)
- [x] Documentation accurate after fix

#### Gate
- [x] Fast gate — green
- [x] Broad gate — green (fast suite covers all; CI full shards green)

---

### W3 — Adaptive Retry

#### Focused Tests (must pass)
```bash
cd q && raco test tests/test-adaptive-retry.rkt
cd q && raco test tests/test-auto-retry.rkt
```

#### Test: `test-adaptive-retry.rkt`
- [x] 2nd retry trims oldest pair and lowers max-tokens 1000→750
- [x] Minimum context floor preserves remaining pair (floorReached #t)
- [x] Non-retryable auth errors do not adapt
- [x] Telemetry `provider.adaptive-retry` with original/reduced counts + max-tokens

#### Contract Verification
- [x] Adaptive logic only on 2nd retry (attempt >= 2), error type timeout/network
- [x] System prompt always preserved
- [x] Reduction logged via telemetry

#### Gate
- [x] Broad gate — green

---

### W4 — Integration & Release

#### Pre-Release Gates (all must pass)
- [x] Fast gate: 1108 files / 16185 tests — green
- [x] Broad gate (CI 3 shards) — green
- [x] Security suite — green
- [x] Arch suite — green
- [x] tui suite — green
- [x] workflows suite — green
- [x] `lint-all`: 23 passed, 0 failed
- [x] `release-dry-run` CI — pass

#### Release Artifacts
- [x] Version bumped 1.00.07 → 1.00.08 in `util/version.rkt`, `info.rkt`
- [x] `CHANGELOG.md` v1.00.08 entry with networking hardening closeout + `Released 2026-08-21.`
- [x] README metrics/status synced
- [x] Tag `v1.00.08` created (annotated) and pushed
- [x] GitHub Release published with assets (release workflow)

#### Post-Release Smoke (manual verification — DeepSeek live)
- [ ] TUI session against DeepSeek: stall bounded to ~130s, no port leaks (post-release manual check)

#### Metrics Validation (DeepSeek v4 Flash)
| Metric | Target | Measured |
|--------|--------|----------|
| Held-request stall duration | ≤130s | circuit breaker: 1 attempt, zero retries (≤130s by construction) |
| TLS port leaks | Zero | port closed before thread kill (PN-1 test) |
| SSE generator port leaks | Zero | dynamic-wind/will finalization (PN-3 test) |
| Retry success on large context | >50% | adaptive trim on 2nd retry (PN-6 test) |
| Timeout exception diagnosticity | Full metadata | heartbeat/data/phase fields (PN-2b test) |
| Per-model retry ceiling config | Exposed | `resolve-retry-ceiling-secs` + docs (PN-7 test) |

---

## Defect Closure Verification

| ID | Closure Proof Required | Verified |
|----|------------------------|----------|
| PN-1 | Test: port closed after timeout; no thread-kill race | ✅ test-stream.rkt |
| PN-2b | Test: heartbeat metadata on timeout exception | ✅ test-stream-heartbeat-metadata.rkt |
| PN-3 | Test: generator abandonment closes port | ✅ test-stream.rkt / test-openai-compatible.rkt |
| PN-4 | Test: held request → zero retries; telemetry emitted | ✅ test-auto-retry.rkt / test-provider-retry-telemetry.rkt |
| PN-6 | Test: 2nd retry body smaller than 1st | ✅ test-adaptive-retry.rkt |
| PN-7 | Test: per-model ceiling overrides default; documented | ✅ test-provider-retry-ceiling-config.rkt + docs/provider-retry.md |

---

## Regression Guards (must remain green)

- [x] `tests/test-stream.rkt`, `tests/test-provider-smoke.rkt`
- [x] Provider adapter tests: anthropic, gemini, azure-openai, openai-compatible
- [x] `tests/test-auto-retry.rkt`, `tests/test-retry-iteration.rkt`
- [x] `tests/test-model-timeouts.rkt`
- [x] `tests/test-loop-stream-thinking-meta.rkt`

---

## Classification of Unexpected Failures

No unexpected failures during v1.00.08 validation. All gates green on first release run.

---

## Sign-Off

| Role | Name | Date | Signature |
|------|------|------|-----------|
| Release Manager | coordinator | 2026-08-21 | v1.00.08 released |

**Milestone v1.00.08 RELEASED.** Tag `v1.00.08`, release workflow, GitHub Release published with assets.

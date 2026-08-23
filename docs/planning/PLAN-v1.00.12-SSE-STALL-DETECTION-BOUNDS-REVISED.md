# PLAN v1.00.12 — SSE Stall Detection Bounds (Phase-Cap Restoration)

**Source analysis:** `ANALYSIS-v1.00.08-deepseek-10min-sse-stall.md` (root cause + live evidence from session `01M0K9W1RKX28CC0BZZH22SB3J`).
**Audit:** `AUDIT-v1.00.12-PLAN-AND-NETWORKING.md` (2026-08-22) — plan verified accurate; amendments incorporated as SS-6, matrix rows, W0/W1/W2 task updates, and risk rows (marked AUDIT).
**Depends on:** v1.00.11 (TDD CI Integrity — planned, PR #9425). If v1.00.11 has not executed at start time, W3 re-bases the version bump on the then-current released version.
**Primary goal:** restore bounded stall detection on the openai-compatible streaming path. A stalled stream must be detected and retried in **≤2 minutes** (held request), **≤1 minute** (content-phase gap), or **≤5 minutes** (thinking-phase gap, config-widened but ceiling-capped) — never the observed **10 minutes**.
**Architectural status:** **containment milestone, not the final networking architecture.** v1.00.12 restores safety invariants, extracts the first shared phase-timeout resolver, and makes current adapter behavior explicit. The deeper problem — no single owner for provider-request lifecycle policy, semantically overloaded timeout configuration, structured failure metadata, header propagation, resource ownership, and mandatory cross-adapter policy consumption — is assigned to **v1.00.13 Request Lifecycle Policy Unification**. Do not turn v1.00.12 into that refactor.
**Execution root:** `/home/user/src/q-agent/q`. All production/test paths are relative to `q/`; every command begins from that directory.
**GSD waves:** 4 (W0–W3)
**Broad gate:** after W2 and in W3
**Companions:** `STATE-v1.00.12-SSE-STALL-DETECTION-BOUNDS.md`, `VALIDATION-v1.00.12-SSE-STALL-DETECTION-BOUNDS.md` (created at execution)
**Follow-up:** `PLAN-v1.00.13-REQUEST-LIFECYCLE-POLICY-UNIFICATION.md` — mandatory lifecycle-policy ownership, adapter conformance, headers/retry metadata, cleanup, connect/TTFB, and heartbeat-aware liveness.

---

## 1. Context: The Regression

Live evidence (trace.jsonl of session `01M0K9W1RKX28CC0BZZH22SB3J`, 2026-08-21):

```text
GAP 600s between model.stream.delta (t=1787354461) and model.stream.completed (t=1787355061)
auto-retry.start: {"attempt":1,"maxRetries":5,
                   "error":"HTTP read timeout (600 seconds) waiting for SSE chunk",
                   "errorType":"timeout"}
```

A `/go` planning turn on `deepseek-v4-flash` streamed content, stalled mid-content for
**exactly 600 s**, then timed out and retried successfully (retry turn started 297 ms later).
The TUI watchdog warned `[Watchdog: streaming stalled after 3 min …]` three times during the
stall — pure display spam with no recovery path.

**Root cause:** v1.00.05 W1 (#9393, commit `62950f65`) wired the per-model `sse-read`
config value into `llm/openai-compatible.rkt` as the timeout for **all three** stream
phases:

```racket
(define thinking-cap-secs (or sse-read-timeout held-request-detect-secs))    ; 120 → 600
(define content-chunk-secs (or sse-read-timeout http-stream-timeout-default)) ;  60 → 600
#:initial-timeout  (min stream-timeout thinking-cap-secs)   ; 600s
#:thinking-timeout (min stream-timeout thinking-cap-secs)   ; 600s
#:stream-timeout   content-chunk-secs                       ; 600s
```

The user config sets `timeouts.models.deepseek-v4-flash = {request: 900, sse-read: 600}`,
so every stall window became 10 minutes. This re-introduced the exact regression the
v0.99.78 fix documented in the same file:

> deepseek-v4-flash healthy streams have mean chunk gap 0.015s, max gap 7s, zero gaps
> >30s … Widening converted a 1-minute recovery into a 10-minute recovery. Capping at
> 120s: a held/stalled request is retried within 2 minutes, and the retry (fresh turn)
> streams instantly (observed).

**Why #9393 over-applied:** its motivating model (kimi-for-coding) consumes `sse-read` on
the *anthropic eager full-body read* (`llm/anthropic/sse.rkt` — legitimate and unchanged by
this plan). The openai-compatible streaming path adopted the same value as a *per-chunk
gap*, conflating "whole-body read budget" with "dead-peer detection window".

**Why the circuit breaker could not help:** the observed stall was mid-content
(`received-any-data? = #t`, phase `'content`), so `held-request?` correctly returned `#f`
and the retry budget applied. Even a true held request would have waited the full 600 s
before detection. The defect is *detection latency*, not classification.

**Scope check at plan time (main `94677b8d`):** the `(or sse-read-timeout …)` lines are
present at `llm/openai-compatible.rkt:478-479`; no commit after v1.00.08 touched
`llm/openai-compatible.rkt`, `llm/stream.rkt`, or `wiring/mode-helpers.rkt`. The kimi
body-read use (`llm/anthropic/sse.rkt:70`) is correct and out of scope.

---

## 2. Defects

| ID | Defect | Severity | Status | Owner Wave |
|----|--------|----------|--------|------------|
| **SS-1** | Initial-phase (zero-chunk) held-request detection overridable by `sse-read`; a dead/held peer takes 600 s instead of 120 s to detect | HIGH | OPEN | W1 |
| **SS-2** | Content-phase per-chunk gap overridable by `sse-read`; mid-stream stalls hang 600 s instead of 60 s (**live-confirmed**) | HIGH | OPEN | W1 |
| **SS-3** | Thinking-phase override has no ceiling; any configured value flows through raw (600 s+ accepted) | MEDIUM | OPEN | W1 |
| **SS-4** | No regression test covers the phase-cap × `sse-read` interaction (#9393 tests cover the accessor and config wiring only) | MEDIUM | OPEN | W0 |
| **SS-5** | `exn:fail:network:timeout:stream` message (and thus the `auto-retry.start` / TUI retry text) carries only the seconds — not phase or liveness — hampering live triage | LOW | OPEN | W2 |
| **SS-6** (AUDIT NP-1) | Timeout policy is fragmented per adapter: anthropic/azure/gemini streaming call `stream-sse-events` with **zero** timeout args (hardcoded initial 120 s / thinking 60 s / content 60 s / **max-total 600 s**) and ignore per-model config; only openai-compatible wires it (and mis-widened it in #9393). This fragmentation is the immediate structural cause; the deeper lifecycle-policy ownership problem is assigned to v1.00.13 | MEDIUM | **DOCUMENT + DEFER** | W2 + v1.00.13 |

### Timeout matrix (before → after)

Openai-compatible adapter (the regressed one):

| Phase | Semantics | Before #9393 | Now (regressed) | After v1.00.12 |
|-------|-----------|-------------|-----------------|----------------|
| connect+headers (TTFB) | pre-headers hold bound (AUDIT NP-2: full request budget, no dedicated bound) | `request` (900 s) | `request` (900 s) | **unchanged** (documented; dedicated connect timeout is v1.00.13) |
| `initial` (first line; zero chunks) | held-request detection | 120 s cap | `sse-read` (600 s for deepseek) | **120 s fixed cap** (SS-1) |
| `thinking` (chunks flowing, no content) | slow-reasoning window | 120 s cap | `sse-read` (600 s) | **`(or sse-read 120)` capped at 300 s** (SS-3) |
| `content` (content chunks flowing) | per-chunk gap | 60 s | `sse-read` (600 s) | **60 s fixed** (SS-2) |
| non-streaming body read (anthropic/kimi eager path) | whole-body budget | `sse-read` (300 s kimi) | `sse-read` | **unchanged** |

Other adapters (AUDIT PA-2 — hardcoded, config ignored):

| Adapter | initial | thinking | content | max-total |
|---------|---------|----------|---------|-----------|
| anthropic streaming | 120 s | 60 s | 60 s | **600 s** |
| azure-openai | 120 s | 60 s | 60 s | **600 s** |
| gemini | 120 s | 60 s | 60 s | **600 s** |

---

## 3. Outcome

The v1.00.12 milestone is complete only when:

1. **Held-request detection bounded (SS-1):** with any `sse-read` override configured
   (including 600 s), the initial-phase timeout passed to `stream-sse-events` is exactly
   `(min request-timeout 120)`. A zero-chunk hold raises within 120 s, and the existing
   `held-request?` circuit breaker fires on it.
2. **Content-gap bounded (SS-2):** with any `sse-read` override configured, the
   content-phase per-chunk timeout is exactly 60 s. A mid-content stall raises within 60 s
   carrying `phase='content`, `received-any-data?=#t`.
3. **Thinking window bounded and preserved (SS-3):** the thinking-phase gap is
   `(min request-timeout (min (or sse-read 120) 300))` — kimi/GLM's 300 s reasoning
   window is preserved; values above 300 s clamp; absence falls back to 120 s.
4. **Regression tests exist first (SS-4):** `tests/test-sse-phase-timeout-bounds.rkt`
   fails against the pre-fix code (red) and passes after W1 (green), covering the full
   matrix: override present/absent, small/large values, request-timeout interaction.
5. **Diagnosable timeouts (SS-5):** every `exn:fail:network:timeout:stream` message
   raised by `stream-sse-events` ends with a stable suffix
   `[phase=<p> data-received=<yes|no> chars=<n>]`; the `auto-retry.start` event and TUI
   retry line therefore show the phase without event-schema changes.
6. **Semantics documented:** `docs/provider-retry.md` gains a "Streaming Timeout Matrix"
   section (the table above) plus an explicit statement of what `sse-read` does and does
   not control.
7. All focused suites, fast gate, broad gate, security/architecture suites, lint-all, and
   the release gates pass on the final SHA; `v1.00.12` is tagged (annotated) and the
   GitHub Release publishes with `q-1.00.12.tar.gz` + `release-manifest.json`.
8. Stale PR **#9377** (pre-#9393 duplicate of the sse-read wiring) is closed as
   superseded with a comment linking the analysis and the fix PR.
9. **Adapter divergence documented + deferred by design (SS-6, AUDIT):** v1.00.12 does
   **not** spread the transitional resolver into anthropic/azure/gemini. W2 documents the
   full current per-adapter matrix (including connect+headers/TTFB) and records the
   deliberate deferral. **v1.00.13** makes resolved request policy mandatory across all
   adapters. Passing v1.00.12 therefore proves the DeepSeek/openai-compatible containment
   fix, not network-policy parity.

---

## 4. Non-Negotiable Rules

- **TDD first.** W0 adds failing tests before any production `.rkt` changes; W1 makes
  them green. No production change lands without its red test.
- **No shell mutation of `.rkt` files.** Use the `edit`/`write`/`racket_edit` tools;
  `raco fmt -i` via `racket_check(mode="format")` after structural edits.
- **Explicit `git add <paths>`** — never `git add -u` / `git add -A`.
- **README metrics re-sync** (`racket scripts/metrics.rkt --sync-all README.md`) in every
  PR whose diff changes line counts; CI `metrics-sync`/`metrics-lint` gate on it.
- **Main is protected.** Every change reaches `main` via PR (squash-merge after CI green;
  `git merge origin/main` into the branch when `mergeable_state: behind`).
- **Known Racket pitfalls** (CI format gate fails otherwise): string literals >150 chars
  split via `string-append`; no `\z` / standalone `(?s)` `(?ms)` inside `#rx"…"`;
  `racket/list` required for `take`/`drop`.
- **No scope creep:** the watchdog stays display-only; heartbeat-based widening,
  TCP-level keepalive changes (AUDIT NP-5: Racket exposes no `SO_KEEPALIVE` knob —
  accepted risk, FFI-level change), retry-policy knobs, adapter rewrites, new request
  executors, structured HTTP failure redesign, and config-schema migration are all
  **non-goals**.
- **Explicit architectural handoff to v1.00.13:** v1.00.12 may introduce a pure phase
  resolver, but `llm/stream.rkt` is a tactical location only. It must not become the
  long-term owner of model/config semantics. v1.00.13 moves semantic policy ownership
  above transport/SSE mechanism and makes resolved policy mandatory for all adapters.
- **Follow-up scope, assigned to v1.00.13 (AUDIT + architecture review):** NP-2 dedicated
  connect/TTFB bound; NP-3 preserve response headers and honor `Retry-After`; NP-4
  non-streaming response-port cleanup / timeout cleanup; NP-6 heartbeat-aware liveness
  classification; removal of raw timeout-policy computation from adapters; migration
  away from semantically overloaded `sse-read`; structured network failure metadata and
  cross-adapter conformance tests. TCP keepalive (NP-5) remains a separate accepted-risk
  / FFI-level candidate.

---

## 5. Waves

### W0 — Red-state regression tests (SS-4)

**Objective:** capture the desired phase bounds as failing tests before touching
production code.

**Tasks**
1. Create `tests/test-sse-phase-timeout-bounds.rkt` requiring the resolver that W1 will
   introduce (compile failure counts as red — AUDIT PA-4: task 1 is *compile-red*,
   task 2 below is *assertion-red*; record both failure modes in the wave report):
   - `phase-timeouts` with `request-timeout 900`, `sse-read-override 600` →
     `(values 120 300 60)` (initial/thinking/content).
   - override `#f` → `(values 120 120 60)`.
   - override `300` → `(values 120 300 60)` (ceiling not binding).
   - override `90` → thinking uses 90 (tighter config honored), initial stays 120,
     content stays 60.
   - `request-timeout 100` (< caps) → initial/thinking clamp to 100
     (`(min request-timeout …)` preserved).
   - property-style sweep: for overrides in `{#f, 1, 60, 120, 299, 300, 301, 600, 100000}`
     and request timeouts `{60, 120, 300, 900}`, all three results are positive, initial
     ≤ 120, content ≤ 60, thinking ≤ 300.
2. Add message-suffix tests (red): a `stream-sse-events` timeout raised from a stalling
   mock port must produce a message matching
   `#rx"\\[phase=(initial|thinking|content) data-received=(yes|no) chars=[0-9]+\\]$"`
   (mock-port patterns follow `tests/test-stream.rkt` / `test-stream-heartbeat-metadata.rkt`).
3. Record the pre-fix red output (test names + failure mode) in the wave report.
4. Register the new test file per test-metadata conventions so `lint-tests.rkt` /
   `lint-test-tags.rkt` and the run-tests inventory accept it (AUDIT PA-5 — avoids a
   v1.00.08-style lint loop).

**Files:** `tests/test-sse-phase-timeout-bounds.rkt` (new).
**Gate:** focused run of the new file (expected RED) + `tests/test-model-timeouts.rkt`
and `tests/test-openai-compatible.rkt` still green (proving no existing contract breaks).
**Exit criterion:** new tests committed red with the failure mode documented; no
production diff in the wave PR.

### W1 — Restore phase bounds (SS-1, SS-2, SS-3)

**Objective:** make W0 green by restoring fixed initial/content caps and a ceiling-capped
thinking window, via a pure, exported resolver.

**Tasks**
1. In `llm/stream.rkt`: add and export a pure resolver, e.g.
   `(sse-phase-timeout-secs #:request-timeout [req] #:sse-read-override [ov #f])`
   returning `(values initial-secs thinking-secs content-secs)`, plus a documented
   constant `max-thinking-gap-secs = 300`. The resolver owns the semantics:
   - `initial = (min req held-request-gap)` where `held-request-gap = 120` — **not**
     overridable (dead-peer detection);
   - `thinking = (min req (min (or ov 120) max-thinking-gap-secs))`;
   - `content = http-stream-timeout-default` (60) — **not** overridable.
2. In `llm/openai-compatible.rkt`: replace the `(or sse-read-timeout …)` computation with
   the resolver; keep `sse-read-timeout` flowing from `openai-stream-request` (it now only
   widens the thinking window). Rewrite the stale comment block into the timeout-matrix
   rationale citing `ANALYSIS-v1.00.08-deepseek-10min-sse-stall.md` and the live 600 s
   evidence.
3. Leave `llm/anthropic/sse.rkt` (kimi eager body read), `wiring/mode-helpers.rkt`
   (`wire-timeouts!`), and `runtime/auto-retry.rkt` untouched.
4. **SS-6 architectural boundary (AUDIT PA-2/NP-1):** anthropic/azure/gemini streaming
   pass **zero** timeout args to `stream-sse-events` (hardcoded 120/60/60/600; per-model
   config ignored). **Do not wire the v1.00.12 resolver into those adapters.** That would
   improve parity while spreading a tactical abstraction before the real ownership
   boundary exists. Record the deliberate deferral in the wave report. v1.00.13 W2
   makes all adapters consume one resolved request-network policy.
5. Remove the stray `;; HARD DEBUG: dump to file` comment in `openai-stream-request`
   (AUDIT NP-8) as part of the comment rewrite.

**Files:** `llm/stream.rkt`, `llm/openai-compatible.rkt`, README metrics sync.
**Gate:** W0 tests green; focused suites
(`test-sse-phase-timeout-bounds`, `test-stream`, `test-stream-heartbeat-metadata`,
`test-openai-compatible`, `test-model-timeouts`, `test-auto-retry`,
`test-provider-retry-telemetry`, `test-provider-retry-ceiling-config`);
**fast gate**;
arch suite (new export).
**Exit criterion:** openai-compatible phase-bound matrix green; fast gate green with evidence; SS-6 deferral recorded.

### W2 — Diagnosable timeouts + documentation (SS-5)

**Objective:** make every stall immediately triageable from the transcript, and pin the
semantics in docs.

**Tasks**
1. `llm/stream.rkt`: extend the three `exn:fail:network:timeout:stream` raise sites
   (total-duration, consecutive-empty, per-chunk read) with the stable suffix
   `[phase=… data-received=… chars=…]` built via `string-append` (keep every literal
   <150 chars). The struct fields remain the machine-readable source of truth; the suffix
   is for humans and the `auto-retry.start` error string (which flows to the TUI retry
   line unchanged).
2. `docs/provider-retry.md`: add the "Streaming Timeout Matrix" section (both tables
   from §2 — the openai-compatible before/after matrix **and** the per-adapter matrix
   with the connect+headers/TTFB bound, AUDIT PA-3), state that
   `timeouts.models.<m>.sse-read` currently controls (a) the anthropic/kimi eager
   whole-body read and (b) the openai-compatible thinking-gap window capped at 300 s —
   and nothing else on v1.00.12; anthropic/azure/gemini streaming remain on their
   documented hardcoded defaults until v1.00.13. Note the watchdog is display-only.
   Add an **Architectural handoff** note: `sse-read` is semantically overloaded legacy
   configuration and v1.00.13 will introduce explicit request-network-policy fields
   while preserving compatibility.
3. `CHANGELOG.md`: v1.00.12 entry (`Fixed` — 10-minute SSE stall regression;
   `Added` — phase/liveness in timeout messages; thinking-gap ceiling).
4. Config guidance (docs only): `deepseek-v4-flash.sse-read: 600` now widens nothing
   beyond the 300 s thinking ceiling; operators may drop it to 300 — optional, not
   required by this plan.

**Files:** `llm/stream.rkt`, `docs/provider-retry.md`, `CHANGELOG.md`, README metrics sync.
**Gate:** W0 message tests green; focused stream suites; **fast gate**; **broad gate**;
`racket scripts/lint-all.rkt` (or repo equivalent) — 0 failures.
**Exit criterion:** broad gate green with evidence; docs and CHANGELOG merged.

### W3 — Integration verification + release

**Objective:** ship v1.00.12.

**Tasks**
1. Version bump: `util/version.rkt` + `info.rkt` from the current released version
   (expected 1.00.11 → 1.00.12; adjust if v1.00.11 slipped). Docs version-sync
   (`racket scripts/sync-version.rkt --all --write`), README status sync, lint-version.
2. Local gates with recorded evidence
   (`--record-gate-evidence`): fast, tui, arch, workflows; security suite; lint-all;
   `lint-release-readiness` (only "uncommitted changes" may remain pre-commit).
   **Do not** run the local full-regression suite — it runs on GitHub in the release
   workflow (established v1.00.07+ practice).
3. Release PR → CI green → squash-merge → **annotated** tag `v1.00.12` → release
   workflow. If `release-core / publish` waits on the `release-repair` environment:
   approve via GraphQL `approveDeployments(input:{workflowRunId, environmentIds, comment})`
   using **global node IDs** (`GET …/actions/runs/<id> --jq .node_id`;
   `GET …/environments/release-repair --jq .node_id`) — a deployment-status POST does
   **not** satisfy `required_reviewers`.
4. Verify the GitHub Release (assets `q-1.00.12.tar.gz` + `release-manifest.json`);
   add the `Released YYYY-MM-DD.` marker to the CHANGELOG entry (strict
   lint-release-readiness) in a follow-up PR if required.
5. Write `STATE-v1.00.12-…` / `VALIDATION-v1.00.12-…`, mirror all three docs to
   `q/docs/planning/` via a docs PR.
6. Housekeeping: close stale PR **#9377** as superseded (comment linking the analysis
   doc and the W1 fix commit).

**Files:** `util/version.rkt`, `info.rkt`, `CHANGELOG.md`, `README.md`, docs sync,
planning mirrors.
**Gate:** all suites + release workflow `success` + published Release.
**Exit criterion:** §3 outcomes 7–8 verified.

---

## 5.1 Handoff Contract to v1.00.13

v1.00.12 deliberately leaves the deeper design problem visible rather than hiding it
behind more adapter-local wiring.

The following statements are **post-v1.00.12 invariants / v1.00.13 inputs**:

1. `stream-sse-events` is transport mechanism: it enforces already-resolved timeouts; it
   is not the long-term owner of model/config semantics.
2. `sse-phase-timeout-secs` is a tactical extraction that proves phase semantics can be
   centralized; v1.00.13 may move/replace it behind a request-policy module without
   changing the v1.00.12 behavioral contract.
3. Provider adapters must not gain additional direct interpretations of raw timeout
   config in v1.00.12.
4. `sse-read` remains backward-compatible in v1.00.12, but its overloaded meaning is
   explicitly technical debt. v1.00.13 owns migration to semantically named fields.
5. NP-2/NP-3/NP-4/NP-6 are not independent cleanup tickets: they are evidence that
   provider-request lifecycle ownership is distributed. v1.00.13 treats them as one
   architectural workstream.
6. The v1.00.13 acceptance bar is stronger than resolver unit tests: **all adapters must
   demonstrably consume one resolved policy and one structured lifecycle/failure
   contract.**

---

## 6. Risk & Rollback

| Risk | Likelihood | Mitigation |
|------|-----------|------------|
| Re-tightening reintroduces the "premature thinking timeout" that motivated the v0.99.83/#9393 widening | Medium | Thinking still honors `sse-read` up to 300 s (kimi/GLM configured 300 s preserved); `initial` is 120 s (not the old feared 60 s); `content` 60 s matches the pre-#9393 design that carried the live evidence |
| A streaming model legitimately pauses >60 s between content chunks | Low | No such model is known (measured gaps ≤7 s); `max-total-timeout` still bounds total duration; if evidence appears, a dedicated opt-in knob can be added deliberately |
| Message-format change breaks consumers parsing the error string | Low | Suffix appended, existing prefix (`HTTP read timeout (Ns) waiting for SSE chunk`) unchanged; the only two in-repo references (`tests/test-gsd-d8-provider-retry-scaling.rkt:116`, `tests/test-streaming-text-preservation.rkt:48`) construct the string as fixture data — they do not assert against the real raised message |
| Release-blocking lint churn (metrics/version-sync/changelog-dates) | Medium | Per-wave README sync; version-expectations tests updated via dynamic `q-version` import (established pattern); `Released` marker added at tag time |
| v1.00.11 lands concurrently with conflicting version bumps | Medium | W3 re-bases the bump on the then-current released version; CHANGELOG entry ordering fixed at merge time |
| Pre-headers (connect/TTFB) holds still stall up to the full `request` budget (900 s) — outside the ≤2 min goal (AUDIT NP-2/PA-3) | Documented limitation | Matrix row + docs; dedicated connect/TTFB timeout is assigned to v1.00.13 |
| Middlebox/NAT idle-drop blackholes an established connection; no TCP keepalive available in Racket (AUDIT NP-5) | Accepted risk | Application-level phase timeouts are the detector — exactly what this milestone restores; FFI keepalive recorded as future work |
| Anthropic/azure/gemini remain policy-divergent through v1.00.12 | Known temporary debt | Deliberate containment boundary; current matrix documented in W2; v1.00.13 W0/W2 adds red conformance tests then mandatory shared policy consumption |

**Rollback:** W1+W2 are two small commits on `llm/stream.rkt`,
`llm/openai-compatible.rkt`, and `docs/` — a targeted `git revert` restores the previous
behavior without touching config or retry machinery.

---

## 7. Verification Hooks (for VALIDATION)

- Unit: `raco test tests/test-sse-phase-timeout-bounds.rkt` — full openai-compatible phase matrix.
- Live-shape: timeout from a stalling mock port carries `[phase=… data-received=… chars=…]`.
- Post-release smoke (manual, DeepSeek): a stalled turn recovers within ≤2 min (held) /
  ≤1 min (content); the `[SYS] [retry: LLM timeout, 1/5…]` line now shows the phase
  suffix; no more than one watchdog warning per stall window.
- Architecture handoff: docs/state explicitly record that adapter parity and lifecycle-policy
  ownership are v1.00.13 work, not silently "fixed" by v1.00.12.

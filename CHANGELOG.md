## v1.00.17 — 2026-08-25

Released 2026-08-25.

> v1.00.17: the /go executor-hardening campaign (#9512–#9516), BUG-0022
> (#9517) remediation, the W8 integration bake, and the v1.00.17 release.

### Features

- **#9512 — per-wave worktree isolation.** `/go` wave executors run in
  dedicated git worktrees (`gsd.worktree-isolation`, default **ON** since the
  W8 bake; `#:isolate? #f` disables for tests), so concurrent waves can no
  longer read or clobber each other's uncommitted trees.
- **#9515 — auto-retry with failure context.** A failed wave delivery attempt
  no longer silently ends the campaign: the executor injects the recorded
  failure reason into the retry prompt ("Previous attempt failed — adapt").
- **#9513 — mutation-stall steering.** Repeated identical tool calls without
  intervening text now trigger steering that forces a concrete implementation
  step instead of an infinite exploration loop.
- **#9514 — role re-anchor after empty response.** An empty/whitespace-only
  model response re-anchors the executor role instead of continuing with a
  decontextualized agent.
- **#9516 — shell-risk false-positive severity.** The shell-risk classifier no
  longer aborts benign multi-command lines; findings are severity-graded.
- **#9518-lesson — branch-based delivery verification (W5).** Wave DONE now
  requires evidence against the wave branch's *pushed* head SHA, never a local
  claim — making "branch merged before its final commit existed upstream"
  unrepresentable.
- **BUG-0022 (#9517) remediation (W1B/W2B).** Connection-pool stale keep-alive
  reuse now transparently retries; the health gate no longer counts same-turn
  retries against the budget (5 → silent truncation to 2 fixed).

### Bug Fixes

- **`release-dry-run.rkt` cwd fragility (W8).** The script resolved
  `util/version.rkt` relative to the caller's cwd, so wave verify commands
  that run it from the campaign base-dir (the parent of `q/`) failed with
  "Run from q/ project root". It now resolves its project root from its own
  file location (`scripts/` always sits directly under `q/`) and runs all
  checks with `cwd = q/`; exit-code semantics unchanged (6/6 checks).

### Reports

- `docs/reports/GSD-EXECUTOR-HARDENING-BAKE-v1.00.17.md` — W8 live-bake
  evidence: dogfooded worktree isolation, branch-based verification, and the
  #9515 failure-context retry on a synthetic no-op first attempt.

## v1.00.16 — 2026-08-24

Released 2026-08-24.

> BUG-0020/BUG-0021 remediations, the v1.00.16 fast-gate
> and TDD-adoption campaign (W0–W4), and the connection-pool chunked-body
> fix that made pooling safe to re-enable everywhere.

### Features

- **Prepared-env fast-gate cutover (W3, #9518).** The reusable
  `.github/actions/setup-racket` action gained a prepared-environment path:
  exact-cache restore with package preflight, relink fallback, and guarded
  install. `ci.yml` routes lanes through it behind `RACKET_PREPARED_ARTIFACT`.
- **`FAST_SHARD_COUNT` guarded study (W3).** Shard-count override wired through
  ci.yml matrix generation with cache-policy documentation in
  `docs/reports/CI-RACKET-CACHE-POLICY.md`; decision recorded: KEEP-3.
- **v1.00.16 W1+W2 banked work (#9511).** `grouped-eligible?` runner contract;
  oauth `#:on-complete` seam with deterministic semaphore sync
  (`test-oauth-callback-nonblocking` 8.23 s → 1.40 s); shared fixture builders
  `tests/helpers/{fast,oauth-callback}-fixtures.rkt`;
  `current-auto-retry-sleep-scale` parameter; `--json-out` crash fix in the
  runner/reporting path (string result paths → `path->string`); timesink
  remediation report `docs/reports/fast-timesink-remediation-v1.00.16.md`.
- **Halving-objective baseline of record (W4, #9519).** Regenerable
  `docs/reports/test-feedback-baseline-v1.00.16.{md,json}` plus
  `fast-gate-budget-v1.00.16.{md,json}` attribution companion, generated from
  retained CI runs 32745843124/32748197712; `baseline-report.rkt --check`
  proves byte-identical regeneration. Honest result recorded: sample p50
  627 s vs target ≤ 244 s (ratio 1.2848×) — MISSED; remaining cost attributed
  to legacy setup install path (343/348 s) + max shard (276/287 s).

### Bug Fixes

- **BUG-0020 — `/go` executor-inheritance contract violation (#9509).**
  `executor-inheritance.rkt` widened to accept the full session-config struct;
  wave executors no longer die with a contract error at spawn.
- **BUG-0021 — pooled chunked-body corruption (#9510).** Pooled connections did
  not decode `Transfer-Encoding: chunked`, so raw hex chunk sizes were spliced
  into SSE `data:` lines at TCP chunk boundaries — surfacing as malformed
  tool-call JSON ("model typos"). New RFC 7230 decoder `make-chunked-input-port`
  in `llm/conn-pool.rkt`: byte-exact reassembly across mid-line splits,
  0-chunk + trailers ⇒ connection stays reusable, framing anomalies ⇒ pool
  fault. Regression-tested against a mock 7-byte-chunk server.

### Breaking / Behavior Changes

- Connection pooling is now ENABLED in local + VPS configs
  (`networking.pool.enabled=true`) after the BUG-0021 fix; pooled responses are
  chunk-decoded transparently. Disable via `networking.pool.enabled=false` to
  return to one-connection-per-request behavior.

### Migration Notes

- No API changes. Operators self-hosting with custom provider configs should
  verify their endpoints tolerate HTTP keep-alive reuse before enabling the
  pool; hosts that close idle connections aggressively may surface first-shot
  network errors until host-specific idle TTLs are tuned.

### Testing

- New regression tests: `test-executor-inheritance.rkt` (BUG-0020);
  chunked-body decoder coverage incl. mock 7-byte-chunk server (BUG-0021);
  oauth-callback nonblocking/security suites re-seamed onto deterministic
  fixtures. Long-generation live bake against GLM-5.3/GLM-5.2/DeepSeek-V4-flash
  verified clean before re-enabling the pool. Local gate evidence (fast, tui,
  arch, workflows) recorded at this version prior to tagging.

### Operational / Release

- Rollback toggles (one line each, documented in
  `docs/reports/test-regression-log.md`): `RACKET_PREPARED_ARTIFACT=off`
  (prepared-env cutover), unset `FAST_SHARD_PLAN` (shard matrix),
  `networking.pool.enabled=false` (pooling).
- Halving-objective remeasure (warm prepared-env restore observation)
  scheduled 2026-09-30.

## v1.00.15 — 2026-08-24

> BUG-0019 remediation: peer FIN/CLOSE-WAIT mid-stream is now detected in
> seconds instead of burning the whole phase timeout, plus an opt-in
> connection pool for openai-compatible providers.

### Added

- **FIN-aware SSE liveness watchdog (W1).** `stream-sse-events` slices idle
  windows into `peer-close-probe-secs` slices (default 5 s, per-model via
  `request.peer-close-probe-secs`) with zero-timeout liveness probes. An
  unclean peer close raises the new structured exception
  `exn:fail:network:peer-closed` carrying `phase`/`data-received?`/
  `content-chars`/`elapsed-ms` plus the SS-5-style message suffix — detection
  latency drops from minutes (full thinking window) to under a second,
  independent of `thinking-gap-cap`. EOF stays normal end-of-stream;
  heartbeat/data bytes keep resetting the idle clock (BUG-0018 rule).
  Auto-retry classifies peer-closed as timeout-tier; v1.00.14
  silent-overflow economics are unchanged.
- **Flag-off connection pooling (W2).** New `llm/conn-pool.rkt`: host-keyed
  `(host, port, tls?)` pool with per-entry custodians, 55 s idle TTL,
  max-per-host 4, single-use-on-fault, and deterministic-framing reuse
  (Content-Length responses check in; chunked/EOF bodies stay single-use).
  Gated by `networking.pool.{enabled,idle-ttl-secs,max-per-host}`, default
  OFF — flag-off behavior is unchanged. Pooled requests skip the
  request-scoped custodian so teardown cannot kill pooled sockets.
  Bake-verified against GLM-5.3/GLM-5.2/DeepSeek-V4-flash; default stays OFF
  until chunked-body reuse lands (SSE responses currently do not reuse).
- **Reproducer suite (W0).** `tests/reproducers/mock-fin-server.rkt` models
  unclean-close / clean-close / heartbeat-alive / true-silence peers;
  recorded platform verdict: on Racket 8.10/OpenSSL 3 an unclean FIN always
  surfaces as `exn:fail:network` (never plain EOF), and even graceful TLS
  closes reach the client as errors — clean end-of-stream relies on the SSE
  `[DONE]` marker, as providers signal it.

## v1.00.14 — 2026-08-23

> BUG-0018 remediation: GLM-5.3 long-thinking sessions no longer die at the
> 300 s thinking-idle cap, and `/model <name>` provably reaches the request
> path on every execution path with a guaranteed `model.switched` trace.

### Added

- **Configurable thinking-gap ceiling (W1).** New per-model config key
  `timeouts.models.<model>.thinking-gap-cap` widens the SSE thinking-idle
  window past the legacy 300 s bound (widen-only precedence — a cap can
  never narrow the resolved window below the legacy bound). Ops-level
  parameter `current-max-thinking-gap-secs` (default 300) preserves
  v1.00.12/v1.00.13 semantics exactly when unset.
- **Keepalive liveness documentation + tests (W1).** The phase timeouts are
  per-read windows: heartbeat comment frames and zero-delta data chunks each
  reset the idle clock; only true silence or the total budget raises
  (`tests/test-midstream-stall.rkt`).
- **Silent-thinking overflow economics (W3).** A stream timeout in the
  thinking phase with zero visible chars gets exactly ONE retry; the second
  consecutive overflow circuit-breaks with actionable guidance ("raise
  thinking-gap-cap or /model switch") instead of burning blind restarts.
  Overflow retries back off proportionally to the consumed thinking window.
- **GSD executor model inheritance (W3, R-B3).** `/go`-spawned executor
  sessions inherit the coordinator's switched provider/model via
  `runtime/session/executor-inheritance.rkt`; without an explicit override,
  startup config semantics are unchanged.

### Fixed

- **BUG-0018 B: /model switch never reached the request path (W2).**
  Root cause R-B1: `build-session-context-for-prompt` (E4) re-applied the
  path-derived model name on EVERY prompt, silently reverting any runtime
  switch before the next request. `set-model!`/`switch-model!` now record an
  explicit `'model-override` marker and E4 defers to it; `dispatch-iteration`
  reconciles session/config divergence loudly (log + `model.divergence.reconciled`
  event).
- **R-B2 observability gap.** `handle-model-command` now refuses UI-only
  switches when no live session exists (error entry instead of fake success),
  publishes `model.switched` on every real switch, and falls back to the q
  logger when the event bus is nil. Transcript entries appear only after the
  session mutation actually succeeded.

### Testing

- `test-request-network-policy.rkt`: precedence rows for
  `thinking-gap-cap-override` (widen-only, request-budget clamped,
  initial/content unaffected).
- `test-sse-phase-timeout-bounds.rkt`: glm-style cap-900 rows; default
  matrix unchanged.
- `test-model-command.rkt`: live-session switch asserts session model-name,
  override marker, guaranteed `model.switched` event payload, and
  request-path config resolution.
- `test-auto-retry.rkt`, `test-executor-inheritance.rkt`,
  `test-provider-recovery-model-switch-e2e.rkt`.

## v1.00.13 — 2026-08-22

> Released 2026-08-22. Request lifecycle policy unification: one mandatory owner
> for provider-request lifecycle policy. Raw timeout configuration resolves once
> into semantically named policy fields consumed by every adapter; response
> headers, resource cleanup, and structured failures survive the request
> boundary; connect/TTFB is bounded; held-request classification is
> heartbeat-aware.

### Features

- **Centralized request-network policy (W1 #9461).** New
  `llm/request-policy.rkt` is the single owner of provider-request lifecycle
  semantics: the `request-network-policy` value (request budget, connect/TTFB,
  initial/thinking/content idle, stream total, body-read budget), the pure
  resolver with safety caps and early validation, and the legacy `sse-read`
  compatibility mapping. The v1.00.12 resolver moved out of `llm/stream.rkt`
  (thin compatibility re-export; stream is mechanism-only again).
- **Mandatory policy consumption across all adapters (W2 #9466).** openai-
  compatible, anthropic (+ kimi eager), azure-openai, and gemini consume one
  resolved policy per request on both streaming and eager paths; adapters no
  longer read raw timeout config or author generic constants. Completes the
  v1.00.12 SS-6 adapter-parity deferral: anthropic/azure/gemini thinking
  window 60 → policy value; stream total 600 → `max(600, 2×request)`; eager
  body reads honor the legacy `sse-read` budget instead of the flat 120 s
  fallback. Cross-adapter conformance harness proves identical mechanism
  arguments for all four adapters; the architecture gate (R1–R5) forbids
  adapters from regaining timeout-policy ownership.
- **Structured failures replace string parsing (W3 #9473).** HTTP status and
  retry-relevant headers survive the request boundary in a machine-readable
  failure context; auto-retry consumes `retry-after-ms` from that context and
  no longer parses exception message text. Human messages are rendered
  alongside, unchanged.

### Added

- **Semantic timeout config keys**: `timeouts.models.<m>.thinking-idle` and
  `timeouts.models.<m>.body-read` (explicit keys win over the legacy alias;
  thinking capped at 300 s). Non-fatal deprecation warning for legacy
  `sse-read` at wiring time.
- **Cross-adapter policy conformance suite** and **architecture ownership
  gate** (durable regression prevention, AC-1..AC-5).
- **Deterministic response-port lifecycle** for non-streaming/eager-body
  requests: close-once semantics across success, status failure, read
  timeout, request timeout, and cancellation (injectable HTTP boundary for
  tests).

### Fixed

- **Retry-After from real headers (RL-5).** `Retry-After` is parsed from the
  actual response header — delta-seconds and HTTP-date (timezone-free parser,
  injectable clock) — instead of being reconstructed from exception text
  (which never worked for real responses).
- **Connect/TTFB bound (RL-4).** An established-but-silent connection fires
  the dedicated `min(request, 120)` window with structured phase
  `'connect/ttfb` — it can no longer consume the full request budget
  (previously up to 900 s).
- **Heartbeat-aware held-request classification (RL-8).** Heartbeat-only
  streams are live-but-no-content, not dead peers; total deadline and
  empty/comment flood ceiling still bound them.
- **Hard remaining-budget reads (NP-7).** Every blocking stream read is
  capped at `min(phase-idle, remaining-total)` — no more overshooting the
  total deadline by a full phase window.

### Breaking / Behavior Changes

All deltas are intentional outcomes of the unification (pinned by the
cross-adapter conformance suite):

1. anthropic/azure/gemini streaming: thinking window 60 s →
   `min(request, min(or thinking-idle 120, 300))`; stream total 600 s →
   `max(600, 2×request)` when request > 300 s.
2. All adapters (incl. openai eager): non-streaming body reads honor the
   legacy `sse-read` (or explicit `body-read`) budget instead of the flat
   120 s fallback.
3. Connect+TTFB on every path bounded at `min(request, 120)` with
   structured phase `'connect/ttfb`.
4. Heartbeat-only initial stalls are live-but-no-content: they no longer
   trip the held-request circuit breaker; total deadline still bounds them.
5. Blocking stream reads capped at `min(phase-idle, remaining-total)`.
6. Retry delays derive from the structured `Retry-After` context
   (HTTP-date + delta-seconds), never from message text.

### Migration Notes

- Existing configs need no change: `request` + legacy `sse-read` resolve to
  the same effective windows as documented (DeepSeek `request=900`,
  `sse-read=600` → thinking 300 s; Kimi `sse-read=300` → honored).
- To widen a specific window, prefer the semantic keys
  `timeouts.models.<m>.thinking-idle` / `body-read`; explicit keys win over
  the legacy alias. `docs/provider-retry.md` carries the resolved-policy
  matrix and migration table.

### Testing

- New suites: `test-request-network-policy` (resolver contract + property
  sweep), `test-provider-network-policy-conformance` (identical mechanism
  arguments across all four adapters), `test-network-failure-context`
  (structured failure context + Retry-After parsing), 
  `test-provider-response-cleanup` (close-once lifecycle matrix),
  `test-request-policy-architecture` (R1–R5 ownership gate, empty
  allowlist), `test-stream-liveness-classification` (heartbeat matrix +
  W4 deadline matrix), `test-request-policy-migration` (DeepSeek/Kimi
  legacy-config proofs).
- All suites green in CI (fast/arch/workflows/tui + sharded regression);
  gate evidence recorded per release run.

### Deprecated

- **Legacy `sse-read` config key.** Still honored (feeds only thinking-idle
  and body-read, with the documented caps and precedence); removal is
  planned after v1.00.13. Docs: `docs/provider-retry.md`.

### Operational / Release

- CI cold-runner repair (with #9488): `raco pkg show`-based package-presence
  guard fixed in `setup-racket`/`prepare-racket-environment` actions; the
  metadata-discovery fixture tree is excluded from repo-root test collection.
- Workspace bytecode wipes spare the frozen discovery fixture
  (tracked stray `.rkt` under `compiled/`); the release readiness gate now
  names dirty files when it fails.

Released 2026-08-22.

## v1.00.12 — 2026-08-22

> Released 2026-08-22. SSE stall detection bounds: containment of the v1.00.05 regression that let
> a wide `sse-read` override stretch stream stalls to the full configured
> window (observed as ~10-minute hangs on deepseek-v4-flash). Phase windows
> are now bounded by design; timeout messages carry triage diagnostics.

### Bug Fixes

- **SS-1/SS-2/SS-3 bounded phase timeouts (W1, #9429).** New pure resolver
  `sse-phase-timeout-secs` in `llm/stream.rkt` returns the three stall windows:
  initial = `min(request-timeout, 120)` (dead-peer bound, never config-widened),
  thinking = `min(request-timeout, min(or sse-read 120, 300))` (reasoning
  window capped at new constant `max-thinking-gap-secs` = 300), content =
  fixed 60 s per-chunk gap. The openai-compatible adapter now wires all three
  through the resolver; the raw `sse-read` config feeds only the thinking
  window. kimi/glm 300 s reasoning windows are preserved while deepseek's
  `sse-read=600` can no longer produce multi-minute mid-content hangs.
- **SS-5 timeout message suffix (W2, #9430).** Every
  `exn:fail:network:timeout:stream` raised from `stream-sse-events` now ends
  with `[phase=<p> data-received=<yes|no> chars=<n>]` for log/UX triage. The
  struct fields remain the machine source of truth for retry classification.

### Documentation

- New "Streaming Timeout Matrix" section in `docs/provider-retry.md`: phase
  table, circuit-breaker interaction/TTFB row, and the v1.00.13 Request
  Lifecycle Policy Unification handoff note (adapter parity deferral, SS-6).

### Testing

- `tests/test-sse-phase-timeout-bounds.rkt` locks the resolver matrix (deepseek
  clamp, no-override defaults, kimi ceiling preservation, sweep invariants)
  and — since W2 — the message-suffix checks migrated from the deleted
  reproducer `tests/reproducers/reproduce-sse-timeout-message-suffix.rkt`.

### Breaking / Behavior Changes

- Models with `sse-read` overrides above 300 s now stall-cap at 300 s in the
  thinking phase instead of running to their full configured value; initial
  and content phases ignore `sse-read` entirely (fixed 120 s / 60 s).
- Timeout exception messages gained the diagnostic suffix (string change only;
  struct fields unchanged).

### Migration Notes

- None required. Existing `timeouts.models.<model>.sse-read` values continue
  to work; values above 300 are clamped for the thinking window.

### Operational / Release

- Containment release: no schema, config-format, or storage changes; safe to
  deploy rolling. Watch for `phase=thinking` stalls now capping at 300 s —
  models that legitimately need longer silent reasoning gaps require the
  v1.00.13 Request Lifecycle Policy Unification follow-up.

Released 2026-08-22.

## v1.00.08 — 2026-08-21

> Provider networking hardening closeout: per-model cumulative retry ceiling
> (`providers.<name>.retry-ceiling-secs`) documented and config-override
> tested through the turn-orchestrator settings path (PN-7).

### Bug Fixes

- **PN-7 cumulative retry ceiling config override.** `runtime/turn-orchestrator.rkt`
  now exposes `resolve-retry-ceiling-secs`, which reads
  `providers.<name>.retry-ceiling-secs` from session-config settings and falls
  back to the module default when absent. A dedicated test
  (`tests/test-provider-retry-ceiling-config.rkt`) proves the per-model value
  overrides the default, that another model's override does not leak, and that
  absent settings/model-name fall back to `default-cumulative-ceiling-secs`.
- **Documentation drift fix.** `docs/provider-retry.md` previously stated the
  default cumulative ceiling was 300s; the default was raised to 900s in
  v1.00.05. The `retry-ceiling-secs` examples and tables now match the actual
  default (900s / 15 min).

### Testing

- New focused test `tests/test-provider-retry-ceiling-config.rkt` (4 cases)
  covering the PN-7 settings-override resolution.
- Existing provider-networking contract tests remain green: stream port
  closure (PN-1), generator finalization (PN-3), SSE heartbeat metadata
  (PN-2b), circuit breaker (PN-4), adaptive retry (PN-6), cumulative ceiling
  (PN-7).

### Breaking / Behavior Changes

- None. `resolve-retry-ceiling-secs` is a pure extraction of the existing
  inline resolution; no retry behavior changes.

### Migration Notes

- None required. Existing `retry-ceiling-secs` config continues to work.

### Operational / Release

- Version stamped `1.00.08`; provider-networking hardening plan v1.00.08
  closed out.

Released 2026-08-21.
## v1.00.07 — 2026-08-20

> macOS platform test fixes (W2): SP12 dash/bash PIPESTATUS conditional + LF3
> symlink path-allowed? case-insensitive fix on APFS; fixes #9406 #9407.
> Merged via PR #9411.

### Bug Fixes

- **SP12 dash/bash PIPESTATUS conditional (#9406).** `tests/test-subprocess-edge-cases.rkt`
  SP12 test now probes `sh-is-dash?` at load time (mirroring `setsid-available?`).
  On dash (Linux `/bin/sh`): asserts exit-2 + "Bad substitution". On bash-as-sh
  (macOS `/bin/sh`): asserts exit-0 + PIPESTATUS[0]=1. Prevents false failure
  on macOS where `/bin/sh` is bash 3.2.
- **LF3 symlink path-allowed? on APFS (#9407).** `sandbox/worker-tools.rkt`
  `path-allowed?` normalizes both resolved path and allowed root to lowercase
  on macOS (`system-type` = `macosx`) for prefix comparison. Fixes symlink
  resolution on case-insensitive APFS filesystem where casing differences
  caused legitimate symlinks within allowed roots to be rejected.

### Testing

- Both test files pass on Linux: `tests/test-subprocess-edge-cases.rkt` (13 tests)
  and `tests/test-worker-security.rkt` (32 tests).
- All existing tests unaffected: auto-retry, stream, worker-security,
  subprocess-edge-cases.

### Breaking / Behavior Changes

- None. The SP12 test is now platform-conditional but documents the expected
  behavior on both platforms. The LF3 fix only changes macOS path comparison
  to be case-insensitive (matching APFS semantics).

### Migration Notes

- None required.

### Operational / Release

- Version stamped `1.00.07`; wave PR #9411 merged to main.

Released 2026-08-20.

## [Unreleased]

> v1.00.02 UX campaign (3a1d608a) 7/7 waves DONE — merged via PR #9370 (main `76ae7946`).
> TUI/GUI event-order parity (W5/W6), BUG-0015 TUI dedup fix, per-wave budget
> 3600 s + configurable, delivery verifier, edit-limit sandbox 2000.

### Bug Fixes

- **TUI tool-call dedup dropped consecutive same-name calls (BUG-0015).** The `/go` TUI
  transcript omitted `read` tool calls when several used the same tool name back-to-back:
  `recent-tool-start?`/`recent-tool-end?` in `tui/state-events/helpers.rkt` keyed dedup on
  tool **name** within a 10-entry window, so the 2nd+ same-name call was treated as a
  duplicate and never appended. Dedup now keys on tool-call identity (`tool-call-id` for
  starts, `result-key` for ends, name-only fallback for legacy payloads). Tests: +4 cases
  in `test-tool-dedup.rkt`, updated concurrent-tool contract in `test-streaming-transitions.rkt`.
- **Edit-limit sandbox default raised 500 → 2000.** The sandbox `execute-edit` path still used
  `DEFAULT-MAX-OLD-TEXT-LEN` 500 while the main-process GSD edit-limit was 2000, so valid
  large edits (e.g. 1039-char old-text) failed through the sandbox. Aligned `edit-contract.rkt`,
  `core-tools.rkt` registration text, `prompts.rkt` executor rule, and `editing-rules.md`.

### Features

- **Configurable per-wave campaign budget (default 3600 s).** `/go --wave-timeout=SECONDS`
  flag > `~/.q/config.json` `wave-timeout-seconds` > default 3600 (was 1800). The resolved
  value is carried on `campaign-request` (`timeout-sec`) so it applies even though the
  campaign runs in a separate thread. Retry ceiling stays capped at 900 s.
- **Delivery verifier for GSD waves.** `extensions/gsd/delivery-verifier.rkt` approves a wave
  when its listed files exist, are committed, and pass the wave's scoped verify command —
  unblocks `/go` wave approval that previously fail-closed to `#f`.

### Testing

- **Cross-frontend event-order matrix (W6).** `tests/ux-frontend-event-order-test.rkt` drives
  the production TUI registry reducers and `make-gui-event-subscriber` through a 7-scenario
  event-order matrix (normal, reversed terminal orders, interleaved thinking deltas,
  duplicate completions, thinking-only turn, cancellation and runtime error mid-thinking),
  asserting identical artifact bodies/ids/lifecycle across both frontends (38 tests).
- **Single disclosure toggle resolution path (W5).** Unified Ctrl+O fallback with
  `dispatch-keymap-action`; TUI visual Up/Down composer navigation (W4) refinements.
- TUI suite green: 88 files / 1,362 tests; full CI green on PR #9370.

## 1.00.06

> TDD plan reassessment closure: full-regression evidence-path repair (#9384),
> DEEP-9 semver assertion fix, macOS platform budget revision, metadata-lint
> enforcement decision (W0–W3); first clean full-regression run + governance
> closure (W4). Merged via PR #9400.

Released 2026-08-20.

### Bug Fixes

- **Full-regression evidence path repaired (#9384).** The `full-regression.yml`
  workflow dropped the six per-shard Linux records (`results-shard-0..5.json`)
  from run-summary inputs and `summarize` never published `run-summary.json`,
  so evidence for the L4 contract could not be recorded. `full-regression.yml`
  now globs and uploads `results-shard-*.json` as named workflow inputs; the
  `summarize` job always publishes `run-summary.json`; the macOS platform job
  runs the suite and uploads usable evidence within a revised timeout budget
  (setup-timeout raised so the platform job no longer dies during setup).
- **DEEP-9 stale semantic-version assertion fixed.** `tests/test-self-hosting-deep.rkt`
  asserted the literal version `1.00.05`, breaking on any bump; it now asserts
  a semver floor (`>= 1.0.0`) so the self-hosting gate stays green across
  releases. Confirmed green at 1.00.06.

### Testing

- **Metadata-lint enforcement decision enacted (W3).** `scripts/lint-all.rkt`
  metadata checks are now blocking in CI (removed from the non-blocking tier);
  a metadata-lint failure fails the `lint` job instead of warn-only.
- **First clean full-regression run (definitive `pass`).** All six Linux shard
  records present in run-summary inputs, `run-summary.json` published with
  `status: pass`, green `workflows-suite`, and a macOS platform job that
  executed the suite and uploaded usable evidence. Recorded in
  `docs/reports/test-regression-log.md`.

### Governance

- **Governance reconciliation.** #9384 closed citing remediation PRs and the
  clean run; #9348 closed as superseded by the canonical
  `docs/TDD-TEST-STRATEGY-PLAN.md` on main (v1.00.06). Plan governance figures
  now match the generated manifest (95 entries / 92 modules).

### Breaking / Behavior Changes

- None. The metadata-lint enforcement (W3) makes previously warn-only CI
  metadata checks blocking, which is a deliberate tightening of the release
  gate rather than a runtime behavior change.

### Migration Notes

- None required.

### Operational / Release

- Version stamped `1.00.06`; wave PR #9400 merged to main.

## 1.00.05

Released 2026-08-20.

### Bug Fixes

- **kimi-for-coding W0/W1/W2 integration.** Fixed three critical issues
  discovered during the v1.00.05 campaign: (1) thinking surfacing in W0 — the
  anthropic adapter now streams `reasoning_content` deltas to the transcript;
  (2) W1 timeout fix — `kimi-for-coding` model now has `request: 900`,
  `sse-read: 300` in `~/.q/config.json`; (3) W2 retry=5 for campaign — the
  `execute-campaign-request!` function now scales retries to 5 for campaign
  sessions only. PRs #9392, #9393, #9394 merged; verified in release workflow.

### Testing

- **kimi-provider smoke tests.** `tests/test-provider-smoke.rkt` validates
  streaming, tool calls, and reasoning_content for kimi-for-coding.
- **Retry scaling verification.** `test-retry-iteration.rkt` confirms campaign
  retry ceiling (5) vs interactive default (2).
- **Version expectations hardened.** `check-version-expectations.rkt` now
  catches hardcoded version literals in test comments (BUG-0009); all 10
  affected test files updated.

### Features

- **kimi-for-coding provider.** New `kimi-coding` provider in `provider.rkt`
  with base URL `https://api.kimi.com/coding`, model `kimi-for-coding`.

### Breaking / Behavior Changes

- **Provider config format changed.** `timeouts.models.kimi-for-coding` object
  required in `~/.q/config.json` for timeout overrides.

### Migration Notes

- Update `~/.q/config.json` with `kimi-for-coding` timeouts.

### Operational / Release

- Version stamped `1.00.05`; PR #9398 merged; tag `v1.00.05` (`f6d6a8a3`);
  GitHub Release published (assets: `q-1.00.05.tar.gz` 4383639 bytes +
  `release-manifest.json`). Milestone #884 closed (3/3 issues).
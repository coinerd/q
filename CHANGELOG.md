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
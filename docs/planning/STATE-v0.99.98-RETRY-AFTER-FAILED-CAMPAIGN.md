# STATE v0.99.98 — /retry after failed /go campaign

Status: DONE (released)
Date: 2026-08-13
Branch: `fix/v09998-retry-after-campaign-failure` → PR #9307 → merge `f6f57a98`
Tag: `v0.99.98` (annotated)
Release workflow: #31745888719 ALL PASS (env approved via `pending_deployments` `environment_ids:[18980944508]`)
Assets: `q-0.99.98.tar.gz` (4093706 B) + `release-manifest.json` published to GitHub Release

## Problem

Live transcript (session `q 681FAHPB | ark-code-latest ctx:11%`):

```
[SYS] [retry: LLM timeout, 1/2...]
[SYS] [circuit-breaker: provider held without responding; stopping auto-retry. Type /retry to resubmit.]
[SYS] [ERROR] /go campaign stopped: wave-failed
[ERR] No previous prompt to retry.
 q   681FAHPB | ark-code-latest ctx:11%
```

After a `/go` campaign fails (circuit-breaker stops auto-retry on provider
timeout), typing `/retry` returned `[ERR] No previous prompt to retry.`
instead of resubmitting the failed prompt — even though the circuit-breaker
explicitly told the user to type `/retry`.

## Root Cause

`/go` campaigns run wave prompts on a **dedicated campaign session** created by
`make-campaign-runner()` (`tui/tui-init.rkt`). When the campaign fails,
`execute-campaign-command` (`tui/commands.rkt`) restores the **pre-campaign
session** via `restore-pre-campaign-session!`.

`/retry` (`tui/commands/runtime-control.rkt` `handle-retry-command`) finds its
prompt from:

1. `cmd-ctx-last-prompt-box` — only set by **regular text submits**
   (`tui/message-dispatch.rkt`). Slash commands never set it, and `/go` is a
   slash command.
2. restored session config `last-user-prompt` — the wave prompt only ever
   lived in the **campaign session's** config (set inside `run-prompt!` in
   `runtime/session/session-lifecycle.rkt`). The campaign session is discarded
   on restore.

Both were empty after a failed campaign → "No previous prompt to retry."

## Fix

`make-campaign-runner` now records each wave prompt into the shared
`tui-ctx-last-prompt-box` before running it. That box is the **same box
object** as `cmd-ctx-last-prompt-box` (via `tui-ctx->cmd-ctx` in
`tui/tui-keybindings.rkt`), so `/retry` after a campaign failure resubmits the
failed wave prompt on the restored session.

## Files

- `tui/tui-init.rkt` — campaign runner records wave prompt into shared
  last-prompt box (v0.99.98)
- `tests/test-process-extension-command.rkt` — new regression: `/retry`
  resubmits wave prompt after failed campaign; no "No previous prompt to
  retry."
- `README.md`, `docs/*`, `util/version.rkt`, `info.rkt`, `CHANGELOG.md` —
  version sync to v0.99.98 + metrics recomputed on clean committed state

## Verification

- Focused: 233 tests passed (campaign, TUI command, session-runner,
  lifecycle, model-switch, go-orchestrator)
- Fast gate: 1085/1087 files, 15714/15714 tests passed; 2 pre-existing
  untracked WIP test files fail (ENVIRONMENT_MISSING, not in CI)
- PR #9307 CI: 17/17 required checks PASS
- Release workflow #31745888719: ALL PASS; assets published and verified

## Notes

- Working tree carries pre-existing untracked WIP (`ui-core/*`, two broken
  test files) plus a WIP modification to `tui/state-types.rkt` that adds a
  `disclosure` field (W2 feature). These are NOT part of v0.99.98 and were
  excluded from the release commit.
- Local metrics must be synced on a clean tracked tree (revert WIP
  `tui/state-types.rkt` → `metrics.rkt --sync-all` → restore) to match CI
  (139342 source lines).

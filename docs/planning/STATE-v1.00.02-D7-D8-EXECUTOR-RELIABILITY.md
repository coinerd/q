# STATE: D7 + D8 executor reliability (campaign 81f9be4b W3) — analysis

- **Plan:** `PLAN-v1.00.02-D7-D8-EXECUTOR-RELIABILITY.md` (this directory)
- **Status (2026-08-16 23:40 CEST):** ANALYSIS COMPLETE — waves proposed, awaiting authorization
- **Trigger:** campaign 81f9be4b W3 failed 5 attempts (attempt-4: provider stall → wave-failed;
  attempt-5: planning-read contract violation + turn error); campaign churns attempts on `/go`

## Defect register (this audit)

| ID | Defect | Severity | Status |
|---|---|---|---|
| D7 | `planning-read`/`planning-write` call `ctx-cwd` (= `extension-ctx-working-directory`, contract `extension-ctx?`) on an `exec-context` → instant contract violation; 100% broken in executors; agent loops | High | Root-caused; fix proposed (W0) |
| D8 | Provider retry policy (max-retries 2, stall-breaker 2, 120 s SSE read, 300 s ceiling) is interactive-tuned; one transient stall converts a 30-tool-progress wave to `wave-failed`; no infra-vs-code distinction; `meta-fix-predicate` inert | High | Root-caused; fix proposed (W1) |
| S2a | Lease lock not truncated before rewrite (stale tail) + `owner ""` on re-dispatch path | Low | Root-caused; fix proposed (W2) |
| S2b | D5 cwd duality recurred (attempt-5 read `q/tui/key-dispatch.rkt`; plan says `q/tui/keybindings/key-dispatch.rkt`) | Medium | Root-caused; fix proposed (W3) |

## Evidence (read-only)

- Sessions: `01M0645MDDKM5HJR24HS8XW2BB` (attempt-4: 30/30 tools, then SSE read timeout ×2 →
  circuit-break progressive-stall → error), `01M0677YJNHTX2P71G1AE7BAJT` (attempt-5:
  planning-read contract violation at 21:22:05Z, exploration-loop ×2, `turn.completed error`
  21:30:13Z)
- Campaign record: W3 `failed` attempt count 5 (fence 11, last ts 1786915322)
- Lock: `#hasheq((acquired . 1786915322) (owner . "") (pid . 1194981))VGGEKK0") (pid . 1194981))`
  (non-truncated stale tail + empty owner)

## Wave status

| Wave | Status | Notes |
|---|---|---|
| W0 (D7 fix) | MERGED — 32f68c6d | get-base-dir context-type dispatch; tests 4/4 |
| W1 (D8 retry/meta-fix) | MERGED — dd43c2f7 | campaign-aware retry scaling + infra-meta-fix (both A+B); tests 8/8 |
| W2 (lock hygiene) | PR #9362 READY | file-truncate before rewrite + empty-owner sanitize; CI clean; rebase pending |
| W3 (D5 cwd) | PR #9363 READY | working-directory contract + File-target validation; CI clean; rebase pending |
| UX fixes (user findings) | MERGED — a0850bf3 | wave iteration hard-stop removed (2000), thinking display restored, [TOOL] path shown |
| Edit whitespace fallback + D3 200 | MERGED — 99da0adf | leading-ws auto-fallback in edit tool + consecutive-tool ceiling 100→200 |
| Iter-budget re-cap + edit-limit 2000 | MERGED — de92fffc | tui-init wave-budget default re-cap fixed; all GSD edit-limit sites 500/1200→2000 |

## Live-session findings (2026-08-17)

User ran `/go` again after the D7/D8 merges and reported three UX defects that
were fixed in PR #9365 (issue #9364):

1. **Wave cancelled at iteration 80** — `current-gsd-wave-max-iterations` default
   50 → derived hard limit only 80. Raised to 2000 (hard scales to 3200+); the
   1800 s timeout and 100-consecutive-tool breaker remain the real bounds.
2. **Thinking tokens not displayed** — `handle-session-started`/`resumed` read
   payload `'sessionId` but real events carry the sid in the envelope
   (`event-session-id`) and payload `'session-id`; the handler reset
   `ui-state-session-id` to `""` which dropped every `model.stream.thinking`
   delta via the `event-can-activate-stream?` gate.
3. **[TOOL] read/edit showed no filename** — `extract-arg-summary` fell through
   to the first-value branch (`read: 156`, `edit: <old-text>`); now prefers
   `path`/`file` keys.

## Log

- 2026-08-16 23:40 — analysis written; plan + state created; open questions to user
  (authorize W0?; D8 approach A/B/both?; campaign pause vs continue?).
- 2026-08-17 00:00 — user authorized all waves; campaign 81f9be4b terminated
  (cancellation set, fence preserved); issues #9355–#9359 created.
- 2026-08-17 00:40 — W0 PR #9360 (D7), W1 PR #9361 (D8 A+B), W2 PR #9362 (S2a),
  W3 PR #9363 (S2b/D5) pushed; CI running (README metrics drift fixed on all
  branches); merges pending CI.
- 2026-08-17 00:55 — W0 (#9360) and W1 (#9361) merged; campaign un-terminated at
  user's request (they were re-testing /go).
- 2026-08-17 ~02:19 — W3 attempt-6 hit the 80-iteration hard limit and was
  cancelled; user reported the three UX defects above.
- 2026-08-17 09:19 — fixes implemented (PR #9365, issue #9364): wave budget
  2000, session.started envelope-sid fallback, arg-summary path preference.
  Tests: test-tui-session-resume 5/5, test-state-types 27/27, test-gsd-policy
  green, test-gsd-go-orchestrator 34/34.
- 2026-08-17 10:00 — PR #9365 merged (a0850bf3); issue #9364 closed. The user's
  live W3 wave resumed on branch feature/issue-9342-wave with its WIP intact;
  W2 (#9362) / W3 (#9363) PRs still pending rebase onto the new main.
- 2026-08-17 09:52 — live W3 wave-failed (session 01M07A6YCFN12E8E0FYJFHT9FT):
  root cause was NOT a hang but the consecutive-tool circuit breaker. The
  executor edited tests/test-preferences-loader.rkt with old-text whose leading
  whitespace was +1 space per level (9/11/19 vs file 8/10/18); exact-match edit
  failed 6x; the model recovered with more tool calls (read/grep/edit) and the
  D3 breaker fired at 100 consecutive tool-only turns (~19 min / 101 iters) →
  wave-failed.
- 2026-08-17 10:30 — fixes implemented (PR #9367, issue #9366) and merged
  (99da0adf): (1) edit-tool leading-whitespace auto-fallback
  (edit-normalize strip-leading? + leading-ws-find-matches threshold 1.0,
  edit-contract apply, edit.rkt Note) so indentation-drift edits succeed
  instead of failing; (2) D3 ceiling 100 → 200
  (current-gsd-max-consecutive-tool-calls) so productive recovery loops are
  not policy-killed. Tests: edit-normalize, edit-matching-correctness,
  edit-execution-parity, gsd-policy. Smoke (308) + extensions (1511) green.
- 2026-08-17 11:45 — root-cause of the STILL-active 80 hard stop in the user's
  fresh transcript: the running TUI (pid 1363419, restarted 10:24) loads the
  live tree checked out on feature/issue-9342-wave @ dd43c2f7 (PRE-FIX). Main
  has all fixes but the checked-out branch never received them. Additionally
  two latent bugs on main were found and fixed (PR #9368, issue #9368, merged
  de92fffc): (a) tui-init make-campaign-runner defaulted the wave budget to
  (dict-ref rt-config 'max-iterations 50) — absent config key → budget 50 →
  hard stop 80 again; now defaults to (current-gsd-wave-max-iterations);
  (b) GSD edit-limit 500/1200 → 2000 everywhere (box default,
  reset-all-gsd-state!, launch-wave-executor, handle-plan-submit) so
  whole-form edits up to the tool's SAFE-MAX-OLD-TEXT-LEN (2000) succeed
  (the user's 1376-char edit failed at 'max 500'). Tests updated across
  gsd-planning-edit-limit/state/context-factory/integration +
  edit-balance-guard. Smoke (308) + extensions (1511) green.
- 2026-08-17 11:45 — deployment gap identified: the user's TUI must restart
  from a main-based checkout to load a0850bf3 + 99da0adf + de92fffc. The live
  tree is frozen (W3 executor WIP on feature/issue-9342-wave); recommend a
  main worktree or merging main into the feature branch after W3 lands.

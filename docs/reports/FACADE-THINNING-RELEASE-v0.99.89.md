# GSD Facade Thinning + Release — v0.99.89 W4 (#9230, milestone #876)

**Status:** DONE (PR pending)
**Branch:** `feature/v09989-w4-gsd-facade-thinning-release`
**Base:** `927c8024` (W3 merge, PR #9257)
**Gate:** Broad + Arch + Workflow + Smoke + Release + independent review

## Goal (roadmap + issue, immutable)

"Reduce GSD facades to composition/re-export without public API breakage;
publish v0.99.89." Acceptance: **pure domains have no I/O imports and
Golden Traces remain equivalent.**

## 1. Facade-Compatibility-Test + dynamic-require probe (Kontrolle)

NEW `tests/test-gsd-facade-compat.rkt` (7 tests + 1 module+ check,
`@suite arch`):

- **dynamic-require loader-convention probe**: `(dynamic-require
  "extensions/gsd-planning.rkt" 'the-extension)` + `'gsd-planning-extension`
  (mirrors extensions/loader.rkt, which resolves extensions via
  `(dynamic-require mod-path 'the-extension)`).
- **Export-surface pins**: the full public export list of
  `gsd-planning.rkt` (39 names) and `gsd/core.rkt` (24 names) — each name
  must dynamic-require without error, so any future thinning must
  consciously update the pin.
- **Legacy wrapper smoke**: `gsd-mode`, `set-gsd-mode!`, `pinned-planning-dir`,
  `total-waves` run inside a fresh parameterized ctx without crash.
- **Pure-domain I/O-free sweep (Acceptance machine-check)**: every
  responsibility-inventory module with domain ∈ (pure-planning
  transition-logic event-projection command-parsing) and no declared
  effects must import NO I/O module (file/port/path/date/system/
  runtime-path/openssl/net/tcp/process/subprocess/sandbox). Covers 14
  modules (shared, wave-status, command-parser, plan-types-parser,
  plan-types, plan-validator, context-bundle, prompts, policy,
  transition-kernel, projection-kernel, transition-logic, wave-executor,
  event-structs).
- **Pure-kernel whitelists**: transition-kernel ⊆ (base match set);
  projection-kernel ⊆ (base string); command-parser ⊆ (base match string
  command-helpers command-types).

## 2. Facade thinning (consumer grep — 2026-08-11)

`gsd/core.rkt`: **all 24 exports have production consumers**
(gsd-command-dispatch/cmd-* → command-handlers.rkt; reset-all-gsd-state! →
command-handlers + gsd-planning + interfaces/sdk-compat; gsd-show-status /
with-gsd-transaction → gsd-planning + command-handlers; gsd-write-guard →
tool-handlers; gsd-commands → command-handlers). No dead wrappers.

`gsd-planning.rkt`: every exported legacy wrapper (DEBT-01) has a
production/SDK/test consumer, so **none were removed** (grep evidence is
documented in the source comment block):
- set-gsd-mode! / set-total-waves! → scripts/sdk-gsd-integration-test.rkt
- set-current-max-old-text-len! → tools/builtins/edit.rkt (rename import)
- mark-wave-complete! → tests/test-sdk-gsd-live.rkt
- emit-gsd-event! → tests/test-gsd-planning.rkt (bus-bridge publish/no-op)
- pinned-planning-dir / set-pinned-planning-dir! / set-gsd-event-bus! →
  internal register-gsd-tools
- current-wave-index / set-current-wave-index! → tests

**Removed:** the dead internal `gsd-snapshot` define (never provided,
never used — verified by dynamic-require probe + grep). The facade was
already composition: every wrapper is a one-line delegation; the extension
definition composes hooks.

**DEFERRED (documented for v0.99.90):** the 3 duplicated local
`gsd-mode`/`gsd-mode?`/`set-gsd-mode!` definitions in command-handlers.rkt
and tool-handlers.rkt cannot import the facade (import cycle: gsd-planning
requires command-handlers). Needs a shared home module.

## 3. Hotspot / co-change re-measure (post-thinning)

- Hotspot top-10 (score = change-freq × LOC): runtime/agent-session.rkt
  62328, tui/tui-render-loop.rkt 41085, tui/commands.rkt 35154,
  state-aware-builder.rkt 28140, tool-coordinator.rkt 27846,
  turn-orchestrator.rkt 27648, llm/openai-compatible.rkt 26676,
  llm/gemini.rkt 23091, tui/tui-init.rkt 20072, **extensions/gsd-planning.rkt
  19199** (263 LOC, 73 changes). The GSD facade remains a high-change hotspot
  by nature (stable public surface, many consumers) — the W1–W4 kernel
  extraction reduced its implementation surface; the compat test now guards
  its API.
- Co-change (last 200 commits, ≥3): strongest pair info.rkt ↔
  util/version.rkt (29); GSD modules no longer appear in the top pairs
  (W1–W4 extraction removed GSD from the tightest co-change clusters).

## 4. Release v0.99.89

- Canonical version bump: `util/version.rkt` 0.99.88 → 0.99.89;
  `sync-version.rkt --write --all` propagated to info.rkt, README badge +
  snippets, and 23 doc surfaces (historical refs preserved; CHANGELOG
  corruption guard ≥50 headers PASSED).
- CHANGELOG 0.99.89 entry: features (golden oracle, transition kernel,
  projection kernel + atomic shell, intent boundary, facade compat) + bug
  fix (crash-between-commit-and-projection reconciliation) + no breaking
  changes.

## Gates (evidence)

| Gate | Result |
|---|---|
| Focused batch (facade-compat + planning + core-boundary + inventory + golden + intent + kernels) | ✅ 234 tests |
| lint-format | ✅ 2090 files 0/0 |
| Fast suite | ✅ 1061 files / 15487 tests |
| Broad suite | ✅ 1238/1239 (1 env-sensitive: test-pre-commit fails ONLY when .rkt files are staged locally; passes 7/7 with clean index — CI runs fresh) |
| Arch suite | ✅ 22 files / 238 tests |
| Workflow suite | ✅ 29 files / 162 tests |
| Release dry-run | ✅ 5/5 |
| lint-version / lint-version-io | ✅ 0 errors |
| Golden traces | ✅ 16/16 UNCHANGED |
| Metrics | ✅ synced (after git add) |

## Reviewer acceptance criteria

1. Pure domains have no I/O imports — machine-checked for all 14 pure
   inventory modules (not just the three kernels).
2. Golden Traces remain equivalent — 16/16 unchanged; no behavior change
   (thinning removed only a dead internal define; zero exported-name
   removals).
3. Public API intact — export-surface pins prove all 39 + 24 names still
   resolve; dynamic-require loader-convention probe passes.
4. Facade thinning is grep-backed and documented in source.
5. Release artifacts consistent (version sync, CHANGELOG, lint gates).

# Plan: v0.99.93 — Session Lifecycle & Retry Follow-up Closure

**Status:** FROZEN — NOT STARTED
**Authority:** roadmap + v0.99.87 freeze contract + W4 (#9246) terminal dispositions
**Plan-ID / Hash:** generated at campaign start
**Dependency:** v0.99.93 released (`128f8259`); milestone #879 closed 6/6
**GitHub:** milestone #880; waves #9276–#9281
**Findings:** six deferred W4 follow-ups (W0-F1…F5, W3-F2); repairs already shipped for #9192 (superseded) and #9121 (milestone-gate job-list sync, `68de6531`)

## Goal

Close the six deferred session-lifecycle/compaction/retry findings with correctness-first changes (guarded ownership claims, canonical prompt terminal contract, close/prompt concurrency coordination, cancellation-aware backoff), preserve oracle/effect equivalence where extraction is performed, and release v0.99.93.

## Immutable wave map

| Wave | Issue | Title | Scope | Required gate / acceptance |
|---|---|---|---|---|
| W0 | #9276 | Prompt ownership claim before outer dynamic-wind guard (W0-F1) | Move `try-claim-prompt!` claim under the outer `dynamic-wind` cleanup; define ownership-claim boundary so a failure in `begin-session-turn!`/outer `turn.started` publication releases state | lifecycle + session-interruption + Fast; ownership released on any pre-guard failure |
| W1 | #9279 | Compaction start-event ownership leak + block/error completion (W0-F4) | Make start-event publication part of the guarded cleanup; ensure automatic compaction never reports completion/cooldown after hook block or body error while leaking ownership | compaction + Fast; same claim/guard pattern as W0 applied in `runtime/compaction` |
| W2 | #9281 | Extract rollback prompt-scope wrapper (W3-F2) | Extract `runtime/session/session-prompt-scope.rkt` (parameterize current-prompt-operation-session + current-rollback-state with inner dynamic-wind save-back); caller-retained save-back and timing preserved | rollback + session-owned + Fast; oracle regeneration shows save-back timing unchanged |
| W3 | #9277 | Unify prompt terminal identity/event (W0-F2) | Decide a single canonical prompt terminal contract replacing the three distinct terminals (inner stream terminal, `turn.completed` w/o id, request-correlated prompt terminal); align events/tests | lifecycle + event taxonomy + Fast; one canonical terminal producer per prompt |
| W4 | #9280 | Cancellation-aware retry backoff + partial-metadata preservation (W0-F5) | Make retry sleep cancellation-aware; preserve retry metadata through partial recovery wrapping | retry + midstream-stall + Fast; backoff aborts on cancel, metadata survives partial wraps |
| W5 | #9278 | Coordinate close with an active prompt (W0-F3) | Add prompt/compaction ownership coordination to `close-session!`; no deactivation/repository-close while `run-prompt!` is active; no `session.updated` after close | **Broad** + session-close + lifecycle + rollback + Fast; concurrency-correct close (separate concurrency milestone) |
| W6 | — | Series Closure and Release | 100% traceability, synchronized projections, public release v0.99.93 | **Broad** + Arch + Security + Workflow + Smoke + Release + Manifest/Bundle + Main CI + independent review |

**Broad gates:** W5 (concurrency) and W6 (release). W2 carries Arch via the extraction oracle. Every issue wave keeps the `session-lifecycle.rkt` 600-line budget and preserves oracle/effect order; success requires correctness and locality, not file movement.

## Sequencing rationale

1. **W0/W1 (F1, F4)** — the two ownership-before-guarded-cleanup defects (session prompt claim; compaction start publication). Highest-risk unguarded state; the same pattern, fixed first.
2. **W2 (W3-F2)** — pure extraction with oracle regeneration; a clean refactor between the two ownership waves and the concurrency work.
3. **W3 (F2)** — canonical terminal contract; depends on W0/W1 settling ownership so terminal events are emitted from a single guarded producer.
4. **W4 (F5)** — retry backoff/metadata; independent, smaller, before the concurrency wave.
5. **W5 (F3)** — close/active-prompt concurrency; High severity, separate concurrency milestone, Broad gate.
6. **W6** — series closure + release.

## Constraints

- No new production abstraction beyond the findings' required changes; no artificial equality.
- Each wave: branch → `gh_wave_start` → TDD → focused/Fast gates → independent read-only review → PR (17/17 CI, required-policy NONE unmet) → squash merge → API-close issue → update STATE/VALIDATION → `builder_report`.
- Tracked `q/docs/planning/` STATE/VALIDATION mirror canonical `.planning/`; mid-campaign doc updates ride the next wave branch.
- Release surfaces (`info.rkt`, `README.md`, `CHANGELOG.md`, `util/version.rkt`) bumped to 0.99.93 only in W6 after all test gates.
- Metrics are git-index based: stage → `racket scripts/metrics.rkt --sync-all` → stage again after any line-count change.
- Known flakes honored (`test-settings.rkt` parallel flake; `test-pre-commit.rkt` dirty-index; tmux-explore `gh` stub).

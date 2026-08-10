# Plan/State Projection Kernel — v0.99.89 W2 (#9228, milestone #876)

**Status:** DONE (pending merge)
**Branch:** `feature/v09989-w2-plan-state-projection-kernel`
**Base:** `eb7807ae` (W1 merge, PR #9255)
**Gate:** GSD governance/workflow + Broad + Fast + lint-format + review

## Goal (roadmap, immutable)

Compute PLAN/STATE/VALIDATION/wave changes purely and apply them through an
atomic effect shell. **Acceptance: projections remain equivalent AND a crash
cannot leave stale partial tracking.**

## Oracle finding addressed

**Finding #2:** crash between the durable commit and the projection update
leaves stale projections; resume does not repair them. W2 makes the
projection computation a pure function and its application atomic, then
adds a durable-record reconciliation on campaign resume.

## What was delivered

### NEW `extensions/gsd/projection-kernel.rkt` — PURE projection kernel
- Requires only `racket/base` + `racket/string` (purity enforced by a
  require-scan fitness test, mirroring the W1 transition-kernel discipline).
- Neutral domain data: `PROJECTION-KINDS` (`plan-index wave-doc state-table`),
  `PROJECTION-STATUS-MARKERS`, `wave-status->projection-string` /
  `wave-status->state-string` (durable campaign symbol → display string).
- Pure transforms (text → text), **byte-identical to the legacy inline
  writers they replace**:
  - `project-plan-index-update` ≡ `wave-docs.rkt` `update-index-line`
  - `project-wave-doc-update` ≡ `write-wave-doc!` header rebuild
  - `project-state-row-update` ≡ `wave-completion.rkt` `update-state-table!`
- `project-wave-status-set` — the complete projection plan for one wave
  status change (plan-index + wave-doc + state-table in one pure call).
- `project-reconciliation-set` — re-derives the full projection set from the
  durable wave statuses; idempotent (in-sync files project to themselves).

### NEW `extensions/gsd/projection-effects.rkt` — atomic effect shell
- `atomic-write-file!` / `apply-atomic-files!` — temp-write-then-rename per
  file (mirrors the durable outbox write pattern); a crash never tears a file.
- `apply-projection-set!` — resolves entries to paths (wave-doc slug via a
  caller-provided resolver), skips idempotent writes, applies atomically.
- `apply-wave-status-projections!` — reads current texts, computes the pure
  set, applies; the completion/failure transition entry point.
- `reconcile-projections-from-waves!` — repairs stale PLAN.md/wave-doc/
  STATE.md from the durable record (the crash-repair entry point).

### Wiring (facade-thin adapters)
- `wave-completion.rkt` `try-complete-wave!` (DONE/FAILED branches) → single
  `apply-wave-status-projections!` call (was `mark-wave-status!` +
  `update-state-table!`).
- `go-orchestrator.rkt` runner-error paths (FAILED ×2) → same single call.
- `go-orchestrator.rkt` `run-campaign!` → reconcile at start (after lease +
  authoritative reload): stale projections left by a crash are repaired
  before any new wave runs; reconcile failures only log, never block.
- `wave-docs.rkt` — new exported helpers `wave-slug` / `plan-slug-map`.
  `mark-wave-status!` / `update-state-table!` remain for other callers
  (`/skip`, `/wave-done`, archive) — unchanged contracts.

### Tests — NEW `tests/test-projection-kernel.rkt` (22 tests)
- Pure transform tables (plan-index markers, wave-doc headers, state rows)
  for every status; unknown-index no-op; idempotency.
- **Byte-equivalence vs legacy**: `project-plan-index-update` ≡
  `update-plan-index-text`; full-file equivalence of the shell vs
  `mark-wave-status!` + `update-state-table!` on identical fixtures.
- `project-wave-status-set` completeness (doc/state omission rules).
- Reconciliation: stale → restored, in-sync → idempotent, missing doc
  skipped, interrupted/pending → Inbox/PENDING rows.
- Shell integration: 3-file atomic apply, idempotent second call,
  crash-repair reconcile on a fixture, missing-PLAN no-op.
- Kernel purity fitness (require-scan allowlist).

### Golden-trace oracle (behavioral equivalence + the W2 behavior change)
- **15/16 traces byte-identical** to W0 (all production projections still
  produce identical content — the refactor is transparent).
- **crash-resume trace now asserts the FIXED behavior**: the W2 reconcile
  repairs the stale W0 projection on resume, so plan-index / wave-docs /
  state-table converge to `((0 done) (1 done))` and `plan-overall`
  becomes `all-done` (pre-W2: stale `pending` + `partly-done`). The
  crash-injection trace (no resume) still pins stale-after-crash.

## Equivalence evidence

- Golden traces 16/16 (15 unchanged + crash-resume repair pin).
- GSD surface batch (`test-gsd-*.rkt` + `test-transition-*.rkt` + golden):
  **1038 tests passed**.
- Projection kernel 22/22; inventory direct run 5/5 (count 29).
- Workflows suite ✅; lint-format 2088 files 0/0.
- Fast suite ✅ 1059 files / 15472 tests.
- Broad suite ✅ 1237 files / 17739 tests.

## Notes

1. The legacy `update-state-table!` / `update-index-line` writers normalize
   the trailing newline of STATE.md/PLAN.md (string-split + join); the kernel
   replicates this exactly for byte-equivalence (idempotent transforms that
   are in sync may still normalize the trailing newline — harmless and
   legacy-consistent; documented in the kernel tests).
2. Stale `.zo` dependents after the wave-docs surface change require the
   full compiled-cache wipe (`find . -type d -name compiled -exec rm -rf {} +`)
   — same lesson as W1.
3. VALIDATION.md is intentionally untouched by completion (production
   semantics: only archive/replan reset it) — the reconcile preserves this.

## Reviewer acceptance criteria (pre-review checklist)

1. Pure projection kernel with neutral domain data; no FS/GitHub/Runtime
   imports (require-scan proves it).
2. Atomic effect shell: temp+rename, batch apply, no torn files.
3. Completion paths use the shell; crash cannot leave partial tracking.
4. Reconcile repairs stale projections on resume (golden crash-resume pin).
5. Equivalence: 15/16 golden traces byte-identical; GSD batch 1038 green.
6. Gates: Fast + Broad + lint-format + workflows all green.

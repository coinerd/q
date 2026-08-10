# Validation: v0.99.89 — GSD Pure Domain Decomposition

**Status:** ACTIVE — W0–W2 validated; W3 next

| Wave | Focused/TDD | Fast | Specific gate | Review | Result |
|---|---|---|---|---|---|
| W0 Golden Workflow Traces | ✅ 16/16 (1 helper + 1 test) | ✅ 1057/15420 | ✅ workflows 29 files/161 tests | ✅ APPROVED (1 recheck) | ✅ DONE — merged `8214e6a4` (PR #9254, #9226 closed) |
| W1 Pure Transition Kernel | ✅ 29/29 (new test file) + 1038 GSD batch | ✅ 1058 files | ✅ pure-kernel + Arch (import closure) | ✅ APPROVED (0 recheck; 2 MINOR + 5 INFO folded `4c55f57b`) | ✅ DONE — merged `eb7807ae` (PR #9255, #9227 closed) |
| W2 Plan/State Projection Kernel | ✅ 22/22 (new test file) + 1038 GSD batch | ✅ 1059 files / 15472 | ✅ workflows + Broad 1237 files / 17739 + lint-format 0/0 | ✅ APPROVED (0 recheck) | ✅ DONE — merged `74da7e8a` (PR #9256, #9228 closed) |
| W3 Command Parsing & Intent Boundary | pending | — | parser fitness + command corpus + Fast | — | — |
| W4 Facade Thinning + Release | pending | — | Broad + Arch + Workflow + Smoke + Release | — | — |

## W0 evidence record

1. Golden matrix: `tests/workflows/gsd/test-gsd-golden-traces.rkt` → 16/16
   (15 original + milestone-close determinism probe).
2. Determinism: plan-creation, go-success, milestone-close double-run
   probes in-suite; reviewer ran milestone-close 20× — byte-identical.
3. Semantic pins verified against production: verifier-first (no DONE
   without approval, no outbox event), interruption (durable-only),
   failure (FAILED projections), retry (attempt-2/fence-2), resume
   (per-run completed, attempt-2/fence-3), replan (new plan-id, old
   record preserved), milestone-close (production `archive-completed-plan!`,
   archived event, FSM reset to idle, durable record + outbox survive),
   crash (durable commit with stale projections; resume does not repair).
4. Workflows suite: `--suite workflows` → 29 files / 161 tests.
5. Fast: `--suite fast` → 1057 files / 15420 tests (first run hit the
   pre-existing test-settings.rkt parallel flake; standalone pass; rerun
   fully green).
6. lint-format: 2083 files, 0 errors, 0 warnings.
7. Metrics: README synced (1283 files / 222306 lines / 34789 assertions).

## W1 acceptance (roadmap)

Pure transitions/preconditions/terminal states in a small pure module;
neutral GSD-domain data only; no FS/GitHub/Runtime/event-bus imports;
facade delegates; property/table tests for allowed/forbidden transitions
and idempotency; fitness test forbids I/O imports in the kernel.
Gate: pure-kernel + Arch + Fast.

## W1 evidence record

1. Kernel tests: `tests/test-transition-kernel.rkt` → 29/29.
2. Facade contracts UNCHANGED: `test-transition-logic.rkt` 46/46,
   `test-transition-matrix.rkt` 75/75, `test-gsd-transition-logic.rkt`
   123/123 — no call-site changes.
3. Equivalence: golden traces 16/16 UNCHANGED; GSD surface batch
   (`test-gsd-*.rkt` + `test-transition-*.rkt` + golden) 1038 passed.
4. Purity: require-scan on `transition-kernel.rkt` → only
   `racket/match` + `racket/set`; no file/GitHub/runtime/event-bus.
5. Fast: `--suite fast` → 1058 files, PASS (12m7 after full compiled-cache
   wipe; recompile dominated).
6. lint-format: 2083 files, 0/0.
7. Inventory direct run: 5/5 (count 27).
8. Reviewer: APPROVED on first review (2 MINOR + 5 INFO); MINORs folded in
   follow-up `4c55f57b` (PLAN amendments, report rewording, campaign-complete?
   coverage+count, terminal-state? comment). No recheck spawned.
9. Metrics: README re-synced after test additions.

## W1 gate notes

- Full compiled-cache wipe was required after the facade rewrite
  (`instantiate-linklet` mismatch in pre-compiled dependents); resolved by
  `find . -type d -name compiled -exec rm -rf {} +` + recompile.
- `raco test -t` does not execute `module+ main` (pre-existing weakness:
  `test-gsd-responsibility-inventory.rkt` is latent-dead in CI, correct on
  direct `racket` run). Out of W1 scope.

## W2 evidence record

1. Kernel tests: `tests/test-projection-kernel.rkt` → 22/22.
2. Byte-equivalence: `project-plan-index-update` ≡ `update-plan-index-text`
   (all statuses); shell ≡ `mark-wave-status!` + `update-state-table!` on
   identical fixtures (PLAN.md/STATE.md/wave docs byte-identical).
3. Golden oracle: 16/16 — 15 traces byte-identical to W0; crash-resume now
   asserts repaired projections (`((0 done) (1 done))`, plan-overall
   `all-done`); crash-injection trace still pins stale-after-crash.
4. GSD surface batch: 1038 tests passed.
5. Workflows suite ✅ (2m45). Fast ✅ 1059 files / 15472. Broad ✅ 1237
   files / 17739. lint-format 2088 files 0/0.
6. Inventory direct run 5/5 (count 29; projection-kernel/effects classified
   event-projection; effects fs-write/fs-rename/mkdir/path-ops).
7. Purity: require-scan on projection-kernel.rkt → only
   racket/base + racket/string.
8. Metrics: README re-synced (assertion count 15472+).

## W2 gate notes

- Stale `.zo` dependents after wave-docs surface change → full compiled-cache
  wipe + recompile (W1 lesson re-applied).
- `wave-slug` is contracted (2 args) — callers pass a closing lambda
  `(lambda (idx) (wave-slug base-dir idx))` as the shell's slug resolver.
- Trailing-newline normalization of PLAN/STATE via string-split+join is
  legacy-consistent and preserved byte-for-byte (kernel replicates it).

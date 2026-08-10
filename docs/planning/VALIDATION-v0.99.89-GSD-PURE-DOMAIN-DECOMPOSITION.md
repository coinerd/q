# Validation: v0.99.89 — GSD Pure Domain Decomposition

**Status:** ACTIVE — W0 validated; W1 next

| Wave | Focused/TDD | Fast | Specific gate | Review | Result |
|---|---|---|---|---|---|
| W0 Golden Workflow Traces | ✅ 16/16 (1 helper + 1 test) | ✅ 1057/15420 | ✅ workflows 29 files/161 tests | ✅ APPROVED (1 recheck) | ✅ DONE — merged `8214e6a4` (PR #9254, #9226 closed) |
| W1 Pure Transition Kernel | pending | — | pure-kernel + Arch | — | — |
| W2 Plan/State Projection Kernel | pending | — | GSD governance/workflow + Broad | — | — |
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

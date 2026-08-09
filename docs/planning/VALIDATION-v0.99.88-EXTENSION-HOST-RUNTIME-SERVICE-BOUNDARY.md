# Validation: v0.99.88 — Extension Host/Runtime Service Boundary

**Status:** ACTIVE — W1 gates green; review pending
**Validated SHA:** W1 feature head (pre-review)

| Wave | Focused/TDD | Fast | Specific gate | Broad/Arch | Review | Result |
|---|---|---|---|---|---|---|
| W0 | ✅ 257/257 (9 files) | ✅ 1053/15369 | characterization 12/12 | Arch informational | ✅ APPROVED | ✅ DONE — merged `61fc7057` |
| W1 | ✅ 75/75 (4 files) | ✅ 1054/15381 | protocol 10/10 + Typed/dynamic-require | ✅ Arch 21/231 | PENDING | REVIEW |
| W2 | PENDING | PENDING | negative exception probe | Broad + Arch required | PENDING | PENDING |
| W3 | PENDING | PENDING | package + Security | Arch required | PENDING | PENDING |
| W4 | PENDING | PENDING | Security + Smoke + Release | Broad + Arch required | PENDING | PENDING |

## W0 evidence record

1. Characterization: `racket scripts/run-tests.rkt tests/test-extension-context-characterization.rkt` → 12/12.
2. Focused: + `test-extension-context.rkt` + `test-extension-loader.rkt` + `test-provider-registry.rkt` + `test-wave4-sdk-expansion.rkt` + `test-gsd-planning.rkt` + `test-gsd-go-orchestrator.rkt` + `test-agent-session-extensions.rkt` + `test-session-switch.rkt` → 9 files / 257 tests.
3. Fast: `--suite fast` → 1053 files / 15369 tests, 0 failures.
4. Format: `racket scripts/lint-format.rkt` → 2076 files, 0 errors, 0 warnings.
5. Reviewer: APPROVED after 1 REQUEST_CHANGES round (CH7/CH4b/CH4c added; §9 corrected; attribution fixed; CH1 scan loosened).

## W1 evidence record

1. Protocol: `test-extension-host-service-protocol.rkt` → 10/10 (contracts,
   registry dual-run, dynamic-require, Typed Racket, compatibility facade).
2. Focused: protocol + provider-registry + extension-context + W0
   characterization → 4 files / 75 tests.
3. Arch: `--suite arch` → 21 files / 231 tests.
4. Fast: `--suite fast` → 1054 files / 15381 tests.
5. Format: 2079 files, 0 errors, 0 warnings.
6. Independent review: pending.

## Release acceptance (W4)

All five old exceptions are removed or newly decided from evidence, none expired; public behavior/dynamic loading remain compatible.

# Validation: v0.99.88 — Extension Host/Runtime Service Boundary

**Status:** ACTIVE — W3 implemented; review pending
**Validated SHA:** W3 feature head (pre-review)

| Wave | Focused/TDD | Fast | Specific gate | Broad/Arch | Review | Result |
|---|---|---|---|---|---|---|
| W0 | ✅ 257/257 (9 files) | ✅ 1053/15369 | characterization 12/12 | Arch informational | ✅ APPROVED | ✅ DONE — merged `61fc7057` |
| W1 | ✅ 76/76 (4 files) | ✅ 1054/15382 | protocol 11/11 + Typed/dynamic-require | ✅ Arch 21/231 | ✅ APPROVED | ✅ DONE — merged `8214d7cd` |
| W2 | ✅ 149/149 (6 files) | ✅ 1055/15390 | dual-run 7/7 + negative probe H3 | ✅ Arch 21/232 + Broad 1241/17657 | ✅ APPROVED | ✅ DONE — merged `0fb0779d` |
| W3 | ✅ 243/243 (13 files) | ✅ 1056/15403 | package 51/51 + Security 63/702 | ✅ Arch 21/235 | PENDING | REVIEW |
| W4 | PENDING | PENDING | Security + Smoke + Release | Broad + Arch required | PENDING | PENDING |

## W0 evidence record

1. Characterization: `racket scripts/run-tests.rkt tests/test-extension-context-characterization.rkt` → 12/12.
2. Focused: + `test-extension-context.rkt` + `test-extension-loader.rkt` + `test-provider-registry.rkt` + `test-wave4-sdk-expansion.rkt` + `test-gsd-planning.rkt` + `test-gsd-go-orchestrator.rkt` + `test-agent-session-extensions.rkt` + `test-session-switch.rkt` → 9 files / 257 tests.
3. Fast: `--suite fast` → 1053 files / 15369 tests, 0 failures.
4. Format: `racket scripts/lint-format.rkt` → 2076 files, 0 errors, 0 warnings.
5. Reviewer: APPROVED after 1 REQUEST_CHANGES round (CH7/CH4b/CH4c added; §9 corrected; attribution fixed; CH1 scan loosened).

## W1 evidence record

1. Protocol: `test-extension-host-service-protocol.rkt` → 11/11 (contracts,
   registry dual-run, dynamic-require, Typed Racket, compatibility facade).
2. Focused: protocol + provider-registry + extension-context + W0
   characterization → 4 files / 75 tests.
3. Arch: `--suite arch` → 21 files / 231 tests.
4. Fast: `--suite fast` → 1054 files / 15382 tests.
5. Format: 2079 files, 0 errors, 0 warnings.
6. Independent review: APPROVED (1 pass; 3 MINOR hardening items folded in).

## W2 evidence record

1. Dual-run: `test-provider-registry-service-isolation.rkt` → 7/7 (D1–D7:
   registration, update, lookup, list order, unregister, errors, facade).
2. Focused: isolation + protocol + characterization + extension-context +
   wave4-sdk + arch-fitness → 6 files / 149 tests.
3. Boundary: `module->imports` of `extensions/context.rkt` → no runtime/, no
   provider-registry; only base/contract/host-services/llm-provider?/types.
4. Arch: `--suite arch` → 21 files / 232 tests (incl. H3 negative probe;
   exception gates updated to 4 extensions exceptions).
5. Policy: `dependency-policy.rktd` context.rkt exception removed.
6. Broad: `racket scripts/run-tests.rkt` → 1241 files / 17657 tests, 0 failures.
7. Fast: `--suite fast` → 1055 files / 15390 tests, 0 failures.

## W3 evidence record

1. Dual-run: `test-package-host-service-isolation.rkt` → 9/9 (E1–E7 +
   E1b/E6b: neutral shape, list/installed?/install/remove parity, error-string
   pass-through, null-service degradation, ctx-injected tool path).
2. Package: `test-package.rkt` + isolation + `test-manifest.rkt` → 3 files /
   51 tests.
3. Focused: 13 files / 243 tests (isolation, protocol, characterization,
   extension-context, wave4-sdk, session-switch, contract-tightening,
   context-hooks, tool-dispatch, phase-e, tool-registration, arch-fitness,
   exception-fitness).
4. Boundary: `extract-requires` of `extensions/ext-package-manager.rkt` →
   no runtime/ (H4); adapter owns `package.rkt` + host-services (H5);
   neutral protocol exports the capability (H6).
5. Arch: `--suite arch` → 21 files / 235 tests (H4/H5/H6; exception gates
   updated to 3 extensions exceptions, runtime-boundary `'()`).
6. Policy: `dependency-policy.rktd` ext-package-manager exception removed
   (MA-04 closed).
7. Security: `--suite security` → 63 files / 702 tests, 0 failures.
8. Fast: `--suite fast` → 1056 files / 15403 tests, 0 failures.
9. Format: `racket scripts/lint-format.rkt` → 2081 files, 0 errors, 0 warnings.

## Release acceptance (W4)

All five old exceptions are removed or newly decided from evidence, none expired; public behavior/dynamic loading remain compatible.

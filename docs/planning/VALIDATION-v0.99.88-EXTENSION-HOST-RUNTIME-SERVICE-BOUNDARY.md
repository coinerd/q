# Validation: v0.99.88 — Extension Host/Runtime Service Boundary

**Status:** ACTIVE — W4 implemented; review pending
**Validated SHA:** W4 feature head (pre-review)

| Wave | Focused/TDD | Fast | Specific gate | Broad/Arch | Review | Result |
|---|---|---|---|---|---|---|
| W0 | ✅ 257/257 (9 files) | ✅ 1053/15369 | characterization 12/12 | Arch informational | ✅ APPROVED | ✅ DONE — merged `61fc7057` |
| W1 | ✅ 76/76 (4 files) | ✅ 1054/15382 | protocol 11/11 + Typed/dynamic-require | ✅ Arch 21/231 | ✅ APPROVED | ✅ DONE — merged `8214d7cd` |
| W2 | ✅ 149/149 (6 files) | ✅ 1055/15390 | dual-run 7/7 + negative probe H3 | ✅ Arch 21/232 + Broad 1241/17657 | ✅ APPROVED | ✅ DONE — merged `0fb0779d` |
| W3 | ✅ 243/243 (13 files) | ✅ 1056/15403 | package 51/51 + Security 63/702 | ✅ Arch 21/235 | ✅ APPROVED | ✅ DONE — merged `5131ca3d` |
| W4 | ✅ 96/96 (5 files) | ✅ 1056/15405 | Security 63/702 + Smoke 19/306 + release dry-run 5/5 | ✅ Arch 21/237 + Broad 1234/17672 | PENDING | REVIEW |

## W3 evidence record

1. Dual-run: `test-package-host-service-isolation.rkt` → 9/9 (E1–E7 +
   E1b/E6b).
2. Package: `test-package.rkt` + isolation + `test-manifest.rkt` → 51/51.
3. Focused: 13 files / 243 tests.
4. Boundary: `extract-requires` of ext-package-manager → no runtime/ (H4).
5. Arch: 21 files / 235 tests (H4/H5/H6; exception gates 3, runtime '()).
6. Policy: ext-package-manager exception removed (MA-04 closed).
7. Security: 63 files / 702 tests.
8. Fast: 1056 files / 15403 tests.
9. Format: 2081 files, 0/0.

## W4 evidence record

1. Policy/boundary focused: `test-arch-boundaries.rkt` +
   `test-arch-fitness.rkt` + `test-architecture-baseline.rkt` +
   `test-maintainability-roadmap-freeze.rkt` +
   `test-extension-exception-fitness.rkt` → 5 files / 96 tests.
2. Exception fitness: 12/12 (incl. new "permanent pair waiver" gate +
   permanent positive probe).
3. Arch: `--suite arch` → 21 files / 237 tests.
4. Security: `--suite security` → 63 files / 702 tests.
5. Fast: `--suite fast` → 1056 files / 15405 tests.
6. Broad: `racket scripts/run-tests.rkt` → 1234 files / 17672 tests.
7. Smoke: `--suite smoke` → 19 files / 306 tests.
8. Release dry-run: `scripts/release-dry-run.rkt` → 5/5.
9. Format: `racket scripts/lint-format.rkt` → 2081 files, 0/0.
10. Policy: dependency-policy.rktd parses as one top-level list (11
    sections); all three extension exceptions permanent pair waivers with
    justification + boundary + destinations.
11. Version: util/version.rkt + info.rkt + README + docs + CHANGELOG at
    0.99.88; lint-version 0 errors; lint-release-notes PASSED.

## Release acceptance (W4)

All five old exceptions terminal: context.rkt + ext-package-manager removed
(W2/W3); dialog-api/ui-surface/widget-lifecycle permanent pair waivers (W4).
None expired. No abstract UI framework built. Release v0.99.88 verified by
Broad + Arch + Security + Smoke + release dry-run + independent review.

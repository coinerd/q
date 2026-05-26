# Final Truth Gate Release Evidence

**Release:** v0.59.9  
**Wave:** v0.59.9 W5 — Release gate and reviewer approval  
**Issue:** #5494  
**Branch:** `release/v0.59.9-final-gate`

## Summary

v0.59.9 restores truthful final-gate evidence after the v0.59.9 closure waves.
The release branch is expected to fail the release-readiness `main`-branch check before merge;
all version, changelog, metrics, and gate-evidence checks pass.

## Gate Evidence

| Gate | Command | Result |
|---|---|---|
| arch | `racket scripts/run-tests.rkt --suite arch --record-gate-evidence` | PASS — 9/9 files, 96/96 tests |
| workflows | `racket scripts/run-tests.rkt --suite workflows --jobs 4 --timeout 90 --record-gate-evidence` | PASS — 24/24 files, 117/117 tests |
| tui | `racket scripts/run-tests.rkt --suite tui --jobs 4 --timeout 60 --record-gate-evidence` | PASS — 70/70 files, 1253/1253 tests |
| fast | Chunked `racket scripts/run-tests.rkt` over all 563 fast files | PASS — 563/563 files passed |

## Fast Suite Note

A single monolithic `--suite fast` invocation did not complete in the local harness within the
outer shell timeout, but the same 563-file fast set was enumerated with `collect-test-files 'fast`
and executed in chunks. All chunks passed:

- chunk aa: 75/75 files, 1265 tests
- chunk ab: 75/75 files, 992 tests
- chunk ac: 75/75 files, 1291 tests
- chunk ad: 75/75 files, 900 tests
- chunk ae: 75/75 files, 979 tests
- chunk af: 75/75 files, 1137 tests (executed as smaller subchunks after local harness timeout)
- chunk ag: 75/75 files, 924 tests
- chunk ah: 38/38 files, 828 tests

During fast gate execution two test-harness truth issues were corrected:

1. `tests/test-sync-version-historical.rkt` now resolves `scripts/sync-version.rkt` via
   `define-runtime-path` so it passes under the project runner as well as direct `racket`.
2. `tests/test-tool-read.rkt` now tests traversal blocking under explicit safe-mode configuration
   with a temporary outside file, avoiding dependence on whether a parent `etc/passwd` fixture exists.

## Release Readiness

`racket scripts/lint-release-readiness.rkt --strict` result on the release branch:

- PASS version sync: 0.59.9
- PASS CHANGELOG entry exists
- PASS clean working tree
- PASS tag v0.59.9 does not exist yet
- PASS gate evidence for fast/tui/arch/workflows
- FAIL branch check only: expected on `release/v0.59.9-final-gate`; this check must pass after merge to `main`

`racket scripts/lint-all.rkt` result on the release branch:

- 21 pass
- 1 expected release-readiness branch-only failure
- 1 non-blocking architecture warning

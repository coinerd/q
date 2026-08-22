# Metadata discovery parity fixture (W0)

Frozen input tree for `q/tests/ci/metadata-discovery-test.rkt`
(W0 of `docs/planning/PLAN-v1.00.11-TDD-CI-INTEGRITY-BASELINES.md`).

Purpose: pin the file-discovery contract of
`q/scripts/run-tests/classify-metadata.rkt` (base-dir resolution) and
`q/scripts/run-tests/classify.rkt` (`collect-test-files`) so that both
invocation modes — direct CLI from the repo root and a clean-copy temp root
that mimics the CI checkout — discover the identical normalized relative
path list.

## Tree contract

| Path (relative to `fixture/tests/`) | Expected discovered? | Why |
|---|---|---|
| `alpha-test.rkt` | YES | ordinary fully-tagged test file |
| `beta-plain.rkt` | YES | no metadata; heuristic classification |
| `link-target.rkt` | YES | ordinary file; symlink target |
| `symlinked-test.rkt` | YES (when symlinks are preserved) | symlink to `link-target.rkt` |
| `nested/nested-test.rkt` | YES | one level of nesting |
| `nested/deep/deep-test.rkt` | YES | two levels of nesting |
| `generated/generated-test.rkt` | YES — current contract has no ignore rule for `generated/` (deliberate pin) | generated/ignorable dir |
| `compiled/stray-test.rkt` | NO | `/compiled/` exclusion (stale bytecode areas) |
| `not-a-test-helper.rkt` | NO | `@not-test #t` exclusion |
| `../outside/outside-test.rkt` | NO | outside the discovery root (`fixture/tests/`) |

`symlinked-test.rkt` is a git symlink (`mode 120000`). Platforms that
materialize symlinks as regular files still satisfy parity — the test only
requires that the entry discovered in mode A equals mode B.

# Test-Fixture Amplification — v1.00.28 W2 (Pristine-Copy Experiment)

Wave: v1.00.28-w2 · Ticket: #9590 · Branch: `campaign/v1.00.28-w2`
Roadmap: `PLAN-v1.00.28-TEST-WORKLOAD-REDUCTION.md` §W2

## Question

Can we stop rebuilding expensive Git test worlds for every small assertion by
constructing each test instance as a physical copy of a pristine, fully
self-contained repository instead of `git clone --no-local`, and is that
switch safe?

## Measured amplification (fixture-census.json)

Counter channel: the W0 fixture counters emit one line per git-repository
construction; per-family counts below are line counts from the W0 census logs
plus fresh runs on this branch.

| Suite | Git repositories | Session temp roots | Other |
|---|---|---|---|
| test-gsd-delivery-verifier | 21 (19× `dv-base`, 2× `dv-shape`) | 21 | 21 subprocess-backed verifier envs |
| test-gsd-wave-worktree | 4 (`git:w`) | 4 | 4 worktrees |
| test-gsd-campaign-repository | 0 | 1 per test (33) | 33 planning trees |
| test-pristine-git-fixture (new) | 14 | 15 | — |
| fast suite | 0 | 0 | — |

Branches/commits per construction are not counter-instrumented (recorded as
`null` in the census with the reason). Amplification concentrates in the
delivery-verifier suite: the same `dv-base` shape is rebuilt 19 times per run.

## Experiment (git-fixture-experiment.json)

Interleaved A/B through the shared constructor `make-private-git-fixture!`;
sample = construct + `git rev-parse --verify HEAD` first-use assert + cleanup;
2 uncounted warmups, 14 interleaved rounds per arm, wall clock. Both arms ran
the identical method; artifacts are checksummed (SHA256SUMS).

| Strategy | Median construct | Median first-use | Median total | Samples |
|---|---|---|---|---|
| `clone` (legacy `git clone --no-local`) | **38.77 ms** | 4.49 ms | **43.51 ms** | 14/14 ok |
| `pristine-copy` (physical copy of pristine baseline) | 143.95 ms | 4.24 ms | 148.52 ms | 14/14 ok |

The hypothesis is refuted on this machine: the physical copy of the pristine
baseline is ~3.7× slower to construct than `git clone --no-local`.

## Stress (stress-evidence.json)

12 concurrent instances per strategy, pristine baseline byte-integrity
snapshotted before and after: both strategies pass all instances
(`pristine-copy` 12/12 in 719 ms, `clone` 12/12 in 184 ms); the baseline
stayed byte-identical (16 files / 1574 bytes before and after). The safety
suite additionally asserts: no hardlinks, no `objects/info/alternates`, no
shared object store, no shared mutable refs, no inherited hooks, repo-local
`user.name`/`user.email` per instance, and byte-level isolation under
concurrent create/destroy.

## Activation gate — truthful HOLD

| Criterion | Result |
|---|---|
| Isolation tests pass | ✅ |
| Parallel stress passes | ✅ |
| Before/after median improves materially | ❌ (3.7× slower) |
| No behavior changes in consuming suites | ✅ (33/33, 25/25, 10/10) |
| Rollback is one edit, contract-tested | ✅ |

The median-improvement criterion fails in the wrong direction, so the
pristine-copy strategy is **not** activated as the construction default. The
legacy `clone` path remains the default; the immutable pristine baseline stays
in the tree as the backing store for the W2 safety suite and the stress
experiment. Rollback remains one edit (the constructor strategy parameter),
contract-tested by the legacy-contract test in
`tests/test-pristine-git-fixture.rkt`.

## Files

- `artifacts/test-runtime/v1.00.28-w2/fixture-census.json`
- `artifacts/test-runtime/v1.00.28-w2/git-fixture-experiment.json`
- `artifacts/test-runtime/v1.00.28-w2/stress-evidence.json`
- `artifacts/test-runtime/v1.00.28-w2/SHA256SUMS`
- `tests/helpers/pristine-git-fixture.rkt`, `tests/test-pristine-git-fixture.rkt` (new)
- consuming suites behind the same constructor contract (edited as findings required)

Evidence trio: `docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w2.rktd`,
bound to the merge SHA at squash-merge per the Delivery Contract.

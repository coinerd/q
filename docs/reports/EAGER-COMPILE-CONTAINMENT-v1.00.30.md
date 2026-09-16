# Eager Compilation Containment — v1.00.30 W2

Milestone #895 · Issue #9688 · Branch `campaign/v1.00.30-w2` · Contract: `.planning/PLAN-v1.00.30-PR-CI-RECOVERY.md`

## What this is

Containment, not a recovery claim. W2 switches the CI Racket setup to the existing eager
full path via the global switch `RACKET_PREPARED_ARTIFACT=off`, so purge-plus-skipped-compile
amplification stops immediately, without weakening BUG-0065 (untrusted checkout bytecode can
never execute) and without introducing a per-file compile fallback.

## Mechanism

- `RACKET_PREPARED_ARTIFACT=off` is recorded by the coordinator as the prior global switch
  before the cohort runs.
- With the switch set, `.github/actions/setup-racket/action.yml` takes the eager full path:
  purge untrusted checkout bytecode → relink current q → `raco setup --no-docs --jobs 4 --pkgs q fmt`.
- The switch selection boundary is evaluated through `scripts/ci/prepared-env-report.rkt`
  (`PREPARED_ENV`), which is the single authority all consumers consult.
- All opting consumers audited: seven consumers consult the switch boundary; none bypasses it.
- Exactly one eager setup boundary per job; the boundary's internal `--jobs 4` is unchanged.
  Runner mode, test membership and worker counts are identical on both paths.

## Rollback semantics (Task 3)

An unsuccessful mitigation is rolled back, not advertised as recovery:

- If the eager full path fails (relink or `raco setup` failure), the action exits non-zero.
  No fallback compiles files piecemeal; no success is reported.
- Restoration (switch unset) returns to the restore path, which purges checkout bytecode
  before use — stale **and future-mtime** `*.zo`/`*.dep`/`compiled/` trees cannot execute.
- Restoring the switch is a return to the incumbent boundary, never claimed as a
  successful mitigation.

Drill evidence: `artifacts/ci-recovery/v1.00.30-w2/rollback-drill.json` (five fixture-controlled
steps, all pass). Local sandbox cannot produce representative `raco setup` wall-clock timings
(package compilation lands in the user install dir, not the checkout), so timing truth is
CI-cohort-owned, not local.

## Measurements (Task 2)

- Runtime/store restore outcome (artifact downloaded, relinked) is now reported separately
  from the usable compiled-code outcome by `scripts/ci/prepared-env-report.rkt`, so a
  restored-but-stale store can never be read as a healthy compile state.
- Purge, eager-compile and runner times, plus cache state, are recorded per CI run.
- Primary PR guard and R1 p95 of the slowest fast-shard runner are measured on the exact
  final head over ≥3 distinct comparable candidate PR head SHAs (coordinator-owned).

## Residual gap (honest reporting)

R1 PASS here means containment ONLY:

- The pre-W4 233–292 s job walls remain: eager full path removes skipped-compile amplification
  but does not rebalance shards (that is W4/W6 scope).
- Runner/job boundary differences between the R1 measurement rig and the primary PR guard
  persist; timings are not directly comparable across rigs.
- The 2171 s anchor is a single-PR maximum, not a population tail; it must not be used as a
  tail estimate.
- The controlled eager full path is containment, **not** the final compiled-root architecture
  (immutable-root activation is a later wave).

If R1 misses, W3 stops and the attempted mitigation is rolled back through controlled
delivery. No unverifiable claim that `off` is faster is made without cohort evidence.

## Verification

```
raco test tests/test-ci-eager-compilation.rkt tests/test-ci-workflow-diagnostics.rkt tests/test-prepared-env-report.rkt
racket scripts/check-deps.rkt
racket scripts/run-tests.rkt --suite fast
racket scripts/metrics.rkt --lint
```

All local checks green on this content at commit time; coordinator runs the owned verify lane
and enforces PR latency sample gates, independent review, protected squash merge and merge-SHA
binding before W3 may start.

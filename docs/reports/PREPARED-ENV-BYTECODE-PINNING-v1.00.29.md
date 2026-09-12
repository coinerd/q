# Prepared-Env Bytecode Pinning (BUG-0065) — systemic fix, v1.00.29 W4

Ticket: BUG-0065 (#9622) · Branch: `campaign/v1.00.29-w4` · Implementation: `a2c91336`, `17c9bca9`

## Summary

The v1.00.28 release-lane fix (commit `04637d83`) purged stale workspace bytecode only in the
release `test` job. v1.00.29 W4 promotes that fix to a **systemic invariant**:

> Every lane that consumes a restored/prepared workspace purges or fails-closed-verifies workspace
> bytecode before executing tests; every lane that produces a prepared artifact stamps it so
> consumers can verify freshness. Mtime is **never** treated as a freshness signal.

## Root cause (why mtime comparison is wrong)

Prepared-environment artifacts materialize `compiled/` output whose `.zo` mtimes are set at
**artifact extraction** — i.e. *newer* than the fresh checkout being restored into. Racket's
bytecode cache trusts mtimes, so a restored workspace executes **producer-era bytecode** instead of
the checked-out sources. Observed: the v1.00.27 cohort-report release failure ran a lowercase
"(c3, v1.00.27 w5)" suite against the v1.00.27 tag. The v1.00.28 fix purged only the release lane;
PR CI, main CI, nightly, and full-regression restore paths still trusted restored bytecode on a
successful restore (the guarded-restore path skipped the action's legacy purge step entirely).

## Mechanism

**Consumer side (every path):** the shared `.github/actions/setup-racket` composite action now runs
`Purge and verify workspace bytecode (BUG-0065, every path)` with `if: always()` — covering full
install+compile, flagged fallback, **and a successful prepared-environment restore**. The step:

1. counts workspace `.zo` outside the frozen fixture (`tests/metadata-discovery/fixture/`),
2. deletes them plus every `compiled/` directory,
3. **fails closed** (`::error` + `exit 1`) if any `.zo` survives,
4. reports loudly and countably: `::notice` with before/after counts and the prepared-env outcome,
   a step-summary block, and a `RUNNER_TEMP/bug-0065-purge-stamp.json` forensics stamp
   (RUNNER_TEMP so strict tag-publish readiness keeps its clean-workspace guarantee).

**Producer side (stamp, don't trust mtimes):** the prepared artifact carries its manifest tuple —
repository, git SHA, source digest, lock digest — stamped by `prepare-racket-environment`;
consumers verify that tuple against the job's own identity on restore (existing guarded-restore
check) **and purge workspace bytecode unconditionally**, so a stale or unmatched artifact can never
place executable producer bytecode.

**Verified-restore metric:** the purge is additive — restore success is still counted as verified
restore; no lane falls back to cold compile, so the ≥95 % verified-restore metric is preserved.
Purge/fallback events are loud and counted, never silent.

## Lane inventory

| Lane | File | Prepared-env restore input | Coverage |
|---|---|---|---|
| PR CI / main CI (fast-gate et al.) | `.github/workflows/ci.yml` (7 setup-racket jobs) | yes | shared action → purge + verify on every path |
| Full regression | `.github/workflows/ci.yml` (full-regression job) | yes | shared action |
| Nightly | `.github/workflows/nightly.yml` | consumes none today | shared action anyway (defense in depth; lane comment records this) |
| Release (test/prepare) | `.github/workflows/release.yml` | yes | **migrated**: bespoke `04637d83` step removed (pinned absent), shared step inherited |
| Prepared-env pilot (producer) | `.github/workflows/prepared-environment-pilot.yml` | produces | stamps manifest tuple; consumer jobs use the shared action |
| release-core / release-repair | `release-core.yml`, `release-repair.yml` | no (fresh checkout / API-only) | out of scope; no restored workspace execution |

## Before / after evidence

- **Before:** only `release.yml` purged (one bespoke step, no post-purge verification, no counting);
  every other restore-success path skipped the purge entirely and trusted extraction-timestamped
  `.zo` mtimes.
- **After:** one shared fail-closed purge/verify step executes on every action path in every lane
  above; release lane's bespoke step is pinned **absent** so it cannot silently return beside the
  shared mechanism.
- **Regression proof:** `tests/test-workflow-purge-contract.rkt` reproduces the incident — a
  restored workspace seeded with mtime-newer stale `.zo` executes post-purge **current** bytecode
  (the v1.00.27 cohort-report repro stays green end-to-end), and a **negative fixture** (a
  workflow with a prepared-env restore input but no purge step) turns the repo-wide scan red.

## Contract tests

- `tests/test-workflow-purge-contract.rkt` (new): full workflow-graph scan — every job carrying a
  prepared-env restore input must be purge-covered (own step, shared action, or lane-exempt
  comment); fail-closed on any future unpatched lane; BUG-0065 repro; negative fixture.
- `tests/test-release-workflow-contract.rkt` (extended): shared-action pins (step name,
  `if: always()`, fail-closed post-purge check, `::notice` counting, RUNNER_TEMP stamp), release
  lane migration pins (bespoke step absent), and ci/nightly lane pins.

## Local tooling guidance

Run `racket scripts/run-tests.rkt` only from a clean checkout of the branch under test. If you
copy a workspace, restore a prepared snapshot, or pull a tarball over an existing checkout, purge
first — mtime is **not** a freshness signal:

```bash
find . -type f -name '*.zo' -not -path './tests/metadata-discovery/fixture/*' -delete
find . -depth -type d -name compiled -not -path './tests/metadata-discovery/fixture/*' -exec rm -rf {} +
```

The fixture exception is intentional: `tests/metadata-discovery/fixture/` keeps a tracked stray
`.rkt` inside a `compiled/` directory as pinned input for the metadata-discovery test — never wipe
it. CI semantics are exactly these two commands plus fail-closed verification.

## Residual

None known. BUG-0065 closes as **fixed-systemic**; the invariant is the explicit precondition for
W6 (prepared-env expansion): any new prepared-artifact consumer inherits the purge via the shared
action, and the workflow-graph contract fails a lane that bypasses it.

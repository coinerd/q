# Prepared-Environment Pilot Report — v1.00.11 (W3)

- **Wave:** W3 of `docs/planning/PLAN-v1.00.11-TDD-CI-INTEGRITY-BASELINES.md`
- **Scope:** report-only pilot; **no consumer cutover** in this wave
- **Decision authority for cutover:** this document (see *Decision* below)
- **Rollback switch:** repository variable `RACKET_PREPARED_ARTIFACT=off`

## 1. What was built

Three repository-owned contracts now exist under `.github/actions/`:

| Contract | Role | Invariants enforced |
|---|---|---|
| `install-racket-runtime` | job-local install from the official installer; exact-key installer-archive cache keyed by OS, arch, variant, distribution, version, source URL and **published SHA-256**; archive is checksum-verified on both hit and miss before it ever executes | I8 (no cross-job runtime), exact cache keys only |
| `prepare-racket-environment` | installs locked dependencies, compiles `q` plus `fmt`, packs a **24-hour immutable** artifact containing only `addon-store/`, `q-compiled/` and `manifest.json`; self-verifies its own staged tree before upload | I9, I10 |
| `restore-racket-environment` | downloads via a `needs` producer only, verifies the manifest against this job's tuple **before any test**, then materializes the addon store + compiled output and runs four read-only health checks (`racket --version`, lock verification, `(require quickcheck fmt)`, `raco fmt --help`). Contains no package-mutation or collection-rebuild commands at all — mismatch is always a hard failure | I8, I9, I10 |

The `manifest.json` is emitted and verified by `ci/prepared-environment/manifest.rkt`
(`emit` / `verify` / `digest`) and binds: schema revision, repository, git SHA,
OS, architecture, Racket tuple (version, variant, distribution), the selected
lock digest, the source digest, and the allowlisted path set. The allowlist
universe is fixed inside the tool (`addon-store/`, `q-compiled/`,
`manifest.json`), so a tampered manifest cannot widen its own allowlist.

Pilot workflow: `.github/workflows/prepared-environment-pilot.yml` (report
only; gates nothing). Matrix: Linux 8.10 (full role), Linux 8.11
(producer-only until the W1 lock entry is green), macOS 8.10 (full role).
Negative cases run per tuple: tampered manifest must fail; an artifact
carrying `.git/config` and a home-dir `.netrc` must fail the allowlist;
valid restore must pass all four health checks; `RACKET_PREPARED_ARTIFACT=off`
must reproduce the current per-job `setup-racket` path.

## 2. Local verification (this tree)

| Check | Result |
|---|---|
| `grep -rn "raco pkg install\|raco pkg update\|raco setup" q/.github/actions/restore-racket-environment/` | zero hits (exit 1) |
| `grep -rn "restore-keys" q/.github/actions/` | zero hits (exit 1) — installer-archive and addon caches use exact keys only |
| `raco test q/tests/ci/verify-manifest-test.rkt` (manifest tool suite) | green |
| SHA-256 conformance of `manifest.rkt digest` against `sha256sum` across variable-length vectors | green |
| `setup-racket` call sites in the six workflows | unchanged — W3 performs no cutover |

## 3. Measurement plan and samples

The pilot emits producer duration, artifact size, consumer restore
wall-clock, and cache-hit rate into the run's step summary at
`.github/workflows/prepared-environment-pilot.yml`. Five samples (≥ 3 PR +
≥ 2 main runs) must be transcribed into the table below before any cutover.

| # | Run | Ref | Tuple | Producer (s) | Artifact (MB) | Restore (ms) | Cache hit | Δ runner-min | Critical-path Δ |
|---|-----|-----|-------|--------------|---------------|--------------|-----------|--------------|-----------------|
| 1 | _pending hosted run_ | — | linux/8.10 | — | — | — | — | — | — |
| 2 | _pending hosted run_ | — | linux/8.10 | — | — | — | — | — | — |
| 3 | _pending hosted run_ | — | linux/8.10 | — | — | — | — | — | — |
| 4 | _pending hosted run_ | — | linux/8.10 | — | — | — | — | — | — |
| 5 | _pending hosted run_ | — | linux/8.10 | — | — | — | — | — | — |

Baseline (current per-job path, from the W3 root-cause analysis): every
hosted job repeats job-local runtime installation, q relinking and
package-visible compilation — the cost the artifact removes per consumer job.

## 4. Decision: **NO-GO** (consumers stay on `setup-racket`)

**No consumer cutover is performed in W3.** Reasons, in decision order:

1. **Integrity tooling is complete and locally verified** (Section 2), but
   hosted-run evidence is not yet transcribed (Section 3 has zero recorded
   samples). The go rule requires five samples before cutover.
2. The **W1 version-indexed lock entry for 8.11 is not green in this tree**
   (`ci/racket-package-lock.rktd` still carries the flat 8.10 lock), so the
   8.11 producer pilot is reported as *blocked on W1* rather than run, and
   8.11 consumers remain disabled per the wave contract.
3. The enable rule for Linux 8.10 consumers is two-part: integrity checks
   pass **and** aggregate runner-minute saving exceeds the added producer
   bootstrap delay. Both halves need the hosted samples above.

### Go conditions (all must hold before W4 cutover)

- [ ] Five pilot samples recorded with producer duration, artifact size,
      restore duration, cache-hit rate, aggregate runner minutes and
      critical-path delay.
- [ ] Tampered-manifest and non-allowlisted-path negative cases red-inverted
      green in a hosted run (they fail the artifact, not the workflow).
- [ ] `RACKET_PREPARED_ARTIFACT=off` PR observed identical to a current
      per-job setup PR (same test selection, same L4 semantics).
- [ ] W1 lock entry for 8.11 green; then and only then an 8.11 consumer pilot.
- [ ] Aggregate Linux-8.10 runner-minute saving > producer bootstrap delay.

### Rollback

Repository variable `RACKET_PREPARED_ARTIFACT=off` restores the current
per-job `setup-racket` path for any consumer without touching test selection
or L4 semantics. All 26 existing call sites keep using
`.github/actions/setup-racket/action.yml` as their default path today.

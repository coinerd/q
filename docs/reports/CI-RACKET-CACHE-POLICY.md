# CI Racket Cache Policy

**Status:** Active for the reusable `.github/actions/setup-racket` action.

## Purpose

The full-regression platform lane previously cached `~/.racket` and workspace bytecode even though the macOS Racket 8.10 user package state was stored elsewhere. The result was a cache miss followed by a complete external-package install and global setup. This policy assigns cache ownership to the reusable setup action and preserves q's package-visible compilation gate on every run.

| Concern | Active rule |
|---|---|
| Cache owner | Only `.github/actions/setup-racket/action.yml` restores the Racket package store. Callers must not add a second Racket package cache. |
| Cache path | The action sets `PLTADDONDIR=$HOME/.cache/q-racket-addon` and caches the versioned addon directory. |
| Cache key | `racket-addon-v2`, runner OS, Racket x64/CS/full distribution tuple, Racket version, `info.rkt`, and `ci/racket-package-lock.rktd` form the exact key. |
| Restore policy | No prefix restore keys are permitted. A manifest or lock change produces a clean store. |
| Workspace bytecode | `compiled/` directories are deleted before setup and are never cached. |
| Compile boundary | Default: every run, including an exact cache hit, relinks q and executes `raco setup --no-docs --jobs 4 --pkgs q fmt`, then checks `raco fmt --help`. W3 ubuntu exception: see "Prepared environment" below. |
| Prepared environment (W3) | The ubuntu fast-gate test shards (`.github/workflows/ci.yml` `test` job calling `setup-racket` with `prepared-environment: auto`; producer job `fast-env`) restore the prepared artifact produced by `prepare-racket-environment` (24h immutable: `addon-store/`, `q-compiled/`, `manifest.json`). `restore-racket-environment` verifies the manifest against the job tuple — schema, repo, SHA, OS, arch, Racket tuple, lock digest, source digest — BEFORE any test. A verified restore replaces the install + compile boundary for that job (the restore's four read-only health checks — `racket --version`, lock verify, `(require quickcheck fmt)`, `raco fmt --help` — replace the compile health probe). Any mismatch, missing artifact, or producer failure falls back to the full path above with a `::warning` and a REBUILT step summary — never silently. Rollback switch: repo variable `RACKET_PREPARED_ARTIFACT=off`. |
| Lane split | **ubuntu vs macOS**: only the ubuntu fast-gate lanes may skip compile on a manifest-verified prepared-environment restore (their fallback path still compiles). The macOS platform lanes have NO prepared-environment path: they keep compile-on-every-hit unconditionally per the v1.00.10 release condition. |
| Shard policy | Fast-shard count is governed by repo variable `FAST_SHARD_COUNT` (default `3` = `[0, 1, 2]`; only the study values `4`/`6`/`8` widen the matrix). The report-only study, activation rule, and revert command live in `docs/reports/test-regression-log.md` (`FAST_SHARD_PLAN` precedent). |

> **Binding constraint:** A cache hit accelerates dependency acquisition. It never authorizes skipping compilation of package-visible q modules. The single W3 exception is the manifest-verified prepared-environment restore on ubuntu fast-gate lanes (row above): the artifact was compiled from identical lock + source digests and is verified before any test, with a loudly flagged rebuild fallback. macOS platform lanes have no such exception.

## Dependency lock maintenance

`ci/racket-package-lock.rktd` is the reviewed identity set for external user-scope packages. `ci/verify-racket-package-lock.rkt` compares its package checksums with `raco pkg show --scope user --all --long --full-checksum`. The linked q package is intentionally excluded from the checksum set because its source is the fresh checkout and is relinked on every run.

When `info.rkt` changes or an explicit dependency update is required, maintainers must populate a clean Racket 8.10 user store, review the new external checksums, update the lock file, and run the verifier. A mismatch fails closed; maintainers must not bypass it with a partial restore or `--ignore-checksums`.

## Cache recovery

A missing q package on an exact cache hit, a failed lock verifier, a missing `raco fmt` command, or a failed health probe is a cache-integrity incident. The job must remain red. The repair is to correct the lock or intentionally increment the `racket-addon-v2` schema in the setup action, which creates a new immutable GitHub cache after a successful trusted main, scheduled, or manual run. Do not add `restore-keys` as a workaround.

## Rollout evidence and timeout policy

The 90-minute macOS budget remains in force through the cache rollout. A manual full-regression dispatch on the cache patch must first populate the exact store. A second unchanged dispatch must report an exact cache hit, a valid lock, a passing Racket package health probe, a passing platform suite, and all-lane L4 `pass` evidence. Only after two successful warm runs with measured setup-plus-test time below 25 minutes may maintainers propose a lower platform timeout.

## References

[1]: https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching "GitHub Docs — Dependency caching"
[2]: https://docs.racket-lang.org/pkg/cmdline.html "Racket Package Management — `raco pkg`"
[3]: https://docs.racket-lang.org/pkg/implementation.html "Racket Package Management — package setup implementation"

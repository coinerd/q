# Release Smoke Contract v0.99.41

**Date:** 2026-06-22
**Status:** Active
**Suite:** `release-smoke`

## Purpose

The `release-smoke` suite is a deterministic, artifact-focused test suite used in
post-release verification within the GitHub Release workflow. It verifies that the
installed tarball works correctly without running environment-sensitive or flaky tests.

## Design Principles

1. **Deterministic** — tests must produce the same result on every run
2. **No browser sidecar** — tests must not require Playwright or live browser services
3. **No live network** — tests must not require provider credentials or API access
4. **No global mutation** — tests must not unpredictably mutate repo state
5. **Source-independent** — tests must work from a tarball install, not just source checkout
6. **Release-critical** — tests verify core functionality users depend on

## Inclusion Rules

A test file is included in `release-smoke` if:

1. It has a `;; @suite release-smoke` metadata tag, OR
2. It matches an entry in the curated file list (`release-smoke-curated-files`)

### Curated Files (12 files, ~150 assertions)

| File | Purpose |
|------|---------|
| `tests/test-version.rkt` | Version string correctness |
| `tests/test-safe-mode.rkt` | Safe mode initialization |
| `tests/test-error-classify.rkt` | Error classification |
| `tests/test-mutating-tool-taxonomy.rkt` | Tool mutation classification |
| `tests/test-verifier-gate.rkt` | Verification gate |
| `tests/test-cli-flags.rkt` | CLI flag parsing |
| `tests/test-extension-tiers.rkt` | Extension tier system |
| `tests/test-capability-aware-spawn.rkt` | Capability-filtered spawning |
| `tests/test-frontmatter-extended.rkt` | Skill frontmatter parsing |
| `tests/test-context-assembly-config.rkt` | Context assembly configuration |
| `tests/test-execution-plane-error-label.rkt` | Execution plane error labeling |
| `tests/test-runtime-packages.rkt` | Runtime package system |
| `tests/test-spawn-subagent-serialization.rkt` | Subagent serialization |

## Exclusion Rules

The following categories of tests are **excluded** from release-smoke:

### Browser Tests (CI-unavailable Playwright)

- `tests/test-browser-playwright-sidecar.rkt` — requires Playwright installed
- `tests/test-browser-playwright-adapter.rkt` — requires Playwright installed
- `tests/test-browser-adapter.rkt` — requires browser binary
- All `tests/test-browser-*.rkt` files

### Source-Migration Tests (require source checkout)

- `tests/test-protocol-types-source-migration.rkt` — requires source directory structure
- `tests/test-run-tests.rkt` — requires test runner source files in specific paths

### Slow/Integration Tests

- `tests/test-sandbox*.rkt` — subprocess sandbox (slow)
- `tests/test-subprocess*.rkt` — subprocess spawning (slow)
- `tests/test-integration.rkt` — full integration (slow)

### Mutating Tests

- `tests/test-sync-readme-status.rkt` — mutates README.md
- `tests/test-sync-version*.rkt` — mutates version files

## Relationship to Other Suites

| Suite | Scope | When Used |
|-------|-------|-----------|
| `smoke` | ~19 curated sanity-gate files | Pre-release, every wave |
| `release-smoke` | 12 deterministic artifact-focused files | Post-release verification |
| `fast` | All tests except slow patterns | Main CI, inter-wave gate |
| `broad` | All tests | Final milestone gate |

`release-smoke` is a **strict subset** of `smoke`. Every file in `release-smoke`
must also pass in the `smoke` suite.

## Usage

```bash
# List files in release-smoke
racket scripts/run-tests.rkt --suite release-smoke --inventory

# Run release-smoke tests
racket scripts/run-tests.rkt --suite release-smoke

# Run with profile (for VPS/headless)
racket scripts/run-tests.rkt --suite release-smoke --profile ci
```

## Contract Enforcement

The contract is enforced by `tests/test-release-smoke-contract.rkt` which verifies:

1. Curated files are included
2. Browser/slow/integration tests are excluded
3. #581 failing files are not included
4. @suite release-smoke tag is respected
5. Curated list is bounded (5-20 files)

## Change Policy

Changes to the curated file list require:

1. Update `release-smoke-curated-files` in `scripts/run-tests/classify.rkt`
2. Update this document
3. Run `raco test tests/test-release-smoke-contract.rkt`
4. Run `racket scripts/run-tests.rkt --suite release-smoke` to verify
5. Update the test count in this document

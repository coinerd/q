# Tooling

This document describes the developer tooling scripts in `scripts/`.

## Audit Script (`scripts/audit-project.rkt`)

A static analysis tool that scans the q codebase for architectural health issues.

### Usage

```bash
racket scripts/audit-project.rkt              # Write markdown report to docs/audits/
racket scripts/audit-project.rkt --stdout     # Print markdown report to stdout
racket scripts/audit-project.rkt --ci         # Exit non-zero on critical findings
racket scripts/audit-project.rkt --json       # Print JSON report to stdout
racket scripts/audit-project.rkt --help       # Show usage information
```

### Scanners

| Scanner | Severity | Description |
|---------|----------|-------------|
| Module Inventory | info | Counts modules, lines, and layer distribution |
| Risky API | warning | Detects eval, system, set!, with-handlers misuse |
| Oversized Modules | warning | Flags files exceeding the line threshold |
| Struct Density | warning | Flags files with too many struct definitions |
| TODO Markers | info | Finds TODO/FIXME/HACK markers needing resolution |

### JSON Output

The `--json` flag produces a structured report with:
- `timestamp`, `version` — audit metadata
- `modules` — `{total, total_lines, by_layer}`
- `risky_apis`, `todos`, `oversized`, `high_struct_density` — findings arrays
- `critical_count` — number of critical-severity findings

### CI Integration

Use `--ci` for advisory CI gating. The script exits with code 1 if any critical findings exist. Currently all scanners report at `info` or `warning` level, so `--ci` passes unless critical scanners are added.

## Version Linter (`scripts/lint-version.rkt`)

Checks that documentation files reference the current version from `util/version.rkt`. Auto-fixes stale references when run without flags.

```bash
racket scripts/lint-version.rkt              # Check + auto-fix
racket scripts/lint-version.rkt --check      # Check only (no auto-fix)
```

Excludes historical files: `security.md`, `CHANGELOG.md`, `docs/adr/`, `docs/security.md`.

## Deprecation Linter (`scripts/lint-deprecation-deadlines.rkt`)

Scans for `TODO(#vX.Y.Z)` patterns and flags those where the target version is ≤ current version (expired).

```bash
racket scripts/lint-deprecation-deadlines.rkt          # Print report
racket scripts/lint-deprecation-deadlines.rkt --ci      # Exit 1 on expired TODOs
```

## Dependency Checker (`scripts/check-deps.rkt`)

Validates that module dependencies conform to the layer policy defined in `docs/architecture/dependency-policy.rktd`.

## Format Linter (`scripts/lint-format.rkt`)

Runs `raco fmt --check` on all `.rkt` files to ensure consistent formatting.

## Version Sync (`scripts/sync-version.rkt`)

Synchronizes the version string across `info.rkt`, `README.md`, and other docs from the canonical source in `util/version.rkt`.

## Unified Lint Runner (`scripts/lint-all.rkt`)

Runs all lint/check scripts in a single process. This is the source of truth for CI lint gates.

```bash
racket scripts/lint-all.rkt                    # run all 16 checks
racket scripts/lint-all.rkt --list             # list checks without running
racket scripts/lint-all.rkt --only=format,deps # run subset
```

Checks include: format, version-sync, version-validate, version-cross, protocols, imports, deps, metrics-sync, metrics-lint, prose, readme-status, audit (non-blocking), tests, deprecation, ci-readiness, arch (non-blocking).

## Pre-commit Hook (`scripts/pre-commit.rkt`)

Installs a Git pre-commit hook that runs fast lint checks (delegated to `lint-all.rkt`) and affected tests.

```bash
racket scripts/pre-commit.rkt --install        # install quick hook
racket scripts/pre-commit.rkt --install-full   # install full hook
racket scripts/pre-commit.rkt                  # run quick checks manually
racket scripts/pre-commit.rkt --full           # run all lints + full tests
```

The pre-commit hook delegates lint checks to `lint-all.rkt` so the two never drift.

## Developer Setup (`scripts/setup-dev.rkt`)

One-command bootstrap for new contributors.

```bash
racket scripts/setup-dev.rkt
```

Verifies Racket toolchain, installs pre-commit hook, verifies required scripts exist, and runs a quick lint health check.

## Lint Alignment Checker (`scripts/check-lint-alignment.rkt`)

Verifies that `pre-commit.rkt`'s fast-lint-checks list covers all required (non-optional) checks from `lint-all.rkt`. Used in CI to catch drift.

```bash
racket scripts/check-lint-alignment.rkt
```

## Automation Contract

The following invariant must hold on every commit to `main`:

1. **lint-all.rkt is the source of truth** for all lint checks.
2. **pre-commit.rkt delegates to lint-all.rkt** — it never duplicates check logic.
3. **check-lint-alignment.rkt runs in CI** and fails the build if pre-commit and lint-all drift.
4. **Default pre-commit mode runs all fast checks** (everything except non-blocking audit/arch).
5. **--full mode runs everything** including slow lints and full test suite.
6. **setup-dev.rkt installs the pre-commit hook** so contributors get local validation automatically.

If you add a new lint script, you MUST:
- Register it in `lint-all.rkt`
- Decide if it's fast (required) or slow (non-blocking)
- Add it to `pre-commit.rkt`'s `fast-lint-checks` list if required
- Verify `check-lint-alignment.rkt` passes before pushing

## Test Boundary Tagging Convention

All test files must declare their boundary kind in a `;; BOUNDARY:` comment
on the second line (immediately after `#lang`). This helps reviewers and
automation understand what layer a test exercises.

### Kinds

| Kind | Meaning | Example files |
|------|---------|---------------|
| `pure` | Pure function, no I/O or side effects | `test-foo-pure.rkt` |
| `contract` | Contract validation, error injection | `test-contract-*.rkt` |
| `fsm` | Finite-state machine or state transition | `test-*-fsm.rkt` |
| `integration` | Multi-module integration, round-trips | `test-event-roundtrip.rkt` |
| `io` | I/O, TUI, sandbox, network mocking | `test-tui-*.rkt` |
| `macro` | Macro expansion, syntax tests | `test-macro-*.rkt` |

### Example

```racket
#lang racket
;; BOUNDARY: pure
(require rackunit "../agent/foo.rkt")
```

Automation may use this tag to select test subsets (e.g. run only `pure`
tests for fast feedback).


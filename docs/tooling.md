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

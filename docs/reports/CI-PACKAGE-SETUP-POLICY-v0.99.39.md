# CI Package-Visible Script Policy (v0.99.39)

**Status**: Active
**Created**: v0.99.39 (W4, #8507)
**Related**: [CI-GitHub-Actions Permanent Remediation Plan](../../.planning/PLAN-v0.99.39-CI-GITHUB-ACTIONS-PERMANENT-REMEDIATION.md)

## 1. Problem Statement

The `raco pkg install` / `raco pkg update` step in CI (via the
`setup-racket` composite action) compiles **all package-visible modules**.
This is a broader compile boundary than `raco make main.rkt` or the test
suite, which only compile modules in their respective dependency graphs.

In v0.99.40, four scripts/CLI modules had compile errors that were
invisible to local development gates but broke every CI run:

| ID | Module | Root Cause |
|----|--------|------------|
| F1 | `cli/generate-certificates.rkt` | Missing `racket/string` require |
| F2 | `scripts/sdk-gsd-integration-test.rkt` | Duplicate `state-machine.rkt` import |
| F3 | `scripts/test-gsd-go-replanning.rkt` | Stale API (`gsd-mode` → `current-gsd-mode`) |
| F4 | `scripts/test-gsd-sdk-live.rkt` | Unnecessary `cancellation.rkt` import |

These were fixed in W1 (#8504). This policy prevents recurrence.

## 2. Core Policy

### Rule 1: Everything package-visible must compile

Every `.rkt` file under a package-visible directory (`scripts/`, `cli/`,
`tools/`, `extensions/`, `runtime/`, `interfaces/`, `agent/`, `gui/`,
`tui/`, `util/`, `sandbox/`, and project root) **must compile** under
`raco make` without errors.

**Enforcement**: The CI `setup-racket` composite action runs `raco pkg
install` / `raco pkg update`, which triggers `raco setup` to compile all
package-visible modules. Any compile error fails the entire CI pipeline.

**Local preflight**: `racket scripts/ci-package-setup.rkt --preflight`

### Rule 2: No compile-time side effects

Package-visible modules must not perform live effects at compile time
(no top-level network calls, file writes, or process spawns outside of
`module+` submodules).

**Rationale**: `raco setup` compiles modules during package installation.
Side effects at the module top level would execute during CI setup,
causing unpredictable behavior.

### Rule 3: Imports must be explicit and current

All `require` statements must reference modules that exist and export the
needed identifiers. Stale references (renamed APIs, removed modules,
duplicate imports) cause compile failures.

**Enforcement**: The package setup compile gate catches these at CI time.
The local preflight (`--preflight`) catches them before push.

### Rule 4: Live/manual scripts stay in `scripts/` but must compile

Scripts that perform live effects (provider calls, integration tests,
dogfood sessions) remain in `scripts/` for discoverability. However, they
must satisfy Rules 1-3: they compile, have no compile-time side effects,
and their imports are current.

Live effects must be guarded behind `module+ main` or function calls,
never at the module top level.

## 3. Script Classification

Package-visible scripts are classified as follows:

| Category | Description | Count | Examples |
|----------|-------------|-------|----------|
| **CLI entry points** | User-facing CLI modules | 8 | `cli/args.rkt`, `cli/render.rkt` |
| **CI/release gate scripts** | Lint, version, release checks | 19 | `scripts/lint-version.rkt`, `scripts/ci-local.rkt` |
| **CI/release tooling** | Version bump, manifest, sync | 10 | `scripts/bump-version.rkt`, `scripts/sync-version.rkt` |
| **Test runner infrastructure** | Test discovery, execution, reporting | 10 | `scripts/run-tests.rkt`, `scripts/run-tests/*.rkt` |
| **Benchmark infrastructure** | Benchmark runners and fixtures | 19 | `scripts/benchmark/*.rkt`, `scripts/bench-*.rkt` |
| **Manual/diagnostic scripts** | Integration tests, live scripts | 6 | `scripts/sdk-gsd-integration-test.rkt`, `scripts/test-gsd-*.rkt` |
| **Reporting/audit scripts** | Architecture, metrics, hotspot reports | 22 | `scripts/abstraction-audit.rkt`, `scripts/metrics.rkt` |
| **Utility scripts** | Setup, cleanup, formatting | 3 | `scripts/setup-dev.rkt`, `scripts/clean-compiled.rkt` |

**Total**: 97 package-visible script/CLI modules.

## 4. Enforcement Mechanisms

### Local (developer machine)

```bash
# Fast preflight: compile all package-visible modules
racket scripts/ci-package-setup.rkt --preflight

# Full CI-equivalent: isolated raco pkg install
racket scripts/ci-package-setup.rkt --full

# Integrated into ci-local.rkt
racket scripts/ci-local.rkt --package-setup   # preflight
racket scripts/ci-local.rkt --github-setup    # full
```

### CI (GitHub Actions)

The `setup-racket` composite action (`.github/actions/setup-racket/action.yml`)
runs `raco pkg install` / `raco pkg update` as part of every CI job.
This compiles all package-visible modules and fails the pipeline on any
compile error.

Diagnostic logging (added in W3, #8506) makes the compile boundary
explicit with version output, package state, log grouping, and
`::error::` annotations pointing to the local reproduction command.

### Automated test coverage

- `tests/test-ci-package-compile-boundary.rkt` (W1): Verifies the 4
  previously-broken modules compile via `raco make` subprocess.
- `tests/test-ci-package-setup.rkt` (W2): Tests command construction,
  failure classification, and module discovery for the package setup gate.
- `tests/test-ci-workflow-diagnostics.rkt` (W3): Verifies CI workflow
  diagnostic naming and logging.

## 5. Adding New Scripts

When adding a new `.rkt` file under a package-visible directory:

1. **Ensure it compiles**: Run `raco make <new-file.rkt>` before committing.
2. **No compile-time side effects**: Put live effects behind `module+ main`
   or function definitions, never at the module top level.
3. **Explicit imports**: Only require modules that exist and export the
   needed identifiers. Avoid wildcard imports where possible.
4. **Run preflight**: `racket scripts/ci-package-setup.rkt --preflight`
   before pushing to verify the new module doesn't break the package
   compile boundary.

## 6. Historical Context

- **v0.99.40**: CI broke on main due to 4 package-visible compile errors
  invisible to local gates. All CI runs failed at the `setup-racket`
  step, skipping all downstream jobs (lint, test, security, etc.).
- **v0.99.39 W0** (#8503): Baseline documentation of the failure pattern.
- **v0.99.39 W1** (#8504): Fixed the 4 compile errors (F1-F4).
- **v0.99.39 W2** (#8505): Added local CI-equivalent package setup gate.
- **v0.99.39 W3** (#8506): Made CI workflow diagnostics explicit.
- **v0.99.39 W4** (#8507): This policy document and script inventory.

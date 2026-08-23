# Test Helpers

Shared helpers for the test suite. Files in this directory are test
infrastructure, not product code — keep them dependency-free of
`runtime/` where possible and never import a test file from here.

## Isolation conventions (introduced in W2)

The test runner executes files as concurrent subprocesses when `--jobs > 1`
(see `scripts/run-tests.rkt`). A test file must therefore own every shared
surface it touches:

1. **Temporary state** — use `helpers/temp-fs.rkt`
   (`with-temp-dir` / `with-temp-file`). These macros guarantee
   creation *and* deletion via `dynamic-wind`, including on exceptions.
   Never call bare `make-temporary-file` with success-path-only cleanup:
   aborted or timed-out runs leak scratch dirs that later parallel runs
   race on.
2. **Repository tree** — never read or mutate checked-in fixtures
   in-place. Copy the fixture into a per-test temp dir first (see
   `test-run-tests-ledger.rkt`, which copies
   `tests/test-suite-ledger.json` before exercising the CLI against it).
3. **Environment / cwd** — do not `putenv` or set `current-directory`
   without restoring. Prefer passing paths/values as explicit arguments
   to the code under test.
4. **Ports / singletons** — allocate ephemeral ports per test; construct
   event buses and loops inside test-local scope rather than at module
   level.

## Retained exemptions

These files carry explicit `@isolation` metadata so the runner schedules
them serially (ahead of the parallel batches); each exemption is owned
and has a documented reason here:

- `tests/test-run-tests-ledger.rkt` — `@isolation process` (canonical value;
  the `subprocess` spelling is a deprecated alias for `process`). It spawns
  `racket scripts/run-tests.rkt` as a child process, and that script's
  startup performs repo-wide stale-bytecode cleaning
  (`scripts/run-tests/classify-filters.rkt`). Concurrent runs would
  delete compiled artifacts under each other's feet — a repository-tree
  shared surface that cannot be isolated from inside the test. The test
  itself operates on a copied fixture ledger in a temp dir and exits
  non-zero when rackunit reports any failure or error.

No file is exempt merely for being flaky; exemptions require a shared
surface that genuinely cannot be isolated, plus an entry in this list.

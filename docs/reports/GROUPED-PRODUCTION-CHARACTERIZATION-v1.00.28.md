# Grouped Production Characterization — v1.00.28 W4

Wave: v1.00.28-w4 (#9629) · Branch: `campaign/v1.00.28-w4` (base `04637d83`)
Artifacts: `artifacts/test-runtime/v1.00.28-w4/` (checksummed by `SHA256SUMS`)

## Question

How much of the real production test inventory can safely execute in persistent
Racket workers (grouped-in-process) instead of one-process-per-file subprocesses?

## Method

The first production cohort (14 files) was drawn from the W0 Q7 grouped-candidate
queue plus its adjacent fast-tier families. Each file was classified with
`scripts/run-tests/grouped-eligibility.rkt` (`tests/metadata/classification`,
pipe-delimited sidecar) using the wave's fail-closed criteria: GROUP-SAFE
requires no process-global mutation, no unbounded background threads, no
intentional exit, no signal manipulation, parameterized-and-restored cwd/env,
private temp state, reset-or-immutable module globals, and randomized-order
stability. Process-lifecycle, signal, hostile-environment, process-global-state,
executable-startup, and fresh-VM-dependent tests are ISOLATED by rule.

For every GROUP-SAFE candidate the full characterization matrix was executed with
`runner.rkt` primitives: `isolated` (subprocess), `grouped` (in-process),
`grouped-repeat`, `grouped-random` (shuffled family order), and
`grouped-concurrent` (parallel run-all-files). Cells compare pass/fail, exit
code, parsed test counts, order independence, repeat stability, wall clock, and
leak checks (cwd/env restoration, sentinel probe).

## Classification result (14 files)

| Decision | Files | Notes |
|---|---|---|
| GROUP-SAFE | 5 | `test-context-assembly-budgeting`, `test-approval-channel`, `test-arch-composition-root`, `test-compaction-command`, `test-ci-package-setup-policy` — unit boundary, no declared mutation, parity complete in all 5 modes |
| ISOLATED | 4 | zero-parsed rackunit output (cannot prove parity) |
| ISOLATED | 1 | declared `sandboxed-home` mutation (`test-test-lint.rkt`) |
| ISOLATED | 3 | declared process/fs mutation (slow integration: `test-check-deps`, `test-metrics-readme-sync`, `test-pre-commit`) |
| ISOLATED | 1 | runner-gate structural form (`test-agent-loop-fsm.rkt`) |

## Parity matrix

25/25 cells pass (`parity-matrix.json`): every GROUP-SAFE file passes isolated
and in all four grouped modes with identical test counts, `order-independent`
and `repeated-stable` true, no leaks. Randomized cohort order recorded in the
matrix. Grouped wall clock is consistently 2–4× faster per file than subprocess
isolation (e.g. budgeting 642 ms → 270/208/184 ms; approval-channel 472 ms →
111 ms).

## Decision-relevant numbers

- **group-safe fast files: 5/14 (35.7%)**
- **group-safe fast work mass: 3,032 / 118,106 ms (2.57%)** — measured isolated
  wall clock of the production fast files standing in for the W0 census
  `median_ms` (report: `group-safe-report.json`)

The work-mass share is the decision number: the large slow-integration families
(`test-pre-commit` alone is ~70% of cohort mass) dominate the fast suite and are
ISOLATED by rule. Even perfect grouped adoption of every eligible fast file
moves total work mass by only a few percent at this cohort scale.

## Fail-closed defaults and rollback

`decide-eligibility` applies strict precedence: `rollback-switch` →
`unclassified` → `classified-isolated` → `parity-missing` → `parity-failed` →
`grouped`. Any unclassified file, incomplete or failing parity evidence, or the
`--mode subprocess` rollback switch (also settable via runner config) forces
subprocess execution. Default execution mode is unchanged (subprocess); no
activation is performed in this wave — the 5 parity-complete files are
activation-eligible for a later wave only with inventory-unchanged proof, and
the rollback switch is contract-tested in `tests/test-runner-work-queue.rkt`.

## Contract tests

- `tests/test-grouped-eligibility.rkt` — 14 checks: fail-closed rows, parity
  completeness vs. passing separation, report math.
- `tests/test-runner-work-queue.rkt` — 11 checks incl. fail-closed default and
  rollback-switch restoration of isolated execution.

## Artifacts

- `artifacts/test-runtime/v1.00.28-w4/parity-matrix.json` — 25 cells + leak checks
- `artifacts/test-runtime/v1.00.28-w4/group-safe-report.json` — file % and mass %
- `artifacts/test-runtime/v1.00.28-w4/SHA256SUMS` — `sha256sum -c` verifies both
- `tests/metadata/classification` — declared cohort classification sidecar

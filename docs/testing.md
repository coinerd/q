# Developer Testing Quick-Start

`./qt` is the local test entry point. The documented everyday workflow is the
four-command loop: focused `quick`, `quick --changed`, `full --file`, and
`default` (measured costs and the no-owner gap path below). Every command
states on its pass line what it does **not** prove, `--explain` prints the
selection without executing anything, and unsupported options fail closed
before any selection or execution. Day-to-day conventions (suites, metadata tags, flaky-test policy):
[`TEST_CONVENTIONS.md`](TEST_CONVENTIONS.md). Strategy and budget history:
[`TDD-TEST-STRATEGY-PLAN.md`](TDD-TEST-STRATEGY-PLAN.md) and the measured W6
budgets below (v1.00.28).

## Commands

| When | Command | What a pass establishes |
|---|---|---|
| Every red-green cycle | `./qt quick` | The audited pure-unit owner table runs green. Reproduction only — never PR verification. |
| Before push | `./qt quick --changed BASE..HEAD` | The impact-selected audited owners for that diff run green. Reproduction only — never a PR pass. |
| Pre-PR | `./qt default` | Complete *offline* PR verification: the required-offline lane (union of the suites the required CI test jobs select). Platform and GitHub-hosted claims remain with CI. |
| Complete available suite | `./qt full` | Every discovered test file (full Racket suite only, not PR/CI verification). |

- `./qt quick` runs all admitted owners; `./qt quick tests/test-foo.rkt` runs
  one admitted file (a non-admitted file is refused). Failures print per-file
  evidence plus `Reproduce: ./qt quick <file>` and exit non-zero.
- `./qt quick --changed BASE..HEAD` maps the diff through the `@covers`
  manifest and the dependency graph onto the audited owners. Unattributable
  owners are INCLUDED (fail open) and an escalation widens the selection to
  the whole audited lane, never narrower. An empty diff or an empty selection
  is a failure, not a pass.
- `./qt default --explain` prints the claim → lane → suite mapping (required
  CI test job → lane → suite), the detected host capabilities, the file
  selection, and the honesty lists — selection only, nothing executed.
- `./qt --lane NAME` runs one CI lane over an existing suite/shard spec (the
  valid names are listed by `./qt --help`). `--lane required-offline` names
  the suite union behind `./qt default` and refuses on the CI surface (a CI
  job runs one suite) — resolve it with `./qt default`.
- `./qt release --candidate SHA` verifies an exact release candidate (see
  below).

Exit behavior is uniform: exit 0 only with a pass line, non-zero on every
other outcome (failed test, missing or invalid result, managed-surface change,
refused selection). Each pass line embeds its own honest label —
`quick passed: N files, M tests (not PR/full verification)`,
`default passed: N files, M tests (offline PR verification; K unavailable-here
listed for CI; …)`, `full passed: N files, M tests (full Racket suite only,
not PR/CI verification)` — and the run output prints `NOT VERIFIED` /
`NOT PROVIDED` lists so the gap stays visible.

### The everyday loop

The documented day is four commands; each one below was executed against the
current `./qt` before this text was written, and the measured costs are warm
samples from the reference machine with their sample counts — recorded runs,
not promises. Targets come from the testing-acceleration plan; both misses
are recorded outcomes and stay here until the next measurement beats them.

1. **Red-green cycle** — `./qt quick <file>` runs exactly one admitted owner
   (`./qt quick tests/test-ansi.rkt`); a non-admitted file is refused. This
   is the innermost loop while editing.
2. **Diff reproduction** — `./qt quick --changed BASE..HEAD` maps the diff
   onto the audited owners through the attribution rules above and runs the
   selected owners.
3. **One-file reproduction on a PR lane** — `./qt full --file <file>` (or
   `./qt default --file <file>`) runs exactly that file through the lane
   machinery; its pass line says `explicit reproduction only`, never a PR
   pass.
4. **Pre-push** — `./qt default` is the complete offline PR verification;
   today it selects 1378 files (`./qt default --explain` prints the
   selection without executing anything).

| Loop stage | Measured (warm, reference machine) | Target |
|---|---|---|
| Focused red-green (`./qt quick <file>`) | p50 4.7 s, p90 5.1 s — 20 samples over two small owners (tests/test-ansi.rkt, 23 tests; tests/test-agent-state.rkt, 7 tests) | ≤ 5 s — **missed by ~0.1 s end-to-end**: the test child itself runs 0.9–1.6 s (the runner's own RUN-SUMMARY wall-clock telemetry, observed via `./qt full --file`), the remainder is qt CLI startup plus the admission re-verification |
| Impact reproduction (`./qt quick --changed BASE..HEAD`) | p90 37.7 s, range 35.1–37.7 s — 5 samples over a real 17-file diff (f7b8e62d..HEAD, the WA/B1–B3 test delta; 3 config escalations widened the selection to the whole audited lane) | ≤ 30 s — **missed**: the fail-open floor below makes a typical selection the whole audited loop (≈ 35 s for 28 files / 325 tests), so impact selection currently saves only the attributed owners |
| Complete audited loop (`./qt quick`) | ≈ 35 s (28 files / 325 tests) | — |

**When the change has no audited owner.** `quick --changed` never answers a
gap with a zero-test green. The verified behaviors:

- A changed production module that no `@covers` mapping reaches (directly or
  transitively) is reported as a named escalation —
  `unmapped-source <file>: no @covers mapping reaches this changed module
  (direct or transitive)` — and the whole audited lane is included (fail
  open): the diff is reproduced by the complete audited loop, never by
  nothing. Demonstrated on an untracked scratch module, then removed.
- Owners with no `@covers` attribution of their own ride along in every
  selection (fail open) — 25 of the 28 current owners — so the selection
  can never drop below them.
- If the selection would be empty (reachable only once every owner carries
  attribution and none is affected — not the case today), the command fails
  with `changed selection is empty: …` and exits non-zero; the same
  fail-closed rule lives in the qt execution path, not just in the report.
- The escalation's fallback recommendation is at suite granularity
  (`fallback_suites` in the impact JSON; the legacy impact report prints it
  as `→ fallback: <suites>`). Naming a per-file integration owner for an
  unmapped module is **not** implemented today — the gap is explained and
  widened, not silently owned.

## Audited admission (`quick`)

`quick` executes only the reviewed owner table with its generated admission
record (`tests/.quick-admission.rktd`): a versioned capability-denylist scan
over each owner and its reviewed closure gates admission.
`./qt quick --audit` prints the live report — `admitted`, `review-required`,
`rejected`, with reasons (28 owners / 325 tests at the time of writing). A
stale record, a closure-structure change, or a denylist hit is
`review-required`; `quick` refuses to run such an owner, and re-checks
admission after the run so a mid-run source change cannot slip through.

Expansion status (testing-acceleration Phase D): the B1–B3 re-tier waves
produced fast *contract* owners
(`tests/test-gsd-auto-review-loop-contract.rkt`,
`tests/test-gsd-auto-review-production-contract.rkt`,
`tests/test-gsd-generated-metadata-correction-contract.rkt`) that are
`@boundary integration` with real Git/process worlds — each carries a
denylist-v1 capability hit (`make-temporary-file`, `delete-file`), so none
is admissible. No genuinely pure extracted decision owner from B1–B3 exists
in the tree yet, so no admission expansion happened: the audited set stays
28 owners / 325 tests. A future expansion must arrive as pure decision code
through this same reviewed scan-plus-record path — a metadata tag alone
admits nothing, and integration contracts stay out of `quick` even when
they carry `@speed fast`.

## Required-offline lane and `unavailable-here`

`./qt default` selects the union of the `fast`, `security`, `workflows`,
`smoke` and `platform` suites — the lanes the required CI test jobs select.
Slow mandatory owners are included through their suite. A file whose declared
`@requires` this host cannot satisfy is not excluded: it is listed as
`unavailable-here` — counted separately on the pass line, never verified,
never blocking. CI stays authoritative for exactly those members.

## Release candidates

`./qt release --candidate SHA` verifies the exact candidate checkout (wrong or
dirty candidates are refused), runs the full suite, and prints every candidate
obligation with a stable slug and state: `governance-parity` (verified by the
local suite run), `ci-verification` (verified only by `--evidence`),
`branch-protection` and `release-provenance` (attestation).
`--evidence <proof-bundle|artifact-dir>` is the only offline CI evidence
input, bound to the exact candidate SHA; absent, malformed, or wrong-SHA
evidence is `unavailable`, never inferred. The command exits non-zero while
any obligation is unverified. Honest limitation: `branch-protection` and
`release-provenance` have no attestation input format yet, so they stay
unavailable and block a candidate-ready verdict today.

## Build-once preparation (`default` / `full` / `release`)

Lane runs compile the selected dependency closure once per run with a standard
`raco make` before any worker dispatches, guarded by a per-run content
identity: hash → make → re-hash. Any mismatch aborts before execution — no
partial verdict — and a preparation failure fails the run closed. The
telemetry line `;; run-tests: preparation ok — targets=… closure=… excluded=…
identity=… (… ms)` reports what was prepared; a selection below the
substantial-work threshold (64 closure modules) is skipped with its reason.
`Q_SKIP_PREPARATION=1` skips preparation (the measurement escape hatch). `./qt quick` keeps its
source-only path: it never prepares and never uses repository bytecode.

## Measured budgets (measured on the legacy CLI)

The numbers below were measured against the legacy `scripts/run-tests.rkt`
CLI (v1.00.28 W6) and are kept for budget context only; `./qt` is the
supported interface and `./qt quick --changed BASE..HEAD` is the supported
impact story.

| What | Legacy command | Measured |
|---|---|---|
| Red-green single file | `racket scripts/run-tests.rkt tests/test-foo.rkt` | p90 2.0 s (target ≤ 5 s ✅) |
| Direct impact over a diff | `racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD` | 52.7 s on the W6 sample file (miss attributed to the selected file's own runtime, not selection ≈ 2 s) |
| Transitive selection preview | `racket scripts/run-tests.rkt --impact-dry-run --explain` | selection ≈ 8 s (target ≤ 120 s ✅) |
| Broad fast tier | CI fast-gate lane | p50 627 s |

The former L1/L2 impact levels of this guide are retired; their legacy flags
(`--changed-base/--changed-head`, `--impact-dry-run`, `--prioritize impact`)
remain available through the adapter below for the consumers that still
require them. Rules of thumb that survive the rename: never start a red-green
cycle with a broad lane — `./qt quick` on the code you are editing is the
≈35 s audited loop (see the everyday-loop table above), the full/default
lanes are 10+ minutes; preview an impact selection with `--explain` before
executing it.

## Legacy runner adapter (`scripts/run-tests.rkt`)

`scripts/run-tests.rkt` is kept as a compatibility adapter only; new work
goes through `./qt`. The consumers that still require it, each of which must
be repointed before any deletion of the adapter:

- `scripts/pre-commit.rkt` — the pre-commit hook runs the full suite through
  it directly;
- `extensions/gsd/effective-verify-binding.rkt` — the approved Verify-chain
  grammar names `racket scripts/run-tests.rkt` stages (focused file and
  `--suite fast`);
- `extensions/gsd/evidence-runner-coverage.rkt` — authenticates the runner's
  trusted closure starting at `scripts/run-tests.rkt` and recognizes its
  focused-runner stage grammar;
- `scripts/testing/changed-selection.rkt` — writes the legacy
  `racket scripts/run-tests.rkt --generate-covers-manifest` command as the
  coverage manifest's `generated_by` provenance string.

Machine-readable W6 evidence: `artifacts/test-runtime/v1.00.28-w6/` (SHA-256 checksummed), report [`reports/LOCAL-TDD-LATENCY-v1.00.28.md`](reports/LOCAL-TDD-LATENCY-v1.00.28.md) (v1.00.28).

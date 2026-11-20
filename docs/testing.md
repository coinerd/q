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
| Before push (committed work) | `./qt quick --changed BASE..HEAD` | The impact-selected audited owners for that diff run green. Reproduction only — never a PR pass. |
| Before push (working tree) | `./qt quick --changed HEAD` | The same selection for the working-tree diff against a single base ref: staged, unstaged, untracked, added, deleted and renamed files all count. Reproduction only — never a PR pass. |
| Pre-PR | `./qt default` | Complete *offline* PR verification: the required-offline lane (union of the suites the required CI test jobs select). Platform and GitHub-hosted claims remain with CI. |
| Complete available suite | `./qt full` | Every discovered test file (full Racket suite only, not PR/CI verification). |

- `./qt quick` runs all admitted owners; `./qt quick tests/test-foo.rkt` runs
  one admitted file (a non-admitted file is refused). Failures print per-file
  evidence plus `Reproduce: ./qt quick <file>` and exit non-zero.
- `./qt quick --changed` takes a commit range (`BASE..HEAD`) or a single base
  ref (`HEAD`, `origin/main`, …). A single base ref is the working-tree path:
  it diffs `BASE` against the working tree (`git diff --name-only BASE`, so
  staged and unstaged changes count) plus untracked files
  (`git ls-files --others`), and a rename appears as its delete+add pair (the
  diff is computed `--no-renames`, so both paths select). Deleted files are
  selected too. Both forms map through the SAME `@covers` manifest and
  dependency graph onto the audited owners — direct attribution, transitive
  graph impacts, config/global escalations and unmapped-source escalations
  behave identically; only the report label differs
  (`working-tree diff against BASE` vs `commit range BASE..HEAD`).
  Unattributable owners are INCLUDED (fail open) and an escalation widens the
  selection to the whole audited lane, never narrower. An empty diff or an
  empty selection is a failure, not a pass.
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
not promises. Targets come from the edit-loop review (7002474250): every miss
below is a recorded outcome and stays here until the next measurement beats
it. Full sample tables, selected owners and baseline setup:
[W2-EDIT-LOOP-MEASUREMENT (v1.00.33 W2)](reports/W2-EDIT-LOOP-MEASUREMENT-v1.00.33.md).

1. **Red-green cycle** — `./qt quick <file>` runs exactly one admitted owner
   (`./qt quick tests/test-ansi.rkt`); a non-admitted file is refused. This
   is the innermost loop while editing. If the edit touched a file in the
   owner's reviewed closure (the test itself or any dependency it requires),
   refresh the admission record first (below) — a stale record refuses the
   owner until you do.
2. **Diff reproduction** — `./qt quick --changed BASE..HEAD` maps the
   committed diff onto the audited owners through the attribution rules above
   and runs the selected owners; `./qt quick --changed HEAD` does the same
   for uncommitted working-tree edits (including untracked scratch files).
3. **One-file reproduction on a PR lane** — `./qt full --file <file>` (or
   `./qt default --file <file>`) runs exactly that file through the lane
   machinery; its pass line says `explicit reproduction only`, never a PR
   pass.
4. **Pre-push** — `./qt default` is the complete offline PR verification;
   today it selects 1379 files (`./qt default --explain` prints the
   selection without executing anything).

| Loop stage | Measured (warm, reference machine) | Target |
|---|---|---|
| Focused red-green (`./qt quick <file>`) | pooled p50 2.0 s, p90 5.0 s — 21 samples over three small owners (tests/test-ansi.rkt 23 tests p90 2.2 s; tests/test-agent-state.rkt 7 tests p90 2.0 s; tests/test-gsd-campaign-decision-pure.rkt 6 tests p90 5.4 s) | ≤ 3 s — **missed** on the pooled p90: 17 of 21 samples are ≤ 2.2 s; the GSD owner misses because its fresh child recompiles a `typed/racket` closure from source each run (source-only loading), ≈ 3 s of child-side compile alone |
| Committed attributable impact (`./qt quick --changed BASE..HEAD`) | per-area p90 12.5 s (TUI, 9 owners), 13.9 s (GSD, 9 owners), 25.4 s (agent, 11 owners) — 5 samples each | ≤ 15 s — **missed** for the agent hub sample: `agent/state.rkt` is a dependency hub, so the graph correctly fans out to 11 owners, two of which are heavy (≈ 13 s and ≈ 5.6 s alone) under source-only child loading |
| Uncommitted attributable impact (`./qt quick --changed HEAD`) | per-area p90 12.5 s (TUI, 9 owners), 13.1 s (GSD, 9 owners), 25.2 s (agent, 12 owners) — 5/2/2 samples | ≤ 15 s — same verdict as the committed row: the fan-out, not the diff input, is the cost |
| Admission refresh after an ordinary content edit | p50 1.6 s, p90 2.1 s — 5 samples of the production record (23 owners) | — (measured; a one-time step per edit that touches a closure file) |
| Deliberate broad fallback (`./qt quick` after an escalation) | p50 22.4 s, p90 23.1 s — 3 samples (23 files / 228 tests) | — (reported separately, as required: an escalation runs the whole audited lane) |
| Complete audited loop (`./qt quick`, no diff) | ≈ 22 s (23 files / 228 tests) | — |

An edit to an admitted owner's closure must be followed by one admission
refresh before `quick` runs that owner again:

```sh
racket -e '(begin (require (file "scripts/testing/quick-audit.rkt")) (refresh-record!))'
```

The refresh re-scans every owner and its closure and rewrites
`tests/.quick-admission.rktd`; it is a pure function of the tree (re-running
it on an unchanged tree reproduces the committed bytes byte-identically), so
it lands as a reviewed, committed diff in the same commit as the edit.
Content-only changes re-admit automatically: the refreshed entry keeps the
same closure and the same verdict, and only the edited file's hash moves
(pinned by tests/test-testing-audit.rkt). A structure change or a capability
denylist hit still records review-required after a refresh — the refresh
cures staleness, never a capability question, which needs a human disposition
in the reviewed table. `quick` re-checks admission after the run too, so a
source change during the run cannot turn into a pass line (the evidence is
fail-closed on the executed identity).

**When the change has no audited owner.** `quick --changed` never answers a
gap with a zero-test green. The verified behaviors:

- A changed production module that no `@covers` mapping reaches (directly or
  transitively) is reported as a named escalation —
  `unmapped-source <file>: no @covers mapping reaches this changed module
  (direct or transitive)` — and the whole audited lane is included (fail
  open): the diff is reproduced by the complete audited loop, never by
  nothing. Demonstrated on an untracked scratch module, then removed.
- Owners with no `@covers` attribution of their own ride along in every
  selection (fail open) — 2 of the 23 current owners, all deliberate:
  - `tests/test-agent-memory-decoupling.rkt` is a recursive source-scan
    owner over layer directories (`runtime/`, `tui/`, `extensions/`, `llm/`,
    `agent/`, `util/`); a fixed module list would silently miss every newly
    added file, so it stays global on purpose;
  - `tests/test-agent-session-config.rkt` exercises a pure language pattern
    (mutable-hash regression) with no production module required at all.
  The other 21 owners carry `@covers` attribution (the modules each directly
  requires, or — for the browser-close contract regression — the module whose
  contract it pins), so a change to one of those modules selects exactly its
  owners and the fail-open floor stays small. Attribution claims are checked
  against the live manifest by tests/test-docs-accuracy.rkt.
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
`rejected`, with reasons (23 owners / 228 tests at the time of writing; the
audit view scans every candidate and costs ≈ 30 s). A
stale record, a closure-structure change, or a denylist hit is
`review-required`; `quick` refuses to run such an owner (exit 1), and
re-checks admission after the run so a mid-run source change cannot slip
through. The executed identity is fail-closed: result evidence carries the
executed path, and a non-#f staleness/identity reason invalidates the result
no matter what exit code or counts the child reported. Per-run admission is
cheap (the CLI startup plus a selected-owner re-check is ≈ 0.9 s warm for one
file, ≈ 3 s for a nine-owner `--changed HEAD --explain`), so the gate is
checked before AND after every run, not sampled.

Expansion status (testing-acceleration Phase D): the B1–B3 re-tier waves
produced fast *contract* owners
(`tests/test-gsd-auto-review-loop-contract.rkt`,
`tests/test-gsd-auto-review-production-contract.rkt`,
`tests/test-gsd-generated-metadata-correction-contract.rkt`) that are
`@boundary integration` with real Git/process worlds — each carries a
denylist-v1 capability hit (`make-temporary-file`, `delete-file`), so none
is admissible. No genuinely pure extracted decision owner from B1–B3 exists
in the tree yet, so no admission expansion came from those waves. The W5
bounded pure pilot then added the first post-expansion owner
(`tests/test-qt-quick-run-classification.rkt`, its two closure capability
hits reviewed in `reviewed-hits`); R3c then extracted the pure
`delivery-outcome` module and admitted its focused owner with zero hits;
R3d (#9948) retired the recursive arch source-scan owners and the v09945
archival audit owners from `quick` — they keep their tier and `default`
selection, so full CI still runs them.
The audited set is now 23 owners / 228 tests. A future expansion must arrive as pure decision code
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

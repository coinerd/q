# W4b — CLI Contract Simplification (v1.00.33)

Review comment `7002475553` (milestone #900, wave W4b of #9952). Mandate: audit
`tests/test-qt-ci-surface.rkt`, `tests/test-qt-quick.rkt` and
`tests/test-qt-lanes.rkt` together; keep authentic CLI entrypoint contracts for
every unique CLI surface; move deterministic assertions to the actual
production pure decision owner (never a copied implementation); eliminate only
demonstrated redundant subprocess launches. No generalized rewrite, no file
removals. Baseline commit: `2ca8d0bb` · Worktree: `wt-900-w4b` · Branch:
`feat/9952-w4b-cli-contracts`. Tests only; `qt`, `scripts/`, production
source and W4a fixture helpers untouched (`git diff` on protected paths is
empty).

## 1. Case map (stimulus → production path → disposition)

Every case in the three files was classified by stimulus, production path,
invariant and detectable defect. The disposition of every case is KEEP, MOVE
(subprocess → pure owner call), MERGE (duplicate execution → one), or DROP
(duplicate contract → surviving pin).

### tests/test-qt-ci-surface.rkt (30 → 17 qt launches)

| Case | Production path | Disposition |
|---|---|---|
| `--json-out` schema + `extra.qt` marker | qt → `resolve-qt-ci-plan` → `run-qt-ci-plan` → `reporting.rkt` | KEEP subprocess (authentic end-to-end runner schema) |
| `--file` explicit-reproduction label (pass line + JSON) | same | KEEP subprocess |
| positional files rejected, `--file FILE` named | qt argv parse + dispatch | KEEP `full` variant; DROP `run` variant — the unknown-COMMAND refusal it hit is pinned WITH its message (`only quick, full, default and release are available`) in test-qt-lanes.rkt's refusal case |
| unsupported flags/values (16 argv mutations) | 5 owned by qt's argv grammar; 11 owned by full-plan.rkt | KEEP 5 qt-grammar launches; MOVE 11 to a pure case driving the EXPORTED `resolve-qt-ci-plan` (the exact function qt calls) with `qt-ci-spec` structs |
| zero executed tests under `STRICT_TEST_RUNNER=1` | `run-qt-ci-plan` + qt's `qt: ` wrap of full-plan errors | KEEP subprocess (also pins qt's error wrapping of a full-plan failure) |
| shard partition/cover semantics | `qt-shard-partition` → classify.rkt `shard-files` | already pure; KEEP |
| `--suite` resolves via classify.rkt predicates | `qt-suite-relative` | already pure; KEEP |
| `--suite`/`--shard` explain echo + sharded execution | qt explain + runner forwarding | KEEP (3 launches: explain + two shard executions — the only end-to-end shard execution proof) |
| execution flags forwarded (`--timeout/--profile/--mode/--repeat/--sequential/--ledger`) | runner forwarding | KEEP subprocess |
| `--shard-plan active` partition | `resolve-qt-ci-plan` | already pure; KEEP |
| deleted plan modes refused before execution | full-plan validation + qt wrap + marker non-execution | MERGE 2 → 1 launch (`report` with marker proof); `measure` hits the same validation branch, now pinned by the pure owner case |
| plain `full --explain` byte-identical | `full-explain-lines` via qt | KEEP subprocess (the pure identity `plan-explain-lines` = `full-explain-lines` is separately pinned in test-qt-lanes.rkt) |
| CI `--explain` reproduction spelling | `qt-ci-explain-lines` via qt | KEEP subprocess |
| `--record-gate-evidence` | gate-evidence module | KEEP subprocess |

### tests/test-qt-quick.rkt (7 → 6 qt launches)

| Case | Disposition |
|---|---|
| subprocess helper drains pipes / times out | KEEP (guards this file's own helper; 2 plain racket launches, no qt) |
| whole quick suite through bounded queue (`quick passed: 28 files`) | KEEP — no CI workflow or policy context executes `./qt quick`; this is the ONLY automated end-to-end execution of the audited 28-owner quick set (admission records stay fresh, sources stay green) and its count is the anti-drift pin |
| campaign decisions: 1 file, 6 tests | KEEP as THE single-file counted-pass contract |
| delivery verifier: 1 file, 10 tests | DROP — same single-file CLI contract as the campaign case with a different payload; the payload is an audited quick owner, so the whole-suite case above already executes its ten decisions on every run |
| invalid jobs/paths fail (`--jobs 0`, missing value, `../` escape, `--bogus`) | KEEP — four distinct quick-parser decisions of the quick admission/refusal surface |

### tests/test-qt-lanes.rkt (20 → 17 qt launches; 33 → 27 fixture-file executions)

All pure cases (lane-verdict table, lane table shape, required-offline drift
pin, lane unions, requires vocabulary, claim→lane→suite mapping, explain
bodies, unavailable-here partition, release obligations/evidence, changed
selection/escalation, frozen full explain/preamble) KEEP unchanged. Execution
and CLI cases:

| Case | Disposition |
|---|---|
| lane-verdict recorded run (`run-plan` on the lane fixture) | MERGE into "default runs exactly the runnable selection" — one recorded run now drives verdict + evidence + markers + timeouts; eliminates a duplicate 6-file fixture execution |
| default CLI: explain selection count = executed count, labels, unavailable-here line | KEEP (2 launches; authentic entrypoint) |
| closed-stdout release explain (EPIPE, exit 0) / closed-stdout red release | KEEP (2 launches; unique EPIPE contracts) |
| release CLI prints obligations, never a pass | KEEP |
| `--changed` unusable base fails closed | KEEP |
| `--changed` real git base (clean-tree fail-closed, changed explain, `full --changed` explain, changed execution) | KEEP (4 launches; W2's working-tree/range admission contract, preserved verbatim) |
| unsupported command/options: 7 spellings + 3 message re-reads | MERGE 10 → 7 launches — the three trailing `let-values` re-INVOKED spellings the loop had already launched, only to read their messages; the message pins now ride on the single invocation per spelling |

## 2. Measurements (matched samples, same host, same order, warm bytecode cache)

Process-launch counts are static and deterministic on Linux
(`full-execution-supported?` = true); times are wall-clock via
`racket scripts/run-tests.rkt tests/<file>`:

| File | qt launches before → after | fixture-child execs before → after | time before → after |
|---|---|---|---|
| test-qt-ci-surface.rkt | 30 → 17 | 10 → 10 | 106.4 s → 79.5 s (−25%) |
| test-qt-quick.rkt | 7 → 6 (+2 helper, unchanged) | 30 → 29 | 66.0 s → 64.1 s (−3%) |
| test-qt-lanes.rkt | 20 → 17 | 33 → 27 | 58.3 s → 49.5 s (−15%) |
| **combined** | **57 → 40 (−30%)** | **73 → 66 (−10%)** | **230.6 s → 193.1 s (−16%)** |

Cold-cache context: the very first runs on this host measured 183.2 / 74.9 /
99.7 s (357.8 s combined) because the runner compiles dependencies on first
execution; the matched warm-cache sequence above is the honest
apples-to-apples comparison. test-qt-quick.rkt's floor is the deliberately
kept whole-suite execution (~40 s of its 64 s); its −3% reflects only the
dropped verifier launch.

## 3. Behavior preservation (red/green evidence)

- **Retained authentic CLI contract** (`default does not accept CI-surface
  option --json-out`): flipping the expected regex to a wrong prefix made the
  subprocess case FAIL with the actual qt stderr shown in the failure params;
  restoring it returned PASS. Red exit 1 → green `VERDICT: ✅ PASS`.
- **Moved deterministic decision** (`unsupported --shard-plan: measure` via
  the pure `resolve-qt-ci-plan` owner case): flipping the `check-exn` pattern
  made the pure case FAIL ("Wrong exception raised"); restoring returned PASS.
- **Owner identity probe**: `./qt full --shard-plan measure --shard-total 2`
  prints `qt: full-plan: unsupported --shard-plan: measure (valid: active)`;
  the pure owner raises `full-plan: unsupported --shard-plan: measure (valid:
  active)` — the moved assertion pins the same production text qt surfaces,
  with qt's `qt: ` wrapping still pinned by the retained CLI cases and the
  `STRICT_TEST_RUNNER` case. No test-only reimplementation exists: the moved
  case calls the exported production function the CLI itself calls.
- W2's working-tree help/validation contracts are untouched: `./qt quick`,
  `./qt quick --changed HEAD` (8 owners selected, exit 0) and
  `./qt default --explain` all pass from the worktree root.

## 4. Gate results

| Gate | Result |
|---|---|
| `racket scripts/run-tests.rkt` × 3 target files (before/after, counts above) | PASS ×3 |
| red/green probes (retained CLI + moved pure) | red exit 1 with exact expected failures; green PASS |
| `./qt quick` / `./qt quick --changed HEAD` / `./qt default --explain` | exit 0 ×3 |
| `racket scripts/lint-format.rkt` | PASSED (0 errors) |
| `racket scripts/lint-tests.rkt` | PASSED |
| `racket scripts/lint-docs.rkt` | PASSED |
| `racket scripts/metrics.rkt --lint` | FAILED pre-sync (test-lines/assertions moved), green after `--sync-all` |
| `racket scripts/tier-ownership-matrix.rkt check` | green (1172 rows) |
| `sha256sum -c SHA256SUMS` (artifact dir) | OK |
| `git diff --exit-code` protected paths | empty |

## 5. Residual risk

- The `quick passed: 28 files` whole-suite pin moves with the audited owner
  table (pre-existing, documented in-file); it is load-bearing anti-drift
  coverage and was deliberately kept.
- qt's refusal cost (~3 s/launch) bounds every retained CLI case; the
  remaining 40 launches are each a distinct contract decision, so further
  reduction would start deleting authentic entrypoint coverage.
- Timing deltas carry ±10% host variance; launch-count deltas are exact.

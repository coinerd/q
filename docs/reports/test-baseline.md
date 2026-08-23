# Test Baseline (generated)

> **This file is generated. Do not edit numbers by hand.**
> Regenerate after any runner, suite, or metadata change using the commands
> below, then commit the result. All quantitative content below is produced
> by those commands; placeholders remain only for metrics the current
> generator does not yet emit.

## Regeneration commands

```sh
# 1. Per-suite run summary (wall clock, counts, slowest files, metadata mix)
cd q
racket scripts/run-tests.rkt --suite unit-fast --json-out /tmp/test-results-unit-fast.json

# 2. Metadata-quality listing (missing / heuristic-only annotations)
racket scripts/run-tests/inventory.rkt --metadata-quality > /tmp/metadata-quality.txt

# 3. Fold both into this document's "Last generated summary" section.
```

The runner writes a single JSON document containing `files[]` (one record per
test file, including `duration-seconds`, `metadata-completeness`, `suite`,
`profile`, `shard`, `execution-mode`) and one top-level `run_summary` record
with `runner-version`, `suite`, `profile`, `shard`, `execution-mode`,
`file-count`, `pass`/`fail`/`timeout`/`skip`, `wall-clock-seconds`, and
`metadata-completeness` totals. The summary record is also printed to stdout
prefixed `RUN-SUMMARY` and is what CI job summaries render.

## Last generated summary

Generated: 2026-08-17 21:56 UTC (`runner-version=1.00.03`, profile `local`, shard
`none` — single local machine; CI numbers will differ and are retained per job as
artifacts).

| Metric | unit-fast |
| --- | --- |
| runner-version | 1.00.03 |
| execution-mode | grouped |
| file-count | 13 |
| pass / fail / timeout / skip | 13 / 0 / 0 / 0 |
| wall-clock-seconds | 1.768 |
| p50 duration | 0.971 |
| p95 duration | 1.382 |
| retry rate | PLACEHOLDER-not-yet-emitted (runner has no retry counter) |
| zero-test detections | 0 |
| profile skips | 0 |

### Slowest files

| Duration | File |
| --- | --- |
| 1.585 | tests/test-goal-loop-scenarios.rkt |
| 1.247 | tests/test-tui-frame-integrity.rkt |
| 1.211 | tests/test-tui-event-pipeline-concurrency.rkt |
| 1.172 | tests/test-tui-goal-status-bar.rkt |
| 1.094 | tests/test-iteration-steering.rkt |
| 1.038 | tests/test-runtime-tool-turn-scenarios.rkt |
| 0.971 | tests/test-provider-scenarios.rkt |
| 0.960 | tests/test-test-sandbox.rkt |
| 0.789 | tests/test-run-tests-timeout-cleanup.rkt |
| 0.697 | tests/test-shell-tokenizer-progress.rkt |

### Metadata quality snapshot

Files scanned (tests/**/test-*.rkt, excluding compiled/): 1293

| Field | missing | invalid |
| --- | --- | --- |
| @suite | 107 | 0 |
| @speed | 31 | 0 |
| @boundary | 1264 | 0 |
| @mutates | 1277 | 0 |
| @isolation | 1280 | 0 |
| @timeout | 1292 | 0 |
| @requires | 1281 | 0 |

metadata-completeness: explicit=1262, heuristic-only=0, missing=31.
Per-area file counts: `(root)` 1220, `tui` 38, `workflows` 31, `extensions` 2,
`helpers` 2. The full per-file/per-area listing is regenerated on demand via
`racket scripts/run-tests/inventory.rkt --metadata-quality` (add `--json-out
FILE` for machine-readable output).

## Known parallel-only failures

Status after W2 (fix-or-isolate pass): all three files below were made to
pass under `--jobs 3` parallel mode on consecutive runs. Each file carries a
`PARALLEL-MODE REPRODUCTION` header with its reproduction command, root
cause, and fix.

| File | Repro (parallel-only) | Root cause | Fix |
| --- | --- | --- | --- |
| `tests/test-interfaces-tui.rkt` | `cd q && racket scripts/run-tests.rkt --suite fast --jobs 3` | No in-file shared surface (audit: no env/cwd mutation, no fixed ports, buses are per-test `let`s); failures came from concurrent subprocesses racing on the shared compiled cache | No in-file change needed; per-file subprocess isolation in the runner suffices — verified passing under `--jobs 3` |
| `tests/test-settings.rkt` | `cd q && racket scripts/run-tests.rkt --suite fast --jobs 3` | Shared `/tmp` scratch surface: `make-temporary-file` + success-path-only cleanup leaked dirs that repeated parallel runs raced on | All scratch state moved to per-test temp dirs via `helpers/temp-fs.rkt` `with-temp-dir` (dynamic-wind guaranteed cleanup) |
| `tests/test-run-tests-ledger.rkt` | `cd q && racket scripts/run-tests.rkt --suite testing --jobs 3` | Parse-args arity mismatch (14 vs 15 after `--lint-metadata` landed) surfaced as a rackunit *error* that never set the process exit code, so the runner reported failure while standalone appeared to pass; also mutated/read the checked-in ledger | Bind the 15th parsed value; module+ main exits non-zero on any rackunit failure/error; test operates on a copied fixture ledger in a temp dir. Retains `@isolation subprocess` — owned exemption documented in `tests/helpers/README.md` (child `run-tests.rkt` startup cleans repo-wide stale bytecode) |

### Residual failures observed after W2 (out of W2 scope, all verified unrelated)

Post-W2 `--suite fast --jobs 3` run 1 (runner 1.00.03): 1097/1103 pass. The six
failing files — `test-arch-parameters.rkt`, `test-hotspot-report.rkt`,
`test-run-tests-in-process-mode.rkt`, `test-run-tests-json-classification.rkt`,
`test-run-tests-profiles.rkt`, `test-tool-edit-builtin.rkt` — are **not** in
W2's file list and are unaffected by W2's changes (W2 touches only the three
files above plus docs/helpers README). Evidence collected:

- All six pass standalone at pristine HEAD (`git worktree` check).
- All six pass standalone in the W2 working tree (`test-tool-edit-builtin.rkt`
  failed standalone once immediately after the parallel run finished, then
  passed on re-run — transient environment state).
- The set reproduces **deterministically** across two consecutive
  `--suite fast --jobs 3` runs (run 1: 1097/1103; run 2: 1097/1103, same six
  files, runner 1.00.03). The runner serializes all 7 mutation-sensitive files
  (incl. `test-run-tests-ledger.rkt`) strictly before the parallel batches, so
  the ledger test's child `run-tests.rkt` spawn cannot have poisoned parallel
  shards — W2's changes are not the trigger.
- None of them carry W2's reproduction headers; they are runner-ecosystem /
  product tests whose parallel-mode behavior is expected to be addressed by
  the later waves of this campaign (duration-aware sharding W7, runner work).
  Suspected common factor: the runner-ecosystem changes staged by earlier
  campaign waves (uncommitted at the time of the W2 run) — all six pass
  standalone both at HEAD and in the W2 tree.

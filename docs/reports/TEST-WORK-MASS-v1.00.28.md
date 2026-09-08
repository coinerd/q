# Test Work Mass — v1.00.28 W0 Runtime Census

Status: numbers pending the live census collection (`campaign/v1.00.28-w0`, attempt 3).
This report is completed when `runtime-census.rkt` finishes the 3×1177 sample run and
`--check` passes at the branch head.

## 1. Executive decision

TO-BE-FILLED (answers §10 Q1–Q7 summary; written only from the census JSON).

## 2. Measurement method

- Inventory: the canonical `fast` profile resolution from `scripts/run-tests/inventory.rkt`
  (metadata-driven selection, same code path as `run-tests.rkt --suite fast`), re-exported
  for the census. No hand-picked subset.
- Samples: ≥3 successful per-file samples (target exactly 3), canonical prepared
  environment (`scripts/run-tests/prepare-env.rkt` semantics), each sample an isolated
  `racket tests/<file>` launch under the same environment the runner uses.
- Timing: wall-clock per sample, `current-inexact-milliseconds` around the child process;
  median and p95 with the same linear interpolation as `hotspot-benchmark.rkt`.
- Retention: all attempts are retained (`samples_ms`, `all_attempts`); failures and
  timeouts are census records (`status` = `pass`/`fail`/`timeout`), never discarded.
- Missing-file rule: every `fast`-selected file must appear with a duration record or an
  explicit collection failure; the census tool errors if the inventory and the census
  disagree in either direction (guarded by `tests/test-runtime-census.rkt`).
- Unknown counters serialize as `null`/`"unknown"`, never `0`.

## 3. Inventory validation

TO-BE-FILLED: selected-file count, on-disk count, match verdict, omissions (must be none).

## 4. Fast work mass

TO-BE-FILLED: `fast_work_mass_ms = Σ median_ms` and the raw sum in seconds/minutes.

## 5. Pareto table

TO-BE-FILLED: top 10 / 25 / 50 / 100 / remainder — summed median mass and % of total each.

## 6. Runtime buckets

TO-BE-FILLED: <250 ms, 250 ms–1 s, 1–2 s, 2–5 s, 5–10 s, >10 s — file count and mass each.

## 7. Boundary split

TO-BE-FILLED: `unit-fast/unit`, `fast/unit`, `fast/integration`, `fast/other` — count,
mass, median file runtime, top contributors.

## 8. Process amplification

TO-BE-FILLED: top subprocess launchers (Q6 input), counter provenance
(dynamic opt-in scopes over project-owned helpers; `unknown` elsewhere).

## 9. Git/fixture amplification

TO-BE-FILLED: Git commands, private Git fixtures, session fixtures, temp directories,
worktrees — top constructors (Q5 input).

## 10. Wait audit

TO-BE-FILLED: requested real sleep totals in `fast` (Q4), plus reference to
`static-wait-scan.json` (§5 term list; triage input, never proof of a defect).

## 11. Grouped-execution candidate mass

TO-BE-FILLED: Q7 candidate count and combined work mass.

## 12. Top 25 annotated hotspots

TO-BE-FILLED: per-file median, share, primary attribution (§8 categories; `UNKNOWN`
allowed and preferred over speculation).

## 13. Remediation queue

TO-BE-FILLED: Q1–Q7 membership with sizes; §10 Q7 top-10 expected-payoff ranking.

## 14. Reproducibility

```
cd q && racket scripts/run-tests/runtime-census.rkt --check \
  artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json
cd q && sha256sum -c artifacts/test-runtime/v1.00.28-census/SHA256SUMS
```

`--check` regenerates the canonical census bytes in a temp dir and compares digests, then
verifies the static scan + SHA256SUMS set. The census is deterministic modulo measured
wall-clock: `--check` validates schema, coverage, and checksum integrity of the committed
artifacts (it does not require identical timings across machines).

## 15. Checksums

TO-BE-FILLED: SHA256SUMS listing for `fast-runtime-census.json` and `static-wait-scan.json`.

## 16. Limitations

TO-BE-FILLED: single-machine variance, `unknown` counter coverage boundaries, static-scan
false-positive nature, p95-of-3 instability.

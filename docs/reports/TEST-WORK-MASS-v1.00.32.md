# v1.00.32 Test Work Mass Report — Phase A artifact-only current-cost baseline

Issue [#9930](https://github.com/coinerd/q/issues/9930), milestone [#899](https://github.com/coinerd/q/milestone/899) (wave WA). Companion data: `docs/reports/TEST-WORK-MASS-v1.00.32.json` (schema `q.test-work-mass/1`, all 1,329 files). Basis: **retained successful CI artifacts only** — no new profiler, no re-instrumentation, no source/test/runner changes. This report supersedes the measurement basis of the v1.00.28 census (`TEST-WORK-MASS-v1.00.28.md`) for current-main decision-making; the census report itself is retained unchanged as history (§10 reconciles the two).

## 1. Executive decision

The canonical `fast` suite at main `22851554` (run `44227179596`, attempt 2) selects **1,329 files** (3 × 443 per shard, disjoint). Their per-file durations sum to **6,511.629 s ≈ 108.5 min** — the sum of per-file durations, **not** runner wall time (§2, §4). The three shard walls are 1,111.721 / 981.348 / 595.384 s (sum 2,688.453 s; critical path 1,111.721 s). The distribution is extremely top-heavy: **3 files carry 19.13 %** of the mass, 8 carry 35.82 %, 14 carry 43.59 %, 50 carry 66.36 %. Every file with a ≥ 60 s median (18 files, 49.5 % of median mass) is a real-world execution — Verify-chain, nested `./qt`/`raco test` runner, git-fixture build, or real wall-clock wait. The static-signal split attributes 4,422.4 s (67.9 %) to duplicated real-world execution; the concrete B1–B3 + CLI-family backlog is in §9, the full ranked top-50 backlog in §8.

**Correction of record.** Earlier planning drafts carried an approximate claim of "~2.5 ks top-3 ≈ 39 %" for this run. The artifact-exact values are **top 3 = 1,245.837 s (19.13 %)** and top 14 = 2,838.694 s (43.59 %); §4 reproduces all aggregates from raw artifact JSON.

## 2. Method and metric discipline

Everything in this report is derived from artifacts GitHub already retained for five green main runs (§3). No test was re-run, no instrumentation added. Five cost fields are kept strictly separate (binding amendment 3) and are never added to each other:

| Field | Source | Meaning |
| --- | --- | --- |
| per-file duration | `files[].duration_seconds` in each `test-results-fast-N` artifact | cumulative wall of that one file's execution (subprocess) |
| Σ per-file durations | sum over all 1,329 files | **6511.629 s** — pure sequential work mass; NOT wall, NOT median, NOT p95, NOT expected PR elapsed |
| shard execution wall | `run_summary.wall_clock_seconds` | wall of the shard; smaller than Σ durations because 4 files run in parallel inside the shard |
| preparation cost | `extra.prepared_environment` telemetry | closure preparation `preparation_elapsed_ms` (69.7–118.7 s per baseline shard) and prepared-env `restore_ms` (≈ 0.7–1.2 s) |
| job elapsed time | GitHub job `started_at → completed_at` | wall of the whole job incl. checkout, setup-racket, bytecode clear, prep, teardown, artifact upload |
| GitHub queue delay | job `started_at` − own attempt start | dispatch latency; conflates needs-graph waiting with runner availability (no per-attempt start exposed for earlier attempts; `created_at` used as labeled proxy) |

Why Σ per-file durations (6,511.629 s) exceeds the shard-wall sum (2,688.453 s): each shard executes its 443 files with a 4-wide worker pool, so four files' durations accumulate per wall second. A PR's fast critical path is the slowest shard wall (1,111.721 s), never the Σ figure; the Σ figure is the atomic-work-mass denominator used for every share in this report.

## 3. Artifact provenance

Selection policy (binding amendment 1): one authoritative successful artifact per (SHA, run attempt, shard); shard index/total, suite, pass/fail summary and per-shard file count (443) validated from artifact JSON; ambiguity resolved by provenance + result truth; ambiguous or failed artifacts are excluded and recorded — never silently selected.

### 3.1 Authoritative fast-suite artifacts (15 shard executions, 5 runs)

| Run | Head SHA | Att | Job | Artifact | Created (UTC) | Shard | Files P/F | Wall (s) | Role |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `44227179596` | `22851554` | 2 | test (0) | `14394747605` | 2026-11-18T03:30:29Z | 0/3 | 443/0 | 1111.721 | authoritative |
| `44227179596` | `22851554` | 2 | test (1) | `14401705110` | 2026-11-18T05:31:23Z | 1/3 | 443/0 | 981.348 | authoritative |
| `44227179596` | `22851554` | 2 | test (2) | `14393973835` | 2026-11-18T03:22:07Z | 2/3 | 443/0 | 595.384 | authoritative |
| `44212916888` | `8fcba8e0` | 1 | test (0) | `14387819340` | 2026-11-18T01:30:54Z | 0/3 | 443/0 | 949.351 | authoritative |
| `44212916888` | `8fcba8e0` | 1 | test (1) | `14387743343` | 2026-11-18T01:24:44Z | 1/3 | 443/0 | 568.239 | authoritative |
| `44212916888` | `8fcba8e0` | 1 | test (2) | `14388009274` | 2026-11-18T01:42:47Z | 2/3 | 442/0 | 1669.898 | authoritative |
| `44183478321` | `41496f0f` | 1 | test (0) | `14375281575` | 2026-11-17T22:09:32Z | 0/3 | 443/0 | 559.505 | authoritative |
| `44183478321` | `41496f0f` | 1 | test (1) | `14374887286` | 2026-11-17T22:08:08Z | 1/3 | 443/0 | 445.081 | authoritative |
| `44183478321` | `41496f0f` | 1 | test (2) | `14376635646` | 2026-11-17T22:26:47Z | 2/3 | 442/0 | 1555.719 | authoritative |
| `44165093455` | `6c4028de` | 1 | test (0) | `14365914296` | 2026-11-17T20:27:57Z | 0/3 | 443/0 | 838.866 | authoritative |
| `44165093455` | `6c4028de` | 1 | test (1) | `14366297090` | 2026-11-17T20:22:27Z | 1/3 | 443/0 | 500.602 | authoritative |
| `44165093455` | `6c4028de` | 1 | test (2) | `14367586239` | 2026-11-17T20:28:33Z | 2/3 | 442/0 | 860.725 | authoritative |
| `44156308113` | `fde734da` | 1 | test (0) | `14362024639` | 2026-11-17T19:41:53Z | 0/3 | 443/0 | 857.267 | authoritative |
| `44156308113` | `fde734da` | 1 | test (1) | `14362188177` | 2026-11-17T19:34:17Z | 1/3 | 442/0 | 475.093 | authoritative |
| `44156308113` | `fde734da` | 1 | test (2) | `14365185883` | 2026-11-17T19:55:13Z | 2/3 | 442/0 | 1691.645 | authoritative |

Run `44227179596` is a partial re-run: attempt 1 failed `test (1)`; the re-run (attempt 2) re-executed only that shard (started 05:12:39Z). Shards 0 and 2 passed on attempt 1 and their attempt-1 artifacts are authoritative for them. Attempt-1 job ids for re-run legs are superseded in the jobs API by the attempt-2 entry, so the stale shard-1 execution is identified by its artifact (created 03:25:28Z, inside the attempt-1 window, verdict `fail`).

### 3.2 Explicit exclusion (retained, never mixed in)

| Run | Att | Artifact | Created (UTC) | Shard | Files P/F | Wall (s) | Reason |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `44227179596` | 1 | `14393884631` | 2026-11-18T03:25:28Z | 1/3 | 442/1 | 786.127 | failed attempt-1 execution of shard 1 (tests/test-tool-coordinator-duration.rkt); superseded by successful attempt-2 artifact `14401705110` |

The stale artifact's 442-pass/1-fail record is retained verbatim in the JSON companion (`provenance.excluded`) and contributes to **no** aggregate in this report.

### 3.3 Companion artifacts per run (small, cross-checked)

Per run: `test-shard-0/1/2` manifests (443-file shard membership; the baseline attempt-2 shard-1 manifest `14400693391` is the one created 05:31:21Z), `prepared-env-records-<run>-0/1/2` (restore telemetry), `prepared-env-report-<run>` (aggregate restore report), `metadata-inventory-<sha>` (static metadata inventory), plus the other-suite result artifacts used in §6: `test-results-security`, `test-results-platform`, `test-results-workflows-0/1`, `test-results-cross-version-811`. Full id lists are in the JSON companion (`provenance.companion_artifacts`). The smoke lane runs one file (`tests/test-provider-smoke.rkt`) inline and emits no artifact.

## 4. Baseline aggregate — corrected numbers (run `44227179596`, attempt 2, SHA `22851554`)

| Quantity | Value |
| --- | --- |
| Selected files | **1,329** (443 × 3, shards disjoint) |
| **Σ per-file durations** | **6511.629 s ≈ 108.5 min** (sum of per-file durations, not wall/median/p95) |
| Shard walls | 1111.721 / 981.348 / 595.384 s (sum 2688.453 s; critical path 1111.721 s) |
| Top 1 | 546.343 s (8.39 %) — `tests/test-gsd-auto-review-production.rkt` |
| Top 3 | 1245.837 s (19.13 %) |
| Top 8 | 2332.154 s (35.82 %) |
| Top 14 | 2838.694 s (43.59 %) |
| Top 25 | 3497.361 s (53.71 %) |
| Top 50 | 4321.434 s (66.36 %) |
| Top 100 | 4858.582 s (74.61 %) |
| Files pass/fail/timeout | 1,329 / 0 / 0 (verdict `pass`; file-level test totals 6,344 + 6,605 + 6,604 = 19,553 tests across the three shards) |

Reproduction: every number in this table was recomputed from the raw artifact JSON (durations at 1 ms precision, IEEE-754 double summation) and matches the values verified at planning time exactly; tolerance statement: rounding of the final sum to 3 decimals introduces at most ±0.001 s per file (±0.5 s over 1,329 files, 0.008 %), and no correction was needed.

## 5. Cost-field separation on the baseline run (never conflated)

| Job | Elapsed (s) | Shard wall (s) | Prep closure (s) | Prep restore (s) | Elapsed − wall (s) | Remainder after prep (s) | Dispatch (s, ref) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| test (0) | 1228.0 | 1111.721 | 69.721 | 0.796 | 116.279 | 46.558 | 648.0 (created_at-proxy) |
| test (1) | 1125.0 | 981.348 | 118.688 | 1.168 | 143.652 | 24.964 | 5.0 (attempt-start) |
| test (2) | 726.0 | 595.384 | 115.881 | 1.191 | 130.616 | 14.735 | 648.0 (created_at-proxy) |

Reading: each test job spends ≈ 70–119 s on closure preparation and ≈ 0.8–1.2 s on prepared-env restore *inside* the job; the remaining ≈ 15–47 s per job is checkout/setup/teardown/upload. Baseline dispatch latencies: attempt-2 `test (1)` started 5.0 s after the attempt-2 run start; attempt-1 jobs started ≈ 648 s after run creation, almost entirely needs-graph waiting (lint → fast-env → test), not runner queueing. `fast-env` itself takes 245–330 s elapsed per run and produces the prepared-env artifact all three test shards restore from.

## 6. Five-run comparison — medians, ranges, matched coverage, environment

| # | Run | Head SHA | Files | Σ durations all (s) | Σ durations matched (s) | Shard walls (s) |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | `44227179596` | `22851554` | 1,329 | 6511.629 | 6509.978 | 1111.721 / 981.348 / 595.384 |
| 2 | `44212916888` | `8fcba8e0` | 1,328 | 7432.279 | 7430.621 | 949.351 / 568.239 / 1669.898 |
| 3 | `44183478321` | `41496f0f` | 1,328 | 5574.047 | 5572.539 | 559.505 / 445.081 / 1555.719 |
| 4 | `44165093455` | `6c4028de` | 1,328 | 5343.776 | 5342.985 | 838.866 / 500.602 / 860.725 |
| 5 | `44156308113` | `fde734da` | 1,327 | 6771.295 | 6771.295 | 857.267 / 475.093 / 1691.645 |

Matched coverage: **1,327 of 1,329** files appear in all five runs. The five runs are five consecutive green main pushes (fde734da → 22851554, commits #9924/#9926/#9927/#9928): `tests/test-docs-accuracy.rkt` exists only at the baseline head (added by #9928) and `tests/test-testing-audit.rkt` is absent from the oldest run. Σ-duration spread across runs is wide — 5,343.8 s … 7,432.3 s, i.e. −18 % / +14 % around the baseline run's 6,511.629 s (the median of the five) — under an identical workflow definition and identical prepared-env cache key — runner-host load variance is real and is the reason all per-file decisions below use medians/ranges, never single samples.

Environment per run: ubuntu-latest; fast lanes Racket 8.10, cross-version lane Racket 8.11; runner version 1.00.32 in all 15 shard executions; prepared-env cache key `prepared-env-fast:497f0f51…` with `verified` restore on all 15 shards; `.github/` unchanged across the window. Remaining environment difference is the four commits' product/test changes themselves.

## 7. Pareto and buckets (baseline run; medians across 5 runs for buckets)

| Rank set | Files | Σ baseline durations (s) | % of mass |
| --- | --- | --- | --- |
| Top 1 | 1 | 546.343 | 8.39 % |
| Top 3 | 3 | 1245.837 | 19.13 % |
| Top 8 | 8 | 2332.154 | 35.82 % |
| Top 14 | 14 | 2838.694 | 43.59 % |
| Top 25 | 25 | 3497.361 | 53.71 % |
| Top 50 | 50 | 4321.434 | 66.36 % |
| Top 100 | 100 | 4858.582 | 74.61 % |
| Remainder | 1229 | 1653.047 | 25.39 % |

| Median bucket | Files | Σ median (s) | % of median mass |
| --- | --- | --- | --- |
| < 250 ms | 1 | 0.244 | 0.00 % |
| 250 ms – 1 s | 519 | 379.502 | 5.77 % |
| 1 – 2 s | 544 | 725.408 | 11.02 % |
| 2 – 5 s | 160 | 453.707 | 6.89 % |
| 5 – 10 s | 31 | 196.258 | 2.98 % |
| 10 – 30 s | 35 | 655.912 | 9.97 % |
| 30 – 60 s | 21 | 912.985 | 13.87 % |
| > 60 s | 18 | 3257.717 | 49.50 % |

Sample-limited distribution points (5 samples per file at best — explicitly NOT p95 in the statistical sense): per-file single-sample (baseline run) p50 = 1.201 s, p90 = 3.524 s, p95 = 12.082 s; over the 1,327 five-run medians p50 = 1.121 s, p90 = 3.532 s, p95 = 12.705 s.

## 8. Ranked migration backlog — top 50 files

Columns: duration per run (d1…d5 keyed to §6), median, min–max range across the five runs, suite membership beyond fast (S=security, P=platform, W=workflows, M=smoke; all 1,329 fast files also run in the Racket 8.11 cross-version lane), dominant work kind, unique `@covers` claims (modules no other fast file covers; joined with the v1.00.32-w0 ownership matrix), proposed destination, expected saving (upper bound = median if the whole file leaves fast; `—` when the real-world share is unknown until a WB wave measures the split), risk.

Work kinds: VC = Verify-chain (real production command/registry path), NQ = nested `./qt` invocation, NR = nested `raco test`, NS = nested runner script, GF = git-fixture build, RW = real wait, PU = pure in-process. Destinations: SF = slow-full (move real-world execution out of the required PR path; contract remainder may stay short-required), SRC = short required contract (slim to contract assertions; fixture/wait reduction), PU = keep as-is. Risk: H/M/L from unique-claim count and mass rank (H = 3+ unique claims or top-20 real-world; the owning wave must run the unique-claims review before any move). These are evidence-based **proposals** for the downstream waves, not decisions.

| # | File | d1 | d2 | d3 | d4 | d5 | Median | Range | Suites | Kind | UQ | Destination | Saving (s) | Risk |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | `tests/test-gsd-auto-review-production.rkt` | 546.3 | 540.7 | 273.1 | 504.2 | 501.0 | 504.2 | 273.1–546.3 | — | VC | 0 | SF | 504.2 | H |
| 2 | `tests/test-gsd-supplemental-evidence-probe.rkt` | 365.2 | 349.8 | 193.1 | 300.8 | 302.6 | 302.6 | 193.1–365.2 | — | NS | 0 | SF | 302.6 | H |
| 3 | `tests/test-gsd-auto-review-loop.rkt` | 334.3 | 490.1 | 447.5 | 242.0 | 504.8 | 447.5 | 242.0–504.8 | — | NR | 0 | SF | 447.5 | H |
| 4 | `tests/test-gsd-generated-metadata-correction.rkt` | 332.6 | 508.3 | 471.2 | 210.8 | 535.9 | 471.2 | 210.8–535.9 | — | VC | 2 | SF | 471.2 | H |
| 5 | `tests/test-gsd-evidence-repair-production.rkt` | 201.7 | 290.5 | 272.9 | 146.4 | 300.9 | 272.9 | 146.4–300.9 | — | NS | 0 | SF | 272.9 | H |
| 6 | `tests/test-qt-interface.rkt` | 194.9 | 187.6 | 67.9 | 92.4 | 90.4 | 92.4 | 67.9–194.9 | — | NQ | 0 | SF | 92.4 | H |
| 7 | `tests/test-gsd-lost-author-finish.rkt` | 189.8 | 259.2 | 273.0 | 125.1 | 280.0 | 259.2 | 125.1–280.0 | — | NR | 0 | SF | 259.2 | H |
| 8 | `tests/test-gsd-evidence-probe.rkt` | 167.3 | 166.9 | 95.1 | 137.9 | 136.5 | 137.9 | 95.1–167.3 | — | NR | 0 | SF | 137.9 | H |
| 9 | `tests/test-gsd-delivery-recovery.rkt` | 99.1 | 141.9 | 131.0 | 73.5 | 144.1 | 131.0 | 73.5–144.1 | — | VC | 1 | SF | 131.0 | H |
| 10 | `tests/test-qt-ci-surface.rkt` | 97.4 | 105.5 | 75.4 | 79.6 | 55.5 | 79.6 | 55.5–105.5 | — | NQ | 0 | SF | 79.6 | H |
| 11 | `tests/test-gsd-delivery-read-ladder.rkt` | 93.8 | 90.1 | 45.4 | 83.6 | 81.8 | 83.6 | 45.4–93.8 | — | NR | 0 | SF | 83.6 | H |
| 12 | `tests/test-gsd-evidence-authority.rkt` | 75.1 | 71.7 | 36.7 | 64.2 | 67.5 | 67.5 | 36.7–75.1 | — | NS | 0 | SF | 67.5 | H |
| 13 | `tests/test-qt-quick.rkt` | 72.0 | 74.4 | 56.2 | 58.9 | 22.1 | 58.9 | 22.1–74.4 | — | NQ | 0 | SF | 58.9 | H |
| 14 | `tests/test-gsd-author-recovery-bounds.rkt` | 69.1 | 63.7 | 37.4 | 69.5 | 73.2 | 69.1 | 37.4–73.2 | — | VC | 1 | SF | 69.1 | H |
| 15 | `tests/test-qt-lanes.rkt` | 68.5 | 72.6 | 53.6 | 49.5 | 32.5 | 53.6 | 32.5–72.6 | — | NQ | 0 | SF | 53.6 | H |
| 16 | `tests/test-gsd-delivery-ladder-wiring.rkt` | 65.4 | 59.1 | 38.7 | 55.5 | 48.6 | 55.5 | 38.7–65.4 | — | NR | 0 | SF | 55.5 | H |
| 17 | `tests/test-gsd-delivery-sync-native.rkt` | 65.3 | 63.8 | 34.7 | 60.1 | 60.9 | 60.9 | 34.7–65.3 | — | VC | 0 | SF | 60.9 | H |
| 18 | `tests/test-milestone-gate.rkt` | 61.9 | 60.8 | 29.0 | 45.3 | 43.5 | 45.3 | 29.0–61.9 | — | NR | 0 | SF | 45.3 | H |
| 19 | `tests/test-wave-delivery-integrity-register.rkt` | 61.8 | 88.2 | 83.5 | 50.9 | 60.0 | 61.8 | 50.9–88.2 | — | NR | 0 | SF | 61.8 | H |
| 20 | `tests/test-ci-recovery-trial.rkt` | 60.0 | 58.9 | 43.1 | 30.7 | 58.4 | 58.4 | 30.7–60.0 | — | PU | 0 | PU | — | L |
| 21 | `tests/test-gsd-delivery-preflight.rkt` | 57.2 | 56.2 | 36.1 | 50.7 | 52.9 | 52.9 | 36.1–57.2 | — | VC | 1 | SF | 52.9 | M |
| 22 | `tests/test-gsd-author-repair-go.rkt` | 55.9 | 45.6 | 31.0 | 46.8 | 38.0 | 45.6 | 31.0–55.9 | — | NR | 0 | SF | 45.6 | M |
| 23 | `tests/test-gsd-executor-ergonomics-prompt.rkt` | 55.2 | 54.3 | 29.0 | 47.7 | 48.1 | 48.1 | 29.0–55.2 | — | NR | 0 | SF | 48.1 | M |
| 24 | `tests/test-golden-flows.rkt` | 54.4 | 53.0 | 28.1 | 38.9 | 39.2 | 39.2 | 28.1–54.4 | — | PU | 0 | PU | — | L |
| 25 | `tests/test-gsd-delivery-write.rkt` | 53.1 | 44.5 | 27.0 | 42.5 | 34.5 | 42.5 | 27.0–53.1 | — | GF | 0 | SRC | — | L |
| 26 | `tests/test-gsd-delivery-merge.rkt` | 52.3 | 94.4 | 71.8 | 53.9 | 91.0 | 71.8 | 52.3–94.4 | — | NR | 0 | SF | 71.8 | M |
| 27 | `tests/test-gsd-delivery-binding.rkt` | 50.7 | 49.0 | 34.7 | 42.9 | 39.2 | 42.9 | 34.7–50.7 | — | NR | 0 | SF | 42.9 | M |
| 28 | `tests/test-gsd-delivery-status.rkt` | 50.6 | 71.6 | 63.4 | 43.7 | 73.2 | 63.4 | 43.7–73.2 | — | NR | 0 | SF | 63.4 | M |
| 29 | `tests/test-gsd-proactive-red-cancellation.rkt` | 47.8 | 47.4 | 46.8 | 47.4 | 47.1 | 47.4 | 46.8–47.8 | — | NR | 0 | SF | 47.4 | M |
| 30 | `tests/test-gsd-supplemental-disposition-store.rkt` | 43.1 | 81.8 | 81.0 | 44.3 | 86.0 | 81.0 | 43.1–86.0 | — | RW | 0 | SRC | — | L |
| 31 | `tests/test-gsd-author-retry-satisfied.rkt` | 39.1 | 59.1 | 50.6 | 37.0 | 61.6 | 50.6 | 37.0–61.6 | — | NR | 1 | SF | 50.6 | M |
| 32 | `tests/test-gsd-evidence-discovery.rkt` | 37.8 | 42.5 | 50.2 | 18.6 | 45.0 | 42.5 | 18.6–50.2 | — | NS | 0 | SF | 42.5 | M |
| 33 | `tests/test-artifact-provenance.rkt` | 36.2 | 33.5 | 26.2 | 30.6 | 28.0 | 30.6 | 26.2–36.2 | W | GF | 1 | SRC | — | M |
| 34 | `tests/test-prepared-env-report.rkt` | 34.4 | 33.0 | 23.4 | 30.5 | 30.6 | 30.6 | 23.4–34.4 | — | NR | 0 | SF | 30.6 | M |
| 35 | `tests/test-gsd-delivery-publish.rkt` | 33.8 | 54.6 | 44.5 | 26.7 | 53.8 | 44.5 | 26.7–54.6 | — | VC | 1 | SF | 44.5 | M |
| 36 | `tests/test-gsd-delivery-coordinator.rkt` | 32.2 | 31.5 | 18.3 | 26.6 | 27.0 | 27.0 | 18.3–32.2 | — | NR | 0 | SRC | — | M |
| 37 | `tests/test-gsd-go-orchestrator.rkt` | 32.1 | 30.3 | 15.9 | 30.4 | 30.9 | 30.4 | 15.9–32.1 | — | NR | 0 | SF | 30.4 | M |
| 38 | `tests/test-gsd-author-session-reclaim.rkt` | 31.1 | 29.8 | 15.3 | 34.7 | 35.4 | 31.1 | 15.3–35.4 | — | NR | 1 | SF | 31.1 | M |
| 39 | `tests/test-test-gate-ownership.rkt` | 30.9 | 36.1 | 23.9 | 24.7 | 20.4 | 24.7 | 20.4–36.1 | — | VC | 0 | SRC | — | M |
| 40 | `tests/test-gsd-delivery-publication.rkt` | 30.0 | 26.2 | 17.1 | 26.1 | 20.0 | 26.1 | 17.1–30.0 | — | NR | 0 | SRC | — | M |
| 41 | `tests/test-verification-job-lifecycle.rkt` | 29.4 | 28.3 | 24.7 | 27.2 | 27.0 | 27.2 | 24.7–29.4 | — | RW | 0 | SRC | — | L |
| 42 | `tests/test-hotspot-report.rkt` | 27.4 | 24.7 | 13.0 | 19.2 | 18.7 | 19.2 | 13.0–27.4 | — | NR | 0 | SRC | — | M |
| 43 | `tests/test-plan-validation-explain-cli.rkt` | 24.6 | 25.4 | 19.0 | 23.0 | 19.4 | 23.0 | 19.0–25.4 | — | NR | 1 | SRC | — | M |
| 44 | `tests/test-gsd-delivery-verifier-git-contract.rkt` | 23.8 | 24.0 | 11.8 | 24.0 | 24.0 | 24.0 | 11.8–24.0 | — | NS | 0 | SRC | — | M |
| 45 | `tests/test-gsd-branch-delivery-verification.rkt` | 23.7 | 26.7 | 35.4 | 10.1 | 29.1 | 26.7 | 10.1–35.4 | — | VC | 0 | SRC | — | M |
| 46 | `tests/test-qt.rkt` | 23.6 | 23.8 | 12.8 | 19.6 | 19.0 | 19.6 | 12.8–23.8 | — | NQ | 0 | SRC | — | M |
| 47 | `tests/test-iteration-transitions.rkt` | 22.7 | 23.7 | 11.6 | 15.7 | 15.5 | 15.7 | 11.6–23.7 | — | PU | 0 | PU | — | L |
| 48 | `tests/test-gsd-evidence-author-repair.rkt` | 22.3 | 31.0 | 32.4 | 14.4 | 32.5 | 31.0 | 14.4–32.5 | — | NR | 0 | SF | 31.0 | M |
| 49 | `tests/test-run-tests-profiles.rkt` | 22.1 | 21.3 | 13.9 | 16.4 | 16.7 | 16.7 | 13.9–22.1 | — | NS | 0 | SRC | — | M |
| 50 | `tests/test-qt-full.rkt` | 22.1 | 30.7 | 28.3 | 20.3 | 34.1 | 28.3 | 20.3–34.1 | — | NQ | 0 | SRC | — | M |

Top-50 destination totals: SF 33 files / 3887.5 s upper-bound saving; SRC 14 files; PU 3 files. Full-inventory distributions: §11.

## 9. Downstream flags — nested runners and duplicated real-world execution

### 9.1 B1–B3 targets (named by the milestone wave map)

| Wave | Target | Baseline (s) | Median (s) | Range | Kind | Unique claims | Proposed destination |
| --- | --- | --- | --- | --- | --- | --- | --- |
| WB1 / B1 | `tests/test-gsd-auto-review-loop.rkt` | 334.327 | 447.5 | 242.0–504.8 | NR | 0 | SF |
| WB2 / B2 | `tests/test-gsd-auto-review-production.rkt` | 546.343 | 504.2 | 273.1–546.3 | VC | 0 | SF |
| WB3 / B3 | `tests/test-gsd-generated-metadata-correction.rkt` | 332.600 | 471.2 | 210.8–535.9 | VC | 2 | SF |

All three run full real owned-Verify generations (real delivery controller/verifier subprocesses, real git fixtures, deterministic independent-model responses only). They are the three largest Verify-chain costs in the suite and the first candidates for the pure/contract/E2E atomic split the wave map mandates.

### 9.2 CLI family — nested `./qt` invocations (6 spawning files)

| File | Baseline (s) | Median (s) | Kind | Destination |
| --- | --- | --- | --- | --- |
| `tests/test-qt-interface.rkt` | 194.891 | 92.4 | NQ | SF |
| `tests/test-qt-ci-surface.rkt` | 97.376 | 79.6 | NQ | SF |
| `tests/test-qt-quick.rkt` | 72.025 | 58.9 | NQ | SF |
| `tests/test-qt-lanes.rkt` | 68.511 | 53.6 | NQ | SF |
| `tests/test-qt.rkt` | 23.643 | 19.6 | NQ | SRC |
| `tests/test-qt-full.rkt` | 22.131 | 28.3 | NQ | SRC |

Each of these binds the `qt` script and passes it to a command runner, so a single fast execution contains a nested runner boot per case. WB4 owns the CLI contract family retier; the runner-boot amortization belongs there.

Text-only `./qt` references (assert or read qt text, do **not** spawn it — recorded as the `qt-text-reference` signal, never counted as runner duplication): `tests/test-run-tests-adapter-equivalence.rkt`, `tests/test-worker-security.rkt`, `tests/test-ci-workflows.rkt`, `tests/test-docs-accuracy.rkt`.

### 9.3 Suite-level duplication of real-world execution

- The `test-cross-version` job re-executes the **entire 1,329-file fast suite** on Racket 8.11 in one unsharded job: wall 3227.543 s, job elapsed 3666.0 s on the baseline run (2,141–3,666 s elapsed across the five runs) — the largest single-job cost on main CI, and a per-push duplication of every fast file.
- 46 fast files also run in `security`, 34 in `platform`, 10 in `workflows`, 1 in `smoke` (suite sizes: 64 / 38 / 36 / 1); their fast duration re-appears in those lanes.
- 89 files statically match nested `raco test` (1,795.0 s) and 23 match nested runner-script invocations (796.7 s); these signals are triage input (code-line matches with execution evidence), not proof — confirmation is each wave's matched measurement.

## 10. Reconciliation with the v1.00.28 census (1,177 files / 980.5 s)

The census measured a different inventory with a different metric; both facts matter:

- **Inventory:** census 1,177 selected files → today 1,329. Overlap is 1163 files; 14 census files are gone from the fast selection and 166 of today's files did not exist in the census (v1.00.29–v1.00.32 campaigns). Per binding amendment 4, no historical file identity was assumed to carry over.
- **Metric:** the census Σ is the sum of per-file **medians of 3 sequential samples** on a prepared, `--jobs 1` environment (dedicated, unloaded). Today's figure is the sum of **single CI samples** measured under each shard's 4-wide parallel execution. For the 1163 common files: Σ census medians = 873.1 s, Σ baseline CI durations = 2034.5 s — a **2.33× contention amplification** on identical work.
- Therefore `980.5 s → 6,511.6 s` is NOT a 6.6× suite-growth regression. Decomposed: ~873.1 s of the 980.5 s census mass belongs to files still selected (the other ~107 s left with the 14 removed files); the same 1,163 files now cost 2034.5 s in CI (≈ 2.33× measurement method), and the 166 new files add the rest. The census remains the reference for unloaded per-file cost; this report is the reference for what main CI actually pays per push.

## 11. Whole-inventory distributions (all 1,329 files)

| Dominant work kind | Files | Σ baseline (s) | % of mass |
| --- | --- | --- | --- |
| Verify-chain | 24 | 1352.087 | 20.76 % |
| nested ./qt runner | 6 | 478.577 | 7.35 % |
| nested raco test | 89 | 1795.028 | 27.57 % |
| nested runner script | 23 | 796.729 | 12.24 % |
| git fixture | 13 | 136.374 | 2.09 % |
| real wait | 48 | 192.699 | 2.96 % |
| pure | 1126 | 1760.135 | 27.03 % |

| Proposed destination | Files | Σ baseline (s) | % of mass | Saving upper bound (s) |
| --- | --- | --- | --- | --- |
| slow-full | 34 | 3782.138 | 58.08 % | 3919.035 if fully realized |
| short required contract | 114 | 830.377 | 12.75 % | — (unknown until each split is measured) |
| pure / keep | 1181 | 1899.114 | 29.16 % | 0 |

Risk distribution: 1113 low / 195 medium / 21 high. 98 files carry at least one unique `@covers` claim (105 uniquely-covered modules total); every such file requires the unique-claims review before re-tiering.

## 12. Limitations

- Five runs = five samples per file at best; one green run establishes no p95 and no flake rate. All p50/p90/p95 figures here are explicitly **sample-limited distribution points**, not tail estimates. Per-file stability claims need the WB waves' matched reruns.
- The five runs span four commits; `tests/test-docs-accuracy.rkt` (baseline only) and `tests/test-testing-audit.rkt` (4/5 runs) are the only coverage gaps, and Σ-duration spread (−18 % / +14 %) mixes commit drift with runner-host load variance. Medians/ranges damp but do not eliminate this.
- Work kinds come from static source signals (nested-runner strings, Verify-fixture requires, git-fixture helpers, `sleep` calls) over the baseline checkout; they are triage input, not runtime attribution — no runtime instrumentation exists (the v1.00.28 census counters are still `unknown`).
- Expected savings for split candidates are upper bounds; the real-world share of a file is unknown until its owning wave measures the split (no instrumentation was added in this phase).
- The attempt-1 job id of the superseded baseline shard-1 execution is not exposed by the jobs API; identification rests on artifact timestamps + verdict truth (§3.1/§3.2).
- Queue delay cannot be separated from needs-graph waiting with the data GitHub exposes; dispatch latencies are reported with their reference point labeled.

## 13. Verification and reproduction

1. Artifact listing: `GET /repos/coinerd/q/actions/runs/<id>/artifacts` for the five run ids in §3.1.
2. Download each authoritative artifact (auth redirect, then re-GET the signed location without auth headers); parse `test-results.json`.
3. Recompute: shard `run_summary` values (§3.1 table), Σ `files[].duration_seconds` = 6,511.629 s, top-k sums (§4), per-run sums (§6). All raw numbers in the JSON companion were produced by that computation; the aggregate tables were generated from the same data in one pass.
4. Consistency checks run before writing this report: 1,329 unique paths; shards disjoint (443/443/443); one authoritative artifact per (run, attempt, shard); no duplicate authoritative artifact ids; all 1,329 files present on disk at `22851554` with an ownership-matrix row.
5. The stale artifact `14393884631` is parsed only for §3.2 and excluded everywhere else.

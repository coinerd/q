# v1.00.28 Test Work Mass Report (W0 Runtime Census)

Schema: `q.census.runtime/1` · Census artifact:
`artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json` · Static scan:
`artifacts/test-runtime/v1.00.28-census/static-wait-scan.json` · Checksums:
`artifacts/test-runtime/v1.00.28-census/SHA256SUMS` · Authoritative contract:
`RUNTIME-AUDIT-SPEC-v1.00.28.md`.

## 1. Executive decision

The canonical `fast` inventory contains **1,177 selected files**; **1,171** produced
≥3 successful samples and **6** are retained explicit collection-failure records.
Total fast **work mass (Σ median) is 980,465 ms ≈ 980.5 s ≈ 16.3 min** of pure
per-test cost. The distribution is extremely long-tailed: the top 25 files carry
only **23.87%** of mass and the single worst file
(`tests/test-runner-scheduler-characterization.rkt`, 39,208 ms) carries 4.00%.
No single hotspot dominates; broad-scheduler wins are capped at ~15% for the top
ten combined. Seven files exceed the 10 s architecture-review threshold (queue
Q1). Runtime work-type counters are still `unknown` (opt-in instrumentation not
yet enabled), so W1 triage must start from the static wait/subprocess scan.

## 2. Measurement method

- Tool: `scripts/run-tests/runtime-census.rkt` (extends
  `scripts/run-tests/hotspot-benchmark.rkt` collection/canonicalization; inventory
  resolution via `scripts/run-tests/inventory.rkt`).
- Command: `racket scripts/run-tests/runtime-census.rkt --samples 3 --jobs 1`
  (recorded in the census JSON `command` field).
- Sampling: 3 successful samples per file, median/p95 with the same linear
  interpolation as the hotspot tooling, canonical prepared environment.
- Metadata: per-test `@speed`/`@suite`/`@boundary`/`@covers` parse from
  `test-metadata.rkt`.
- Work-type counters: opt-in dynamic scopes only; uninstrumented counters
  serialize as `null` / aggregate `unknown` — never `0`.
- Static companion scan: RUNTIME-AUDIT-SPEC §5 term list over the whole test
  tree (`static-wait-scan.json`; matches are triage input, not proof of defect).
- Census `--check` regenerates canonical bytes and verifies SHA256SUMS.

## 3. Inventory validation

| Check | Result |
| --- | --- |
| Inventory-selected files | 1,177 |
| Files measured (≥3 samples) | 1,171 |
| Explicit collection-failure records (retained) | 6 |
| Completeness errors (disk↔census mismatch) | 0 — every on-disk `fast` file has a record or explicit failure |
| Record statuses | 1,171 `pass` · 6 `collection-failure` |

## 4. Fast work mass

**Σ median = 980,465 ms (≈ 16.3 minutes).** Bucket split (§4.3):

| Bucket | Files | Mass (ms) | Mass share |
| --- | --- | --- | --- |
| < 250 ms | 37 | 8,388 | 0.86% |
| 250 ms – 1 s | 951 | 476,530 | 48.60% |
| 1 – 2 s | 117 | 146,018 | 14.89% |
| 2 – 5 s | 45 | 133,852 | 13.65% |
| 5 – 10 s | 14 | 94,648 | 9.65% |
| > 10 s | 7 | 121,029 | 12.34% |

## 5. Pareto table

| Rank set | Files | Mass (ms) | % of work mass |
| --- | --- | --- | --- |
| Top 1 | 1 | 39,208 | 4.00% |
| Top 10 | 10 | 147,403 | 15.03% |
| Top 25 | 25 | 234,054 | 23.87% |
| Top 50 | 50 | 313,225 | 31.95% |
| Top 100 | 100 | 403,953 | 41.20% |
| Remainder | 1,071 | 576,512 | 58.80% |

## 6. Runtime buckets

See §4 table. 80.9% of files (951 + 37) sit at or below 1 s, but they account for
49.5% of mass; the 66 files ≥ 2 s account for 35.6% of mass.

## 7. Boundary split

| Boundary | Files | Mass (ms) | Mass share |
| --- | --- | --- | --- |
| `fast/unit` | 866 | 625,750 | 63.82% |
| `fast/integration` | 254 | 290,485 | 29.63% |
| `fast/other` | 51 | 64,230 | 6.55% |
| `unit-fast/unit` | 0 | 0 | 0.00% |

## 8. Process amplification

Runtime `subprocesses` counters: **unknown** (instrumentation not opted in —
`null`, never 0). Static §5 scan triage: 74 `subprocess`, 151 `system`,
10 `system*`, 17 `system*/exit-code`, 18 `subprocess-wait` term matches across
706 files with ≥1 term match (1,609 total matches, 22 distinct terms). Queue Q6
(process amplification) is empty pending W1 instrumentation; the scan is the
triage input.

## 9. Git/fixture amplification

Runtime counters `git_fixtures`, `session_fixtures`, `worktrees`, `temp_dirs`,
`temp_files`: **unknown**. Static triage: 60 `git`, 294 `make-temporary-file`,
249 `delete-directory/files`, 88 `make-directory*`, 48 `make-directory` term
matches. Queue Q5 empty pending instrumentation.

## 10. Wait audit

Requested real sleep counters: **unknown** at runtime. Static scan: 106 `sleep`
term matches, plus 192 `timeout`, 74 `retry`, 36 `sync/timeout`, 13 `backoff`,
9 `poll`, 28 `thread-wait`, 12 `tcp-listen`, 1 `alarm-evt`,
17 `current-milliseconds`, 112 `current-seconds`. Queue Q4 (real-time wait
candidates in `fast`/`unit-fast`) is empty at the metadata level; W1 must
re-derive it from the static scan before treating fast sleeps as absent.

## 11. Grouped-execution candidate mass

Queue Q7: **710 files, 419,427 ms = 42.78% of work mass** shows low
isolation-risk signals (fast/unit-dominated, no collection failure) — the
group-safe candidate pool.

## 12. Top 25 annotated hotspots

| # | File | Median (ms) | % mass | Attribution (§8) |
| --- | --- | --- | --- | --- |
| 1 | tests/test-runner-scheduler-characterization.rkt | 39,208 | 4.00 | UNKNOWN |
| 2 | tests/test-gsd-delivery-verifier.rkt | 18,465 | 1.88 | UNKNOWN |
| 3 | tests/test-golden-flows.rkt | 14,467 | 1.48 | UNKNOWN |
| 4 | tests/test-run-tests-profiles.rkt | 13,812 | 1.41 | UNKNOWN |
| 5 | tests/test-runner-work-queue.rkt | 12,537 | 1.28 | UNKNOWN |
| 6 | tests/test-milestone-gate.rkt | 11,988 | 1.22 | UNKNOWN |
| 7 | tests/test-runner-grouped-characterization.rkt | 10,552 | 1.08 | UNKNOWN |
| 8 | tests/test-run-tests-overhead-diagnostics.rkt | 9,690 | 0.99 | UNKNOWN |
| 9 | tests/test-test-gate-ownership.rkt | 8,724 | 0.89 | UNKNOWN |
| 10 | tests/test-hotspot-report.rkt | 7,960 | 0.81 | UNKNOWN |
| 11 | tests/test-audit-v09945-w10-integration.rkt | 7,784 | 0.79 | UNKNOWN |
| 12 | tests/test-runtime-census.rkt | 7,407 | 0.76 | UNKNOWN |
| 13 | tests/test-midstream-stall.rkt | 6,724 | 0.69 | UNKNOWN |
| 14 | tests/test-registry-watcher.rkt | 6,570 | 0.67 | UNKNOWN |
| 15 | tests/test-gsd-branch-delivery-verification.rkt | 6,407 | 0.65 | UNKNOWN |
| 16 | tests/test-tool-call-freeze.rkt | 6,065 | 0.62 | UNKNOWN |
| 17 | tests/test-gateway-ipc-concurrent.rkt | 6,030 | 0.62 | UNKNOWN |
| 18 | tests/test-prepared-env-report.rkt | 5,655 | 0.58 | UNKNOWN |
| 19 | tests/test-iteration-transitions.rkt | 5,539 | 0.56 | UNKNOWN |
| 20 | tests/test-conn-pool.rkt | 5,055 | 0.52 | UNKNOWN |
| 21 | tests/test-stream-heartbeat-metadata.rkt | 5,038 | 0.51 | UNKNOWN |
| 22 | tests/test-run-tests-ledger.rkt | 4,767 | 0.49 | UNKNOWN |
| 23 | tests/test-adaptive-retry.rkt | 4,608 | 0.47 | UNKNOWN |
| 24 | tests/test-release-build-manifest-integration.rkt | 4,502 | 0.46 | UNKNOWN |
| 25 | tests/test-bash-background-child.rkt | 4,500 | 0.46 | UNKNOWN |

Attribution is `UNKNOWN` per §8 allowance: runtime counters are uninstrumented,
so no category may be asserted yet.

## 13. Remediation queue

- **Q1 — fast > 10 s (mandatory architecture review): 7 files** — ranks 1–7 in
  §12 (runner-scheduler-characterization, gsd-delivery-verifier, golden-flows,
  run-tests-profiles, runner-work-queue, milestone-gate,
  runner-grouped-characterization).
- **Q2 — fast 5–10 s (optimize or justify): 14 files** — ranks 8–21 in §12.
- **Q3 — `fast + integration` boundary-realism review: 0 files.**
- **Q4 — real-time wait candidates: 0 files** (metadata-level; see §10 caveat).
- **Q5 — fixture amplification: 0 files** (pending instrumentation; static triage
  in §9).
- **Q6 — process amplification: 0 files** (pending instrumentation; static triage
  in §8).
- **Q7 — grouped-execution candidates: 710 files / 419,427 ms (42.78%).**

## 14. Reproducibility

1. `racket scripts/run-tests/runtime-census.rkt --samples 3 --jobs 1`
2. `racket scripts/run-tests/runtime-census.rkt --check
   artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json`
3. `sha256sum -c artifacts/test-runtime/v1.00.28-census/SHA256SUMS`

Census `q_sha`: `74cff2f892c2afd2db36b5114bc45191a5d1a460`. Static scan: 1,470
files scanned, 706 with ≥1 §5 term match, 1,609 matches, 22 distinct terms,
schema `q.census.static-wait-scan/1`.

## 15. Checksums

Bound in `artifacts/test-runtime/v1.00.28-census/SHA256SUMS`
(census JSON, static scan JSON, this report). `--check` re-verifies canonical
bytes at every invocation.

## 16. Limitations

- Work-type counters (§4.5) are `unknown` everywhere: opt-in dynamic scopes are
  defined but not yet enabled on project-owned fixture/process/git helpers, so
  §10 questions 3–5 are answered only as static-scan triage.
- 6 collection failures are retained as explicit records, not durations.
- Static scan matches are keyword occurrences, not runtime behavior.
- Timing medians carry normal CI-machine variance; `--check` compares canonical
  bytes, not re-measured wall time.

---

## §10 Audit completion gate — the seven questions

1. **Top-25 share of fast work mass?** **23.87%** (234,054 / 980,465 ms; §5).
2. **Share from `fast + integration`?** **29.63%** (254 files,
   290,485 / 980,465 ms, boundary split §7; suite-cross-reference queue Q3 = 0
   files tagged both suites).
3. **Requested real sleep in fast?** **Unknown at runtime** (counters
   uninstrumented → `null`, never 0); static triage: 106 `sleep` term matches
   (§10). W1 must instrument before this number is authoritative.
4. **Files spawning the most subprocesses?** **Unknown at runtime**; static
   triage: 74 `subprocess` term matches for W1 ranking (§8).
5. **Files constructing the most Git/session fixtures?** **Unknown at runtime**;
   static triage: 60 `git`, 294 `make-temporary-file`, 249
   `delete-directory/files` matches (§9).
6. **Share of fast work mass that appears group-safe?** **42.78%**
   (710 files, 419,427 / 980,465 ms; Q7, §11).
7. **Ten highest-expected-payoff remediations?** The top ten of §12 in rank
   order — runner-scheduler-characterization (39,208 ms, 4.00%),
   gsd-delivery-verifier (18,465), golden-flows (14,467), run-tests-profiles
   (13,812), runner-work-queue (12,537), milestone-gate (11,988),
   runner-grouped-characterization (10,552), run-tests-overhead-diagnostics
   (9,690), test-gate-ownership (8,724), hotspot-report (7,960) — together
   147,403 ms = **15.03%** of fast work mass; the first seven are the Q1
   mandatory architecture reviews.

## 17. W7 remeasurement (post W1–W6 remediation) — W0 baseline rows above are never rewritten

Re-run of the census with the identical W0 method (same runner, environment class,
sample floor, and checksummed artifacts; `compiled/` purged before the run so the
census measures the checked-out sources). Baseline = the W0 census
(`artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json`, checksummed in
§15); post = `artifacts/test-runtime/v1.00.28-w7/fast-runtime-census.json`.
Comparison tool: `scripts/run-tests/work-mass-comparison.rkt`
(`--baseline` / `--post` / `--out`, with `removed-since-baseline.json` as the
removals manifest; it asserts inventory equality — a file present in W0 but
silently absent in W7 is a red error) and the percentage math is checked
against the stored medians by `tests/test-work-mass-comparison.rkt`.

### 17.1 Comparison rows (W0 → W7)

| Row | W0 baseline | W7 post | Δ | Δ % |
|-----|-------------|---------|---|-----|
| Fast work mass (ms) | 980,465 | 927,785 | −52,680 | **−5.37 %** |
| Top 1 contribution (ms) | 39,208 | 38,582 | −626 | −1.60 % |
| Top 10 contribution (ms) | 147,403 | 146,171 | −1,232 | −0.84 % |
| Top 25 contribution (ms) | 234,054 | 230,191 | −3,863 | −1.65 % |
| Top 50 contribution (ms) | 313,225 | 299,820 | −13,405 | −4.28 % |
| Top 100 contribution (ms) | 403,953 | 382,883 | −21,070 | −5.22 % |
| Bucket < 250 ms (files) | 37 | 75 | +38 | — |
| Bucket 250 ms–1 s (files) | 951 | 948 | −3 | — |
| Bucket 1–2 s (files) | 117 | 89 | −28 | — |
| Bucket 2–5 s (files) | 45 | 41 | −4 | — |
| Bucket 5–10 s (files) | 14 | 12 | −2 | — |
| Bucket > 10 s (files) | 7 | 8 | +1 | — |
| Total process launches | unknown | unknown | unknown | unknown |
| Git command launches | unknown | unknown | unknown | unknown |
| Fixture constructions | unknown | unknown | unknown | unknown |
| Total requested real sleep (ms) | unknown | unknown | unknown | unknown |
| Grouped-safe share (% of fast mass / files) | 42.78 % (710 files, 419,427 ms) | 43.84 % (711 files, 406,778 ms) | +1.06 pp | — |

The four runtime counters stay `unknown` on both sides on purpose: W0 and W7 use
the identical method, and instrumenting launch counters mid-series would make the
rows non-comparable. The W0 static triage stands as the only available proxy:
wait-pattern matches 1,609 (706 files) at W0 vs 1,630 (710 files) at W7
(`static_scan` aggregate) — no material movement, consistent with the W1 finding
that asked-for sleeps are not where fast-lane time goes.

Inventory equality holds: 2 files present at W0 are absent at W7, both explicit
old-campaign residue removals recorded in
`artifacts/test-runtime/v1.00.28-w7/removed-since-baseline.json`
(`tests/test-browser-audit-w1-v0984.rkt`, `tests/test-browser-audit-w2-v0983.rkt`).
No silent drops. In the other direction, 5 files exist at W7 that were absent at
W0; each carries an explicit added row in the census (4 new campaign tests plus
`tests/test-deterministic-clock.rkt`, recorded as a collection-failure row).

### 17.2 Workload verdict (intermediate decision guide)

Fast work mass improved **5.37 %** (980,465 → 927,785 ms). Per the intermediate
guide (< 10 % insufficient; 10–25 % partial; 25–40 % meaningful; ≥ 40 % strong):
**INSUFFICIENT**. This is an intermediate result against the intermediate guide
only; it does not replace or relax the fixed final gate of this campaign.

### 17.3 Next bottleneck (recorded decision)

Work mass did **not** improve materially, so the next lever is **another targeted
test-design pass** on the remaining hotspot mass — specifically the top-10 files,
which are barely moved at 146,171 ms (15.7 % of the post mass; W1–W6 remediations
bought −5.37 % overall but left the head of the Pareto curve nearly frozen).
Scheduling, sharding, and worker-count changes are explicitly NOT the accepted
next step: more workers would only hide an unchanged workload. Candidate targets
for the next remediation round, in order:
`tests/test-runner-scheduler-characterization.rkt` (38,582 ms, still the single
largest file), the top-10 block, and the eight > 10 s files (128,186 ms post).


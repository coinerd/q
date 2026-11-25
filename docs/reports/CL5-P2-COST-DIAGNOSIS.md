# CL5 P2-A — why the comprehensive measurement exceeded 3 600 s

Scope: diagnosis only. Nothing here changes what CI runs, what "comprehensive" means, or the
selector. The measurement this document characterises is the *existing* production surface,
executed by the *existing* production runner with the *existing* CI settings.

Host: 4 vCPU, 7 GB RAM, `nproc` = 4, baseline load average 0.9–2.0 with no measurement running.
Revision measured: `dad0a9b94b3e4decaa5b458f05c15e4c19d881e3` (tree `222ef2b19fdc6698267ac4d5fad74696c7b8d168`).

## 1. The two surfaces are not the same workload

| Surface | Files | What it is | Where it comes from |
|---|---|---|---|
| **C** — production comprehensive | **1 397** | The union of every CI test producer at this revision: fast 0–2, platform, security, workflows 0–1, smoke | `./qt --lane <lane> --shard-index/--shard-total --explain` for each producer (`scripts/cl5/ci-producers.rkt`) |
| **L** — local `./qt full` | **1 602** | "Complete available Racket suite"; everything C has plus 205 more | `./qt full --explain` |

`C ⊂ L`: zero files of C fall outside L; L adds 205 files that no CI producer runs.

Decisive detail: **all 17 files that declare `@timeout ≥ 1200 s` live in that 205-file difference.**
None of them is in C. The declared-timeout distribution over the whole test tree is
23×300 s, 6×420, 1×480, 13×600, 10×900, 7×1200, 3×1500, 1×1800, 1×2400, 2×3600, 2×7200, **1×14400**.
CI's comprehensive surface is bounded by ≤ 900 s per file; `./qt full` is bounded by 4 h per file.

## 2. What the two failed runs were actually doing

**P-1 (the original 3 600 s probe, feasibility wave).** `./qt full` at the historical tree
`2ca8d0bb`, default `--jobs 2`, no compiled cache. Killed at 3 600 s (3 733 s elapsed) still inside
execution. Surface L, half the workers, cold cache, unbounded tail.

**P-2 (this wave, at `dad0a9b9`, `--jobs 4`, cold cache).** The production runner's preparation step
(one bulk `raco make` over the selected closure) took **919 115 ms ≈ 15.3 min** for 1 601 targets /
2 556 modules. Execution then ran 34 files before reaching
`tests/test-gsd-supplemental-disposition-campaign.rkt`, which declares `@timeout 14400` and was still
running after 34 minutes; the run was stopped deliberately. Two independent costs are visible here:
a cold-cache preparation phase that alone consumes 25 % of a one-hour budget, and a single file that
can legally consume four hours.

**Structural serialisation.** The runner dispatches mutation-owning files first under a
`repository-exclusive` resource — 70 files in L, 5 per C fast shard — so the opening phase of every
full run is serial by design (the rule exists to stop a mutation being observed by another test; it
is not overhead that can be optimised away without changing the safety model).

## 3. The production surface *is* measurable: 4 501 s on this host

Eight producer invocations, CI's own settings (fast `--jobs 4` ×3 shards, platform `--jobs 4`,
security `--jobs 4`, workflows `--jobs 2` ×2 shards, smoke's declared command), warm cache, one
invocation each:

| Producer | Files | Wall (s) | Prep (s) | Σ per-file (s) | Max file (s) | Parallel efficiency | Verdict |
|---|---|---|---|---|---|---|---|
| fast-0 | 451 | 931.0 | 95.6 | 2 786 | 171 | 3.33 / 4 | **incomplete** (1 timeout) |
| fast-1 | 451 | 1 097.9 | 91.6 | 3 049 | 201 | 3.03 / 4 | pass |
| fast-2 | 450 | 1 980.5 | 177.2 | 4 031 | 245 | 2.24 / 4 | **incomplete** (1 timeout) |
| platform | 38 | 74.8 | 9.6 | 113 | 13 | 1.74 / 4 | pass |
| security | 64 | 142.2 | 20.7 | 372 | 38 | 3.06 / 4 | pass |
| workflows-0 | 19 | 123.1 | 17.1 | 155 | 45 | 1.47 / 2 | pass |
| workflows-1 | 18 | 150.3 | 12.7 | 160 | 107 | 1.16 / 2 | pass |
| smoke | 1 | 1.5 | — | 2 | 2 | 1.00 / 1 | pass |
| **Σ = C_comp** | **1 397 unique / 1 492 slots** | **4 501.4** | 424.5 | 10 668 | — | — | **degraded** |

Two files timed out on this host, through two different mechanisms: `tests/test-gsd-executor-ergonomics-prompt.rkt`
hit the harness default of 120 s, and `tests/test-gsd-evidence-repair-production-contract.rkt` hit its
own declared `@timeout 210` (elapsed 210 062 ms). CI passes both. They are recorded as `incomplete`,
never as passes, and the record's comprehensive block therefore carries a **derived**
`status: "degraded"` with machine-readable reasons (`fast-0: 1 timeout(s)`, `fast-2: 1 timeout(s)`) —
the wall time is a measurement of the surface, but the surface did not fully pass here.

Per-file evidence (1 492 entries: path, elapsed ms, status) and every derived aggregate are in
`docs/reports/CL5-P2-COST-dad0a9b9.json`, re-derivable by `scripts/cl5/cost-record-cli.rkt --verify`.
The record also re-derives the surface shape it measured — `unique_files = 1397`, `slot_count = 1492`,
the 95 duplicated slots, `status = degraded` and its reasons — so the surface claims in §1 are
checkable against the record itself, not only against a re-run of `--explain`.

## 4. Answering the question directly

| Hypothesis | Verdict | Evidence |
|---|---|---|
| Normal full-suite cost | **Yes, for surface L** | 1 602 files × ~6.7 s mean serial ≈ 10 700 s of work; at `--jobs 2` that is ~5 300 s minimum, before the unbounded tail |
| A pathological long-running test | **Yes, and it is structural** | the 4 h-declaring file is in L but not in C; 17 such files exist, all outside C |
| Contention | **No** | baseline load 0.9–2.0 on 4 cores; the measured runs saturate the host (load ~5) as expected; parallel efficiency 2.2–3.3 of 4 |
| Test-runner overhead | **Partly** | bare `racket -e '(void)'` = 0.98 s; cold bulk preparation = 919 s (15.3 min) once per cold tree; warm preparation 10–177 s per producer; Σ per-file 10 668 s vs 4 501 s wall |

So the 3 600 s timeout was **not a defect and not contention**: it was surface L with cold cache at
half the workers, a surface that includes every long-tail integration test CI deliberately excludes.
The production-equivalent comprehensive surface completes in **75 minutes** on this host.

## 5. What remains unmeasured (honest gaps)

1. **Surface L has no completed measurement.** It is the developer-facing question ("how long does
   local verification take?"), and it is a different question from C_comp. Measuring it needs a
   declared timeout cap for the long-tail files, which is a design decision, not a diagnosis.
2. **One invocation each; no repeats.** Variance is unknown; the design pre-registers repeats.
3. **Single host.** 4 vCPU / 7 GB; nothing here establishes a number for another machine.
4. **Warm cache.** The 4 501 s is a warm-cache figure; cold adds ~919 s for the closure (measured on
   L, not on C).
5. **Two timeouts** on this host make this record `degraded`.

## 6. Reproduce

```bash
# surfaces
./qt full --explain | grep -c '^tests/'                      # 1602
for s in "fast 0 3" "fast 1 3" "fast 2 3" "platform 0 1" "security 0 1" \
         "workflows 0 2" "workflows 1 2"; do set -- $s
  ./qt --lane $1 --shard-index $2 --shard-total $3 --explain | grep '^tests/'
done | sort -u | wc -l                                        # 1397 (+ smoke's declared file)

# producers (CI's own settings)
./qt --lane fast       --jobs 4 --shard-index 0 --shard-total 3 --json-out run.json
./qt --lane fast       --jobs 4 --shard-index 1 --shard-total 3 --json-out run.json
./qt --lane fast       --jobs 4 --shard-index 2 --shard-total 3 --json-out run.json
./qt --lane platform   --jobs 4 --json-out run.json
./qt --lane security   --jobs 4 --json-out run.json
./qt --lane workflows  --jobs 2 --shard-index 0 --shard-total 2 --json-out run.json
./qt --lane workflows  --jobs 2 --shard-index 1 --shard-total 2 --json-out run.json
raco test tests/test-provider-smoke.rkt

# record
racket scripts/cl5/cost-record-cli.rkt --from-spec docs/reports/CL5-P2-COST-SPEC-dad0a9b9.json \
  --out docs/reports/CL5-P2-COST-dad0a9b9.json
racket scripts/cl5/cost-record-cli.rkt --verify docs/reports/CL5-P2-COST-dad0a9b9.json
```

The committed record's `run_json` inputs are the raw `--json-out` files from that session; the record
embeds the per-file evidence, so verification does not depend on them. The spec is provenance for
the exact commands and measured walls.

## 7. Consequence for the efficiency gate

M6 is **no longer `unavailable` on measurement grounds**: a comprehensive reference workload can be
executed and timed on a matched host, and the record/verifier make the number auditable. What this
diagnosis does *not* establish is an efficiency result: there is no population (P3), no selected-arm
comparison, and this record is `degraded` by two timeouts. Those remain open.

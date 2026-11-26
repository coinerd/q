# CL5 P2-B — comprehensive cost measurement: design, revision 2

Status: **rev 2 incorporates the owner's five decisions and two amendments (2026-11-25).** Nothing
here changes what CI runs or what "comprehensive" means; the surfaces stay exactly the ones that
exist today. Rev 2 is the document submitted for independent approval. Evidence: P2-A
(`CL5-P2-COST-DIAGNOSIS.md`, record `CL5-P2-COST-dad0a9b9.json`) and the timeout resolution
(`CL5-P2-TIMEOUT-RESOLUTION.md`).

Changes from rev 1 are marked **[rev2]**.

## 1. Surfaces (unchanged)

| Surface | Question | Definition | Producer |
|---|---|---|---|
| **C** | "What does full verification cost before a push?" | Union of the CI producers at revision *r*: fast 0–2, platform, security, workflows 0–1, smoke | `./qt --lane …` with CI's own flags, plus the smoke step's declared command |
| **L** | "What does a developer's `./qt full` cost?" | `./qt full`'s selection at *r* (superset of C) | `./qt full --jobs n` |

At `dad0a9b9`: C = 1 397 unique files / 1 492 producer executions; L = 1 602 files.

## 2. Metrics **[rev2]**

Two metrics, never substituted for one another:

1. **Primary — elapsed wall time of one complete CI-equivalent execution schedule.**
   *Schedule* = a single invocation of the production adapter over the exact surface, on the
   measurement host: `racket scripts/run-tests/runner.rkt <exact file list> --jobs <n>` (or the
   `./qt` lane form where the lane *is* the surface). `n` = `nproc` unless a hypothesis fixes another
   value, and the same `n` is used for both arms. The reportable number is the wall clock of that one
   invocation, with its `preparation` phase reported separately.
2. **Secondary — resource cost.** `C_comp_resource = Σ per-producer wall` (the 8-producer record:
   4 501.4 s at `dad0a9b9`). This is a cost-accounting figure, **not** a substitute for elapsed time
   and never reported as one.

**Slot accounting [rev2].** Work and cost accounting count **1 492 executions**; coverage accounting
counts **1 397 unique files**. Every report states both, labelled. Overlapping files are charged
twice because CI pays them twice; they are counted once in coverage.

**Smoke [rev2].** The measured smoke step is the **exact command declared by the workflow**, verified
against the pinned workflow definition by
`tests/test-cl5-baseline.rkt` ("smoke is the file named by ci.yml's own command"). The broader
`--lane smoke` selection is **not** used for measurement; it selects more files than CI runs.

## 3. Matched-host and cache conditions **[rev2]**

1. **Comparability precondition.** A wall-time speedup claim requires the comprehensive and selected
   workloads to be executed on the same host, in the same session, with the same adapter, the same
   `--jobs`, the same cache label and the same timeout policy. If those conditions cannot be met, the
   result is **resource cost only** — no wall-time speedup claim is made from it.
2. **Paired measurements [rev2].** Arms are measured as pairs in one session, in alternating order
   across repeats (A,B then B,A), so drift affects both arms. Every individual timing is reported;
   nothing is averaged across a spread above the tolerance.
3. **Cache [rev2].** **Warm cache is the primary incremental-verification metric.** Cold-start
   preparation and cold total cost are reported separately. A warm selected run is **never** compared
   with a cold comprehensive run.
4. **Quiet host.** No other measurement, build or suite runs concurrently; load average before and
   after each invocation is recorded, and a run started above the pre-registered load ceiling is
   repeated.
5. **Selection overhead charged.** The selected arm includes the cost of computing its selection
   (`--explain`/planning), so the selector pays for its own decision.

## 4. Timeouts and censorship **[rev2]**

1. Per-file timeouts are the harness's existing policy: declared `@timeout`, else 120 s. No
   measurement invents its own cap.
2. A file that hits its timeout is `timeout`/`incomplete`; a producer with any timeout is never
   `pass`; the record's comprehensive block carries a **derived** `status` (`ok` | `degraded`) plus
   `status_reasons`, re-checked by the verifier.
3. **Censoring rule [rev2].** Caps: **7 200 s per C producer**, **14 400 s for L**. A run that exceeds
   its cap is **censored**: recorded as `unavailable` with the phase reached and the file count
   completed. A censored run is **not a completed measurement** and **cannot support a GO decision**;
   it may be reported as evidence about cost.
4. A run killed by an operator timeout is not a measurement at all.
5. **Timeout resolution [rev2/rev3].** Before any cost-based GO, the timeouts observed on the
   reference host must be resolved: reproducible and passing under the official surface's declared
   limits, or the affected surface is `degraded` and therefore inadmissible for a GO. Tests are not
   removed, timeouts are not silently extended, and the comprehensive set is not redefined to obtain a
   green measurement. Any timeout-policy change is a separate, reviewed change
   (`CL5-P2-TIMEOUT-RESOLUTION.md` records the reproducibility investigation;
   `CL5-P2C-PROFILING.md` records the runtime investigation and finds no removable work;
   `CL5-P2C-TIMEOUT-MARGIN-POLICY.md` is the proposal awaiting review, **not in force**).
   **rev3 addition:** per-file timeouts are wall-clock limits applied under the surface's concurrency,
   so a margin derived from standalone runs is unsound — the ergonomics file sits at 58 % of its limit
   standalone yet exceeded it at 4 workers. Any accepted policy must derive margin at the measurement's
   own job count.
6. **Warm-up precondition [rev3].** A measurement session begins with one recorded successful warm-up
   invocation of the surface. A preparation-phase failure (observed once during P2-C, from a
   probably-stale compiled artifact) invalidates the session rather than being counted as a test
   timeout.

## 5. Evidence and verification (unchanged mechanics, rev2 acceptance)

- One record per session: `docs/reports/CL5-P2-COST-<rev8>.json`, schema `cl5-cost/1`, plus the spec
  (`CL5-P2-COST-SPEC-<rev8>.json`). Per-file evidence embedded; every aggregate re-derived by
  `racket scripts/cl5/cost-record-cli.rkt --verify REC` (fail-closed: counts, serial, max, timeouts,
  slot/unique counts, comprehensive sum, derived status, no-pass-with-timeout).
- Acceptance **[rev2]**: verifier passes; **two paired invocations minimum**; a spread above **15 %**
  triggers a **third** run rather than an average; the GO arm uses the **slower** wall of its
  accepted runs, applied to **both** arms consistently; all individual timings, the load record and
  the cache label are reported; a `degraded` or censored result is disclosed next to any number it
  qualifies.
- A record whose selection does not match the revision's own producer file lists is refused.

## 6. What this design does not authorise

- No CI change (producer set, workflows, gate semantics).
- No production selector, scheduler or admission change.
- No bounded substitute suite. If one is ever proposed it needs its own review plus a coverage
  analysis, and it may not silently replace C in a safety or efficiency argument.
- No efficiency or GO claim from P2 alone: P2 supplies the instrument. A GO additionally needs the P3
  population, a selected arm measured under §3, and the timeout resolution in §4.5.
- No reuse of W3a's historical corpus, and no reuse of mutant timings as evidence about ordinary
  development changes.

## 7. Decisions taken (was: open questions) — rev 2

| Rev-1 question | Decision (owner) | Implementation |
|---|---|---|
| Σ-producers vs single invocation | **Elapsed wall of a complete CI-equivalent schedule is primary; Σ is resource cost** | §2.1 / §2.2 |
| 1 492 slots vs 1 397 unique | **Both, labelled**: slots for work/cost, unique for coverage | §2 "Slot accounting" |
| Smoke | **Exact CI-declared command**, pinned against the workflow | §2 "Smoke" |
| L long tail | **Keep caps 14 400 s (L) / 7 200 s (C)**; censored ⇒ not a completed measurement, no GO | §4.3 |
| Warm vs cold | **Warm primary** for incremental verification; cold reported separately; never mixed | §3.3 |
| *(amendment)* comparability | No wall-time speedup claim unless both arms run under comparable host/concurrency | §3.1 |
| *(amendment)* paired runs | Paired, alternating order, individual timings, consistent slower-time rule, variance shown | §3.2 / §5 |

## 8. Residual risks

1. One host class (4 vCPU / 7 GB); no claim transfers to another machine.
2. Warm-cache numbers depend on the preparation step having run in the same session; the record
   states the label and the preparation time.
3. The two observed timeouts make the current C record `degraded`; §4.5 governs resolution.
4. Execution-infrastructure improvements (cold-start preparation, scheduling, long-tail tests) are a
   **separate** concern. They must not be folded into a selection-speedup claim, and no full-suite
   optimisation is undertaken before representative selected-arm measurements exist.

# CL5 P2-B — comprehensive cost measurement: design for review

Status: **proposal, not in force.** Nothing in this document changes what CI runs or what
"comprehensive" means. Its approval is a prerequisite for using any cost number in a decision.
P2-A (`CL5-P2-COST-DIAGNOSIS.md`) supplies the evidence this design is built on.

## 1. Two questions, two surfaces

The owner's split is adopted literally: PR-CI latency and developer-facing local verification are
different workloads and must not be merged into one number.

| Surface | Question it answers | Definition | Producer |
|---|---|---|---|
| **C** (comprehensive / CI) | "What does full verification cost before a push?" | Union of the CI test producers at revision *r*: fast 0–2, platform, security, workflows 0–1, smoke | `./qt --lane …`, CI's own flags, plus the smoke step's declared command |
| **L** (local full) | "What does a developer's `./qt full` cost?" | `./qt full`'s own selection at *r* (a superset of C) | `./qt full --jobs n` |

`C_comp(r)` is defined as the sum of the per-producer wall clocks when each producer is executed once
by the production runner on the reference host — because that is what CI does, one job per producer.
`C_full(r)` is one `./qt full` invocation. They are never mixed, and a claim must name its surface.

Explicitly rejected: defining a "bounded comprehensive" suite with fewer files. If such a suite is
ever wanted, it is a separate proposal with a coverage analysis, and it may not silently substitute
for C in a safety or efficiency argument.

## 2. Measurement conditions (all required, all recorded)

1. **Matched host.** All arms of one comparison run on the same physical host inside one session.
   The record carries `nproc`, total RAM, kernel, and the load average before/after each invocation.
   Cross-host comparison is invalid; CI durations are never substituted for a local measurement.
2. **Workers fixed by surface.** C uses CI's own `--jobs` (fast/platform/security 4, workflows 2);
   L uses the developer default unless the hypothesis names another value. A comparison between two
   arms must use identical `--jobs`.
3. **Cache label.** `cold` = no compiled cache for the tree; `warm` = a preparation step has already
   run. Both arms of a comparison share the label. Cold cost is reported separately (preparation
   phase) and never mixed into a warm number.
4. **Phase decomposition.** Every record keeps `preparation_ms` (the runner's bulk compile) separate
   from per-file `elapsed_ms`. Preparation is reported, not hidden.
5. **Selection overhead included.** For a selected arm, the time to compute the selection
   (`--explain`/planning) is part of that arm's cost, so the comparison charges the selector for its
   decision.
6. **Quiet host.** No other measurement, build or test suite runs concurrently. Load before each
   invocation is recorded; a run started above a pre-registered load ceiling is repeated.

## 3. Timeouts and overrun policy (the part the owner asked to be explicit)

1. Per-file timeouts are the harness's existing policy: declared `@timeout` metadata, else 120 s.
   A measurement does not invent its own cap.
2. A file that hits its timeout is `timeout`/`incomplete`. A producer with any timeout is never
   `pass`; the record's comprehensive block is marked `degraded`.
3. **A `degraded` C_comp may be reported but may not support a GO** until either (a) the timeouts are
   resolved on the reference host, or (b) the failing files are pre-registered as out-of-surface for
   the experiment — which is a protocol change, not a measurement convenience.
4. Per-producer wall-clock cap: pre-registered at **7 200 s** for C producers and **14 400 s** for L.
   Exceeding the cap ⇒ the producer is `unavailable` with the phase reached and the number of files
   completed. No extrapolation, no partial-sum-as-total, no substitution from another host.
5. A run killed by an external timeout (the operator's `timeout` command) is **not a measurement**
   and cannot be recorded as one; only the harness's own verdict counts.

## 4. Evidence format and verification

- One JSON record per measurement session: `docs/reports/CL5-P2-COST-<rev8>.json`, schema
  `cl5-cost/1`, plus the spec that produced it (`CL5-P2-COST-SPEC-<rev8>.json`).
- The record embeds per-file evidence (path, elapsed_ms, status) for every producer, its wall clock,
  preparation time, and the host/cache descriptors.
- `racket scripts/cl5/cost-record-cli.rkt --verify REC` re-derives, fail-closed: declared file counts,
  Σ per-file, max per-file, timeout counts, the comprehensive sum, duplicate entries, and the
  no-pass-with-timeout rule. A record that does not re-derive is refused, not corrected.
- Committed revision and tree SHAs identify the workload; a record whose selection does not match the
  revision's own producers is refused (the producer file lists are the evidence).

## 5. Acceptance criteria for a usable C_comp

1. The record verifies.
2. **Two invocations per producer**, same session, same cache label; both are reported. The GO arm
   uses the **slower** of the two walls; a spread above 15 % is reported as instability and triggers a
   third run rather than an average.
3. Host and cache descriptors present; load ceiling respected.
4. `degraded` status disclosed next to the number wherever the number is used.
5. The record is reproducible from the committed spec (same commands, same flags).

## 6. What this design does not authorise

- No change to CI: the producer set, workflows and gate semantics stay as they are.
- No production selector, scheduler or admission change.
- No efficiency or GO claim: P2 supplies an instrument and a first measurement. The population comes
  from P3, and the comparison still requires a selected arm measured under §2.
- No reuse of W3a's historical corpus, and no reuse of mutant timings as evidence about ordinary
  development changes.

## 7. Open questions for the reviewer

1. **Aggregate choice.** Is `C_comp = Σ per-producer wall` (CI's structure) the right reference, or
   should the reference be one unsharded `--lane fast`-style invocation? The sum overstates the wall
   clock a single developer would wait but matches what CI actually spends across jobs; the design
   picks the sum because the selector's savings apply to the whole pre-push verification, and
   because sharding exists for CI parallelism, not as a property of the workload.
2. **Overlap.** The C union is 1 397 files but 1 492 producer slots (95 files run in more than one
   producer). Charging the slots is intentional (CI pays them twice); should the record also carry a
   de-duplicated "unique-file cost" view? Proposal: yes, as a derived field, never as the headline.
3. **Smoke.** It is a 1.5 s workflow step with no JSON artifact; it is modelled as an inline
   single-file producer. Is that acceptable, or should smoke be excluded from C_comp as negligible?
4. **Surface L cap.** L's 17 long-tail files (up to 4 h) make a per-file cap necessary. Is the
   proposed 14 400 s operator cap the right instrument, or should L exclude ≥ 1200 s files by
   pre-registration and state that exclusion in every L number?
5. **Warm vs cold as the headline.** CI shards are cold per job; a developer's second run is warm.
   Proposal: report both labels and never compare across them — confirmation wanted.

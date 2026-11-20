# CL0 Agent-Task Benchmark — Frozen Corpus & Measurement Protocol (#9977)

Parent initiative: #9976. Starting revision for every task:
**`003e425d2f1206436eb9c0b84353604ce85bb082`** (main, v1.00.32).
Status: **corpus and protocol frozen; all agent-trial measurements PENDING** (none were run in
the CL0 environment — see §6 Limitations). This file is the contract CL2 and CL5 execute.

## Amendment log (original corpus preserved in git history; trials always cite the corpus
revision they executed)

| Date | Base revision | Amendment | Reason |
|---|---|---|---|
| 2026-11-20 | `003e425d` (freeze commit `22b6d875`) | — | original frozen corpus |
| 2026-11-20 | `003e425d` (amended in PR #9983 follow-up) | T1 oracle extended to the full decision tuple incl. `route` (verified by execution); T2 oracle replaced test-case counting with a mutation-detection procedure (demonstrated mechanically); T5 re-scoped from `util/ansi.rkt` to `tui/char-width.rkt` + `tests/tui/test-char-width.rkt` with independent behavioral assertions (the original target had no width helpers and the requested function duplicated existing `truncate-to-visible-width`); first in-scope edit distinguished from first correct edit; oracle-stability amendment procedure added | CL0 review (#9977 follow-up): weak oracles corrected before any independent agent trials |

Behavioral oracles are expressed against OBSERVABLE behavior (return values, pass/fail of
tests, decision tuples), never against implementation internals. When a later phase
intentionally moves an owner path (e.g. the CL4 extraction), the protocol is amended by a new
row in the table above — recording the new path, the revision where it moved, and a re-verified
oracle — while the behavioral assertions themselves stay unchanged; comparisons that span the
move cite both corpus revisions and the amendment row. An ownership path recorded in a task is
an expected fact at its corpus revision, not the grading criterion, unless the task grades
ownership explicitly (T1/T3/T4 do — those grade the answer set, which is itself behavior of
the repository at that revision).

Design intent: 4–6 representative, bounded tasks across subsystems; navigation/diagnosis tasks
measure *cognitive load to locate authoritative code*; implementation tasks measure
*context-to-first-edit and first-attempt correctness*. All oracles are mechanically checkable
(racket expressions, test commands, or diff constraints). Ownership sets below were verified
against the CL0 measured graph (`docs/reports/CL0-BASELINE-003e425d.md`), not guessed.

## 1. Measurement procedure (identical for every task)

1. **Fresh context.** One new agent session per trial; no memory of prior trials; identical
   system prompt and tool set; repository checked out at the pinned revision in a clean
   worktree (no bytecode caches: `find . -name compiled -type d -prune -exec rm -rf {} +`
   before cold trials; warm trials state their cache condition).
2. **Fixed model configuration.** One model + thinking configuration per experimental series,
   recorded in the results file; never mixed within a comparison.
3. **Prompt.** Verbatim task prompt from §2–§3 below (executor may not rephrase). The agent
   is told its answer or patch only; no hints about file locations.
4. **Recorded per trial** (from the harness/session log, not from memory):
   - context tokens consumed before the first in-scope edit (for implementation tasks) or
     before the final answer (navigation tasks); **first in-scope edit** = first source-file
     write inside the task's ownership set; **first correct edit** = the first in-scope edit
     whose final state passes the task oracle — both are recorded, because they measure
     different things (context cost vs first-attempt correctness);
   - count of source and documentation files read (opened) before that same point;
   - count of investigation tool calls before that same point;
   - wall-clock time to first reference of the authoritative implementation (owner-location
     time), judged by the first tool call naming the owner path;
   - total elapsed time;
   - number of subsystems (first path segment) touched by writes;
   - number of unrelated modifications (writes outside the task ownership set);
   - first-attempt correctness (oracle verdict on the first submitted answer/patch, before any
     retry);
   - regression/verification outcome (the oracle commands' exit status).
5. **Trial count.** Minimum n = 3 per task per configuration; report every sample (median, p90,
   max) with sample size; no target massaging.
6. **Results file.** One JSON per series:
   `{revision, host, model, config, task, trials: [{...recorded fields}], limitations}` under
   `.planning/benchmarks/CL0-tasks/` (workspace, not committed) until CL5 freezes a comparison.

## 2. Navigation/diagnosis tasks (oracle = frozen facts)

### T1 — Campaign decision interpretation (GSD)

Prompt: "A campaign has waves `[w0 done, w1 deferred]`, no cancellation, no pending delivery,
no authenticated proofs, lane view `#f`, stale flag `#f`. What single decision does
`decide-campaign-action` return? Report every decision field: kind, route, code, wave-index,
merge-sha. Name the module that owns the decision function and the module that is its only
effect interpreter. Answer with: kind, route, code, wave-index, merge-sha, decision module
path, interpreter module path, durable-state writer module paths."

Frozen oracle (verified at the pinned revision by executing the fixture; the graded tuple is
the five decision fields in order kind/route/code/wave-index/merge-sha):

```racket
(require (only-in "extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest make-campaign-wave make-campaign-record)
         (prefix-in cd: "extensions/gsd/campaign-decision.rkt"))
;; record = waves (w0 done, w1 deferred); views = campaign-views rec '() #f
;;          (cd:campaign-delivery-view #f (hash)) #f "pending" #f
;; executed at the pinned revision:
;;   kind       = 'complete
;;   route      = 'none
;;   code       = 'campaign-complete
;;   wave-index = #f
;;   merge-sha  = #f
```

Required answer set: fields exactly `'complete`, `'none`, `'campaign-complete`, `#f`, `#f`;
`extensions/gsd/campaign-decision.rkt` (decision),
`extensions/gsd/go-orchestrator.rkt` (interpreter),
`extensions/gsd/campaign-state.rkt` + `extensions/gsd/campaign-repository.rkt` (writers).
Grading: fields and module paths machine-compared (order-insensitive for the paths).

### T3 — Agent/state.rkt impact fan-out explanation (agent/session)

Prompt: "An edit to `agent/state.rkt` makes `./qt quick --changed HEAD` select roughly a dozen
audited test owners. Explain why, and list the production modules that directly require
`agent/state.rkt`."

Frozen oracle (measured at the pinned revision; 13 direct static dependents):
`agent/effect-executor.rkt`, `agent/event-emitter.rkt`, `agent/iteration/main-loop.rkt`,
`agent/loop-dispatch.rkt`, `agent/loop-phases.rkt`, `agent/loop-stream.rkt`, `agent/loop.rkt`,
`agent/stream-runner.rkt`, `extensions/gsd/author-execution.rkt`,
`extensions/gsd/command-handlers.rkt`, `extensions/gsd/go-orchestrator.rkt`,
`extensions/gsd/stall-policy.rkt`, `runtime/iteration/step-executor.rkt`.
Causal requirement: the answer must state that `agent/state.rkt` has **no** outgoing internal
requires (fan-out 0) and that the owners are selected because audited tests transitively
exercise the loop/session modules that require it — i.e. an impact-graph (inbound closure)
effect, not a state.rkt defect. Grading: list match (order-insensitive) + the fan-out-0
statement present.

### T4 — Provider retry ownership location (provider)

Prompt: "A DeepSeek-compatible provider stalls: HTTP 200 with SSE headers but zero chunks
before the stream timeout ('held request'). Where does the code live that (a) classifies this
case as non-retryable for the circuit breaker, (b) owns the retry/backoff policy configuration?
Answer with the two owning module paths and one test file that pins each."

Frozen oracle (verified from `docs/provider-retry.md` PN-4 + module headers):
(a) classification lives in the provider error/retry layer — `llm/provider-errors.rkt`
(classification primitives) consumed by the retry layer `runtime/provider-retry.rkt`;
(b) policy configuration — `runtime/provider-retry.rkt` (retry/backoff policy owner; the
iteration-side typed policy `runtime/iteration/retry-policy.rkt` governs loop retry, not
provider transport). Acceptable test pins include `tests/test-retry-error-classification.rkt`,
`tests/test-provider-retry-ceiling-config.rkt`, `tests/test-provider-retry-telemetry.rkt`.
Grading: (a) must name `llm/provider-errors.rkt` or `runtime/provider-retry.rkt` for
classification and `runtime/provider-retry.rkt` for policy; a location inside
`runtime/iteration/retry-policy.rkt` scores as a **near-miss** (recorded, not failed — this
ambiguity is itself the measured quantity).

## 3. Implementation tasks (oracle = tests + diff constraint)

### T2 — Delivery outcome regression test (GSD delivery, test-only)

Prompt: "Add one rackunit `test-case` to `tests/test-gsd-delivery-stop-decision.rkt` that pins
one additional row of the pure typed-stop classification table of
`extensions/gsd/delivery-outcome.rkt` (read the module's contract comments first). Do not
modify any production file."

Oracle (two parts, both must hold):

1. **Presence:** `raco test tests/test-gsd-delivery-stop-decision.rkt` exits 0 with ≥1 more
   test-case than at the pinned revision; `git diff --name-only 003e425d` shows **only** that
   file.
2. **Mutation detection (the added test must detect meaningful incorrect behavior, not merely
   count):** apply mutation **M-STOP-1** — in `delivery-stop-outcome`, misclassify the
   `review-infra` case as `'retryable` (change the outcome kind in that `case` clause) — then
   the added test-case must FAIL; restore the module and it must PASS again. The grading
   harness applies M-STOP-1 mechanically (single string substitution, documented here so every
   trial uses the identical mutation). Ownership set (verified):
   `extensions/gsd/delivery-outcome.rkt` (read-only reference; the `@covers` subject of the
   test), `tests/test-gsd-delivery-stop-decision.rkt` (write).

**Mechanical verification of the oracle design (2026-11-20, revision `a9f110f1` worktree):**
with the owner test's requires re-pointed at a copy of the module, the 6 existing test-cases
pass against the clean module and **1 test fails against M-STOP-1** with the precise behavioral
diff `expected: '(review-infra . "reviewer session timed out")` vs
`actual: '(retryable . "reviewer session timed out")`. An added row-pinning test-case of the
same shape therefore detects the mutation by construction; the trial harness re-runs this
two-state check per trial and records both verdicts.

### T5 — Width-aware truncation helper with ellipsis (UI boundary)

*Amended 2026-11-20 (see amendment log): the original task targeted `util/ansi.rkt`, which has
no width helpers, and would have duplicated the existing `truncate-to-visible-width`. The
correct home is the Unicode-width module itself.*

Prompt: "Add a pure function `truncate-with-ellipsis : string? exact-nonnegative-integer? ->
string?` to `tui/char-width.rkt` that returns the input unchanged when its visible width fits
the budget, and otherwise truncates it (reusing the module's grapheme/width machinery) and
appends a single-cell ellipsis so the result's visible width never exceeds the budget. Add
one `test-case` for it in `tests/tui/test-char-width.rkt`. Nothing else changes."

Oracle: `racket tests/tui/test-char-width.rkt` exits 0 with ≥1 more test-case than at the
pinned revision; `git diff --name-only 003e425d` shows exactly `tui/char-width.rkt` and
`tests/tui/test-char-width.rkt`.

**Independent behavioral assertions** (each must hold; written as observable input/output so
they survive reimplementation; verified compatible with the module's current semantics):

1. Zero budget: `(truncate-with-ellipsis "anything" 0)` ⇒ visible width 0 (empty string).
2. Exact fit: visible width ≤ budget ⇒ returned unchanged (no ellipsis appended), e.g.
   `(truncate-with-ellipsis "abcdef" 6)` ⇒ `"abcdef"`.
3. ASCII truncation: `(truncate-with-ellipsis "abcdef" 4)` ⇒ visible width exactly 4 and a
   suffix ellipsis (e.g. `"abc…"`; grading accepts any split whose visible width is 4 and whose
   visible prefix is a prefix of the input).
4. ANSI escapes carry zero visible width: for `s = "\x1b[31mabcdef\x1b[0m"`, truncation at
   budget 4 keeps total visible width 4, never splits an escape sequence mid-way (every
   `\x1b[...m` run in the result is complete), and the visible characters are a prefix of the
   input's visible characters.
5. Unicode display width: `(truncate-with-ellipsis "漢字" 3)` ⇒ visible width exactly 3
   (CJK chars are width 2; the ellipsis is width 1); a string of combining-mark-decorated
   graphemes truncates on grapheme boundaries — no result ever ends with a dangling combining
   mark that the module's `string-visible-width` would count differently from the budget.
6. Ellipsis accounting: for every budget ≥ 1 and input wider than the budget, the result's
   `string-visible-width` equals the budget exactly; for budget 0 the result's visible width
   is 0.

Grading: assertions 1–6 checked mechanically by a grader script that requires
`tui/char-width.rkt` at the trial's HEAD and evaluates each expression; the agent's own
test-case passing is necessary but not sufficient.

Ownership set (verified): `tui/char-width.rkt` (`contract-out` provider of
`string-visible-width`/`truncate-to-visible-width`), `tests/tui/test-char-width.rkt` (write).
Measured quantities: tokens/files/calls before the first in-scope edit and before the first
correct edit; unrelated modifications must be 0.

## 4. Comparison discipline

- CL2/CL5 comparisons re-run this corpus at their own pinned revision with the **same** model
  configuration(s), host class, fresh contexts, and n ≥ 3 per task; deltas are reported per
  task with sample sizes.
- Architecture improvements may legitimately move file counts/tokens/time; correctness and
  verification outcomes may never regress; any gate or claim weakening invalidates the trial.
- Navigation-task answers are graded by script from the frozen answer sets above — no LLM
  judging.

## 5. Limitations and pending work

- **PENDING: every measured quantity in §1 step 4.** The CL0 environment could not execute
  independent fresh-agent trials; nothing in this file is a trial measurement. The pre-verified
  quantities are the frozen oracles (T1 tuple executed at the pinned revision; T2 mutation
  demonstrated; T5 assertions verified against the current module semantics) and ownership sets.
- Five tasks cannot cover q's full task space; they deliberately span the four boundaries named
  in #9977 (GSD campaign/delivery, agent/session, provider, UI/tool) and mix diagnosis with
  bounded implementation.
- Owner-location time is judged from tool-call traces; a harness should implement it
  mechanically (first tool call whose arguments contain the owner path) to avoid judgment calls.
- The prompts are English-only and repository-specific; results do not transfer to other code.
- `./qt` and admission semantics are authoritative; trials must not bypass them. T5 edits
  `tui/char-width.rkt`, which appears in admitted owners' closures — a trial that touches it
  may need the documented admission refresh before `quick` reproduces; that step is part of
  the task's honest cost, not overhead to skip.

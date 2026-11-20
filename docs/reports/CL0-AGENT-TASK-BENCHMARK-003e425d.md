# CL0 Agent-Task Benchmark — Frozen Corpus & Measurement Protocol (#9977)

Parent initiative: #9976. Starting revision for every task:
**`003e425d2f1206436eb9c0b84353604ce85bb082`** (main, v1.00.32).
Status: **corpus and protocol frozen; all agent-trial measurements PENDING** (none were run in
the CL0 environment — see §5 Limitations). This file is the contract CL2 and CL5 execute.

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
   - context tokens consumed before the first useful edit (for implementation tasks) or before
     the final answer (navigation tasks); "useful edit" = first source-file write inside the
     task's ownership set;
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
`decide-campaign-action` return (kind, route, code)? Name the module that owns the decision
function and the module that is its only effect interpreter. Answer with: kind, code, decision
module path, interpreter module path, durable-state writer module paths."

Frozen oracle (verified at the pinned revision by executing the fixture):

```racket
(require (only-in "extensions/gsd/campaign-state.rkt"
                  make-campaign-manifest make-campaign-wave make-campaign-record)
         (prefix-in cd: "extensions/gsd/campaign-decision.rkt"))
;; record = waves (w0 done, w1 deferred); views = campaign-views rec '() #f
;;          (cd:campaign-delivery-view #f (hash)) #f "pending" #f
;; ⇒ (cd:campaign-decision-kind d)   = 'complete
;;   (cd:campaign-decision-code d)   = 'campaign-complete
```

Required answer set: kind `complete`, code `campaign-complete`,
`extensions/gsd/campaign-decision.rkt` (decision),
`extensions/gsd/go-orchestrator.rkt` (interpreter),
`extensions/gsd/campaign-state.rkt` + `extensions/gsd/campaign-repository.rkt` (writers).
Grading: exact module paths; kind/code machine-compared.

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

Oracle: `raco test tests/test-gsd-delivery-stop-decision.rkt` exits 0 with ≥1 more test-case
than at the pinned revision; `git diff --name-only 003e425d` shows **only** that file.
Ownership set (verified): `extensions/gsd/delivery-outcome.rkt` (read-only reference; the
`@covers` subject of the test), `tests/test-gsd-delivery-stop-decision.rkt` (write). Measured
quantities: tokens/files/calls before the first useful edit; unrelated modifications must be 0.

### T5 — Pure TUI formatting helper (UI/tool boundary)

Prompt: "Add a pure function `ansi-ellipsis-truncate : string? exact-nonnegative-integer? ->
string?` to `util/ansi.rkt` that truncates a string to a display width and appends an ellipsis
when truncating, reusing the module's existing width helpers. Add one `test-case` for it in
`tests/test-ansi.rkt`. Nothing else changes."

Oracle: `raco test tests/test-ansi.rkt` exits 0; `git diff --name-only 003e425d` shows exactly
`util/ansi.rkt` and `tests/test-ansi.rkt`. Ownership set: `util/ansi.rkt` (`@covers` owner of
`tests/test-ansi.rkt`, 23 tests at the pinned revision), the test file. This task measures the
everyday loop end-to-end (locate → edit → admission refresh if needed → focused green).

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
  independent fresh-agent trials; nothing in this file is a measurement. The only pre-verified
  quantities are the frozen oracles and ownership sets.
- Five tasks cannot cover q's full task space; they deliberately span the four boundaries named
  in #9977 (GSD campaign/delivery, agent/session, provider, UI/tool) and mix diagnosis with
  bounded implementation.
- Owner-location time is judged from tool-call traces; a harness should implement it
  mechanically (first tool call whose arguments contain the owner path) to avoid judgment calls.
- The prompts are English-only and repository-specific; results do not transfer to other code.
- `./qt` and admission semantics are authoritative; trials must not bypass them (T5 explicitly
  exercises the admission refresh path).

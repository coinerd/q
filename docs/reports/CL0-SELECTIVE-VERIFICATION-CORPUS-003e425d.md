# CL0 Prospective Selective-Verification Defect Corpus (#9977, review extension)

Parent initiative: #9976 (amended: contract-driven, component-local verification).
Corpus revision: pinned against main `003e425d2f1206436eb9c0b84353604ce85bb082` unless a case
names its own historical fix revision. Recorded 2026-11-20 in PR #9983.

Purpose: a **safety benchmark** for any future change-aware/component-local verification
selector (CL4 pilot and CL5 comparison). It asks the question the agent-task benchmark does
not: *when verification is narrowed to a selected set, do real regressions still get caught,
and does the selector escalate exactly when it must?* This corpus is **not** an authoritative
CI gate and does not change scheduling, caching, branch protection or required contexts; it is
shadow-mode evidence only, per the amended initiative's transition rules.

Status: **definitions frozen; execution PENDING except where marked VERIFIED** (executed in the
CL0 environment on the revision named in the case). Several cases are executable locally today;
cases needing other platforms, network-isolated TLS fixtures, or historical checkouts are
marked pending with their blocking condition named.

## Case selection method

Cases come from three sources, in priority order: (1) `.planning/bugs/BUG-00xx` records
(pinned versions, reproducers, detecting tests), (2) regression tests already in `tests/`
whose subject is a listed risk category, (3) controlled fault injections where historical
evidence is thin — each injection is a single, documented behavioral mutation applied to a
copy of the tree, never committed. Twelve cases cover the eleven required risk categories;
four fault-injection candidates are defined for categories with thin history. No case was
manufactured to make selection look good or bad; where a case is expected to be *hard* for a
selector, that is stated up front.

## Measurement protocol (applied to every case)

For each case, at its pinned revision, run the prospective selector in shadow mode:

1. **Scope.** Compute the selected verification set S for the case's changed-file set using
   the selector under evaluation (owner/contract/test attribution, escalations applied).
   Record S and compare against the case's declared `expected-scope`.
2. **Defect state.** Check out the defect state (historical fix revision's parent) or apply the
   fault injection. Record the exact tree digest.
3. **Selected run.** Execute S. Record pass/fail and elapsed time.
4. **Comprehensive run.** Execute the authoritative comprehensive lane (today: the required
   CI test surface; locally `./qt full` where feasible, else the CI job). Record pass/fail —
   this is the ground truth.
5. **Classify:**
   - **detected** — S fails while the defect is present;
   - **missed** — S passes while comprehensive fails (a false green; the dangerous outcome);
   - **true-negative** — S and comprehensive both pass (clean control, run once per case);
   - **unnecessary-selection** — S includes components with no declared dependency on the
     changed set (recorded as cost, not defect);
   - **escalation-correct** — S widened via a conservative escalation rule exactly where the
     case declares escalation required;
   - **fallback-correct** — when attribution is missing/stale, S fell back to the documented
     conservative default rather than a zero-test green.
6. **Report.** Per case: all of the above plus elapsed times. Aggregate: detection rate,
   missed-defect list, false-green rate, unnecessary-selection rate, escalation precision,
   fallback correctness, elapsed p50/p90 for selected vs comprehensive. n = 1 per case per
   selector version; a selector change re-runs the whole corpus (it is small by design).

The corpus must always be executed against **comprehensive verification as ground truth** — a
selector that is never compared against the full lane has produced no evidence.

## Cases

### D1 — Provider stream liveness: peer closes without TLS close_notify (BUG-0019)

- **Category:** internal component regression; persistence of transport state; recovery.
- **Pinned revision:** fix delivered v1.00.15; defect state = parent of the W1 fix commit on
  `fix/v1.00.15-bug0019-w1` (record: `.planning/bugs/BUG-0019-sse-fin-detection-connection-pooling/`).
- **Changed component / responsibility:** `llm/stream.rkt` (SSE read loop liveness) +
  `llm/request-policy.rkt` (probe cadence) + `runtime/auto-retry.rkt` (classification).
- **Affected behavioral contract:** a peer FIN without TLS close_notify terminates the stream
  within the probe window; clean EOF still yields end-of-stream; heartbeats are never killed.
- **Known detecting tests:** `tests/test-stream-peer-close.rkt`,
  `tests/test-midstream-stall.rkt`, `tests/test-sse-phase-timeout-bounds.rkt`,
  `tests/test-auto-retry.rkt`.
- **Expected selective scope:** stream/auto-retry owners + the provider transport boundary
  contract tests; NOT session, TUI, or GSD internals.
- **Escalation required?** Boundary contract affected (retry classification is a shared
  transport boundary) — yes, provider-boundary escalation.
- **Comprehensive comparison:** the four detecting tests + the full lane at the defect revision.
- **Executable now?** Yes locally (mock FIN server reproducer is checked in). Control and
  defect-state runs PENDING (needs the historical checkout).

### D2 — Message-shape regression: reasoning/thinking lost when content present (BUG-0003)

- **Category:** behavioral interface violation (provider stream accumulation contract).
- **Pinned revision:** v0.99.x fix era (record: `.planning/bugs/BUG-0003-thinking-vanishes-on-content/`).
- **Changed component:** stream accumulation / message assembly (`agent/loop-stream.rkt` family).
- **Affected contract:** thinking blocks survive alongside content in accumulated messages.
- **Known detecting tests:** the message-shape owner tests under `tests/` (streaming
  accumulation owners; exact file list to be pinned from the bug record at execution time).
- **Expected scope:** stream/message owners only.
- **Escalation:** none beyond owner closure — pure data-shape contract.
- **Pending:** detect-test pinning requires the historical record's test names; executable
  after that pin.

### D3 — Event-ordering: consecutive-tool circuit breaker (BUG-0016)

- **Category:** event-ordering change; iteration-loop control flow.
- **Pinned revision:** record `.planning/bugs/BUG-0016-wave-agent-consecutive-tool-breaker.md`.
- **Changed component:** iteration loop breaker + event emission order.
- **Affected contract:** breaker triggers after N consecutive tool failures; turn-end event
  ordering preserved.
- **Known detecting tests:** iteration/event owners (breaker + event-taxonomy owners; pin at
  execution from the bug record).
- **Expected scope:** agent iteration owners + event bus boundary.
- **Escalation:** event-taxononomy is a shared boundary — moderate escalation (event owners).
- **Pending:** same pinning condition as D2.

### D4 — Shared dependency corruption: pooled chunked body (BUG-0021)

- **Category:** shared dependency change (`llm/conn-pool.rkt` / `llm/http-helpers.rkt` serve
  every provider adapter).
- **Pinned revision:** v1.00.x fix era (record `.planning/bugs/BUG-0021-pooled-chunked-body-corruption/`).
- **Affected contract:** chunked response bodies reassembled byte-exactly across pooled
  connections.
- **Known detecting tests:** `tests/test-conn-pool.rkt` + provider adapter owners.
- **Expected scope:** pool/http owners + ALL provider adapters (transitive consumers) — the
  canonical "shared dependency ⇒ transitive consumers" case; a selector that returns only the
  pool owners is a miss by definition.
- **Escalation:** required (transitive-consumer widening).
- **Pending:** historical checkout.

### D5 — Behavioral interface violation: unknown wave-status spelling silently read as `pending`

- **Category:** behavioral interface violation; vocabulary owner.
- **Pinned revision:** the typed-rejection contract is current at `003e425d`
  (`extensions/gsd/wave-status.rkt`; `exn:fail:wave-status`).
- **Fault injection F-VS-1 (documented):** restore the historical silent fallback — unknown
  status → `'pending` — in a tree copy; the injection is one `case` default change.
- **Affected contract:** unknown status is a typed rejection carrying value + accepted
  spellings; comparison keys give unknowns their own identity.
- **Known detecting tests:** `tests/test-gsd-wave-status.rkt`,
  `tests/test-go-orchestrator-wave-status.rkt` + every consumer that pins statuses.
- **Expected scope:** wave-status owner + GSD status consumers.
- **Escalation:** consumers of the vocabulary are many (fan-in) — consumer-contract escalation.
- **Executable now?** Yes (fault injection on current tree). **VERIFIED-DESIGN note:** the
  analogous one-clause mutation technique was demonstrated for T2 (see benchmark protocol);
  execution of F-VS-1 itself PENDING.

### D6 — Persistence and recovery: interrupted plan-snapshot / reconciliation

- **Category:** persistence & recovery.
- **Pinned revision:** `003e425d` (components current: `extensions/gsd/plan-snapshot.rkt`,
  `extensions/gsd/reconciliation-checkpoint.rkt`, `extensions/gsd/campaign-repository.rkt`).
- **Fault injection F-PS-1:** truncated staging write (partial `plan-snapshot.tmp-*` payload);
  the snapshot reader must reject the partial state (documented invariant) and the
  reconciliation checkpoint must classify it as interrupted, not corrupt.
- **Known detecting tests:** `tests/test-gsd-plan-snapshot.rkt`,
  `tests/test-gsd-reconciliation-checkpoint.rkt`, snapshot drift-class owners.
- **Expected scope:** snapshot/checkpoint/repository owners; NOT UI or LLM.
- **Escalation:** durable-state writer boundary — yes (single-writer contract).
- **Executable now?** Yes (fixture-based fault injection). PENDING execution.

### D7 — Cancellation and concurrency: planning artifacts under concurrent `/plan`

- **Category:** cancellation and concurrency; lease/fencing.
- **Pinned revision:** record `.planning/bugs/BUG-0013-planning-artifacts-no-concurrency.md`;
  current machinery `extensions/gsd/campaign-lease.rkt` + fenced writes in
  `campaign-repository.rkt`.
- **Fault injection F-LF-1:** drop the fence-token comparison on one transition path (one
  condition) in a tree copy.
- **Known detecting tests:** lease/fence owners + campaign-repository owners
  (`tests/test-gsd-campaign-state.rkt` family; pin exact fence test at execution).
- **Expected scope:** lease + repository + go-orchestrator interpreter.
- **Escalation:** authorization/authority boundary — conservative yes.
- **Pending:** historical record + injection execution.

### D8 — Authorization boundary: scratch trust channel and safe mode

- **Category:** authorization and security boundaries.
- **Pinned revision:** `003e425d` (`tools/scratch.rkt`; remote `trusted-scratch-root` refusal;
  safe-mode read-only allowlist).
- **Fault injection F-SM-1:** accept a forged `trusted-scratch-root` on the remote path (one
  refusal branch removed).
- **Known detecting tests:** `tests/test-tool-scratch-allocation.rkt`,
  `tests/test-worker-security.rkt`, `tests/test-browser-safe-mode.rkt`.
- **Expected scope:** scratch + worker-security + safe-mode owners.
- **Escalation:** security boundary — ALWAYS conservative escalation to the security lane,
  regardless of selection confidence (case exists to verify exactly this rule).
- **Executable now?** Yes; PENDING execution.

### D9 — Shared dependency hub: `agent/state.rkt` edit fans to 11 audited owners

- **Category:** shared dependency change; selection-cost realism.
- **Pinned revision:** `003e425d`; direct dependents frozen in the CL0 report §1.1.
- **Change:** any edit to `agent/state.rkt` (e.g. its CL0-era 13-dependents set).
- **Affected contract:** ephemeral loop-state accessors (no persistence by design).
- **Known detecting tests:** `tests/test-agent-state.rkt` + the 10 further audited owners the
  impact graph selects (measured in CL0: `./qt quick --changed HEAD` → 11 owners, ≈25 s).
- **Expected scope:** the 11 owners (verified live during CL0) — a selector returning 1 owner
  is a miss; returning the whole lane is unnecessary-selection (recorded as cost).
- **Escalation:** no — the impact graph already encodes the closure; this case measures
  whether selection reproduces the *verified* graph.
- **Executable now?** **VERIFIED (selection half)** — the 11-owner selection and 25.6 s
  elapsed were measured in CL0 on revision `211e0518`'s tree. Defect-injection half PENDING.

### D10 — Added source file with no ownership mapping (unmapped-source escalation)

- **Category:** added/changed/renamed/deleted files; missing test ownership.
- **Pinned revision:** observed live in PR #9983 (new `scripts/architecture-graph-helpers.rkt`
  + `architecture-graph-analysis.rkt`, revision `fa48b103`–`211e0518`).
- **Change:** add a production module with no `@covers` mapping.
- **Affected contract:** `quick --changed` never answers a gap with a zero-test green — a
  named escalation widens to the whole audited lane.
- **Known detecting behavior:** `./qt quick --changed HEAD` fails open with a named escalation
  (23 files / 228 tests).
- **Expected scope:** whole audited lane (conservative escalation).
- **Escalation:** required by contract.
- **Executable now?** **VERIFIED** — measured twice on PR #9983 (25.6 s, 23 files / 228 tests).

### D11 — Stale derived metadata: published metrics lag the tree (metrics-sync/prose CI failure)

- **Category:** verification-selection and policy changes; stale test ownership analog.
- **Pinned revision:** observed live in PR #9983 CI run 44809549492 (job 136542266119):
  `metrics-sync` + `metrics-lint` + `prose` FAILED because README's published metrics were
  computed before the branch's new files were tracked; a local targeted `metrics.rkt --lint`
  had PASSED while the files were untracked (metrics count git-tracked files).
- **Change:** any commit adding/removing tracked .rkt files without running
  `scripts/metrics.rkt --sync-all`.
- **Affected contract:** README published metrics equal tree-computed metrics; prose counts
  match.
- **Known detecting tests:** the `metrics-sync`/`metrics-lint`/`prose` checks inside
  `scripts/lint-all.rkt` (CI lint-quality job).
- **Expected scope:** the derived-metadata sync checks — a *targeted six-command lint* does
  NOT include them in their failing form (targeted `--lint` after tracking does; before
  tracking it cannot). This case measures the targeted-vs-comprehensive gap precisely.
- **Escalation:** policy/metadata surface — conservative escalation to the full lint-quality
  check.
- **Executable now?** **VERIFIED** — reproduced locally on `a9f110f1` (5 metric errors + 2
  prose errors pre-sync; `lint-all.rkt` 23 passed / 2 warnings post-sync, 81 s local).

### D12 — Renamed/deleted files: diff-pair selection

- **Category:** renamed/deleted source and test files.
- **Pinned revision:** `003e425d` (`./qt quick --changed` computes diffs `--no-renames`;
  renames appear as delete+add pairs — documented contract in `docs/testing.md`).
- **Fault injection F-RD-1:** select with rename-aware diffing (toggle the documented
  `--no-renames` behavior in a tree copy); a rename of an owner-covered file must still map
  to the owner through its delete+add pair.
- **Known detecting behavior:** documented contract + `tests/test-qt-quick.rkt` family.
- **Expected scope:** owner of the renamed file (via pair) — not a gap escalation.
- **Escalation:** no, if pair mapping works; gap escalation if the add side is unmapped.
- **Executable now?** Yes; PENDING execution.

## Fault-injection candidates (categories with thin historical records)

| ID | Target | Injected behavioral fault | Detecting owners |
|---|---|---|---|
| F-EB-1 | `util/event/event-bus.rkt` | publish reordering under concurrent subscribers (ordered-delivery invariant) | `tests/test-event-bus.rkt`, `tests/test-iteration-event-golden.rkt` |
| F-HC-1 | `runtime/session/session-store*` | append skipping one hash-chain link; verification/repair must detect | `tests/test-hash-chain.rkt`, `tests/test-session-store-integrity.rkt` |
| F-PE-1 | `runtime/provider-retry.rkt` | held-request (200 + SSE headers, zero chunks) classified retryable — circuit breaker must fire | `tests/test-retry-error-classification.rkt`, `tests/test-midstream-stall.rkt` |
| F-AU-1 | `runtime/auth` / owner-correction store | accept a symlinked store path (refusal invariant) | worker-security/owner-correction owners |

All four are executable locally (fixture/port based) and are PENDING execution; each follows
the same protocol as D-cases, with comprehensive ground truth mandatory.

## Explicit non-goals

- No authoritative selective CI gate is implemented or proposed for adoption in CL0; results
  feed CL4/CL5 evidence and a separate maintainer-approved policy transition.
- No weakening of required contexts, static selection, or exact-SHA evidence.
- The corpus deliberately includes cases (D4, D9, D10, D11, D8) that stress the selector's
  *worst* realistic inputs; a selector evaluated only on clean internal regressions would be
  unfalsifiable.

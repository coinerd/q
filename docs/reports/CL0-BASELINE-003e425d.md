# CL0 Baseline — Cognitive Load & Architecture (issue #9977)

Parent initiative: #9976 (Cognitive Load Reduction & Agent-Oriented Architecture).
Status: **baseline only** — evidence of record; no production refactoring, no CL1+ implementation.

> **Review extension (2026-11-20, PR #9983 follow-up).** The sections below numbered 8–9 and
> the amendments to §4.2/§6/§7 were added after provisional baseline acceptance, against the
> amended parent initiative (contract-driven, component-local verification). The original
> pinned baseline sections (1–7) remain recorded at main
> `003e425d2f1206436eb9c0b84353604ce85bb082`; later observations name their own revision
> (`211e0518`/`a9f110f1`, PR #9983 branch) and date.

| Item | Value |
|---|---|
| Pinned revision | `003e425d2f1206436eb9c0b84353604ce85bb082` (main, v1.00.32) |
| Date recorded | 2026-11-20 |
| Host | Linux 6.8.0-134-generic x86_64 (4-vCPU KVM guest, 7.8 GiB RAM) |
| Toolchain | Racket 8.10 [cs] |
| History window | 200 non-merge commits: 2026-09-11 → 2026-11-20 (3 release-only commits and 2 exact moves excluded by the tool) |
| Branch / worktree | `feat/9977-cl0-baseline` / `wt-9977-cl0` |

## Reproduction commands

```sh
# 1. Structural + evolutionary snapshot (22 s on the reference host)
racket scripts/architecture-baseline.rkt \
  --revision 003e425d2f1206436eb9c0b84353604ce85bb082 \
  --raw docs/reports/architecture-baseline-cl0-003e425d.rktd \
  --markdown /tmp/cl0-baseline.md --last 200

# 2. Graph views over the snapshot: SCCs, transitive closure, cross-subsystem edges
racket scripts/architecture-graph-analysis.rkt \
  --snapshot docs/reports/architecture-baseline-cl0-003e425d.rktd \
  --raw docs/reports/architecture-graph-cl0-003e425d.rktd \
  --markdown docs/reports/architecture-graph-cl0-003e425d.md --top 25
```

Step 1 is the existing canonical tool (deterministic; same revision → byte-identical raw).
Step 2 is a pure consumer of that raw output (new in this change; see
`scripts/architecture-graph-helpers.rkt`, tested by `tests/test-architecture-graph-helpers.rkt`).
Machine-readable evidence of record: `docs/reports/architecture-baseline-cl0-003e425d.rktd`
(931 KB) and `docs/reports/architecture-graph-cl0-003e425d.rktd` (3.2 KB).

## 1. Structural complexity

Headline counts (measured at the pinned revision; README reconciliation below):

| Metric | Value |
|---|---:|
| Tracked Racket files | 2656 (1679 under `tests/`) |
| Source modules (non-test) | 977 |
| Source / test lines | 239,510 / 366,554 |
| Provide specs (public surface) | 12,552 |
| Static dependency edges (incl. tests) | 10,020 |
| Static edges, production graph | 3483 |
| **Strongly connected components** | **0** — the static production graph is a DAG |
| Test cases / checks | 21,156 / 52,725 |
| Architecture policy exceptions | 5 (all `PERMANENT`, documented) |

### 1.1 Fan-out / fan-in and transitive closure

Static fan-out peaks (module → modules it requires directly):
`extensions/gsd/go-orchestrator.rkt` 44, `extensions/gsd/command-handlers.rkt` 39,
`runtime/agent-session.rkt` 38, `runtime/session/session-lifecycle.rkt` 38,
`tui/tui-render-loop.rkt` 33. These are composition/coordination roots, where fan-out is the
role, not an anomaly. `agent/state.rkt` has **fan-out 0, direct fan-in 13** — the reported
"agent/state.rkt hub" is an *impact* hub (many audited owners transitively depend on it), not a
static fan-out hub; see §4.

Transitive closure hubs (largest reachable set below): `main.rkt` 617,
`interfaces/tui.rkt` 521, `tui/tui-init.rkt` 520, `interfaces/sdk.rkt` 468,
`runtime/agent-session.rkt` 286 of 977 modules, `extensions/gsd/go-orchestrator.rkt` 280.
Transitive dependents (largest reachable set above, i.e. blast radius of an edit):
`util/error/deprecation.rkt` 366 (direct fan-in 1), `util/error/errors.rkt` 365 (44),
`util/event/event.rkt` 289 (49), `util/content/content-parts.rkt` 286 (69),
`util/telemetry.rkt` 286, `util/error/error-helpers.rkt` 285 (55),
`util/event/event-bus.rkt` 255 (52). The foundation `util/` layer is the load-bearing sink:
796 inbound edges and a transitive dependent reach that dwarfs every other subsystem.

### 1.2 Cross-subsystem edges (first-path-segment partition)

Top directed edges: runtime→util 284, agent→util 118, tools→util 94, extensions→util 90,
tui→util 62, tui→runtime 45, runtime→llm 44, wiring→runtime 43, gui→ui-core 38,
runtime→agent 38. Edges that cross the *documented* layering direction and are worth CL1
attention (all currently permitted by policy, exceptions, or the coarse partition — none is a
violation per the fitness tests): tui→extensions 22, extensions→runtime 22, tools→runtime 23,
extensions→sandbox 14, extensions→scripts 13, tui→ui-core 33.

### 1.3 Dynamic / unresolved dependencies

The baseline snapshot records **static relative-path requires only**. 24 production modules
call `dynamic-require`; the heaviest are `extensions/gsd/responsibility-inventory.rkt` (14,
inventory metadata), `extensions/gsd/supplemental-evidence-controller.rkt` (8),
`agent/registry.rkt` (8), `extensions/gsd/evidence-discovery.rkt` (4). The supplemental-evidence
lane is therefore the largest subsystem whose true runtime topology is **not** visible to static
analysis; CL1 maps must mark these edges unresolved rather than inferring them.

### 1.4 Policy reconciliation (dependency-policy debt)

The `dependency-policy.rktd` reconciliation tables embed *recorded* numbers that have drifted
from measured values at this revision (all six composition-root fan-out rows and 47 of 48
provide-risk rows read DRIFT; e.g. `runtime/settings.rkt` recorded 221 provides vs 79 measured,
`runtime/settings-query.rkt` recorded 226 vs 69). The policy **rules** (exceptions, layer
boundaries) are enforced by `tests/test-arch-fitness.rkt` and pass; what drifted is the
descriptive number columns. Debt: CL1 should stop maintaining recorded numbers that a tool can
measure, keeping only the normative entries (exceptions, roots, ownership).

README reconciliation: `test-files`, `source-modules`, `source-lines` MATCH; `test-lines`
(DRIFT by 9) and `checks` (DRIFT by 1) differ because **two independent counters** exist —
`scripts/metrics.rkt` (CI lint authority, publishes README) and
`scripts/architecture-baseline-helpers.rkt` (counts from Git blobs at the pinned revision).
Same intent, two methodologies, divergent bytes. Recorded as duplication debt in §3; not fixed
here (fixing means choosing one counting authority — a CL1 decision).

## 2. Evolutionary complexity (200 commits, 2026-09-11 → 2026-11-20)

Top production hotspots (loc × changed-commits), excluding tests:

| Path | LOC | Fan-in | Fan-out | Changes | Hotspot |
|---|---:|---:|---:|---:|---:|
| `extensions/gsd/responsibility-inventory.rkt` | 1496 | 2 | 0 | 28 | 41,888 |
| `ci/prepared-environment/compiled-root.rkt` | 2845 | 4 | 2 | 14 | 39,830 |
| `extensions/gsd/delivery-coordinator.rkt` | 1451 | 24 | 21 | 20 | 29,020 |
| `extensions/gsd/go-orchestrator.rkt` | 1692 | 51 | 44 | 12 | 20,304 |
| `extensions/gsd/supplemental-evidence-controller.rkt` | 1826 | 29 | 26 | 10 | 18,260 |
| `extensions/gsd/delivery-finalize.rkt` | 854 | 16 | 16 | 14 | 11,956 |
| `extensions/gsd/command-handlers.rkt` | 1100 | 12 | 39 | 10 | 11,000 |
| `extensions/gsd/delivery-verifier.rkt` | 1407 | 56 | 12 | 10 | 14,070 |
| `extensions/gsd/delivery-receipt.rkt` | 823 | 46 | 10 | 11 | 9,053 |
| `extensions/gsd/delivery-journal.rkt` | 887 | 87 | 5 | 10 | 8,870 |

Co-change evidence (pairs changed together ≥3 times; 550 pairs at threshold, top 100 in raw):
the delivery cluster co-changes as a set — `delivery-coordinator`↔`delivery-handoff` 6,
`delivery-finalize`↔`delivery-journal` 6, `delivery-finalize`↔`delivery-receipt` 6,
`delivery-journal`↔`delivery-receipt` 7, `delivery-coordinator`↔`go-orchestrator` 5, and each
delivery module co-changes with `responsibility-inventory.rkt` (7–11 times). The inventory is a
*deliberate* governance contract (its test asserts the classified module list), but it is also
the single most co-changed production file in the repository: any GSD effect/dependency change
requires touching it plus its test (21 changes) — a fixed coordination tax on every GSD refactor.

Representative multi-subsystem changes from the window (coordination cost evidence):

- `d20ce530` (#9956, 30 files): `qt` CLI + ownership manifests (`tests/.quick-admission.rktd`,
  `tests/.coverage-manifest.json`) + README + GSD/TUI/agent test owners + docs.
- `52a66324` (#9955, 15 files): CI workflows + `artifacts/tier-ownership` + ownership-map
  reports + `scripts/gsd-gate*` + docs.
- `f264b06d` (#9969, 12 files): GSD delivery-outcome extraction + quick admission + coverage
  manifest + README + docs-accuracy test.

Pattern: a qualifying code change fans out to 4–6 *derived metadata surfaces* (admission
record, coverage manifest, ownership-map reports, README counts, testing docs). Each sync is
individually justified by a guarantee; collectively they are the largest procedural fan-out a
developer faces (§3).

Change-locality verdict: **GSD delivery + supplemental evidence** is the poor-locality region
(8–10 modules co-changing with high frequency, coordinated through journals and one controller);
`ci/prepared-environment/compiled-root.rkt` (2845 LOC, 14 changes) is a second, CI-local
hotspot. The agent/runtime core (`agent/loop*`, `session-lifecycle`) shows comparatively low
change frequency after the v0.99.9x extractions — consistent with the MA-* terminal ledger's
evidence-backed rejections of further extraction there.

## 3. Behavioral complexity — state / decision / effect ownership

Assessment basis: `docs/gsd-architecture.md` (single-owner rules), `docs/architecture/overview.md`
(owner annotations), `extensions/gsd/responsibility-inventory.rkt` (machine-checked effects),
`docs/architecture/maintainability-terminal-v0.99.93.rktd`, and the measured graph. No
persistence was mutated; every claim below names its owner module.

| Workflow | Durable state owner (single writer) | Decision owner | Effect owner | Projection / evidence |
|---|---|---|---|---|
| Campaign execution | `campaign-state.rkt` / `campaign-repository.rkt` (fenced, journaled) | `campaign-decision.rkt` (pure `decide-campaign-action`) | `go-orchestrator.rkt` (only interpreter; lease `campaign-lease.rkt`) | `projection-kernel/effects.rkt`, delivery receipts |
| Supplemental evidence | `supplemental-evidence-journal.rkt` (+ disposition stores) | authority: `supplemental-evidence-authority.rkt`; routing inside `supplemental-evidence-controller.rkt` | controller + `evidence-probe.rkt`; **8 dynamic-require edges** | evidence journals, proof bundles |
| Independent review | `review-proof.rkt` (proof authority; fan-in 64) | `review-session.rkt` / `review-dispatch` | review adapters | proofs, `review-audit` |
| Delivery & recovery | `delivery-journal.rkt` (evidence sink; fan-in 87) + `delivery-receipt.rkt` | `delivery-outcome.rkt` (pure, extracted #9969); `delivery-verifier.rkt` verification facts | `delivery-coordinator.rkt` (1451 LOC), `delivery-finalize.rkt`, recovery `delivery-recovery.rkt` + `reconciliation-checkpoint.rkt` | receipts, journal, publication |
| Session lifecycle | `runtime/session/session-store.rkt` (append-only JSONL + hash chain) | `session-lifecycle.rkt` (run-prompt! pipeline) | `agent-session.rkt` wiring, controls | session JSONL (append-only), event bus |
| Agent state & iteration | none persistent by design — `agent/state.rkt` is ephemeral (fan-out 0), loop-result typed | `agent/iteration/step-interpreter.rkt` (recurse/stop) | `agent/loop.rkt` (composition root), `main-loop.rkt` | typed events (`agent/event-structs/*`) |
| Context assembly | derived only (summaries/pins inside session store) | budgeting + selection + `task-state` FSM | none beyond store reads | tiered-context, turn-context |
| Permission & tool execution | no new state; scratch allocation `tools/scratch.rkt` (session-tokened) | `policy.rkt` (`gsd-decide-action`), per-state tool blocklist, safe-mode allowlist | `tools/scheduler.rkt`, `sandbox/subprocess.rkt` (custodian-bounded) | tool events, audit |

Reading: the **campaign** workflow has the cleanest separation (pure decision kernel + one
effect interpreter + fenced single writer — the W3 v1.00.31 result). The **delivery** workflow
is the largest remaining concentration: 8+ modules where the evidence sink (`delivery-journal`,
87 direct dependents) and the coordinator sit close together; the pure decision
(`delivery-outcome.rkt`) is only partially extracted from routing. The **supplemental evidence**
workflow is the weakest static-visibility area (dynamic requires) and has the largest single
controller module (1826 LOC, fan-in 29) that mixes observe/decide/coordinate. Essential vs
avoidable: journaling, fencing, leases and receipts are essential domain complexity (crash
safety and authority); what looks avoidable is routing/controller breadth *around* those
kernels — exactly the CL4 candidate evidence, to be confirmed by CL1 maps before any extraction.

## 4. Procedural complexity & feedback loop

### 4.1 Reported vs fresh measurements (same host class, this worktree)

`./qt` semantics unchanged and authoritative; targets and their recorded misses live in
`docs/testing.md` (v1.00.33 W2 measurement). Fresh samples taken for CL0 (n small, stated;
nearest-rank):

| Measurement | Reported (docs/testing.md) | Fresh CL0 samples | n |
|---|---|---|---|
| `racket -e '(void)'` process startup | — | 0.81 s (warm page cache) | 3 |
| `raco test tests/test-ansi.rkt` (focused TUI) | p50 1.96 / p90 2.16 s | 1.18–1.31 s | 3 |
| `raco test tests/test-gsd-campaign-decision-pure.rkt` | p50 4.95 / p90 5.42 s | 4.17–4.64 s | 6 |
| `raco make extensions/gsd/campaign-decision.rkt` (cold .zo) | — | **4.18 s** | 1 |
| same, warm | — | **0.62 s** | 1 |
| `./qt quick --changed HEAD` with unmapped new source (escalation) | full-lane p50 22.4 s | **25.6 s** (23 files / 228 tests) | 1 |
| `./qt quick <owner>` (admitted owner) | — | pass, 1 file / 23 tests | 1 |

Fresh samples **reproduce** the recorded picture on this host: the focused miss is real and its
cause decomposes cleanly — of the ≈4.4 s pure-GSD owner run, ≈3.5 s is compilation (cold
`raco make` 4.18 s vs warm 0.62 s) of a 10-module / 4,413-LOC closure whose chain includes the
`#lang typed/racket` module `extensions/gsd/plan-types.rkt` via `plan-snapshot.rkt`. 13
production modules genuinely use `typed/racket`. This is a **process/compile cost**, not
architectural coupling: CL3 (with #9911's persistent worker) addresses the compile half; the
dependency half is already small (fan-out of the owner's subject module: 1).

The reported `agent/state.rkt` impact fan-out (11 owners ≤15 s target, missed at ≈25 s) is
confirmed as an *impact-graph* phenomenon: `agent/state.rkt` itself has direct fan-in 13 and
fan-out 0; the 11 owners are selected because they transitively exercise the loop modules that
require it. Reducing that number means reducing how many audited owners share that closure —
a test-ownership geometry question, not a state.rkt refactor.

### 4.2 Procedural inventory (costs and duplication)

- **Test discovery/admission:** audited owner table (23 owners / 228 tests) is committed and
  fail-closed; admission refresh after closure edits: p50 1.6 s (reported); `--audit` review
  view ≈30 s (reported; off the run path). Unmapped new source escalates `quick --changed` to
  the whole audited lane (measured 25.6 s above) — correct fail-open behavior with a real cost:
  a new script without an admitted owner makes every `--changed` run pay the full lane until
  the owner is admitted.
- **Impact selection:** the `--changed --explain` floor is ≈3 s because the production
  dependency graph is rebuilt per call (reported; unchanged here). Bounded caching is a CL3
  candidate, not CL0 work.
- **Derived metadata fan-out per qualifying change:** admission record, coverage manifest,
  ownership-map reports, README metric counts, testing docs — 4–6 surfaces (evidence: commits
  `d20ce530`, `52a66324`, `f264b06d`). Every surface guards a claim; the *discovery* of "which
  surfaces does my change touch?" is manual today. Candidate (CL1): derive the touch-list from
  the existing `@covers` manifest + dependency graph; no new metadata. *(Review extension —
  concrete instance, revision `211e0518`: PR #9983's first CI run failed `metrics-sync`,
  `metrics-lint` and `prose` because README's published metrics lagged the branch's new
  files; a targeted local `metrics.rkt --lint` had passed while the files were still
  untracked, because the counter reads git-tracked files. Fixed with the authoritative
  `scripts/metrics.rkt --sync-all`; full `lint-all.rkt` then 23 passed / 2 warnings. Details
  and the targeted-vs-comprehensive distinction: §9.2.)*
- **Counting duplication:** two test-line/check counters (§1.4). Candidate: CL1 declares one
  counting authority; the other consumes it.
- **Stale policy columns:** recorded fan-out/provide numbers in `dependency-policy.rktd`
  (§1.4). Candidate: CL1 drops tool-measurable columns, keeps normative entries.
- **Documentation location:** current-truth architecture docs are
  `docs/architecture/overview.md`, `docs/gsd-architecture.md`, `docs/architecture/dependency-policy.rktd`,
  `docs/testing.md`, `TEST_CONVENTIONS.md`; 669 historical reports live in `docs/reports/`
  (opt-in history). The mapping "task type → authoritative doc" is learnable but undocumented;
  CL1 canonical maps are exactly this artifact.

## 5. Agent-oriented benchmark (protocol; trials pending)

Full frozen corpus, oracles and measurement protocol: **`docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md`**.
Five tasks across GSD campaign/delivery, agent/session, provider, and tool/extension boundaries,
each pinned to `003e425d` with an acceptance oracle and a fixed measurement procedure (tokens
before first in-scope edit and first correct edit, files read, tool calls, owner-location time,
elapsed, subsystems touched, unrelated modifications, verification outcome). *(Review extension:
oracles strengthened — T1 full decision tuple verified by execution, T2 mutation-detection
oracle demonstrated mechanically, T5 re-scoped to `tui/char-width.rkt` with independent
behavioral assertions; amendment log in the protocol.)*

Complementary safety corpus for prospective selective verification:
**`docs/reports/CL0-SELECTIVE-VERIFICATION-CORPUS-003e425d.md`** (12 historical/fault-injection
cases + measurement protocol; execution pending except the marked VERIFIED selection
measurements).

**No agent trials were run in this environment.** The measurements require independent fresh
agent contexts with controlled model configurations, which CL0's authorization and tooling do
not provide here. Per the honest-reporting requirement, all trial measurements are marked
PENDING; the protocol is written so CL2/CL5 (or any maintainer) can execute it unchanged and
produce comparable numbers. Claimed capabilities are limited to: the corpus is frozen, the
oracles are mechanically checkable (test commands / assertions), and the file-ownership sets
were verified against the measured graph.

## 6. Intervention matrix and recommendations (CL1–CL5)

Candidates ranked by (expected cognitive-load reduction × evidence) ÷ (cost × risk):

| # | Candidate | Evidence | Reuses | Risk | Verdict for phase |
|---|---|---|---|---|---|
| 1 | **Canonical subsystem maps** (CL1): task-type → authoritative docs/owners/tests, derived from `@covers` manifest, responsibility inventory, dependency policy, measured graph | §2 coordination tax; §3 ownership table; §4.2 discovery cost | all listed infra + new graph helpers | low (docs + manifests; no runtime) | **GO (CL1 core)** |
| 2 | **Stop maintaining tool-measurable policy numbers** (one counting authority; drop recorded fan-out/provide columns) | §1.4 drift evidence | baseline tool as the measurer | low | **GO, inside CL1** |
| 3 | **Pure decision kernel + observe→decide→execute boundary for ONE delivery/supplemental workflow** (CL4 pilot, below) | §2 co-change cluster; §3 delivery/supplemental concentration; W3 precedent (campaign-decision) | effect-ports, fake ports, golden traces | medium | **PROPOSE pilot (CL4)** |
| 4 | **Semantic navigator MVP** (CL2: symbol→owner→tests lookup with uncertainty labels) | §3/§4.2 discovery cost; maps from #1 are its index | graph helpers, quick-audit `@covers`, impact machinery | medium (stale-index risk; needs discipline) | **GO after CL1**, benchmark-gated |
| 5 | **Interactive feedback / persistent evaluation worker** (CL3 + #9911) | §4.1: ≈3.5 s compile of 4.4 s focused run; TR boundary chain; impact p90 25 s vs 15 s target | runner infrastructure, #9911 proposal | medium-high (isolation, security, stale state) | **EVALUATE (CL3)**; measure compile-vs-fanout separately as scoped there |
| 6 | Deep capability interfaces across *all* subsystems | no evidence of broad need; MA-* rejections precedent | — | fragmentation | **REJECT as general program; pilot-only** |
| 7 | Universal effect framework / blanket Typed Racket migration | explicitly non-goal in #9976; §1 shows DAG already clean | — | high | **REJECT** |
| 8 | Bulk file moves / size-driven splitting | hotspot ≠ defect (MA-10 closed by evidence-backed rejection) | — | regression risk | **REJECT** |

**Proposed CL4 pilot (bounded):** *supplemental-evidence controller observe→decide split* —
extract the controller's routing into a pure decision function over explicit views (journal
facts, authority facts, capability facts), keeping `supplemental-evidence-controller.rkt` as
interpreter and the journal/disposition stores as the only writers, mirroring the proven
W3 campaign-decision shape. Evidence: largest controller module (1826 LOC), 8 unresolved
dynamic edges concentrated there, top co-change cluster, high fan-in authority neighbor.
Acceptance: unchanged behavior (golden traces + fake ports + failure/restart scenarios),
reduced coordinator/routing fan-in in a re-run of the CL0 commands, no public API break,
required verification green. **CL4 must re-verify this choice against CL1 maps before
starting** — if the maps show the delivery-cluster routing (candidate area per #9981) has
higher coordination cost, swap the pilot; the decision procedure is recorded here. *(Review
extension: acceptance is amended by the CL4 criterion in the recommendations below — the
extraction alone does not prove component-local verification; contract coverage and safe
verification selection must be demonstrated too.)*

**CL1–CL5 recommendations (amended 2026-11-20 for the contract-driven, component-local
verification objective; go/revise/reject criteria):**
- **CL1 GO — re-scoped:** canonical subsystem maps **plus component behavioral-contract
  mapping** — for each component: public behavioral interface, existing structural contracts,
  guarantees actually established by tests, provider/consumer relationships, contract/test
  ownership, dynamic dependencies and unresolved impacts — derived from existing metadata
  (inventory, `@covers`, dependency policy, measured graph). This is not solely a
  documentation initiative: CL1's deliverable is the contract/ownership data layer that
  selection and navigation consume. GO criterion: the maps demonstrably reduce reads-to-owner
  and identify affected contracts on ≥2 frozen benchmark tasks (fresh-context protocol).
- **CL2 GO after CL1:** the navigator exposes relevant contracts, owners and tests (not only
  files/symbols) for the changed set; matched-task deltas must beat CL0 numbers; it stays
  developer tooling — never an authority for PR verification without separate evidence and
  approval. Reject features whose maintenance exceeds the load they remove.
- **CL3 EVALUATE strictly per #9911 scope:** interactive exploration and focused tests remain
  distinct from authoritative verification; the worker must improve isolated component
  verification (the measured ≈3.5 s compile share, §4.1) without becoming a selective-CI
  substitute. Startup/compile separation is already evidenced; worker go/revise/reject on
  measured debugging-task benefit vs isolation/security cost.
- **CL4 GO for the pilot only with the amended acceptance bar:** the bounded GSD component
  (supplemental-evidence observe→decide remains the leading candidate; re-confirm against CL1
  maps) must demonstrate **independent understandability, modifiability and verification**:
  contract coverage adequate for the component's guarantees (decision matrix, journal
  invariants, failure/restart paths), a demonstrated safe selected set vs comprehensive
  results on the defect corpus (including D5/D6-class cases), and preserved composition
  guarantees. The extraction alone does NOT prove component-local verification.
- **CL5 GO as the comparative gate:** prospective selective verification (defect corpus +
  historical changes + current comprehensive results, shadow mode) is compared against
  comprehensive verification; the deliverable is an evidence-backed go/revise/reject decision
  with rollback criteria. Any authoritative CI-policy change requires separate maintainer
  approval; the 2–5 min ordinary-PR target is bounded by setup-side overhead (§9.3), which
  selection cannot remove.
- Explicit rejections unchanged: universal effect frameworks, blanket TR migration,
  size-driven splitting, general deep-interface extraction programs.

## 7. Uncertainties, blockers, unmeasured quantities

- **Agent-trial metrics are PENDING** (§5): tokens-before-edit, files-read, tool-calls,
  owner-location time, first-attempt correctness have no measured values; sample sizes, model
  configurations and confounders are defined in the protocol for the executor.
- Fresh CL0 latency samples are n = 1–6 (stated per row); they corroborate but do not replace
  the reported distributions in `docs/testing.md`.
- Static graph excludes dynamic requires (24 production modules, §1.3); the true
  supplemental-evidence topology is partially unresolved. CL1 must mark, not infer.
- Co-change analysis is bounded by the 200-commit window and excludes release-only commits and
  exact moves (tool policy, versioned in the raw generator record).
- The two-counter README drift (§1.4) means "test lines" has two right answers depending on
  tool; both are recorded, neither was changed.
- Machine context: Racket 8.10 cs, Linux KVM guest; absolute times do not transfer to other
  hosts; ratios and pass/fail verdicts do.
- *(Review extension)* CI measurements (§9) are one observed PR run plus 12 recent successful
  runs for the total-duration figure; per-phase distributions beyond that sample are not
  measured, and no improvement candidate was implemented. Behavioral-contract assessment (§8)
  covers four components — representative, not a repository-wide inventory (CL1 scope).
- *(Review extension)* The selective-verification defect corpus is defined but unexecuted
  except the marked VERIFIED selection measurements (D9/D10/D11); detected/missed/false-green
  rates are PENDING until the shadow-mode protocol runs.

## Related existing work (not duplicated)

`docs/reports/ARCHITECTURE-BASELINE-v0.99.87.md` (previous baseline, superseded numbers),
`docs/architecture/maintainability-terminal-v0.99.93.rktd` + `MAINTAINABILITY-REASSESSMENT`
(MA-01..MA-12 ledger), `docs/architecture/orchestration-surface-reduction-v0.99.92.rktd`
(extraction rejections), `docs/reports/W2-EDIT-LOOP-MEASUREMENT-v1.00.33.md` (edit-loop
distribution of record), `docs/testing.md` (workflow + targets), `extensions/gsd/responsibility-inventory.rkt`
(machine-checked GSD ownership), `benchmarks/` (SDK-pipeline mock harness — complementary,
not agent trials).

## 8. Contract-driven, component-local verification (review extension, 2026-11-20)

Assessment basis: existing metadata only — `provide`/`contract-out` forms,
`extensions/gsd/responsibility-inventory.rkt` (machine-checked effects), `@covers`/
admission/quick-audit records, dependency-policy, the measured graph (§1), and the test
suite. No new contract framework or repository-wide component inventory is introduced in CL0.

**Structural vs behavioral independence.** The static graph is a clean DAG (§1.1) and the
policy is enforced, but that establishes only *structural* independence: no import cycles,
documented directions. *Behavioral* independence additionally requires that a component's
observable guarantees hold without re-executing other components — input/output semantics,
state invariants, event ordering, error/recovery, effects and resource ownership,
authorization, cancellation/concurrency. Three measured facts show the gap between the two:
(1) `util/` is reachable from 796 inbound edges and up to 365 transitive dependents — a
behavioral change in `util/error/errors.rkt` or `util/event/event.rkt` can alter observable
behavior everywhere while changing zero signatures; (2) 24 modules resolve dependencies
dynamically (`dynamic-require`), invisible to static analysis; (3) provider consumers bind to
message/stream *shapes*, not signatures (BUG-0003-class regressions), and view producers bind
to struct payloads whose authority comes from the producer, not the consumer (campaign views).
Acyclic imports + unchanged signatures are therefore **not sufficient evidence** for safe
component-local verification; they are necessary context only.

### 8.1 Component subset assessed

| Component (cohesive responsibility) | Public behavioral interface | Structural contracts | Guarantees actually established by tests | Provider / consumer conformance | Composition & integration | Implicit assumptions / incomplete contracts | Unknown / dynamic deps | Broader verification required when |
|---|---|---|---|---|---|---|---|---|
| **Campaign decision** (`extensions/gsd/campaign-decision.rkt`) — the ONE pure `/go` routing decision | 4 transparent view structs (`campaign-delivery-view`, `campaign-lane-view`, `campaign-views`) + `campaign-decision` (kind/route/code/wave-index/merge-sha/message) + `decide-campaign-action`, `decide-lane-action`, `lane-route`, `route-code`, `campaign-decision-next-action` | plain `provide`; transparent structs; **no `contract-out`** — structural contracts are the inventory's declared effects (`'()`) and the typed decision vocabulary | decision matrix pinned: `tests/test-gsd-campaign-decision.rkt` (22 test-cases, precedence incl. stale-readback/cancel-before-pending, finalize requires authenticated SHA, deferred disjointness, typed block/resume routes) + `tests/test-gsd-campaign-decision-pure.rkt` (6; purity isolation) | provider = view producers: `delivery-handoff.rkt` (authenticated delivery facts), `delivery-finalize.rkt` (lane facts) — pinned in their owner tests; consumer = `go-orchestrator.rkt` `case` dispatch on `kind` (line 1438), pinned by `tests/test-gsd-go-orchestrator.rkt` (81 test-cases) | composition: `tests/test-gsd-governance-workflow.rkt` (29), campaign-entry and supplemental/review resume routes | decision TRUSTS producer authenticity (`awaiting` SHA assumed 40-hex; "authenticated" is the producer's claim); wave-index first-record-order semantics; message text is operator-only (never parsed) — contract is documented but not machine-checked | none static (fan-out 2); authority chain dynamic-free | durable-state transitions, lease/fencing, or the interpreter's effect loop change; any change to view struct fields (consumers compile but semantics move) |
| **Supplemental evidence** (`supplemental-evidence-controller.rkt` + authority + journal) — governed supplemental T/K/S lane; the CL4 pilot candidate | 18+ provided operations spanning pointer I/O, task proposal, recovery, disposition, discovery, live-operation observation | no `contract-out`; inventory declares domain `campaign-state` with effects `(fs-write fs-read fs-rename fs-delete mkdir sha256 git parameterize path-ops dynamic-require)` — the widest effect set in the inventory | 34 supplemental* test files (entry, advance-failure, disposition store/campaign, preserved integration, publication, terminal observation) pin the governed flows incl. failure/restart paths | authority: `supplemental-evidence-authority.rkt` is the sole authority interpreter; controller consumes decisions — pinned by authority/evidence-readiness owners | journal + disposition stores + go-orchestrator routes; `tests/test-gsd-supplemental-*-integration` exercise composition | controller mixes observe/decide/coordinate (1826 LOC, fan-in 29); routing decisions not yet a pure kernel (the CL4 pilot evidence); pointer/disposition file formats documented but not centrally contracted | **8 dynamic-require edges** — the largest unresolved static blind spot; CL1 maps must mark them | any journal-schema or authority change; recovery paths; anything touching the dynamic dispatch targets |
| **Shared event foundation** (`util/event/*`, `agent/event-structs/*`) — cross-component event language | typed event envelopes + bus (synchronous, ordered, exception-isolated, circuit-breaker) | `typed/racket` contracts on core event modules (13 production TR modules incl. event.rkt, event-access.rkt) | `tests/test-event-bus.rkt` (+ relocation/unification owners), golden iteration-event owners | providers = every emitter; consumers = TUI/SDK/extension sinks — conformance via golden event tests | `tests/test-iteration-event-golden.rkt`, hook-golden | ordering guarantees documented; consumer tolerance to new event kinds varies by sink | none | any event-taxononomy or bus-ordering change (CL0 §2 co-change: event-structs files have 256–289 transitive dependents) |
| **Session store** (`runtime/session/session-store*`) — append-only JSONL, single durable session writer | facade provide; hash-chain verification/repair surface | inventory-level contracts; typed TR at store edges (agent-session) | `tests/test-session-store.rkt`, `tests/test-hash-chain.rkt`, `tests/test-session-store-integrity.rkt`, tree/fork owners | providers = session lifecycle writers only; consumers = resume/fork/context assembly | `tests/test-agent-session-tree.rkt`, session-tree-nav | repair semantics vs corruption classification boundary documented in code, thin in prose | none | recovery/replay behavior, hash-chain format, or fork semantics change |

### 8.2 What the subset shows for the amended objectives

- **Existing coverage is asymmetric:** pure-decision components (campaign decision) already
  have near-model contract shape — typed vocabulary, decision matrix tests, one interpreter,
  provider/consumer owners identifiable from `@covers` — while effectful coordinators
  (supplemental evidence) concentrate mixed responsibilities and dynamic dispatch. The amended
  objective's first-order gap is therefore *behavioral contract coverage and ownership
  clarity for effectful components*, not test volume (21,156 test-cases exist).
- **Verification-model mapping is derivable today** for these components from existing
  metadata: internal change → owner test + affected boundary (e.g. campaign-decision edit →
  the two decision tests + orchestrator consumer test); public interface change → + provider
  conformance (delivery-handoff/finalize owners) + composition (governance workflow);
  shared-dependency change (util/event, conn-pool) → transitive consumers (measured closure).
- **Conservative conditions are enumerable** from existing data: security/authority surfaces
  (worker-security, scratch, safe-mode), verification infrastructure (qt, admission,
  metadata), unresolved impact (dynamic-require modules, unmapped new source — the D10/D11
  corpus cases), and global invariants (hash chain, event ordering). These are exactly the
  escalation rules the defect corpus (`docs/reports/CL0-SELECTIVE-VERIFICATION-CORPUS-003e425d.md`)
  exercises.

## 9. Remote CI critical-path baseline (review extension, 2026-11-20)

Method: GitHub Actions REST jobs/steps API for PR #9983 run 44809549492 (head `211e0518`,
triggered 20:14:32Z) plus job-log timestamps; representativeness from the 12 most recent
successful CI runs on main/PRs (total wall clock 1456–1713 s, 2026-11-20). No workflow,
caching, branch-protection or required-context policy was changed.

### 9.1 PR #9983 run 44809549492 — phase measurements

| Phase | Measured | Evidence |
|---|---|---|
| Publish → CI start | ≈ 3 s (push 20:14:32 → jobs created 20:14:33 → started 20:14:35) | jobs API `created_at`/`started_at` |
| Queue time | 2–3 s per job (all 17 jobs) | jobs API |
| Env setup + package install (cold-side lint jobs, no prepared artifact) | `setup-racket` 330 s (lint-quality) / 365 s (lint); inside it, the package-setup compile sub-step = **301.4 s** (`duration_ms=301418` in the job log) — ≈200 s of that is `raco setup` compiling the `q/tests` collection alone | job log step markers |
| Cache preparation (producer) | `fast-env` `prepare-racket-environment` 263 s | jobs API step |
| Cache restoration (shards) | test-shard `setup-racket` 21–36 s (prepared artifact restore) vs 330–365 s cold | jobs API steps |
| Job-dependency waiting (critical path) | `lint` (374 s) → `fast-env` (282 s) → test shards: **11 min 1 s from lint start (20:14:35) to main test-job start (20:25:36)** | jobs API timestamps |
| Racket compilation before execution | test shards: compile gate 59–82 s per shard (bounded eager compile) | jobs API steps |
| Actual test execution | shards in progress at sampling; recent-run shard execution ≈ 746–840 s job duration incl. setup (run 44771765560) | jobs API |
| Lint and security verification | lint-quality `lint-all.rkt` 39 s (on a warm store, post-setup); `security` suite 113 s execution / 387 s job | job log / jobs API |
| Publish → authoritative verdict (recent successful runs) | **1456–1713 s total wall clock (≈24–29 min)** across 12 runs; aggregate runner seconds ≈ 5160 for one main run vs 1525 s critical path (3.4× fan-out) | runs API, 2026-11-20 sample |
| Retries/reruns | attempt 1; no retries in this run | `run_attempt: 1` |

The three inspected observations are **confirmed exactly**: ≈301 s package-setup compile in
lint-quality (301.418 s sub-step), ≈5 min 30 s Racket setup (330 s step), ≈11 min from
lint-job start to main test jobs start (11 min 01 s). Representativeness: the shape (two
sequential setup stages — lint-gate side, then producer → shards — each paying large
Racket/package setup) recurs across the sampled runs; absolute numbers vary with cache hits.

### 9.2 The failed run as derived-metadata coordination evidence

The same run's `lint-quality` failed `metrics-sync`, `metrics-lint` and `prose`: the branch's
new files changed tree counts, and README's published metrics lagged. A targeted local
`scripts/metrics.rkt --lint` had **passed** earlier on this branch while the new files were
still untracked — the counter reads git-tracked files, so targeted local lint could not see
the drift that comprehensive lint-quality sees. Fixed by the authoritative
`scripts/metrics.rkt --sync-all` (README 1680 test files / 979 source modules / 239,865
source lines / 366,702 test lines / 52,746 assertions); post-sync local `lint-all.rkt`:
**23 passed / 2 warnings (81 s)**. Recorded as a concrete instance of (a) derived-metadata
coordination overhead (§4.2) and (b) the difference between targeted lint and the complete
lint-quality check. No metric requirement was weakened; CL1 owns any architectural
simplification of them.

### 9.3 Feasible improvement candidates (recorded only; no CI change in CL0)

1. **Sequential setup stages dominate the critical path:** with prepared-environment
   restore, shard setup drops from ≈330 s to 21–36 s — extending producer-restore coverage to
   more lanes (or making the prepared artifact the default path where its guards allow) is the
   single largest measured lever.
2. **`q/tests` collection compilation inside `raco setup` (~200 s of the 301 s cold sub-step)**
   — the test corpus is compiled as part of package setup on the fallback path; a lane split
   that defers test-collection compilation off the critical setup stage would cut the
   producer's 263 s and the cold jobs' 301 s (existing lane-split machinery already exists —
   v1.00.16 W3 policy doc).
3. **Lint-quality could consume the prepared environment or run before the heavy setup** —
   39 s of lint inside a 375 s job shows a 10× setup-to-work ratio; the W2 topology checkpoint
   deliberately keeps it producer-independent, so any change here must respect that recorded
   constraint.
4. **Critical-path shape:** 2–5 min ordinary-PR verdict is unreachable while publish→verdict
   includes two sequential setup stages (~9 min before shards start). Component-local
   *selection* alone cannot fix this — the fixed overhead is environment, not test volume;
   this bounds what CL4/CL5 can honestly claim from selection without setup-side changes.

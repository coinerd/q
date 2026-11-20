# CL0 Baseline — Cognitive Load & Architecture (issue #9977)

Parent initiative: #9976 (Cognitive Load Reduction & Agent-Oriented Architecture).
Status: **baseline only** — evidence of record; no production refactoring, no CL1+ implementation.

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
  the existing `@covers` manifest + dependency graph; no new metadata.
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
before first useful edit, files read, tool calls, owner-location time, elapsed, subsystems
touched, unrelated modifications, first-attempt correctness, verification outcome).

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
higher coordination cost, swap the pilot; the decision procedure is recorded here.

**CL1–CL5 recommendations (go/revise/reject criteria):**
- CL1 GO if maps demonstrably reduce reads-to-owner on ≥2 of the frozen benchmark tasks
  (reproduce §4 protocol with fresh contexts).
- CL2 GO only if CL1 maps exist and the navigator's matched-task deltas beat CL0 numbers;
  reject features whose maintenance exceeds the load they remove.
- CL3 EVALUATE strictly per #9911 scope: startup/compile separation is already evidenced here
  (§4.1); the worker must beat one-shot on representative debugging tasks without weakening
  isolation; otherwise reject.
- CL4 PROPOSED pilot above; reject extraction if locality gain does not survive the before/after
  import + fan-in comparison (MA-10 discipline).
- CL5 GO only as the comparative gate for whatever shipped; its protocol is frozen today.

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

## Related existing work (not duplicated)

`docs/reports/ARCHITECTURE-BASELINE-v0.99.87.md` (previous baseline, superseded numbers),
`docs/architecture/maintainability-terminal-v0.99.93.rktd` + `MAINTAINABILITY-REASSESSMENT`
(MA-01..MA-12 ledger), `docs/architecture/orchestration-surface-reduction-v0.99.92.rktd`
(extraction rejections), `docs/reports/W2-EDIT-LOOP-MEASUREMENT-v1.00.33.md` (edit-loop
distribution of record), `docs/testing.md` (workflow + targets), `extensions/gsd/responsibility-inventory.rkt`
(machine-checked GSD ownership), `benchmarks/` (SDK-pipeline mock harness — complementary,
not agent trials).

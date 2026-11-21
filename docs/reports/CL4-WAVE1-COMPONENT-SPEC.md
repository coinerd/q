# CL4 Wave 1 — component boundary specification (proposal)

Status: **proposal for review. No production code, API, test-runner or CI policy is changed by this
document.** Base revision `83ce55bb05ddaabc6e62f78e8b6d168bd387cfe6`.

Parent initiative: #9976. Issue: #9981. Precedent: `extensions/gsd/campaign-decision.rkt` (pure
decision kernel) + `extensions/gsd/effect-ports.rkt` (named effects).

## 1. Method and reproduction

Evidence was read from existing artifacts only; nothing was inferred from module size alone.

| Source | Use | Reproduction |
| --- | --- | --- |
| CL0 baseline | hotspot, co-change, authority/effect ownership | `docs/reports/CL0-BASELINE-003e425d.md` §2–§3 |
| CL1 maps/manifest | capability ownership, dynamic-edge marks | `racket scripts/subsystem-manifest.rkt --out /tmp/m.rktd`, `docs/architecture/subsystem-maps/gsd-supplemental-evidence.md` |
| CL2 navigator | static callers, requires, exports, owner tests, uncertainty | `racket scripts/semantic-navigator.rkt module <path> --machine` |
| Coverage manifest | current test selection for the disposition area | `tests/.coverage-manifest.json` |

CL0 already names the region: the GSD delivery + supplemental-evidence workflow is the
poor-locality area, `supplemental-evidence-controller.rkt` (1826 LOC, fan-in 29) mixes
observe/decide/coordinate, and supplemental evidence has the largest unresolved static blind spot
(8 `dynamic-require` edges). CL0's own candidate list ends with
"supplemental-evidence controller observe→decide split". This spec narrows that to one cohesive
gate rather than restructuring the controller.

## 2. Candidate comparison (measured, revision `83ce55bb`)

| Candidate | Prod callers | Test callers | Requires | Exports | Dynamic edges | Existing pure kernel | Owner tests |
| --- | ---: | ---: | ---: | ---: | ---: | --- | ---: |
| **A. Disposition eligibility gate** (`supplemental-evidence-disposition.rkt`) | 0 static (reached via controller/TUI) | 0 static | 17 | 2 | 0 own; controller reaches it dynamically | partial (`supplemental-disposition-policy.rkt`, `supplemental-disposition-claim.rkt`) | 1 |
| B. Delivery-stage routing (`delivery-finalize.rkt`) | 1 | 15 | 16 | 20 | 0 | `delivery-outcome.rkt` (partially extracted, #9969) | 7 |
| C. Independent review proof lifecycle (`review-proof.rkt`) | 18 | 46 | 10 | 15 | 0 | `review-proof-format.rkt` (pure formatting only) | 3 |

Reference modules: `campaign-decision.rkt` (3 prod / 3 test callers, 1 require, 5 exports) shows the
target shape; `effect-ports.rkt` (0 requires, 23 exports) shows the effect vocabulary.

### Why A, and not B or C

- **A** is a single state-machine gate (may this parent wave be disposed?) whose rules are currently
  interleaved with a lease, an authority lock, `git rev-parse`, journal/receipt reads and pointer
  I/O. A rule change today means editing effectful code and re-running slow, repo-mutating ladders.
- **B** already has a pure decision extracted and 15 test callers; further splitting would fragment
  effect orchestration across more modules without a new behavioral boundary. CL0 §3 records the
  delivery decision as "only partially extracted", but the remaining coordinator code is effect
  sequencing, not a missing decision kernel.
- **C** has the widest fan-in in the repository (18 production, 46 test callers) and its remaining
  mixed concern is authenticated proof capture plus a subprocess verification run; extracting a
  pure core there would either duplicate authentication or churn 60+ callers — too broad for one
  bounded pilot wave.

## 3. Current behavior of the selected component

Owner: `extensions/gsd/supplemental-evidence-disposition.rkt` (235 LOC, exports exactly two
functions). It is the sole owner of "may this exact parent wave be dispositioned, and if so install
the claim".

### 3.1 Authority and effect map (today)

| Concern | Current carrier | Notes |
| --- | --- | --- |
| Durable state (single writer) | `supplemental-disposition-claim.rkt` via `install-supplemental-disposition-claim!` | compare-and-swap on `old` → `target`; the module never writes wave status |
| Authority facts | `supplemental-evidence-authority.rkt` (lock + approval validation), `evidence-readiness.rkt` (authenticated receipt context), `review-proof.rkt` (receipt-owned proof) | adapters consume decisions; disposition re-authenticates before retaining bytes |
| Policy identity | `supplemental-disposition-policy.rkt` (fixed amendment), `supplemental-disposition-claim.rkt` (`disposition-late-terminal?`) | immutable identity, never an authorization callback |
| Preserved binding | `supplemental-preserved-candidate.rkt` (`preserved-candidate-binding`, `validate-preserved-candidate!`) | rechecked at proposal and immediately before install |
| Concurrency | `campaign-lease.rkt` (one lease holder) + `call-with-supplemental-authority-lock` | whole propose/install runs under lease **and** lock |
| Environment | `git rev-parse HEAD` (`control-head`), `current-gsd-author-phase` | effectful facts woven into the gate |
| Pointer I/O | `supplemental-evidence-controller.rkt` (`load-supplemental-pointer`, `supplemental-pointer-path`) | static back-edge to the module that dynamically requires this one |

### 3.2 Ordered refusals that must be preserved

The gate is authentication-sensitive; the pilot must keep this exact order and these fail-closed
outcomes (messages may move, text must not drift):

1. `author phase cannot dispose tasks` — checked before taking the lease.
2. `campaign lease busy` — no lease, no observation.
3. `outside the fixed disposition amendment` — plan/wave/pointer must equal the fixed amendment.
4. `parent terminal or digest namespace changed` — terminal body digest and record digest match.
5. `wrong failed parent allocation` — allocation must match the amendment.
6. `parent must own current failed (or exact fixed late awaiting-delivery) uncancelled unpaused attempt and fences`.
7. `parent context, frozen snapshot or receipt changed` — context equality (preserved mode uses the
   retained-context helper; ordinary mode keeps live-source equality).
8. `retained clean source or authenticated owned receipt proof changed` — manifest/head/tree equality.

Then proposal additionally refuses `missing parent pointer`, `expected parent proposal changed`,
`fresh bounded expiry required`, `active pointer is not the fixed amendment predecessor`,
`invalid disposition lineage`, and preserved-mode `preserved candidate binding is malformed` /
`preserved candidate requires the fixed third-generation amendment`. Install additionally refuses
`exact genuine disposition task approval required` and `disposition requires task/v3`.

### 3.3 Testability today

| Test | Lines | Annotation | Current role |
| --- | ---: | --- | --- |
| `tests/test-gsd-supplemental-disposition-eligibility.rkt` | 402 | slow, integration, `@mutates repo`, timeout 7200 | the only precedence/refusal matrix; needs a real campaign ladder |
| `tests/test-gsd-supplemental-preserved-integration.rkt` | 1276 | slow, integration, `@mutates repo`, timeout 3600 | `@covers` the disposition module end to end |
| `tests/test-gsd-supplemental-disposition-campaign.rkt` | 512 | slow, integration, timeout 14400 | real broker install path |
| `tests/test-gsd-supplemental-disposition-frontend.rkt` | 363 | slow, integration, timeout 1200 | TUI/CLI consequence |
| `tests/test-gsd-supplemental-disposition-store.rkt` | 846 | fast claim, unit, timeout 600 | store CAS; notes ~81 s warm with real fixtures |
| `tests/test-gsd-supplemental-preserved-candidate.rkt` | 820 | slow, unit, timeout 600 | preserved authentication |

Coverage manifest: only two entries mention this area, so selection is opaque: a one-rule change can
pull in multi-thousand-line, repo-mutating ladders.

## 4. Proposed boundary (Wave 2 scope, not implemented here)

New module: `extensions/gsd/supplemental-disposition-decision.rkt` — pure, no I/O, no parameters,
no subprocess, no state.

```text
(struct disposition-request-view (amendment pointer parent-sha expires-at preserved?))
(struct disposition-parent-view  (proposal terminal allocation))
(struct disposition-campaign-view (record wave attempt))
(struct disposition-context-view  (context source journal receipt manifest))
(struct disposition-decision      (status code message facts))   ; status = allow | refuse
(define (decide-supplemental-disposition views now) disposition-decision)
```

- **Inputs**: only already-authenticated facts. The adapter observes them (lease + lock + journal,
  receipt, proof and git reads) and passes them in; the core never reads the filesystem, never takes
  a lease, never calls `git`.
- **Outputs**: one typed `disposition-decision`. `allow` carries the exact facts the installer needs
  (task lineage inputs); `refuse` carries a stable machine `code` plus the verbatim message the
  adapter raises today, so refusal text has a single source.
- **Adapter**: `supplemental-evidence-disposition.rkt` keeps its two exports and its lease/lock/
  effect sequencing, calls the core inside the same lease, and executes install/claim as today.
- **No public API change**: exports stay `propose-supplemental-disposition!` and
  `install-supplemental-disposition!`; controller and TUI are untouched.

### 4.1 Contract dimensions

| Dimension | Specification |
| --- | --- |
| Inputs/outputs | five view structs + `now` → one `disposition-decision` (`allow`/`refuse`); `allow` only when every authenticated fact matches |
| State ownership | core owns **no** state; durable writer remains `supplemental-disposition-claim.rkt` (single CAS writer); wave status is never mutated here |
| Side effects | core: none (verified by a purity test that forbids file, subprocess, parameter and `dynamic-require` use); adapter: lease, lock, git head, journal/receipt/proof reads, claim install |
| Ordering | refusals 1–8 above in the same sequence; authorization precedes any retention; install re-check precedes the claim mutation; no prose-based authorization anywhere |
| Concurrency | decision is a pure function of one observation epoch; it must never be cached or reused across leases; the adapter must recompute after acquiring the lease and lock and before install; install keeps the existing compare-and-swap (`old` → `target`); idempotent re-install with an identical target remains a no-op refusal path, not a second write |
| Failure | fail-closed: any missing/unparseable fact, non-matching digest, changed fence or unauthenticated approval yields `refuse` with a stable code; exceptions in the adapter are unchanged (`error` → refusal, nothing retained) |
| Tests | new fast unit matrix for the decision core (each refusal code + one allow case); existing slow owners stay unchanged as integration/golden guards; no assertion may be weakened |
| Compatibility | no export, struct, journal, receipt or pointer format change; controller dynamic dispatch and its 4 dynamic edges unchanged |

### 4.2 Expected test selection

New, fast, mutation-free: `tests/test-gsd-supplemental-disposition-decision.rkt` with one case per
refusal code (8), the preserved/ordinary context split, the late-terminal admission, expiry bounds,
and one positive allow. Target: `< 5 s`, no `@mutates repo`, no fixture ladder.

Retained unchanged (integration guards): the six tests listed in §3.3. This selection is **shadow
evidence only** — required CI contexts, admission records and `qt` remain authoritative.

## 5. Before/after comparison criteria (fixed now, measured in Wave 2)

| Criterion | Before (today) | Target after | How measured |
| --- | --- | --- | --- |
| Locality of a rule change | disposition + policy/claim + slow eligibility ladder (≥3 files, repo mutation) | core + its unit test only (≤2 files, no mutation) | file list of a scripted "add one refusal" change |
| Decision-core dependencies | 17 requires including git, lease, journal, receipts | ≤3 (policy/format/claim only) | `raco make` requires list / navigator `requires` |
| Public surface | 2 exports | 2 exports (unchanged) | navigator `exports` |
| Static edges | controller ↔ disposition back-edge + dynamic reach | unchanged or fewer dynamic edges | navigator `module` output before/after |
| Verification feedback | slow ladders on any rule change | new pure suite p50 measured; integration owners unchanged | `raco test` timing, n ≥ 5 |
| Contract completeness | refusals asserted only end-to-end | every refusal code has a direct provider/consumer case | test matrix coverage list |
| Metadata fan-out cost | — | recorded (coverage manifest, admission, ownership, README) | `git diff --stat` of the Wave 2 PR |

**Rejection criteria.** Reject the extraction if it (a) adds the module but a rule change still
requires a slow fixture test, (b) needs more than three non-pure requires, (c) duplicates refusal
logic or text in two places, (d) changes a public export or moves authentication into the core, or
(e) only adds files and indirection without improving the locality/feedback measurements.

## 6. Risks and non-goals

- **TOCTOU.** Mitigation: the core never observes; views come from one lease-and-lock epoch and the
  adapter recomputes immediately before install.
- **Authentication drift.** Mitigation: the core consumes only facts the existing authenticated
  helpers already produced; `validate-preserved-candidate!` and receipt/proof loaders stay in the
  adapter and remain mandatory.
- **Refusal-text drift.** Mitigation: messages live in the core and the adapter raises them.
- **Non-goals:** controller split, journal/receipt/pointer format changes, preview/message changes,
  new permission model, test-runner/scheduler/CI changes, generic framework or DSL, any broad GSD
  rearrangement, and CL0 trials (owned by CL5 #9982).

## 7. Wave 2 plan after approval

Files: `extensions/gsd/supplemental-disposition-decision.rkt`,
`extensions/gsd/supplemental-evidence-disposition.rkt` (adapter only),
`tests/test-gsd-supplemental-disposition-decision.rkt`, plus the tracked metadata syncs that a new
test file requires (coverage manifest, quick admission, ownership map, README counts).

Verify: `raco test tests/test-gsd-supplemental-disposition-decision.rkt`, the retained owner tests,
`racket scripts/lint-all.rkt`, metadata lint/alignment, and the impact-selected `./qt` subset; then
re-run the CL2 navigator and the §5 measurements, and publish before/after numbers in the PR. Stop
again at the review boundary before any merge decision.

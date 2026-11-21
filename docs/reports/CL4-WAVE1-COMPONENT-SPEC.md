# CL4 Wave 1 — component boundary specification (revision 2, proposal)

Status: **proposal for review. No production code, public API, test-runner or CI policy is changed
by this document.** Base revision `83ce55bb05ddaabc6e62f78e8b6d168bd387cfe6`.

Revision 2 incorporates three review corrections: entrypoint-specific refusal stages, an
effect-free transitive dependency rule for the decision core, and a separation of source-change
locality from authoritative verification.

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

CL0 already names the region: GSD delivery + supplemental evidence is the poor-locality area,
`supplemental-evidence-controller.rkt` (1826 LOC, fan-in 29) mixes observe/decide/coordinate, and
supplemental evidence has the largest unresolved static blind spot (8 `dynamic-require` edges).
This spec narrows CL0's "observe→decide split" to one cohesive gate.

## 2. Candidate comparison (measured, revision `83ce55bb`)

| Candidate | Prod callers | Test callers | Requires | Exports | Dynamic edges | Existing pure kernel | Owner tests |
| --- | ---: | ---: | ---: | ---: | ---: | --- | ---: |
| **A. Disposition eligibility gate** (`supplemental-evidence-disposition.rkt`) | 0 static (controller/TUI) | 0 static | 17 | 2 | 0 own | partial (policy/claim) | 1 |
| B. Delivery-stage routing (`delivery-finalize.rkt`) | 1 | 15 | 16 | 20 | 0 | `delivery-outcome.rkt` (#9969) | 7 |
| C. Review proof lifecycle (`review-proof.rkt`) | 18 | 46 | 10 | 15 | 0 | `review-proof-format.rkt` (formatting only) | 3 |

Reference shape: `campaign-decision.rkt` (3 prod / 3 test callers, 1 require, 5 exports) and
`effect-ports.rkt` (0 requires, 23 exports).

**Why A.** It is a single state-machine gate whose rules are interleaved with a lease, an authority
lock, `git rev-parse`, journal/receipt reads and pointer I/O. **Why not B.** The decision kernel is
already extracted; what remains is effect sequencing, so splitting would fragment without a new
behavioral boundary. **Why not C.** 18 production and 46 test callers plus authentication capture
make a pure-core extraction either duplicative or a 60-caller churn — too broad for one pilot wave.

## 3. Current behavior of the selected component

Owner: `extensions/gsd/supplemental-evidence-disposition.rkt` (235 LOC, exactly two exports).

### 3.1 Authority and effect map

| Concern | Current carrier | Notes |
| --- | --- | --- |
| Durable state (single writer) | `supplemental-disposition-claim.rkt` (`install-supplemental-disposition-claim!`) | compare-and-swap `old` → `target`; wave status never mutated here |
| Authority facts | `supplemental-evidence-authority.rkt`, `evidence-readiness.rkt`, `review-proof.rkt` | adapters consume decisions; disposition re-authenticates before retaining bytes |
| Policy identity | `supplemental-disposition-policy.rkt`, `supplemental-disposition-claim.rkt` | immutable identity, never an authorization callback |
| Preserved binding | `supplemental-preserved-candidate.rkt` | rechecked at proposal and immediately before install |
| Concurrency | `campaign-lease.rkt` + `call-with-supplemental-authority-lock` | propose/install run under lease **and** lock |
| Environment | `git rev-parse HEAD`, `current-gsd-author-phase` | effectful facts woven into the gate |
| Pointer I/O | `supplemental-evidence-controller.rkt` | static back-edge to the module that dynamically requires this one |

### 3.2 Entrypoint stages and refusal ordering (must be preserved)

The current code has **three ordering contexts**, not one flat list. Moving all checks into a single
function would change which refusal wins and could perform an authenticated read where the old code
refused earlier.

| Stage | Entrypoint | Preconditions | Ordered refusals (current text) |
| --- | --- | --- | --- |
| 0a | both | before any lease | `author phase cannot dispose tasks` |
| 0b | both | lease acquisition | `campaign lease busy` |
| P1 | proposal | pre-lease, pure shape/binding policy | `preserved candidate binding is malformed`, `preserved candidate requires the fixed third-generation amendment` |
| P2 | proposal | under lease + authority lock, before eligibility | `missing parent pointer`, `expected parent proposal changed` |
| E | both | under lease + authority lock | 1 `outside the fixed disposition amendment`; 2 `parent terminal or digest namespace changed`; 3 `wrong failed parent allocation`; 4 `parent must own current failed (or exact fixed late awaiting-delivery) uncancelled unpaused attempt and fences`; 5 `parent context, frozen snapshot or receipt changed`; 6 `retained clean source or authenticated owned receipt proof changed` |
| P3 | proposal | after eligibility | `fresh bounded expiry required`, `active pointer is not the fixed amendment predecessor`, `invalid disposition lineage`, then `validate-preserved-candidate!` |
| I1 | install | under lease + authority lock, before eligibility | `exact genuine disposition task approval required`, `disposition requires task/v3`, then preserved-binding extraction |
| I2 | install | after eligibility | `validate-preserved-candidate!`, then CAS claim install |
| — | both | after E (proposal) / I2 (install) | `propose-retain!` / `install-supplemental-disposition-claim!` |

Observable-effect invariants to preserve:

- P1 refusals happen **before** any lease or lock is taken (a malformed preserved binding must not
  churn lease/lock state).
- P2 and I1 refusals happen **after** lease + lock and before any eligibility read.
- E refusals happen before any retention or claim write; E always runs under lease **and** lock.
- No stage may reorder the others; a refusal at an earlier stage must win even when later-stage
  conditions are also invalid.

### 3.3 Testability today

| Test | Lines | Annotation | Current role |
| --- | ---: | --- | --- |
| `tests/test-gsd-supplemental-disposition-eligibility.rkt` | 402 | slow, integration, `@mutates repo`, timeout 7200 | only precedence/refusal matrix; real campaign ladder |
| `tests/test-gsd-supplemental-preserved-integration.rkt` | 1276 | slow, integration, `@mutates repo`, timeout 3600 | manifest `@covers` entry for the disposition module |
| `tests/test-gsd-supplemental-disposition-campaign.rkt` | 512 | slow, integration, timeout 14400 | real broker install path |
| `tests/test-gsd-supplemental-disposition-frontend.rkt` | 363 | slow, integration, timeout 1200 | TUI/CLI consequence |
| `tests/test-gsd-supplemental-disposition-store.rkt` | 846 | fast claim, unit, timeout 600 | store CAS; ~81 s warm with real fixtures |
| `tests/test-gsd-supplemental-preserved-candidate.rkt` | 820 | slow, unit, timeout 600 | preserved authentication |

Only two coverage-manifest entries mention this area, so selection is opaque: a one-rule change can
pull in multi-thousand-line, repo-mutating ladders.

## 4. Proposed boundary (Wave 2 scope, not implemented here)

New module: `extensions/gsd/supplemental-disposition-decision.rkt` — pure decision evaluation over
facts the adapter already authenticated.

### 4.1 Stage-preserving extraction

The core is invoked **only at stage E**, never at P1/P2/P3/I1/I2:

- P1 stays a pure predicate evaluated by the adapter **before** the lease. Its inputs are named
  booleans computed by the adapter (`preserved-shape-valid?`, `third-generation?`), so the core needs
  no policy import.
- P2 stays adapter-side (pointer load + expected-sha comparison).
- Stage E becomes one pure call: `(decide-supplemental-disposition eligibility-views)`.
- P3/I1/I2 stay adapter-side; P3 additionally builds the descriptor and seals the task, I2
  re-authenticates preserved facts and performs the CAS install.

### 4.2 Core inputs are normalized immutable facts (no effectful imports)

```text
(struct disposition-amendment-view (plan-id wave parent-proposal parent-approval
                                    terminal-body-sha256 terminal-record-sha256 allocation))
(struct disposition-terminal-view  (proposal-sha256 terminal-sha256 terminal-record-sha256
                                    allocation late-terminal?))
(struct disposition-campaign-view   (record-present? wave-found? attempt-found? wave-status
                                     cancelled? budget-paused? attempt-id attempt-fence
                                     coordinator-fence))
(struct disposition-context-view    (plan-id wave snapshot-sha256 expected-context actual-context))
(struct disposition-source-view     (source-present? manifest-present? execution-cwd context-repo
                                     branch-match? origin-match? head-match? tree-match?))
(struct disposition-decision        (status code message))   ; status = allow | refuse
```

- Every field is either a primitive or an already-authenticated immutable value read by the adapter;
  `late-terminal?` and the context/ownership comparisons are computed by the adapter where they
  require authority or storage helpers.
- `decide-supplemental-disposition` evaluates stages in order and returns `refuse` with the exact
  current message and a stable machine code, or `allow`.
- **Dependency invariant:** the core's **transitive import closure contains no effectful module**.
  Target closure is `{racket/base}` only (structs and pure helpers in the same file). If a pure
  predicate is shared, it moves to a leaf module whose own closure is also `{racket/base}`.
  `supplemental-disposition-claim.rkt`, `supplemental-disposition-policy.rkt`,
  `supplemental-evidence-format.rkt` and `supplemental-evidence-authority.rkt` must **not** appear in
  the closure — they import storage, authority or I/O.
- Enforcement in tests: walk the CL2 navigator's static import index from the core and fail closed on
  any unresolved edge or module outside the allowed set; plus a source audit asserting the core never
  uses `dynamic-require`, `system`, file ports, `current-directory` or parameters.

### 4.3 Contract dimensions

| Dimension | Specification |
| --- | --- |
| Inputs/outputs | five view structs → one `disposition-decision` (`allow`/`refuse` + stable code + verbatim adapter message) |
| State ownership | core owns no state; durable writer stays `supplemental-disposition-claim.rkt` (single CAS); wave status untouched |
| Side effects | core: none, and none transitively; adapter keeps lease, lock, git head, journal/receipt/proof reads, retention and claim install |
| Ordering | stages 0, P1, P2, E, P3, I1, I2 as in §3.2; earlier-stage refusal always wins; no prose-based authorization |
| Concurrency | decision is a pure function of one observation epoch; never cached across leases; recomputed after lease+lock and again before install; CAS conflict stays a refusal path; duplicate install with identical target stays a no-op |
| Failure | fail-closed: missing/unparseable/changed facts → `refuse`; adapter keeps raising, nothing retained; no partial write before E/I2 |
| Tests | new fast unit matrix (individual + **simultaneous invalid conditions** for precedence), purity/closure test, adapter effect tests; existing slow owners retained unchanged |
| Compatibility | two exports unchanged; no struct/journal/receipt/pointer format change; controller dynamic edges unchanged |

### 4.4 Expected test selection

New fast, mutation-free files:

- `tests/test-gsd-supplemental-disposition-decision.rkt` — one case per refusal code, one allow,
  preserved vs ordinary context split, late-terminal admission, malformed/absent facts, and
  **multi-fault precedence cases** (e.g. amendment mismatch + terminal drift; wrong allocation +
  stale fence; context drift + source drift) asserting the winning code and the exact message.
- `tests/test-gsd-supplemental-disposition-decision-purity.rkt` — transitive closure and source
  audit described above.

Retained unchanged as integration guards: the six tests in §3.3. Shadow evidence only: required CI
contexts, admission records and `qt` selection remain authoritative.

## 5. Before/after measurement (reported separately, per review)

| Measure | Wave 2 goal | How measured |
| --- | --- | --- |
| Source-change locality | core + focused tests, ideally ≤2 files | file list of a scripted "add one refusal" change |
| Local feedback | fast pure tests, target < 5 s | `raco test` timing, n ≥ 5 |
| Required verification | **unchanged**: existing gates and `qt` selection | the full required `./qt` result recorded before/after |
| Shadow test selection | demonstrates coverage of changed decisions and affected consumer contracts | decision-code matrix + consumer/owner test mapping (shadow only) |
| End-to-end correctness | retained integration, restart, failure and CAS tests still pass | the six slow owners, unchanged |
| Dependency invariant | no effectful module in the core's transitive closure | navigator static walk + source audit |
| Public surface / edges | 2 exports, no new dynamic edges | navigator `module` before/after |
| Metadata fan-out cost | recorded, not hidden | `git diff --stat` for coverage manifest, admission, ownership, README |

**Rejection criteria.** Reject the extraction if (a) a rule change still requires a slow fixture
test, (b) the core's transitive closure contains any effectful module, (c) refusal text or ordering
is duplicated or changes, (d) a public export or authentication ownership moves, (e) any required
verification result changes, or (f) the change only adds files/indirection without improving these
measurements.

## 6. Risks and non-goals

- **Stage drift.** Mitigation: stage table is the contract; precedence tests cover multi-fault
  inputs; adapter keeps P1/P2/P3/I1/I2 in place.
- **TOCTOU.** Mitigation: core never observes; views come from one lease+lock epoch and are
  recomputed before install.
- **Authentication drift.** Mitigation: `validate-preserved-candidate!`, receipt/proof loaders and
  approval validation stay in the adapter and remain mandatory.
- **Non-goals:** controller split, journal/receipt/pointer format changes, permission model changes,
  test-runner/scheduler/CI changes, generic framework or DSL, broad GSD rearrangement, and CL0
  trials (owned by CL5 #9982).

## 7. Wave 2 plan (after these corrections are accepted)

Files: `extensions/gsd/supplemental-disposition-decision.rkt`,
`extensions/gsd/supplemental-evidence-disposition.rkt` (stage-preserving adapter),
`tests/test-gsd-supplemental-disposition-decision.rkt`,
`tests/test-gsd-supplemental-disposition-decision-purity.rkt`, plus tracked metadata syncs a new
test file requires.

Verify: the two new test files, the six retained owner tests, `racket scripts/lint-all.rkt`,
metadata lint/alignment, and the unchanged required `./qt` verification; then re-run the CL2
navigator and report §5 in the PR. Stop at another review boundary; no merge without approval.

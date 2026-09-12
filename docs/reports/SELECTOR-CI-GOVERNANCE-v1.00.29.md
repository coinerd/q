# Selector CI Governance — v1.00.29 (W7 deliverable)

**Wave:** v1.00.29-W7 "Selector governance and bounded shadow design"
**Spec:** `PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md` §6 W5 (authoritative), §10 (selector threat model), §11.3 (no silent fallback), §1.3 (canonical impact-selector constraint)
**Status:** PROPOSED this wave. The evaluator ships as shadow/offline tooling only; no CI wiring of any kind.
**Evaluator:** `scripts/impact-selector/evaluate.rkt` (schema `q.impact-decision/1`) @ `campaign/v1.00.29-w7` 6481184e
**Replay evidence:** `artifacts/proof-graph/v1.00.29-w7/replay-results.json` (+ `SHA256SUMS`)

---

## 0. Hard prohibition — mechanically verified, still in force

`.github/workflows/**` contains **no** test-impact execution job (the historical
over-budget `test-impact` job stays dead; nothing returns under a new name).
Mechanical proof from this wave's worktree at the implementation SHA:

```text
$ grep -rin "test-impact\|impact-select" .github/workflows/
(no matches; grep exit code 1 = no matches found)
```

A wider scan for the bare token `impact` finds exactly one hit — a comment in
`full-regression.yml` restating the prohibition itself ("impact selection is
opt-in local-only — never a CI gate"), i.e. the prohibition is documented
inside CI, not violated by it. The evaluator added by this wave has **no
test-executing code path** (no subprocess, no system call, no thread, no
sleep, no network — asserted by a static source scan in
`tests/test-impact-selector-evaluate.rkt`) and is referenced by nothing in
`.github/workflows/`.

---

## 1. Why a new governed design (and not a restoration)

The canonical TDD strategy (§ *Impact selection — implemented, fail-open,
local opt-in only*) records that the former `test-impact` GitHub Actions job
exceeded the repository's 30-minute job limit and was removed, and that it
must not be reintroduced. Any future CI use of change-impact selection is a
**new governed design** requiring (spec §1.3): a separately reviewed strategy
amendment; a bounded selector-evaluation design; a non-required pilot;
retained miss/fallback/latency evidence; and explicit approval before any
broad required PR proof is replaced.

W7 delivers items 1–3 of that list as *design + offline evidence*, and frames
the approval gate that decides whether the W8 pilot runs at all. Under this
wave the selector cannot reduce required coverage: every decision it emits is
either the broad tier, a narrow *recommendation* that nobody acts on, or an
explicit `fallback:broad`.

## 2. Static, explanation-only evaluation architecture

The evaluator is a pure function of two inputs:

1. **A diff manifest** (`q.impact-diff/1`): the changed file list + change
   kind (`modified | added | deleted | renamed`, with `old_path` for renames).
   In the future pilot this comes from `git diff-tree` of the PR merge
   commit; in this wave's replay it came from exactly that command.
2. **The W0 proof graph** (`artifacts/proof-graph/v1.00.29-w0/graph.json` +
   its normative sibling `claims.json`), optionally accompanied by a
   `selector-mapping.json` companion in the same directory (schema
   `q.selector-mapping/1`): @covers-derived coverage entries (test → covered
   sources), area rules (path prefix → area), and *uncertain-source markers*
   for modules whose dependencies cannot be resolved statically.

Output: **one decision record** (`q.impact-decision/1`), never an action:

```text
decision  = "broad" | "selected" | "fallback:broad"
reason    = named reason (member of the declared broadening-reasons list)
reason_class = §10 threat-model class id
source_sha, graph_sha256 (FIPS 180-4), consumer
affected_claim_set  (§11.3: W0 claim_ids; empty only when unknowable)
tests_run = 0, zero_test_execution = true   (by construction)
selected = {tests, suites}   (only when decision = "selected")
budget_ms, budget_hard_timeout_ms, budget_ok, elapsed_ms
fallback_chain = per-file outcome/reason trail
```

Three properties are structural, not aspirational:

- **Explanation-only.** The decision record is the product. Nothing executes
  tests; nothing consumes the decision in this wave. The `consumer` field is
  the fixed placeholder string `unwired-shadow-evaluator (…)` so a future
  reader can never mistake a recorded decision for a gated one.
- **Determinism.** Canonical JSON (sorted keys, fixed escaping); identical
  inputs yield byte-identical records except the single measured field
  `elapsed_ms` (pinned by test). SHA-256 is an in-module FIPS 180-4
  implementation so graph identity binds without external dependencies.
- **Pinned dependency set.** `racket/base` + `racket/contract` + `json` only.
  Contracts are declared with column-0 named combinators (the W5
  check-deps lesson). No network, no threads, no sleeps.

### 2.1 Selection semantics (when *is* narrowing recommended?)

Only when **all** of the following hold — one failure anywhere collapses the
whole diff to `fallback:broad`:

- the diff is non-empty and not docs-only, not huge (≤40 non-docs changes),
  and touches exactly **one production area** (changed test files attach to
  the diff's production area rather than counting as a second area);
- every changed production source has a coverage entry from the
  @covers-derived mapping (or is itself a changed `test-*.rkt`, which selects
  itself);
- no file hit a broadening class (§3).

The recommended set is then the **union** of the covering test files (plus
changed test files themselves) — never smaller than the set of obligations
the diff demonstrably creates, and always accompanied by
`affected_claim_set` naming the W0 claims that would still be owed.

### 2.2 Missing-mapping discipline

The coverage mapping is *positive-only*: a source with no @covers-derived
entry is **unmapped**, and unmapped means broaden (`missing-mapping-metadata`).
The same applies to modules whose requires cannot be resolved statically: the
mapping generator refuses to emit entries for them (optionally recording an
`uncertain_sources` marker naming the reason: `dynamic-require`,
`macro-generated`, `generated-source`). Absence of evidence is treated as
evidence of risk — the selector never guesses.

## 3. Conservative fallback table (every §10 class broadens)

Expected behavior under uncertainty: **broaden/fallback, never a narrower
selection.** The table below is the complete class→decision mapping; the
`reason` values are the machine-readable names emitted in the record.

| # | §10 class / uncertainty | Trigger (first deterministic hit wins) | Decision | `reason` |
|---|---|---|---|---|
| 1 | changed test helper | file under `tests/` that is neither a `test-*` file nor a fixture (whoever requires it is statically unknowable) | `fallback:broad` | `helper-change` |
| 2 | changed fixture helper | fixture/golden/testdata file under `tests/` | `fallback:broad` | `fixture-change` |
| 3 | changed runner | `scripts/run-tests.rkt`, `scripts/run-tests/**` | `fallback:broad` | `runner-change` |
| 3b | runner-adjacent helper | other `scripts/**` tooling | `fallback:broad` | `runner-helper-change` |
| 4 | changed workflow | `.github/workflows/**` | `fallback:broad` | `workflow-change` |
| 5 | package/dependency metadata | `info.rkt`, `*.rktd`, `config/**`, `pkg/**`, `.github/**` non-workflow | `fallback:broad` | `config-or-dependency-metadata` |
| 6 | dynamic require | `uncertain_sources` marker `dynamic-require` (or unmapped ⇒ #9) | `fallback:broad` | `dynamic-require` |
| 7 | macro-generated dependency | marker `macro-generated` | `fallback:broad` | `macro-generated-dependency` |
| 8 | reader/generated source | `/generated/` or `/gen/` path, or marker `generated-source` | `fallback:broad` | `generated-source` |
| 9 | missing `@covers` / unknown mapping | mapped-area source with no coverage entry; any unmapped non-source file | `fallback:broad` | `missing-mapping-metadata` |
| 10 | malformed dependency graph | graph JSON unparseable / not an object | `fallback:broad` | `graph-parse-failure` |
| 11 | deleted source | change kind `deleted` | `fallback:broad` | `deleted-or-renamed-source` |
| 12 | renamed source | change kind `renamed` (both endpoints conservative) | `fallback:broad` | `deleted-or-renamed-source` |
| 13 | multi-area change | ≥2 distinct production areas in one diff | `fallback:broad` | `multi-area-change` |
| 14 | security-sensitive path | `security/**`, `sandbox/**`, credential/security lint scripts | `fallback:broad` | `security-sensitive-path` |
| 15 | platform-sensitive path | `gui/**`, `browser/**`, `interfaces/**`, `tui/**` | `fallback:broad` | `platform-sensitive-path` |
| 16 | unknown path | no area rule matches | `fallback:broad` | `unknown-path` |
| 17 | empty diff | zero changes | `broad` | `empty-diff` |
| 18 | huge diff | >40 non-docs changes | `fallback:broad` | `huge-diff` |
| 18b | timeout / resource exceedance | elapsed > budget (or > hard timeout) at any stage checkpoint | `fallback:broad` | `budget-exceeded` (`budget_ok=false`) |
| — | malformed diff manifest | schema mismatch / malformed entries | `fallback:broad` | `invalid-diff-manifest` |
| — | claims inventory unavailable | `claims.json` sibling missing/unparseable (§11.3 claim set unknowable) | `fallback:broad` | `missing-claims-metadata` |
| — | docs-only change | every change `.md/.markdown/.rst/.txt` | `broad` | `docs-only-change` |

Notes.

- **Never narrower.** There is no decision value below `broad` except the
  single `selected` case of §2.1, which additionally names the claims still
  owed. `broad` (empty diff, docs-only) *is* the broad tier — the gate is
  never skipped or shrunk.
- **Precedence is fixed and documented** so any replay is reproducible:
  deleted/renamed → docs → workflow → security-sensitive → config/metadata →
  runner → scripts → unknown area → fixture → helper → platform-sensitive →
  generated → uncertain markers → changed test file → mapped source.
- **Every broadening emits the full §11.3 record** (reason, source SHA,
  consumer, affected claim set, zero-tests statement, elapsed-ms). No silent
  fallback exists in the design; a fallback *is* the output.

## 4. Miss semantics

A *miss* is a changed file that should have been selected but was not. Under
this design misses can only arise in the `selected` case (a fallback is by
definition total coverage by the broad gate). Miss semantics, binding for the
W8 pilot if it is ever approved:

1. **Detection.** In shadow/pilot operation every `selected` decision is
   accompanied by a broad-gate execution of the same SHA. The miss set is the
   set of broad-gate test failures (or compilation failures) in tests the
   selector did **not** recommend. Shadow comparison against the broad gate
   is the only accepted miss oracle; test execution inside the evaluation
   phase remains forbidden.
2. **Accounting.** Misses are recorded per PR with: the missed test file, the
   source file whose change caused it, the reason the mapping did not connect
   them (`missing-mapping-metadata` is the canonical suspect), and the
   decision record digest. Miss counts are never averaged away: a cohort
   reports `misses/n` exactly, with zero-miss claims stated as zero only when
   the sample actually contains them (spec §11.2 discipline: unknown is never
   coerced to zero).
3. **Consequence.** Any observed miss triggers mapping repair (add the
   missing @covers entry / fix the stale one) before the selector can be
   considered for anything beyond shadow. **A selector with an unresolved
   miss on record stays out of gating permanently.** Miss rate is a
   governance input to the W8 verdict, not a tuning knob to be traded against
   savings after the fact.
4. **Fallback asymmetry.** Every fallback *adds* coverage (broad) rather than
   subtracting it, so fallbacks cannot create misses — they can only create
   cost, which is bounded by §5 and measured in the replay.

## 5. Resource budget

| Parameter | Value | Enforcement |
|---|---|---|
| selector/explanation p95 | **≤ 60 s** (`default-budget-ms = 60000`) | stage checkpoints; exceedance ⇒ `fallback:broad`, `budget_ok=false` |
| hard timeout | **≤ 120 s** (`hard-timeout-ms = 120000`) | `--budget-ms` is clamped to the hard timeout; exceedance ⇒ `fallback:broad` |
| test execution in the evaluation phase | **ZERO** | no executing code path exists (static source scan asserted in the unit suite); `tests_run = 0` in every record |
| exceedance semantics | `fallback:broad`, **never "select less"** | pinned by `case-budget` test: budget 0 ⇒ `fallback:broad` + `budget_ok=false` |

Measured reality (offline replay, 15 historical PRs): per-PR evaluation
**p50 = 1 ms, p95 = 2 ms, max = 2 ms**, zero budget violations. The budget is
therefore not a tuning target but a guardrail four orders of magnitude away
from the observed operating point; it exists so the W8 pilot can never
re-play the failure mode of the removed job (which died against a
30-minute *job* budget, not a 60-second *decision* budget — the removed job's
cost was test execution, which this design does not do at all).

## 6. Offline/replay evaluation methodology

The replay evaluates the design against **real historical PR diffs** without
executing anything:

1. **PR universe.** `git log --first-parent --format=%H%x09%s -40 origin/main`;
   keep entries whose subject carries a squash-merge PR marker `(#N)`
   (newest first); take the first 15.
2. **Changed files.** `git diff-tree --no-commit-id --name-status -r <sha>`
   per PR (diff vs first parent; squash commits are non-merge).
3. **Mapping companion.** `tests/.coverage-manifest.json` is read from each
   PR's **parent tree** (`git show <sha>^:tests/.coverage-manifest.json`) so
   the @covers state is historically faithful to the change; static
   `area_rules` complete the mapping. `uncertain_sources` is intentionally
   not populated in the replay (the conservative direction is preserved by
   coverage absence; the marker branch is unit-tested).
4. **Graph input.** The W0 graph + claims inventory, the same committed files
   for every PR (graph SHA-256 recorded in the artifact).
5. **Evaluation.** The evaluator CLI per PR, exactly as a future pilot would
   invoke it; decision records aggregated.

**Results (15 merged PRs, 177 changed files):**

| Metric | Value |
|---|---|
| decisions: `selected` / `broad` / `fallback:broad` | **1 / 0 / 14** |
| selection rate | **6.7%** |
| fallback rate | **93.3%** |
| p50 / p95 / max evaluator time | **1 ms / 2 ms / 2 ms** |
| budget violations | **0** |
| tests executed | **0** |

Interpretation, stated plainly: on real repository history the design
recommends narrowing for **1 in 15** PRs — a single-area `extensions` change
whose parent-tree coverage manifest maps it to exactly one test file — and
falls back broad on everything else, with named, per-file reasons. The one
`selected` case still names the ten W0 test-surface claims it would owe. This
is the intended operating point: a shadow selector that would have been
almost always-broad on history, at microsecond decision cost, with every
broadening explained. The full per-PR table (SHA, subject, files changed,
decision, reason, elapsed, budget flag, affected claim count) is bound in
`replay-results.json`.

## 7. Artifact retention policy

Retention is independent of broad test execution (the selector produces no
test evidence and therefore retains none):

- **Decision records** (`q.impact-decision/1`) are retained per evaluated SHA
  for **24 months** (mirrors the proof-bundle retention horizon), as plain
  canonical JSON plus the manifest and mapping inputs; digests recorded at
  write time (`graph_sha256`, source SHA).
- **Replay aggregates** (`replay-results.json` + `SHA256SUMS`) are **retained
  permanently** as wave evidence — they are the baseline against which any
  future pilot's fallback/miss/latency numbers are compared.
- **Rollback evidence.** The design is documentation + offline tooling only;
  per spec §11.6 no runtime rollback is required while nothing consumes the
  evaluator. If governance rejects the amendment, the records above remain as
  the audit trail of the rejection and the tool remains local-only.
- **Immutability.** Retained artifacts are checksum-bound at creation
  (`SHA256SUMS`) and are never rewritten; corrections land as new artifacts
  that reference the superseded digest.

## 8. Strategy amendment proposal (PROPOSED — not merged)

The following text is proposed for `docs/TDD-TEST-STRATEGY-PLAN.md`, to be
inserted as a new subsection after the existing *Impact selection —
implemented, fail-open, local opt-in only* section. **It is a proposal only;
the canonical strategy document is intentionally untouched by this wave.**

> ### Impact selection — proposed bounded shadow design (v1.00.29 W7 proposal; NOT YET ADOPTED)
>
> This subsection is proposed to **amend** the paragraph above in exactly one
> respect: the sentence "it must not be reintroduced into
> `.github/workflows/`" is superseded **only** for the new bounded,
> explanation-only evaluator (`q.impact-decision/1`,
> `scripts/impact-selector/evaluate.rkt`), and only under every one of the
> following conditions. The removed over-budget `test-impact` job remains
> dead under any other shape; this amendment does not rehabilitate it.
>
> 1. **Explanation-only, by construction.** The evaluator executes no tests
>    (no test-executing code path exists), emits a decision record with a
>    named reason, affected claim set, and zero-tests-run statement, and its
>    decisions are never required gates. The prior "static gates are the
>    authoritative PR protection" clause is unchanged.
> 2. **Bounded.** Selector/explanation p95 ≤ 60 s; hard timeout ≤ 120 s;
>    exceedance yields `fallback:broad`, never a smaller selection.
> 3. **Conservative fallbacks.** Unknown mapping, dynamic require,
>    macro-generated dependency, reader/generated source, missing `@covers`,
>    runner/helper/fixture/workflow/config changes, deleted/renamed source,
>    malformed graph, missing claims metadata, multi-area and huge diffs,
>    security- and platform-sensitive paths: all broaden.
> 4. **Non-required pilot first.** Any CI appearance is a non-required,
>    budgeted pilot comparing selector decisions against the unchanged broad
>    PR gates, with misses detected against the broad gate and retained per
>    PR. A miss with an unresolved mapping cause ends the pilot.
> 5. **Retained evidence.** Per-PR decision records (24-month retention) and
>    permanent replay/cohort aggregates with checksums.
> 6. **Explicit approval.** Replacing any broad required PR proof requires a
>    further decision on recorded pilot evidence; the shadow pilot itself
>    replaces nothing.
>
> This amendment is distinguishable from the removed job in cost model
> (microsecond static decision vs. minutes of test execution), in output
> (explanation record vs. pass/fail gate), and in authority (never required vs.
> was a required check that blew its budget). Until this amendment is reviewed
> and merged, W8 does not run.

## 9. W8 go/no-go gate (recorded state)

**W8 gate state: OPEN, PENDING AMENDMENT REVIEW.** The amendment proposal in
§8 is delivered by this wave but is not reviewed, not merged, and has no
effect. The decision rule, recorded here verbatim as the framing this wave is
required to deliver:

- **APPROVE** (amendment reviewed and merged): W8 runs the controlled,
  **non-required, budgeted** selector pilot — shadow decisions per PR,
  broad gates unchanged, misses/fallbacks/latency retained against §4–§7.
- **REJECT** (or not reviewed by the W8 decision point): W8 is
  **skipped-with-decision** — closed with this record as the decision of
  note — and **W9 proceeds with non-selector proof reuse only** (the W0
  duplicate-classification reuse candidates: exact-duplicate and
  positively-proven compatible-reusable mass). Impact selection remains
  local-only and fail-open, exactly as the canonical strategy states today.

Either way the acceptance gates of this wave hold: the design has no required
CI execution; fallback expands on every uncertainty class; the selector cannot
reduce required coverage in W7; historical/replay evidence is retained with
SHAs.

## 10. Threat-model coverage

All eighteen §10 classes plus the two named failure modes are exercised by
`tests/test-impact-selector-evaluate.rkt` (79 checks, green) against a small
inline fixture graph; every case asserts broaden/fallback, the zero-execution
record fields, the budget fields, and §11.3 completeness. The class → test →
expected-outcome map is maintained in `scripts/impact-selector/README.md`.

## 11. Limitations, stated honestly

- The replay's mapping companion derives from per-PR-parent coverage
  manifests; sources unmapped at that time (the majority of the tree — the
  manifest covers the reviewed pilot areas) fall back broad. The 6.7%
  selection rate is therefore an *upper-bound-flavored* observation for the
  current mapping, not a projection of savings.
- `uncertain_sources` markers are unit-tested but not yet produced by any
  generator; until one exists, static-analysis blindness to dynamic require /
  macros is covered by the missing-mapping fallback (#9), which is the same
  safety direction.
- One pre-existing, environment-sensitive failure (`tests/test-interfaces-tui.rkt`,
  "selection-text P1", TTY-sensitive; on the W3/W5 flake record) reproduces in
  this sandbox's fast run. It is unrelated to this wave's diff (new files
  only); recorded, not coerced.

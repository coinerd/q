# W5 Bounded Pure Production Simplification (v1.00.33)

Review amendment `7002476080` (milestone #900, wave W5, issue #9953) pre-approved
a two-item bounded candidate list with stop conditions. Baseline: `524c93a4` ·
Worktree: `wt-900-w5` · Branch: `feat/9953-w5-bounded-pure`.

## 1. Candidate ranking and the primary candidate's honest outcome

**Candidate 1 (primary): GSD retry eligibility / exhaustion decision — EVALUATED
FIRST AND REJECTED; its premise is false on this baseline.** Evidence:

- The infra budget/backoff/exhaustion seams were already extracted verbatim into
  `extensions/gsd/infra-retry-policy.rkt` (v1.00.22 W7, BUG-0042: budget consume,
  backoff, exhausted-message; BUG-0067 slow lane; BUG-0069 patience payload).
  The remaining inline pieces in `go-orchestrator.rkt` are single-site.
- Verification-repair eligibility is a single owned durable transition
  (`resolve-verification-rejection!`) beside its pure predicates
  (`repairable-verification-rejection?`, `verification-repair-no-progress?`,
  BUG-0060) — one production call site, already centralized.
- The `/go` routing decision is already ONE pure module (`campaign-decision.rkt`,
  W3): `decide-campaign-action` over typed views, no I/O.
- The timeout eligibility ladder (`go-orchestrator.rkt`, `(and (not owned-phases?)
  (eq? kind 'timed-out) (> retries-left 0))`) and the no-change eligibility ladder
  (`(and (not owned-phases?) (> no-change-retries-left 0)
  (delivery-verification-no-change? verifier-result))`) are inline but each has
  EXACTLY ONE production site. Six budget checks exist
  (`verification-repair.rkt` `(> retries-left 0)`, orchestrator timeout/no-change/
  infra/timeout-message, `delivery-journal.rkt` rejection budget), but every one
  is a semantically DIFFERENT decision with a single caller — there is no
  duplicated (2+-site) inline retry-eligibility decision to extract.
- The only literal copy-paste in the retry arm — the identical
  `parameterize`+`run-once*` re-entry block repeated in the infra fast and slow
  lanes — is an effectful recursive re-entry (sleep, cancellation polling,
  re-run). Extracting it into a "pure" function would move effects into it
  (stop condition 2), and extracting a generalized eligibility predicate over
  semantically different decisions would increase net complexity
  (stop condition 3).

This is the W3-requested per-item proof ("each still needs its own proof that it
is the same decision before extraction") — the proof is negative for retry
eligibility. Deferred to the normal backlog, not manufactured.

**Candidate 2 (secondary): testing-CLI result-interpretation decision — SELECTED.**
Gate honesty: (a) candidate 1 does not yield — verified above; (b) W4b's report
(`W4B-CLI-CONTRACTS-v1.00.33.md`) does not name this exact ladder verbatim; what
it establishes is the disposition context: every deterministic argv/selection
decision was moved to the pure `resolve-qt-ci-plan` owner, all remaining qt
launches pin production contracts, and §5 states further reduction would delete
authentic entrypoint coverage — i.e. runner RESULT INTERPRETATION remained the
one production-owned, integration-only-tested decision area of `scripts/testing/`.
Direct baseline inspection of that area (not just tests) found the still-
duplicated production decision. This deviation from a verbatim W4b quote is
flagged as the wave's premise risk in §7.

## 2. The decision extracted

`classify-quick-child-run` — ONE pure trust classification of a finished audited
child run into `(values reported-exit reason)`, in
`scripts/testing/quick-runner.rkt` (the module that already owned
`quick-result` and `quick-result-valid?`; no new module):

- inputs: the evidence-plane fact (`'matched` / `'mismatched` / `'absent`)
  plus the child-plane facts already observed at the boundary
  (`timed-out?`, `truncated?`, the child's exit code);
- outputs: the reason (`'identity` / `'timeout` / `'truncated` / `'exit` /
  `'no-evidence` / `#f`) and the reported exit code (only `'exit` carries the
  child's own code; every other reason reports 1);
- precedence lives exactly once: identity mismatch (the trust root) > timeout >
  truncated > nonzero exit > the evidence tail;
- purity: no I/O, no clock, no parameters, no mutable state, no authority; the
  subprocess observation stays at the effect boundary (`run-one` reads the
  subprocess result and evidence file first, then classifies the captured
  facts once — no TOCTOU window);
- behavior-preserving: both former branches map to the same results for every
  fact combination (the two branches had previously re-derived this by
  copy-paste, including two subtly different exit-code mappings and the
  `child-exited-nonzero?` helper, now removed — its check is the classifier's
  exit arm).

## 3. Production call site and removed duplication

`run-one` (`scripts/testing/quick-runner.rkt`) — the trust boundary every audited
result passes through, reached from two production entry paths:

- `run-quick` → scheduler tasks → `run-one` (`./qt quick`, the 28-owner audited
  lane, CI-surface tested);
- `run-quick-file` → `full-plan.rkt` `run-files`/`run-plan` → `run-one`
  (`./qt full`, `./qt default`, every lane plan).

Before: the reason precedence ladder was written TWICE inline (evidence-well-
formed branch lines 116–119, evidence-missing branch lines 133–137), plus two
exit-code mapping expressions (`(if (and reason (not (eq? reason 'exit))) 1
exit)` vs `(if (child-exited-nonzero? raw) exit 1)`) that must agree by
copy-paste discipline. After: one classifier, two thin call sites. Net
production diff: +33/−30 lines in one module (the classifier is 10 lines);
copies of the precedence decision: 2 → 1; copies of the exit-code mapping:
2 → 1; `child-exited-nonzero?`: deleted. No behavior change (table-equivalence
pinned in §4).

## 4. Coverage and admission mapping

- New focused owner: `tests/test-qt-quick-run-classification.rkt` (`#lang
  racket`, rackunit `test-case` inside `module+ test`, `@suite testing`,
  `@boundary unit`, `@speed fast`, `@covers scripts/testing/quick-runner.rkt`):
  7 test cases / 21 `check-equal?` rows directly over the pure decision —
  every evidence × child-fact combination including both shapes under timeout/
  truncated + nonzero exit, the identity head winning over child-plane facts,
  fail-closed `no-evidence`, the only-'exit exit-code contract, and a
  determinism case. No filesystem/network/process/real-clock/mutable-global
  access in the decision or the tests.
- `tests/.coverage-manifest.json` regenerated in-commit: 223 → 224 entries.
- Existing owners keep their tiers: `tests/test-qt-output-limits.rkt`
  (integration, real children, 4 MB caps + forced timeout) still pins the
  truncated/timeout arms end-to-end through `run-quick`; `tests/test-qt-quick.rkt`
  still pins the whole-suite CLI contract. No retiering.

## 5. Red/green defect evidence

Two defects injected at the pure decision, executed, then reverted:

| Probe | Defect | Pure owner | Integration owner (`test-qt-output-limits`) | Production CLI |
|---|---|---|---|---|
| Baseline (pre-extraction) | — | — | PASS (8.658 s) | — |
| GREEN after extraction | — | PASS 6 cases (1.323 s) | PASS | `./qt quick tests/test-ansi.rkt` exit 0 |
| RED A: precedence drift (`truncated` demoted below the exit check) | classifier only | **FAIL** (4/6 cases pass, 2 fail) | PASS — **blind**: its truncated fixture child exits 0, and no audited owner exercises truncated+nonzero exit | unchanged output |
| RED B: clean matched arm misclassified as `'no-evidence` | classifier only | **FAIL** (3/6 cases fail) | (not re-run; arm not exercised) | **`./qt quick tests/test-ansi.rkt` exit 1**: `qt: quick failed or produced missing/invalid results` |
| GREEN after revert | — | PASS (1.035 s) | PASS | exit 0, `quick passed: 1 files, 23 tests` |

Probe A is the honest measured-gain datum: the duplicated-ladder defect class
(precedence drift between the two former copies) was NOT detectable by the
existing effectful integration lane; the direct table owner catches it in ~1 s.
Probe B is the production-call proof through the real `./qt` entrypoint, same
pattern as W3's coordinator-owner red. The final committed diff was re-verified
line-by-line after the probes; every green above was re-run on the reverted
tree.

## 6. Measured feedback/maintainability impact

| What | Before | After | Measured |
|---|---|---|---|
| Focused feedback for a defect at the result-interpretation decision | none — nearest owner was the 8.7 s integration file (subprocess fixtures: 4 MB streams + a forced timeout), and it covers 2 of 5 reason arms | `racket scripts/run-tests.rkt tests/test-qt-quick-run-classification.rkt` | 0.85–1.3 s (3 runs); all 5 arms + both exit mappings pinned |
| Defect classes detectable directly | precedence-drift between the two ladder copies: undetectable except by accident | any precedence/arm/mapping drift | RED A row above |
| Copies of the precedence decision in production | 2 (+2 exit mappings, +1 helper) | 1 | one module diff |

## 7. Honest deviations and residual risk

- Premise verification: candidate 2's "W4b reveals" gate is satisfied by W4b's
  disposition context plus direct baseline inspection, not by a verbatim W4b
  finding; the reviewer may reject on that reading — the candidate-1 negative
  proof stands either way.
- Probe A's production blindness is a property of the retained integration
  fixtures, not of this change; widening them was out of scope (their cost is
  the point of the direct owner). The truncated+nonzero-exit combination is
  now pinned directly.
- The focused owner is NOT added to the curated `qt quick` audited set (that
  table lives in `scripts/testing/quick-audit.rkt`, outside this wave's allowed
  file scope — same W3 precedent). Consequently `./qt quick --changed HEAD`
  escalates fail-open to the whole 28-owner lane (`quick-runner.rkt: no @covers
  mapping reaches an audited owner`); conservative and correct.
- The wave deliberately does not touch receipt staleness, evidence
  admissibility, metadata-correction authorization/replay, agent/runtime state
  machines, `.github/`, PR-check policy, `.planning/`, or W1–W4 files.
- No `(sleep ...)` fixtures added (no class-F wait-audit row needed); no
  reviewer-session builders touched (provider-builder trap not applicable).

## 8. Gates (all executed in this worktree)

| Gate | Result |
|---|---|
| `racket scripts/run-tests.rkt tests/test-qt-quick-run-classification.rkt` | PASS (1.0–1.3 s) |
| `racket scripts/run-tests.rkt tests/test-qt-output-limits.rkt` | PASS (8.7 s), baseline + post-revert |
| Red/green probes (§5) | pure owner RED under both defects; production CLI RED under probe B; all GREEN after revert |
| `./qt quick` | PASS 28 files / 325 tests (37.3 s) |
| `./qt quick --changed HEAD` | PASS 28 files (fail-open escalation, §7) |
| `./qt default --explain` | exit 0 (required-offline plan resolved, 1382 files) |
| `racket scripts/lint-all.rkt` | post-sync: metrics gates green; arch + release-readiness warnings non-blocking, pre-existing |
| `racket scripts/lint-format.rkt` | PASSED (0 errors / 2655 files) |
| `racket scripts/lint-tests.rkt` | 0 errors (386 warnings, 110 infos — pre-existing) |
| `racket scripts/lint-docs.rkt` | PASSED, 0 warnings |
| `racket scripts/lint-contract-changes.rkt --diff HEAD~1..HEAD` | see commit-time run |
| `racket scripts/check-imports.rkt` | no conflicts |
| `racket scripts/metrics.rkt --lint` | all 5 static metrics match README (post-sync) |
| `racket scripts/tier-ownership-matrix.rkt check` | green (1173 rows; 1172 → 1173) |
| `sha256sum -c SHA256SUMS` (artifact dir) | ownership-matrix.json: OK |
| `git diff --exit-code -- scripts/required-pr-checks.policy .github .planning` | empty |

Artifacts regenerated in-commit after `git add -A` and before
`metrics.rkt --sync-all`: `tests/.coverage-manifest.json` (224 entries),
`tests/tier-ownership-matrix.json` (1173 rows), `reports/test-ownership-map.{md,
json}` (regenerated; byte-identical — the new owner is a `testing`-suite unit
and shifts no production-area aggregate), `artifacts/tier-ownership/
v1.00.32-w0/ownership-matrix.json` + fresh `SHA256SUMS` (`193f6d4c…`), README
metrics (test files 1682, source lines 239413, test lines 366653, assertions
52726).

## 9. Attribution

Candidate list and stop conditions: review amendment `7002476080` (issue
#9953). Pilot pattern (one pure decision, direct table owner, red through the
production caller): W3 `7002474966`. Scripts/testing disposition context: W4b
`7002475553`. Negative retry-eligibility proof recorded for the milestone
backlog; W3's `delivery-stop-outcome` decision untouched.

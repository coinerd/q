# W3 Pure-Production Pilot (v1.00.33)

Review 7002474966 (milestone #900, wave W3, issue #9951) approved a ranked
backlog and asked for exactly ONE narrow pure-production pilot: delivery
stage transitions and review outcome classification. This report is the
pilot's evidence. Baseline: `d20ce530` · Worktree: `wt-900-w3` · Branch:
`feat/9951-w3-pure-pilot`.

## 1. The decision extracted

`delivery-stop-outcome` — ONE pure classification of a typed controller
effect result into the operator-facing `delivery-outcome` value, in
`extensions/gsd/delivery-coordinator.rkt` (the module that already owned
`delivery-outcome` and `review-rejected-message`; no new module, no new
framework):

- inputs: the typed kind symbol (`awaiting-review`, `review-rejected`,
  `review-infra`, `retryable`, `blocked`, `ok`) plus its immutable payload
  hash;
- outputs: a fresh `delivery-outcome` value for every typed stop — the
  review outcome classification (`awaiting-review` /
  `review-rejected` with the typed payload kept /
  `review-infra`) plus `retryable` and `blocked` — or `#f` for `ok`;
- `#f` for `ok` is the boundary contract: advancing the delivery journal one
  stage is the caller's authenticated effect (durable fence recheck against
  the fence captured at step start + `durable-blocker` re-run before the
  write), and that effect is deliberately NOT pure and NOT moved;
- purity: no I/O, no parameters, no clock, no journal access; deterministic
  over its two immutable inputs (pinned by a dedicated test case);
- `#:reason-fallback` / `#:blocked-message` carry the refused-preflight
  path's pre-existing specific fallbacks, so all three call sites keep
  byte-identical operator text.

## 2. Production call sites and removed duplication

All inside `run-delivery-coordinator!` (`extensions/gsd/delivery-coordinator.rkt`):

| Site | Before | After |
|---|---|---|
| First ladder action after a cleared preflight (`context-ready → implementation-review`) | verbatim 20-line inline `case` mapping the controller result | `(delivery-stop-outcome ...)` + the `cond` that keeps the authenticated advance |
| Every ordinary ladder action (`implementation-review` … `delivered`) | the SAME verbatim 20-line inline `case` (copy-pasted duplicate) | identical shape, same classifier |
| Refused preflight (typed stop of the preflight action) | a third near-copy `case` with preflight-specific reason fallbacks | one call with `#:reason-fallback` / `#:blocked-message` |

Net: three inline copies of the review-outcome/stop mapping → ONE owned pure
decision + the effect boundary. 85 insertions / 63 deletions total, one
module touched. No behavior change:

- `ok` still advances the journal only after `still-current?` (captured
  fence, no cancellation) and `durable-blocker` re-run — the TOCTOU/stale
  window is unchanged, still closed at the effect boundary;
- `delivery-effect-result?` still rejects malformed controller results
  before classification;
- the preflight path's specific defaults (`"delivery preflight refused"`,
  `"delivery preflight: <reason>"`) are preserved byte-for-byte via the
  keyword parameters;
- retry eligibility, receipt identity/staleness, evidence admissibility and
  metadata-correction authorization/replay were NOT touched (they are
  later backlog items, explicitly out of pilot scope).

## 3. Coverage and admission mapping

- New focused owner: `tests/test-gsd-delivery-stop-decision.rkt`
  (`#lang racket/base`, rackunit `test-case`, `@suite unit-fast`,
  `@boundary unit`, `@speed fast`), directly table-driven over the pure
  decision: every typed kind → exact outcome shape, default fallbacks, the
  `review-rejected` payload identity (`eq?` on the input hash — the message
  is never parsed back), the preflight parameterization, `ok` → `#f`, and a
  determinism case. No filesystem/network/process/real-clock/mutable-global
  access in the decision or the tests.
- `@covers extensions/gsd/delivery-coordinator.rkt` — the module the tests
  import and production calls; `tests/.coverage-manifest.json` regenerated
  in-commit.
- Deep owners stay in their current tiers: `test-gsd-delivery-coordinator.rkt`
  (integration, `@suite extensions`) keeps every durable journal/fence/
  receipt contract, including the `awaiting-review` and `retryable` typed
  stops through the production path. No retiering, no W1-scope retier.

## 4. Red/green defect evidence

Defect injected at the pure decision (`awaiting-review` deliberately
misclassified as a `retryable` outcome), executed, then reverted:

| Run | Result |
|---|---|
| Baseline: `racket scripts/run-tests.rkt tests/test-gsd-delivery-stop-decision.rkt` | PASS 6/6 (2.3 s) |
| Baseline: `… tests/test-gsd-delivery-coordinator.rkt` | PASS 34/34 (33.0 s) |
| RED (defect in): new owner + coordinator owner | FAIL — new owner 5/6 (`awaiting-review` row), coordinator owner 33/34 (`awaiting-review stops immediately and never advances the journal`) |
| GREEN (defect reverted): new owner | PASS 6/6 (2.1 s); `git diff` back to exactly the pilot change |

The coordinator owner going red under the pure-decision defect is the
proof that production imports and calls `delivery-stop-outcome` — the
failure surfaces through `run-delivery-coordinator!`'s real awaiting-review
path, not only through the direct unit rows.

## 5. Measured feedback impact

| What | Before | After | Measured |
|---|---|---|---|
| Focused feedback for a defect at the stop classification | none — nearest owner was the 33 s integration file | `racket scripts/run-tests.rkt tests/test-gsd-delivery-stop-decision.rkt` | 2.1–2.3 s (3 runs) |
| Regression blast radius for the coordinator module | unchanged | unchanged | `test-gsd-delivery-coordinator` still 33 s, still integration tier |

The new owner is a light `racket/base` closure (no `typed/racket` in its
require graph from this file), so it does not add a heavy focused lane; the
W2-documented GSD focused-target miss (typed closure recompiles) does not
apply to it.

Honest scope note: the pilot owner is NOT added to the curated `qt quick`
audited set — that owner table lives in `scripts/testing/` (outside this
wave's allowed file scope). Consequences, both observed: `./qt quick
<file>` refuses it (`not admitted to quick`), and `./qt quick --changed
HEAD` escalates to the whole 28-owner quick lane (fail open, `unmapped-source:
extensions/gsd/delivery-coordinator.rkt: no @covers mapping reaches this
changed module` — the covers manifest maps the module to tests generally,
but the quick-lane selection only trusts owners inside the audited set).
The escalation is conservative and correct; a follow-up that adds the owner
to the audited quick set (scripts/testing owner) would narrow the
`--changed` lane back to the focused owner plus riders.

## 6. Gates (all executed in this worktree)

| Gate | Result |
|---|---|
| `racket scripts/run-tests.rkt tests/test-gsd-delivery-stop-decision.rkt` | PASS 6/6 (2.15–2.28 s) |
| `racket scripts/run-tests.rkt tests/test-gsd-delivery-coordinator.rkt` | PASS 34/34 (33 s) |
| `racket scripts/run-tests.rkt tests/test-gsd-review-dispatch.rkt tests/test-gsd-delivery-controller-residuals.rkt tests/test-gsd-delivery-ladder-wiring.rkt` | PASS 57/57 (60 s) |
| Red/green probe (§4) | new owner + production owner RED under the defect; GREEN after revert |
| `./qt quick` | PASS 28 files / 325 tests |
| `./qt quick --changed HEAD` | PASS 28 files (fail-open escalations, §5) |
| `./qt default --explain` | exit 0 (required-offline plan resolved) |
| `racket scripts/lint-all.rkt` | 23 passed, 2 warnings (arch, release-readiness — non-blocking, pre-existing) |
| `racket scripts/lint-format.rkt` | 0 errors / 2653 files |
| `racket scripts/lint-tests.rkt` | 0 errors (386 warnings, 110 infos — pre-existing) |
| `racket scripts/lint-docs.rkt` | PASSED, 0 warnings |
| `racket scripts/lint-contract-changes.rkt --diff HEAD` | PASS (no concerning contract changes) |
| `racket scripts/check-imports.rkt` | no conflicts |
| `racket scripts/metrics.rkt --lint` | all 5 static metrics match README |
| `sha256sum -c SHA256SUMS` (artifact dir) | ownership-matrix.json: OK |
| `racket scripts/tier-ownership-matrix.rkt check` | green (1172 rows) |
| `git diff --exit-code -- scripts/required-pr-checks.policy .github .planning` | empty |

Artifacts regenerated in-commit after the new test file (staged with `git
add -A` before `metrics.rkt --sync-all`): `tests/.coverage-manifest.json`
(223 entries), `tests/tier-ownership-matrix.json` (1172 rows),
`reports/test-ownership-map.{md,json}`, `artifacts/tier-ownership/
v1.00.32-w0/ownership-matrix.json` + fresh `SHA256SUMS`, README metrics
(1680 test files). The `tests/.quick-admission.rktd` refresh reproduced the
committed bytes byte-identically (the audited quick set is unchanged by
this wave — §5).

## 7. Honest residuals

- The classifier's `case` has no clause for hypothetical unknown kinds
  (they fall into the shared `blocked` arm); production cannot produce
  them (`delivery-effect-result?` guards), and the pre-pilot code had the
  same catch-all shape.
- The pilot does not touch the later backlog items (retry eligibility,
  receipt identity/staleness, evidence admissibility, metadata-correction
  authorization/replay); each still needs its own proof that it is the same
  decision before extraction.
- The W1 residual risks (exact-SHA full-regression evidence outstanding)
  are untouched by this wave; no security-sensitive retiering happened.

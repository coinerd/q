# Evidence integrity (digest identity, head binding, record purity) — v1.00.31 W2

Status: implemented; wave gates pending
Register: F2, F3, F4 (+ F12 sentinel close-out, owner W2)
Owner: v1.00.31 W2 (`campaign/v1.00.31-w2`, issue #9725)

## 1. What F2/F3/F4 actually were

The v1.00.30 W4 evidence branch (`campaign/v1.00.30-w4`, head `ce13d038`, tree
`fc80fa2e`) carried a complete-looking schema-2 trio that could not survive
contact with tooling (`.planning/campaigns/96974…/BLOCKED-w4.md`):

1. **F2 — authored, unreproducible content digest.** The evidence recorded
   `content-digest 542f7cbb…`, but the true excluded-evidence digest at the tip
   was `a7ad71fb…`. Nothing ever recomputed it: `gsd-wave-gate` only compared
   the digest argument it was handed to the recorded value. A wave could claim
   verification for a digest no tool ever produced.
2. **F3 — evidence/review head ≠ verified head.** `implementation-sha` was
   `bbb6500a`, but the receipt-verified head and tree were `ce13d038` /
   `fc80fa2e`; the review's `reviewed-sha` matched the same wrong head. No guard
   bound the identity fields to the durable receipt head.
3. **F4 — the "evidence" commit was not evidence-only.** `ce13d038` also changed
   `README.md` and `tests/test-w9-ci-workflow-verification.rkt`. The purity
   guard (`require_receipt_tip`) only refused *post*-receipt source drift, so a
   commit that mixed records with source edits from the start was invisible.
4. **F12 (close-out owned here) — sentinel evidence passes.** The gate validated
   identity and narrative fields with `non-empty-string?` alone, so the literal
   sentinel `"PENDING"` satisfied `review.reviewer`, `review.scope`,
   `review.report`, and `red-first.command`/`failure`. A staged binding draft
   (observed live in this campaign's own W0 `prepare` output) could be
   "finalized" by flipping only status fields, publishing a record that asserts
   an independent review which never occurred.

## 2. What was changed

| Change | Where |
| --- | --- |
| New tooling that computes the excluded-evidence content digest with the same base/head/exclusion contract as `gsd-delivery.py digest()` (long-form `:(exclude)` magic pathspecs), and four subcommands — `digest`, `verify` (typed `digest-ok` / `digest-mismatch` / `malformed-digest`, fail-closed), `bind` (the only sanctioned digest-authoring path), `record-commit` (typed `pure` / `impure-record-commit: <paths>`) | `scripts/gsd-evidence-bind.rkt` (new) |
| Receipt-head cross-check: with `--receipt-head <sha>` the gate refuses `head-binding-mismatch` naming both SHAs unless evidence `implementation-sha` *and* review `reviewed-sha` equal the durable receipt head; conditional for historical trios that predate the field, so delivered records stay readable (no retro-invalidation) | `scripts/gsd-wave-gate.rkt` |
| Sentinel/placeholder refusal (`placeholder-evidence`) in every identity/narrative field plus substantive minimum content (`insufficient-review-content`): review `scope`/`report` ≥ 64 chars, `red-first.failure` ≥ 32 chars; the CLI `main` now *returns* usage errors instead of exiting in-process | `scripts/gsd-wave-gate.rkt` |
| `durable-receipt-head` accessor (verified receipt head, `#f` when absent) | `extensions/gsd/delivery-receipt.rkt` |
| `prepare()` refuses mixed record commits via the `record-commit` verdict (F4), and finalized `read_staged_trio` asserts a genuine reviewer identity against the sentinel deny-list (F12) | `scripts/gsd-delivery.py` |
| Red-first fixtures proving each refusal is enforced by tooling: hand-authored digest refused then bound+verified; wrong receipt head refused and matching head silent; mixed commit refused with the foreign path named; sentinel draft refused while the same draft with real content passes | `tests/test-gsd-evidence-bind.rkt`, `tests/test-gsd-wave-gate-digest.rkt`, `tests/test-gsd-wave-gate.rkt`, `tests/test-gsd-delivery-receipt.rkt`, `tests/helpers/w2-mini-git-repo.rkt` (new) |
| Failure-register guards F2/F3/F4 registered falsifiably: each guard asserts both the refusal *and* the control, so deleting the check flips the guard to `not-refused` | `tests/test-wave-delivery-integrity-register.rkt` |

## 3. Red-first evidence

Captured from the real CLIs against synthetic repos (`raw/` in this artifact
directory; SHA256SUMS below):

- `raw/f2-digest-mismatch-refusal.txt` — `verify` refuses `digest-mismatch`
  naming recorded vs computed digest; `bind` then re-verifies `digest-ok`.
- `raw/f3-head-binding-mismatch-refusal.txt` — gate refuses with two
  `head-binding-mismatch` reasons (evidence + review), passes with the
  matching receipt head.
- `raw/f4-impure-record-commit-refusal.txt` — `record-commit` refuses
  `impure-record-commit: README.md (commit …)`; an evidence-only control scans
  `pure`.
- `raw/f12-sentinel-refusal.txt` — otherwise-complete record with
  `reviewer "PENDING"` is refused `placeholder-evidence`; the identical record
  with a genuine reviewer passes the strict gate.

## 4. Backward compatibility

The receipt-head cross-check is conditional for historical trios (records that
predate the durable receipt-head field are not retro-invalidated), and the
exclusion contract is byte-identical to Python's
`git diff --binary base...head -- . ':(exclude)docs/reports/gsd-wave-.{evidence,reviews,validation}/**'`
(long-form magic pathspec). The W1 publication range
`43f89413ac…→69647f1ce5…` reproduces the empty-tree digest
`e3b0c442…b855` through the new Racket tool
(`tests/test-gsd-evidence-bind.rkt`), matching W1's recorded binding.

## 5. Consequential seam updates

- `tests/test-gsd-governance-workflow.rkt`: its shared pass-fixture carried
  short-but-genuine narratives (`scope "W0 F-10 F-13"`, a one-line report, and a
  25-character `red-first.failure`). The contract-mandated minimum-content
  floors require substantive content, so the fixture narratives were lengthened
  (refusal-direction tests are unaffected: they fail for their own typed
  reasons).
- `README.md`: metrics table re-synced via `scripts/metrics.rkt --sync-all`
  (source/test line and assertion counts moved with the new files).

## 6. Verification

- Focused: `test-gsd-wave-gate.rkt`, `test-gsd-wave-gate-digest.rkt`,
  `test-gsd-evidence-bind.rkt`, `test-wave-delivery-integrity-register.rkt`,
  `test-gsd-delivery-receipt.rkt` — PASS (42 tests).
- Review round 1 non-blocking findings and their disposition: the `--check`
  mode named by the wave contract is delivered as the `check` subcommand (a
  fail-closed alias of `verify`, tested in both directions); refusal verdicts
  deliberately exit 0 with typed codes on stdout (consumers parse stdout;
  nonzero exit is reserved for tool failure), noted here as an F8-adjacent
  ergonomics consideration; the binding-time `review()` reconciliation
  (`gsd-delivery.py`) accepts a `reviewed-sha` that is an evidence-only
  ancestor of the receipt head — required so the binding ladder can bind a
  pre-merge reviewed head to the post-merge receipt, and fail-closed via
  `require_receipt_tip` on any non-evidence drift; a duplicated require block
  in the CLI test was removed.
- Python controller lane: `tests/test-gsd-delivery-controller.py` — 123 tests OK
  (incl. `test_prepare_refuses_mixed_record_commit`,
  `test_binding_review_refuses_sentinel_reviewer_identity`).
- Gate consumers: `test-gsd-governance-workflow.rkt`,
  `test-gsd-delivery-runtime.rkt`, `test-gsd-delivery-coordinator.rkt`,
  `test-gsd-delivery-journal.rkt`, `test-gsd-branch-delivery-verification.rkt`,
  verifier decision/e2e — PASS.
- `scripts/check-deps.rkt` OK; `scripts/metrics.rkt --lint` OK;
  arch suite 32/32 PASS; fast suite RUN-SUMMARY retained in this directory's
  evidence (see `fast-run-summary.txt`).

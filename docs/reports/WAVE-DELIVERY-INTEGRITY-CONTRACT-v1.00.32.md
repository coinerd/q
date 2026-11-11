# Wave-delivery integrity contract v1.00.31

Frozen failure-mode register for milestone `v1.00.31 - GSD Wave Delivery Integrity`
(milestone #896). Materialised **verbatim** from the failure-mode register of
`.planning/PLAN-v1.00.31-GSD-WAVE-DELIVERY-INTEGRITY.md` by
`artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json`.

The register is frozen: a later re-scoping needs a reviewed plan amendment, never a
silent edit. Every row must end with a fixture that fails pre-fix and passes
post-fix; no row may be closed by assertion alone (acceptance gate A0).

Provenance and trust boundary: the plan file lives outside this repository
(`.planning/` is disk-resident, not tracked), so the in-repo verbatim copy of its
register table is
`artifacts/wave-delivery-integrity/v1.00.31-w0/raw/plan-failure-mode-register.txt`,
taken directly from the frozen plan at W0 freeze time. Two different digests are
recorded for two different things: `plan-sha256` in `failure-register.json` is the
SHA-256 of the frozen **plan file itself** (which is repo-external, so only the
coordinator can re-verify it against the plan), whereas the SHA-256 of the in-tree
excerpt is the one recorded for `raw/plan-failure-mode-register.txt` in the
artifact `SHA256SUMS`. Re-verifying the plan digest is a coordinator-side check;
in-tree, the harness verifies that this document, the machine-readable register
and the raw excerpt agree row-for-row on all four contract columns, in both
directions (no dropped row, no invented row).

Row count: **13**.

| Mode | Observed in v1.00.30 W4 | Permanent guard | Owning wave | Refusal contract |
|---|---|---|---|---|
| **F1** Declared workflow/action invocation ≠ actual script CLI | `.github/actions/prepare-racket-environment/action.yml:146-149` calls `scripts/ci/compiled-root.rkt build --out … --checkout … --trusted-label …`; the CLI has no `--out` and requires `--module`/`--final-dir`. Reproduced: `compiled-root: unknown switch: --out`, exit 1, under `set -euo pipefail`. The compiled-root lane could never activate in CI. | Every workflow/action → script invocation is **executed or exactly matched** by a test; substring assertions are banned for invocations | W1 | Red test; unknown-switch / missing-argument is a wave failure |
| **F2** Authored, unreproducible content digest | Evidence `content-digest` `542f7cbb…` is unreproducible; the true digest at the tip is `a7ad71fb…`. | Digest is computed by tooling at the exact head and written by that tool; the gate/verifier recomputes and refuses a mismatch | W2 | Typed refusal `digest-mismatch` |
| **F3** Evidence/review head ≠ verified head | Evidence `implementation-sha` is `bbb6500a`, but the receipt-verified head and tree are `ce13d038` / `fc80fa2e`. Review `reviewed-sha` matches the wrong head. | `implementation-sha` and `reviewed-sha` must equal the durable receipt head | W2 | Typed refusal `head-binding-mismatch` |
| **F4** Record commit touches non-evidence paths | The "evidence" commit `ce13d038` also changed `README.md` and `tests/test-w9-ci-workflow-verification.rkt`. | The final record commit must be evidence-only; a mixed commit is refused with a precise file list | W2 | Typed refusal `impure-record-commit` |
| **F5** Wave recorded verified / ladder attempted with an unpushed branch | The branch was never pushed; `/go` died with `wave-blocked (delivery command failed (git, exit 128))`. | Push precondition before any receipt is recorded and before any ladder step; git failures classified | W3 | Typed refusal `branch-not-published` / `remote-ref-missing` |
| **F6** Delivery-tool GitHub API route wrong (resolver head filter) | `scripts/gsd-delivery.py:437` resolves PRs with `head={owner}/{repo}:{branch}` instead of `head={owner}:{branch}`, so `resolve_existing_pr`/`resolve_merged_pr` always return none and the `governance` ladder action can never succeed. | Route construction covered by API-contract fixtures that mirror the **real** API, plus one coordinator-owned live-contract check | W4 | Typed refusal; `governance` proven end-to-end |
| **F7** Recorded provenance head stale / artifacts internally inconsistent | `artifacts/ci-recovery/v1.00.30-w4/*` and the report are pinned to stale head `8299409c`; `rollback-drill.json` is internally inconsistent (phase prose 260/245 ms vs `eager-fallback-ms` 247/256 ms). | Determinism + consistency lint: recorded head/tree must match the commit; cross-artifact timings must agree | W5 | Typed refusal `provenance-drift` |
| **F8** Opaque failure surfacing (bare exit code) | - | Every delivery/ladder subprocess failure maps to a typed reason with the command, exit code and class | W3 | Typed `wave-blocked` reason with diagnosis |
| **F9** Wave marked DONE while delivery is pending (observed: v1.00.30 W4 was `[DONE]`/`Status: DONE` while `delivery-w4.rktd` is `delivery-pending`, breaching the frozen contract "never mark this wave complete while delivery is pending"; live instance corrected 2026-09-20 — campaign record W4 → `pending`, plan index → `[Inbox]`, doc header → `Status: Inbox`, `delivery-w4.rktd` unchanged) | - | The completion path refuses `done` unless the delivery journal for that wave reads `delivered` (or an explicit typed carry-forward is recorded); the plan-index bracket and the wave-doc header stay in lockstep with the journal | W3 | Typed refusal `delivery-pending-cannot-complete` |
| **F10** A status rollback (a wave going `done` → non-done) leaves its completion event in the derived outbox; `reconcile-completion-outbox!` appends only for `done` waves and never prunes, so the ledger leads the durable record — the inverse of its stated invariant ("may only lag the durable commit, never lead (no invented DONE)") | - | Rollback/un-completion prunes the affected waves' derived outbox events, or reconcile becomes two-way and drops events whose wave is no longer `done`; the invariant is asserted by a test that rolls a wave back | W3 | Typed refusal `outbox-leads-record` |
| **F11** Unsatisfiable merge gate (second-account GitHub approval demanded in a single-account repository) | Amended approval contract: a recorded operator authorization in the evidence record's `merge-authorization` object (operator, wave, verified implementation head, action, source) plus the APPROVED independent non-author schema-2 review artifact bound to the verified implementation head; both refusal directions enforced by tested code and the unsatisfiable second-account requirement removed | W4 | Typed refusals `no-operator-authorization` / `no-review-artifact` / `head-binding-mismatch` |
| **F12** Gate accepts sentinel placeholders as evidence | **Gate accepts sentinel placeholders as evidence.** `scripts/gsd-wave-gate.rkt` validates identity and narrative fields with `non-empty-string?` alone, so the literal sentinel `"PENDING"` satisfies `review.reviewer`, `review.timestamp`, `review.scope`, `review.report`, `remaining-items[].owner`/`rationale` and `red-first.command`/`failure`. A staged draft can therefore be "finalized" by flipping only `status`/`verdict`/`content-digest`/`planning-sync` and the four `result` fields and still pass the strict gate, publishing a record that asserts an independent review which never happened. Observed live in this wave's own W0 binding draft, whose `prepare` output carries exactly those sentinels; `gsd-delivery.py`'s finalized `read_staged_trio` never inspects the reviewer at all | Sentinel/placeholder values are refused structurally: a deny-list rejects `PENDING`/TODO-style placeholders in every identity and narrative field, with substantive minimum-content checks on the review `scope`/`report` and on `red-first.failure`, so "finalized" is unreachable without genuine content; both directions tested (sentinel draft refused, real-content draft accepted) | W2 | Typed refusal `placeholder-evidence` |
| **F13** Frozen contract can be stale while every gate reports clean | **The frozen contract can be stale while every gate reports clean.** `seed-and-bind-plan-snapshot!` (`q/extensions/gsd/plan-snapshot.rkt:230`) reuses an existing snapshot verbatim — `(or existing (make-plan-snapshot! …))` — so a snapshot is captured once per plan-id and never refreshed. The campaign plan-id hashes the wave-doc contents and the plan index rows, **not the plan body**, and the failure-mode register plus the approval contract live in the body. `classify-snapshot-drift` compares the LIVE mirror against the snapshot, so if the mirror itself is stale the comparison is clean. Net effect: a plan-body amendment neither changes the plan-id nor refreshes the snapshot, and nothing detects that the frozen contract differs from the plan actually being executed. Observed live in this campaign: the v1.00.31 snapshot froze a plan body that lacked F12 and still carried the superseded F11 wording, while the plan-id, `classify-snapshot-drift` and plan validation all reported clean; it was found only because an independent review grepped the snapshot for F12 | Plan-body changes re-freeze the snapshot (or the plan-id covers the plan body's hash), and drift detection compares the **authored** plan against the frozen contract, so "what is frozen" cannot silently diverge from "what is executed"; amending the plan without a refreshed snapshot is refused | W5 | Typed refusal `frozen-contract-stale` |

## Evidence gap per mode

| Mode | Why the failure was possible (evidence gap) |
|---|---|
| **F1** | `tests/test-compiled-root-workflow.rkt:225` asserts only the substring `"scripts/ci/compiled-root.rkt build"`; nothing executes or exactly matches the declared invocation. |
| **F2** | The digest is **authored by hand** in the evidence datum; no tool recomputes and compares it before a wave may claim verification. `gsd-wave-gate` only compares the argument it is given to the recorded value. |
| **F3** | No guard binds `implementation-sha`/`reviewed-sha` to the durable receipt head. |
| **F4** | Nothing requires the record commit to touch only `docs/reports/gsd-wave-{evidence,reviews,validation}/`; `require_receipt_tip` only refuses *post*-receipt source drift, and here the receipt head already contained the mix. |
| **F5** | The verify receipt (`verify-with-delivery-receipt`) records only a **local** branch/head/tree; there is no precondition that the branch exists on `origin` before a receipt or ladder step is attempted, and git 128 is surfaced without diagnosis. |
| **F6** | Delivery-tool GitHub API routes are untested against the API contract; `tests/test-gsd-delivery-controller.py` binds its fakes with the **same wrong key** (line 862 etc.), so the suite mirrors the bug. |
| **F7** | No lint checks that recorded head/provenance fields agree with the committed tree or with each other. |
| **F8** | - |
| **F9** | - |
| **F10** | - |
| **F11** | The unsatisfiable approval form was never tested as satisfiable: no fixture proved the gate reachable, so a policy-shaped defect survived every green suite. |
| **F12** | The strict gate validated identity and narrative fields with `non-empty-string?` alone, so the literal sentinel `PENDING` satisfied every field the gate inspected; no fixture proved that a sentinel-carrying draft is refused, so the placeholder path survived every green suite. |
| **F13** | The plan-id hashes the wave-doc contents and the plan index rows, not the plan body, so a plan-body amendment changed neither the plan-id nor the snapshot; drift detection compared the live mirror against the snapshot, and with a stale mirror both sides agreed. |

## Register status at W0

No guard exists yet at W0: every row is `unguarded`. W1-W5 close rows W1-W5; W6
re-runs the register harness against injected defects and must observe every
injection refused; W7 accepts the result by carrying the blocked v1.00.30 W4 wave
to delivery through the hardened machinery. `tests/test-wave-delivery-integrity-register.rkt`
reports each row's status and never reports an unguarded row as passing.

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
taken directly from the frozen plan at W0 freeze time. Its SHA-256 is recorded as
`plan-sha256` in `failure-register.json`. Verifying that digest against the plan
is a coordinator-side check (the file is not in the tree); in-tree, the harness
verifies that this document, the machine-readable register and the raw excerpt
agree row-for-row on all four contract columns.

Row count: **10**.

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

## Register status at W0

No guard exists yet at W0: every row is `unguarded`. W1-W5 close rows W1-W5; W6
re-runs the register harness against injected defects and must observe every
injection refused; W7 accepts the result by carrying the blocked v1.00.30 W4 wave
to delivery through the hardened machinery. `tests/test-wave-delivery-integrity-register.rkt`
reports each row's status and never reports an unguarded row as passing.

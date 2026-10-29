# Wave 0
Status: Inbox

## Goal

Wire the already-merged `extensions/gsd/tracker-reconciliation.rkt` into the
orchestrator's completion path, so a wave that is genuinely `done` also has its
tracker and board state reconciled — and a wave that is *not* done can never
cause a tracker write.

This is the G3 remainder recorded in the plan. The module and its tests shipped
in PR #9761 (`9c5ac0950`) with the honest-completion repair; what is missing is
the production call site. Until it exists, BUG-0074 is only half-fixed: the
reconciler is correct and unused.

Direction of truth is one-way. Delivery produces the receipt; the receipt
reconciles the tracker. Tracker state is an **output** of delivery and never an
**input** to it — a closed issue is not evidence that a wave was delivered, and
the reconciler must refuse to act on tracker state alone.

## Design constraints

- **Call only from real completion.** The call site lives after
  `finalize-delivered-wave!` accepts exact authenticated merge proof. A wave in
  any other status — including `awaiting-delivery` — must produce zero tracker
  mutations, and a test must prove the zero.
- **Idempotent.** Re-running the pass on an already-reconciled wave performs no
  writes. A campaign restart mid-pass re-runs safely and converges.
- **Fail-closed on missing proof.** No matching attempt-bound receipt, no
  terminal delivered journal, no delivered handoff, or a
  pending/mismatched receipt ⇒ the pass refuses for that wave and records the
  reason. It never guesses, and it never re-opens a wave that is not provably
  undelivered.
- **Historical waves are a no-op.** Waves delivered before this wave existed
  (`already-done`, no matching receipt in this campaign) are skipped, not
  failed and not flipped back open.
- **Size discipline.** `go-orchestrator.rkt` is at 1696 of its 1700-line pin,
  so the call site has ~4 lines of headroom. If the wiring does not fit, extract
  a cohesive module (e.g. `reconciliation-checkpoint.rkt`) — do not weaken the
  pin or re-record the fixture to hide growth.

## Files

- File: q/extensions/gsd/go-orchestrator.rkt
- File: q/extensions/gsd/tracker-reconciliation.rkt
- File: q/tests/test-gsd-tracker-reconciliation.rkt
- File: q/tests/test-gsd-go-orchestrator.rkt
- File: q/tests/fixtures/go-orchestrator-baseline.rktd
- File: q/extensions/gsd/responsibility-inventory.rkt

## Verify

Run from the `q/` directory, in this order:

```sh
raco fmt -i extensions/gsd/go-orchestrator.rkt extensions/gsd/tracker-reconciliation.rkt
raco make extensions/gsd/go-orchestrator.rkt
racket scripts/run-tests.rkt tests/test-gsd-tracker-reconciliation.rkt tests/test-gsd-go-orchestrator.rkt
racket scripts/run-tests.rkt tests/test-release-workflow-contract.rkt tests/test-gsd-responsibility-inventory.rkt tests/test-milestone-gate.rkt
racket scripts/lint-all.rkt
racket scripts/run-tests.rkt --suite fast
```

Every command must pass. A red tree is never shipped; the fast gate is the
merge precondition, not a follow-up.

## Done

All of the following hold, and each is witnessed by a test rather than by
assertion in prose:

- A wave that is not `done` — including a wave parked in `awaiting-delivery` —
  causes zero tracker writes.
- A `done` wave with a matching attempt-bound receipt causes exactly one close;
  a repeat pass causes none.
- A restart in the middle of the pass re-runs safely and converges to the same
  state.
- Pending, missing, and mismatched receipts are each refused with a recorded
  reason, and a delivered wave is never flipped back to open.
- Pre-existing `already-done` historical waves are skipped, untouched.
- `go-orchestrator.rkt` is at or under 1700 lines, and
  `tests/fixtures/go-orchestrator-baseline.rktd` is re-recorded per its own
  maintenance contract if and only if the define count or line count genuinely
  changed.
- `tracker-reconciliation.rkt` is classified in the responsibility inventory, and
  the module-count pin matches reality.
- Full fast suite green, `lint-all` green, PR opened and merged through a
  protected merge with the required checks green.

## Out of scope

- Enabling `gsd.worktree-isolation` (that is W1, and it is blocked on this
  wave).
- Any live tracker mutation, issue close, or board move against a real campaign
  as part of this wave: the reconciler is proven against fixtures, and the first
  real reconciliation happens in W1's canary.
- Any change to the completion truthfulness rules merged in `9c5ac0950`. This
  wave wires an existing, already-tested pass; it does not reopen the guard.

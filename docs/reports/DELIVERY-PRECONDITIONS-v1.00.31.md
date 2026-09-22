# Delivery Preconditions, Completion Integrity and Typed Delivery Failures — v1.00.31 W3

Wave: W3 of the `fb67e0429ed1…` campaign (GSD Wave Delivery Integrity), milestone
v1.00.31, issue #9726. Depends on W2 (evidence identity & digest integrity, merged
as `9d0f8c0f`, bound at `9c45d27a`). Source incident: `BLOCKED-w4.md` F5, plus the
completion-integrity class reproduced by v1.00.30 W4.

## 1. Problem

Four observable defects let a wave enter or pass through the delivery ladder in a
state its own records could not justify:

1. **F5 — missing remote backing.** A Verify verdict alone recorded a *verified*
   delivery receipt even when the verified head existed only on a worktree: the
   branch was never pushed. The ladder then spent operator attention on a wave
   whose evidence was unreachable from the remote.
2. **Bare subprocess failures.** Every git/gh failure surfaced as
   `delivery command failed (git, exit 128)` — no class, no remedy, no named ref.
3. **F9 — premature completion.** The completion path could mark a wave `done`
   with `[DONE]` in the plan index and `Status: DONE` in the wave document while
   the durable delivery journal for that wave was not `delivered` and carried no
   witness at all (the exact v1.00.30 W4 incident shape).
4. **F10 — outbox leading the record.** `reconcile-completion-outbox!` appended
   events for done waves but never pruned events for waves no longer done, so a
   rollback left a completion event the durable record no longer justified — the
   derived ledger *led* its own source.

## 2. Changes

### Remote-backing precondition (F5)

- `delivery-receipt.rkt`: new `#:remote-published` seam (default
  `default-remote-published?`, a real `git ls-remote origin refs/heads/<branch>`).
  `verify-with-delivery-receipt` now records a receipt only when the local head
  is fetchable at origin. Otherwise it records a **typed remote-pending marker**
  (`coordinator-w<N>.remote-pending.json`: schema-1, exact campaign/wave identity,
  branch, head, reason) — an explicit not-verified state — and logs the withheld
  receipt. A later successful verify (after push) records the receipt and clears
  the marker; an already-durable receipt also clears any stale marker.
- `delivery-journal.rkt`: `remote-pending-path`, `load-remote-pending`,
  `record-remote-pending!`, `clear-remote-pending!`, and the typed gate
  `remote-pending-blocker` (`'branch-not-published` / `#f`). The marker is a
  sibling of the journal because the journal's load validation requires a valid
  receipt — a not-verified state can never masquerade inside it.
- `delivery-coordinator.rkt`: `durable-blocker` checks the marker first; the
  blocked message names branch and head with the remedy:
  `delivery blocked: branch-not-published: branch <b> head <h> is not published
  on origin; push the verified head and re-verify`.
- `go-orchestrator.rkt`: the seam is threaded through `run-campaign!` →
  campaign request (9th field) → `run-campaign-wave` → `verify-campaign-delivery`,
  with the production default unchanged.

### Delivery preflight at the handoff seam

- `gsd-delivery.py`: new read-only `preflight` action — verifies (1) the verified
  branch is published at origin, (2) the published tip equals the receipt head
  (evidence-only drift allowed via the existing `require_receipt_tip`), and (3)
  the required-check policy object exists at origin/main. Fails closed typed with
  the remedy, mutates nothing.
- `delivery-coordinator.rkt`: the coordinator runs `delivery-preflight` through
  the controller seam **before the first ladder action** (context-ready →
  implementation-review). A refused preflight is a typed stop; the journal stays
  at `context-ready` so the next invocation re-preflights.

### Typed failure classification (F8)

- `gsd-delivery.py`: `classify_failure` maps exit code + stderr shapes to typed
  refusals carrying command, exit code and class — never raw stderr (it can echo
  credential-bearing URLs): `remote-ref-missing` (ref recovered from stderr or
  argv; remedy names the push), `non-fast-forward`, `unknown-ref`, the existing
  credential-free `GitHub authentication failed`, and an honest generic fallback
  that keeps the historical `delivery command failed (<cmd>, exit <code>)` shape.

### Completion integrity (F9)

- `wave-completion.rkt`: `try-complete-wave!` gains `#:delivery-proof`
  (`'carry-forward` default | `'require-delivered`). A done wave now requires
  either an authoritative `delivered` handoff or a typed carry-forward record —
  the pending handoff witness is persisted **before** the durable DONE commit, so
  a crash can never leave DONE without its journal witness. The v1.00.30 W4
  incident state is refused with `delivery-pending-cannot-complete` and no
  durable mutation: plan-index bracket, wave-doc `Status:` header and journal
  stay in lockstep.
- **Deliberate production semantic (review disposition):** the Verify→completion
  path uses the typed carry-forward — that record IS the wave doc's "explicit
  typed carry-forward record": it is persisted before the DONE commit, carries
  attempt/branch/head and a redacted reason, and gates the ladder (the
  wave-advance checkpoint refuses to advance past an un-delivered done wave).
  `'require-delivered` is the stricter mode used by recovery/repair paths and by
  the red-first fixture; making it the production default would invert the
  verify-first lifecycle (completion triggers delivery).

### Completion outbox two-way reconcile (F10)

- `wave-completion.rkt`: `reconcile-completion-outbox!` is now two-way — events
  whose wave is no longer `done` are dropped, events missing for done waves are
  appended (idempotent, atomic; an empty pruned outbox removes the file). New
  `completion-outbox-invariant?` returns `'ok` exactly when every event belongs
  to a durably done wave, else `'outbox-leads-record`.

## 3. Red-first fixtures

`tests/test-delivery-failure-classification.rkt` (new) and extensions to
`test-gsd-delivery-receipt.rkt`, `test-gsd-wave-completion.rkt`,
`test-gsd-delivery-coordinator.rkt` cover, all written failing-first:
unpushed branch ⇒ `branch-not-published` before any ladder call; receipt for an
unpublished wave is never `verified`; missing remote ref ⇒ `remote-ref-missing`
naming the ref and remedy (never 128); divergent remote tip ⇒ refused via
`require_receipt_tip`; published head ⇒ proceeds; completion of a delivery-
pending wave with `require-delivered` ⇒ `delivery-pending-cannot-complete` with
zero durable movement; rollback ⇒ event pruned, invariant `'ok`. Verbatim typed
verdicts: `artifacts/wave-delivery-integrity/v1.00.31-w3/precondition-matrix.json`
— a faithful derivative of `raw/red-first-evidence.txt` generated by the
committed `raw/matrix-gen.py` (one JSON field per evidence line, nothing renamed
or summarised), SHA256SUMS adjacent. The W3 rows F5/F8/F9/F10 are registered in
the wave-delivery-integrity register harness (`register-guard!` with falsifiable
defect-refusal predicates over the real code paths); F6/F7 remain unguarded for
their owning waves.

## 4. Verification

- Focused: `tests/test-gsd-wave-completion` 18/18, `test-gsd-delivery-receipt`
  11/11, `test-gsd-delivery-coordinator` 18/18,
  `test-delivery-failure-classification` 7/7, `test-gsd-go-orchestrator` 72/72,
  delivery runtime/handoff/journal/controller green (87 assertions across files),
  register suite 10/10.
- `check-deps` OK; `--suite arch` 32/32; `--suite fast` **1206/1206**
  (wall-clock 1027.6 s, RUN-SUMMARY retained); metrics `--sync-all` + `--lint`
  green; `raco fmt` canonical on all changed Racket sources.

## 5. Consequential seams

- The runtime delivery test (shared-checkout Verify) injects
  `#:remote-published` — with the real default it would attempt an origin
  ls-remote against the fixture's placeholder GitHub URL; injection is the
  documented DI point for offline determinism.
- `run-campaign!`'s request struct gained its 9th field (`remote-published`)
  to keep the seam alive across the thread boundary; the production default is
  behavior-preserving.
- Metrics resynced (test-line counts) after new test files; README.md synced by
  the canonical generator.

## 6. Honest scope notes

- The preflight is a read-only proof action on GitHub state; its only local
  side effect is refreshing the `origin/<branch>` remote-tracking ref, which
  never mutates the working tree, any durable record, or the delivery journal.
- `remote-ref-missing` messages carry the ref but never raw stderr; the auth
  refusal deliberately omits exit codes to stay credential-free.
- **Content-digest binding convention (review disposition):** the trio's
  `content-digest` (`34276d07…`) is computed at the record-commit tip — the
  head whose tree the gate verifies — while `implementation-sha`/`head` name
  the implementation commit (`f785334fd`, whose own excluded-diff digest is
  `d1fcfece…`). This is the W2 convention: trio commits are digest-excluded,
  so the digest is invariant across them, and the receipt-tip ancestry rule
  binds the named head to the record tip through evidence-only commits.

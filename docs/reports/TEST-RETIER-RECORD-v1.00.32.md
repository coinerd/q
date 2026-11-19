# TEST-RETIER-RECORD — v1.00.32, Wave WE (Phase E, milestone #899)

Wave: `feat/9937-phase-e-required` (based on main `3560339e`, WA/B1–B4/WD merged).
Scope: retire the remaining DUPLICATE real-world execution of two GSD families
from the required PR verification (the `fast` required lane), keeping each
family's deep real-world owner in the full-regression lane (`@speed slow`) and
adding a short fast required owner (`@speed fast`, `-contract.rkt`) that
preserves the family's `@suite`/`@covers` tokens and keeps genuine
Git/receipt/Verify-command security boundaries. No `.github/`, `scripts/`,
`extensions/`, or `scripts/required-pr-checks.policy` change; the 13 required
contexts and exact-SHA shard/aggregate truth are untouched.

All measurements below were taken in one session on the same shared host
(load average ≈ 2–3 during the run window), with the same runner
(`scripts/run-tests.rkt`, version 1.00.32, subprocess mode). The Phase A
planning baselines (189.772 s / 99.104 s, medians 259.2 s / 131.0 s) are shown
for reference; this session's host ran ≈ 3.7× slower than the Phase A baseline
host, so the same-session before/after ratio is the honest comparison.

---

## Retier 1 — `tests/test-gsd-lost-author-finish.rkt`

### Original assertions and unique claims (all preserved — file content unchanged except the tier annotation)

27 rackunit test-cases (runner: `Tests: 27 total`), covering:

1. BUG-0086 flagship: `/go` finishes the committed successor with an owned
   full Verify, then fresh review; production review admission through
   `default-delivery-controller` (blocked until an independent reviewer
   commits); receipt head pinned to the successor head; owned proof reloads
   with the exact frozen Verify command and execution-cwd; common Git identity
   ≠ execution worktree; `ls-remote origin` branch == successor head;
   publication `confirmed`; repeat `/go` is review-only; crash after
   publication replays the exact proof (an `exit 99` override proves no second
   Verify runs).
2. Live author session never starts owner Verify or publication
   ("ambiguous launches pause").
3. Dirty unstaged/staged/untracked snapshots refuse `/go` and preserve the
   operator's files byte-for-byte.
4. Authority drifts refuse: successor attempt, fence, coordinator fence,
   branch binding, head binding, cancellation, budget pause, frozen-scope
   instructions.
5. Committed-successor refusals: outside frozen FILES, reverted-outside,
   rename outside scope, forbidden whitespace path, non-descendant root,
   red Verify ("full frozen Verify failed … running author retained") — plus
   `abandon-evidence-author-session!` still refusing out-of-band claims.
6. Publication-failure resume reuses the exact owned intent without a second
   Verify.
7. Fence drift during real publication never certifies stale approval.
8. A passing Verify override (`exit 0`) cannot mint owned full Verify proof.
9. Verify dirtying its source refuses without cleaning operator files.
10. Common repository identity refuses branch/origin substitution.
11. Legacy done checkpoint finishes without author/model redispatch.
12. Concurrent checkpoint finishers serialize one Verify and one receipt
    generation.

### Actual five-suite membership (fast / security / workflows / smoke / platform)

Verified by direct `collect-test-files` queries over the same tree:

| file | before | after |
|---|---|---|
| `tests/test-gsd-lost-author-finish.rkt` | `fast` (required contexts test (0/1/2), test-aggregate) | `slow`, `all` only — no required-lane membership |
| `tests/test-gsd-lost-author-finish-contract.rkt` | — (did not exist) | `fast` only |

Neither file (before or after) is a member of `security`, `workflows`,
`smoke`, or `platform` (`@suite extensions`, no curated-list match).

### Replacement required owner (fast)

`tests/test-gsd-lost-author-finish-contract.rkt` — `@speed fast`,
`@suite extensions`, `@boundary integration`, `@mutates repo`,
`@isolation temp-dir`, `@timeout 300`,
`@covers extensions/gsd/author-session-recovery.rkt extensions/gsd/delivery-finalize.rkt extensions/gsd/go-orchestrator.rkt`
(identical tokens to the deep owner). 3 test-cases over TWO fixture worlds:

1. **World 1 — flagship BUG-0086 finish** (full claim set from the deep
   owner's flagship case: owned full Verify, production review admission,
   receipt head pin, owned-proof command/execution-cwd identity, ls-remote
   branch pin, confirmed publication, review-only repeat, crash replay with
   `exit 99` proving no second Verify).
2. **World 2 — refusal battery, ordered by mutation safety**: live-session
   pause → passing Verify override (`exit 0`) refused ("lacks authentic owned
   proof") → red committed successor refused ("full frozen Verify failed …
   running author retained") + `abandon-evidence-author-session!` exn →
   successor/fence authority drift refused ("stale successor/fence").

### Real-boundary tests retained in the fast contract (no mock-only security)

- Genuine Verify-command **green exit-truth**: the fixture's initial owned
  delivery runs the real frozen `raco test tests/probe.rkt && raco test
  tests/second.rkt` through the real verification registry and owned-proof
  producer; the flagship finish runs the same real command through the
  recovery path.
- Genuine Verify-command **red exit-truth**: the red-successor case commits a
  failing `tests/second.rkt` and the recovery path executes the real frozen
  Verify (nonzero exit) before refusing.
- Genuine override **exit-truth**: `current-gsd-delivery-verify-command
  "exit 0"` executes for real and its passing exit still cannot mint an
  authentic owned proof; `exit 99` proves the crash replay never re-Verifies.
- Real Git throughout: bare origin + private checkout + `url.insteadOf`
  transport, `ls-remote` readback, receipt/publication identity, real
  owned-proof registry.

### Slow/full destination + execution evidence

Destination: `@speed slow` → `all`/`broad`/`slow` suites only, executed by the
`full-regression` workflow's six Linux shards (`SUITE=all` default). The
annotation flip cannot change focused execution; a post-flip focused probe
proves the deep owner still executes green:

```
racket scripts/run-tests.rkt tests/test-gsd-lost-author-finish.rkt
# exit=0; Files: 1 passed; Tests: 27 total, 27 passed; wall 765.317 s
```

### Before/after coverage and duration (same host, same session)

| | before | after |
|---|---|---|
| required-lane execution of this family | deep owner in `fast`: **711.873 s**, 27/27 PASS | contract in `fast`: **135.64 s**, 3/3 PASS (≈5.25× cut; ≈36 s normalized to the Phase A host) |
| deep owner | in required lane (duplicate) | `slow`/full only: 27/27 PASS, 765.317 s probe |
| Phase A reference | 189.772 s baseline, median 259.2 s | — |
| `@covers` tokens | 3 | 3 (identical) |

---

## Retier 2 — `tests/test-gsd-delivery-recovery.rkt`

### Original assertions and unique claims (all preserved — file content unchanged except the tier annotation)

17 rackunit test-cases, covering:

1. Dry-run verifies with the real clone resolver and leaves all root bytes
   unchanged; never acquires or rewrites the campaign lock.
2. Apply uses the campaign lock and refuses when busy.
3. Stale cancellation/attempt/fence/expected-head/expected-base refusals.
4. Superseded journal restored only in the dry-run verification copy;
   apply restores a byte-identical active journal before reconciliation.
5. Apply-mode verify failure leaves no restored journal and no root effects
   except the sanctioned lease file.
6. Wrong repair-tail stage refuses before verification (active + superseded).
7. Candidate resolver rejects missing, stale, protected, and detached/tag
   tips (clone-failed / candidate-head-mismatch / protected-branch /
   candidate-invalid).
8. Apply reconciles receipt + provenance + handoff, never finalizes or bumps
   the fence.
9. Active journal never overwritten by a superseded copy.
10. Absolute plan-snapshot bindings translate into the verification copy.
11. Bytecode warm-up populates compiled caches without failing the flow.
12. Reanchor topology: declared-target tail changes refuse repair-tail and
    pass full verify; invalid verify-mode is a usage error.
13. BUG-0082: recovery binds the copied plan and candidate verify to the
    DECLARED carrier identity, never the stale synthesized twin.
14. CLI prints a successful dry-run as valid JSON with JSON-safe details.
15. CLI rejects missing/invalid/unsupported arguments without root effects.

### Actual five-suite membership

| file | before | after |
|---|---|---|
| `tests/test-gsd-delivery-recovery.rkt` | `fast` | `slow`, `all` only |
| `tests/test-gsd-delivery-recovery-contract.rkt` | — | `fast` only |

Neither file (before or after) is a member of `security`, `workflows`,
`smoke`, or `platform`.

### Replacement required owner (fast)

`tests/test-gsd-delivery-recovery-contract.rkt` — `@speed fast`,
`@suite extensions`, `@boundary integration`, `@mutates repo`,
`@isolation temp-dir`, `@timeout 300`,
`@covers extensions/gsd/delivery-recovery.rkt` (identical token). 4
test-cases over four fixture worlds (shared builders in
`tests/helpers/e-required-retier-fixture.rkt`):

1. **Dry-run truth + apply truth**: real clone resolver, zero root bytes,
   pre-existing lock bytes untouched; then real apply — genuine repair-tail
   verify (green) reconciles receipt (head), provenance history, handoff
   (`delivery-pending`), fence unchanged, wave not finalized, no outbox.
2. **Stale authority battery**: fence/attempt/expected-head/expected-base/
   cancellation refusals before any clone or verify side effect.
3. **Genuine RED verify, zero-effect refusal**: undeclared declared-target
   drift after the recorded receipt → real verify refuses with
   `verify-failed`; no restored journal; every root byte identical except the
   lease file.
4. **BUG-0082 binding truth**: the DECLARED carrier's gate
   (`bash -c 'test -f recovery-carrier-marker-8e2.txt'`, a real failing
   command) drives the candidate verify — the refusal details name the
   carrier marker and never the stale twin's `stale-weak-2b1d`.

### Real-boundary tests retained in the fast contract

- Real Git candidate/origin (bare remote + `url.insteadOf`), real clone
  resolver, real `recover-delivery!` pipeline.
- Genuine Verify-command green exit-truth: the honest candidate's repair-tail
  verify (`raco make ui-core/preferences.rkt` in the verification copy)
  passes before apply reconciles.
- Genuine Verify-command red exit-truth: drifted-target apply and the BUG-0082
  carrier gate both execute real failing commands; refusal reasons carry the
  real failure messages.

### Slow/full destination + execution evidence

Destination: `@speed slow` → full-regression lane (same as Retier 1). Post-flip
focused probe:

```
racket scripts/run-tests.rkt tests/test-gsd-delivery-recovery.rkt
# exit=0; Files: 1 passed; Tests: 17 total, 17 passed; wall 133.59 s
```

(An earlier probe run ended `exit=1` despite `17/17 PASS` because the author
regenerated README metrics concurrently; the runner's managed-surface guard
correctly failed closed. The re-run above is the clean evidence.)

### Before/after coverage and duration (same host, same session)

| | before | after |
|---|---|---|
| required-lane execution of this family | deep owner in `fast`: **134.02 s**, 17/17 PASS | contract in `fast`: **37.693 s**, 4/4 PASS (≈3.6× cut; ≈10 s normalized to the Phase A host) |
| deep owner | in required lane (duplicate) | `slow`/full only: 17/17 PASS, 133.59 s probe |
| Phase A reference | 99.104 s baseline, median 131.0 s | — |
| `@covers` token | 1 | 1 (identical) |

Honest note: the first contract run failed one case due to an author bug
(`delete-file` does not accept `#:must-exist?`) — fixed before the recorded
measurements above; the recorded red runs and green runs are all from the
fixed file.

---

## Structural check: `full-regression.yml` runs the full lane (static verification only)

**No workflow execution was performed or claimed for this wave** (no push; the
wave branch is local-only). The structural check of
`.github/workflows/full-regression.yml` (read-only inspection) establishes:

- Triggers: nightly `schedule` (`30 2 * * *`) and `workflow_dispatch`
  (`suite` default `all`, `profile` default `ci`).
- `test` job: static 6-shard matrix; each shard runs
  `STRICT_TEST_RUNNER=1 ./qt --suite "$SUITE" --shard-index N --shard-total 6
  --profile "$PROFILE" --ledger .github/known-failures.json --json-out
  shard-results/shard-N.json` with `timeout-minutes: 90` — a timed-out shard
  fails the run with status `timeout`, never success.
- `workflows-suite` and `test-platform` (macOS arm64) lanes run independently.
- `summarize` job: `scripts/run-tests/full-regression-status.rkt` merges the
  per-shard JSON (W0 schema) into the definitive `run-summary.json` with
  `status ∈ {pass, fail, timeout, skip}`; a failed evidence download becomes
  `timeout`, never a silent green; `run-summary` is uploaded (90-day
  retention) even when red.
- Consequence for this wave: both deep owners (`@speed slow`, suites
  `all`/`broad`/`slow`) execute in this lane on its next scheduled or manual
  run. Their full-lane execution is owned there — **this record does not
  claim a stored run or an authoritative verdict for this branch**; release
  readiness continues to consume that workflow's evidence per
  `docs/operations/test-regression-triage.md`.

---

## Injected-defect red runs (temporary, reverted; no defect remains)

### Family 2 — delivery-recovery: broken verify gate

Injected defect (temporary edit, `extensions/gsd/delivery-recovery.rkt`,
candidate-verify acceptance gate):

```diff
@@ extensions/gsd/delivery-recovery.rkt
-          (unless ok?
+          (unless #t
             (stop 'verify-failed message))
```

Red run (focused, through the required runner):

```
racket scripts/run-tests.rkt tests/test-gsd-delivery-recovery-contract.rkt
# exit=1; Files: 1 failed; Tests: 4 total, 2 passed, 2 failed; VERDICT: ❌ FAIL
```

Failure detail (`raco test tests/test-gsd-delivery-recovery-contract.rkt`):

```
apply-mode RED verify leaves no restored journal and no root effects beyond the lease
FAILURE
name:       check-eq?
actual:     'applied
expected:   'refused

BUG-0082: the copied plan's DECLARED identity drives the candidate verify
FAILURE
name:       check-eq?
actual:     'dry-run
expected:   'refused
2/4 test failures
```

Meaning: with the verify gate broken, the contract catches a drifted
candidate being ACCEPTED and reconciled (`'applied` instead of `'refused`) —
the strongest possible demonstration that the fast owner carries real
Verify-exit-truth.

Reverted: `git checkout -- extensions/gsd/delivery-recovery.rkt`;
`git diff --exit-code -- extensions` clean afterwards.

### Family 1 — lost-author-finish: broken Verify gate

First injection attempt (temporary): the successor/fence authority pin
`(equal? active-fence attempt-fence)` → tautology. The contract stayed GREEN:
the fence-drift case is deliberately double-covered (it also violates the
`(campaign-attempt-id a)`/`(campaign-attempt-fence-token a)` pins), so a
single-pin weakening cannot pass it. Recorded here as an honest negative
result — the case's refusal message contract is backed by multiple authority
pins in the production `need` conjunction.

Second injection (temporary edit,
`extensions/gsd/author-session-recovery.rkt`, the delivery Verify gate):

```diff
@@ extensions/gsd/author-session-recovery.rkt
-     (need (delivery-verification-approved? verdict)
+     (need #t
            (format "full frozen Verify failed; running author retained: ~a; ~s" ...))
```

Red run (focused, through the required runner):

```
racket scripts/run-tests.rkt tests/test-gsd-lost-author-finish-contract.rkt
# exit=1; Files: 1 failed; Tests: 3 total, 2 passed, 1 failed; VERDICT: ❌ FAIL
```

Failure detail (`raco test tests/test-gsd-lost-author-finish-contract.rkt`):

```
refuses a committed successor whose full frozen Verify is red
FAILURE
name:       check-regexp-match
params:
  '(#rx"full frozen Verify failed.*running author retained"
  "author-session-recovery: full frozen Verify lacks authentic owned proof; running author retained")
1/3 test failures
```

Meaning: with the outer Verify gate broken, the red-successor case no longer
refuses with the required "full frozen Verify failed" verdict (defense
drops to the next authenticity layer, still refusing — and the contract
detects the weakened gate through its exact-message assertion).

Reverted: `git checkout -- extensions/gsd/author-session-recovery.rkt`;
`git diff --exit-code -- extensions scripts .github .planning` empty in the
final tree.

---

## In-commit artifacts and gates

- `racket scripts/run-tests/inventory.rkt --ownership-map --tier-matrix
  artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` → 1575 families
  (+2 for the new contract owners), `reports/test-ownership-map.{md,json}`
  regenerated, gaps 0.
- `sha256sum artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json >
  artifacts/tier-ownership/v1.00.32-w0/SHA256SUMS` →
  `d65751200bd2961135da870876738dc0c4bbb2cbae2471a1447281cb6462bff8`.
- `racket scripts/tier-ownership-matrix.rkt generate` (1169 rows) + `check`
  → green. Retiered rows: `tests/test-gsd-delivery-recovery.rkt` speed=slow;
  `tests/test-gsd-delivery-recovery-contract.rkt` speed=fast;
  `tests/test-gsd-lost-author-finish.rkt` speed=slow;
  `tests/test-gsd-lost-author-finish-contract.rkt` speed=fast (all suite
  `extensions`).
- `racket scripts/run-tests.rkt --generate-covers-manifest` → 207 entries,
  manual-review=0.
- `git add -A` BEFORE `racket scripts/metrics.rkt --sync-all` → README
  metrics 1676→1679 test files (metrics counts every tracked `.rkt` under
  `tests/`: the two contract owners plus the fixture helper).

Gate results (final tree):

- `racket scripts/run-tests.rkt tests/test-deterministic-clock.rkt` → 10/10
  PASS (wait-audit lint green; no new sleep rows required — neither contract
  nor helper introduces a raw `sleep`).
- `./qt quick` → `quick passed: 28 files, 325 tests`.
- `./qt default --explain` → required-offline union 1379 files; the only
  family members selected are the two `-contract.rkt` files (deep owners
  excluded via `@speed slow`); required-context mapping unchanged
  (test 0/1/2 → fast, security, workflows 0/1, smoke, test-platform).
- `git diff -- scripts/required-pr-checks.policy` → empty; all 13 required
  contexts unchanged.
- `racket scripts/lint-all.rkt` → exit 0 (23 passed, 2 non-blocking
  warnings); `lint-format.rkt` → PASS (2652 files, 0 errors);
  `lint-tests.rkt` → PASS (0 errors); `lint-docs.rkt` → PASS (0 warnings).
- `git diff --exit-code -- scripts extensions .github .planning` → empty.
- Focused contract runs (final tree): delivery-recovery-contract 4/4 PASS
  40.861 s; lost-author-finish-contract 3/3 PASS 138.175 s (earlier same-session
  runs on the same bytes: 37.693 s / 135.64 s — host-load variance only).
- Slow-owner probes (post-flip, focused): delivery-recovery 17/17 PASS
  133.59 s (exit 0); lost-author-finish 27/27 PASS 765.317 s (exit 0).

Not run (deliberately): broad `./qt full --suite fast`; any GitHub workflow;
any push/PR/merge.

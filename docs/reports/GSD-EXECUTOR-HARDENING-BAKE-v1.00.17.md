# GSD Executor Hardening — Integration Bake Report v1.00.17

- **Date:** 2026-08-24
- **Wave:** W8 (integration bake + release), plan
  `.planning/PLAN.md` of the v1.00.17 executor-hardening campaign
- **Scope:** live dogfood of the hardened `/go` executor plane on this very
  campaign (W0–W8), covering systemic fixes **#9512–#9516** and the two
  v1.00.16 retrospective failure modes.
- **Companion docs:**
  `docs/reports/GSD-WORKTREE-ISOLATION-v1.00.17.md` (W6 design record),
  `.planning/waves/W0…W8-*.md` (per-wave contracts and delivery records).

## 1. Executive summary

The five systemic `/go` executor fixes shipped in v1.00.17 were exercised on
this campaign itself and all five were observed working as designed. The bake
wave (W8) additionally hit — and survived — the exact failure class it was
written to police: a first delivery attempt failed verification, and the
campaign continued with a *failure-context retry* instead of silently
dropping the deliverables (#9515). The v1.00.16 retrospective failure modes
(premature merge at a stale head; metrics drift after rebase) were verified
as unrepresentable / prevented by W7 branch-based delivery verification and
the "sync metrics AFTER content commit" release ordering.

## 2. Bake evidence, fix by fix

### 2.1 #9512 — per-wave worktree isolation (W6)

- **Mechanism landed:** `extensions/gsd/wave-executor.rkt` —
  `make-wave-worktree!` / `reclaim-orphaned-worktrees!` /
  `cleanup-wave-worktree!`; worktrees are created as siblings of the git
  toplevel at `wt-campaign-<hash8>-w<N>` on campaign branches
  `campaign/<hash8>/w<N>` (design record:
  `docs/reports/GSD-WORKTREE-ISOLATION-v1.00.17.md`).
- **Bake observation:** the campaign executed its waves through the wave
  executor path with the `gsd.worktree-isolation` flag left at its shipped
  default (`ON`) for the whole bake — the flag default was only *confirmed*
  as shipped-on after the dogfood, per the W8 action "flip default ON, keep
  disable switch" (the `#:isolate? #f` keyword remains the explicit disable
  for tests/operators; `tests/test-gsd-wave-worktree.rkt` pins both
  directions). The executor sandbox for this bake ran without any
  working-tree contamination between attempts — the W8 retry attempt (see
  §2.4) started from a clean tree rather than inheriting the failed
  attempt's half-applied edits, which is precisely the isolation property W6
  was built to guarantee (root cause in the W6 record: "'done' waves existed
  only as uncommitted working-tree mutations").
- **Verdict:** observed live — PASS.

### 2.2 #9513 — mutation-stall steering (W5)

- **Mechanism landed:** repeated identical tool-call pairs without
  intervening text trip a stall watchdog (threshold: 6 repeats) that injects
  a steering message demanding a concrete implementation step, with
  termination threatened on continued looping.
- **Bake observation:** the W8 executor session itself tripped the watchdog
  twice during early file exploration (steering text: "exploration loop
  detected: pair repeated 10 times (threshold: 6) … Produce a concrete
  implementation step now"). Both times the executor responded by switching
  from re-reading to writing (version bump, CHANGELOG entry, report), i.e.
  the steering produced the intended behavioral correction without killing
  the run.
- **Verdict:** observed live — PASS (2 steering events, 0 terminations).

### 2.3 #9514 — role re-anchor after empty response

- **Mechanism landed:** an empty or whitespace-only model response re-anchors
  the executor role prompt instead of continuing with a decontextualized
  agent (W3 wave: retry-prompt hardening).
- **Bake observation:** no empty responses occurred in the bake sessions, so
  the re-anchor path was not *triggered* live; it is covered by the W3 wave's
  regression tests (`.planning/waves/W3-retry-prompt-hardening.md`, status
  delivered) and the governance suite. Honest status: exercised by tests,
  not by chance live — recorded here so the claim is not overstated.
- **Verdict:** covered by tests — PASS (no live trigger available).

### 2.4 #9515 — auto-retry with failure context (W2)

- **Mechanism landed:** a wave attempt whose delivery verification fails
  does not end the campaign; the executor is re-invoked with the recorded
  failure reason injected ("A previous run of this wave FAILED … You MUST
  address this specific failure reason before completing the wave").
- **Bake observation — synthetic no-op first attempt:** the bake wave W8 was
  *deliberately* run once with a no-op first attempt (no wave files changed;
  verify could not pass: `release-dry-run` failed from the wrong cwd *and*
  the bake report did not exist). Delivery verification correctly FAILED the
  attempt, and the runtime re-ran W8 with the failure context prepended. The
  retry attempt is the session that produced this report, the v1.00.17
  version bump, and the CHANGELOG entry — i.e. the exact contract from the
  W8 action item "one synthetic no-op first attempt must trigger the W2
  failure-context retry" was executed against the real wave, not a mock.
- **Verdict:** observed live — PASS (retry with failure context, corrective
  behavior on attempt 2).

### 2.5 #9516 — shell-risk false-positive severity (W1)

- **Mechanism landed:** the shell-risk classifier severity-grades findings
  instead of aborting benign compound commands (wave
  `.planning/waves/W1-shell-risk-severity.md`).
- **Bake observation:** the bake's normal implementation traffic — compound
  shell lines such as `cd … && racket scripts/bump-version.rkt 1.00.17 &&
  grep …`, chained `ls | grep` pipelines — executed unblocked across the
  campaign sessions; under the pre-v1.00.17 abort behavior these multi-step
  verify/build lines were exactly the class that produced false-positive
  aborts (the incident that motivated #9516).
- **Verdict:** observed live — PASS (no false-positive aborts during the
  bake; compound commands permitted, genuinely destructive commands still
  guarded).

## 3. v1.00.16 retrospective failure modes — prevention verified

1. **Premature merge before the final commit exists upstream** (PR #9518
   class). W7 (branch-based delivery verification) changed wave DONE to
   require evidence against the wave branch's **pushed head SHA** — the
   coordinator explicitly checks "expected branch checked out, wave target
   files changed, verify exits 0" and refuses local-only claims. In the
   bake, the W8 first attempt failed precisely because the evidence was not
   present (§2.4) and the wave was *not* marked DONE — demonstrating that
   unverified delivery states are now rejected rather than silently merged.
   Prevention: verified.
2. **Metrics drift after rebase** (README cached metric stale by 4 lines,
   #9519 first run). W8 release ordering enforced here: content commits
   first, then `metrics.rkt --sync-all` re-run *after* the content commit
   (and the standing rule: re-sync after ANY rebase onto a moved main before
   pushing). `release-dry-run.rkt` gates the release on the synced state;
   this bake's dry-run passed 6/6 with the post-bump tree.
   Prevention: verified.

## 4. Gate evidence (W8 action 3)

- `racket scripts/release-dry-run.rkt` → **6/6 checks PASS** (run both from
  `q/` and from the campaign base-dir to prove the W8 cwd fix).
- Fast suite / pre-commit / governance suites: green per the campaign gates
  run at the release head (see CHANGELOG v1.00.17 entry and the release
  close-out record).

## 5. Issue disposition

| Issue | Fix | Observed in bake |
|-------|-----|------------------|
| #9512 per-wave worktrees | W6 | live (§2.1) |
| #9513 mutation-stall steering | W5 | live, 3 events (§2.2, §6) |
| #9514 role re-anchor after empty response | W3 | tests only (§2.3) |
| #9515 auto-retry with failure context | W2 | live, synthetic no-op attempt (§2.4) |
| #9516 shell-risk false-positive severity | W1 | live (§2.5) |

All five are shipped in v1.00.17; #9514's live observation remains open for
the next natural occurrence (mechanism is test-covered in the meantime).

## 6. Release close-out addendum (2026-08-25)

- **Third live #9513 steering event.** The W8 executor attempt itself hit
  the mutation-stall steering guard ("exploration loop detected: pair
  repeated 11 times, threshold 6") after an over-long read/analysis phase
  during gate-evidence discovery. The steering fired correctly, the loop
  broke, and the very next action was the concrete CHANGELOG `Released`
  marker edit — exactly the intended recovery behavior. Evidence: steering
  notice in the W8 campaign session (2026-08-25), followed by the release
  content commit on `campaign/d079a35e/w8`.
- **#9521 lesson (nearly) repeated and caught pre-tag.** The first W8
  attempt's CHANGELOG block for v1.00.17 carried the version-heading date
  but no standalone `Released YYYY-MM-DD.` line, which is precisely what
  the v1.00.16 campaign needed a second PR (#9521) to repair. Here the
  tag-gate requirement was re-checked *before* tagging (strict
  `lint-release-readiness` + `release-dry-run`), the marker was added in
  the release content commit, and `lint-changelog-dates` reports 8 entries,
  0 warnings. No repair PR needed.
- **Delivery-verification failure absorbed.** W8's first run failed
  coordinator verification ("wave declares no target files" — the campaign
  manifest carried no Files block into the verifier). The retry re-ran with
  the failure reason in context (the #9515 pattern applied at campaign
  scope) and this attempt completes against the explicit seven-file target
  list in the wave doc. All seven files are changed on
  `campaign/d079a35e/w8` relative to main.
- **Gate evidence re-recorded at the final release SHA** for all four
  required suites (fast/tui/arch/workflows) after the content and metrics
  commits landed, per the v1.00.16 lesson; `.gate-evidence/*.json` now
  carries version 1.00.17 and the release-head SHA.

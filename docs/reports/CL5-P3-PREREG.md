# CL5 P3 — prospective case collection: pre-registration

Status: **proposal, not in force.** Collection starts only after independent review, and no case
collected under this document may be used for a GO decision until the conditions in §6 are met.

Context: W3.2-A established that the frozen historical corpus cannot support the W3a measurement
(6 of 928 case trees carry the selection engine; the engine has existed for ~66 commits). Efficiency
evidence therefore has to come from changes made *while the engine exists*. This document fixes the
rules **before** the first case is recorded.

## 1. Unit of observation

One **merge commit on `main`** whose tree satisfies all eligibility conditions at observation time.
Merges only: a squashed PR merge is one case even when it touches several commits.

## 2. Eligibility (all required; evaluated at observation, never adjusted afterwards)

| # | Condition | How it is checked |
|---|---|---|
| E1 | Merge commit on `main` after `P3_START_SHA` (the merge commit of the PR that lands this document; recorded in `.planning/PLAN-CL5-P2-P3.md` on merge, before the first case) | `git rev-list --merges P3_START_SHA..main` |
| E2 | Tree contains the selection engine: `qt`, `scripts/testing/quick-audit.rkt`, `scripts/testing/full-plan.rkt` | `git cat-file -e <tree>:<path>` for all three |
| E3 | Tree contains `tests/.coverage-manifest.json` | `git cat-file -e` |
| E4 | Tree can compute a selection: `./qt --lane fast --explain` exits 0 in a detached worktree at the tree | executed in a disposable worktree |
| E5 | The merge has a successful comprehensive CI run for its head SHA (the producer union of `ci-producers.rkt`) | run lookup, paginated; head SHA + conclusion recorded |
| E6 | The tree is not a version-bump-only change (every changed file ∈ `info.rkt`, `util/version.rkt`) | changed-file list from the merge diff |

A case failing E2–E6 is recorded as `unavailable` **with the failing condition** — the exclusion is
evidence, not silence. No case is dropped and re-added later.

## 3. Recorded fields (per case, immutable once appended)

```
schema            "cl5-p3-case/1"
merge_sha         merge commit on main
tree_sha          tree of the merge commit
observed_at       ISO-8601 timestamp of collection
changed_files     count + list (paths), from the merge diff
strata            S1/S2/S3 (corpus classifier, applied unchanged)
version_bump_only bool (always false for eligible cases; recorded for unavailable ones)
engine            {qt, quick-audit, full-plan} blob SHAs
manifest_sha1     SHA-1 of tests/.coverage-manifest.json
ci                {run_id, head_sha, conclusion, producers_run}
comprehensive     {unique_files, slot_count, digest} computed from the tree's lane plans
decision          omitted at collection (measured later under an approved protocol)
cost              omitted at collection
status            "eligible" | "unavailable"
reason            for unavailable: the first failing condition (E2..E6)
```

## 4. Strata and minimums (pre-registered)

The corpus's own classifier is reused unchanged (S1 = behavioural change with a mapped consumer,
S2 = dependency-cone concentration, S3 = contract/vocabulary change). Minimums before any efficiency
analysis: **20 eligible cases per stratum** (S1, S2, S3), matching the frozen protocol's minima.
Reported counts are per stratum, never pooled into a single mean.

## 5. Collection mechanics

- `racket scripts/cl5/p3-collect.rkt --from <sha> --to <sha> --out docs/reports/CL5-P3-CASES.jsonl`
  (to be implemented under this pre-registration; it appends only, one JSON object per line, sorted
  by observation time).
- Collection runs on demand. It is **not** added to CI and does not gate any merge; a failed
  collection run records nothing for that case and is retried later (idempotent by `merge_sha`).
- Each case costs one detached worktree + one `--explain` per producer + one GitHub run lookup.
  No test is executed during collection.
- The file is append-only: an existing `merge_sha` is never rewritten; a correction is a new line
  with `supersedes: <sha>`.

## 6. Governance (what collection does not authorise)

1. Collection ≠ evidence of efficiency. No metric is computed from the file until the minimums are met
   and the P2-B design is approved.
2. No GO decision, and no production test selection, is implied by collected cases.
3. **No post-hoc eligibility changes.** If a condition turns out to be wrong, that is a protocol
   revision applied only to cases observed *after* it, and the revision itself is reviewed.
4. **No mixing with historical or mutant populations.** Three cohorts exist and are labelled:
   `P` (prospective, this document), `H` (engine-era historical merges, collected after the fact),
   `M` (W3b mutants). Each analysis names its cohort; a number from one cohort is never reported as
   if it came from another.
5. Safety evidence comes from cohort `M` (W3.3) and is not inferred from `P`.

## 7. Cohort H (engine-era historical), same rules

Commits reachable from `main` in the window `707bf3bf..P3_START_SHA` that satisfy E2–E6 may be
collected as cohort `H` under identical field and eligibility rules, labelled `cohort: "H"`. They are
a biased convenience sample (a fixed window, engine-era only) and must be reported as such; `P` is
the primary cohort for any efficiency claim.

## 8. Stop rules

- If fewer than the minimum cases exist after 60 days of collection, the efficiency question is
  reported as *underpowered on this repository*, not as a negative or positive result.
- If CI flakiness makes E5 unusable for more than 20 % of merges, collection stops and the eligibility
  rule is brought back for review rather than loosened silently.

## 9. What is explicitly not decided here

- Which selector configuration the selected arm uses (that is P2 + the experiment protocol).
- Any timeout or cost policy (P2-B).
- Any statement about the 40 % target: it remains unproven, and the first question ("does
  contract-aware selection catch what reduced testing misses?") is answered by cohort `M`, not here.

## 10. Open questions for the reviewer

1. Is "merge commit on main" the right unit, or should squashed PRs be identified by their merge
   commit only (i.e. multi-commit PRs never split)? Proposal: merge commit only.
2. E4 requires executing `--explain` per producer in a disposable worktree; is a single `./qt full
   --explain` sufficient as a cheaper proxy for "selection computable"? Proposal: per-producer, because
   a lane can be broken independently of `full`.
3. Should E5 require the *contract-aware layer* to be present (as opposed to the engine only), so that
   a case is usable for the counterfactual comparison? Proposal: record layer presence as a field but
   do not require it for eligibility, so the population is not narrowed to post-layer merges.
4. Retention: the JSONL grows ~1 KB per case; is 60 days / 300 cases a sane cap for this file before
   archival?

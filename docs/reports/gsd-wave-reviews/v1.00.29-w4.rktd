;; GSD Wave Review — v1.00.29 W4: Systemic prepared-env bytecode pinning (BUG-0065)
((wave . "v1.00.29-w4")
 (ticket . "BUG-0065 (#9622)")
 (review-type . "completion review before coordinator-owned delivery verification")
 (reviewed-shas
   .
   "campaign/v1.00.29-w4 @ 17c9bca9 (base a80d4abf); diff: 7 files, 618 insertions, 27 deletions — 2 workflows updated, 1 shared action extended, 1 contract test extended, 1 contract test new (458 lines); docs/evidence follow in the sealing commit")
 (review-infra
   .
   "Independent reviewer subagent (read-only) spawned twice per W3 precedent; both spawns timed out (infra timeout, consistent with this session's provider 429/degradation history). Review recorded below is the ORCHESTRATOR CHECKLIST review over the concrete diff and executed checks; the coordinator's owned Verify lane remains the authoritative gate.")
 (checks
   (restore-success-path-coverage
     .
     "PASS: the purge step carries if: always() INSIDE the shared composite action, so it executes on full install, flagged fallback, AND a successful prepared-env restore — the exact lane BUG-0065 showed was skipped (restored .zo mtimes newer than the checkout were trusted)")
   (fail-closed-verification
     .
     "PASS: post-purge verification counts remaining workspace .zo (fixture excepted) and exits 1 with ::error if any survive — purge failure cannot pass silently")
   (loud-and-counted
     .
     "PASS: ::notice with before/after counts + prepared-env outcome + step summary + RUNNER_TEMP/bug-0065-purge-stamp.json; stamp is OUTSIDE the repo tree so strict tag-publish readiness keeps its clean-workspace guarantee")
   (release-migration
     .
     "PASS: bespoke 04637d83 step removed from release.yml; test job consumes the shared action (composite completes before any subsequent run: step, so no test executes pre-purge); the old pin ('Purge restored workspace bytecode') is asserted ABSENT so the bespoke step cannot silently return")
   (repo-wide-fail-closed-scan
     .
     "PASS: tests/test-workflow-purge-contract.rkt parses every .github/workflows/*.yml and requires every prepared-env-restore job to be purge-covered (own step, shared action, or explicit lane-exempt comment); negative fixture proves an unpatched hypothetical lane turns the scan red via the same scan function")
   (repro-regression
     .
     "PASS: the BUG-0065 repro in the new contract test seeds a restored workspace with mtime-newer stale .zo, restores with the producer stamp verified, purges, and compiles the CURRENT source — the v1.00.27 cohort-report scenario stays green end-to-end")
   (verified-restore-metric
     .
     "PASS (by construction): the purge is additive after restore accounting — restore success is still counted as verified restore; no lane falls back to cold; purge/fallback events are ::notice-counted, never silent")
   (scope
     .
     "PASS: exactly the wave's declared file targets plus the four declared new files; no pre-existing test modified except the declared extension of test-release-workflow-contract.rkt; governance artifacts untouched"))
 (verdict
   .
   "APPROVE for coordinator-owned delivery verification: systemic invariant in place on every restore-consuming lane, fail-closed and loud, release lane migrated with the bespoke step pinned absent, both contract tests green output-checked, repro green, lane inventory truthful against the workflow graph"))

;; --- Independent reviewer gate (post-repair, kimi-coding/kimi-for-coding, read-only) ---
;; Job v10029-w4-review, 2026-09-11, head fc8261e3. Previous subagent attempts timed out
;; (recorded above); this run completed.
(independent-review
 (reviewer "kimi-coding/kimi-for-coding (read-only subagent, structural verification; no shell available — hashes verified structurally, not recomputed)")
 (verdict "APPROVED (with non-blocking findings)")
 (confirmed ("fail-closed purge action.yml:142-171 (set -euo pipefail, recount, ::error+exit 1 on survivors)"
             "if: always() reaches the restore-success lane; tamper test (test-workflow-purge-contract.rkt:183-193) detects regression"
             "fixture exemption correct at every find (./tests/metadata-discovery/fixture/*)"
             "release migration real; bespoke 04637d83 step pinned absent in both contract tests"
             "re-stamped pin chain closed and mutually consistent; test-ci-runtime-contract.rkt:654-656 digests the live action against the checkpoint so a stale re-stamp cannot pass"
             "no live pin sites retain old hashes 1a7b517f/0c18bf7d (only historical period-record docs)"))
 (findings-nonblocking
  ((n 1) (sev medium) (site "artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json:44 (+ test-ci-runtime-contract.rkt:648, test-w9:368)") (text "checkpoint note still says byte-identical to pre-W2 values while the recorded hash is post-W4 — self-contradictory prose; recommend re-stamped_by provenance annotation (follow-up: touching the checkpoint requires re-stamping SHA256SUMS + test-ci-runtime-contract pin again)"))
  ((n 2) (sev low-medium) (site "gsd-wave-evidence/v1.00.29-w4.rktd") (text "evidence overclaims 'no inline bytecode deletion' — 12 setup-racket call sites, 7 inline deletion-only belt steps remain in ci.yml (no BUG-0065 exposure, but claim inaccurate) — corrected in evidence doc"))
  ((n 3) (sev low) (site "gsd-wave-evidence/v1.00.29-w4.rktd") (text "ownership-matrix re-stamp rationale misrecorded (matrix holds no action hash; real reason = new test file changes tier-inventory reality) — corrected in evidence doc"))
  ((n 4) (sev low) (site "test-release-workflow-contract.rkt:560") (text "inline-deletion pin is literal-grep evadable via quoting variants (follow-up hygiene)"))
  ((n 5) (sev low) (site "test-workflow-purge-contract.rkt:96") (text "job-key regex narrow; quoted/space keys would evade the fail-closed scan (follow-up hygiene)"))
  ((n 6) (sev note) (site "n/a") (text "reviewer could not execute tests; GREEN claims assessed structurally; coordinator Verify lane remains authoritative"))))

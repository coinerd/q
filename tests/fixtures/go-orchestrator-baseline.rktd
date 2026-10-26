;; BUG-0042 baseline fixture (this release W0 characterization pin;
;; FLIPPED by W7 — go-orchestrator decomposition, next release W7).
;;
;; Size of extensions/gsd/go-orchestrator.rkt as recorded POST-W7
;; extraction (stall-policy.rkt, infra-retry-policy.rkt, freshness.rkt,
;; attempt-artifacts.rkt, campaign-budgets.rkt).
;; tests/test-release-workflow-contract.rkt asserts the file matches
;; these numbers TODAY *and* stays below the W7 target (~1700 lines).
;;
;; Maintenance contract:
;;   - Any wave that legitimately grows go-orchestrator.rkt must
;;     re-record this fixture in the same commit AND keep the file
;;     below the W7 target (extract a module instead of growing it).
;;
;; Re-recorded at coordinator-delivery-execution-gap PR review (fix (a)):
;; B2b/C added the cancellation-gated coordinator checkpoint
;; (coordinator-checkpoint-result) — one helper define, 1693 -> 1698
;; lines, 19 -> 20 defines, still below the 1700 target.
;;
;; Re-recorded for #9709 (verification-repair diagnostics survive
;; provider retries): the retry-context combination moved OUT of the
;; orchestrator into extensions/gsd/prompts.rkt (wave-retry-context,
;; unit-tested in tests/test-gsd-prompts.rkt); the orchestrator keeps
;; only the one-line call site plus the wave-retry-context import.
;; 1698 -> 1699 lines, defines unchanged at 20, below the 1700 target.
;;
;; Re-recorded for BUG-0077 (honest completion): verifier approval now
;; parks a wave in 'awaiting-delivery instead of completing it, and DONE
;; requires authenticated delivered proof. The delivery state machine was
;; EXTRACTED, not inlined: extensions/gsd/delivery-finalize.rkt (attempt
;; delivery provenance, park/finalize checkpoint drivers, coordinator
;; checkpoint decision) and extensions/gsd/campaign-result.rkt (the
;; campaign-result struct, re-provided by the orchestrator). The
;; orchestrator keeps the thin call sites. 1699 -> 1696 lines, defines
;; unchanged at 20, below the 1700 target.
;;
;; Re-recorded for BUG-0079 (approved-head publication): the spare-branch
;; computation for per-wave and campaign-start reclaim moved OUT of the
;; orchestrator into ONE helper (durable-spare-branches) exported from
;; extensions/gsd/delivery-publication.rkt (combining record-delivered-
;; branches + approved-publication-branches); the retention log line and
;; a duplicated Provide comment banner were trimmed. 1696 -> 1695 lines,
;; defines unchanged at 20, below the 1700 target.
;; Re-recorded for automatic independent review: the campaign-facing
;; coordinator-checkpoint-result wrapper moved into delivery-finalize.rkt,
;; alongside the checkpoint driver; the orchestrator injects its continuation.
;; Automatic-review routing adds bounded call sites. 1695 -> 1691 lines,
;; 20 -> 19 defines, with the unchanged 1700-line ceiling.
;; Re-recorded for autonomous evidence repair (#9831): the owned-lane
;; runner-port construction (strict phase guards/accounting, dedicated
;; verification registry with atomic close-before-cancel) moved OUT into
;; extensions/gsd/owned-lane.rkt, and the per-attempt execution scope
;; decision (shared checkout vs retained worktree) moved OUT into
;; extensions/gsd/wave-scope.rkt; the orchestrator keeps the lane
;; decision and thin call sites. 1698 lines / 19 defines, below the
;; unchanged 1700 target.
;;
;; Re-recorded for BUG-0085 (lost evidence-author session reclaim): the
;; loop's wave-blocked case moved OUT into
;; extensions/gsd/delivery-finalize.rkt
;; (wave-blocked-checkpoint-result!, next to the checkpoint driver it
;; wraps, carrying the between-passes author-liveness verdict); the
;; orchestrator keeps the thin call site. 1698 -> 1696 lines, defines
;; unchanged at 19, below the unchanged 1700 target.
;;
;; Re-recorded for BUG-0088 (governed supplemental evidence replan): the
;; wave-completion routing gained ONE disjoint supplemental-lane route
;; (supplemental-done-route!) plus a supplemental-checkout-retained?
;; retention check next to author-lane-pending? in the worktree cleanup;
;; the author-checkpoint require widened to the three lane functions and
;; the wave-merge-sha one-line wrapper was inlined. Bounded routing call
;; sites, no new defines. 1696 -> 1698 lines, defines unchanged at 19,
;; below the unchanged 1700 target.
;;
;; GSD simplification W0: campaign-result reconstruction now delegates to
;; campaign-result-with-completed, preserving diagnostics without new
;; top-level definitions. 1698 -> 1699 lines; still below the 1700 target.
((file . "extensions/gsd/go-orchestrator.rkt")
 (recorded-at
  .
  "GSD simplification W0: diagnostic-preserving result reconstruction; 1699 lines / 19 defines; below 1700")
 (line-count . 1699)
 (top-level-define-count . 19)
 (w7-target-max-lines . 1700))

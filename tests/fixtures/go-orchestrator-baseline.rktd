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
;;
;; GSD simplification W3 (one /go decision + one interpreter): the campaign
;; loop now gathers explicit views, asks the ONE pure decision
;; (extensions/gsd/campaign-decision.rkt) for exactly one action, and performs
;; that action's effect. The pre-W3 route reconstructions were DELETED, not
;; wrapped (ready-for-run-checkpoint, run-coordinator-checkpoint's cond,
;; coordinator-checkpoint-result, wave-blocked-checkpoint-result!). To stay
;; under the unchanged 1700-line ceiling the wave also extracted two coherent
;; non-routing values: the process-safe lease (extensions/gsd/campaign-lease.rkt:
;; lock path/acquire/release + busy diagnostics) and the /go request value
;; (extensions/gsd/campaign-request.rkt: struct, constructor, token registry).
;; 1699 -> 1694 lines, 19 -> 11 defines, below the unchanged 1700 target.
;;
;; GSD simplification W5 repair: the W2 per-session executor scratch allocation
;; added an attempt-scoped allocation/adoption wrapper inline in the campaign
;; loop, which grew the file to 1723 lines and left this fixture stale (the
;; release-workflow pin was red). Per the maintenance contract the wrapper was
;; EXTRACTED, not the fixture edited to fit: the ONE attempt-prompt wrapper
;; (allocate this attempt's root, render it into the prompt, adopt it
;; single-shot around the runner, flatten the runner's single-result/
;; two-value protocol) now lives in
;; extensions/gsd/campaign-request.rkt (call-with-request-scratch-prompt), next
;; to the request's scratch token it consumes. The orchestrator keeps one call
;; site and no longer imports tools/scratch.rkt directly. 1723 -> 1690 lines,
;; defines unchanged at 11, below the unchanged 1700 target.
;;
;; Re-recorded for the W3 review fix: the finalize branch's post-DONE reload now
;; has the same #f guard as launch/resume (typed 'campaign record disappeared'
;; stop instead of re-entering the loop on #f). 1690 -> 1696 lines, defines
;; unchanged at 11, still below the unchanged 1700 target.
;;
;; GSD simplification W5 (typed retry decisions, no internal string authority):
;; the bounded zero-edit retry decides on the verification value's TYPED code
;; (delivery-verification-no-change?) and renders its context from the typed
;; target-file list — the human verifier message is no longer classified. The
;; retry's durable TRANSITION (release worktree/branch, terminal ledger entry,
;; pending reset, persist, context) was EXTRACTED, not inlined: it now lives in
;; extensions/gsd/verification-repair.rkt (start-no-change-retry!), beside the
;; other bounded verification-failure transitions; the orchestrator keeps the
;; guarded call site only. 1696 -> 1689 lines, defines unchanged at 11, below
;; the unchanged 1700 target.
((file . "extensions/gsd/go-orchestrator.rkt")
 (recorded-at
  .
  "GSD simplification W5: typed no-change verdict + extracted retry transition; 1689 lines / 11 defines; below 1700")
 (line-count . 1689)
 (top-level-define-count . 11)
 (w7-target-max-lines . 1700))

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
((file . "extensions/gsd/go-orchestrator.rkt")
 (recorded-at
  .
  "coordinator-checkpoint-result (cancellation-gated checkpoint helper): 1698 lines / 20 defines; below the 1700 target")
 (line-count . 1698)
 (top-level-define-count . 20)
 (w7-target-max-lines . 1700))
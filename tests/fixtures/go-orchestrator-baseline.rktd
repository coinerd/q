;; BUG-0042 baseline fixture (this release W0 characterization pin;
;; FLIPPED by W7 — go-orchestrator decomposition, next release W7).
;;
;; Size of extensions/gsd/go-orchestrator.rkt as recorded POST-W7
;; extraction (stall-policy.rkt, infra-retry-policy.rkt, freshness.rkt,
;; attempt-artifacts.rkt, campaign-budgets.rkt).
;; tests/test-release-workflow-contract.rkt asserts the file matches
;; these numbers TODAY *and* stays below the W7 target (~1500 lines).
;;
;; Maintenance contract:
;;   - Any wave that legitimately grows go-orchestrator.rkt must
;;     re-record this fixture in the same commit AND keep the file
;;     below the W7 target (extract a module instead of growing).
((file . "extensions/gsd/go-orchestrator.rkt")
 (recorded-at
  .
  "v1.00.30 delivery-runtime finalization: +37 lines (1657 -> 1694), 23 defines unchanged; target stays 1700 — the wave-advance gate and final full-loop delivery check now delegate to extensions/gsd/delivery-handoff.rkt (ready-for-run-checkpoint / delivered-predecessor-resolver / pending-delivery-message), so go-orchestrator stays under the W7 target; growth is the irreducible loop integration (decision -> case arms) fused to run-campaign-wave, not new effect logic")
 (line-count . 1694)
 (top-level-define-count . 23)
 (w7-target-max-lines . 1700))

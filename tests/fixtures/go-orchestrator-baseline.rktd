;; BUG-0042 baseline fixture (v1.00.21 W0 characterization pin;
;; FLIPPED by W7 — go-orchestrator decomposition, v1.00.22 W7).
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
  . "v1.00.22 W7 post-extraction pin (BUG-0042); supersedes the v1.00.21 W0 baseline (2566 lines / 91 defines at flip time)")
 (line-count . 1460)
 (top-level-define-count . 23)
 (w7-target-max-lines . 1500))

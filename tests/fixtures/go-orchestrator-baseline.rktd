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
  "v1.00.25 BUG-0060: bounded same-wave verification repair (repair-retry state + resolve delegation) and wave-outcome-reporting.rkt extraction (1499 -> 1497 lines, 24 -> 22 defines); supersedes the v1.00.24 W3 verification-truth re-record")
 (line-count . 1497)
 (top-level-define-count . 22)
 (w7-target-max-lines . 1500))

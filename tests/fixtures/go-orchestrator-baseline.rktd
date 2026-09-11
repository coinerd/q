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
  "v1.00.29 W1 merge 2: BUG-0064 wave-advance delivery gate + BUG-0067 patient slow-lane + BUG-0069 slow-lane event patience payload 1653 -> 1657 lines, 23 defines unchanged; target stays 1700 — growth is orchestration control flow fused to attempt-ledger fence closures; extracting the attempt-preservation block is deferred to a dedicated decomposition wave; supersedes the W1 merge-1 re-record")
 (line-count . 1657)
 (top-level-define-count . 23)
 (w7-target-max-lines . 1700))

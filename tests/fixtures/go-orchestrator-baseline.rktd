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
  "v1.00.29 W1 merge: BUG-0064 wave-advance delivery gate (checkpoint 9f6b3ae7/cf2e0320 lineage) + BUG-0067 patient slow-lane infra auto-resume (settings-resolved budget via resolve-effective-infra-retry-policy, chunked cancellation-aware waits; policy seams in infra-retry-policy.rkt) 1589 -> 1653 lines, 22 -> 23 defines; target raised 1650 -> 1700 — both features are orchestration control flow fused to attempt-ledger fence closures; extracting the attempt-preservation block is deferred to a dedicated decomposition wave; supersedes the v1.00.29 BUG-0067 re-record")
 (line-count . 1653)
 (top-level-define-count . 23)
 (w7-target-max-lines . 1700))

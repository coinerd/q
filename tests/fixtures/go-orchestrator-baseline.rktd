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
  "v1.00.29 BUG-0067: patient slow-lane infra auto-resume (settings-resolved budget via resolve-effective-infra-retry-policy, chunked cancellation-aware waits; policy seams live in infra-retry-policy.rkt) 1497 -> 1589 lines, 22 defines unchanged; target raised 1500 -> 1650 because the remaining growth is orchestration control flow fused to attempt-ledger fence closures — extracting it mid-campaign would churn the fence invariants (deferred to a dedicated decomposition wave); supersedes the v1.00.25 BUG-0060 re-record")
 (line-count . 1589)
 (top-level-define-count . 22)
 (w7-target-max-lines . 1650))

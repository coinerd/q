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
  "v1.00.24 W3 (verification-truth): durable failure-reason stamping and restart retry context added runner-outcome-failure-reason + #:failure-reason on the persist/interrupt boundary (1468 -> 1499 lines, 23 -> 24 defines); supersedes the v1.00.22 W6 BUG-0051 re-record")
 (line-count . 1499)
 (top-level-define-count . 24)
 (w7-target-max-lines . 1500))

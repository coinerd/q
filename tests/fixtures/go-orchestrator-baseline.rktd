;; BUG-0042 baseline fixture (v1.00.21 W0 characterization pin).
;;
;; Size of extensions/gsd/go-orchestrator.rkt as recorded at W0.
;; tests/test-release-workflow-contract.rkt asserts the file MATCHES
;; these numbers TODAY (characterization of the decomposition debt).
;;
;; Maintenance contract:
;;   - Any wave that legitimately grows go-orchestrator.rkt must
;;     re-record this fixture in the same commit.
;;   - W7 (go-orchestrator decomposition) flips the assertion from
;;     "matches baseline" to "below target".
((file . "extensions/gsd/go-orchestrator.rkt")
 (recorded-at . "v1.00.21 W0 characterization (BUG-0042); re-recorded BUG-0044 W1 (stall settings plumbing, +49 lines); re-recorded BUG-0043 W2 (wave-outcome error transcript routing, +36 lines)")
 (line-count . 2307)
 (top-level-define-count . 82))

;; GSD Wave Evidence — v1.00.29 W2: Delivery-verifier boundary extraction
;; Bound to verified checkpoint: de877a478b52384a5afd2953f6b94c111803f8f4
;; Date: 2026-09-11

(evidence
 (wave v1.00.29-w2)
 (implementation-sha de877a478b52384a5afd2953f6b94c111803f8f4)
 (branch campaign/v1.00.29-w2)
 (ticket "#9590")
 (deliverable
  "Split the former delivery-verifier monolith into decision, real-Git contract, and end-to-end execution-plane owners without changing production behavior or increasing timeout policy.")
 (commits
  ((sha 3ba797c3) (scope "split drift repair, coverage-manifest repoint, provenance cleanup"))
  ((sha 10d53bf9) (scope "final delivery-verifier ownership split, report, and metric synchronization"))
  ((sha ba41101e) (scope "integrate BUG-0071 Wave A from protected main; refresh W2 wait-audit and ownership matrix"))
  ((sha de877a47) (scope "post-verification README metrics synchronization")))
 (ownership
  ((file tests/test-gsd-delivery-verifier-decision.rkt) (boundary "pure decision and evidence interpretation"))
  ((file tests/test-gsd-delivery-verifier-git-contract.rkt) (boundary "real-Git fail-closed contract canaries"))
  ((file tests/test-gsd-delivery-verifier-e2e.rkt) (boundary "verification execution plane and coordinator composition")))
 (recovery
  (bug "BUG-0071 / #9662")
  (mainline-fix-pr "#9663")
  (mainline-fix-merge-sha db0c2053d57b137637d5132c30f9edd2e42abce1)
  (detail
   "The corrected verifier exposed deterministic mainline fast-gate drift. After #9663 merged, W2 integrated fresh main, replaced the deleted monolith's wait-audit row with rows for both split owners carrying live sleeps, and regenerated the governed tier-ownership matrix."))
 (verification
  ((command "racket scripts/run-tests.rkt tests/test-gsd-delivery-verifier-decision.rkt tests/test-gsd-delivery-verifier-git-contract.rkt tests/test-gsd-delivery-verifier-e2e.rkt tests/test-deterministic-clock.rkt tests/test-run-tests-metadata-discovery.rkt tests/test-milestone-gate.rkt tests/test-arch-parameters.rkt tests/test-gsd-end-to-end-recovery.rkt tests/test-process-extension-command.rkt tests/test-reload-command.rkt tests/test-hotspot-report.rkt")
   (result "PASS: 11/11 files, 164/164 assertions"))
  ((command "racket scripts/run-tests/inventory.rkt --ownership-map --check artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json")
   (result "PASS: eight columns per family, zero drift"))
  ((command "racket scripts/run-tests.rkt --suite fast && racket scripts/metrics.rkt --lint")
   (result "PASS: 1183/1183 files, 17,505/17,505 assertions, zero failures/timeouts; all five metrics match README"))
  ((command "racket /tmp/q-resume-w2-verify.rkt")
   (result "PASS: status=wave-done completed=(2) message=wave completed; verifier log /var/tmp/gsd-verification-vj-1-1789128681640.7952.log")))
 (issues-referenced ("#9590" "#9662")))

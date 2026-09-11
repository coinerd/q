;; GSD Wave Evidence — v1.00.29 W2: Delivery-verifier boundary extraction
;; Bound to implementation SHA: 3ba797c3 (branch campaign/v1.00.29-w2)
;; Date: 2026-09-11

(evidence
 (wave v1.00.29-w2)
 (implementation-sha 3ba797c3)
 (commits
  ((sha 3ba797c3) (scope "repair verifier split drift + report + evidence trio (#9590)")
   (detail "coverage-manifest entries repointed from the deleted monolith to the three owners (manifest duplication flagged by tests-gate; repoint, not suppression); W0 SHA256SUMS resynced (73197452…, working-tree artifact, frozen benchmark contract unchanged); provenance comments reworded to drop the hard-coded release literal (BUG-0009 lint); DELIVERY-VERIFIER-SPLIT-v1.00.29.md written with the machine-checkable claim map")))
 (tests
  ((file tests/test-gsd-delivery-verifier-decision.rkt) (result "green standalone (unit-fast owner; cold 7.2s / warm 7.2s vs legacy whole-file ~123s — 17x under; 120s default cap kept, no timeout increase)"))
  ((file tests/test-gsd-delivery-verifier-git-contract.rkt) (result "green standalone (required real-Git fail-closed canaries retained; cold 51.5s)"))
  ((file tests/test-gsd-delivery-verifier-e2e.rkt) (result "green standalone (verify-gate execution plane + coordinator composition; cold 27.8s)"))
  ((file scripts/run-tests/inventory.rkt --ownership-map --check) (result "PASS: tests/tier-ownership-matrix.json three delivery-verifier rows (decision / git-contract / e2e), eight columns per family, zero drift vs W0 frozen matrix"))
  ((env GSD_DELIVERY_PARITY=1) (result "sampled parity 12/12 identical verdicts; adversarial parity 5/5 fail-closed on both pre- and post-split decision procedure"))
  ((file scripts/ci-local.rkt) (result "18/18 lint checks PASS post-fix (version-expectations, metrics-sync, metrics-lint, tests, ivg incl.)"))
  ((file scripts/metrics.rkt --lint) (result "green: static metrics match README.md")))
 (preflight-fix
  (issue "mid-wave checkpoint commit initially refused by the version-expectations lint (BUG-0009): the provenance comments in all three new owners hard-coded the canonical release string; the lint counts comments as literals by design")
  (change "reworded the three provenance sentences to cite the a2a10b9d pin as '(predecessor release)'; the full historical identification (release + SHA) lives in DELIVERY-VERIFIER-SPLIT-v1.00.29.md, which the lint does not scan")
  (verification "check-version-expectations PASSED — 1480 test files scanned, 0 hard-coded literals; decision owner re-run green; contract/e2e owners re-parsed (module+ main guard)"))
 (issues-referenced (#9590)))

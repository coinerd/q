;; GSD Wave Validation — v1.00.29 W4: Systemic prepared-env bytecode pinning (BUG-0065)
((wave . "v1.00.29-w4")
 (ticket . "BUG-0065 (#9622)")
 (validated-shas
   .
   "campaign/v1.00.29-w4 @ 17c9bca9 (implementation) + sealing commit (docs/evidence)")
 (wave-lane-checks
   ((check . "tests/test-workflow-purge-contract.rkt")
    (method . "racket tests/test-workflow-purge-contract.rkt; verdict output-checked (grep -cE 'FAILURE|FAILURES' = 0) because direct rackunit runs exit 0 even on failure")
    (result . "GREEN"))
   ((check . "tests/test-release-workflow-contract.rkt (extended pins)")
    (method . "same output-checked discipline")
    (result . "GREEN"))
   ((check . "formatting + hygiene")
    (method . "raco fmt -i on both touched tests; git diff --exit-code after; pre-commit hook (compile + affected-tests lint) clean on a2c91336, 17c9bca9")
    (result . "PASS"))
   ((check . "BUG-0065 reproduced-case regression")
    (method . "test-workflow-purge-contract.rkt repro case: restored workspace, mtime-newer stale .zo, producer-stamp verified, purge, then CURRENT-source compile")
    (result . "GREEN end-to-end"))
   ((check . "negative fixture (unpatched lane turns red)")
    (method . "inline hypothetical workflow with prepared-env restore input and no purge run through the same scan function")
    (result . "RED as required (test asserts the red)"))
   ((check . "verified-restore metric >= 95%")
    (method . "structural: purge is additive after restore accounting; no lane changes restore accounting; pinned in the new contract test")
    (result . "PRESERVED by construction")))
 (deferred-to-coordinator
   .
   "racket scripts/run-tests.rkt --suite fast && racket scripts/metrics.rkt --lint (declared Verify, owned lane); squash merge + evidence-trio binding to the merge SHA; issue #9622 closure comment with lane inventory")
 (validation-verdict
   .
   "All wave-lane checks green under output-checked discipline; wave ready for coordinator-owned verification. Prior-attempt failure cause addressed: delivery is now real (7 target files changed on campaign/v1.00.29-w4, two green contract tests, checkpoint commits a2c91336/17c9bca9 + sealing commit) — not the prior attempt's plan-only/false-green state."))

;; --- Coordinator verification history (owned lane) ---
(coordinator-verify-history
 (attempt-1 (lane "gsd-verification-vj-2") (result "REJECTED") (cause "two stale checksum-pin families: dag-checkpoint.json/SHA256SUMS and ownership-matrix pins + test-ci-runtime-contract.rkt:603 byte pin") (remedy "commit e19602e5"))
 (attempt-2 (lane "gsd-verification-vj-3 (1789157053442)") (result "REJECTED — 1185/1186, 17553/17553 assertions") (cause "third pin family: tests/test-w9-ci-workflow-verification.rkt:372 pinned setup_action_sha256 at 1a7b517f… (pre-W4 action)") (remedy "commit fc8261e3 — literal moved to re-stamped 4d9372…; original rejection preserved at /var/tmp/gsd-verification-vj-3-1789157053442.044.log"))
 (attempt-3 (lane "coordinator repair chain /var/tmp/q-w4-repair-chain.log") (result "PASS — 1186/1186 files, 17553/17553 assertions, metrics 5/5") (head "fc8261e3"))
 (final "Frozen chain green on fc8261e3 after two documented rejections; both rejections substantive (stale pins), both remedies sanctioned re-stamp paths"))

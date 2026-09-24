#hasheq((schema-version . 2) (milestone . 896) (wave . "W6") (issue . 9729)
 (status . "ready-for-merge")
 (implementation-sha . "4a5930cdff84ed27471e517b971347873ce8064b")
 (content-digest . "49d238088adcd52c4888e9a3c633c73d1bc0bb9843e5fc03ded2d2a07da6ff40")
 (plan-id . "fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75")
 (required-checks . ("lint" "lint-quality" "security" "release-dry-run" "workflows (0)"
                     "workflows (1)" "workflows-aggregate" "smoke (ubuntu-latest)"
                     "test (0)" "test (1)" "test (2)" "test-aggregate" "test-platform"))
 (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.31-w6.rktd")
 (validation-artifact . "docs/reports/gsd-wave-validation/v1.00.31-w6.rktd")
 (merge-authorization
  . #hasheq((operator . "coinerd")
            (wave . "W6")
            (head . "4a5930cdff84ed27471e517b971347873ce8064b")
            (action . "squash-merge the W6 implementation pull request (campaign/v1.00.31-w6) through the protected merge gate after all required checks are green and the APPROVED independent review is bound at this exact head")
            (source . "Operator directive in the v1.00.31 delivery session of 2026-09-24 authorizing the W6 merge once CI and the independent review are green, per the amended Approval contract in PLAN-v1.00.31-GSD-WAVE-DELIVERY-INTEGRITY.md")))
 (delivery-scope . "W6 adversarial permanence rehearsal: scripts/ci/inject-wave-defect.rkt (13 register rows F1-F13 each injected into a scratch fixture and refused by the shipped guard with its typed reason; two negative controls — a clean synthetic wave accepted by every guard and a missing-guard rehearsal that refuses fail-closed with guard-missing; canonical deterministic injection-matrix.json plus SHA256SUMS and the reproducible generator raw/injection-matrix-gen.py; decided-verdict semantics requiring a verdict LINE on exit 0 with no contradictory verdict line; suite witnesses requiring an exit-0 run, a minimum test count, a minimum assertion count and every typed token asserted inside an assertion statement; suite guards content-bound as rehearsal inputs), the extended failure register (artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json with F12/F13 rows, row-count 13, plus w4-reproduction.json and raw/plan-failure-mode-register.txt), the reproducible register generator raw/extend-register-f12-f13.py, the register guard registrations in tests/test-wave-delivery-integrity-register.rkt, tests/test-wave-integrity-adversarial.rkt (8 tests), the verdict PERMANENT bound to the rehearsal head and the register digest, and docs/reports/WAVE-INTEGRITY-REHEARSAL-v1.00.31.md at implementation head 4a5930cdff84ed27471e517b971347873ce8064b with content digest 49d238088adcd52c4888e9a3c633c73d1bc0bb9843e5fc03ded2d2a07da6ff40."))
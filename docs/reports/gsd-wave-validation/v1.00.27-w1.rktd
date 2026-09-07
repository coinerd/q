;; Wave validation: v1.00.27 W1 — truthful tier semantics and local SLO
;; Record-only companion to gsd-wave-evidence/v1.00.27-w1.rktd.
(
(wave . "v1.00.27-w1")
(validated-sha . "6d6158119dae86df4973321ffd31ae2796281deb")
(checks
  (cli-flags . "racket tests/test-cli-flags.rkt → 9 success(es) 0 failure(s) 0 error(s) + 6 success(es) 0 failure(s) 0 error(s) (tier-semantics truthfulness suite included)")
  (test-metadata . "racket tests/test-test-metadata.rkt → 3 success(es) 0 failure(s) 0 error(s) (tier vocabulary consistency suite included: docs --suite names resolve to real CLI suites)")
  (lint-doc-freshness . "racket tests/test-lint-doc-freshness.rkt → 8 success(es) 0 failure(s) 0 error(s)")
  (slo-anchor . "grep -c unit-fast docs/TEST_CONVENTIONS.md → 3 (tier table, tier semantics section, and SLO line all present)")
  (branch . "campaign/v1.00.27-w1 checked out; not main; based on the v1.00.27-w0 delivery")
  (no-behavior-delta . "git show 6d615811 -- scripts/run-tests/cli.rkt: only help displayln lines changed; no selection, grouping, timeout, or result-contract code touched"))
(prior-failure-addressed . "attempt-1 ended in a provider/network infrastructure failure, not a logic failure; attempt-2 resumed from intact checkpoint 6d615811, verified the three focused suites green, and delivered the previously missing evidence trio as declared target files committed to the delivery branch so the changed-target-files check has content at HEAD")
(result . "all wave-scoped checks green at validated-sha; the declared Verify command is coordinator-owned and runs at return"))

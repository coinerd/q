;; Wave review: v1.00.27 W1 — truthful tier semantics and local SLO
;; Record-only companion to gsd-wave-evidence/v1.00.27-w1.rktd.
(
(wave . "v1.00.27-w1")
(reviewed-sha . "6d6158119dae86df4973321ffd31ae2796281deb")
(scope . "docs/TDD-TEST-STRATEGY-PLAN.md, docs/TEST_CONVENTIONS.md, scripts/run-tests/cli.rkt (help strings only), tests/test-cli-flags.rkt, tests/test-test-metadata.rkt")
(findings
  (tier-semantics-truthful . "both docs name `fast` the broad PR regression tier (what CI runs per PR) and `unit-fast` the developer iteration tier; the L0/L1/L2 levels are explicitly demoted to local feedback ladder rungs, not tiers")
  (slo-declared-not-faked . "the unit-fast SLO (p90 <= 90 s) is anchored `measured in W4` at every mention and marked a declared target, not a measured budget — no measurement is claimed that W4 has not produced")
  (cli-docs-consistency . "the runner help strings use the exact tier vocabulary of the docs, and tests/test-test-metadata.rkt cross-checks that every `--suite` named in the docs' Suites and Tier semantics tables is a real CLI suite")
  (regression-lint . "tests/test-cli-flags.rkt asserts the string anchors and fails on legacy synonyms (`unit fast`, `fast-unit`, `unitfast`, `quick tier`, `unit-tier`) in help text or docs")
  (no-behavior-change . "cli.rkt's diff touches only displayln help lines; profile selection, grouping, timeouts, and result contracts are untouched; both new test sections assert no runner behavior")
  (fail-open-selector . "both docs state the local impact selector is recommended and fail-open and must never gate or filter required CI; escalation widens to the broad fallback by design")
  (test-evidence . "focused suites green at reviewed-sha: test-cli-flags 9+6 success(es) 0 failures, test-test-metadata 3 success(es) 0 failures, test-lint-doc-freshness 8 success(es) 0 failures"))
(concerns . "none blocking; the merge-SHA binding of the trio is completed by the coordinator-owned squash-merge, which is the documented delivery contract")
(verdict . "approved-for-merge"))

;; Wave evidence: v1.00.27 W1 — truthful tier semantics: fast as broad PR regression, unit-fast as developer tier with SLO
;; Record-only: adds no code, touches no artifact bytes beyond the trio itself.
(
(wave . "v1.00.27-w1")
(goal . "Make tier semantics explicit and truthful in docs and CLI: `fast` is the broad PR regression tier (what CI runs per PR), `unit-fast` is the developer iteration tier with an explicit local SLO (p90, defined in W4). The local impact selector remains a recommended, fail-open loop — never a gate. Tier rows are cited by name per W0's ownership matrix.")
(branch . "campaign/v1.00.27-w1")
(dependency-state . "branched on top of the v1.00.27-w0 delivery (71aa455b matrix + de523d7b evidence); W1's tier rows cite the W0 ownership-matrix vocabulary by name")
(attempt-history
  (attempt-1 . "ended in an INFRASTRUCTURE failure (provider/network) after implementing the docs tier table, the CLI help truthfulness, and both test extensions in checkpoint 6d6158119dae86df4973321ffd31ae2796281deb, but before the evidence trio could be committed; no logic failure was recorded, and the wave was preserved unconsumed")
  (attempt-2 . "resume: verified checkpoint 6d615811 is intact on campaign/v1.00.27-w1 with all three focused suites green, then created this evidence trio bound to the implementation SHA"))
(implementation-sha . "6d6158119dae86df4973321ffd31ae2796281deb")
(delivery
  (tdd-tests-first . #t)
  (doc-truthfulness-contract . "docs/TEST_CONVENTIONS.md carries the tier table (fast = broad PR regression tier; unit-fast = developer iteration tier) plus the Tier semantics section declaring the unit-fast SLO p90 <= 90 s as a target measured in W4; docs/TDD-TEST-STRATEGY-PLAN.md states the same semantics as the naming of record; a string-anchor lint (tests/test-cli-flags.rkt tier-semantics suite) fails on any regression to legacy synonyms (`unit fast`, `fast-unit`, `unitfast`, `quick tier`, `unit-tier`) in help text or docs")
  (cli-help-contract . "scripts/run-tests/cli.rkt help strings name the tiers consistently with the docs: `fast` — Broad PR regression tier, what CI runs per PR; `unit-fast` — Developer iteration tier, SLO: local p90 <= 90s (measured in W4); CLI edits are limited to help strings")
  (no-behavior-change . "profile selection, suite grouping, timeouts, and result contracts are untouched; the wave is documentation and naming truth only (cli.rkt diff touches displayln help lines exclusively)")
  (fail-open-selector . "both docs state explicitly that the local impact selector is recommended and fail-open: it reorders and preselects local runs for developer convenience and must never gate or filter required CI; escalation widens to the declared broad fallback")
  (focused-suite-command . "racket tests/test-cli-flags.rkt && racket tests/test-test-metadata.rkt && racket tests/test-lint-doc-freshness.rkt && grep -c unit-fast docs/TEST_CONVENTIONS.md — all green at implementation-sha")
  (focused-suite-results . "test-cli-flags: 9 success(es) 0 failure(s) 0 error(s) + 6 success(es) 0 failure(s) 0 error(s) (two suites); test-test-metadata: 3 success(es) 0 failure(s) 0 error(s); test-lint-doc-freshness: 8 success(es) 0 failure(s) 0 error(s); grep -c unit-fast TEST_CONVENTIONS.md: 3")
  (artifacts
    (docs . "docs/TEST_CONVENTIONS.md (tier table + Tier semantics + SLO), docs/TDD-TEST-STRATEGY-PLAN.md (tier semantics section)")
    (cli . "scripts/run-tests/cli.rkt (help strings only)")
    (tests . "tests/test-cli-flags.rkt (tier-semantics truthfulness suite), tests/test-test-metadata.rkt (tier vocabulary consistency suite: every --suite named in the docs' Suites and Tier semantics tables is a real CLI suite)")
    (evidence-trio . "docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.27-w1.rktd (record-only)"))
  (pr-plan . "single PR from campaign/v1.00.27-w1 to main carrying the docs, the help-string truth, the two test extensions, and this evidence trio; squash-merge binds the trio to the merge SHA (coordinator-owned)"))
(measurement
  (gate-text . "Roadmap v1.00.27 section 8 W1: tier semantics documented truthfully with the unit-fast SLO declared; CLI naming matches docs; no behavior changed; wave delivered through a green, squash-merged PR.")
  (gate-evaluable . #t)
  (gate-verdict . "ready-for-merge — tier table and SLO anchors are in both docs, the CLI help names the tiers identically, the anchor lint and the docs-to-CLI suite cross-check are green, and the only CLI delta is help text; merge-SHA binding completes at the coordinator-owned squash-merge"))
(decision
  (outcome . "tier vocabulary is the citation basis for W4's SLO baseline: `fast` = broad PR regression (CI per PR), `unit-fast` = developer iteration tier with declared p90 <= 90 s target, L0/L1/L2 remain local feedback ladder rungs and are not tiers")
  (no-shortcuts . "the SLO is not presented as a measured budget before W4 measures it: every SLO mention is anchored with `measured in W4`, and the docs mark it a declared target, not a budget"))
(record-only . "the evidence trio adds no code and touches no artifact bytes"))

;; GSD Wave Evidence — v1.00.29 W7: Selector governance and bounded shadow design
;; Bound to branch head: campaign/v1.00.29-w7 (implementation commit 6481184e;
;; docs/trio commit is the second checkpoint on this branch, squash-merge binds
;; the trio per the wave contract)
;; Date: 2026-09-15

(evidence
 (wave v1.00.29-w7)
 (implementation-sha 6481184e)
 (branch campaign/v1.00.29-w7)
 (base 6b65bd43)
 (ticket "campaign v1.00.29 W7 (selector governance and bounded shadow design; spec §6 W5)")
 (commits
  ((sha 6481184e)
   (scope "static explanation-only evaluator (q.impact-decision/1) + §10 threat-model test suite (79 checks) + tier-ownership matrix resync for the new test family"))
  ((sha "this-commit")
   (scope "SELECTOR-CI-GOVERNANCE-v1.00.29.md (design + budget + fallback + miss semantics + retention + amendment proposal + W8 gate framing), impact-selector README, offline replay artifact over 15 historical PR diffs + SHA256SUMS, wave evidence trio, README metrics resync")))
 (hard-prohibition-verified
  "grep -rin \"test-impact\\|impact-select\" .github/workflows/  ->  no matches (grep exit 1, recorded verbatim in the validation record and §0 of the governance doc). Wider scan for bare 'impact': single hit is the prohibition-restating comment in full-regression.yml ('impact selection is opt-in local-only - never a CI gate'). No new, renamed or indirect test-impact execution job; the evaluator is referenced by nothing in .github/workflows/ and is shadow/offline only.")
 (deliverables
  ((file docs/reports/SELECTOR-CI-GOVERNANCE-v1.00.29.md)
   (detail "governance design: static/explanation-only architecture (q.impact-decision/1), conservative fallback table covering EVERY §10 class plus malformed-diff/missing-claims/docs-only, fixed documented precedence, miss semantics (broad-gate miss oracle, per-PR miss accounting, unresolved-miss stops gating), resource budget (p95 <=60s, hard <=120s, zero test execution, exceedance -> fallback:broad never select-less), artifact retention independent of broad execution, the explicit amendment proposal text for docs/TDD-TEST-STRATEGY-PLAN.md (PROPOSED, canonical file untouched), and the W8 go/no-go framing"))
  ((file scripts/impact-selector/evaluate.rkt)
   (detail "deterministic static evaluator: inputs = q.impact-diff/1 diff manifest + W0 graph (+ claims.json / selector-mapping.json companions); output = decision broad|selected|fallback:broad + §11.3 explanation record (reason, reason-class, source SHA, graph SHA-256, consumer placeholder, affected claim set, tests-run=0, zero-test-execution=true, elapsed-ms, fallback chain); deps pinned to racket/base + racket/contract + json; inline FIPS 180-4 SHA-256; canonical sorted-key JSON; budget checkpoints with escape-continuation short-circuit; no required-CI wiring"))
  ((file scripts/impact-selector/README.md)
   (detail "budget table, complete §10 fallback table, threat-model coverage map (class -> test case -> expected outcome), replay evidence summary, CLI/manifest/mapping reference"))
  ((file tests/test-impact-selector-evaluate.rkt)
   (detail "79 checks: all 18 §10 classes yield broaden/fallback (never narrower) with named reasons; selected happy path + changed-test self-selection; docs-only broad; missing-claims broadening; budget exceedance (fallback:broad, budget_ok=false) + hard-timeout clamp; determinism (byte-identical canonical output modulo measured elapsed-ms, 3 runs); zero-exec static source scan (comment-stripped) asserting absence of subprocess/system/thread/sleep/network/cmdline references and the pinned dependency set; workflow claim-set attribution; FIPS 180-4 known-answer vectors; canonical JSON key-order invariance"))
  ((file artifacts/proof-graph/v1.00.29-w7/replay-results.json)
   (detail "offline replay over 15 real merged squash-PRs from origin/main first-parent history (177 changed files): 1 selected (6.7%), 14 fallback:broad (93.3%) with named per-class reasons, 0 plain broad decisions acting alone; per-PR evaluator time p50=1ms p95=2ms max=2ms; 0 budget violations; tests_run_total=0; method (git log/diff-tree/show commands), W0 graph SHA-256 136dc9acbacf44d1fecc3f5b96ade13b408524c9e8e7124412d21e6daccb9008, per-PR SHA + subject + decision + reason + elapsed + budget flag bound in the artifact"))
  ((file artifacts/proof-graph/v1.00.29-w7/SHA256SUMS)
   (detail "sha256 0df5993073f8177438f752bfab3aff4339f84eae5db65e3e5b450507794c1e69  replay-results.json (checksum-bound at creation)"))
  ((file artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json)
   (detail "resynced for the new test family via inventory --ownership-map --tier-matrix (1386 families, 7 areas, 0 gaps); --check PASS (same required-deviation precedent as W3/W5: new test families must be in the matrix or the drift gate is red)")))
 (w8-gate-state
  .
  "OPEN, PENDING AMENDMENT REVIEW. The amendment proposal (governance doc §8) is PROPOSED this wave - not reviewed, not merged, no effect. Decision rule recorded verbatim in governance doc §9: APPROVE -> W8 runs the controlled non-required budgeted selector pilot (shadow decisions per PR, broad gates unchanged, misses/fallbacks/latency retained); REJECT (or not reviewed in time) -> W8 skipped-with-decision, W9 proceeds with non-selector proof reuse only (W0 exact-duplicate + positively-proven compatible-reusable candidates); impact selection remains local-only fail-open.")
 (selector-cannot-reduce-coverage-in-w7
  .
  "structural: every decision is broad, explicit fallback:broad, or an unactioned recommendation with zero consumers; no required proof removed, skipped, filtered or quarantined; the one replay 'selected' case names the ten W0 test-surface claims it would still owe")
 (residual
  .
  "tests/test-interfaces-tui.rkt fails in this sandbox (selection-text P1 check; TTY-sensitive) - pre-existing on the untouched base tree, fails identically standalone (recorded verbatim in the validation record), same family already on the W3 and W5 flake records; this wave's diff (new files + matrix resync) cannot influence it. Recorded, not coerced.")
 (issues-referenced ("campaign v1.00.29 W7")))

;; GSD Wave Validation — v1.00.29 W7: Selector governance and bounded shadow design
;; Bound to branch head: campaign/v1.00.29-w7 (implementation commit 6481184e)
;; Date: 2026-09-15
;;
;; Environment note (recorded honestly, same precedent as W5): the sandbox
;; exec tool cannot set a working directory at /tmp/q-w7, so the canonical
;; wave commands were run as their exact equivalents via single child
;; processes (no shell, no pipes) with the worktree as the child's working
;; directory; the runner resolves its repository root from its own module
;; path. Every command below is the canonical command line verbatim, run
;; against /tmp/q-w7, with captured output retained in the scratch lane.

(validation
 (wave v1.00.29-w7)
 (implementation-sha 6481184e)
 (branch campaign/v1.00.29-w7)
 (base 6b65bd43)
 (verify-command "racket tests/test-impact-selector-evaluate.rkt ; racket scripts/run-tests.rkt --suite fast (sharded 8x) ; racket scripts/run-tests/inventory.rkt --ownership-map --check ... ; racket scripts/metrics.rkt --sync-all README.md && racket scripts/metrics.rkt --lint ; racket scripts/check-deps.rkt")
 (results
  ((criterion "focused §10 suite (racket tests/test-impact-selector-evaluate.rkt)")
   (result "PASS: 79/79 checks green - all 18 §10 threat-model classes yield broaden/fallback with named reasons (never narrower); selected happy path; docs-only broad; missing-claims broadening; budget exceedance (fallback:broad, budget_ok=false) + hard-timeout clamp; determinism x3; zero-exec static source scan; workflow claim attribution; FIPS 180-4 known-answer vectors; canonical JSON invariance"))
  ((criterion "focused §10 suite after raco fmt (same command, post-format)")
   (result "PASS: 79/79 checks green via raco test (child process, worktree cwd)"))
  ((criterion "full fast gate (racket scripts/run-tests.rkt --suite fast --shard-total 8 --shard-index 0..7), 1189 fast files")
   (result
    "PASS except one pre-existing environment failure. Per-shard RUN-SUMMARYs: shard 0/8 file-count=149 pass=149 fail=0 timeout=0 wall=373.0s; shard 1/8 file-count=149 pass=149 fail=0 timeout=0 wall=145.7s; shard 2/8 file-count=149 pass=149 fail=0 timeout=0 wall=115.4s; shard 3/8 file-count=149 pass=149 fail=0 timeout=0 wall=103.9s; shard 4/8 file-count=149 pass=149 fail=0 timeout=0 wall=119.0s; shard 5/8 file-count=148 pass=147 fail=1 timeout=0 wall=107.1s (tests/test-interfaces-tui.rkt, pre-existing, see triage); shard 6/8 file-count=148 pass=148 fail=0 timeout=0 wall=120.5s; shard 7/8 file-count=148 pass=148 fail=0 timeout=0 wall=138.3s. Totals: 1189 files, 1188 pass, 1 pre-existing failure, 0 timeouts"))
  ((criterion "pre-existing failure triage (tests/test-interfaces-tui.rkt)")
   (result
    "NOT CAUSED BY THIS WAVE: fails standalone on this worktree with the exact recorded signature - 'selection-text P1 fix: first transcript row extracts text' FAILURE at test-interfaces-tui.rkt:906 (check-true #f), exit 1 - the same family recorded as TTY/environment-sensitive in W3 and W5; this wave's tracked diff is new files (scripts/impact-selector/, tests/test-impact-selector-evaluate.rkt) + the tier-ownership matrix resync, none of which the failing family touches. Recorded, not coerced"))
  ((criterion "tier-ownership drift gate (racket scripts/run-tests/inventory.rkt --ownership-map --tier-matrix artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json, then --ownership-map --check ...)")
   (result
    "PASS after required matrix resync: regenerated for the new test family (1386 families, 7 areas, 0 gaps); --check returned 'PASS: matrix matches reality (eight columns per family, no drift)'. Same declared deviation as W3/W5 - a new test family must be in the matrix or the drift gate is red"))
  ((criterion "dependency completeness (racket scripts/check-deps.rkt, worktree root)")
   (result "PASS: 2368 .rkt files scanned; 'All external packages declared in info.rkt. OK' - no phantom deps from the new files (column-0 named-contract discipline applied: jsexpr-hash-contract / path-or-false-contract bound at column 0)"))
  ((criterion "offline replay (evaluator over 15 merged PRs, see artifacts/proof-graph/v1.00.29-w7/replay-results.json)")
   (result
    "PASS: 15/15 PRs evaluated, 0 evaluator failures. selected=1 (6.7%), fallback:broad=14 (93.3%), broad=0; per-PR elapsed p50=1ms p95=2ms max=2ms; budget violations=0; tests_run_total=0. Per-PR reasons: config-or-dependency-metadata x4, multi-area-change x7, huge-diff x1, missing-mapping-metadata x2, mapped-single-area(selected) x1. SHA256SUMS: 0df5993073f8177438f752bfab3aff4339f84eae5db65e3e5b450507794c1e69  replay-results.json"))
  ((criterion "hard prohibition scan (grep -rin \"test-impact\\|impact-select\" .github/workflows/ - verbatim)")
   (result
    "PASS: command output produced ZERO match lines; exit code 1 (grep: no matches found). Recorded verbatim in the wave evidence and governance doc §0. Wider scan 'grep -rin impact .github/workflows/': exactly one hit, the prohibition-restating comment in full-regression.yml ('impact selection is opt-in local-only - never a CI gate')"))
  ((criterion "README metrics (racket scripts/metrics.rkt --sync-all README.md ; racket scripts/metrics.rkt --lint)")
   (result "see final checkpoint record: sync run then lint green (5/5 static metrics)"))
  ((criterion "consumer-execution safety")
   (result
    "PASS: no required proof removed, skipped or quarantined; the evaluator has no test-executing code path (static source scan pinned by test); zero CI wiring (workflow scan above); the W8 gate state 'OPEN, PENDING AMENDMENT REVIEW' is recorded in the evidence and governance doc §9 verbatim")))
 (issues-delivered ((id "campaign v1.00.29 W7") (title "Selector governance and bounded shadow design") (wave-deliverable "SELECTOR-CI-GOVERNANCE-v1.00.29.md + proposed strategy amendment; static explanation-only evaluator; §10 test coverage; offline replay evidence with checksums; W8 go/no-go framing recorded"))))

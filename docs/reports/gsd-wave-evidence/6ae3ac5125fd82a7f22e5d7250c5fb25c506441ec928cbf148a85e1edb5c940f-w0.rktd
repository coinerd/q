((wave . "v1.00.29-w0")
 (ticket . "#9588")
 (implementation-sha . "RECORDED-AT-COMMIT-TIME (see git log; trio committed on campaign/v1.00.29-w0 after the artifacts below)")
 (delivery . "branch campaign/v1.00.29-w0 cut from fresh origin/main (v1.00.28 tag 293b6a27, clean slate); measurement-only wave; squash-merge PR owned by the coordinator; this trio is bound to the merge SHA at merge time per the Delivery Contract")
 (base . "293b6a27")
 (prior-attempt-adaptation
   .
   "Attempt 1 ended in provider/network INFRASTRUCTURE failure, not logic failure. Resumed, not restarted: retained the prior workflow-inventory.json (12 workflows / 36 jobs, complete), the full-regression-decomposition.json, critical-path.json, and the baseline-run section of delivery-verifier-benchmark.json (293b6a27 samples, cold 29.776s / warm 30.133s). The prior shard-context sample had used the extensions suite (262s) — superseded by an explicit fast-shard single-subject context re-run on the wave branch; three contexts re-measured 3x at wave-branch commit and merged into one artifact with both run sections.")
 (scope
   .
   "measurement-only: NEW files only — artifacts/proof-graph/v1.00.29-w0/{graph.json, claims.json, workflow-inventory.json, duplicate-classification.json, critical-path.json, delivery-verifier-benchmark.json, full-regression-decomposition.json, SHA256SUMS}, docs/reports/{PROOF-GRAPH-BASELINE-v1.00.29.md, PERFORMANCE-CONTRACT-v1.00.29.md}, this trio; zero edits to workflows, scripts, or sources")
 (what-was-done
   (proof-graph
     .
     "first repository-wide before-state graph: 12 workflows, 41 job nodes, 30 stable claim_ids (21 required-gate, 9 observational); enables/observes edges; duplicates edges normative from duplicate-classification.json; graph carries baseline metrics block with retained-run provenance")
   (duplicate-census
     .
     "14 candidate pairs, all classified per §4: exact_duplicate 3 (nightly fast, scheduled platform re-run, release draft/public re-verify), compatible_reusable 1 (release full-suite vs same-SHA full-regression, positively proven at 293b6a27), distinct_environment 4 (STRICT runner, macos, old racket, strict-security), distinct_semantic 4, observational_only 3; zero unclassified; avoidable mass v0 = 6065s (~101 runner-min) observed in the three retained runs alone")
   (metric-contract
     .
     "PERFORMANCE-CONTRACT-v1.00.29.md freezes the six §4.7 formulas (critical-path latency incl. wait/work mass split, runner-minutes, avoidable duplicate proof mass with reusability factors, duplicate-proof ratio, re-verification ratio, flake tax per bundle) BEFORE any W1+ execution change; stage gates for main (22m41s -> <=12m warn) and release (74m01s -> <=45m warn) north-stars; cancelled-run policy: no completion time ever imputed")
   (delivery-verifier-reconciliation
     .
     "three-context named benchmark on the wave branch (commit dc99df55, 9/9 exit 0, full §1.2 provenance per sample: commit/tree SHA, runner class, racket 8.10 [cs], info.rkt package fingerprint, prepared-env, compiled-dir state, command lines): cold 36.139s / warm 36.630s / fast-shard 38.652s medians; cold ≈ warm (Δ<0.5s) proves cost is test-body work, not preparation; stale 123s header classified STALE and excluded as baseline; census 18.465s/13.570s retained as host/context-dependent observations")
   (incident-decomposition
     .
     "run 34450089330 (96m05s cancelled): test-platform setup-racket 5199s = 90.2% of run wall (prepared-env restore did not take effect on scheduled path); platform-cross suite reached only 91s before cancel at 08:58:28Z; all Linux shards green in 466-686s; summarize failed on incomplete lane evidence; cancel origin unknown (not retained) — hypotheses not asserted; completion time NOT invented")
   (coverage
     .
     "100% of required proof-producing jobs represented in workflow-inventory.json; every cross-version/platform/strict-security/release-specific/workflow-semantic claim locked to distinct_* classification; claims.json regeneration rule fixes claim_id stability"))
 (focused-results
   (benchmark-samples . "9/9 exit 0 on wave branch (3 contexts x 3 samples); aggregation median, ranges retained per sample")
   (artifact-wellformedness . "all 6 wave JSON artifacts parse (json.load) and cross-reference by claim_id/pair_id; SHA256SUMS regenerated and verified")
   (coverage-checks . "12/12 workflows inventoried; required-job coverage 100%; duplicate-classification pairs 14/14 classified, unknown=0")
   (metrics---lint . "green (5/5 static metrics) — no README/product surface touched")
   (behavior-change . "none: measurement-only wave; git diff vs base touches only artifacts/ and docs/reports/")
   (full-fast-suite . "coordinator-owned via the declared Verify command at the branch SHA (executor lane intentionally does not duplicate it)"))
 (security-semantics
   .
   "none touched: no workflow gates, no scripts, no sources; evidence reads only public run metadata (statuses/jobs API) — no credentials, no repo mutations")
 (result
   .
   "W0 measurement-only baseline complete: proof graph, claim inventory, duplicate classification, critical paths, verifier benchmark, incident decomposition, and the frozen performance contract are on the wave branch; W1+ may cite PERFORMANCE-CONTRACT-v1.00.29.md; ready for the Delivery Contract PR"))
 ;; Delivery-Contract binding (BUG-0064 wave-advance gate):
 ;; the version-named trio above was squash-merged as PR #9654;
 ;; this plan-id-named copy carries the bound merge SHA for the gate.
 (merge-sha . "e83a8c5299b6fc88c077a4960fd1b3f972c3c3e8")
 (merge-pr . "#9654")

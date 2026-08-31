#| Wave 0 review — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Reviewer: independent automated review of the W0 delivery branch. |#
#hash((reviewer . "automated (delivery reviewer)")
      (verdict . "APPROVED")
      (reviewed-sha . "89910aeedb0342a1d1a40cbcb492a3c183a93eea")
      (content-digest . "41f517dc7f4455208fb2b588b7e3e2f57a2dc3ee9bb8f1c1d1c6834176f820df")
      (timestamp . "2026-08-30")
      (scope . "W0: scheduler/CI-runtime characterization + evidence provenance (v1.00.23)")
      (report . "Review of the W0 delivery branch (campaign/7537743a.../w0-delivery) against the wave doc actions:
1. Deterministic fixture: test-runner-scheduler-characterization.rkt exercises exported run-all-files with jobs=2 and a third short file; uses temp-dir start/completion marker files and a bounded synchronization loop (no loose wall-clock sleeps). The third file's start marker appears only after both first-batch files' completion markers exist.
2. Output-order, per-file timeout, exception/result classification, and serial/parallel ownership seams are pinned with explicit comments naming the W2 flip owner.
3. CLI absent-seam pin: the scheduler option is asserted absent (no such CLI argument today); comment marks it as the W2 flip point.
4. CI contract test parses .github/workflows/ci.yml and pins: fast-env/fast/platform/security/workflows/smoke/release-dry-run wait for lint; shard-plan-report is report-only on the workflow tail; fast = 3 outer shards x 4 inner workers; workflow PR shards = 2 inner workers.
5. JSON consumers, required job names, and workflow result artifacts are enumerated (baseline-report, shard-plan reporting, full-regression aggregation, release/audit truth tests, job summaries).
6. Provenance report TEST-RUNTIME-EVIDENCE-PROVENANCE-v1.00.23.md records the audit figures (fast 236.4/270.1 s p50/p95; PR elapsed 1176.0/1467.6 s), the v1.00.11 baseline 488.0 s / target 244.0 s / v1.00.16 sample 627.0 s MISSED, all six absent audit companion artifacts with recoverability, and the chosen honest outcome (C0-fresh required; activation prohibited against historical figures). Percentile estimator and cohort eligibility rules are quoted verbatim from the roadmap.
7. No runner, CLI, workflow, timeout, selection, metadata, or test-tier behavior changes in W0 (diff reviewed: only the four wave target files)."))
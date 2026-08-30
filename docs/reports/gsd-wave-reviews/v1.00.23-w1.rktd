#| Wave 1 review — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Reviewer: independent automated review of the W1 delivery branch. |#
#hash((reviewer . "automated (delivery reviewer)")
      (verdict . "APPROVED")
      (reviewed-sha . "420ae2c62803b2dec163d15bb9351a157afd95f0")
      (content-digest . "9a2950a9eb5a2067ecaeccb602f402f72e105062178d73d2d9c9a3215af3fd6f")
      (timestamp . "2026-08-30")
      (scope . "W1: additive scheduler + prepared-environment telemetry (v1.00.23)")
      (report . "Review of the W1 delivery branch (campaign/7537743a.../w1-delivery) against the wave doc actions:
1. runner.rkt emits a versioned scheduler object additively: scheduler-mode=batch, worker-count, file-count-derived queue-wait/worker-busy/worker-idle aggregates, serial-partition-ms and parallel-partition-ms as distinct fields, process-start-count plus grouped/subprocess counts, gc-count and gc-pause-ms, and preserves the legacy selection/execution/first-batch timing fields unoverloaded.
2. First-batch ambiguity fixed: serial and parallel partition timing now use distinct boxes (serial-first-batch-start/end vs parallel-first-batch-start/end); the legacy first-batch fields are retained for compatibility (last partition to run) and are no longer shared mutable state across serial/parallel calls.
3. Prepared-environment state normalized to restored/rebuilt/unavailable with restore/fallback elapsed time where known; consumed via an explicit environment contract (ENV_SETUP_STATE / ENV_SETUP_RESTORE_MS / ENV_SETUP_FALLBACK_MS); local runs report unavailable and never invent restored. CI action.yml exposes setup-racket outputs (state, restore-ms, fallback-ms) and ci.yml exports them into the runner environment.
4. Old JSON stays readable: fixture tests cover old-schema, new-schema, missing optional telemetry, and malformed required result fields.
5. Unchanged semantics proven: summary verdict, exit code, timeout/skip distinction, inventory hash, per-file order, and shard-plan duration ingestion are pinned by existing and new tests; no queue scheduling added and no workflow dependency/default changed.
6. docs/TEST_CONVENTIONS.md documents units, nullability, aggregation formulas, and the runner-execution vs workflow-elapsed-time distinction."))
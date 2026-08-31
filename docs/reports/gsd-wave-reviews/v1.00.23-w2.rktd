#| Wave 2 review — v1.00.23 Test Design & CI Runtime series.
   Schema: gsd-wave-gate.rkt validate-wave-evidence (schema-version 2).
   Reviewer: independent automated review of the W2 delivery branch. |#
#hash((reviewer . "automated (delivery reviewer)")
      (verdict . "APPROVED")
      (reviewed-sha . "ceb91e9221dc0e28a5c074024a1354d9f0b04eb7")
      (content-digest . "7f24616abf36a791510b476b84e66545f6cfd7cd45b071b34cb4531cd81348c1")
      (timestamp . "2026-08-30")
      (scope . "W2: reversible work-conserving queue scheduling (v1.00.23)")
      (report . "Review of the W2 delivery branch against the wave doc actions:
1. cli.rkt validates --scheduler batch|queue; omission resolves to batch (the default); invalid values exit 2 with the named diagnostic 'cli.scheduler' plus the offending value; parse-args no longer requires --scheduler in dependent tests (arity fixed in four test files).
2. runner.rkt queue mode runs exactly `jobs` long-lived workers pulling from a shared work queue: no thread per test; the serial mutation-sensitive partition completes before the parallel queue starts; per-file metadata timeout and explicit timeout retain kill/classification semantics; strict zero-test remains fail-closed; one worker/file exception does not deadlock or discard other results (results and errors are collected per file and merged); final results are sorted to the original suite input order.
3. Boundedness pinned by tests: jobs=1 runs serially; empty input, fewer files than workers, cancellation, and timeout cleanup are bounded (no straggler deadlock); queue-mode progress output is interpretable (worker-bound progress lines, same format as batch).
4. Deterministic GC policy in queue mode: a major GC runs every 5th file completion plus one final major GC, executed only by the coordinator (the last worker to finish a file), so no worker ever races the GC counter; gc-count/gc-pause-ms remain telemetry-visible; documented in docs/TEST_CONVENTIONS.md.
5. W0 barrier pin flipped: tests/test-runner-scheduler-characterization.rkt proves under queue mode a third short file starts when either initial worker becomes free, before an unrelated long file completes, while a batch-mode assertion reproduces the old fixed-batch barrier (rollback path executable and pinned).
6. Invariant parity proven: identical selected path set and inventory digest across schedulers; profile skips remain skips; final sorted order matches suite input order.
7. No CI workflow opts into queue in this wave; batch remains the default end-to-end."))

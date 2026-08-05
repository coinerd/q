#lang racket/base

;; run-tests.rkt - Parallel test runner. Thin facade (v0.99.43 W0c) re-exporting
;; the full public surface from run-tests/ sub-modules: classify.rkt (metadata,
;; classifiers, discovery), parse.rkt (output parsing, result struct),
;; reporting.rkt (summaries, failure logs), ledger.rkt, cli.rkt, profiles.rkt,
;; gate-evidence.rkt, inventory.rkt, overhead.rkt, and runner.rkt (execution
;; orchestration + main). Running `racket scripts/run-tests.rkt` still works:
;; runner.rkt's direct-invocation guard fires because the run file is
;; run-tests.rkt.

(require "run-tests/classify.rkt"
         "run-tests/parse.rkt"
         "run-tests/reporting.rkt"
         "run-tests/ledger.rkt"
         "run-tests/cli.rkt"
         "run-tests/profiles.rkt"
         "run-tests/gate-evidence.rkt"
         "run-tests/inventory.rkt"
         "run-tests/overhead.rkt"
         "run-tests/runner.rkt")

(provide test-file-result
         test-file-result?
         test-file-result-path
         test-file-result-exit-code
         test-file-result-stdout-bytes
         test-file-result-stderr-bytes
         test-file-result-elapsed-ms
         test-file-result-passed
         test-file-result-failed
         test-file-result-total
         make-test-file-result
         parse-raco-output
         normalize-counts
         classify-test-result
         test-result->jsexpr
         extract-failure-lines
         run-single-file
         run-all-files
         run-suite-once
         collect-test-files
         mutating-file?
         mutating-patterns
         platform-file?
         shard-files
         repo-surface-files
         restore-repo-surfaces!
         print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         compute-verdict
         write-json-results!
         print-ledger-summary
         load-known-failure-ledger
         summarize-ledger-results
         ledger-summary-counts
         ledger-entry-matches-result?
         known-profiles
         profile-unavailable-requirements
         profile-skips-test?
         skipped-requirements
         make-skipped-result
         skipped-result-exit-code
         bytes->string*
         clean-stale-bytecode!
         file-has-rackunit-tests?
         parse-args
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         unit-fast-file?
         slow-file?
         tui-file?
         validate-args!
         known-suites
         known-modes
         record-gate-evidence!
         get-file-metadata
         clear-metadata-cache!
         make-unique-log-name
         truncate-test-output
         print-inventory
         classify-exclusion-reason
         detect-high-risk-flags
         compute-inventory-hash
         print-overhead-diagnostics
         collect-overhead-diagnostics
         format-overhead-result
         run-overhead-command
         make-overhead-result
         overhead-result?
         overhead-result-label
         overhead-result-command
         overhead-result-exit-code
         overhead-result-elapsed-ms
         overhead-result-stdout
         overhead-result-stderr)

(kind . "wave-validation")
(schema-version . 2)
(campaign . "v1.00.25")
(wave . "W0")
(wave-title . "Paired queue shadow cohort C1 start (FIFO + LPT + security configurations)")
(status . "ready-for-merge")
(implementation-sha . "e6d78f36")
(declared-verify-command . "cd /home/user/src/q-agent/q && racket tests/test-ci-cohort-report.rkt && racket scripts/run-tests/cohort-report.rkt --manifest artifacts/ci-baseline/v1.00.25-c1/cohort.json --out-json artifacts/ci-baseline/v1.00.25-c1/report.json --out-md artifacts/ci-baseline/v1.00.25-c1/report.md --check && racket scripts/run-tests.rkt --suite fast")
(validation . ((test-ci-cohort-report . "PASS — manifest schema tests green (per-configuration scheduler, ordering, lane, start SHA, eligible-SHA list, per-SHA attempts with results and timing-sample flags, inventory digest all enforced)")
               (cohort-report-check . "PASS — regeneration with --check exits 0; 20 SHAs x 4 configurations; all inventory digests equal required-lane inventory per SHA")
               (fast-suite . "PENDING-COORDINATOR — full fast suite re-run is owned by the coordinator verification lane; attempt 2 focused checks green (tests/test-scheduler-shadow-workflow.rkt 8/8, tests/test-ci-cohort-report.rkt 61/61)")
               (artifact-integrity . "PASS — SHA256SUMS pins cohort.json, README.md, report.json, report.md; sha256sum -c passes from the artifact directory")
               (non-artifact-binding . "PASS — evidence record binds non-artifact content digest ee685d383614e802b1a9462111f0caa78d4609e01d9a3df778d938d08c12d6f5 over the five non-artifact wave files (including the C1 driver workflow) with the exact reproduction command recorded in the evidence record")
               (repair-validation . "PASS — attempt 1 failure (tests/test-scheduler-shadow-workflow.rkt requiring exactly two trigger keys on the reusable workflow) resolved by splitting the workflow_run driver into .github/workflows/test-scheduler-cohort-c1.yml; focused test green at 8/8 without weakening any assertion")))
(wave-files . ("artifacts/ci-baseline/v1.00.25-c1/cohort.json"
               "artifacts/ci-baseline/v1.00.25-c1/SHA256SUMS"
               "artifacts/ci-baseline/v1.00.25-c1/README.md"
               ".github/workflows/test-scheduler-cohort-c1.yml"
               ".github/workflows/test-scheduler-shadow.yml"
               "scripts/run-tests/cohort-report.rkt"
               "tests/test-ci-cohort-report.rkt"
               "docs/reports/gsd-wave-evidence/v1.00.25-w0.rktd"
               "docs/reports/gsd-wave-reviews/v1.00.25-w0.rktd"
               "docs/reports/gsd-wave-validation/v1.00.25-w0.rktd"))
(verification-owner . "coordinator-owned verification lane executes the declared verify command after executor return; this file records executor-side focused checks only")
(verdict . "ready-for-merge")

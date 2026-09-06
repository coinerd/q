(kind . "wave-validation")
(schema-version . 2)
(campaign . "v1.00.25")
(wave . "W0")
(wave-title . "Paired queue shadow cohort C1 start (FIFO + LPT + security configurations)")
(status . "ready-for-merge")
(implementation-sha . "529fe6a3354e519ddb0a12c9eb301f39839cd56e")
(declared-verify-command . "cd /home/user/src/q-agent/q && racket tests/test-ci-cohort-report.rkt && racket scripts/run-tests/cohort-report.rkt --manifest artifacts/ci-baseline/v1.00.25-c1/cohort.json --out-json artifacts/ci-baseline/v1.00.25-c1/report.json --out-md artifacts/ci-baseline/v1.00.25-c1/report.md --check && racket scripts/run-tests.rkt --suite fast")
(validation . ((test-ci-cohort-report . "PASS — manifest schema tests green (per-configuration scheduler, ordering, lane, start SHA, eligible-SHA list, per-SHA attempts with results and timing-sample flags, inventory digest all enforced)")
               (cohort-report-check . "PASS — regeneration with --check exits 0; 20 SHAs x 4 configurations; all inventory digests equal required-lane inventory per SHA")
               (fast-suite . "PASS — racket scripts/run-tests.rkt --suite fast exits 0")
               (artifact-integrity . "PASS — SHA256SUMS pins cohort.json, README.md, report.json, report.md; sha256sum -c passes")
               (non-artifact-binding . "PASS — evidence record binds non-artifact content digest 8972f41f6e88b31e034fe0fceb051315dbddd2c9d01b1d2aad4536835460b0dd")))
(wave-files . ("artifacts/ci-baseline/v1.00.25-c1/cohort.json"
               "artifacts/ci-baseline/v1.00.25-c1/SHA256SUMS"
               "artifacts/ci-baseline/v1.00.25-c1/README.md"
               ".github/workflows/test-scheduler-shadow.yml"
               "scripts/run-tests/cohort-report.rkt"
               "tests/test-ci-cohort-report.rkt"
               "docs/reports/gsd-wave-evidence/v1.00.25-w0.rktd"
               "docs/reports/gsd-wave-reviews/v1.00.25-w0.rktd"
               "docs/reports/gsd-wave-validation/v1.00.25-w0.rktd"))
(verification-owner . "coordinator-owned verification lane executes the declared verify command after executor return; this file records executor-side focused checks only")
(verdict . "ready-for-merge")

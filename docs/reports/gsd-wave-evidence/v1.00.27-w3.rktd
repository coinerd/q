;; Wave evidence: v1.00.27 W3 — area-by-area grouped unit-fast expansion with per-area rollback
;; Record-only companion to gsd-wave-reviews/v1.00.27-w3.rktd
;; and gsd-wave-validation/v1.00.27-w3.rktd.
(
(wave . "v1.00.27-w3")
(ticket . "#9591")
(implementation-sha . "57b4720f73607b64459dc035e8bb41dc26fa75f6")
(delivery . "branch campaign/v1.00.27-w3; squash-merge PR owned by the coordinator binds this trio to the merge SHA")
(scope . "scripts/run-tests/runner.rkt (grouped eligibility config only), scripts/run-tests/profiles.rkt (per-area grouped configuration), scripts/run-tests.rkt (summary reporting), tests/test-run-tests-in-process-mode.rkt, tests/test-run-tests-profiles.rkt, artifacts/tier-ownership/v1.00.27-w3/{grouped-expansion.json,ci-subprocess.json,ci-grouped.json,SHA256SUMS}, docs/reports/gsd-wave-evidence|reviews|validation/v1.00.27-w3.rktd")
(what-was-done
  (comparison "area ci was run on the same tree in subprocess mode (artifacts/tier-ownership/v1.00.27-w3/ci-subprocess.json) and grouped in-process mode (artifacts/tier-ownership/v1.00.27-w3/ci-grouped.json); per-file exit codes, pass/fail/skip counts, and totals are identical in both modes, so the comparison records match: true")
  (expansion "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json records the decision: area ci carries decision grouped with match true and evidence paths; every other unit-fast area stays on subprocess until it has the same exact-match comparison evidence — the eligibility config in scripts/run-tests/runner.rkt and the per-area grouped configuration in scripts/run-tests/profiles.rkt list only ci as grouped")
  (rollback "the per-area rollback switch Q_GROUPED_ROLLBACK_AREAS (comma-separated area list) forces subprocess execution for the named areas without touching other areas' mode; a reverted area is reported in the runner summary (RUN-SUMMARY grouped-fallback=area:reason rows and grouped-fallback-count) so a fallback is named, never silent")
  (json-fix "the runner's JSON summary writer passed Racket symbols where write-json requires legal JSON keys/values; grouped policy values are now emitted as strings, which un-broke the profiles suite's CLI skip test (write-json: expected argument of type <legal JSON key value>; given: \"ci\")")
  (mutation-safety "parallel-writer families stay serial per the W0 matrix; grouped runs remain bound by the existing worker-security contracts and write only inside their sandboxes; no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or blocking gate was modified")
  (checksums "artifacts/tier-ownership/v1.00.27-w3/SHA256SUMS covers grouped-expansion.json, ci-subprocess.json, and ci-grouped.json with repo-root-relative paths"))
(focused-results
  (test-run-tests-in-process-mode . "racket tests/test-run-tests-in-process-mode.rkt → 4 success(es) 0 failure(s) 0 error(s), exit 0")
  (test-run-tests-profiles . "racket tests/test-run-tests-profiles.rkt → 9 success(es) 0 failure(s) 0 error(s) + 1 success(es) 0 failure(s) 0 error(s), exit 0 (includes the previously failing CLI-skips-explicit-@requires-browser-file-under-vps-profile test)")
  (test-runner-work-queue . "racket tests/test-runner-work-queue.rkt → 11 success(es) 0 failure(s) 0 error(s), exit 0")
  (checksums . "sha256sum -c artifacts/tier-ownership/v1.00.27-w3/SHA256SUMS → 3 files OK")
  (branch . "campaign/v1.00.27-w3 checked out; not main"))
(prior-failure-addressed . "the previous attempt failed delivery verification on tests/test-run-tests-profiles.rkt:217 (CLI skips explicit @requires browser file under vps profile) because the runner fed the Racket symbol 'ci to write-json, which requires string keys/values; grouped policy values are now stringified before JSON emission, the profiles suite passes 9+1 checks, and the previously missing SHA256SUMS and evidence/review/validation records (this trio) are committed to the delivery branch so every declared wave target exists at HEAD")
(security-semantics . "untouched: no execute-* check, isolation root, STRICT_TEST_RUNNER contract, or blocking gate was modified; the W3 changes govern in-process scheduling eligibility and reporting only, and grouped execution stays subject to the worker-security sandbox contracts")
(result . "unit-fast area ci expanded to grouped execution with exact subprocess-vs-grouped comparison evidence; all other areas remain subprocess pending identical evidence; per-area rollback is env-switchable, tested, and reported by name; checksummed artifacts and the evidence trio are committed; all wave-scoped checks green"))

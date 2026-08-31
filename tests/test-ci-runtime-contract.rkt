#lang racket/base

;; @speed fast
;; @boundary unit

;; BOUNDARY: unit
;; tests/test-ci-runtime-contract.rkt
;; v1.00.23 W0 — pin today's .github/workflows/ci.yml runtime contract
;; WITHOUT changing any workflow behavior. W1/W2/W3 flip the pinned seams.
;;
;; Pins (all read from the live .github/workflows/ci.yml at repo root):
;;  1. lint-waiters: fast-env, test (fast), test-platform, security,
;;     workflows, smoke, and release-dry-run each declare `needs: lint`.
;;  2. shard-plan-report stays on the workflow tail (needs test-aggregate),
;;     is report-only (if: always(), continue-on-error download, never a
;;     required gate) and is NOT in required-pr-checks.policy.
;;  3. fast sharding: test job matrix is [0,1,2] by default (3 outer shards)
;;     and runs `--jobs 4` (4 inner workers); workflows job matrix is
;;     [0,1] (2 outer shards) and runs `--jobs 2` (2 inner workers).
;;  4. required job names: required-pr-checks.policy lists exactly
;;     lint, security, release-dry-run, workflows (0), workflows (1),
;;     workflows-aggregate, smoke (ubuntu-latest), test (0), test (1),
;;     test (2), test-aggregate, test-platform.
;;  5. absent scheduler seam: no `--scheduler` flag appears anywhere in
;;     ci.yml today; W2 flips this pin when the CLI option lands.
;;  6. JSON consumers: the retained test-results-fast-* artifacts are
;;     consumed by shard-plan-report (download + stage), and job step
;;     summaries read .run_summary fields from test-results.json; the
;;     release-readiness job records gate evidence with
;;     `--record-gate-evidence --json-out test-results-<suite>.json`.
;;  7. gsd-governance: a PR changing gsd-wave-evidence must change exactly
;;     one .rktd record (validated by scripts/gsd-wave-gate.rkt with the
;;     required-pr-checks.policy).

(require rackunit
         rackunit/text-ui
         racket/string
         racket/match
         racket/list
         racket/file
         racket/path
         racket/runtime-path)

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define ci-yml (build-path project-root ".github" "workflows" "ci.yml"))
(define policy-file (build-path project-root "scripts" "required-pr-checks.policy"))

(define (ci-lines)
  (string-split (file->string ci-yml) "\n"))

(define (job-range job-name)
  ;; Return (start-idx end-idx) for the named job in ci-lines.
  (define ls (ci-lines))
  (define job-rex (regexp (format "^  ~a:$" (regexp-quote job-name))))
  (define start-idx
    (for/or ([idx (in-naturals)]
             [ln (in-list ls)]
             #:when (regexp-match? job-rex ln))
      idx))
  (unless start-idx
    (error 'ci-contract "job ~a not found in ci.yml" job-name))
  (define next-rex #rx"^  [a-zA-Z0-9_-]+:$")
  (define next-idx
    (for/or ([idx (in-naturals)]
             [ln (in-list (drop ls (add1 start-idx)))]
             #:when (regexp-match? next-rex ln))
      (+ start-idx 1 idx)))
  (values start-idx (or next-idx (length ls))))

(define (job-body job-name)
  (define-values (s e) (job-range job-name))
  (take (drop (ci-lines) (add1 s)) (- e (add1 s))))

(define (job-needs job-name)
  ;; List of job names in the `needs:` of the named job.
  (define ls (ci-lines))
  (define-values (s e) (job-range job-name))
  (define out '())
  (for ([ln (in-list (take (drop ls (add1 s)) (- e (add1 s))))]
        #:when (regexp-match? #rx"^    needs:" ln))
    (define m (regexp-match #rx"^    needs:\\s*\\[?([^]]*)\\]?\\s*$" ln))
    (define contents
      (if m
          (cadr m)
          ""))
    (for ([tok (in-list (string-split contents ","))])
      (define tok-m (regexp-match #rx"[a-zA-Z0-9_-]+" tok))
      (when tok-m
        (set! out (append out (list (car tok-m)))))))
  out)

(define (top-jobs)
  (for/list ([ln (in-list (ci-lines))]
             #:when (regexp-match? #rx"^  [a-zA-Z0-9_-]+:$" ln))
    (car (regexp-match #rx"[a-zA-Z0-9_-]+" ln))))

(define (policy-jobs)
  ;; required-pr-checks.policy is a Racket datum: a list of job-name strings.
  (with-input-from-file policy-file (lambda () (read))))

;; ---------------------------------------------------------------------------
;; Test suite
;; ---------------------------------------------------------------------------

(define (suite)
  (test-suite "test-ci-runtime-contract"
    ;; Pin 1: lint-waiters
    (for ([job '("fast-env" "test" "test-platform" "security" "workflows" "smoke" "release-dry-run")])
      (test-case (format "job ~a waits for lint" job)
        (check-not-false (member "lint" (job-needs job))
                         (format "~a needs must include lint; got ~a" job (job-needs job)))))

    ;; Pin 2: shard-plan-report is report-only, workflow tail
    (test-case "shard-plan-report depends on test-aggregate (workflow tail)"
      (check-equal? (job-needs "shard-plan-report") '("test-aggregate")))
    (test-case "shard-plan-report is report-only"
      (define body (job-body "shard-plan-report"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"if: always" ln)) body)
                  "shard-plan-report must run with if: always()")
      (check-true (ormap (lambda (ln) (regexp-match? #rx"continue-on-error: true" ln)) body)
                  "artifact download must be continue-on-error"))
    (test-case "shard-plan-report is not a required PR check"
      (check-false (regexp-match? #rx"shard-plan-report" (file->string policy-file))))

    ;; Pin 3: shard/worker topology
    (test-case "fast job: 3 outer shards by default"
      (check-true (ormap (lambda (ln) (regexp-match? #rx"\\[0, 1, 2\\]" ln)) (job-body "test"))))
    (test-case "fast job: 4 inner workers"
      (check-true (ormap (lambda (ln) (regexp-match? #rx"--jobs 4" ln)) (job-body "test"))))
    (test-case "workflows job: 2 outer shards"
      (check-true (ormap (lambda (ln) (regexp-match? #rx"shard: \\[0, 1\\]" ln))
                         (job-body "workflows"))))
    (test-case "workflows job: 2 inner workers"
      (check-true (ormap (lambda (ln) (regexp-match? #rx"--jobs 2" ln)) (job-body "workflows"))))

    ;; Pin 4: required job names
    (test-case "required-pr-checks.policy pins the required job-name set"
      (define expected
        '("lint" "security"
                 "release-dry-run"
                 "workflows (0)"
                 "workflows (1)"
                 "workflows-aggregate"
                 "smoke (ubuntu-latest)"
                 "test (0)"
                 "test (1)"
                 "test (2)"
                 "test-aggregate"
                 "test-platform"))
      (check-equal? (sort (policy-jobs) string<?) (sort expected string<?)))
    (test-case "policy names correspond to real jobs"
      (define jobs (top-jobs))
      (for ([tok (in-list (policy-jobs))])
        (define bare (car (regexp-split #rx" \\(" tok)))
        (check-not-false (member bare jobs)
                         (format "policy name ~a must map to job ~a in ci.yml" tok bare))))

    ;; Pin 5: absent scheduler seam (W2 flips this)
    (test-case "no --scheduler option anywhere in ci.yml (W2 flips this)"
      (check-false (regexp-match? #rx"--scheduler" (file->string ci-yml))))

    ;; Pin 6: JSON / artifact consumers
    (test-case "shard-plan-report consumes retained test-results-fast-* artifacts"
      (define body (job-body "shard-plan-report"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"pattern: test-results-fast-" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"test-results.json" ln)) body)))
    (test-case "fast test job uploads test-results-fast-<shard> JSON artifacts"
      (define body (job-body "test"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"test-results-fast-" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"--json-out test-results.json" ln)) body)))
    (test-case "job step summaries read .run_summary fields"
      (check-true (ormap (lambda (ln) (regexp-match? #rx"run_summary" ln)) (job-body "test"))))
    (test-case "release-readiness records gate evidence for all four suites"
      (define body (job-body "release-readiness"))
      (for ([suite '("fast" "tui" "arch" "workflows")])
        (check-true (ormap (lambda (ln)
                             (regexp-match?
                              (regexp (format "--record-gate-evidence --json-out test-results-~a.json"
                                              suite))
                              ln))
                           body)
                    (format "release-readiness must record gate evidence for ~a" suite))))

    ;; Pin 7: gsd-governance evidence contract
    (test-case "gsd-governance validates exactly one changed wave-evidence record"
      (define body (job-body "gsd-governance"))
      (check-true (ormap (lambda (ln)
                           (regexp-match? #rx"Expected exactly one changed wave evidence record" ln))
                         body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"gsd-wave-gate.rkt" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"required-pr-checks.policy" ln)) body)))))

(module+ main
  (exit (run-tests (suite))))

(module+ test
  (exit (run-tests (suite))))

#lang racket/base
(require (only-in "../util/version.rkt" q-version))

;; @speed fast
;; @boundary unit

;; BOUNDARY: unit
;; tests/test-ci-runtime-contract.rkt
;; Pin today's .github/workflows/ci.yml runtime contract
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
;;  8. (v<q-version> W0) fail-closed gate semantics: a missing required check
;;     fails the gate naming the missing check — never passes; fast-env is
;;     a job but not a required check; every aggregate job needs its shards
;;     and both aggregates + all shards are required.
;;  9. (v<q-version> W0) the live required-check graph is captured in
;;     artifacts/ci-topology/v<q-version>-w0/graph-snapshot.json, bound by
;;     SHA256SUMS, and must equal the policy pin (drift names itself).
;; 10. (v<q-version> W0) a same-SHA DAG timing baseline (per-job start/end for
;;     one PR run) travels with the snapshot for W1-W3 comparison.

(require rackunit
         rackunit/text-ui
         racket/string
         racket/match
         racket/list
         racket/file
         racket/path
         racket/runtime-path
         racket/format
         racket/string
         json)

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
    (define m (regexp-match #rx"^    needs:(.*)$" ln))
    (define contents
      (string-trim (string-replace (string-replace (if m
                                                       (cadr m)
                                                       "")
                                                   "["
                                                   "")
                                   "]"
                                   "")))
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

;; Pure-Racket SHA-256 (lowercase hex) so the checksum pin has no collect
;; dependencies beyond racket/base. Verified against sha256sum(1) on the
;; snapshot artifact.
(define (sha256-hex path)
  (define (m32 x)
    (bitwise-and x #xffffffff))
  (define (rotr x n)
    (bitwise-ior (arithmetic-shift (m32 x) (- n)) (arithmetic-shift (m32 x) (- 32 n))))
  (define (word bs i)
    (+ (* (bytes-ref bs i) 16777216)
       (* (bytes-ref bs (+ i 1)) 65536)
       (* (bytes-ref bs (+ i 2)) 256)
       (bytes-ref bs (+ i 3))))
  (define k
    (list->vector '(#x428a2f98 #x71374491
                               #xb5c0fbcf
                               #xe9b5dba5
                               #x3956c25b
                               #x59f111f1
                               #x923f82a4
                               #xab1c5ed5
                               #xd807aa98
                               #x12835b01
                               #x243185be
                               #x550c7dc3
                               #x72be5d74
                               #x80deb1fe
                               #x9bdc06a7
                               #xc19bf174
                               #xe49b69c1
                               #xefbe4786
                               #x0fc19dc6
                               #x240ca1cc
                               #x2de92c6f
                               #x4a7484aa
                               #x5cb0a9dc
                               #x76f988da
                               #x983e5152
                               #xa831c66d
                               #xb00327c8
                               #xbf597fc7
                               #xc6e00bf3
                               #xd5a79147
                               #x06ca6351
                               #x14292967
                               #x27b70a85
                               #x2e1b2138
                               #x4d2c6dfc
                               #x53380d13
                               #x650a7354
                               #x766a0abb
                               #x81c2c92e
                               #x92722c85
                               #xa2bfe8a1
                               #xa81a664b
                               #xc24b8b70
                               #xc76c51a3
                               #xd192e819
                               #xd6990624
                               #xf40e3585
                               #x106aa070
                               #x19a4c116
                               #x1e376c08
                               #x2748774c
                               #x34b0bcb5
                               #x391c0cb3
                               #x4ed8aa4a
                               #x5b9cca4f
                               #x682e6ff3
                               #x748f82ee
                               #x78a5636f
                               #x84c87814
                               #x8cc70208
                               #x90befffa
                               #xa4506ceb
                               #xbef9a3f7
                               #xc67178f2)))
  (define data (file->bytes path))
  (define len (bytes-length data))
  (define padded
    (let* ([zeros (let loop ([n 0])
                    (if (= 56 (modulo (+ len 1 n) 64))
                        n
                        (loop (add1 n))))]
           [total (+ len 1 zeros 8)]
           [bs (make-bytes total 0)])
      (bytes-copy! bs 0 data)
      (bytes-set! bs len #x80)
      (define bitlen (* 8 len))
      (for ([i (in-range 8)])
        (bytes-set! bs (- total 1 i) (bitwise-and (arithmetic-shift bitlen (* -8 i)) #xff)))
      bs))
  (define h
    (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  (define w (make-vector 64 0))
  (for ([off (in-range 0 (bytes-length padded) 64)])
    (for ([i (in-range 16)])
      (vector-set! w i (word padded (+ off (* 4 i)))))
    (for ([i (in-range 16 64)])
      (define s0
        (bitwise-xor (rotr (vector-ref w (- i 15)) 7)
                     (rotr (vector-ref w (- i 15)) 18)
                     (arithmetic-shift (vector-ref w (- i 15)) -3)))
      (define s1
        (bitwise-xor (rotr (vector-ref w (- i 2)) 17)
                     (rotr (vector-ref w (- i 2)) 19)
                     (arithmetic-shift (vector-ref w (- i 2)) -10)))
      (vector-set! w i (m32 (+ (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1))))
    (let loop ([a (vector-ref h 0)]
               [b (vector-ref h 1)]
               [c (vector-ref h 2)]
               [d (vector-ref h 3)]
               [e (vector-ref h 4)]
               [f (vector-ref h 5)]
               [g (vector-ref h 6)]
               [hh (vector-ref h 7)]
               [i 0])
      (if (= i 64)
          (begin
            (for ([i2 (in-range 8)])
              (vector-set! h i2 (m32 (+ (vector-ref h i2) (list-ref (list a b c d e f g hh) i2))))))
          (let* ([S1 (bitwise-xor (rotr e 6) (rotr e 11) (rotr e 25))]
                 [ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-not e) g))]
                 [t1 (m32 (+ hh S1 ch (vector-ref k i) (vector-ref w i)))]
                 [S0 (bitwise-xor (rotr a 2) (rotr a 13) (rotr a 22))]
                 [maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c))]
                 [t2 (m32 (+ S0 maj))])
            (loop (m32 (+ t1 t2)) a b c (m32 (+ d t1)) e f g (add1 i))))))
  (define (hex8 n)
    (define s (number->string n 16))
    (string-append (make-string (- 8 (string-length s)) #\0) s))
  (string-append* (for/list ([i (in-range 8)])
                    (hex8 (vector-ref h i)))))

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

    ;; Pin 2 (W3): shard-plan-report is relocated post-workflow; the ci.yml
    ;; workflow tail is now test-aggregate. The job itself is byte-identical in
    ;; .github/workflows/shard-plan-telemetry.yml (checksum-pinned via the W3
    ;; checkpoint; deeper pins live in tests/test-w3-telemetry-relocation.rkt).
    (test-case "shard-plan-report is relocated out of ci.yml (W3)"
      (check-false (member "shard-plan-report" (top-jobs))
                   "ci.yml must no longer define shard-plan-report")
      (define telemetry-yml
        (build-path project-root ".github" "workflows" "shard-plan-telemetry.yml"))
      (check-true (file-exists? telemetry-yml)
                  "the relocated report must live in shard-plan-telemetry.yml")
      (define cp
        (call-with-input-file (build-path project-root
                                          "artifacts"
                                          "ci-topology"
                                          (format "v~a-w3" q-version)
                                          "dag-checkpoint.json")
                              read-json))
      (check-equal? (sha256-hex telemetry-yml)
                    (hash-ref (hash-ref cp 'telemetry_relocation_contract) 'telemetry_workflow_sha256)
                    "the relocated job must stay byte-identical to the recorded relocation")
      (check-true (regexp-match? #rx"workflow_run" (file->string telemetry-yml))
                  "post-workflow trigger: workflow_run: CI completed"))
    (test-case "test-aggregate is the ci.yml workflow tail"
      (check-equal? (job-needs "test-aggregate") '("test" "test-platform"))
      (for ([job (in-list (top-jobs))])
        (check-false (member 'test-aggregate (job-needs job))
                     (format "~a must not depend on the new workflow tail" job))))
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
        '("lint" "lint-quality"
                 "security"
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

    ;; Pin 5b (W4, decision: HOLD): the measured CI trial (see
    ;; artifacts/ci-topology/v<q-version>-w4/shard-measurement.json) recorded queue
    ;; p50s above the batch baseline with unreliable p95, so per the wave rule
    ;; the workflow keeps the product-default batch scheduler. The runner-side
    ;; TEST_RUNNER_SCHEDULER env seam and its kill switch remain available for
    ;; a future measured re-trial — reasserted in
    ;; tests/test-runner-scheduler-characterization.rkt.
    (test-case "workflow shards keep the product-default scheduler (W4 hold: no scheduler env in ci.yml)"
      (check-false
       (regexp-match? #rx"TEST_RUNNER_SCHEDULER[ \t]*:" (file->string ci-yml))
       "W4 hold: ci.yml must not set TEST_RUNNER_SCHEDULER anywhere —
        workflow shards run the product default (batch) until a reliable
        measurement justifies queue")
      (check-false (regexp-match? #rx"--scheduler" (file->string ci-yml))
                   "no --scheduler option may appear in ci.yml (env seam only, and currently unset)"))
    (test-case "scheduler env stays scoped to the workflow shards"
      (for ([job (in-list (top-jobs))])
        (unless (equal? job "workflows")
          (check-false
           (ormap (lambda (ln) (regexp-match? #rx"TEST_RUNNER_SCHEDULER" ln)) (job-body job))
           (format "job ~a must not set TEST_RUNNER_SCHEDULER (workflow shards only)" job)))))
    (test-case "workflow shard commands keep jobs=2 and JSON shape (scheduler choice does not alter them)"
      (define body (job-body "workflows"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"--jobs 2" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"--json-out test-results.json" ln)) body)))

    ;; Pin 6: JSON / artifact consumers
    (test-case "relocated report consumes retained test-results-fast-* artifacts (post-workflow)"
      (define t
        (file->string (build-path project-root ".github" "workflows" "shard-plan-telemetry.yml")))
      (check-true (regexp-match? #rx"test-results-fast" t)
                  "telemetry consumes this run's retained per-shard artifacts")
      (check-true (regexp-match? #rx"test-results.json" t) "per-shard JSON inputs unchanged"))
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
    (test-case "gsd-governance validates one wave record and tolerates aggregate pushes"
      (define body (job-body "gsd-governance"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"Multiple wave evidence records changed" ln))
                         body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"-gt 1" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"gsd-wave-gate.rkt" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"required-pr-checks.policy" ln)) body)))

    ;; Pin 8 (v<q-version> W0): fail-closed required-check gate semantics
    (test-case "W0: the protected-main gate is fail-closed by characterization"
      (define (required-gate-verdict required completed)
        (define missing
          (for/list ([c (in-list required)]
                     #:unless (member c completed))
            c))
        (if (null? missing)
            'pass
            (list 'fail missing)))
      (define required (policy-jobs))
      (check-equal? (required-gate-verdict required required) 'pass)
      (check-equal? (required-gate-verdict required '())
                    (list 'fail required)
                    "an empty completion set must fail the whole gate")
      (define one-missing (remove "test (1)" required))
      (check-equal? (required-gate-verdict required one-missing)
                    (list 'fail (list "test (1)"))
                    "a single missing check fails the gate naming exactly it")
      ;; a missing required check NEVER passes (fail-closed, not fail-open)
      (for ([c (in-list required)])
        (check-not-equal? (required-gate-verdict required (remove c required))
                          'pass
                          (format "gate passed despite missing ~a" c))))
    (test-case "W0: fast-env is an optimization env job, never a required check"
      (check-not-false (member "fast-env" (top-jobs)) "fast-env must exist as a job in ci.yml")
      (check-false (member "fast-env" (policy-jobs)) "fast-env must NOT be a required check"))
    (test-case "W0: every aggregate job needs its shard jobs and both are required"
      (check-true (andmap (lambda (j) (and (member j (job-needs "test-aggregate")) #t))
                          '("test" "test-platform"))
                  "test-aggregate must need test and test-platform")
      (check-true (andmap (lambda (j) (and (member j (job-needs "workflows-aggregate")) #t))
                          '("workflows"))
                  "workflows-aggregate must need workflows")
      (check-not-false (member "test-aggregate" (policy-jobs)))
      (check-not-false (member "workflows-aggregate" (policy-jobs)))
      (for ([shard
             '("test (0)" "test (1)" "test (2)" "test-platform" "workflows (0)" "workflows (1)")])
        (check-not-false (member shard (policy-jobs))
                         (format "shard ~a must be a required check" shard))))

    ;; Pin 9 (v<q-version> W0, re-pointed W1): the W0 graph snapshot stays
    ;; checksum-bound as the HISTORICAL baseline. Its recorded policy_pin is
    ;; compared against its own frozen capture — not against the live policy,
    ;; which W1 legitimately extends with lint-quality.
    (test-case "W1: the W0 graph snapshot remains checksum-bound as the historical baseline"
      (define snap
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w0" q-version)
                    "graph-snapshot.json"))
      (check-true (file-exists? snap) "W0 graph-snapshot.json must exist")
      (check-equal? (sha256-hex snap)
                    "6e0eee763b696c772482e3f4c3c060d95d96546c690394b2aa5f85139eb41a25"
                    "W0 snapshot must stay byte-for-byte the checksummed baseline")
      (define j (call-with-input-file snap read-json))
      (check-equal? (sort (map ~a (hash-ref j 'policy_pin)) string<?)
                    (sort '("lint" "security"
                                   "release-dry-run"
                                   "smoke (ubuntu-latest)"
                                   "test (0)"
                                   "test (1)"
                                   "test (2)"
                                   "test-aggregate"
                                   "test-platform"
                                   "workflows (0)"
                                   "workflows (1)"
                                   "workflows-aggregate")
                          string<?)
                    "W0 snapshot policy_pin must equal its frozen W0-era capture")
      (check-false
       (member "lint-quality" (hash-ref j 'policy_pin))
       "the W0 baseline predates lint-quality; if it appears, the baseline was rewritten"))

    ;; Pin 10 (v<q-version> W1): the W1 DAG checkpoint binds the CURRENT required
    ;; topology: live policy equality, the §7.1 safe-apply sequence with lint
    ;; required at every step, and the same-SHA before/after timing shape
    ;; showing test (0) no longer waits for Racket lint steps. The checkpoint
    ;; is a topology checkpoint — explicitly NOT a cohort.
    (test-case "W1: dag-checkpoint exists, is checksum-bound, and equals the live policy"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w1" q-version)
                    "dag-checkpoint.json"))
      (check-true (file-exists? cp) "W1 dag-checkpoint.json must exist")
      (check-equal? (sha256-hex cp)
                    "2f4d64e8728a18578ab81758a02acb93950613e9a49d0ee5903f99a3f67739e2"
                    "W1 dag-checkpoint.json must stay byte-for-byte the recorded checkpoint")
      (define j (call-with-input-file cp read-json))
      (check-equal? (hash-ref j 'wave) (format "v~a-w1" q-version))
      (check-equal? (sort (map ~a (hash-ref j 'policy_pin)) string<?)
                    (sort (policy-jobs) string<?)
                    "checkpoint policy_pin must equal the live required-pr-checks.policy")
      (check-not-false (member "lint-quality" (policy-jobs)) "lint-quality must be a required check"))
    (test-case "W1: checkpoint records the post-split required graph with both lint jobs required"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w1" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define prot (hash-ref j 'branch_protection))
      (define ctxs (map ~a (hash-ref prot 'required_status_checks)))
      (check-true (hash-ref prot 'strict) "branch protection must keep strict up-to-date")
      (check-not-false (member "lint" ctxs) "lint must remain required throughout (never fail-open)")
      (check-not-false (member "lint-quality" ctxs)
                       "lint-quality must be required in the post-split graph")
      (check-equal? (sort ctxs string<?)
                    (sort (map ~a (policy-jobs)) string<?)
                    "required contexts must equal the policy set exactly"))
    (test-case "W1: safe-apply sequence keeps lint required at every step and orders lint-quality green-before-required"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w1" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define seq (hash-ref j 'safe_apply_sequence))
      (check-true (>= (length seq) 5) "the §7.1 sequence must record its ordered steps")
      (check-equal? (map (lambda (s) (hash-ref s 'step)) seq)
                    (build-list (length seq) add1)
                    "sequence steps must be recorded in apply order")
      (for ([s (in-list seq)])
        (check-true (hash-ref s 'lint_required)
                    (format "step ~a must keep lint required (no fail-open window)"
                            (hash-ref s 'step))))
      (define first-protection-add
        (for/first ([s (in-list seq)]
                    #:when (equal? (hash-ref s 'action) "add-lint-quality-protection"))
          s))
      (check-not-false first-protection-add "sequence must add the protection requirement")
      (check-true (hash-ref first-protection-add 'precondition_met)
                  "lint-quality must report green on main BEFORE protection (precondition recorded)"))
    (test-case "W1: same-SHA timing checkpoint shows test (0) no longer waits for Racket lint steps"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w1" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define timing (hash-ref j 'timing_checkpoint))
      (check-equal? (hash-ref timing 'label) "topology checkpoint — not a cohort")
      (check-equal? (hash-ref (hash-ref timing 'before) 'head_sha)
                    "c6ee39e43025cc49202a27084aed459221dac43c"
                    "before must be the W0 baseline SHA")
      (define before (hash-ref timing 'before))
      (define after (hash-ref timing 'after))
      ;; same-SHA pairing: before and after are anchored to the same commit SHA
      (check-equal? (hash-ref before 'head_sha) (hash-ref after 'anchor_sha))
      (define (job-t side name)
        (for/first ([jj (in-list (hash-ref side 'jobs))]
                    #:when (equal? (hash-ref jj 'name) name))
          jj))
      (define b-lint (job-t before "lint"))
      (define b-test0 (job-t before "test (0)"))
      (define a-lint (job-t after "lint"))
      (define a-lq (job-t after "lint-quality"))
      (define a-test0 (job-t after "test (0)"))
      (check-not-false (and b-lint b-test0 a-lint a-lq a-test0)
                       "before/after must both record per-job start/end for lint and test (0)")
      (for ([jj (in-list (list b-lint b-test0 a-lint a-lq a-test0))])
        (check-not-false (and (hash-ref jj 'started_at) (hash-ref jj 'completed_at))
                         "per-job timing entries must carry start/end times"))
      ;; the split effect: lint's completion moves BEFORE test (0)'s start by
      ;; the Racket-lint duration that moved to the parallel lint-quality job
      (check-true (string<? (hash-ref a-lint 'completed_at) (hash-ref a-test0 'started_at))
                  "after the split, lint completes before test (0) starts")
      (check-true (string<? (hash-ref a-test0 'started_at) (hash-ref b-test0 'started_at))
                  "test (0) starts earlier under the split topology (same-SHA shape)")
      (check-true (string<? (hash-ref a-lint 'completed_at) (hash-ref b-lint 'completed_at))
                  "lightweight lint finishes earlier than the old heavyweight lint")
      (check-true (string<? (hash-ref a-lq 'completed_at) (hash-ref a-test0 'started_at))
                  "lint-quality runs in parallel and completes before test (0) under the split")
      ;; topology-not-cohort honesty: derived projection, provenance recorded
      (check-equal? (hash-ref timing 'derived_from)
                    (format "artifacts/ci-topology/v~a-w0/graph-snapshot.json#timing_baseline"
                            q-version))
      (check-equal? (hash-ref timing 'measurement_kind) "derived-shape-projection"))
    (test-case "W1: lint-quality is parallel in the workflow graph (no downstream waits on it)"
      (check-equal? (job-needs "lint-quality")
                    '()
                    "lint-quality must be a parallel required job, not a bottleneck")
      (for ([job
             '("fast-env" "test" "test-platform" "security" "workflows" "smoke" "release-dry-run")])
        (check-not-false (member "lint" (job-needs job))
                         (format "~a must still need only the lightweight lint gate" job))))

    ;; Pin 11 (v<q-version> W2): fast-env starts after the LIGHTWEIGHT lint
    ;; gate, concurrent with lint-quality. The resequencing touches the
    ;; needs edge only: the prepared-environment producer's
    ;; manifest/OS/Racket/lockfile verification steps stay byte-identical
    ;; and the loud fallback on prepared-env restore failure still fails
    ;; the lane.
    (test-case "W2: dag-checkpoint exists, is checksum-bound, and equals the live policy"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w2" q-version)
                    "dag-checkpoint.json"))
      (check-true (file-exists? cp) "W2 dag-checkpoint.json must exist")
      (check-equal? (sha256-hex cp)
                    "0c18bf7d204df7313fdeea213714828db825b22debc9bbfa5969019905a67187"
                    "W2 dag-checkpoint.json must stay byte-for-byte the recorded checkpoint")
      (define j (call-with-input-file cp read-json))
      (check-equal? (hash-ref j 'wave) (format "v~a-w2" q-version))
      (check-equal? (sort (map ~a (hash-ref j 'policy_pin)) string<?)
                    (sort (policy-jobs) string<?)
                    "checkpoint policy_pin must equal the live required-pr-checks.policy"))
    (test-case "W2: needs-edge pin — fast-env needs only lint, no ordering edge with lint-quality"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w2" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define edges (hash-ref j 'needs_edges))
      ;; the live workflow: fast-env waits exactly once, for the lightweight gate
      (check-equal? (job-needs "fast-env")
                    '("lint")
                    "fast-env must need exactly lint (lightweight gate), nothing heavier")
      (check-equal? (length (job-needs "fast-env")) 1 "fast-env must have exactly one needs edge")
      ;; the recorded DAG: same pin, plus the absence of a lint-quality edge
      (check-equal? (map ~a (hash-ref edges 'fast-env)) '("lint"))
      (check-equal? (hash-ref edges 'fast-env_to_lint-quality_ordering_edge) "none")
      (check-false (member "lint-quality" (job-needs "fast-env"))
                   "fast-env must NOT wait for lint-quality")
      (check-false (member "fast-env" (job-needs "lint-quality"))
                   "lint-quality must NOT wait for fast-env")
      ;; test jobs keep their W0/W1-pinned requirements exactly
      (check-equal? (job-needs "test") '("lint" "fast-env"))
      (check-equal? (map ~a (hash-ref edges 'test)) '("lint" "fast-env"))
      (check-equal? (job-needs "test-platform") '("lint"))
      (check-equal? (map ~a (hash-ref edges 'test-platform)) '("lint")))
    (test-case "W2: prepared-env verification steps byte-identical, loud fallback fails the lane"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w2" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define contract (hash-ref j 'fast_env_verification_contract))
      (define prepare-action
        (build-path project-root ".github" "actions" "prepare-racket-environment" "action.yml"))
      (define setup-action (build-path project-root ".github" "actions" "setup-racket" "action.yml"))
      ;; byte-identity: the resequencing must not touch the producer or
      ;; the guarded restore action — hash equal to the recorded pre-change value
      (check-true (file-exists? prepare-action))
      (check-equal? (sha256-hex prepare-action)
                    (hash-ref contract 'prepare_action_sha256)
                    "manifest/OS/Racket/lockfile verification steps must stay byte-identical")
      (check-equal? (sha256-hex setup-action)
                    (hash-ref contract 'setup_action_sha256)
                    "the guarded restore action must stay byte-identical")
      (define prepare-text (file->string prepare-action))
      ;; manifest/OS/Racket/lockfile verification dimensions still pinned
      (check-true (string-contains? prepare-text "manifest.rkt emit"))
      (check-true (string-contains? prepare-text "manifest.rkt verify"))
      (check-true (string-contains? prepare-text "lock_digest"))
      (check-true (string-contains? prepare-text "::error::"))
      (check-true (string-contains? prepare-text "exit 1"))
      ;; loud fallback: a failed/mismatched prepared-env restore is flagged
      ;; and the manifest-missing restore fails the step (never silent)
      (define setup-text (file->string setup-action))
      (check-true (string-contains? setup-text
                                    "::warning::prepared-environment restore failed or mismatched")
                  "the fallback must stay loud (::warning), never a silent rebuild")
      (check-true (string-contains? setup-text "REBUILT"))
      (check-true (string-contains? (file->string ci-yml) "needs.fast-env.result")
                  "test shards must keep gating PREPARED_ENV on the fast-env result"))
    (test-case "W2: same-SHA timing shape — fast-env after lightweight lint, concurrent with lint-quality"
      (define cp
        (build-path project-root
                    "artifacts"
                    "ci-topology"
                    (format "v~a-w2" q-version)
                    "dag-checkpoint.json"))
      (define j (call-with-input-file cp read-json))
      (define timing (hash-ref j 'timing_checkpoint))
      (check-equal? (hash-ref timing 'label) "topology checkpoint — not a cohort")
      (check-equal? (hash-ref timing 'measurement_kind) "derived-shape-projection")
      (check-equal? (hash-ref timing 'derived_from)
                    (format "artifacts/ci-topology/v~a-w1/dag-checkpoint.json#timing_checkpoint"
                            q-version))
      (define before (hash-ref timing 'before))
      (define after (hash-ref timing 'after))
      (check-equal? (hash-ref before 'head_sha)
                    "c6ee39e43025cc49202a27084aed459221dac43c"
                    "before anchors the measured W0 baseline run")
      (check-equal? (hash-ref before 'head_sha)
                    (hash-ref after 'anchor_sha)
                    "same-SHA pairing: before and after anchor the same commit")
      (define (job-t side name)
        (for/first ([jj (in-list (hash-ref side 'jobs))]
                    #:when (equal? (hash-ref jj 'name) name))
          jj))
      (define b-fast-env (job-t before "fast-env"))
      (define a-lint (job-t after "lint"))
      (define a-lq (job-t after "lint-quality"))
      (define a-fast-env (job-t after "fast-env"))
      (check-not-false (and b-fast-env a-lint a-lq a-fast-env)
                       "before/after must record per-job start/end for the moved jobs")
      (for ([jj (in-list (list b-fast-env a-lint a-lq a-fast-env))])
        (check-not-false (and (hash-ref jj 'started_at) (hash-ref jj 'completed_at))
                         "per-job timing entries must carry start/end times"))
      ;; the W2 effect: fast-env starts right after the LIGHTWEIGHT lint
      (check-true (string<? (hash-ref a-lint 'completed_at) (hash-ref a-fast-env 'started_at))
                  "fast-env starts only after lightweight lint completes")
      (check-true (string<? (hash-ref a-fast-env 'started_at) (hash-ref b-fast-env 'started_at))
                  "fast-env starts earlier once lint is lightweight (same-SHA shape)")
      ;; concurrency: the fast-env and lint-quality execution intervals overlap
      (check-true (string<? (hash-ref a-lq 'started_at) (hash-ref a-fast-env 'completed_at))
                  "lint-quality is still running when fast-env starts (concurrent)")
      (check-true (string<? (hash-ref a-fast-env 'started_at) (hash-ref a-lq 'completed_at))
                  "fast-env is still running when lint-quality finishes (concurrent)"))))

(module+ test
  (exit (run-tests (suite))))

(module+ main
  (exit (run-tests (suite))))

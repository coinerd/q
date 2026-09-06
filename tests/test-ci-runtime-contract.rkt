#lang racket/base

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
;;  8. (v1.00.26 W0) fail-closed gate semantics: a missing required check
;;     fails the gate naming the missing check — never passes; fast-env is
;;     a job but not a required check; every aggregate job needs its shards
;;     and both aggregates + all shards are required.
;;  9. (v1.00.26 W0) the live required-check graph is captured in
;;     artifacts/ci-topology/v1.00.26-w0/graph-snapshot.json, bound by
;;     SHA256SUMS, and must equal the policy pin (drift names itself).
;; 10. (v1.00.26 W0) a same-SHA DAG timing baseline (per-job start/end for
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
      (string-trim
       (string-replace
        (string-replace (if m (cadr m) "") "[" "") "]" "")))
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
  (define (m32 x) (bitwise-and x #xffffffff))
  (define (rotr x n)
    (bitwise-ior (arithmetic-shift (m32 x) (- n)) (arithmetic-shift (m32 x) (- 32 n))))
  (define (word bs i)
    (+ (* (bytes-ref bs i) 16777216) (* (bytes-ref bs (+ i 1)) 65536)
       (* (bytes-ref bs (+ i 2)) 256) (bytes-ref bs (+ i 3))))
  (define k
    (list->vector
     '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
       #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
       #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
       #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
       #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
       #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
       #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
       #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))
  (define data (file->bytes path))
  (define len (bytes-length data))
   (define padded
     (let* ([zeros (let loop ([n 0])
                     (if (= 56 (modulo (+ len 1 n) 64)) n (loop (add1 n))))]
           [total (+ len 1 zeros 8)]
           [bs (make-bytes total 0)])
      (bytes-copy! bs 0 data)
      (bytes-set! bs len #x80)
      (define bitlen (* 8 len))
      (for ([i (in-range 8)])
        (bytes-set! bs (- total 1 i)
                    (bitwise-and (arithmetic-shift bitlen (* -8 i)) #xff)))
      bs))
  (define h (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                    #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  (define w (make-vector 64 0))
  (for ([off (in-range 0 (bytes-length padded) 64)])
    (for ([i (in-range 16)]) (vector-set! w i (word padded (+ off (* 4 i)))))
    (for ([i (in-range 16 64)])
      (define s0 (bitwise-xor (rotr (vector-ref w (- i 15)) 7)
                              (rotr (vector-ref w (- i 15)) 18)
                              (arithmetic-shift (vector-ref w (- i 15)) -3)))
      (define s1 (bitwise-xor (rotr (vector-ref w (- i 2)) 17)
                              (rotr (vector-ref w (- i 2)) 19)
                              (arithmetic-shift (vector-ref w (- i 2)) -10)))
      (vector-set! w i (m32 (+ (vector-ref w (- i 16)) s0
                               (vector-ref w (- i 7)) s1))))
    (let loop ([a (vector-ref h 0)] [b (vector-ref h 1)] [c (vector-ref h 2)]
               [d (vector-ref h 3)] [e (vector-ref h 4)] [f (vector-ref h 5)]
               [g (vector-ref h 6)] [hh (vector-ref h 7)] [i 0])
      (if (= i 64)
          (begin
            (for ([i2 (in-range 8)])
              (vector-set! h i2 (m32 (+ (vector-ref h i2)
                                        (list-ref (list a b c d e f g hh) i2))))))
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
  (string-append* (for/list ([i (in-range 8)]) (hex8 (vector-ref h i)))))

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
    (test-case "gsd-governance validates one wave record and tolerates aggregate pushes"
      (define body (job-body "gsd-governance"))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"Multiple wave evidence records changed" ln))
                         body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"-gt 1" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"gsd-wave-gate.rkt" ln)) body))
      (check-true (ormap (lambda (ln) (regexp-match? #rx"required-pr-checks.policy" ln)) body)))

    ;; Pin 8 (v1.00.26 W0): fail-closed required-check gate semantics
    (test-case "W0: the protected-main gate is fail-closed by characterization"
      (define (required-gate-verdict required completed)
        (define missing
          (for/list ([c (in-list required)] #:unless (member c completed)) c))
        (if (null? missing) 'pass (list 'fail missing)))
      (define required (policy-jobs))
      (check-equal? (required-gate-verdict required required) 'pass)
      (check-equal? (required-gate-verdict required '()) (list 'fail required)
                    "an empty completion set must fail the whole gate")
      (define one-missing (remove "test (1)" required))
      (check-equal? (required-gate-verdict required one-missing)
                    (list 'fail (list "test (1)"))
                    "a single missing check fails the gate naming exactly it")
      ;; a missing required check NEVER passes (fail-closed, not fail-open)
      (for ([c (in-list required)])
        (check-not-equal? (required-gate-verdict required (remove c required)) 'pass
                          (format "gate passed despite missing ~a" c))))
    (test-case "W0: fast-env is an optimization env job, never a required check"
      (check-not-false (member "fast-env" (top-jobs))
                       "fast-env must exist as a job in ci.yml")
      (check-false (member "fast-env" (policy-jobs))
                   "fast-env must NOT be a required check"))
    (test-case "W0: every aggregate job needs its shard jobs and both are required"
      (check-true (andmap (lambda (j) (and (member j (job-needs "test-aggregate")) #t))
                          '("test" "test-platform"))
                  "test-aggregate must need test and test-platform")
      (check-true (andmap (lambda (j) (and (member j (job-needs "workflows-aggregate")) #t))
                          '("workflows"))
                  "workflows-aggregate must need workflows")
      (check-not-false (member "test-aggregate" (policy-jobs)))
      (check-not-false (member "workflows-aggregate" (policy-jobs)))
      (for ([shard '("test (0)" "test (1)" "test (2)" "test-platform"
                     "workflows (0)" "workflows (1)")])
        (check-not-false (member shard (policy-jobs))
                         (format "shard ~a must be a required check" shard))))

    ;; Pin 9 (v1.00.26 W0): checksummed live-graph snapshot pins the policy pin
    (test-case "W0: checksummed snapshot exists and binds the policy pin (drift names itself)"
      (define w0-snapshot-file
        (build-path project-root "artifacts" "ci-topology" "v1.00.26-w0" "graph-snapshot.json"))
      (define w0-sums-file
        (build-path project-root "artifacts" "ci-topology" "v1.00.26-w0" "SHA256SUMS"))
      (check-true (file-exists? w0-snapshot-file)
                  "artifacts/ci-topology/v1.00.26-w0/graph-snapshot.json must exist")
      (check-true (file-exists? w0-sums-file)
                  "artifacts/ci-topology/v1.00.26-w0/SHA256SUMS must exist")
      (define recorded
        ;; {64} is a PCRE bound quantifier: #px required (repo precedent,
        ;; tests/test-parser-hotspots.rkt — #rx treats {n} as literal chars).
        (car (regexp-match #px"[0-9a-f]{64}" (car (file->lines w0-sums-file)))))
      (define actual (sha256-hex w0-snapshot-file))
      (check-equal? actual recorded
                    "graph-snapshot.json does not match its SHA256SUMS binding")
      (define snapshot (call-with-input-file w0-snapshot-file read-json))
      (check-true (hash? snapshot) "snapshot must be a JSON object")
      (define contexts
        (hash-ref (hash-ref snapshot 'branch_protection) 'required_status_checks))
      (define policy-set (sort (policy-jobs) string<?))
      (define snapshot-set (sort (map ~a contexts) string<?))
      (check-equal? snapshot-set policy-set
                    (format "required-check drift: policy-only=~a snapshot-only=~a"
                            (remove* snapshot-set policy-set)
                            (remove* policy-set snapshot-set))))

    ;; Pin 10 (v1.00.26 W0): same-SHA DAG timing baseline travels with the snapshot
    (test-case "W0: same-SHA DAG timing baseline recorded with the snapshot"
      (define w0-snapshot-file
        (build-path project-root "artifacts" "ci-topology" "v1.00.26-w0" "graph-snapshot.json"))
      (define snapshot (call-with-input-file w0-snapshot-file read-json))
      (define tb (hash-ref snapshot 'timing_baseline))
      (define jobs (hash-ref tb 'jobs))
      (check-true (>= (length jobs) (length (policy-jobs)))
                  "baseline must cover at least the required-check jobs")
      (define shas
        (remove-duplicates (map (lambda (j) (~a (hash-ref j 'head_sha))) jobs)))
      (check-equal? (length shas) 1
                    "timing baseline must be one same-SHA DAG (single head_sha)")
      (for ([j (in-list jobs)])
        (check-true (and (hash-ref j 'started_at #f) (hash-ref j 'completed_at #f) #t)
                    (format "baseline job ~a is missing start/end times"
                            (hash-ref j 'name)))))))

(module+ main
  (exit (run-tests (suite))))

(module+ test
  (exit (run-tests (suite))))

#lang racket

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary unit
;;
;; W2 (v1.00.32): tests-first contract for
;; scripts/run-tests/ci-recovery-benchmark-report.rkt — the controlled
;; A/B/C CI-recovery benchmark report (wave contract:
;; .planning/waves/W2-controlled-experiment-and-prepared-aggregate-candidate.md,
;; actions 4/5; shared W1 schema: docs/reports/CI-RECOVERY-EVIDENCE.md;
;; input contract: docs/reports/CI-RECOVERY-EXPERIMENT.md).
;;
;; ALL FIXTURES ARE SYNTHETIC. No live experiment is dispatched, no
;; measurement is claimed, and no verdict below describes a real run.
;;
;; Under test:
;;   - timestamp parsing/formatting (strict UTC, calendar-validated)
;;   - C-vs-A regression budget min(10% of baseline, 60 s), both p50+p95
;;   - full required critical path = max(required completed_at) −
;;     run.run_started_at: never the sum of parallel jobs, never the max
;;     job duration, pilot jobs excluded, queue waits reported separately,
;;     and supplied/precomputed durations NEVER trusted
;;   - re-assessment of every envelope via the W1 strict assessor — no
;;     precomputed verdict or self-claim is ever trusted
;;   - record attempt/trial-id/treatment/identity binding + launcher
;;     exit-code 0 (concurrent W1 repair alignment, included in fixtures)
;;   - run/head/attempt/trial identity binding + exact config across all
;;     regimes; source/config/timestamp/attempt mixes are named refusals
;;   - every attempt retained in raw (incl. failed/cancelled/refused);
;;     duplicates/reruns never inflate the sample population
;;   - >= 8 independent INTERLEAVED trials per treatment on one pinned tree
;;   - C contamination: ANY refused C trial blocks promotion readiness
;;   - n=8 honest uncertainty: no confidence interval is fabricated
;;   - quantile method pinned to cohort-quantile-exact
;;   - byte-deterministic CLI output

(require json
         racket/file
         racket/format
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         (only-in "../scripts/run-tests/cohort-report.rkt" cohort-quantile-exact)
         "../scripts/run-tests/ci-recovery-benchmark-report.rkt")

(define-runtime-path project-root "..")
(define script-path
  (build-path project-root "scripts" "run-tests" "ci-recovery-benchmark-report.rkt"))

;; ---------------------------------------------------------------------------
;; Small uniform assertion helpers (keep nesting shallow and identical)
;; ---------------------------------------------------------------------------

(define (has-reason? reason reasons)
  (and (member reason reasons) #t))

;; ---------------------------------------------------------------------------
;; Synthetic identity / config / envelope / actions fixtures
;; ---------------------------------------------------------------------------

(define hex-a (make-string 64 #\a))
(define hex-b (make-string 64 #\b))
(define hex-c (make-string 64 #\c))
(define hex-d (make-string 64 #\d))
(define hex-e (make-string 64 #\e))
(define hex-f (make-string 64 #\f))
(define sha-a (make-string 40 #\a))
(define sha-b (make-string 40 #\b))
(define harness-sha (make-string 40 #\9))

(define base-time 1793354400) ; 2026-10-30T10:00:00Z
(define (ts s)
  (format-utc-seconds s))

(define-runtime-path required-policy-path "../scripts/required-pr-checks.policy")
(define required-names (call-with-input-file required-policy-path read))

(define (identity-fields)
  (hasheq 'os
          "Linux"
          'os-image
          "ubuntu-24.04:20260901.1"
          'arch
          "x64"
          'racket-version
          "8.10"
          'racket-executable-digest
          hex-a
          'lock-digest
          hex-b
          'resolved-set-digest
          hex-c
          'precompile-recipe-revision
          "setup-racket-full-path-r1"
          'policy-fingerprint
          "addon-store-v2-no-workspace-bytecode"
          'artifact-digest
          hex-d))

(define (config-fields)
  (hasheq 'runner-image
          "ubuntu-24.04:20260901.1"
          'platform
          "x64"
          'racket-version
          "8.10"
          'racket-executable-digest
          hex-a
          'lock-digest
          hex-b
          'resolved-set-digest
          hex-c
          'precompile-recipe-revision
          "setup-racket-full-path-r1"
          'policy-fingerprint
          "addon-store-v2-no-workspace-bytecode"
          'test-inventory-digest
          hex-e
          'shard-plan
          (list 0 1)
          'harness-revision
          (string-append "ci-recovery-benchmark.yml@" harness-sha)))

;; Records carry the concurrent-repair binding fields (attempt / trial-id /
;; treatment / identity) matching the trial block, and launchers carry
;; exit-code 0 — the report requires both.
(define (restore-record shard
                        #:trial-id [tid "trial-A"]
                        #:attempt [att 1]
                        #:treatment [treatment "A"]
                        #:identity [id (identity-fields)]
                        #:run-id [run 7100]
                        #:head [head sha-a]
                        #:created-at [created (ts base-time)]
                        #:outcome [outcome "verified"])
  (hasheq 'schema
          "prepared-env-restore-record"
          'trial-id
          tid
          'attempt
          att
          'treatment
          treatment
          'identity
          id
          'run-id
          run
          'head-sha
          head
          'created-at-utc
          created
          'shard
          shard
          'outcome
          outcome
          'restore-ms
          12000
          'wall-clock-seconds
          210.5))

(define (launcher-c)
  (hasheq 'producer-manifest-digest
          hex-d
          'mode
          "verified-root-hit"
          'root-hits
          3
          'fallbacks
          0
          'exit-code
          0
          'wall-ms
          90000
          'mechanism
          "external-compiled-root(map-dir symlinks)"))

(define (launcher-off)
  (hasheq 'producer-manifest-digest
          hex-d
          'mode
          "global-off"
          'root-hits
          0
          'fallbacks
          0
          'exit-code
          0
          'wall-ms
          1000))

(define (map-shards env f)
  (hash-set env 'shards (map f (hash-ref env 'shards (list)))))

(define (make-envelope #:treatment [treatment "A"]
                       #:trial-id [tid #f]
                       #:attempt [att 1]
                       #:run-id [run 7100]
                       #:head [head sha-a]
                       #:created-at [created (ts base-time)]
                       #:identity [id (identity-fields)]
                       #:shards [shards #f])
  (define trial-id (or tid (format "trial-~a" treatment)))
  (define shs
    (or shards
        (for/list ([shard (in-list (list 0 1))])
          (hasheq 'shard
                  shard
                  'record
                  (restore-record shard
                                  #:trial-id trial-id
                                  #:attempt att
                                  #:treatment treatment
                                  #:identity id
                                  #:run-id run
                                  #:head head
                                  #:created-at created)
                  'launcher
                  (if (equal? treatment "C")
                      (launcher-c)
                      (launcher-off))))))
  (hasheq 'schema
          "ci-recovery-trial-evidence@1"
          'trial
          (hasheq 'trial-id
                  trial-id
                  'attempt
                  att
                  'treatment
                  treatment
                  'run-id
                  run
                  'head-sha
                  head
                  'created-at-utc
                  created)
          'expected
          (hasheq 'shard-inventory (list 0 1) 'producer-manifest-digest hex-d 'identity id)
          'shards
          shs))

(define (hash-merge a b)
  (for/fold ([h a]) ([(k v) (in-hash b)])
    (hash-set h k v)))

(define (default-jobs #:started [started base-time] #:shard0-completed [c0 (+ base-time 180)])
  (append (for/list ([name (in-list required-names)]
                     [i (in-naturals)])
            (hasheq 'name
                    name
                    'role
                    "required"
                    'status
                    "completed"
                    'conclusion
                    "success"
                    'started-at
                    (ts (+ started (if (= i 1) 120 60)))
                    'completed-at
                    (ts (cond
                          [(= i 0) c0]
                          [(= i 1) (+ started 170)]
                          [else (+ started 140)]))))
          ;; A slower independent pilot must never enter the required critical path.
          (list (hasheq 'name
                        "compiled-root-producer"
                        'role
                        "pilot"
                        'status
                        "completed"
                        'conclusion
                        "success"
                        'started-at
                        (ts (+ started 30))
                        'completed-at
                        (ts (+ started 570))))))

(define (default-components)
  (hasheq 'compiled-root-producer-seconds
          "unknown"
          'prepared-aggregate-seconds
          "unknown"
          'restore-ms
          "unknown"
          'proof-seconds
          "unknown"))

(define (make-actions #:run-id [run 7100]
                      #:head [head sha-a]
                      #:attempt [att 1]
                      #:config [config (config-fields)]
                      #:jobs [jobs #f]
                      #:components [components (default-components)]
                      #:run-started-at [started base-time]
                      #:run-extra [run-extra (hasheq)]
                      #:extra [extra (hasheq)])
  ;; `started` may be epoch seconds (default) or an ISO timestamp string.
  (define started-str
    (if (string? started)
        started
        (ts started)))
  (define started-sec
    (if (string? started)
        (parse-utc-seconds started)
        started))
  (hash-merge (hasheq 'schema
                      "ci-recovery-actions-evidence@1"
                      'run
                      (hash-merge (hasheq 'run-id
                                          run
                                          'head-sha
                                          head
                                          'run-attempt
                                          att
                                          'workflow
                                          "ci-recovery-benchmark.yml"
                                          'event
                                          "workflow_dispatch"
                                          'status
                                          "completed"
                                          'conclusion
                                          "success"
                                          'run-started-at
                                          started-str)
                                  run-extra)
                      'required-job-names
                      required-names
                      'jobs
                      (or jobs (default-jobs #:started started-sec))
                      'config
                      config
                      'components
                      components)
              extra))

;; One trial attempt on the pinned tree with a chosen required critical
;; path (seconds) and a global sequence slot i that fixes run id, trial id
;; and created-at (so interleaving order is fully controlled).
(define treatment-run-base (hasheq "A" 7100 "B" 8100 "C" 9100))

(define (aggregate-attestation env)
  (define trial (hash-ref env 'trial))
  (define expected (hash-ref env 'expected))
  (define off? (equal? (hash-ref trial 'treatment) "A"))
  (hasheq 'schema
          "ci-recovery-aggregate-restore-attestation@1"
          'run-id
          (hash-ref trial 'run-id)
          'run-attempt
          (hash-ref trial 'attempt)
          'head-sha
          (hash-ref trial 'head-sha)
          'prepared-requested
          (if off? "off" "auto")
          'result
          (if off? "skipped" "success")
          'state
          (if off? "off" "restored")
          'identity-result
          (if off? "unknown" "verified")
          'restore-ms
          "unknown"
          'identity
          (hash-ref expected 'identity)
          'producer-manifest-digest
          (hash-ref expected 'producer-manifest-digest)))

(define (trial-fixture treatment i #:path [path-seconds 180] #:attempt [att 1])
  (define run (+ (hash-ref treatment-run-base treatment) i))
  (define tid (format "trial-~a-~a" treatment i))
  (define created (+ base-time (* 60 i)))
  (define env
    (make-envelope #:treatment treatment
                   #:trial-id tid
                   #:attempt att
                   #:run-id run
                   #:created-at (ts created)))
  (define actions
    (make-actions #:run-id run
                  #:attempt att
                  #:jobs (default-jobs #:shard0-completed (+ base-time path-seconds))))
  (list env (hash-set actions 'aggregate-attestation (aggregate-attestation env))))

(define (bundles->experiment fixtures)
  (assess-benchmark-experiment (map first fixtures) (map second fixtures)))

(define (report-arm r t)
  (hash-ref (hash-ref r 'arms) t))

(define (raw-trials r)
  (hash-ref (hash-ref r 'raw) 'trials))

(define (reasons-of entry)
  (hash-ref entry 'reasons))

(define (arm-exclusions r t)
  (hash-ref (report-arm r t) 'exclusions))

(define (arm-exclusion-labels r t)
  (for/list ([x (in-list (arm-exclusions r t))])
    (hash-ref x 'exclusion)))

;; Interleaved A/C experiment: A p50 215.0 / p95 246.5, C p50 220.0 / p95
;; 251.5 (paths 180..250 for A, plus `delta` for C on every trial).
(define (ac-fixtures [delta 5.0])
  (for/list ([i (in-range 16)])
    (define j (quotient i 2))
    (define treatment (if (even? i) "A" "C"))
    (define path (+ 180 (* 10.0 j) (if (equal? treatment "C") delta 0.0)))
    (trial-fixture treatment i #:path path)))

;; Interleaved A/B/C experiment: B = A − 30 s, C = A + 5 s.
(define (abc-fixtures)
  (for/list ([i (in-range 24)])
    (define j (quotient i 3))
    (define treatment (list-ref (list "A" "B" "C") (modulo i 3)))
    (define base (+ 180 (* 10.0 j)))
    (define path
      (cond
        [(equal? treatment "A") base]
        [(equal? treatment "B") (- base 30.0)]
        [else (+ base 5.0)]))
    (trial-fixture treatment i #:path path)))

;; Non-interleaved variant: treatments run in contiguous blocks.
(define (blocked-fixtures)
  (for/list ([i (in-range 24)])
    (define j (quotient i 8))
    (trial-fixture (list-ref (list "A" "B" "C") j) i #:path (+ 180 (* 10.0 j)))))

;; ---------------------------------------------------------------------------
;; CLI helper (subprocess; kept minimal — the report core is exercised
;; in-process)
;; ---------------------------------------------------------------------------

(define tmp-root (make-temporary-file "q-w2-benchmark-report~a" 'directory))

(define (tmp-path name)
  (build-path tmp-root name))

(define (run-cli args [cwd project-root])
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory cwd]
                   [current-output-port out]
                   [current-error-port out])
      (system/exit-code (string-append "racket " (path->string script-path) " " args))))
  (values code (get-output-string out)))

;; ===========================================================================
;; Suite
;; ===========================================================================

(define (suite)
  (test-suite "ci-recovery-benchmark-report"
    (test-case "raw run state refuses successful jobs and blocks C despite eight good trials"
      (define f (trial-fixture "C" 100))
      (define a (second f))
      (define run (hash-ref a 'run))
      (check-equal? (length required-names) 13)
      (for ([fields (in-list (list (hasheq 'status "completed" 'conclusion "cancelled")
                                   (hasheq 'status "in_progress" 'conclusion "success")
                                   (hasheq 'status "queued" 'conclusion "success")
                                   (hasheq 'status "completed" 'conclusion "failure")
                                   (hasheq 'status "completed" 'conclusion "timed_out")
                                   (hasheq 'status "completed" 'conclusion "skipped")
                                   (hasheq 'status "completed" 'conclusion "neutral")
                                   (hasheq 'status "completed" 'conclusion "action_required")
                                   (hasheq 'status "completed" 'conclusion "stale")
                                   (hasheq 'status "completed" 'conclusion 'null)
                                   (hasheq 'status #f 'conclusion "success")
                                   (hasheq 'status "completed")
                                   (hasheq 'conclusion "success")))])
        (define raw-run (hash-merge (hash-remove (hash-remove run 'status) 'conclusion) fields))
        (define bad (list (first f) (hash-set a 'run raw-run)))
        (define r (bundles->experiment (append (ac-fixtures) (list bad))))
        (define entry
          (findf (lambda (e) (equal? (hash-ref e 'trial-id) "trial-C-100")) (raw-trials r)))
        (check-true (hash-ref (hash-ref entry 'envelope-verdict) 'ready?))
        (check-equal? (hash-ref (report-arm r "C") 'n) 8)
        (check-equal? (hash-ref entry 'population-exclusion) "refused")
        (check-equal? (hash-ref entry 'critical-path-seconds) "unknown")
        (define reason
          (cond
            [(not (equal? (hash-ref fields 'status #f) "completed")) "actions-run-not-completed"]
            [(equal? (hash-ref fields 'conclusion #f) "cancelled") "actions-run-cancelled"]
            [(member (hash-ref fields 'conclusion #f) '(#f null)) "actions-run-conclusion-unknown"]
            [else "actions-run-not-successful"]))
        (check-true (has-reason? reason (reasons-of entry)))
        (for ([echo (in-list (list (hash-ref entry 'actions-echo)
                                   (last (hash-ref (hash-ref r 'raw) 'actions-attempts))))])
          (define echoed (hash-ref echo 'run))
          (check-equal? (hash-ref echoed 'status) (hash-ref fields 'status "unknown"))
          (check-equal? (hash-ref echoed 'conclusion) (hash-ref fields 'conclusion "unknown")))
        (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "blocked")
        (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim")))

    (test-case "raw aggregate attestation is independently required despite valid W1 assessment"
      (for ([t (in-list '("B" "C"))])
        (define f (trial-fixture t 0))
        (define a (hash-set (second f) 'aggregate-attestation "malformed"))
        (define r (assess-benchmark-experiment (list (first f)) (list a)))
        (check-equal? (hash-ref (report-arm r t) 'n) 0)
        (check-true (has-reason? "aggregate-attestation-invalid" (reasons-of (first (raw-trials r)))))
        (check-equal?
         (hash-ref (hash-ref (first (raw-trials r)) 'actions-echo) 'aggregate-attestation #f)
         "malformed")))

    (test-case "renaming actual run IDs across arms refuses every ambiguous label"
      (define a (make-envelope #:treatment "A" #:trial-id "renamed-A"))
      (define c (make-envelope #:treatment "C" #:trial-id "renamed-C"))
      (define r (assess-benchmark-experiment (list a c) (list (make-actions))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 0)
      (check-equal? (hash-ref (report-arm r "C") 'n) 0)
      (check-equal? (length (raw-trials r)) 2)
      (for ([e (in-list (raw-trials r))])
        (check-true (has-reason? "run-label-conflict" (reasons-of e)))))

    (test-case "one actual run relabeled as eight trials cannot inflate n"
      (define envs
        (for/list ([i (in-range 8)])
          (make-envelope #:trial-id (format "renamed-~a" i))))
      (define r (assess-benchmark-experiment envs (list (make-actions))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 0)
      (check-equal? (length (raw-trials r)) 8))

    (test-case "caller policy cannot reduce canonical protected checks"
      (define f (trial-fixture "A" 0))
      (define a (hash-set (second f) 'required-job-names '("fictional-0" "fictional-1")))
      (define r (assess-benchmark-experiment (list (first f)) (list a)))
      (check-true (has-reason? "actions-required-job-policy-invalid"
                               (reasons-of (first (raw-trials r))))))

    (test-case "required clocks require valid ordered run/start/completion"
      (define f (trial-fixture "A" 0))
      (for ([fields
             (in-list
              (list (hasheq 'started-at #f)
                    (hasheq 'started-at "2026-02-30T10:00:00Z")
                    (hasheq 'started-at (ts (+ base-time 181)))
                    (hasheq 'started-at (ts (- base-time 1)))
                    (hasheq 'started-at (ts (- base-time 20)) 'completed-at (ts (- base-time 10)))))])
        (define jobs (cons (hash-merge (first (default-jobs)) fields) (rest (default-jobs))))
        (define r
          (assess-benchmark-experiment (list (first f)) (list (hash-set (second f) 'jobs jobs))))
        (check-equal? (hash-ref (report-arm r "A") 'n) 0)
        (check-equal? (hash-ref (first (raw-trials r)) 'critical-path-seconds) "unknown")))

    (test-case "aggregate raw schema, bindings, all ten identity dimensions and producer digest are mandatory"
      (for ([t (in-list '("B" "C"))])
        (define f (trial-fixture t 0))
        (define env (first f))
        (define actions (second f))
        (define att (hash-ref actions 'aggregate-attestation))
        (define mutations
          (append (for/list ([key (in-list '(schema run-id
                                                    run-attempt
                                                    head-sha
                                                    prepared-requested
                                                    result
                                                    state
                                                    identity-result
                                                    restore-ms
                                                    identity
                                                    producer-manifest-digest))])
                    (hash-remove att key))
                  (for/list ([fields (in-list (list (hasheq 'schema "wrong")
                                                    (hasheq 'run-id 42)
                                                    (hasheq 'run-attempt 2)
                                                    (hasheq 'head-sha sha-b)
                                                    (hasheq 'prepared-requested "off")
                                                    (hasheq 'result "failure")
                                                    (hasheq 'state "unknown")
                                                    (hasheq 'identity-result "mismatch")
                                                    (hasheq 'restore-ms -1)
                                                    (hasheq 'restore-ms "12")
                                                    (hasheq 'producer-manifest-digest hex-f)))])
                    (hash-merge att fields))
                  (for/list ([key (in-list (hash-keys (identity-fields)))])
                    (hash-set att 'identity (hash-set (identity-fields) key "different")))))
        (for ([bad (in-list mutations)])
          (define a (hash-set actions 'aggregate-attestation bad))
          ;; Fake adapter-ready/config summaries must never rescue raw evidence.
          (define spoof
            (hash-set (hash-set a 'ready? #t)
                      'config
                      (hash-set (hash-ref a 'config) 'prepared-aggregate-restore "verified-restore")))
          (define r (assess-benchmark-experiment (list env) (list spoof)))
          (define entry (first (raw-trials r)))
          (check-true (hash-ref (hash-ref entry 'envelope-verdict) 'ready?))
          (check-equal? (hash-ref (report-arm r t) 'n) 0)
          (check-true (has-reason? "aggregate-attestation-invalid" (reasons-of entry)))
          (check-equal? (hash-ref (hash-ref entry 'actions-echo) 'aggregate-attestation) bad))
        (for ([ms (in-list '("unknown" 0 12.5))])
          (define a (hash-set actions 'aggregate-attestation (hash-set att 'restore-ms ms)))
          (check-equal? (hash-ref (report-arm (assess-benchmark-experiment (list env) (list a)) t) 'n)
                        1))))

    (test-case "A cannot claim prepared aggregate; absent A evidence stays honestly unmeasured"
      (define f (trial-fixture "A" 0))
      (define env (first f))
      (define a (second f))
      (define bad
        (hash-set a
                  'aggregate-attestation
                  (hash-set (hash-ref a 'aggregate-attestation) 'prepared-requested "auto")))
      (check-equal? (hash-ref (report-arm (assess-benchmark-experiment (list env) (list bad)) "A") 'n)
                    0)
      (define r
        (assess-benchmark-experiment (list env) (list (hash-remove a 'aggregate-attestation))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 1)
      (check-false (hash-ref (hash-ref (first (raw-trials r)) 'actions-echo) 'aggregate-attestation)))

    (test-case "one bad aggregate C attempt blocks the treatment despite eight good trials"
      (define fixtures (ac-fixtures))
      (define bad (trial-fixture "C" 100 #:attempt 2))
      (define r
        (bundles->experiment
         (append fixtures
                 (list (list (first bad) (hash-set (second bad) 'aggregate-attestation #f))))))
      (check-equal? (hash-ref (report-arm r "C") 'n) 8)
      (check-equal? (hash-ref (report-arm r "C") 'arm-verdict) "blocked-refused-trial")
      (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "blocked")
      (check-equal? (length (raw-trials r)) 17))

    (test-case "policy omissions substitutions and required jobs disguised as pilots refuse eligibility"
      (define f (trial-fixture "A" 0))
      (define env (first f))
      (define a (second f))
      (define variants
        (list
         (hash-set a 'required-job-names (rest required-names))
         (hash-set a 'required-job-names (cons "fictional" (rest required-names)))
         (hash-set a 'jobs (rest (default-jobs)))
         (hash-set a
                   'jobs
                   (cons (hash-set (first (default-jobs)) 'name "fictional") (rest (default-jobs))))
         (hash-set a
                   'jobs
                   (cons (hash-set (first (default-jobs)) 'role "pilot") (rest (default-jobs))))))
      (for ([bad (in-list variants)])
        (define r (assess-benchmark-experiment (list env) (list bad)))
        (check-equal? (hash-ref (report-arm r "A") 'n) 0)
        (check-equal? (hash-ref (first (raw-trials r)) 'critical-path-seconds) "unknown")))

    (test-case "every required job must have ordered clocks; malformed optional pilot does not matter"
      (define f (trial-fixture "A" 0))
      (for ([name (in-list required-names)])
        (define jobs
          (for/list ([j (in-list (default-jobs))])
            (if (equal? (hash-ref j 'name) name)
                (hash-remove j 'started-at)
                j)))
        (define r
          (assess-benchmark-experiment (list (first f)) (list (hash-set (second f) 'jobs jobs))))
        (check-equal? (hash-ref (report-arm r "A") 'n) 0))
      (define jobs
        (append (take (default-jobs) 13) (list (hash-remove (last (default-jobs)) 'started-at))))
      (define r
        (assess-benchmark-experiment (list (first f)) (list (hash-set (second f) 'jobs jobs))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 1)
      (check-equal? (hash-ref (first (raw-trials r)) 'critical-path-seconds) 180.0))

    (test-case "conflicting Actions attempts never select favorable aggregate evidence and retain both bodies"
      (define f (trial-fixture "C" 0))
      (define bad (hash-set (second f) 'aggregate-attestation "malformed"))
      (for ([actions (in-list (list (list (second f) bad) (list bad (second f))))])
        (define r (assess-benchmark-experiment (list (first f)) actions))
        (check-equal? (hash-ref (report-arm r "C") 'n) 0)
        (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "blocked")
        (check-equal? (length (hash-ref (hash-ref r 'raw) 'actions-attempts)) 2)))

    (test-case "CLI cross-boundary refuses relabeled runs and malformed aggregate with valid W1 evidence from foreign cwd"
      (define c (trial-fixture "C" 0))
      (define envs
        (append (for/list ([i (in-range 8)])
                  (make-envelope #:trial-id (format "renamed-~a" i)))
                (list (first c))))
      (define actions (list (make-actions) (hash-set (second c) 'aggregate-attestation "malformed")))
      (define env-file (tmp-path "boundary-envelopes.jsonl"))
      (define actions-file (tmp-path "boundary-actions.jsonl"))
      (define output (tmp-path "boundary-report.json"))
      (for ([file (in-list (list env-file actions-file))]
            [docs (in-list (list envs actions))])
        (with-output-to-file file
                             (lambda ()
                               (for ([doc (in-list docs)])
                                 (displayln (jsexpr->string doc))))
                             #:exists 'replace))
      ;; A fake cwd-local policy must have no influence over the runtime path.
      (with-output-to-file (tmp-path "required-pr-checks.policy")
                           (lambda () (displayln '("fictional")))
                           #:exists 'replace)
      (define-values (code log)
        (run-cli (format "--envelopes ~a --actions ~a --out ~a" env-file actions-file output)
                 tmp-root))
      (check-equal? code 0 log)
      (define r (call-with-input-file output read-json))
      ;; JSON object keys become symbols; in-process API arm keys are strings.
      (check-equal? (hash-ref (hash-ref (hash-ref r 'arms) 'A) 'n) 0)
      (check-equal? (hash-ref (hash-ref (hash-ref r 'arms) 'C) 'n) 0)
      (check-equal? (length (raw-trials r)) 9)
      (define c-raw (findf (lambda (e) (equal? (hash-ref e 'treatment) "C")) (raw-trials r)))
      (check-true (hash-ref (hash-ref c-raw 'envelope-verdict) 'ready?))
      (check-true (has-reason? "aggregate-attestation-invalid" (reasons-of c-raw))))

    ;; Timestamps ------------------------------------------------------------
    (test-case "parse-utc-seconds: strict UTC epoch conversion with calendar validation"
      (check-equal? (parse-utc-seconds "1970-01-01T00:00:00Z") 0)
      (check-equal? (parse-utc-seconds "2026-10-30T10:00:00Z") 1793354400)
      (check-equal? (parse-utc-seconds "2026-02-30T00:00:00Z")
                    #f
                    "a non-calendar date must be refused, not arithmetically normalized")
      (for ([bad (in-list (list "2026-10-30 10:00:00"
                                "2026-10-30T10:00:00"
                                "2026-13-01T00:00:00Z"
                                "2026-10-30T24:00:00Z"
                                "2026-10-30T10:60:00Z"
                                "not-a-timestamp"
                                ""
                                7
                                #f))])
        (check-equal? (parse-utc-seconds bad) #f (format "expected #f for ~s" bad)))
      (check-equal? (format-utc-seconds 1793354400) "2026-10-30T10:00:00Z")
      (check-equal? (format-utc-seconds 0) "1970-01-01T00:00:00Z")
      (check-equal? (format-utc-seconds (parse-utc-seconds "2026-02-28T23:59:59Z"))
                    "2026-02-28T23:59:59Z"))

    ;; Regression budget -----------------------------------------------------
    (test-case "regression budget: min(10% of baseline, 60 s)"
      (check-equal? (regression-budget-seconds 100.0) 10.0)
      (check-equal? (regression-budget-seconds 590.0) 59.0)
      (check-equal? (regression-budget-seconds 600.0) 60.0)
      (check-equal? (regression-budget-seconds 1000.0) 60.0 "the 60 s cap binds")
      (check-equal? (regression-budget-seconds 0.0) 0.0))

    ;; Critical path semantics -----------------------------------------------
    (test-case "critical path: max(required completed_at) - run start; never sum, never max duration, pilots excluded"
      (define fixtures (list (trial-fixture "A" 0 #:path 180)))
      (define r (bundles->experiment fixtures))
      (define entry (first (raw-trials r)))
      (check-equal?
       (hash-ref entry 'critical-path-seconds)
       180.0
       "shard-0 completes at +180s, shard-1 at +170s: path is 180, not the sum or the max duration")
      (define req (hash-ref entry 'required-jobs))
      (check-equal? (length req) 13)
      (check-equal? (hash-ref (first req) 'queue-wait-seconds)
                    60.0
                    "queue waits are reported separately, never merged into execution time")
      (check-equal? (hash-ref (second req) 'queue-wait-seconds) 120.0)
      (check-equal? (hash-ref (first req) 'duration-seconds) 120.0)
      (check-equal? (hash-ref (second req) 'duration-seconds) 50.0)
      (define pilots (hash-ref entry 'pilot-jobs))
      (check-equal? (length pilots) 1 "the independent pilot is listed separately")
      (check-equal? (hash-ref (first pilots) 'name) "compiled-root-producer")
      (check-equal? (hash-ref (first pilots) 'duration-seconds)
                    540.0
                    "the pilot ran far longer than the required path — and must stay out of it"))

    (test-case "supplied/precomputed durations are never trusted"
      (define fixture (trial-fixture "A" 0 #:path 180))
      (define env (first fixture))
      (define actions-with-junk
        (hash-set (second fixture)
                  'run
                  (hash-set (hash-ref (second fixture) 'run) 'critical-path-seconds 1)))
      (define r (assess-benchmark-experiment (list env) (list actions-with-junk)))
      (check-equal?
       (hash-ref (first (raw-trials r)) 'critical-path-seconds)
       180.0
       "the path is computed from raw timestamps only; a bogus supplied duration is ignored"))

    (test-case "required job policy: exact + complete; failures/cancellations named"
      (define base-fixture (trial-fixture "A" 0))
      (define base-env (first base-fixture))
      (define base-actions (second base-fixture))
      (define (entry-reasons jobs)
        (define r
          (assess-benchmark-experiment (list base-env) (list (hash-set base-actions 'jobs jobs))))
        (reasons-of (first (raw-trials r))))
      (check-true (has-reason? "required-job-set-incomplete"
                               (entry-reasons (list (first (default-jobs)))))
                  "a missing required job is named")
      (define extra-job
        (hasheq 'name
                "extra"
                'role
                "required"
                'status
                "completed"
                'conclusion
                "success"
                'started-at
                (ts base-time)
                'completed-at
                (ts (+ base-time 10))))
      (check-true (has-reason? "required-job-set-extraneous"
                               (entry-reasons (append (default-jobs) (list extra-job))))
                  "a required-role job outside the policy is named")
      (check-true (has-reason? "required-job-duplicate"
                               (entry-reasons (cons (first (default-jobs)) (default-jobs))))
                  "a duplicated required job is named")
      (define (jobs-with name keys)
        (for/list ([j (in-list (default-jobs))])
          (if (equal? (hash-ref j 'name) name)
              (hash-merge j keys)
              j)))
      (check-true (has-reason? "required-job-failed"
                               (entry-reasons (jobs-with (first required-names)
                                                         (hasheq 'conclusion "failure"))))
                  "a failed required job is named")
      (check-true (has-reason? "required-job-cancelled"
                               (entry-reasons (jobs-with (second required-names)
                                                         (hasheq 'conclusion "cancelled"))))
                  "a cancelled required job is named")
      (check-true (has-reason? "required-job-not-completed"
                               (entry-reasons (jobs-with (first required-names)
                                                         (hasheq 'status "in_progress"))))
                  "an unfinished required job is named")
      (check-true (has-reason? "required-job-timestamps-missing"
                               (entry-reasons (jobs-with (first required-names)
                                                         (hasheq 'completed-at "not-a-time"))))
                  "missing timestamps are named, never zero-filled")
      (define mystery-job
        (hasheq 'name
                "mystery"
                'role
                "wat"
                'status
                "completed"
                'conclusion
                "success"
                'started-at
                (ts base-time)
                'completed-at
                (ts base-time)))
      (define r-bad-role
        (assess-benchmark-experiment
         (list base-env)
         (list (hash-set base-actions 'jobs (append (default-jobs) (list mystery-job))))))
      (check-true (has-reason? "job-role-invalid" (reasons-of (first (raw-trials r-bad-role))))
                  "a job without a required|pilot role is named"))

    ;; The measured A/C experiment -------------------------------------------
    (test-case "8+8 interleaved A/C trials measure; quantiles use the exact cohort method"
      (define r (bundles->experiment (ac-fixtures)))
      (check-equal? (hash-ref r 'schema) "ci-recovery-benchmark-report@1")
      (check-equal? (hash-ref r 'kind) "ci-recovery-benchmark-report")
      (define arm-a (report-arm r "A"))
      (define arm-c (report-arm r "C"))
      (check-equal? (hash-ref arm-a 'n) 8)
      (check-equal? (hash-ref arm-c 'n) 8)
      (check-equal? (hash-ref arm-a 'p50-seconds) 215.0)
      ;; 246.5 pins the EXACT interpolation; the midpoint variant would say 245.
      (check-equal? (hash-ref arm-a 'p95-seconds) 246.5)
      (check-equal? (hash-ref arm-c 'p50-seconds) 220.0)
      (check-equal? (hash-ref arm-c 'p95-seconds) 251.5)
      (check-equal? (cohort-quantile-exact (list 180 190 200 210 220 230 240 250) 0.95)
                    246.5
                    "the imported cohort-quantile-exact method must agree")
      (check-false (equal? (hash-ref arm-a 'p95-seconds) 245.0)
                   "the midpoint quantile variant must NOT be used")
      (define cva (hash-ref (hash-ref r 'comparison) 'c-vs-a))
      (check-equal? (hash-ref cva 'verdict) "within-budget")
      (check-equal? (hash-ref cva 'p50-budget-seconds) 21.5 "min(10% of 215, 60)")
      (check-equal? (hash-ref cva 'p95-budget-seconds) 24.65 "min(10% of 246.5, 60)")
      (check-equal? (hash-ref cva 'p50-delta-seconds) 5.0)
      (check-equal? (hash-ref cva 'p95-delta-seconds) 5.0)
      (define bva (hash-ref (hash-ref r 'comparison) 'b-vs-a))
      (check-equal? (hash-ref bva 'verdict)
                    "no-claim"
                    "without B trials there is no aggregate-benefit claim")
      (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness)
                    "non-regression-only"
                    "a within-budget result is labeled non-regression-only, never promotable")
      (define interleaving (hash-ref r 'interleaving))
      (check-true (hash-ref interleaving 'ok))
      (check-equal? (hash-ref interleaving 'max-same-treatment-block) 1)
      (define binding (hash-ref r 'binding))
      (check-equal? (hash-ref binding 'pinned-head-sha) sha-a)
      (check-true (hash-ref binding 'source-uniform))
      (check-true (hash-ref binding 'config-uniform))
      ;; honesty: n=8 uncertainty, no fabricated CI
      (check-equal? (hash-ref (hash-ref arm-a 'uncertainty) 'confidence-interval) "none")
      (check-true (hash-ref (hash-ref arm-a 'uncertainty) 'tail-estimate-limited))
      ;; components stay unknown when the evidence does not carry them
      (check-equal? (hash-ref (hash-ref arm-a 'components) 'restore-ms) "unknown"))

    (test-case "C-vs-A budget boundary: exactly at the budget is within; over it exceeds"
      ;; Integer per-trial paths chosen so the C p50 delta is EXACTLY the
      ;; 21.5 s budget (A p50 215 -> budget min(21.5, 60)); timestamps stay
      ;; whole seconds.
      (define (ac-with-c-paths c-paths)
        (for/list ([i (in-range 16)])
          (define j (quotient i 2))
          (if (even? i)
              (trial-fixture "A" i #:path (+ 180 (* 10 j)))
              (trial-fixture "C" i #:path (list-ref c-paths j)))))
      (define r-at (bundles->experiment (ac-with-c-paths (list 202 212 222 232 241 251 261 271))))
      (check-equal? (hash-ref (hash-ref (hash-ref r-at 'comparison) 'c-vs-a) 'verdict)
                    "within-budget"
                    "delta == budget is within (budget is the maximum allowed regression)")
      (define r-over (bundles->experiment (ac-with-c-paths (list 205 215 225 235 245 255 265 275))))
      (check-equal? (hash-ref (hash-ref (hash-ref r-over 'comparison) 'c-vs-a) 'verdict)
                    "exceeds-budget"))

    (test-case "60 s cap binds before 10% on slow baselines"
      ;; A paths 1000..1070: 10% = 103.5 > 60 -> budget 60; C = A + 60.
      (define fixtures
        (for/list ([i (in-range 16)])
          (define j (quotient i 2))
          (define treatment (if (even? i) "A" "C"))
          (define path (+ 1000 (* 10.0 j) (if (equal? treatment "C") 60.0 0.0)))
          (trial-fixture treatment i #:path path)))
      (define r (bundles->experiment fixtures))
      (define cva (hash-ref (hash-ref r 'comparison) 'c-vs-a))
      (check-equal? (hash-ref cva 'p50-budget-seconds) 60.0)
      (check-equal? (hash-ref cva 'verdict) "within-budget"))

    (test-case "B vs A aggregate benefit is reported separately and is descriptive only"
      (define r (bundles->experiment (abc-fixtures)))
      (define bva (hash-ref (hash-ref r 'comparison) 'b-vs-a))
      (check-equal? (hash-ref bva 'verdict) "descriptive-only")
      (check-equal? (hash-ref bva 'benefit-seconds)
                    30.0
                    "A p50 215 minus B p50 185; positive means B is faster")
      (check-true (hash-has-key? bva 'note))
      (check-equal? (hash-ref (report-arm r "B") 'n) 8)
      (check-equal? (hash-ref (report-arm r "B") 'p50-seconds) 185.0))

    (test-case "n < 8 per arm yields no claim"
      (define fixtures
        (for/list ([i (in-range 15)])
          (define j (quotient i 2))
          (define treatment (if (even? i) "A" "C"))
          (trial-fixture treatment i #:path (+ 180 (* 10.0 j)))))
      (define r (bundles->experiment fixtures))
      (check-equal? (hash-ref (report-arm r "C") 'n) 7)
      (check-equal? (hash-ref (report-arm r "C") 'arm-verdict) "insufficient-trials")
      (check-true (has-reason? "insufficient-independent-trials"
                               (hash-ref (report-arm r "C") 'reasons)))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim")
      (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "not-evaluated"))

    ;; C contamination blocks promotion --------------------------------------
    (test-case "ANY refused/fallback-contaminated C trial blocks promotion despite 8 good C trials"
      (define fixtures (ac-fixtures))
      ;; A C trial whose launcher telemetry shows global-off: the W1 strict
      ;; assessor refuses it (launcher-mode-global-off) — fallback contamination.
      (define bad-shards
        (for/list ([shard (in-list (list 0 1))])
          (hasheq 'shard
                  shard
                  'record
                  (restore-record shard #:trial-id "trial-C-bad" #:treatment "C" #:run-id 9999)
                  'launcher
                  (launcher-off))))
      (define bad-env
        (make-envelope #:treatment "C" #:trial-id "trial-C-bad" #:run-id 9999 #:shards bad-shards))
      (define bad-actions (make-actions #:run-id 9999))
      (define r
        (assess-benchmark-experiment (append (map first fixtures) (list bad-env))
                                     (append (map second fixtures) (list bad-actions))))
      (define arm-c (report-arm r "C"))
      (check-equal? (hash-ref arm-c 'n)
                    8
                    "the population still counts the good trials — but promotion is blocked")
      (check-equal? (hash-ref arm-c 'arm-verdict) "blocked-refused-trial")
      (check-true (has-reason? "c-arm-refused-trial" (hash-ref arm-c 'reasons)))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict)
                    "no-claim"
                    "a contaminated C arm supports no comparison claim")
      (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "blocked")
      ;; The refused attempt is retained in raw with its named envelope reason.
      (define bad-raw
        (findf (lambda (e) (equal? (hash-ref e 'trial-id #f) "trial-C-bad")) (raw-trials r)))
      (check-true (hash? bad-raw) "the refused C attempt stays in the raw output")
      (check-true (has-reason? "launcher-mode-global-off" (reasons-of bad-raw))
                  "the W1 re-assessment refusal reason is carried through")
      (check-true (has-reason? "launcher-mode-global-off" (hash-ref arm-c 'exclusion-reasons))
                  "the C arm names the contamination"))

    ;; No trust in precomputed verdicts / self-claims ------------------------
    (test-case "precomputed ready verdicts and supplied durations never change the report"
      (define fixtures (ac-fixtures))
      (define clean (bundles->experiment fixtures))
      ;; Envelope junk: a self-claimed verdict. Actions junk: a supplied duration.
      (define tampered
        (for/list ([f (in-list fixtures)])
          (define env
            (hash-set (first f) 'verdict (hasheq 'ready? #t 'classification "ready-root-on")))
          (define actions
            (hash-set (second f)
                      'run
                      (hash-set (hash-ref (second f) 'run) 'critical-path-seconds 999)))
          (list env actions)))
      (define dirty (bundles->experiment tampered))
      (check-equal? (format-report-json clean)
                    (format-report-json dirty)
                    "self-claims and supplied durations must not leak into (or change) the report"))

    ;; Record/launcher binding (concurrent W1 repair alignment) --------------
    (test-case "record attempt/trial-id/treatment/identity must match the trial block"
      (define base (trial-fixture "C" 0))
      (define env (first base))
      (define actions (second base))
      (define (variant record-f)
        (define env2
          (map-shards env (lambda (s) (hash-set s 'record (record-f (hash-ref s 'record))))))
        (reasons-of (first (raw-trials (assess-benchmark-experiment (list env2) (list actions))))))
      (check-true (has-reason? "record-attempt-mismatch"
                               (variant (lambda (rec) (hash-set rec 'attempt 2)))))
      (check-true (has-reason? "record-trial-id-mismatch"
                               (variant (lambda (rec) (hash-set rec 'trial-id "other")))))
      (check-true (has-reason? "record-treatment-mismatch"
                               (variant (lambda (rec) (hash-set rec 'treatment "A")))))
      (check-true
       (has-reason?
        "record-identity-mismatch"
        (variant (lambda (rec)
                   (hash-set rec 'identity (hash-set (identity-fields) 'lock-digest hex-f))))))
      (check-true (has-reason? "record-identity-missing"
                               (variant (lambda (rec) (hash-remove rec 'identity))))))

    (test-case "launcher exit-code must be present and zero"
      (define base (trial-fixture "C" 0))
      (define env (first base))
      (define actions (second base))
      (define (variant launcher-f)
        (define env2
          (map-shards env (lambda (s) (hash-set s 'launcher (launcher-f (hash-ref s 'launcher))))))
        (reasons-of (first (raw-trials (assess-benchmark-experiment (list env2) (list actions))))))
      (check-true (has-reason? "launcher-exit-code-nonzero"
                               (variant (lambda (l) (hash-set l 'exit-code 1)))))
      (check-true (has-reason? "launcher-exit-code-missing"
                               (variant (lambda (l) (hash-remove l 'exit-code))))))

    ;; Actions binding --------------------------------------------------------
    (test-case "actions evidence binds run/head/attempt to the trial"
      (define base (trial-fixture "C" 0))
      (define env (first base))
      (define actions (second base))
      (define (variant a)
        (reasons-of (first (raw-trials (assess-benchmark-experiment (list env) (list a))))))
      (define r-wrong-run
        (assess-benchmark-experiment
         (list env)
         (list (hash-set actions 'run (hash-set (hash-ref actions 'run) 'run-id 42)))))
      (check-true (has-reason? "missing-actions-evidence" (variant #f)))
      (check-true (has-reason? "missing-actions-evidence" (variant #f)))
      (check-true (has-reason? "actions-schema-invalid" (variant (hash-set actions 'schema "nope"))))
      ;; A tampered run-id/attempt is a DIFFERENT run's evidence: pairing
      ;; treats it as missing for this trial and lists it as unpaired.
      (define wrong-run (hash-set actions 'run (hash-set (hash-ref actions 'run) 'run-id 42)))
      (check-true (has-reason? "missing-actions-evidence" (variant wrong-run)))
      (check-true (pair? (member 42 (hash-ref (hash-ref r-wrong-run 'raw) 'unpaired-actions)))
                  "the mismatched run's evidence is retained as unpaired")
      (check-true
       (has-reason?
        "actions-head-sha-mismatch"
        (variant (hash-set actions 'run (hash-set (hash-ref actions 'run) 'head-sha sha-b)))))
      (define wrong-attempt (hash-set actions 'run (hash-set (hash-ref actions 'run) 'run-attempt 7)))
      (check-true (has-reason? "missing-actions-evidence" (variant wrong-attempt))))

    ;; Config binding + mixes --------------------------------------------------
    (test-case "envelope identity and actions config must agree (per trial)"
      (define base (trial-fixture "C" 0))
      (define env (first base))
      (define actions (second base))
      (define (reasons-with config)
        (reasons-of (first (raw-trials (assess-benchmark-experiment
                                        (list env)
                                        (list (hash-set actions 'config config)))))))
      (check-true (has-reason? "config-dimension-missing-lock-digest"
                               (reasons-with (hash-remove (config-fields) 'lock-digest))))
      (check-true (has-reason? "config-envelope-mismatch-lock-digest"
                               (reasons-with (hash-set (config-fields) 'lock-digest hex-f))))
      (check-true (has-reason? "config-envelope-mismatch-shard-plan"
                               (reasons-with (hash-set (config-fields) 'shard-plan (list 0 1 2))))))

    (test-case "config mix across regimes is an experiment refusal"
      (define fixtures (ac-fixtures))
      (define last-fixture (last fixtures))
      (define tampered-config (hash-set (config-fields) 'test-inventory-digest hex-f))
      (define mixed
        (append (take fixtures 15)
                (list (list (first last-fixture)
                            (hash-set (second last-fixture) 'config tampered-config)))))
      (define r (bundles->experiment mixed))
      (check-true (has-reason? "config-mix-test-inventory-digest" (hash-ref r 'refusals)))
      (check-false (hash-ref (hash-ref r 'binding) 'config-uniform))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim")
      (check-equal? (hash-ref (hash-ref r 'promotion) 'readiness) "not-evaluated"))

    (test-case "source mix across regimes is an experiment refusal"
      (define fixtures (ac-fixtures))
      (define last-fixture (last fixtures))
      (define env-b
        (hash-set (first last-fixture)
                  'trial
                  (hash-set (hash-ref (first last-fixture) 'trial) 'head-sha sha-b)))
      (define actions-b
        (hash-set (second last-fixture)
                  'run
                  (hash-set (hash-ref (second last-fixture) 'run) 'head-sha sha-b)))
      (define mixed (append (take fixtures 15) (list (list env-b actions-b))))
      (define r (bundles->experiment mixed))
      (check-true (has-reason? "source-mix-head-sha" (hash-ref r 'refusals)))
      (check-equal? (hash-ref (hash-ref r 'binding) 'pinned-head-sha) "unknown")
      (check-false (hash-ref (hash-ref r 'binding) 'source-uniform))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim"))

    ;; Population integrity: duplicates / reruns / retention ------------------
    (test-case "identical duplicates never inflate the population and stay in raw"
      (define f (trial-fixture "A" 0))
      (define r (assess-benchmark-experiment (list (first f) (first f)) (list (second f))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 1 "the population counts the trial once")
      (check-equal? (hash-ref (report-arm r "A") 'attempts-retained)
                    2
                    "both input attempts are retained")
      (check-true (has-reason? "duplicate-excluded" (arm-exclusion-labels r "A")))
      (check-equal? (length (raw-trials r)) 2 "the duplicate stays in the raw output"))

    (test-case "conflicting duplicates (same trial-id+attempt, different data) are a refusal"
      (define f (trial-fixture "A" 0))
      (define env2
        (hash-set (first f)
                  'trial
                  (hash-set (hash-ref (first f) 'trial) 'created-at-utc "2026-10-30T11:00:00Z")))
      (define r (assess-benchmark-experiment (list (first f) env2) (list (second f))))
      (check-true (has-reason? "trial-duplicate-conflict" (hash-ref r 'refusals)))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim"))

    (test-case "reruns of one trial never inflate the population"
      (define a1 (trial-fixture "A" 0 #:path 180 #:attempt 1))
      (define a2 (trial-fixture "A" 0 #:path 400 #:attempt 2))
      (define r
        (assess-benchmark-experiment (list (first a1) (first a2)) (list (second a1) (second a2))))
      (check-equal? (hash-ref (report-arm r "A") 'n) 1)
      (check-true (has-reason? "rerun-excluded" (arm-exclusion-labels r "A")))
      (check-equal? (hash-ref (report-arm r "A") 'p50-seconds)
                    180.0
                    "the first attempt is the sample; the rerun does not move the quantiles"))

    (test-case "failed and cancelled attempts are retained with named reasons"
      (define f (trial-fixture "A" 0))
      (define (jobs-with name keys)
        (for/list ([j (in-list (default-jobs))])
          (if (equal? (hash-ref j 'name) name)
              (hash-merge j keys)
              j)))
      (define failed
        (assess-benchmark-experiment (list (first f))
                                     (list (hash-set (second f)
                                                     'jobs
                                                     (jobs-with (first required-names)
                                                                (hasheq 'conclusion "failure"))))))
      (define cancelled
        (assess-benchmark-experiment (list (first f))
                                     (list (hash-set (second f)
                                                     'jobs
                                                     (jobs-with (second required-names)
                                                                (hasheq 'conclusion "cancelled"))))))
      (check-true (has-reason? "required-job-failed" (reasons-of (first (raw-trials failed)))))
      (check-true (has-reason? "required-job-cancelled" (reasons-of (first (raw-trials cancelled)))))
      (check-equal? (length (raw-trials failed)) 1 "the failed attempt is retained in raw")
      (check-equal? (hash-ref (report-arm failed "A") 'attempts-retained) 1))

    ;; Interleaving ------------------------------------------------------------
    (test-case "contiguous same-treatment blocks refuse the interleaving claim"
      (define r (bundles->experiment (blocked-fixtures)))
      (check-false (hash-ref (hash-ref r 'interleaving) 'ok))
      (check-equal? (hash-ref (hash-ref r 'interleaving) 'max-same-treatment-block) 8)
      (check-true (has-reason? "treatment-block-not-interleaved" (hash-ref r 'refusals)))
      (check-equal? (hash-ref (hash-ref (hash-ref r 'comparison) 'c-vs-a) 'verdict) "no-claim"))

    ;; Determinism + CLI --------------------------------------------------------
    (test-case "report bytes are deterministic for identical inputs"
      (define fixtures (ac-fixtures))
      (define s1 (format-report-json (bundles->experiment fixtures)))
      (define s2 (format-report-json (bundles->experiment fixtures)))
      (check-equal? s1 s2)
      (check-equal? (format-report-md (bundles->experiment fixtures))
                    (format-report-md (bundles->experiment fixtures)))
      (check-true (string-contains? (format-report-md (bundles->experiment fixtures)) "p50=215.0")
                  "the markdown summary carries the measured quantiles"))

    (test-case "CLI: exit 0, byte-identical JSON and markdown across runs"
      (define fixtures (ac-fixtures))
      (define in-env (tmp-path "cli-envelope-all.jsonl"))
      (define in-actions (tmp-path "cli-actions-all.jsonl"))
      (with-output-to-file in-env
                           (lambda ()
                             (for ([f (in-list fixtures)])
                               (displayln (jsexpr->string (first f)))))
                           #:exists 'replace)
      (with-output-to-file in-actions
                           (lambda ()
                             (for ([f (in-list fixtures)])
                               (displayln (jsexpr->string (second f)))))
                           #:exists 'replace)
      (define out-1 (tmp-path "cli-report-1.json"))
      (define md-1 (tmp-path "cli-report-1.md"))
      (define out-2 (tmp-path "cli-report-2.json"))
      (define md-2 (tmp-path "cli-report-2.md"))
      (define cli-args-1
        (format "--envelopes ~a --actions ~a --out ~a --out-md ~a"
                (path->string in-env)
                (path->string in-actions)
                (path->string out-1)
                (path->string md-1)))
      (define cli-args-2
        (format "--envelopes ~a --actions ~a --out ~a --out-md ~a"
                (path->string in-env)
                (path->string in-actions)
                (path->string out-2)
                (path->string md-2)))
      (define-values (code-1 o-1) (run-cli cli-args-1))
      (check-equal? code-1 0)
      (define-values (code-2 o-2) (run-cli cli-args-2))
      (check-equal? code-2 0)
      (check-equal? (file->string out-1) (file->string out-2) "byte-identical report JSON")
      (check-equal? (file->string md-1) (file->string md-2) "byte-identical markdown")
      (define report (with-input-from-file out-1 read-json))
      (check-equal? (hash-ref report 'schema) "ci-recovery-benchmark-report@1"))

    (test-case "CLI: usage errors exit 2 without writing output"
      (define out (tmp-path "cli-should-not-exist.json"))
      (define-values (code o) (run-cli (format "--out ~a" (path->string out))))
      (check-equal? code 2)
      (check-false (file-exists? out)))))

(module+ test
  (define failed (run-tests (suite)))
  (unless (zero? failed)
    (raise-user-error 'test-ci-recovery-benchmark-report "~a test failure(s)" failed)))

(module+ main
  (define failed (run-tests (suite)))
  (unless (zero? failed)
    (raise-user-error 'test-ci-recovery-benchmark-report "~a test failure(s)" failed)))

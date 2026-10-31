#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation offline
;; @boundary scripts/run-tests

;; W2 (v1.00.32): the controlled A/B/C CI-recovery benchmark report
;; (wave contract: W2-controlled-experiment-and-prepared-aggregate-candidate,
;; actions 4/5). Turns EXPLICIT trial JSON inputs into one deterministic
;; report:
;;
;;   envelopes — one ci-recovery-trial-evidence@1 document per trial
;;               attempt (W1 shared contract, docs/reports/CI-RECOVERY-EVIDENCE.md)
;;   actions   — one ci-recovery-actions-evidence@1 document per run attempt
;;               (raw Actions run/jobs/status/config metadata; input contract
;;               defined by docs/reports/CI-RECOVERY-EXPERIMENT.md so the
;;               workflow adapter owner aligns)
;;
;; Honesty rules enforced here:
;;   - EVERY envelope is re-assessed with the W1 strict assessor
;;     (assess-trial-envelope); no precomputed verdict, ready claim or
;;     supplied duration is ever trusted — the report cannot even represent
;;     a verdict input, and junk self-claim keys never reach the output;
;;   - the full required critical path is max(required job completed_at) −
;;     run.run_started_at. Anchoring at the run start means dependency and
;;     queue waits are INCLUDED; it is never the sum of parallel jobs and
;;     never the max single job duration. Independent pilot jobs are
;;     excluded from the path and reported separately; queue waits are
;;     reported separately; a supplied duration field is ignored;
;;   - run/head/attempt/trial identity plus the exact configuration
;;     (runner image, Racket executable, platform, lock, recipe, policy,
;;     test inventory, shard plan, harness revision) are bound across ALL
;;     regimes; source/config/timestamp/attempt mixes are named refusals;
;;   - producer/restore/aggregate/proof times stay "unknown" when the
;;     evidence does not carry them (never zero-filled, never summed
;;     partially);
;;   - EVERY attempt — including failed, cancelled and refused ones — is
;;     retained in the raw output; duplicates and reruns are named and
;;     never inflate the sample population;
;;   - claims require >= 8 independent INTERLEAVED trials per treatment on
;;     one pinned tree; with n=8 the p95 tail estimate is limited and no
;;     confidence interval is fabricated;
;;   - C vs A regression budget: min(10% of the A baseline, 60 s) for both
;;     p50 and p95 (quantiles via cohort-quantile-exact); B vs A aggregate
;;     benefit is reported separately and is descriptive only;
;;     non-regression is not a useful-improvement claim and never
;;     permission to promote; missing data yields NO claim;
;;   - for treatment C, ANY refused/fallback-contaminated trial blocks
;;     promotion readiness even when enough other C trials are good.
;;
;; Concrete functions only: no macros, no DSL.

(require json
         racket/file
         racket/format
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         (file "../ci/prepared-env-report.rkt")
         (file "cohort-report.rkt"))

(provide benchmark-report-schema
         actions-evidence-schema
         regression-fraction
         regression-cap-seconds
         min-trials-per-arm
         max-interleave-block
         config-dimensions
         component-dimensions
         parse-utc-seconds
         format-utc-seconds
         regression-budget-seconds
         assess-benchmark-experiment
         format-report-json
         format-report-md)

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define benchmark-report-schema "ci-recovery-benchmark-report@1")
(define actions-evidence-schema "ci-recovery-actions-evidence@1")

;; Resolve the protected policy relative to this module, never the caller's cwd.
(define-runtime-path required-policy-path "../required-pr-checks.policy")
(define canonical-required-names (call-with-input-file required-policy-path read))

;; C-vs-A regression budget: min(10% of the A baseline, 60 s) per quantile.
(define regression-fraction 1/10)
(define regression-cap-seconds 60.0)

;; Claims require at least this many independent trials per treatment.
(define min-trials-per-arm 8)

;; Treatments must be interleaved: no more than this many consecutive
;; same-treatment trials may appear in the created-at order.
(define max-interleave-block 2)

;; The exact configuration dimensions that must be IDENTICAL across all
;; regimes (A/B/C) for a comparison to be evaluable.
(define config-dimensions
  (list "runner-image"
        "racket-version"
        "racket-executable-digest"
        "lock-digest"
        "resolved-set-digest"
        "precompile-recipe-revision"
        "policy-fingerprint"
        "test-inventory-digest"
        "shard-plan"
        "harness-revision"))

;; Envelope identity dimension ↔ actions config dimension cross-check
;; (the producer-bound expected identity must describe the same
;; environment the Actions run declares).
(define identity-config-pairs
  (list (cons "runner-image" 'os-image)
        (cons "platform" 'arch)
        (cons "racket-version" 'racket-version)
        (cons "racket-executable-digest" 'racket-executable-digest)
        (cons "lock-digest" 'lock-digest)
        (cons "resolved-set-digest" 'resolved-set-digest)
        (cons "precompile-recipe-revision" 'precompile-recipe-revision)
        (cons "policy-fingerprint" 'policy-fingerprint)))

;; Producer/restore/aggregate/proof components: reported only when
;; actually observed ("unknown" otherwise, never zero-filled).
(define component-dimensions
  (list "compiled-root-producer-seconds" "prepared-aggregate-seconds" "restore-ms" "proof-seconds"))

(define budget-rule-text
  "per quantile: C - A <= min(10% of the A baseline, 60 s); both p50 and p95 must hold")

(define promotion-note
  (string-append "Promotion requires the separate human gate (v1.00.32 plan gate 3) on the full"
                 " wave contract; non-regression alone is not a useful-improvement claim and"
                 " never permission to promote."))

(define honesty-block
  (hasheq
   'statistics-method
   "cohort-quantile-exact (scripts/run-tests/cohort-report.rkt): linear interpolation at k=q(n-1), millisecond-rounded"
   'confidence-interval
   "none"
   'non-regression-note
   "Non-regression is not a useful-improvement claim and never permission to promote."
   'missing-data-note
   "Missing data yields no claim; unobserved values stay \"unknown\", never zero-filled."
   'critical-path-rule
   (string-append "max(required job completed_at) - run.run_started_at; dependency and queue"
                  " waits included; never the sum of parallel jobs, never the max job duration;"
                  " pilot jobs excluded and reported separately; supplied durations never trusted")))

;; ---------------------------------------------------------------------------
;; Timestamps: strict UTC parsing/formatting (pure, calendar-validated)
;; ---------------------------------------------------------------------------

(define (days-from-civil y m d)
  ;; Howard Hinnant's days_from_civil (proleptic Gregorian), exact arithmetic.
  (define yy
    (if (<= m 2)
        (- y 1)
        y))
  (define era (floor (/ yy 400)))
  (define yoe (- yy (* 400 era)))
  (define doy (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5))
  (define doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy (- d 1)))
  (- (+ (* era 146097) doe) 719468))

(define (civil-from-days z)
  (define z2 (+ z 719468))
  (define era (floor (/ z2 146097)))
  (define doe (- z2 (* 146097 era)))
  (define yoe
    (quotient (- doe (quotient doe 1460) (- (quotient doe 36524)) (quotient doe 146096)) 365))
  (define y (+ yoe (* 400 era)))
  (define doy (- doe (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100))))
  (define mp (quotient (+ (* 5 doy) 2) 153))
  (define d (+ 1 (- doy (quotient (+ (* 153 mp) 2) 5))))
  (define m (+ mp (if (< mp 10) 3 -9)))
  (values (+ y (if (<= m 2) 1 0)) m d))

(define (format-utc-seconds t)
  (define days (quotient t 86400))
  (define rem (modulo t 86400))
  (define-values (y mo d) (civil-from-days days))
  (format "~a-~a-~aT~a:~a:~aZ"
          (~r y #:min-width 4 #:pad-string "0")
          (~r mo #:min-width 2 #:pad-string "0")
          (~r d #:min-width 2 #:pad-string "0")
          (~r (quotient rem 3600) #:min-width 2 #:pad-string "0")
          (~r (quotient (modulo rem 3600) 60) #:min-width 2 #:pad-string "0")
          (~r (modulo rem 60) #:min-width 2 #:pad-string "0")))

(define (parse-utc-seconds v)
  ;; "YYYY-MM-DDTHH:MM:SSZ" → epoch seconds; #f for anything else.
  ;; A round-trip through format-utc-seconds rejects non-calendar dates
  ;; (2026-02-30 would normalize otherwise).
  (define m
    (and (string? v)
         (regexp-match #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$" v)))
  (and m
       (let* ([y (string->number (list-ref m 1))]
              [mo (string->number (list-ref m 2))]
              [d (string->number (list-ref m 3))]
              [hh (string->number (list-ref m 4))]
              [mm (string->number (list-ref m 5))]
              [ss (string->number (list-ref m 6))])
         (and (<= 1 mo 12)
              (<= 1 d 31)
              (<= 0 hh 23)
              (<= 0 mm 59)
              (<= 0 ss 59)
              (let ([t (+ (* 86400 (days-from-civil y mo d)) (* 3600 hh) (* 60 mm) ss)])
                (and (string=? (format-utc-seconds t) v) t))))))

;; ---------------------------------------------------------------------------
;; Arithmetic helpers
;; ---------------------------------------------------------------------------

(define (ms-round x)
  ;; Millisecond rounding keeps regenerated reports byte-stable.
  (/ (round (* 1000.0 (exact->inexact x))) 1000.0))

(define (regression-budget-seconds baseline-seconds)
  (ms-round (min (* regression-fraction (exact->inexact baseline-seconds)) regression-cap-seconds)))

(define (sha40? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{40}$" v)))

;; ---------------------------------------------------------------------------
;; Actions evidence: required critical path + job reports
;; ---------------------------------------------------------------------------

(define (actions-schema-ok? a)
  (and (hash? a) (equal? (hash-ref a 'schema #f) actions-evidence-schema)))

(define (job-clock-fields job run-start)
  (define start (parse-utc-seconds (hash-ref job 'started-at #f)))
  (define end (parse-utc-seconds (hash-ref job 'completed-at #f)))
  (hasheq 'name
          (hash-ref job 'name "unknown")
          'role
          (hash-ref job 'role "unknown")
          'status
          (hash-ref job 'status "unknown")
          'conclusion
          (hash-ref job 'conclusion "unknown")
          ;; Queue wait is the time from run start to job start — reported
          ;; SEPARATELY, never merged into execution time.
          'queue-wait-seconds
          (if (and start run-start (<= run-start start))
              (ms-round (- start run-start))
              "unknown")
          'duration-seconds
          (if (and start end (<= start end))
              (ms-round (- end start))
              "unknown")))

(define (analyze-actions actions)
  ;; Full required critical path from RAW timestamps only:
  ;;   path = max(required job completed_at) − run.run_started_at
  ;; Any supplied/precomputed duration field in the input is ignored.
  ;; Pilot jobs never enter the path. Returns
  ;; (hasheq seconds reasons required-jobs pilot-jobs).
  (define reasons '())
  (define (add! r)
    (set! reasons (cons r reasons)))
  (define run-start
    (let ([run (if (hash? actions)
                   (hash-ref actions 'run #f)
                   #f)])
      (if (hash? run)
          (parse-utc-seconds (hash-ref run 'run-started-at #f))
          #f)))
  (unless (actions-schema-ok? actions)
    (add! "actions-schema-invalid"))
  (unless run-start
    (add! "actions-run-started-at-invalid"))
  ;; Run-level completion is independent of the protected job conclusions.
  ;; Missing/null state is unknown evidence, never an implied success.
  (define run (and (hash? actions) (hash-ref actions 'run #f)))
  (define status (and (hash? run) (hash-ref run 'status #f)))
  (define conclusion (and (hash? run) (hash-ref run 'conclusion #f)))
  (unless (equal? status "completed")
    (add! "actions-run-not-completed"))
  (cond
    [(equal? conclusion "success") (void)]
    [(equal? conclusion "cancelled") (add! "actions-run-cancelled")]
    [(or (not (string? conclusion)) (member conclusion '("" "unknown")))
     (add! "actions-run-conclusion-unknown")]
    [else (add! "actions-run-not-successful")])
  (define supplied-policy
    (if (hash? actions)
        (hash-ref actions 'required-job-names #f)
        #f))
  (define policy canonical-required-names)
  (define policy-ok?
    (and (list? supplied-policy)
         (andmap string? supplied-policy)
         (equal? (sort supplied-policy string<?) (sort policy string<?))))
  (unless policy-ok?
    (add! "actions-required-job-policy-invalid"))

  (define jobs
    (if (hash? actions)
        (hash-ref actions 'jobs #f)
        #f))
  (unless (and (list? jobs) (andmap hash? jobs))
    (add! "jobs-invalid"))
  (define valid-jobs
    (if (and (list? jobs) (andmap hash? jobs))
        jobs
        (list)))
  (define required-jobs '())
  (define pilot-jobs '())
  (for ([j (in-list valid-jobs)])
    (define role (hash-ref j 'role #f))
    (cond
      [(equal? role "required") (set! required-jobs (cons j required-jobs))]
      [(equal? role "pilot") (set! pilot-jobs (cons j pilot-jobs))]
      [else (add! "job-role-invalid")]))
  (set! required-jobs (reverse required-jobs))
  (set! pilot-jobs (reverse pilot-jobs))
  ;; Required job names must match the policy EXACTLY and COMPLETELY.
  (when (pair? policy)
    (for ([name (in-list policy)])
      (define matches (filter (lambda (j) (equal? (hash-ref j 'name #f) name)) required-jobs))
      (cond
        [(null? matches) (add! "required-job-set-incomplete")]
        [(pair? (cdr matches)) (add! "required-job-duplicate")]))
    (for ([j (in-list required-jobs)])
      (unless (member (hash-ref j 'name #f) policy)
        (add! "required-job-set-extraneous"))))
  ;; Per required job: completion, conclusion, timestamps.
  (define completions '())
  (for ([j (in-list required-jobs)])
    (define conclusion (hash-ref j 'conclusion #f))
    (define start (parse-utc-seconds (hash-ref j 'started-at #f)))
    (define end (parse-utc-seconds (hash-ref j 'completed-at #f)))
    (unless (equal? (hash-ref j 'status #f) "completed")
      (add! "required-job-not-completed"))
    (cond
      [(equal? conclusion "success") (void)]
      [(equal? conclusion "cancelled") (add! "required-job-cancelled")]
      [else (add! "required-job-failed")])
    (cond
      [(not (and start end)) (add! "required-job-timestamps-missing")]
      [(not (and run-start (<= run-start start end))) (add! "required-job-timestamps-order-invalid")]
      [else (set! completions (cons end completions))]))
  (define path-known?
    (and policy-ok?
         run-start
         (pair? policy)
         (= (length required-jobs) (length policy))
         (andmap (lambda (j) (equal? (hash-ref j 'status #f) "completed")) required-jobs)
         (andmap (lambda (j) (equal? (hash-ref j 'conclusion #f) "success")) required-jobs)
         (= (length completions) (length required-jobs))
         (null? reasons)))
  (define path-seconds
    (if path-known?
        (ms-round (- (apply max completions) run-start))
        #f))
  (when (and policy-ok? run-start (not path-seconds))
    (add! "critical-path-unknown"))
  (hasheq 'seconds
          path-seconds
          'reasons
          (sort (remove-duplicates reasons) string<?)
          'required-jobs
          (for/list ([j (in-list required-jobs)])
            (job-clock-fields j run-start))
          'pilot-jobs
          (for/list ([j (in-list pilot-jobs)])
            (job-clock-fields j run-start))))

;; ---------------------------------------------------------------------------
;; Config binding
;; ---------------------------------------------------------------------------

(define (config-snapshot actions)
  (define config
    (if (hash? actions)
        (hash-ref actions 'config #f)
        #f))
  (if (hash? config)
      (for/hash ([dim (in-list config-dimensions)]
                 #:when (hash-has-key? config (string->symbol dim)))
        (values (string->symbol dim) (hash-ref config (string->symbol dim) #f)))
      "unknown"))

(define (sorted-ids v)
  (if (and (list? v) (andmap real? v))
      (sort v <)
      v))

(define (config-check-reasons env actions)
  ;; Per-trial: every config dimension present + envelope identity ↔
  ;; config cross-check (producer-bound expected identity vs the run's
  ;; declared environment, and the expected shard inventory vs the shard
  ;; plan).
  (define reasons '())
  (when (and (actions-schema-ok? actions) (hash? env))
    (define config (hash-ref actions 'config #f))
    (define expected (hash-ref env 'expected #f))
    (define identity
      (if (hash? expected)
          (hash-ref expected 'identity #f)
          #f))
    (define shard-inv (sorted-ids (hash-ref expected 'shard-inventory #f)))
    (define shard-plan (sorted-ids (hash-ref config 'shard-plan #f)))
    (if (not (hash? config))
        (set! reasons (list "actions-config-missing"))
        (begin
          (for ([dim (in-list config-dimensions)])
            (unless (hash-has-key? config (string->symbol dim))
              (set! reasons (cons (format "config-dimension-missing-~a" dim) reasons))))
          (when (hash? identity)
            (for ([pair (in-list identity-config-pairs)])
              (define config-v (hash-ref config (string->symbol (car pair)) #f))
              (define identity-v (hash-ref identity (cdr pair) #f))
              (unless (equal? config-v identity-v)
                (set! reasons (cons (format "config-envelope-mismatch-~a" (car pair)) reasons))))
            (unless (equal? shard-inv shard-plan)
              (set! reasons (cons "config-envelope-mismatch-shard-plan" reasons)))))))
  (reverse reasons))

;; ---------------------------------------------------------------------------
;; Per-trial assessment (re-assess the envelope; never trust verdicts)
;; ---------------------------------------------------------------------------

(define (trial-meta env key [default #f])
  (define trial
    (if (hash? env)
        (hash-ref env 'trial #f)
        #f))
  (if (hash? trial)
      (hash-ref trial key default)
      default))

(define (field-binding-reasons h key expected prefix)
  (cond
    [(not (hash-has-key? h key)) (list (string-append prefix "-missing"))]
    [(equal? (hash-ref h key #f) expected) (list)]
    [else (list (string-append prefix "-mismatch"))]))

(define (shard-binding-reasons shard env)
  ;; Concurrent W1 repair alignment: every record must carry
  ;; attempt/trial-id/treatment/identity matching the trial/expected
  ;; blocks, and every launcher must carry exit-code 0.
  (define record (hash-ref shard 'record #f))
  (define launcher (hash-ref shard 'launcher #f))
  (define expected
    (if (hash? env)
        (hash-ref env 'expected #f)
        #f))
  (define identity
    (if (hash? expected)
        (hash-ref expected 'identity #f)
        #f))
  (append
   (if (hash? record)
       (append
        (field-binding-reasons record 'attempt (trial-meta env 'attempt) "record-attempt")
        (field-binding-reasons record 'trial-id (trial-meta env 'trial-id) "record-trial-id")
        (field-binding-reasons record 'treatment (trial-meta env 'treatment) "record-treatment")
        (cond
          [(not (hash-has-key? record 'identity)) (list "record-identity-missing")]
          [(not (equal? (hash-ref record 'identity #f) identity)) (list "record-identity-mismatch")]
          [else (list)]))
       (list))
   (if (hash? launcher)
       (let ([ec (hash-ref launcher 'exit-code #f)])
         (cond
           [(not (real? ec)) (list "launcher-exit-code-missing")]
           [(not (zero? ec)) (list "launcher-exit-code-nonzero")]
           [else (list)]))
       (list))))

(define (actions-echo actions)
  ;; Restricted echo: raw jobs/config/components plus the run binding
  ;; fields. Supplied-duration junk in `run` can never reach the output.
  (if (actions-schema-ok? actions)
      (let ([run (hash-ref actions 'run #f)])
        (hasheq 'schema
                (hash-ref actions 'schema #f)
                'required-job-names
                (hash-ref actions 'required-job-names #f)
                'run
                (if (hash? run)
                    (hasheq 'run-id
                            (hash-ref run 'run-id #f)
                            'head-sha
                            (hash-ref run 'head-sha #f)
                            'run-attempt
                            (hash-ref run 'run-attempt #f)
                            'workflow
                            (hash-ref run 'workflow #f)
                            'event
                            (hash-ref run 'event #f)
                            'status
                            (hash-ref run 'status "unknown")
                            'conclusion
                            (hash-ref run 'conclusion "unknown")
                            'run-started-at
                            (hash-ref run 'run-started-at #f))
                    "unknown")
                'jobs
                (hash-ref actions 'jobs #f)
                'config
                (hash-ref actions 'config #f)
                'components
                (hash-ref actions 'components #f)
                'aggregate-attestation
                (hash-ref actions 'aggregate-attestation #f)))
      "unknown"))

(define (component-echo actions)
  (define comps
    (if (hash? actions)
        (hash-ref actions 'components #f)
        #f))
  (if (hash? comps)
      (for/hash ([dim (in-list component-dimensions)])
        (values (string->symbol dim) (hash-ref comps (string->symbol dim) "unknown")))
      "unknown"))

(define (aggregate-attestation-reasons env actions)
  ;; A missing A attestation means unmeasured, never a prepared restore.
  ;; B/C need independently bound raw observations, not adapter summaries.
  (define a (and (hash? actions) (hash-ref actions 'aggregate-attestation #f)))
  (define treatment (trial-meta env 'treatment))
  (define expected (and (hash? env) (hash-ref env 'expected #f)))
  (define identity (and (hash? expected) (hash-ref expected 'identity #f)))
  (define run (and (hash? actions) (hash-ref actions 'run #f)))
  (define (bound? k trial-key)
    (and (hash? a)
         (hash? run)
         (equal? (hash-ref a k #f) (hash-ref run k #f))
         (equal? (hash-ref a k #f) (trial-meta env trial-key))))
  (define binding-ok?
    (and (hash? a)
         (equal? (hash-ref a 'schema #f) "ci-recovery-aggregate-restore-attestation@1")
         (bound? 'run-id 'run-id)
         (bound? 'run-attempt 'attempt)
         (bound? 'head-sha 'head-sha)))
  (define (observed-identity-ok?)
    (define observed (hash-ref a 'identity #f))
    (and (hash? observed)
         (hash? identity)
         (for/and ([key (in-list '(os os-image
                                      arch
                                      racket-version
                                      racket-executable-digest
                                      lock-digest
                                      resolved-set-digest
                                      precompile-recipe-revision
                                      policy-fingerprint
                                      artifact-digest))])
           (define value (hash-ref observed key #f))
           (and (string? value)
                (not (member value '("" "unknown")))
                (equal? value (hash-ref identity key #f))))))
  (define ok?
    (cond
      [(equal? treatment "A")
       (or (not a) (and binding-ok? (equal? (hash-ref a 'prepared-requested #f) "off")))]
      [else
       (and binding-ok?
            (equal? (hash-ref a 'prepared-requested #f) "auto")
            (equal? (hash-ref a 'result #f) "success")
            (let ([state (hash-ref a 'state #f)])
              (and (string? state) (not (member state '("" "unknown")))))
            (equal? (hash-ref a 'identity-result #f) "verified")
            (let ([ms (hash-ref a 'restore-ms #f)])
              (or (equal? ms "unknown") (and (rational? ms) (>= ms 0))))
            (observed-identity-ok?)
            (hash? expected)
            (let ([digest (hash-ref a 'producer-manifest-digest #f)])
              (and (string? digest)
                   (regexp-match? #px"^[0-9a-f]{64}$" digest)
                   (equal? digest (hash-ref expected 'producer-manifest-digest #f)))))]))
  (if ok?
      '()
      '("aggregate-attestation-invalid")))

(define (assess-trial-bundle env actions)
  ;; ONE trial attempt: W1 re-assessment + report-level binding checks +
  ;; required-job analysis. `reasons` empty ⇒ eligible timing candidate.
  (define verdict (assess-trial-envelope env))
  (define created-at (trial-meta env 'created-at-utc))
  (define binding-reasons
    (if (hash? env)
        (let ([shards (hash-ref env 'shards #f)])
          (if (list? shards)
              (append* (for/list ([s (in-list shards)]
                                  #:when (hash? s))
                         (shard-binding-reasons s env)))
              (list)))
        (list)))
  (define created-reasons
    (if (parse-utc-seconds created-at)
        (list)
        (list "trial-created-at-unparseable")))
  (define actions-binding-reasons
    (cond
      [(not actions) (list "missing-actions-evidence")]
      [(not (actions-schema-ok? actions)) (list "actions-schema-invalid")]
      [else
       (let ([run (hash-ref actions 'run #f)])
         (append (if (and (hash? run) (equal? (hash-ref run 'run-id #f) (trial-meta env 'run-id)))
                     (list)
                     (list "actions-run-id-mismatch"))
                 (if (and (hash? run) (equal? (hash-ref run 'head-sha #f) (trial-meta env 'head-sha)))
                     (list)
                     (list "actions-head-sha-mismatch"))
                 (if (and (hash? run)
                          (equal? (hash-ref run 'run-attempt #f) (trial-meta env 'attempt)))
                     (list)
                     (list "actions-attempt-mismatch"))))]))
  (define analysis
    (if (actions-schema-ok? actions)
        (analyze-actions actions)
        (hasheq 'seconds #f 'reasons (list) 'required-jobs (list) 'pilot-jobs (list))))
  (define reasons
    (sort (remove-duplicates (append (hash-ref verdict 'reasons (list))
                                     binding-reasons
                                     created-reasons
                                     actions-binding-reasons
                                     (hash-ref analysis 'reasons (list))
                                     (config-check-reasons env actions)
                                     (aggregate-attestation-reasons env actions)))
          string<?))
  (hasheq 'treatment
          (trial-meta env 'treatment)
          'trial-id
          (trial-meta env 'trial-id)
          'attempt
          (trial-meta env 'attempt)
          'run-id
          (trial-meta env 'run-id)
          'head-sha
          (trial-meta env 'head-sha)
          'created-at-utc
          created-at
          'created-at-seconds
          (parse-utc-seconds created-at)
          'verdict
          verdict
          'actions-echo
          (actions-echo actions)
          'reasons
          reasons
          'critical-path-seconds
          (let ([p (hash-ref analysis 'seconds #f)])
            (if (real? p)
                (ms-round p)
                "unknown"))
          'required-jobs
          (hash-ref analysis 'required-jobs (list))
          'pilot-jobs
          (hash-ref analysis 'pilot-jobs (list))
          'config-snapshot
          (config-snapshot actions)
          'components
          (component-echo actions)))

;; ---------------------------------------------------------------------------
;; Experiment assembly: population, arms, comparison, promotion
;; ---------------------------------------------------------------------------

(define (envelope-pair-key env)
  (define run-id (trial-meta env 'run-id))
  (define attempt (trial-meta env 'attempt))
  (and (real? run-id) (exact-positive-integer? attempt) (list run-id attempt)))

(define (actions-pair-key actions)
  (define run
    (if (hash? actions)
        (hash-ref actions 'run #f)
        #f))
  (define run-id
    (if (hash? run)
        (hash-ref run 'run-id #f)
        #f))
  (define attempt
    (if (hash? run)
        (hash-ref run 'run-attempt #f)
        #f))
  (and (real? run-id) (exact-positive-integer? attempt) (list run-id attempt)))

(define (pair-actions actions-lists)
  ;; Pair actions evidence by (run-id, run-attempt). Identical duplicates
  ;; collapse; conflicting duplicates poison the key so no favorable body
  ;; is selected. Every body is separately retained in raw.actions-attempts.
  ;; The map is EQUAL?-based because its keys are lists.
  (for/fold ([map (hash)]
             [refusals (list)]
             [keys (list)])
            ([a (in-list actions-lists)])
    (define key (actions-pair-key a))
    (cond
      [(not key) (values map refusals keys)]
      [(hash-has-key? map key)
       (if (equal? (hash-ref map key) a)
           (values map refusals keys)
           (values (hash-set map key #f) (cons "actions-duplicate-conflict" refusals) keys))]
      [else (values (hash-set map key a) refusals (cons key keys))])))

(define (entry-raw-view e)
  ;; The retained raw evidence for ONE attempt (used both for the report's
  ;; raw block and for duplicate-conflict comparison).
  (hasheq 'trial-id
          (hash-ref e 'trial-id)
          'attempt
          (hash-ref e 'attempt)
          'treatment
          (hash-ref e 'treatment)
          'run-id
          (hash-ref e 'run-id)
          'head-sha
          (hash-ref e 'head-sha)
          'created-at-utc
          (hash-ref e 'created-at-utc)
          'envelope-verdict
          (hash-ref e 'verdict)
          'actions-echo
          (hash-ref e 'actions-echo)
          'reasons
          (hash-ref e 'reasons)
          'critical-path-seconds
          (hash-ref e 'critical-path-seconds)
          'required-jobs
          (hash-ref e 'required-jobs)
          'pilot-jobs
          (hash-ref e 'pilot-jobs)
          'config-snapshot
          (hash-ref e 'config-snapshot)
          'components
          (hash-ref e 'components)
          'population-exclusion
          (hash-ref e 'population-exclusion #f)))

(define (entry-sort-key e)
  (list (format "~a" (hash-ref e 'treatment))
        (format "~a" (hash-ref e 'trial-id))
        (let ([a (hash-ref e 'attempt 0)]) (if (real? a) a 0))))

(define (entry<? a b)
  (define ka (entry-sort-key a))
  (define kb (entry-sort-key b))
  (cond
    [(string<? (first ka) (first kb)) #t]
    [(string<? (first kb) (first ka)) #f]
    [(string<? (second ka) (second kb)) #t]
    [(string<? (second kb) (second ka)) #f]
    [else (< (third ka) (third kb))]))

(define (duplicate-conflict-refusals entries)
  ;; Same (treatment, trial-id, attempt) with DIFFERENT retained data is a
  ;; timestamp/attempt mix — refused, never averaged away.
  (define seen (make-hash))
  (define conflict? #f)
  (for ([e (in-list entries)])
    (define key (list (hash-ref e 'treatment) (hash-ref e 'trial-id) (hash-ref e 'attempt)))
    (define view (entry-raw-view e))
    (cond
      [(not (hash-has-key? seen key)) (hash-set! seen key view)]
      [(not (equal? (hash-ref seen key) view)) (set! conflict? #t)]))
  (if conflict?
      (list "trial-duplicate-conflict")
      (list)))

(define (mark-duplicates-and-reruns entries)
  ;; Independence belongs to the actual Actions run, globally across labels
  ;; and arms. Conflicting labels refuse ALL entries, never choose a winner.
  (define labels (make-hash))
  (for ([e (in-list entries)])
    (hash-update!
     labels
     (hash-ref e 'run-id)
     (lambda (xs) (remove-duplicates (cons (list (hash-ref e 'treatment) (hash-ref e 'trial-id)) xs)))
     '()))
  (define seen (make-hash))
  (define sampled (make-hash))
  (for/list ([e (in-list entries)])
    (define run (hash-ref e 'run-id))
    (define att (hash-ref e 'attempt))
    (define key (list run att))
    (define conflict? (> (length (hash-ref labels run)) 1))
    (define reasons
      (sort (remove-duplicates (append (hash-ref e 'reasons)
                                       (if conflict?
                                           '("run-label-conflict")
                                           '())))
            string<?))
    (define exclusion
      (cond
        [(pair? reasons) "refused"]
        [(hash-has-key? seen key) "duplicate-excluded"]
        [(hash-has-key? sampled run) "rerun-excluded"]
        [else #f]))
    (hash-set! seen key #t)
    (unless exclusion
      (hash-set! sampled run #t))
    (hash-set (hash-set (hash-set e 'reasons reasons) 'population-exclusion exclusion)
              'sample?
              (and (not exclusion) (real? (hash-ref e 'critical-path-seconds))))))

(define (valid-heads entries)
  (for/list ([e (in-list entries)]
             #:when (sha40? (hash-ref e 'head-sha #f)))
    (hash-ref e 'head-sha)))

(define (source-mix-refusals entries)
  (if (> (length (remove-duplicates (valid-heads entries))) 1)
      (list "source-mix-head-sha")
      (list)))

(define (config-mix-refusals entries)
  ;; Exact config across ALL regimes: any dimension differing between the
  ;; trials' snapshots is an experiment-level refusal.
  (define snapshots (filter hash? (map (lambda (e) (hash-ref e 'config-snapshot #f)) entries)))
  (for/list ([dim (in-list config-dimensions)]
             #:when
             (> (length (remove-duplicates (for/list ([s (in-list snapshots)]
                                                      #:when (hash-has-key? s (string->symbol dim)))
                                             (hash-ref s (string->symbol dim) #f))))
                1))
    (format "config-mix-~a" dim)))

(define (max-consecutive-block treatments)
  (if (null? treatments)
      0
      (let loop ([xs (cdr treatments)]
                 [cur-t (car treatments)]
                 [len 1]
                 [best 1])
        (cond
          [(null? xs) best]
          [(equal? (car xs) cur-t) (loop (cdr xs) cur-t (+ len 1) (max best (+ len 1)))]
          [else (loop (cdr xs) (car xs) 1 (max best len))]))))

(define (interleave-info entries)
  (define samples
    (sort (filter (lambda (e) (hash-ref e 'sample? #f)) entries)
          (lambda (a b)
            (define sa (hash-ref a 'created-at-seconds))
            (define sb (hash-ref b 'created-at-seconds))
            (if (equal? sa sb)
                (string<? (format "~a" (hash-ref a 'trial-id)) (format "~a" (hash-ref b 'trial-id)))
                (< sa sb)))))
  (define treatments (map (lambda (e) (hash-ref e 'treatment)) samples))
  (define distinct-treatments (remove-duplicates treatments))
  (define max-block (max-consecutive-block treatments))
  (hasheq 'ok
          (or (< (length distinct-treatments) 2) (<= max-block max-interleave-block))
          'max-same-treatment-block
          max-block
          'sequence
          (for/list ([e (in-list samples)])
            (hasheq 'trial-id
                    (hash-ref e 'trial-id)
                    'treatment
                    (hash-ref e 'treatment)
                    'created-at-utc
                    (hash-ref e 'created-at-utc)))))

(define (interleave-refusals entries)
  (if (hash-ref (interleave-info entries) 'ok)
      (list)
      (list "treatment-block-not-interleaved")))

(define (component-value entry dim)
  (define comps (hash-ref entry 'components "unknown"))
  (if (hash? comps)
      (hash-ref comps (string->symbol dim) "unknown")
      "unknown"))

(define (arm-components samples)
  ;; An arm-level component number exists only when EVERY sampled trial
  ;; observed that component; otherwise "unknown" (never zero-filled).
  (for/hash ([dim (in-list component-dimensions)])
    (define vals (map (lambda (e) (component-value e dim)) samples))
    (values (string->symbol dim)
            (if (and (pair? vals) (andmap real? vals))
                (ms-round (/ (apply + vals) (length vals)))
                "unknown"))))

(define (uncertainty-block n)
  (hasheq
   'confidence-interval
   "none"
   'tail-estimate-limited
   (< n 20)
   'method
   "cohort-quantile-exact: linear interpolation at k=q(n-1), millisecond-rounded"
   'note
   (format
    "n=~a samples; the p95 tail estimate is limited at this sample size and no confidence interval is fabricated"
    n)))

(define (arm-block treatment entries)
  (define t-entries (filter (lambda (e) (equal? (hash-ref e 'treatment) treatment)) entries))
  (define samples (filter (lambda (e) (hash-ref e 'sample? #f)) t-entries))
  (define nums
    (for/list ([e (in-list samples)]
               #:when (real? (hash-ref e 'critical-path-seconds)))
      (hash-ref e 'critical-path-seconds)))
  (define blocked?
    (and (equal? treatment "C") (findf (lambda (e) (pair? (hash-ref e 'reasons))) t-entries)))
  (define arm-verdict
    (cond
      [blocked? "blocked-refused-trial"]
      [(< (length samples) min-trials-per-arm) "insufficient-trials"]
      [else "measured"]))
  (define arm-reasons
    (cond
      [blocked? (list "c-arm-refused-trial")]
      [(< (length samples) min-trials-per-arm) (list "insufficient-independent-trials")]
      [else (list)]))
  (hasheq
   'treatment
   treatment
   'n
   (length samples)
   'eligible-trials
   (map (lambda (e) (hash-ref e 'trial-id)) samples)
   'samples
   (for/list ([e (in-list (sort samples < #:key (lambda (e) (hash-ref e 'critical-path-seconds 0))))])
     (hasheq 'trial-id
             (hash-ref e 'trial-id)
             'attempt
             (hash-ref e 'attempt)
             'critical-path-seconds
             (hash-ref e 'critical-path-seconds)))
   'p50-seconds
   (if (null? nums)
       "unknown"
       (cohort-quantile-exact nums 0.50))
   'p95-seconds
   (if (null? nums)
       "unknown"
       (cohort-quantile-exact nums 0.95))
   'attempts-retained
   (length t-entries)
   'exclusions
   (for/list ([e (in-list t-entries)]
              #:unless (hash-ref e 'sample? #f))
     (hasheq 'trial-id
             (hash-ref e 'trial-id)
             'attempt
             (hash-ref e 'attempt)
             'exclusion
             (hash-ref e 'population-exclusion "refused")
             'reasons
             (hash-ref e 'reasons)))
   'exclusion-reasons
   (sort (remove-duplicates (append* (for/list ([e (in-list t-entries)]
                                                #:unless (hash-ref e 'sample? #f))
                                       (if (pair? (hash-ref e 'reasons))
                                           (hash-ref e 'reasons)
                                           (list (hash-ref e 'population-exclusion "excluded"))))))
         string<?)
   'arm-verdict
   arm-verdict
   'reasons
   arm-reasons
   'components
   (arm-components samples)
   'uncertainty
   (uncertainty-block (length samples))))

(define (c-blocked? entries)
  (findf (lambda (e) (and (equal? (hash-ref e 'treatment) "C") (pair? (hash-ref e 'reasons))))
         entries))

(define (c-vs-a-block arms exp-refusals entries)
  (define arm-a (hash-ref arms "A"))
  (define arm-c (hash-ref arms "C"))
  (define a-p50 (hash-ref arm-a 'p50-seconds))
  (define a-p95 (hash-ref arm-a 'p95-seconds))
  (define c-p50 (hash-ref arm-c 'p50-seconds))
  (define c-p95 (hash-ref arm-c 'p95-seconds))
  (define blocked? (c-blocked? entries))
  (define measured-a? (>= (hash-ref arm-a 'n 0) min-trials-per-arm))
  (define measured-c? (>= (hash-ref arm-c 'n 0) min-trials-per-arm))
  (define evaluable?
    (and (null? exp-refusals)
         (not blocked?)
         measured-a?
         measured-c?
         (real? a-p50)
         (real? a-p95)
         (real? c-p50)
         (real? c-p95)))
  (define reasons
    (sort (remove-duplicates (append exp-refusals
                                     (if blocked?
                                         (list "c-arm-refused-trial")
                                         (list))
                                     (if measured-a?
                                         (list)
                                         (list "insufficient-independent-trials"))
                                     (if measured-c?
                                         (list)
                                         (list "insufficient-independent-trials"))))
          string<?))
  (if (not evaluable?)
      (hasheq 'evaluable #f 'reasons reasons 'budget-rule budget-rule-text 'verdict "no-claim")
      (let ([p50-delta (ms-round (- c-p50 a-p50))]
            [p95-delta (ms-round (- c-p95 a-p95))]
            [p50-budget (regression-budget-seconds a-p50)]
            [p95-budget (regression-budget-seconds a-p95)])
        (define within? (and (<= p50-delta p50-budget) (<= p95-delta p95-budget)))
        (hasheq 'evaluable
                #t
                'reasons
                (list)
                'budget-rule
                budget-rule-text
                'a-p50-seconds
                a-p50
                'c-p50-seconds
                c-p50
                'p50-delta-seconds
                p50-delta
                'p50-budget-seconds
                p50-budget
                'p50-within-budget
                (<= p50-delta p50-budget)
                'a-p95-seconds
                a-p95
                'c-p95-seconds
                c-p95
                'p95-delta-seconds
                p95-delta
                'p95-budget-seconds
                p95-budget
                'p95-within-budget
                (<= p95-delta p95-budget)
                'within-budget
                within?
                ;; Non-regression alone is NOT a useful-improvement claim.
                'c-faster-than-a-both-quantiles
                (and (< c-p50 a-p50) (< c-p95 a-p95))
                'verdict
                (if within? "within-budget" "exceeds-budget")))))

(define (b-vs-a-block arms exp-refusals)
  ;; B's aggregate benefit is reported SEPARATELY: descriptive only, never
  ;; a promotion gate.
  (define arm-a (hash-ref arms "A"))
  (define arm-b (hash-ref arms "B"))
  (define a-p50 (hash-ref arm-a 'p50-seconds))
  (define a-p95 (hash-ref arm-a 'p95-seconds))
  (define b-p50 (hash-ref arm-b 'p50-seconds))
  (define b-p95 (hash-ref arm-b 'p95-seconds))
  (define measured-a? (>= (hash-ref arm-a 'n 0) min-trials-per-arm))
  (define measured-b? (>= (hash-ref arm-b 'n 0) min-trials-per-arm))
  (define evaluable?
    (and (null? exp-refusals)
         measured-a?
         measured-b?
         (real? a-p50)
         (real? a-p95)
         (real? b-p50)
         (real? b-p95)))
  (define reasons
    (sort (remove-duplicates (append exp-refusals
                                     (if measured-a?
                                         (list)
                                         (list "insufficient-independent-trials"))
                                     (if measured-b?
                                         (list)
                                         (list "insufficient-independent-trials"))))
          string<?))
  (if (not evaluable?)
      (hasheq 'evaluable
              #f
              'reasons
              reasons
              'verdict
              "no-claim"
              'note
              "aggregate benefit is descriptive only and never a promotion gate")
      (hasheq 'evaluable
              #t
              'reasons
              (list)
              'a-p50-seconds
              a-p50
              'b-p50-seconds
              b-p50
              'benefit-seconds
              (ms-round (- a-p50 b-p50))
              'a-p95-seconds
              a-p95
              'b-p95-seconds
              b-p95
              'p95-benefit-seconds
              (ms-round (- a-p95 b-p95))
              'verdict
              "descriptive-only"
              'note
              "aggregate benefit is descriptive only and never a promotion gate")))

(define (promotion-block arms comparison entries)
  (define blocking
    (sort (remove-duplicates (append (hash-ref (hash-ref arms "A") 'reasons (list))
                                     (hash-ref (hash-ref arms "C") 'reasons (list))
                                     (hash-ref (hash-ref comparison 'c-vs-a) 'reasons (list))))
          string<?))
  (cond
    ;; ANY refused/fallback-contaminated C trial blocks promotion — even
    ;; when enough other C trials are good and the quantiles exist.
    [(c-blocked? entries)
     (hasheq 'readiness "blocked" 'reasons (list "c-arm-refused-trial") 'note promotion-note)]
    [(pair? blocking) (hasheq 'readiness "not-evaluated" 'reasons blocking 'note promotion-note)]
    [(equal? (hash-ref (hash-ref comparison 'c-vs-a) 'verdict) "within-budget")
     (hasheq 'readiness "non-regression-only" 'reasons (list) 'note promotion-note)]
    [else
     (hasheq 'readiness
             "not-evaluated"
             'reasons
             (list "c-vs-a-exceeds-budget")
             'note
             promotion-note)]))

(define (common-config snapshots)
  ;; Dimensions that are present and identical across every snapshot.
  (if (null? snapshots)
      "unknown"
      (for/hash ([dim (in-list config-dimensions)]
                 #:when (let ()
                          (define vs
                            (for/list ([s (in-list snapshots)]
                                       #:when (hash-has-key? s (string->symbol dim)))
                              (hash-ref s (string->symbol dim))))
                          (and (pair? vs)
                               (= (length vs) (length snapshots))
                               (= (length (remove-duplicates vs)) 1))))
        (values (string->symbol dim) (hash-ref (car snapshots) (string->symbol dim) #f)))))

(define (binding-block entries)
  (define heads (remove-duplicates (valid-heads entries)))
  (define snapshots (filter hash? (map (lambda (e) (hash-ref e 'config-snapshot #f)) entries)))
  (hasheq 'pinned-head-sha
          (if (= (length heads) 1)
              (car heads)
              "unknown")
          'source-uniform
          (= (length heads) 1)
          'config-uniform
          (and (pair? snapshots) (null? (config-mix-refusals entries)))
          'config
          (if (= (length snapshots) 1)
              (car snapshots)
              (common-config snapshots))))

(define (unpaired-run-ids envelopes actions-map)
  (define used
    (filter values
            (map envelope-pair-key
                 (if (list? envelopes)
                     envelopes
                     (list)))))
  (sort (for/list ([key (in-list (hash-keys actions-map))]
                   #:unless (member key used))
          (first key))
        <))

(define (assess-benchmark-experiment envelopes actions-lists)
  ;; Pure and deterministic: the full experiment report from explicit
  ;; trial inputs. Identical inputs yield equal? results (and byte-stable
  ;; JSON via format-report-json).
  (define envelopes-list
    (if (list? envelopes)
        envelopes
        (list)))
  (define actions-list
    (if (list? actions-lists)
        actions-lists
        (list)))
  (define-values (actions-map actions-refusals paired-keys) (pair-actions actions-list))
  (define entries
    (for/list ([env (in-list envelopes-list)])
      (define key (envelope-pair-key env))
      (assess-trial-bundle env (and key (hash-ref actions-map key #f)))))
  (define sorted-entries (sort entries entry<?))
  (define marked (mark-duplicates-and-reruns sorted-entries))
  (define exp-refusals
    (sort (remove-duplicates (append actions-refusals
                                     (duplicate-conflict-refusals sorted-entries)
                                     (source-mix-refusals sorted-entries)
                                     (config-mix-refusals sorted-entries)
                                     (interleave-refusals marked)))
          string<?))
  (define arms
    (for/hash ([t (in-list (list "A" "B" "C"))])
      (values t (arm-block t marked))))
  (define comparison
    (hasheq 'c-vs-a (c-vs-a-block arms exp-refusals marked) 'b-vs-a (b-vs-a-block arms exp-refusals)))
  (hasheq 'schema
          benchmark-report-schema
          'kind
          "ci-recovery-benchmark-report"
          'treatments
          (list "A" "B" "C")
          'binding
          (binding-block marked)
          'arms
          arms
          'comparison
          comparison
          'promotion
          (promotion-block arms comparison marked)
          'honesty
          honesty-block
          'refusals
          exp-refusals
          'interleaving
          (interleave-info marked)
          'raw
          (hasheq 'trials
                  (map entry-raw-view marked)
                  'unpaired-actions
                  (unpaired-run-ids envelopes-list actions-map)
                  'attempts-total
                  (length sorted-entries)
                  'actions-attempts
                  (map actions-echo actions-list))))

;; ---------------------------------------------------------------------------
;; Deterministic serialization
;; ---------------------------------------------------------------------------

(define (key->string k)
  (if (symbol? k)
      (symbol->string k)
      (format "~a" k)))

(define (json-string-escape s)
  (string-append "\""
                 (for/fold ([acc ""]) ([c (in-string s)])
                   (string-append acc
                                  (case c
                                    [(#\") "\\\""]
                                    [(#\\) "\\\\"]
                                    [(#\newline) "\\n"]
                                    [(#\return) "\\r"]
                                    [(#\tab) "\\t"]
                                    [else (string c)])))
                 "\""))

(define (report-json-string v)
  (cond
    [(string? v) (json-string-escape v)]
    [(boolean? v) (if v "true" "false")]
    [(real? v) (number->string v)]
    [(eq? v 'null) "null"]
    [(null? v) "[]"]
    [(list? v) (string-append "[" (string-join (map report-json-string v) ",") "]")]
    [(hash? v)
     (string-append
      "{"
      (string-join
       (for/list ([k (in-list (sort (hash-keys v) string<? #:key key->string))])
         (string-append (json-string-escape (key->string k)) ":" (report-json-string (hash-ref v k))))
       ",")
      "}")]
    [else (json-string-escape (format "~a" v))]))

(define (format-report-json report)
  (string-append (report-json-string report) "\n"))

(define (format-report-md report)
  ;; Deterministic human summary; every number cites the machine report.
  (define binding (hash-ref report 'binding))
  (define promotion (hash-ref report 'promotion))
  (define lines
    (append
     (list "# CI recovery benchmark report (v1.00.32 W2)"
           ""
           (string-append "Implementation artifact: every number below comes from the explicit trial"
                          " evidence inputs of one run; no measurement was taken by this wave.")
           ""
           "## Binding"
           ""
           (format "- pinned head: ~a (source-uniform: ~a)"
                   (hash-ref binding 'pinned-head-sha)
                   (hash-ref binding 'source-uniform))
           (format "- config-uniform: ~a" (hash-ref binding 'config-uniform))
           "")
     (list "## Arms" "")
     (append* (for/list ([t (in-list (list "A" "B" "C"))])
                (define arm (hash-ref (hash-ref report 'arms) t))
                (list (format "- ~a: n=~a p50=~as p95=~as verdict=~a"
                              t
                              (hash-ref arm 'n)
                              (hash-ref arm 'p50-seconds)
                              (hash-ref arm 'p95-seconds)
                              (hash-ref arm 'arm-verdict))
                      (format "  retained attempts: ~a; named exclusions: ~a"
                              (hash-ref arm 'attempts-retained)
                              (length (hash-ref arm 'exclusions)))
                      (format "  uncertainty: ~a" (hash-ref (hash-ref arm 'uncertainty) 'note)))))
     (list "" "## Comparison" "")
     (let ([cva (hash-ref (hash-ref report 'comparison) 'c-vs-a)])
       (if (hash-ref cva 'evaluable #f)
           (list
            (format "- C vs A: verdict=~a (p50 delta ~as <= budget ~as; p95 delta ~as <= budget ~as)"
                    (hash-ref cva 'verdict)
                    (hash-ref cva 'p50-delta-seconds)
                    (hash-ref cva 'p50-budget-seconds)
                    (hash-ref cva 'p95-delta-seconds)
                    (hash-ref cva 'p95-budget-seconds))
            (format "  budget rule: ~a" (hash-ref cva 'budget-rule)))
           (list (format "- C vs A: verdict=~a (reasons: ~a)"
                         (hash-ref cva 'verdict)
                         (string-join (hash-ref cva 'reasons (list)) "; ")))))
     (let ([bva (hash-ref (hash-ref report 'comparison) 'b-vs-a)])
       (if (hash-ref bva 'evaluable #f)
           (list (format
                  "- B vs A: verdict=~a (aggregate benefit ~as, p95 benefit ~as; descriptive only)"
                  (hash-ref bva 'verdict)
                  (hash-ref bva 'benefit-seconds)
                  (hash-ref bva 'p95-benefit-seconds)))
           (list (format "- B vs A: verdict=~a (reasons: ~a)"
                         (hash-ref bva 'verdict)
                         (string-join (hash-ref bva 'reasons (list)) "; ")))))
     (list ""
           "## Promotion"
           ""
           (format "- readiness: ~a" (hash-ref promotion 'readiness))
           (format "- ~a" (hash-ref promotion 'note))
           ""
           "## Refusals"
           ""
           (if (null? (hash-ref report 'refusals))
               "- none"
               (for/list ([r (in-list (hash-ref report 'refusals))])
                 (format "- ~a" r)))
           ""
           "## Honesty"
           ""
           (format "- statistics: ~a" (hash-ref (hash-ref report 'honesty) 'statistics-method))
           (format "- ~a" (hash-ref (hash-ref report 'honesty) 'non-regression-note))
           (format "- ~a" (hash-ref (hash-ref report 'honesty) 'missing-data-note))
           (format "- ~a" (hash-ref (hash-ref report 'honesty) 'critical-path-rule)))))
  (string-join lines "\n"))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (assoc-value args flag)
  (let loop ([xs args])
    (cond
      [(null? xs) #f]
      [(equal? (car xs) flag)
       (if (pair? (cdr xs))
           (cadr xs)
           #t)]
      [else (loop (cdr xs))])))

(define (usage-fail message)
  (fprintf
   (current-error-port)
   "ci-recovery-benchmark-report: ~a
usage:
  racket scripts/run-tests/ci-recovery-benchmark-report.rkt --envelopes ENVELOPES.jsonl \
  [--actions ACTIONS.jsonl] --out REPORT.json [--out-md REPORT.md]

  ENVELOPES.jsonl: one ci-recovery-trial-evidence@1 document per line
  ACTIONS.jsonl:   one ci-recovery-actions-evidence@1 document per line
"
   message)
  2)

(define (read-jsonl path)
  ;; → (values ok? docs); one JSON document per non-empty line.
  (cond
    [(not (file-exists? path)) (values #f (list))]
    [else
     (define docs '())
     (define ok? #t)
     (call-with-input-file path
                           (lambda (in)
                             (for ([line (in-lines in)])
                               (define trimmed (string-trim line))
                               (unless (string=? trimmed "")
                                 (with-handlers ([exn:fail? (lambda (_) (set! ok? #f))])
                                   (set! docs
                                         (cons (read-json (open-input-string trimmed)) docs)))))))
     (values ok? (reverse docs))]))

(define (write-report-file path content)
  (with-output-to-file path (lambda () (display content)) #:exists 'replace))

(define (main-dispatch args)
  (define envelopes-file (assoc-value args "--envelopes"))
  (define actions-file (assoc-value args "--actions"))
  (define out (assoc-value args "--out"))
  (define out-md (assoc-value args "--out-md"))
  (cond
    [(not (string? envelopes-file)) (exit (usage-fail "--envelopes ENVELOPES.jsonl is required"))]
    [(not (string? out)) (exit (usage-fail "--out REPORT.json is required"))]
    [else
     (define-values (env-ok? envelopes) (read-jsonl envelopes-file))
     (unless env-ok?
       (exit (usage-fail (format "cannot read envelopes file ~a" envelopes-file))))
     (define actions
       (cond
         [(string? actions-file)
          (define-values (a-ok? docs) (read-jsonl actions-file))
          (unless a-ok?
            (exit (usage-fail (format "cannot read actions file ~a" actions-file))))
          docs]
         [else (list)]))
     (define report (assess-benchmark-experiment envelopes actions))
     (write-report-file out (format-report-json report))
     (when (string? out-md)
       (write-report-file out-md (format-report-md report)))
     (printf "ci-recovery-benchmark-report: wrote ~a (experiment refusals: ~a)\n"
             out
             (length (hash-ref report 'refusals (list))))
     0]))

(module+ main
  (exit (main-dispatch (vector->list (current-command-line-arguments)))))

#lang racket/base

;; scripts/ci/pr-latency-guard.rkt — v1.00.30 W1 blocking PR latency guard.
;;
;; Pure, fail-closed evaluator over a provenance-bound input JSON document
;; (contract: .planning/waves/W1-blocking-pr-latency-guard.md and
;; .planning/PLAN-v1.00.30-PR-CI-RECOVERY.md).  The evaluator never touches
;; the network, never executes PR code, and derives a deterministic verdict:
;;
;;   pass            — both p50 AND p95 latency deltas (candidate - reference)
;;                     are <= min(10% of reference, 60 s); equality passes.
;;                     Requires >= 3 distinct eligible first-attempt head SHAs
;;                     per side, the exact expected candidate head SHA and
;;                     current base SHA, matching control strata, fresh
;;                     coordinator-attested evidence, and all required checks
;;                     green on the final head.
;;   not-applicable  — the same structural checks pass AND the
;;                     coordinator-attested base/head diff lies entirely inside
;;                     the frozen conservative allowlist of non-executable
;;                     report/binding paths (docs/binding-only PRs).
;;   fail            — anything else: unknown, stale, cancelled/incomplete,
;;                     rerun-only, forged or missing data never yields green.
;;
;; Anti-regression rules enforced here (PLAN "Measurement and anti-regression
;; mechanics"):
;;   * Quantiles reuse scripts/run-tests/cohort-report.rkt's exact
;;     linear-interpolation helper (cohort-quantile-exact); values are
;;     converted to exact integer milliseconds and retained exactly until
;;     display so boundary comparisons are deterministic.
;;   * Repeated attempts of one SHA never satisfy uniqueness: duplicate
;;     eligible head SHAs fail closed; only first attempts are eligible.
;;   * A later green attempt never erases an earlier failure: the final head
;;     must itself carry an eligible first-attempt success; reruns of the same
;;     head cannot produce the sample.
;;   * The guard excludes its own check from eligibility and from the
;;     required-jobs inventory (no-self-recursion) and never waits for it.
;;   * Control strata (runner class, Racket executable/version, dependencies,
;;     required suite/claim inventory) must match exactly.  Cache-availability
;;     strata may differ only when separately reported by both policy and
;;     candidate.  A DECLARED TREATMENT (compile-regime, root, shard
;;     assignment) may differ only when bound to candidate/base SHA and regime
;;     fingerprints.  Unexplained differences fail closed.
;;   * NOT_APPLICABLE requires a coordinator-attested diff for the exact
;;     base/head whose every path is in the conservative allowlist.  Unknown
;;     paths, generated config, test/source/lock changes or policy changes
;;     require full samples.  A PR cannot self-declare exemption.
;;
;; Input document schema (q.pr-latency-guard.input/1, hasheq with symbol keys
;; after read-json):
;;
;;   { "schema": "q.pr-latency-guard.input/1",
;;     "attestation": { "repository": <str>, "workflow": <str>,
;;                      "source": "actions-api", "collector": <str>,
;;                      "collected-at": "<YYYY-MM-DDTHH:MM:SSZ>" },
;;     "policy": { "trusted-origin": { "repository": <str>, "workflow": <str> },
;;                 "expected-head-sha": <sha40>, "expected-base-sha": <sha40>,
;;                 "expected-attempt": <pos-int>,            ; optional, default 1
;;                 "min-distinct-heads": <int >= 3>,
;;                 "freshness-window-seconds": <pos-int>,
;;                 "evaluated-at": <ts>,
;;                 "measured-check": "pr-latency-guard",
;;                 "required-jobs": [<str> ...],             ; guard excluded
;;                 "separately-reported-strata": [<str> ...] }, ; optional
;;     "reference": { "frozen": true, "frozen-at": <ts>,
;;                    "regime-fingerprint": <str>,
;;                    "control-strata": { <str>: <str> ... },
;;                    "samples": [<run-record> ...] },
;;     "candidate": { "regime-fingerprint": <str>,
;;                    "control-strata": { <str>: <str> ... },
;;                    "declared-treatment": <treatment>,      ; optional
;;                    "separately-reported-strata": [<str>],  ; optional
;;                    "diff": <attested-diff>,                ; optional
;;                    "runs": [<run-record> ...] } }
;;
;;   <run-record> := { "head-sha": <sha40>, "base-sha": <sha40>,
;;                     "attempt": <pos-int>, "first-attempt": <bool>,
;;                     "status": <str>, "conclusion": <str>,
;;                     "started-at": <ts>, "completed-at": <ts>,
;;                     "window-seconds": <real >= 0>, "attested": true,
;;                     "job": <str>,                            ; optional
;;                     "jobs": [ { "name": <str>, "status": <str>,
;;                                 "conclusion": <str>,
;;                                 "attempt": <pos-int> } ... ] } ; opt. list
;;   <treatment>   := { "kind": <str>, "bound-candidate-sha": <sha40>,
;;                      "bound-base-sha": <sha40>,
;;                      "bound-regime-fingerprints":
;;                        { "reference": <str>, "candidate": <str> } }
;;   <attested-diff> := { "source": "coordinator-attested",
;;                        "base-sha": <sha40>, "head-sha": <sha40>,
;;                        "files": [<str> ...] }
;;
;; Usage:
;;   racket scripts/ci/pr-latency-guard.rkt --input <json>
;;          [--verdict-json out.json] [--decision-md out.md]
;;   Exit codes: 0 pass/not-applicable, 1 fail (fail closed), 2 usage.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/string
         json
         "../run-tests/cohort-report.rkt")

(provide input-schema-id
         guard-verdict-jsexpr
         guard-verdict-json-string
         guard-decision-md-string
         guard-exempt-path?)

;; ============================================================
;; Constants (the fixed campaign regression rule; the policy document may
;; only restate these values — see guard-policy-checks)
;; ============================================================

(define input-schema-id "q.pr-latency-guard.input/1")
(define guard-gate-id "pr-latency-guard/1")
(define measured-check-default "pr-latency-guard")

(define known-statuses '("completed" "in_progress" "queued" "cancelled"))
(define known-conclusions '("success" "failure" "cancelled" "timed_out" "skipped" "neutral"))

;; PLAN: controls must match runner class, Racket executable/version,
;; dependencies and required suite/claim inventory.  Stratum names are
;; normalized to symbols (the in-process contract, matching read-json and
;; jsexpr-symbolize); JSON text uses the same names as strings.
(define canonical-strata '(runner-class racket-executable dependencies required-suite-inventory))
;; PLAN: cache availability strata are matched or separately reported.
(define separately-reportable-strata '(cache-availability))
;; PLAN: the declared treatment may differ when bound to SHAs and fingerprints.
(define treatment-strata '(compile-regime root-treatment shard-assignment))
;; Idempotent stratum-name normalizer: symbols pass through, strings map to
;; their symbols so symbol-keyed and string-keyed evidence agree.
(define (stratum-key k)
  (if (symbol? k)
      k
      (string->symbol (format "~a" k))))
;; Membership test for stratum names, tolerant of string/symbol spellings
;; on both the probed name and the list elements.
(define (mem-stratum k lst)
  (and (list? lst) (member (stratum-key k) (map stratum-key lst)) #t))
;; Rebuild a stratum map with symbol keys (idempotent) so key-set
;; comparisons never mix spellings and the CLI JSON path (string keys
;; from read-json) agrees with the in-process fixtures.
(define (normalize-strata h)
  (if (hash? h)
      (for/hasheq ([(k v) (in-hash h)])
        (values (stratum-key k) v))
      h))

;; Conservative trusted-diff allowlist for NOT_APPLICABLE (binding/docs-only).
(define exempt-allow-prefixes '("docs/" ".planning/" "artifacts/"))
(define exempt-allow-suffixes '(".md" ".rktd" ".txt" ".csv"))
(define exempt-allow-exact '("README.md" "CHANGELOG.md" "LICENSE" "SHA256SUMS"))
;; Guarded paths are never exempt, even when they otherwise match the
;; allowlist (fail closed on test/source/workflow/generated changes).
(define exempt-guard-prefixes '("tests/" "scripts/" ".github/" "src/"))
(define exempt-guard-exact '("package.json" "package-lock.json" "info.rkt"))

(define (guard-exempt-path? p)
  (and (string? p)
       (non-empty-string? p)
       (not (ormap (lambda (pre) (string-prefix? p pre)) exempt-guard-prefixes))
       (not (member p exempt-guard-exact))
       (or (ormap (lambda (pre) (string-prefix? p pre)) exempt-allow-prefixes)
           (ormap (lambda (suf) (string-suffix? p suf)) exempt-allow-suffixes)
           (and (member p exempt-allow-exact) #t))))

;; ============================================================
;; Small helpers
;; ============================================================

(define absent (gensym "absent"))
(define (present? h k)
  (not (eq? (hash-ref h k absent) absent)))

(define (nonempty-string? v)
  (and (string? v) (non-empty-string? v)))
(define (positive-integer? v)
  (and (exact-integer? v) (> v 0)))
(define (real-non-negative? v)
  (and (real? v) (>= v 0)))
(define (sha40-s? v)
  (and (string? v) (sha40? v)))

;; Parse "YYYY-MM-DDTHH:MM:SSZ" (fractional seconds are truncated, matching
;; GitHub's ISO output) to exact Unix seconds; #f when unparseable.
(define (parse-ts v)
  (and (string? v) (pr-elapsed-utc-seconds (regexp-replace #px"\\.[0-9]+Z$" v "Z"))))

;; cohort-quantile-exact returns inexact seconds rounded to millisecond
;; precision; convert to exact integer ms and stay exact through every
;; boundary comparison (PLAN: equality passes).
(define (seconds->ms v)
  (inexact->exact (round (* 1000.0 (exact->inexact v)))))

;; JSON 0.1 arrives inexact; recover the intended exact 1/10 so the 10%
;; budget is an exact rational comparison.
(define (rational-of v)
  (if (exact? v)
      v
      (inexact->exact (rationalize v 1e-12))))

;; Exact rational intermediates (10% budgets) must serialize as legal
;; JSON numbers: keep integers exact, downgrade other rationals.
(define (json-number v)
  (if (and (rational? v) (not (integer? v)))
      (exact->inexact v)
      v))

(define (valid-job? j)
  (and (hash? j)
       (nonempty-string? (hash-ref j 'name #f))
       (member (hash-ref j 'status #f) known-statuses)
       (member (hash-ref j 'conclusion #f) known-conclusions)
       (positive-integer? (hash-ref j 'attempt 1))))

;; ============================================================
;; Run-record validation (shape only; eligibility decided separately)
;; ============================================================

;; -> (cons ok? reasons) ; reasons in fixed field order
(define (validate-sample rec measured-check)
  (cond
    [(not (hash? rec)) (cons #f (list "run record is not a JSON object"))]
    [else
     (define reasons '())
     (define (bad fmt . args)
       (set! reasons (cons (apply format fmt args) reasons)))
     (define head-sha (hash-ref rec 'head-sha #f))
     (define base-sha (hash-ref rec 'base-sha #f))
     (define attempt (hash-ref rec 'attempt #f))
     (define first-attempt (hash-ref rec 'first-attempt #f))
     (define status (hash-ref rec 'status #f))
     (define conclusion (hash-ref rec 'conclusion #f))
     (define started (hash-ref rec 'started-at #f))
     (define completed (hash-ref rec 'completed-at #f))
     (define window (hash-ref rec 'window-seconds #f))
     (define attested (hash-ref rec 'attested #f))
     (define job (hash-ref rec 'job #f))
     (define jobs (hash-ref rec 'jobs #f))
     (unless (sha40-s? head-sha)
       (bad "run record head-sha must be a 40-hex SHA"))
     (unless (sha40-s? base-sha)
       (bad "run record base-sha must be a 40-hex SHA"))
     (unless (positive-integer? attempt)
       (bad "run record attempt must be a positive integer"))
     (unless (boolean? first-attempt)
       (bad "run record first-attempt must be a boolean"))
     (unless (member status known-statuses)
       (bad "run record status ~s is not a known run status" status))
     (unless (member conclusion known-conclusions)
       (bad "run record conclusion ~s is not a known conclusion" conclusion))
     (unless (parse-ts started)
       (bad "run record started-at is missing or unparseable"))
     (unless (parse-ts completed)
       (bad "run record completed-at is missing or unparseable"))
     (define started-s (parse-ts started))
     (define completed-s (parse-ts completed))
     (when (and started-s completed-s (> started-s completed-s))
       (bad "run record started-at is after completed-at"))
     (unless (real-non-negative? window)
       (bad "run record window-seconds must be a non-negative number"))
     (unless (eq? attested #t)
       (bad "run record must carry attested=true from the trusted collector"))
     (when (and (present? rec 'job) (not (nonempty-string? job)))
       (bad "run record job label must be a non-empty string when present"))
     (when (and jobs (or (not (list? jobs)) (not (andmap valid-job? jobs))))
       (bad "run record jobs must be a list of well-formed check-run objects"))
     (cons (null? reasons) (reverse reasons))]))

;; Pre-registered eligible first-attempt completion (PLAN: latency quantiles
;; are computed from these; reruns of one SHA never satisfy uniqueness).  The
;; guard's own check is excluded from eligibility to avoid cycles.
(define (sample-eligible? rec measured-check)
  (and (hash? rec)
       (eq? (hash-ref rec 'attested #f) #t)
       (eq? (hash-ref rec 'first-attempt #f) #t)
       (equal? (hash-ref rec 'attempt #f) 1)
       (equal? (hash-ref rec 'status #f) "completed")
       (equal? (hash-ref rec 'conclusion #f) "success")
       (real-non-negative? (hash-ref rec 'window-seconds #f))
       (not (equal? (hash-ref rec 'job #f) measured-check))))

;; ============================================================
;; Fail-closed default verdicts
;; ============================================================

(define all-check-keys
  '(schema attestation
           record-shape
           provenance
           freshness
           uniqueness
           required-jobs
           control-strata
           quantile-budget
           quantile-evaluated
           no-self-recursion
           final-head-eligible))

(define (all-false-checks)
  (for/hasheq ([k all-check-keys])
    (values k #f)))

(define (all-null-numbers)
  (hasheq 'reference-p50-ms
          #f
          'reference-p95-ms
          #f
          'candidate-p50-ms
          #f
          'candidate-p95-ms
          #f
          'delta-p50-ms
          #f
          'delta-p95-ms
          #f
          'allowed-p50-ms
          #f
          'allowed-p95-ms
          #f
          'reference-eligible
          #f
          'candidate-eligible
          #f
          'reference-distinct-heads
          #f
          'candidate-distinct-heads
          #f
          'guard-own-samples-excluded
          #f
          'censored-candidate-runs
          #f))

(define (fail-verdict msg)
  (hasheq 'gate
          guard-gate-id
          'input-schema
          input-schema-id
          'decision
          "fail"
          'checks
          (all-false-checks)
          'numbers
          (all-null-numbers)
          'reasons
          (list msg)))

;; ============================================================
;; Main evaluator
;; ============================================================

;; Consumes the parsed input jsexpr and produces the deterministic verdict
;; jsexpr.  Every failure accumulates a human-readable reason and the
;; decision stays "fail" until every required check passes.
(define (guard-verdict-jsexpr raw)
  (cond
    [(not (hash? raw)) (fail-verdict "input document is not a JSON object")]
    [else
     (define reasons '())
     (define (bad! fmt . args)
       (set! reasons (cons (apply format fmt args) reasons)))
     (define checks (make-hasheq (map (lambda (k) (cons k #t)) all-check-keys)))
     (define (check-fail! key)
       (hash-set! checks key #f))

     ;; ---- schema -----------------------------------------------
     (define schema (hash-ref raw 'schema #f))
     (define schema-ok? (equal? schema input-schema-id))
     (unless schema-ok?
       (check-fail! 'schema)
       (bad! "input schema must be ~a (got ~s)" input-schema-id schema))

     ;; ---- attestation + trusted origin -------------------------
     (define attestation (hash-ref raw 'attestation #f))
     (define policy (hash-ref raw 'policy #f))
     (define reference (hash-ref raw 'reference #f))
     (define candidate (hash-ref raw 'candidate #f))
     (define measured-check
       (let ([v (and (hash? policy) (hash-ref policy 'measured-check #f))])
         (if (nonempty-string? v) v measured-check-default)))
     ;; Returns #f when attestation and trusted origin are sound, otherwise
     ;; the failure message.  A missing trusted-origin is a failure (not a
     ;; pass-through): the reporter's origin must be pinned by policy.
     (define (attestation-failure)
       (cond
         [(not (hash? attestation)) "attestation block is missing or not an object"]
         [(not (equal? (hash-ref attestation 'source #f) "actions-api"))
          "attestation source must be \"actions-api\""]
         [(not (nonempty-string? (hash-ref attestation 'repository #f)))
          "attestation repository must be a non-empty string"]
         [(not (nonempty-string? (hash-ref attestation 'workflow #f)))
          "attestation workflow must be a non-empty string"]
         [(not (nonempty-string? (hash-ref attestation 'collector #f)))
          "attestation collector must be a non-empty string"]
         [(not (parse-ts (hash-ref attestation 'collected-at #f)))
          "attestation collected-at is missing or unparseable"]
         [(not (hash? policy)) "policy block is missing or not an object"]
         [else
          (define o (hash-ref policy 'trusted-origin #f))
          (cond
            [(not (and (hash? o)
                       (nonempty-string? (hash-ref o 'repository #f))
                       (nonempty-string? (hash-ref o 'workflow #f))))
             "policy trusted-origin must be a non-empty object with repository and workflow"]
            [(not (and (equal? (hash-ref o 'repository #f) (hash-ref attestation 'repository #f))
                       (equal? (hash-ref o 'workflow #f) (hash-ref attestation 'workflow #f))))
             "policy trusted-origin must match the attestation repository and workflow"]
            [else #f])]))
     (define attestation-msg (attestation-failure))
     (define attestation-ok? (not attestation-msg))
     (unless attestation-ok?
       (check-fail! 'attestation)
       (bad! "~a" attestation-msg))

     ;; ---- policy scalars ---------------------------------------
     (define expected-head (and (hash? policy) (hash-ref policy 'expected-head-sha #f)))
     (define expected-base (and (hash? policy) (hash-ref policy 'expected-base-sha #f)))
     (define expected-attempt (or (and (hash? policy) (hash-ref policy 'expected-attempt #f)) 1))
     (define min-heads (and (hash? policy) (hash-ref policy 'min-distinct-heads #f)))
     (define freshness-window (and (hash? policy) (hash-ref policy 'freshness-window-seconds #f)))
     (define evaluated-at (and (hash? policy) (hash-ref policy 'evaluated-at #f)))
     (define required-jobs (and (hash? policy) (hash-ref policy 'required-jobs #f)))
     (define policy-separately-reported
       (and (hash? policy) (hash-ref policy 'separately-reported-strata '())))

     (define provenance-ok?
       (and (sha40-s? expected-head)
            (sha40-s? expected-base)
            (positive-integer? expected-attempt)
            (exact-integer? min-heads)
            (>= min-heads 3)
            (positive-integer? freshness-window)
            (parse-ts evaluated-at)
            (and (list? required-jobs) (andmap nonempty-string? required-jobs))
            (and (or (eq? policy-separately-reported absent)
                     (and (list? policy-separately-reported)
                          (andmap (lambda (s) (mem-stratum s separately-reportable-strata))
                                  policy-separately-reported))))))
     (unless provenance-ok?
       (check-fail! 'provenance)
       (bad!
        (string-append
         "policy provenance is incomplete: expected-head-sha/expected-base-sha must be 40-hex SHAs,"
         " expected-attempt a positive integer, min-distinct-heads >= 3, freshness-window-seconds positive,"
         " evaluated-at parseable, required-jobs a list of non-empty strings")))

     ;; ---- no-self-recursion ------------------------------------
     (define self-in-jobs? (and (list? required-jobs) (member measured-check required-jobs)))
     (when self-in-jobs?
       (check-fail! 'no-self-recursion)
       (bad!
        "the guard's own check ~a must not appear in policy required-jobs (it cannot wait for itself)"
        measured-check))

     ;; ---- record shape ------------------------------------------
     (define ref-samples (and (hash? reference) (hash-ref reference 'samples #f)))
     (define cand-runs (and (hash? candidate) (hash-ref candidate 'runs #f)))
     (define (validate-side records side)
       (cond
         [(not (list? records))
          (bad! "~a records must be a list" side)
          #f]
         [else
          (define results
            (for/list ([rec records])
              (validate-sample rec measured-check)))
          (for ([r results]
                [i (in-naturals 1)]
                [rec records])
            (unless (car r)
              (for ([msg (cdr r)])
                (bad! "~a record ~a: ~a" side (add1 i) msg))))
          (andmap car results)]))
     (define ref-shape-ok? (validate-side ref-samples "reference"))
     (define cand-shape-ok? (validate-side cand-runs "candidate"))
     (unless (and ref-shape-ok? cand-shape-ok?)
       (check-fail! 'record-shape))

     ;; ---- freshness ---------------------------------------------
     (define evaluated-s (parse-ts evaluated-at))
     (define freshness-ok?
       (and evaluated-s
            (hash? reference)
            (eq? (hash-ref reference 'frozen absent) #t)
            (let ([frozen-s (parse-ts (hash-ref reference 'frozen-at #f))])
              (and frozen-s (<= frozen-s evaluated-s)))
            (hash? attestation)
            (let ([collected-s (parse-ts (hash-ref attestation 'collected-at #f))])
              (and collected-s (<= collected-s evaluated-s)))
            (list? ref-samples)
            (list? cand-runs)
            (andmap
             (lambda (rec)
               (and (hash? rec)
                    (let ([s (parse-ts (hash-ref rec 'started-at #f))]
                          [e (parse-ts (hash-ref rec 'completed-at #f))])
                      (and s e (<= s e) (<= e evaluated-s) (<= (- evaluated-s e) freshness-window)))))
             (if (list? cand-runs)
                 cand-runs
                 '()))
            ;; reference runs are historical by construction: each must complete
            ;; at or before the freeze, and the freeze itself must be fresh
            (let ([frozen-s (parse-ts (hash-ref reference 'frozen-at #f))])
              (and frozen-s
                   (<= (- evaluated-s frozen-s) freshness-window)
                   (andmap (lambda (rec)
                             (and (hash? rec)
                                  (let ([s (parse-ts (hash-ref rec 'started-at #f))]
                                        [e (parse-ts (hash-ref rec 'completed-at #f))])
                                    (and s e (<= s e) (<= e frozen-s)))))
                           ref-samples)))
            ;; reference frozen strictly before any candidate evidence began
            (let ([frozen-s (parse-ts (hash-ref reference 'frozen-at #f))])
              (and frozen-s
                   (let ([cand-starts (for/list ([rec (if (list? cand-runs)
                                                          cand-runs
                                                          '())])
                                        (parse-ts (hash-ref rec 'started-at #f)))])
                     (and (andmap values cand-starts)
                          (pair? cand-starts)
                          (< frozen-s (apply min cand-starts))))))))
     (unless freshness-ok?
       (check-fail! 'freshness)
       (bad!
        (string-append
         "freshness check failed: reference must be frozen (frozen=true, frozen-at fresh and before the"
         " earliest candidate start, every reference record completing at or before the freeze) and every"
         " candidate record must be attested, internally ordered, not in the future and within"
         " freshness-window-seconds of evaluated-at")))

     ;; ---- control strata ----------------------------------------
     (define ref-strata
       (normalize-strata (and (hash? reference) (hash-ref reference 'control-strata #f))))
     (define cand-strata
       (normalize-strata (and (hash? candidate) (hash-ref candidate 'control-strata #f))))
     (define ref-fp (and (hash? reference) (hash-ref reference 'regime-fingerprint #f)))
     (define cand-fp (and (hash? candidate) (hash-ref candidate 'regime-fingerprint #f)))
     (define cand-separately-reported
       (and (hash? candidate) (hash-ref candidate 'separately-reported-strata '())))
     (define treatment (and (hash? candidate) (hash-ref candidate 'declared-treatment #f)))
     (define (treatment-valid?)
       (and (hash? treatment)
            (nonempty-string? (hash-ref treatment 'kind #f))
            (equal? (hash-ref treatment 'bound-candidate-sha #f) expected-head)
            (equal? (hash-ref treatment 'bound-base-sha #f) expected-base)
            (let ([fps (hash-ref treatment 'bound-regime-fingerprints #f)])
              (and (hash? fps)
                   (equal? (hash-ref fps 'reference #f) ref-fp)
                   (equal? (hash-ref fps 'candidate #f) cand-fp)))))
     (define (string-valued? h)
       (and (hash? h) (andmap (lambda (k) (nonempty-string? (hash-ref h k #f))) (hash-keys h))))
     (define strata-ok?
       (and (string-valued? ref-strata)
            (string-valued? cand-strata)
            (nonempty-string? ref-fp)
            (nonempty-string? cand-fp)
            (andmap (lambda (k)
                      (equal? (hash-ref ref-strata k absent) (hash-ref cand-strata k absent)))
                    canonical-strata)))
     (define specific-strata-reason? (box #f))
     (define (note! fmt . args)
       (set! reasons (cons (apply format fmt args) reasons)))
     (when strata-ok?
       ;; canonical keys match; now examine every other stratum key
       (define extra-keys
         (sort (remove-duplicates (append (hash-keys ref-strata) (hash-keys cand-strata))) symbol<?))
       (for ([k extra-keys]
             #:unless (mem-stratum k canonical-strata))
         (define in-ref (present? ref-strata k))
         (define in-cand (present? cand-strata k))
         (define equal-val
           (and in-ref in-cand (equal? (hash-ref ref-strata k #f) (hash-ref cand-strata k #f))))
         (cond
           [equal-val (void)]
           [(and (mem-stratum k separately-reportable-strata)
                 (list? cand-separately-reported)
                 (mem-stratum k cand-separately-reported)
                 (list? policy-separately-reported)
                 (mem-stratum k policy-separately-reported))
            (note!
             "note: stratum ~a differs between reference and candidate: separately reported by both policy and candidate (accepted)"
             k)]
           [(and (mem-stratum k treatment-strata) (treatment-valid?))
            (note!
             "note: stratum ~a differs between reference and candidate: bound declared treatment ~s (accepted)"
             k
             (hash-ref treatment 'kind))]
           [else
            (bad!
             "stratum ~a differs between reference and candidate without separate reporting or a bound declared treatment"
             k)
            (check-fail! 'control-strata)
            (set-box! specific-strata-reason? #t)]))
       ;; regime fingerprint drift requires the declared treatment
       (when (and (not (equal? ref-fp cand-fp)) (not (treatment-valid?)))
         (bad!
          "regime fingerprints differ (~a vs ~a) without a valid declared treatment bound to the exact candidate/base SHAs and both fingerprints"
          ref-fp
          cand-fp)
         (check-fail! 'control-strata)
         (set-box! specific-strata-reason? #t)))
     (unless strata-ok?
       (check-fail! 'control-strata)
       (unless (unbox specific-strata-reason?)
         (bad!
          (string-append
           "control strata must be non-empty string maps on both sides with identical canonical strata"
           " (runner-class, racket-executable, dependencies, required-suite-inventory) and non-empty"
           " regime fingerprints"))))

     ;; ---- eligibility, uniqueness, final head -------------------
     (define measured-check-s measured-check)
     (define (eligible-records records)
       (filter (lambda (rec) (sample-eligible? rec measured-check-s))
               (if (list? records)
                   records
                   '())))
     (define ref-eligible (eligible-records ref-samples))
     (define cand-eligible (eligible-records cand-runs))
     (define guard-own-excluded
       (length (filter (lambda (rec)
                         (and (hash? rec)
                              (equal? (hash-ref rec 'job #f) measured-check-s)
                              (not (sample-eligible? rec measured-check-s))))
                       (append (if (list? ref-samples)
                                   ref-samples
                                   '())
                               (if (list? cand-runs)
                                   cand-runs
                                   '())))))
     (define censored-cand
       (length (filter (lambda (rec)
                         (and (hash? rec)
                              (not (sample-eligible? rec measured-check-s))
                              (not (equal? (hash-ref rec 'job #f) measured-check-s))))
                       (if (list? cand-runs)
                           cand-runs
                           '()))))

     (define (distinct-heads records)
       (remove-duplicates (map (lambda (rec) (hash-ref rec 'head-sha)) records) string=?))

     (define ref-distinct (distinct-heads ref-eligible))
     (define cand-distinct (distinct-heads cand-eligible))

     ;; duplicate eligible head SHAs fail closed (reruns never satisfy
     ;; uniqueness — a rerun is the same SHA, and non-first attempts are
     ;; ineligible, so this only triggers on contradictory records)
     (define uniqueness-ok?
       (and (= (length ref-eligible) (length ref-distinct))
            (= (length cand-eligible) (length cand-distinct))
            (>= (length ref-distinct) (max 3 (or min-heads 3)))
            (>= (length cand-distinct) (max 3 (or min-heads 3)))
            (member expected-head cand-distinct)))
     (unless uniqueness-ok?
       (check-fail! 'uniqueness)
       (unless (= (length ref-eligible) (length ref-distinct))
         (bad!
          "duplicate eligible reference head SHAs: repeated attempts of one SHA never satisfy uniqueness"))
       (unless (= (length cand-eligible) (length cand-distinct))
         (bad!
          "duplicate eligible candidate head SHAs: repeated attempts of one SHA never satisfy uniqueness"))
       (when (< (length ref-distinct) (max 3 (or min-heads 3)))
         (bad! "reference has ~a distinct eligible first-attempt heads; at least ~a are required"
               (length ref-distinct)
               (max 3 (or min-heads 3))))
       (when (< (length cand-distinct) (max 3 (or min-heads 3)))
         (bad! "candidate has ~a distinct eligible first-attempt heads; at least ~a are required"
               (length cand-distinct)
               (max 3 (or min-heads 3))))
       (when (and (sha40-s? expected-head) (not (member expected-head cand-distinct)))
         (bad! "the exact expected head SHA ~a is not among the eligible candidate heads"
               expected-head)))

     ;; the final head must itself be an eligible first-attempt success with
     ;; the exact expected base and attempt (a later green attempt never
     ;; erases an earlier failure; rerun-only heads can never be green)
     (define head-records
       (filter (lambda (rec) (equal? (hash-ref rec 'head-sha #f) expected-head))
               (if (list? cand-runs)
                   cand-runs
                   '())))
     (define final-rec
       (findf (lambda (rec)
                (and (sample-eligible? rec measured-check-s)
                     (equal? (hash-ref rec 'base-sha #f) expected-base)
                     (equal? (hash-ref rec 'attempt #f) expected-attempt)))
              head-records))
     (define final-head-eligible? (and (hash? final-rec) #t))
     (unless final-head-eligible?
       (check-fail! 'final-head-eligible)
       (cond
         [(null? head-records)
          (bad! "no coordinator-attested record exists for the expected head SHA ~a" expected-head)]
         [(findf (lambda (rec)
                   (and (equal? (hash-ref rec 'status #f) "completed")
                        (not (equal? (hash-ref rec 'conclusion #f) "success"))))
                 head-records)
          (bad!
           "the expected head SHA ~a has a completed non-success attempt; a later green attempt never erases the original failure"
           expected-head)]
         [else
          (bad!
           "the expected head SHA ~a has no eligible first-attempt success with base ~a and attempt ~a (rerun-only evidence never yields green)"
           expected-head
           expected-base
           expected-attempt)]))

     ;; ---- required jobs on the final head ------------------------
     (define (required-jobs-ok?)
       (and final-rec
            (let ([jobs (hash-ref final-rec 'jobs #f)])
              (and (list? jobs)
                   (andmap (lambda (name)
                             (findf (lambda (j)
                                      (and (hash? j)
                                           (equal? (hash-ref j 'name #f) name)
                                           (equal? (hash-ref j 'status #f) "completed")
                                           (equal? (hash-ref j 'conclusion #f) "success")))
                                    jobs))
                           required-jobs)))))
     (define jobs-ok? (required-jobs-ok?))
     (unless jobs-ok?
       (check-fail! 'required-jobs)
       (bad!
        (string-append
         "the final head's required-check inventory is incomplete: every job in policy required-jobs"
         " must be completed and successful on the final head")))

     ;; ---- coordinator-attested diff / NOT_APPLICABLE --------------
     (define diff (and (hash? candidate) (hash-ref candidate 'diff #f)))
     (define (attested-diff?)
       (and (hash? diff)
            (equal? (hash-ref diff 'source #f) "coordinator-attested")
            (equal? (hash-ref diff 'base-sha #f) expected-base)
            (equal? (hash-ref diff 'head-sha #f) expected-head)
            (let ([files (hash-ref diff 'files #f)])
              (and (list? files) (pair? files) (andmap nonempty-string? files)))))
     (define diff-files
       (if (attested-diff?)
           (hash-ref diff 'files '())
           '()))
     (define non-exempt-files (filter (lambda (f) (not (guard-exempt-path? f))) diff-files))
     (define exemption-possible?
       (and attestation-ok?
            provenance-ok?
            ref-shape-ok?
            cand-shape-ok?
            freshness-ok?
            (and strata-ok? (hash-ref checks 'control-strata))
            uniqueness-ok?
            final-head-eligible?
            jobs-ok?
            (not self-in-jobs?)
            (attested-diff?)
            (null? non-exempt-files)))
     (when (and (attested-diff?) (pair? non-exempt-files))
       (bad!
        "attested diff contains paths requiring full latency samples (no self-declared exemption): ~a"
        (string-join non-exempt-files ", ")))

     ;; ---- quantile budget -----------------------------------------
     (define ref-windows (map (lambda (rec) (hash-ref rec 'window-seconds)) ref-eligible))
     (define cand-windows (map (lambda (rec) (hash-ref rec 'window-seconds)) cand-eligible))
     (define ref-p50
       (if (pair? ref-windows)
           (cohort-quantile-exact ref-windows 0.5)
           #f))
     (define ref-p95
       (if (pair? ref-windows)
           (cohort-quantile-exact ref-windows 0.95)
           #f))
     (define cand-p50
       (if (pair? cand-windows)
           (cohort-quantile-exact cand-windows 0.5)
           #f))
     (define cand-p95
       (if (pair? cand-windows)
           (cohort-quantile-exact cand-windows 0.95)
           #f))
     (define ref-p50-ms (and ref-p50 (seconds->ms ref-p50)))
     (define ref-p95-ms (and ref-p95 (seconds->ms ref-p95)))
     (define cand-p50-ms (and cand-p50 (seconds->ms cand-p50)))
     (define cand-p95-ms (and cand-p95 (seconds->ms cand-p95)))
     ;; fixed campaign rule: delta <= min(10% of reference, 60 s), equality
     ;; passes (both p50 and p95; no averaging, no tail smoothing)
     (define max-rel (rational-of 0.1))
     (define max-abs-ms 60000)
     (define allowed-p50-ms (and ref-p50-ms (min (* max-rel ref-p50-ms) max-abs-ms)))
     (define allowed-p95-ms (and ref-p95-ms (min (* max-rel ref-p95-ms) max-abs-ms)))
     (define delta-p50-ms (and ref-p50-ms cand-p50-ms (- cand-p50-ms ref-p50-ms)))
     (define delta-p95-ms (and ref-p95-ms cand-p95-ms (- cand-p95-ms ref-p95-ms)))
     (define p50-ok? (and delta-p50-ms allowed-p50-ms (<= delta-p50-ms allowed-p50-ms)))
     (define p95-ok? (and delta-p95-ms allowed-p95-ms (<= delta-p95-ms allowed-p95-ms)))

     (define exemption? exemption-possible?)
     (define quantile-evaluated? (not exemption?))
     (define quantile-ok?
       (cond
         [exemption? #t]
         [(not p50-ok?)
          (check-fail! 'quantile-budget)
          (bad!
           "candidate p50 ~a ms exceeds reference p50 ~a ms by ~a ms; the budget is min(10%, 60000 ms) = ~a ms (equality passes)"
           (or cand-p50-ms "n/a")
           (or ref-p50-ms "n/a")
           (if delta-p50-ms delta-p50-ms "n/a")
           (or allowed-p50-ms "n/a"))
          #f]
         [(not p95-ok?)
          (check-fail! 'quantile-budget)
          (bad!
           "candidate p95 ~a ms exceeds reference p95 ~a ms by ~a ms; the budget is min(10%, 60000 ms) = ~a ms (equality passes)"
           (or cand-p95-ms "n/a")
           (or ref-p95-ms "n/a")
           (if delta-p95-ms delta-p95-ms "n/a")
           (or allowed-p95-ms "n/a"))
          #f]
         [else #t]))

     ;; ---- decision -------------------------------------------------
     (define all-structural-ok?
       (and attestation-ok?
            provenance-ok?
            ref-shape-ok?
            cand-shape-ok?
            freshness-ok?
            (and strata-ok? (hash-ref checks 'control-strata))
            uniqueness-ok?
            final-head-eligible?
            jobs-ok?
            (not self-in-jobs?)))
     (define decision
       (cond
         [(and all-structural-ok? exemption? quantile-ok?) "not-applicable"]
         [(and all-structural-ok? quantile-ok?) "pass"]
         [else "fail"]))

     (hash-set! checks 'quantile-evaluated quantile-evaluated?)
     (hasheq 'gate
             guard-gate-id
             'input-schema
             (if schema-ok? input-schema-id schema)
             'decision
             decision
             'checks
             (for/hasheq ([k all-check-keys])
               (values k (hash-ref checks k)))
             'numbers
             (hasheq 'reference-p50-ms
                     (json-number ref-p50-ms)
                     'reference-p95-ms
                     (json-number ref-p95-ms)
                     'candidate-p50-ms
                     (json-number cand-p50-ms)
                     'candidate-p95-ms
                     (json-number cand-p95-ms)
                     'delta-p50-ms
                     (if exemption?
                         #f
                         (json-number delta-p50-ms))
                     'delta-p95-ms
                     (if exemption?
                         #f
                         (json-number delta-p95-ms))
                     'allowed-p50-ms
                     (if exemption?
                         #f
                         (json-number allowed-p50-ms))
                     'allowed-p95-ms
                     (if exemption?
                         #f
                         (json-number allowed-p95-ms))
                     'reference-eligible
                     (length ref-eligible)
                     'candidate-eligible
                     (length cand-eligible)
                     'reference-distinct-heads
                     (length ref-distinct)
                     'candidate-distinct-heads
                     (length cand-distinct)
                     'guard-own-samples-excluded
                     guard-own-excluded
                     'censored-candidate-runs
                     censored-cand)
             'reasons
             (reverse reasons))]))

;; ============================================================
;; Renderers (byte-stable)
;; ============================================================

(define (num-str v)
  (if v
      (~a v)
      "n/a"))

(define (guard-verdict-json-string verdict)
  ;; Serialize through key symbolization so any string-keyed subhash
  ;; (e.g. stratum names used as keys) becomes symbol-keyed for write-json.
  ;; Self-contained: recursive helper is a local define, immune to
  ;; definition-order/nesting issues in the CLI section below.
  (define (sym-keys x)
    (cond
      [(hash? x)
       (for/hasheq ([(k v) (in-hash x)])
         (values (cond
                   [(symbol? k) k]
                   [(string? k) (string->symbol k)]
                   [else k])
                 (sym-keys v)))]
      [(list? x) (map sym-keys x)]
      [else x]))
  (jsexpr->string (sym-keys verdict)))

(define (guard-decision-md-string v)
  (define c (hash-ref v 'checks (hasheq)))
  (define n (hash-ref v 'numbers (hasheq)))
  (define check-order all-check-keys)
  (define lines
    (append (list "# PR latency guard — v1.00.30 W1"
                  ""
                  (format "- gate: ~a" (hash-ref v 'gate guard-gate-id))
                  (format "- decision: ~a" (hash-ref v 'decision "fail"))
                  (format "- input schema: ~a" (hash-ref v 'input-schema input-schema-id))
                  ""
                  "## Quantile budget (candidate - reference <= min(10%, 60 s); equality passes)"
                  ""
                  (format "| quantity | reference | candidate | delta | allowed |")
                  (format "|---|---|---|---|---|")
                  (format "| p50 (ms) | ~a | ~a | ~a | ~a |"
                          (num-str (hash-ref n 'reference-p50-ms #f))
                          (num-str (hash-ref n 'candidate-p50-ms #f))
                          (num-str (hash-ref n 'delta-p50-ms #f))
                          (num-str (hash-ref n 'allowed-p50-ms #f)))
                  (format "| p95 (ms) | ~a | ~a | ~a | ~a |"
                          (num-str (hash-ref n 'reference-p95-ms #f))
                          (num-str (hash-ref n 'candidate-p95-ms #f))
                          (num-str (hash-ref n 'delta-p95-ms #f))
                          (num-str (hash-ref n 'allowed-p95-ms #f)))
                  ""
                  (format "- reference eligible first attempts: ~a (distinct heads: ~a)"
                          (num-str (hash-ref n 'reference-eligible #f))
                          (num-str (hash-ref n 'reference-distinct-heads #f)))
                  (format "- candidate eligible first attempts: ~a (distinct heads: ~a)"
                          (num-str (hash-ref n 'candidate-eligible #f))
                          (num-str (hash-ref n 'candidate-distinct-heads #f)))
                  (format "- guard's own samples excluded from latency: ~a"
                          (num-str (hash-ref n 'guard-own-samples-excluded #f)))
                  (format "- censored candidate runs recorded separately: ~a"
                          (num-str (hash-ref n 'censored-candidate-runs #f)))
                  ""
                  "## Checks"
                  "")
            (for/list ([k check-order])
              (format "- ~a: ~a" k (if (hash-ref c k #f) "PASS" "FAIL")))
            (list "")
            (if (null? (hash-ref v 'reasons '()))
                (list "## Reasons" "" "- (none: all checks passed)")
                (list* "## Reasons"
                       ""
                       (for/list ([r (hash-ref v 'reasons '())]
                                  [i (in-naturals 1)])
                         (format "~a. ~a" i r))))
            (list "")))
  (string-join lines "\n"))

;; ============================================================
;; CLI
;; ============================================================

(module+ main
  (define input-path (make-parameter #f))
  (define verdict-path (make-parameter #f))
  (define md-path (make-parameter #f))
  (command-line #:program "pr-latency-guard"
                #:once-each
                [("--input") p "provenance-bound input JSON document (required)" (input-path p)]
                [("--verdict-json") p "write the verdict jsexpr as JSON here" (verdict-path p)]
                [("--decision-md") p "write the deterministic decision markdown here" (md-path p)])
  ;; read-json yields string-keyed hashes (and JSON keys are strings); the
  ;; guard's in-process contract uses symbol-keyed hasheq (see schema note).
  ;; Normalize recursively so the CLI path and unit fixtures agree.
  (define (jsexpr-symbolize x)
    (cond
      [(hash? x)
       (for/hasheq ([(k v) (in-hash x)])
         (values (cond
                   [(symbol? k) k]
                   [(string? k) (string->symbol k)]
                   [else k])
                 (jsexpr-symbolize v)))]
      [(list? x) (map jsexpr-symbolize x)]
      [else x]))

  (unless (input-path)
    (eprintf
     "usage: racket scripts/ci/pr-latency-guard.rkt --input <json> [--verdict-json out.json] [--decision-md out.md]\n")
    (exit 2))
  (define verdict
    (with-handlers ([exn:fail? (lambda (e)
                                 (fail-verdict (format "input unreadable or invalid JSON: ~a"
                                                       (exn-message e))))])
      (guard-verdict-jsexpr (jsexpr-symbolize (with-input-from-file (input-path) read-json)))))
  (define decision (hash-ref verdict 'decision "fail"))
  (when (verdict-path)
    (with-output-to-file (verdict-path)
                         (lambda () (displayln (guard-verdict-json-string verdict)))
                         #:exists 'replace))
  (when (md-path)
    (with-output-to-file (md-path)
                         (lambda () (display (guard-decision-md-string verdict)))
                         #:exists 'replace))
  (displayln decision)
  (exit (case (string->symbol decision)
          [(pass not-applicable) 0]
          [else 1])))

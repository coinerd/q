#lang racket/base

;; tests/test-pr-latency-guard.rkt — v1.00.30 W1 regression suite for
;; scripts/ci/pr-latency-guard.rkt (blocking PR latency guard).
;;
;; Covers the wave-mandated cases (waves/W1-blocking-pr-latency-guard.md
;; task 2): 500 s reference with the +50 s boundary, 1000 s reference with
;; the +60 s boundary, p95-only regression, wrong head/base/attempt,
;; duplicate heads, absent required jobs, NOT_APPLICABLE allowlist rules,
;; no-self-recursion, rerun-only evidence, staleness, forged provenance and
;; determinism of the emitted verdict.

(require rackunit
         racket/file
         racket/port
         racket/string
         json
         "../scripts/ci/pr-latency-guard.rkt")

;; ------------------------------------------------------------
;; Fixture builders
;; ------------------------------------------------------------

(define (sha n)
  (substring (string-append (number->string n 16) (make-string 40 #\a)) 0 40))

(define T-FROZEN "2026-01-01T00:00:00Z")
(define T-START "2026-01-02T00:00:00Z")
(define T-END "2026-01-02T01:00:00Z")
(define T-EVAL "2026-01-05T00:00:00Z")

(define HEAD-1 (sha 1))
(define HEAD-2 (sha 2))
(define HEAD-3 (sha 3))
(define HEAD-4 (sha 4))
(define BASE (sha 100))

(define CANONICAL-STRATA
  (hasheq "runner-class"
          "ubuntu-latest"
          "racket-executable"
          "racket/8.16"
          "dependencies"
          "lock@2026-01-01"
          "required-suite-inventory"
          "fast-suite@1"))

(define (check-job name)
  (hasheq 'name name 'status "completed" 'conclusion "success" 'attempt 1))

(define REQUIRED-JOBS '("lint" "test (0)"))
(define DEFAULT-JOBS (map check-job REQUIRED-JOBS))

;; Build a run record; optional keys are omitted (not #f) when absent so
;; shape validation sees exactly what a real collector would emit.
(define (rec head
             window
             #:base [base BASE]
             #:attempt [attempt 1]
             #:first? [first? #t]
             #:status [status "completed"]
             #:conclusion [conclusion "success"]
             #:start [start T-START]
             #:end [end T-END]
             #:job [job #f]
             #:jobs [jobs DEFAULT-JOBS]
             #:attested [attested #t])
  (define h
    (hasheq 'head-sha
            head
            'base-sha
            base
            'attempt
            attempt
            'first-attempt
            first?
            'status
            status
            'conclusion
            conclusion
            'started-at
            start
            'completed-at
            end
            'window-seconds
            window
            'attested
            attested))
  (define h2
    (if job
        (hash-set h 'job job)
        h))
  (if (eq? jobs #f)
      h2
      (hash-set h2 'jobs jobs)))

(define (mk-policy #:head [head HEAD-3]
                   #:base [base BASE]
                   #:attempt [attempt 1]
                   #:required-jobs [required REQUIRED-JOBS]
                   #:evaluated-at [evaluated-at T-EVAL]
                   #:freshness [freshness 2592000]
                   #:min-heads [min-heads 3]
                   #:separately [separately '("cache-availability")]
                   #:trusted-origin [trusted-origin (hasheq 'repository "coinerd/q" 'workflow "CI")])
  (hasheq 'trusted-origin
          trusted-origin
          'expected-head-sha
          head
          'expected-base-sha
          base
          'expected-attempt
          attempt
          'min-distinct-heads
          min-heads
          'freshness-window-seconds
          freshness
          'evaluated-at
          evaluated-at
          'measured-check
          "pr-latency-guard"
          'required-jobs
          required
          'separately-reported-strata
          separately))

(define (mk-reference #:frozen [frozen #t]
                      #:frozen-at [frozen-at T-FROZEN]
                      #:fingerprint [fingerprint "lazy/1"]
                      #:strata [strata CANONICAL-STRATA]
                      #:samples [samples (list (rec HEAD-1 500) (rec HEAD-2 500) (rec HEAD-3 500))])
  (hasheq 'frozen
          frozen
          'frozen-at
          frozen-at
          'regime-fingerprint
          fingerprint
          'control-strata
          strata
          'samples
          samples))

(define (mk-candidate #:fingerprint [fingerprint "lazy/1"]
                      #:strata [strata CANONICAL-STRATA]
                      #:runs [runs (list (rec HEAD-1 500) (rec HEAD-2 500) (rec HEAD-3 500))]
                      #:treatment [treatment #f]
                      #:separately [separately '()]
                      #:diff [diff #f])
  (define h (hasheq 'regime-fingerprint fingerprint 'control-strata strata 'runs runs))
  (define h2
    (if treatment
        (hash-set h 'declared-treatment treatment)
        h))
  (define h3
    (if (null? separately)
        h2
        (hash-set h2 'separately-reported-strata separately)))
  (if diff
      (hash-set h3 'diff diff)
      h3))

(define (mk-input #:policy [policy (mk-policy)]
                  #:reference [reference (mk-reference)]
                  #:candidate [candidate (mk-candidate)]
                  #:attestation [attestation
                                 (hasheq 'repository
                                         "coinerd/q"
                                         'workflow
                                         "CI"
                                         'source
                                         "actions-api"
                                         'collector
                                         "coordinator"
                                         'collected-at
                                         T-EVAL)]
                  #:schema [schema input-schema-id])
  (hasheq 'schema
          schema
          'attestation
          attestation
          'policy
          policy
          'reference
          reference
          'candidate
          candidate))

(define (treatment kind)
  (hasheq 'kind
          kind
          'bound-candidate-sha
          HEAD-3
          'bound-base-sha
          BASE
          'bound-regime-fingerprints
          (hasheq 'reference "lazy/1" 'candidate "eager/2")))

(define (attested-diff files)
  (hasheq 'source "coordinator-attested" 'base-sha BASE 'head-sha HEAD-3 'files files))

;; ------------------------------------------------------------
;; Core boundary math (PLAN: candidate - reference <= min(10%, 60 s);
;; equality passes; both p50 and p95)
;; ------------------------------------------------------------

(define (decide in)
  (hash-ref (guard-verdict-jsexpr in) 'decision))

(test-case "valid green evidence passes"
  (check-equal? (decide (mk-input)) "pass")
  (define v (guard-verdict-jsexpr (mk-input)))
  (for ([k '(schema attestation
                    record-shape
                    provenance
                    freshness
                    uniqueness
                    required-jobs
                    control-strata
                    quantile-budget
                    no-self-recursion
                    final-head-eligible)])
    (check-true (hash-ref (hash-ref v 'checks) k) (format "check ~a" k)))
  (check-true (hash-ref (hash-ref v 'checks) 'quantile-evaluated))
  (check-equal? (hash-ref (hash-ref v 'numbers) 'delta-p50-ms) 0)
  (check-equal? (hash-ref (hash-ref v 'numbers) 'reference-distinct-heads) 3)
  (check-equal? (hash-ref (hash-ref v 'numbers) 'candidate-distinct-heads) 3))

(test-case "500 s reference: +50 s boundary passes (equality passes)"
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 550)
                                                                         (rec HEAD-2 550)
                                                                         (rec HEAD-3 550)))))
                "pass"))

(test-case "500 s reference: +51 s fails (10% budget binds at 50 s)"
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 551)
                                                                         (rec HEAD-2 551)
                                                                         (rec HEAD-3 551)))))
                "fail"))

(test-case "1000 s reference: +60 s boundary passes (60 s cap binds)"
  (check-equal?
   (decide
    (mk-input
     #:reference (mk-reference #:samples (list (rec HEAD-1 1000) (rec HEAD-2 1000) (rec HEAD-3 1000)))
     #:candidate (mk-candidate #:runs (list (rec HEAD-1 1060) (rec HEAD-2 1060) (rec HEAD-3 1060)))))
   "pass"))

(test-case "1000 s reference: +61 s fails"
  (check-equal?
   (decide
    (mk-input
     #:reference (mk-reference #:samples (list (rec HEAD-1 1000) (rec HEAD-2 1000) (rec HEAD-3 1000)))
     #:candidate (mk-candidate #:runs (list (rec HEAD-1 1061) (rec HEAD-2 1061) (rec HEAD-3 1061)))))
   "fail"))

(test-case "p95-only regression fails even when p50 is flat"
  ;; p50 of [500 500 570] = 500 (delta 0, passes); p95 = 570 (+70 > 50 s).
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 500)
                                                                         (rec HEAD-2 500)
                                                                         (rec HEAD-3 570)))))
                "fail"))

;; ------------------------------------------------------------
;; Provenance, uniqueness and liveness
;; ------------------------------------------------------------

(test-case "wrong expected head fails (unknown evidence never green)"
  (check-equal? (decide (mk-input #:policy (mk-policy #:head (sha 99)))) "fail"))

(test-case "wrong base on the final head fails"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs (list (rec HEAD-1 500)
                                                             (rec HEAD-2 500)
                                                             (rec HEAD-3 500 #:base (sha 77))))))
                "fail"))

(test-case "attempt mismatch on the final head fails"
  (check-equal? (decide (mk-input #:policy (mk-policy #:attempt 2))) "fail"))

(test-case "duplicate eligible head SHAs fail closed"
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 500)
                                                                         (rec HEAD-2 500)
                                                                         (rec HEAD-2 500)
                                                                         (rec HEAD-3 500)))))
                "fail"))

(test-case "fewer than 3 distinct candidate heads fails"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs (list (rec HEAD-1 500)
                                                             (rec HEAD-2 500)
                                                             (rec HEAD-3 500 #:attested #f)))))
                "fail"))

(test-case "rerun-only final head never yields green (later green attempt never erases a failure)"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs
                                                (list (rec HEAD-1 500)
                                                      (rec HEAD-2 500)
                                                      (rec HEAD-3 500 #:conclusion "failure")
                                                      (rec HEAD-3 480 #:first? #f #:attempt 2)))))
                "fail"))

(test-case "absent required jobs on the final head fails"
  (check-equal?
   (decide (mk-input #:candidate
                     (mk-candidate #:runs (list (rec HEAD-1 500)
                                                (rec HEAD-2 500)
                                                (rec HEAD-3 500 #:jobs (list (check-job "lint")))))))
   "fail"))

(test-case "non-success required job on the final head fails"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs (list (rec HEAD-1 500)
                                                             (rec HEAD-2 500)
                                                             (rec HEAD-3
                                                                  500
                                                                  #:jobs (list (check-job "lint")
                                                                               (hasheq 'name
                                                                                       "test (0)"
                                                                                       'status
                                                                                       "completed"
                                                                                       'conclusion
                                                                                       "failure"
                                                                                       'attempt
                                                                                       1)))))))
                "fail"))

(test-case "the guard's own samples are excluded from latency"
  (define v
    (guard-verdict-jsexpr
     (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 500)
                                                      (rec HEAD-2 500)
                                                      (rec HEAD-3 500)
                                                      (rec HEAD-4 999 #:job "pr-latency-guard"))))))
  (check-equal? (hash-ref v 'decision) "pass")
  (check-equal? (hash-ref (hash-ref v 'numbers) 'candidate-distinct-heads) 3)
  (check-equal? (hash-ref (hash-ref v 'numbers) 'guard-own-samples-excluded) 1))

(test-case "second attempts never enter the quantiles"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs
                                                (list (rec HEAD-1 500)
                                                      (rec HEAD-2 500)
                                                      (rec HEAD-3 500)
                                                      (rec HEAD-1 5000 #:first? #f #:attempt 2)))))
                "pass"))

;; ------------------------------------------------------------
;; Freshness and attestation
;; ------------------------------------------------------------

(test-case "stale evidence fails"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs (list (rec HEAD-1 500)
                                                             (rec HEAD-2 500)
                                                             (rec HEAD-3
                                                                  500
                                                                  #:start "2025-11-01T00:00:00Z"
                                                                  #:end "2025-11-01T01:00:00Z")))))
                "fail"))

(test-case "unfrozen reference fails"
  (check-equal? (decide (mk-input #:reference (mk-reference #:frozen #f))) "fail"))

(test-case "reference frozen after candidate work started fails"
  (check-equal? (decide (mk-input #:reference (mk-reference #:frozen-at "2026-01-03T00:00:00Z")))
                "fail"))

(test-case "unattested records fail closed"
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:runs (list (rec HEAD-1 500)
                                                             (rec HEAD-2 500)
                                                             (rec HEAD-3 500 #:attested #f)))))
                "fail"))

(test-case "forged attestation source fails"
  (check-equal? (decide (mk-input #:attestation (hasheq 'repository
                                                        "coinerd/q"
                                                        'workflow
                                                        "CI"
                                                        'source
                                                        "pr-comment"
                                                        'collector
                                                        "coordinator"
                                                        'collected-at
                                                        T-EVAL)))
                "fail"))

(test-case "trusted-origin mismatch fails"
  (check-equal? (decide (mk-input #:policy
                                  (mk-policy #:trusted-origin
                                             (hasheq 'repository "other/repo" 'workflow "CI"))))
                "fail"))

(test-case "missing trusted-origin fails closed"
  (check-equal? (decide (mk-input #:policy (hash-remove (mk-policy) 'trusted-origin))) "fail"))

(test-case "non-object and wrong-schema inputs fail closed"
  (check-equal? (decide (jsexpr->string "not-an-object")) "fail") ; guard against string input
  (check-equal? (decide (hasheq 'schema "q.other/1")) "fail"))

;; ------------------------------------------------------------
;; Control strata and declared treatment
;; ------------------------------------------------------------

(test-case "canonical strata mismatch fails"
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:strata (hash-set CANONICAL-STRATA
                                                                               "racket-executable"
                                                                               "racket/8.15"))))
                "fail"))

(test-case "cache-availability drift passes only when separately reported by both sides"
  (check-equal?
   (decide (mk-input #:reference
                     (mk-reference #:strata (hash-set CANONICAL-STRATA "cache-availability" "warm"))
                     #:candidate
                     (mk-candidate #:strata (hash-set CANONICAL-STRATA "cache-availability" "cold")
                                   #:separately '("cache-availability"))))
   "pass")
  (check-equal?
   (decide (mk-input #:reference
                     (mk-reference #:strata (hash-set CANONICAL-STRATA "cache-availability" "warm"))
                     #:candidate
                     (mk-candidate #:strata (hash-set CANONICAL-STRATA "cache-availability" "cold"))))
   "fail"))

(test-case "regime fingerprint drift requires the bound declared treatment"
  (check-equal? (decide (mk-input #:candidate (mk-candidate #:fingerprint "eager/2")))
                "fail"
                (format "reasons=~s"
                        (hash-ref (guard-verdict-jsexpr
                                   (mk-input #:candidate (mk-candidate #:fingerprint "eager/2")))
                                  'reasons)))
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:fingerprint "eager/2"
                                                #:treatment (treatment "compile-regime"))))
                "pass")
  ;; treatment bound to the wrong SHAs is invalid
  (check-equal? (decide (mk-input #:candidate
                                  (mk-candidate #:fingerprint "eager/2"
                                                #:treatment (hash-set (treatment "compile-regime")
                                                                      'bound-candidate-sha
                                                                      (sha 42)))))
                "fail"))

;; ------------------------------------------------------------
;; NOT_APPLICABLE (conservative trusted-diff allowlist)
;; ------------------------------------------------------------

(test-case "docs/binding-only diffs are NOT_APPLICABLE even over budget"
  (check-equal?
   (decide (mk-input #:candidate
                     (mk-candidate #:runs (list (rec HEAD-1 900) (rec HEAD-2 900) (rec HEAD-3 900))
                                   #:diff (attested-diff (list "docs/reports/x.md"
                                                               ".planning/waves/W1.md")))))
   "not-applicable"))

(test-case "unknown/executable diff paths require full samples"
  (check-equal?
   (decide (mk-input #:candidate
                     (mk-candidate #:runs (list (rec HEAD-1 500) (rec HEAD-2 500) (rec HEAD-3 500))
                                   #:diff (attested-diff (list "docs/x.md" "scripts/tool.rkt")))))
   "pass")
  (check-equal?
   (decide (mk-input #:candidate
                     (mk-candidate #:runs (list (rec HEAD-1 500) (rec HEAD-2 500) (rec HEAD-3 500))
                                   #:diff (attested-diff (list "src/main.rkt")))))
   "pass"))

(test-case "generated config, tests and policy files are never exempt"
  (check-false (guard-exempt-path? "package-lock.json"))
  (check-false (guard-exempt-path? "info.rkt"))
  (check-false (guard-exempt-path? "tests/foo.rkt"))
  (check-false (guard-exempt-path? ".github/workflows/ci.yml"))
  (check-false (guard-exempt-path? "scripts/anything.rkt"))
  (check-true (guard-exempt-path? "docs/a.md"))
  (check-true (guard-exempt-path? ".planning/b.rktd"))
  (check-true (guard-exempt-path? "artifacts/ci-recovery/x.json"))
  (check-true (guard-exempt-path? "SHA256SUMS"))
  (check-false (guard-exempt-path? "random.rkt"))
  (check-false (guard-exempt-path? "")))

(test-case "a PR cannot self-declare the exemption (diff must be coordinator-attested and SHA-bound)"
  (check-equal?
   (decide
    (mk-input
     #:candidate
     (mk-candidate
      #:runs (list (rec HEAD-1 900) (rec HEAD-2 900) (rec HEAD-3 900))
      #:diff
      (hasheq 'source "self-declared" 'base-sha BASE 'head-sha HEAD-3 'files (list "docs/x.md")))))
   "fail")
  (check-equal?
   (decide (mk-input #:candidate
                     (mk-candidate #:runs (list (rec HEAD-1 900) (rec HEAD-2 900) (rec HEAD-3 900))
                                   #:diff
                                   (hash-set (attested-diff (list "docs/x.md")) 'head-sha (sha 5)))))
   "fail"))

;; ------------------------------------------------------------
;; No self-recursion
;; ------------------------------------------------------------

(test-case "the guard cannot require itself"
  (check-equal? (decide (mk-input #:policy (mk-policy #:required-jobs
                                                      (append REQUIRED-JOBS
                                                              (list "pr-latency-guard")))))
                "fail"))

;; ------------------------------------------------------------
;; Determinism and renderers
;; ------------------------------------------------------------

(test-case "verdict JSON is byte-stable across evaluations"
  (define a (guard-verdict-json-string (guard-verdict-jsexpr (mk-input))))
  (define b (guard-verdict-json-string (guard-verdict-jsexpr (mk-input))))
  (check-equal? a b))

(test-case "decision markdown names the decision and the budget rule"
  (define md (guard-decision-md-string (guard-verdict-jsexpr (mk-input))))
  (check-true (string-contains? md "decision: pass"))
  (check-true (string-contains? md "min(10%, 60 s)")))

;; ------------------------------------------------------------
;; CLI end-to-end: green exits 0, red exits 1
;; ------------------------------------------------------------

(require racket/path
         racket/file)

(define this-file (syntax-source #'here))
(define pkg-root (simplify-path (build-path (path-only this-file) "..")))
(define cli-script (path->string (build-path pkg-root "scripts/ci/pr-latency-guard.rkt")))
;; `exec-file` is often a bare program name (e.g. "racket"), which `subprocess`
;; resolves against the child's current directory (not PATH); resolve a
;; filesystem-absolute interpreter path for spawning the CLI under test.
(define racket-bin
  (path->string (or (find-executable-path (find-system-path 'exec-file))
                    (find-system-path 'exec-file))))

(define (run-cli in-jsexpr)
  (define dir (make-temporary-directory "plg~a"))
  (define in-file (build-path dir "input.json"))
  (define out-json (build-path dir "verdict.json"))
  (define out-md (build-path dir "decision.md"))
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
  (with-output-to-file in-file
                       (lambda () (displayln (jsexpr->string (sym-keys in-jsexpr))))
                       #:exists 'replace)
  (define out-txt (build-path dir "stdout.txt"))
  (define err-txt (build-path dir "stderr.txt"))
  (define stdout (open-output-file out-txt #:exists 'replace))
  (define stderr (open-output-file err-txt #:exists 'replace))
  (define-values (p _c-stdout _c-stdin _c-stderr)
    (subprocess stdout
                #f
                stderr
                (find-executable-path (find-system-path 'exec-file))
                cli-script
                "--input"
                (path->string in-file)
                "--verdict-json"
                (path->string out-json)
                "--decision-md"
                (path->string out-md)))
  (subprocess-wait p)
  (close-output-port stdout)
  (close-output-port stderr)
  (define out (file->string out-txt))
  (define err (file->string err-txt))
  (values (subprocess-status p)
          out
          err
          (if (file-exists? out-json)
              (with-input-from-file out-json read-json)
              #f)
          (if (file-exists? out-md)
              (file->string out-md)
              #f)))

(test-case "CLI: green input exits 0 and writes verdict + decision artifacts"
  (define-values (code stdout err verdict md) (run-cli (mk-input)))
  (check-equal? code
                0
                (format "stderr: ~a | stdout: ~a | verdict: ~s"
                        err
                        stdout
                        (and verdict (or (hash-ref verdict 'reasons #f) verdict))))
  (check-true (string-contains? stdout "pass"))
  (check-equal? (hash-ref verdict 'decision) "pass")
  (check-true (string-contains? md "decision: pass")))

(test-case "CLI: red input (over budget) exits 1 with fail-closed verdict"
  (define-values (code stdout err verdict md)
    (run-cli (mk-input #:candidate (mk-candidate #:runs (list (rec HEAD-1 900)
                                                              (rec HEAD-2 900)
                                                              (rec HEAD-3 900))))))
  (check-equal? code 1 (format "stderr: ~a" err))
  (check-true (string-contains? stdout "fail"))
  (check-equal? (hash-ref verdict 'decision) "fail")
  (check-true (string-contains? md "quantile-budget: FAIL")))

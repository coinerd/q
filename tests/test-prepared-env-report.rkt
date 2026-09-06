#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration

;; W5 (current milestone): prepared-environment restore evidence report tooling.
;;
;; The tool `scripts/ci/prepared-env-report.rkt` is built tests-first by
;; this file. It must support three modes:
;;
;;   --emit-restore-record  (ci.yml wiring: one machine-readable restore
;;                           outcome per test shard, consumed by the report)
;;   --aggregate            (observation window -> committed report.json)
;;   --manifest <f> --check (durable, machine-checked gate; the wave verify
;;                           command plus [--write-checksums] for binding)
;;
;; Honesty rules under test: restore outcomes are exactly
;; `verified | rebuilt | fallback | unknown`; every non-verified restore
;; names its fallback cause (the literal "unknown" is the only honest
;; attribution gap, never invented); missing numeric data is the string
;; "unknown", never a fabricated zero; the committed report is bound to a
;; SHA256SUMS file; and the committed window report passes --check.

(require json
         racket/file
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         (only-in "../util/version.rkt" q-version))

(define-runtime-path project-root "..")
(define script-path (build-path project-root "scripts" "ci" "prepared-env-report.rkt"))
(define committed-dir
  (build-path project-root "artifacts" "ci-baseline" (format "v~a-prepared-env" q-version)))
(define committed-manifest (build-path committed-dir "report.json"))
(define committed-sums (build-path committed-dir "SHA256SUMS"))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (env-prefix envs)
  (if (null? envs)
      ""
      (string-append (string-join (for/list ([kv (in-list envs)])
                                    (format "~a='~a'" (car kv) (cdr kv)))
                                  " ")
                     " ")))

(define (run-script args [envs '()])
  ;; Run the report tool from the repo root with optional environment
  ;; assignments; return (values exit-code combined-output).
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory project-root]
                   [current-output-port out]
                   [current-error-port out])
      (system/exit-code
       (string-append (env-prefix envs) "racket " (path->string script-path) " " args))))
  (values code (get-output-string out)))

(define (script-succeeds args [envs '()])
  (define-values (code out) (run-script args envs))
  (unless (zero? code)
    (fail (format
           "expected exit 0 for: ~aracket scripts/ci/prepared-env-report.rkt ~a\n--- output ---\n~a"
           (env-prefix envs)
           args
           out)))
  (void))

(define (script-fails args [envs '()])
  (define-values (code out) (run-script args envs))
  (unless (positive? code)
    (fail
     (format
      "expected nonzero exit for: ~aracket scripts/ci/prepared-env-report.rkt ~a\n--- output ---\n~a"
      (env-prefix envs)
      args
      out)))
  (void))

(define tmp-root (make-temporary-file "q-w5-prepared-env-report~a" 'directory))

(define (tmp-path name)
  (build-path tmp-root name))

(define (write-jsexpr path jsexpr)
  (with-output-to-file path (lambda () (write-json jsexpr)) #:exists 'replace))

(define (read-jsexpr path)
  (with-input-from-file path read-json))

;; Build a well-formed report fixture (used to exercise --check rules).
(define (valid-report restores)
  (hasheq 'schema-version
          1
          'kind
          "prepared-env-restore-report"
          'restores
          restores
          'observation
          (hasheq 'counts
                  (hasheq 'verified 1 'rebuilt 0 'fallback 0 'unknown 0)
                  'rate-denominator
                  1
                  'verified-restore-rate
                  1.0
                  'gate
                  (hasheq 'threshold-percent 95 'verdict "pass" 'fallback-causes (hasheq)))
          'fresh-measurements
          (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))

(define (emit-command out extra)
  (string-append "--emit-restore-record --out "
                 (path->string out)
                 " --run-id 1001 --head-sha aaaa1111 --shard 0"
                 " --created-at-utc 2026-09-05T10:00:00Z"
                 (if (non-empty-string? extra)
                     (string-append " " extra)
                     "")))

;; ---------------------------------------------------------------------------
;; Suite
;; ---------------------------------------------------------------------------

(define (suite)
  (test-suite "test-prepared-env-report"

    (test-case "tool and committed evidence files exist"
      (check-true (file-exists? script-path) "scripts/ci/prepared-env-report.rkt must exist")
      (check-true (file-exists? committed-manifest)
                  (format "artifacts/ci-baseline/v~a-prepared-env/report.json must exist" q-version))
      (check-true (file-exists? committed-sums) "SHA256SUMS must sit next to the committed report"))

    (test-case "emit: verified restore (telemetry contract honored, cache key bound)"
      (define out (tmp-path "emit-verified.json"))
      (script-succeeds
       (emit-command
        out
        "--wall-clock-seconds 210.5 --fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_RESTORE_MS" . "12000")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome) "verified")
      (check-equal? (hash-ref rec 'restore-ms) 12000)
      (check-equal? (hash-ref rec 'cache-key) "prepared-env-fast:deadbeef")
      (check-equal? (hash-ref rec 'sha-context) "aaaa1111")
      (check-false (hash-has-key? rec 'fallback-cause)))

    (test-case "emit: no telemetry -> outcome unknown, durations unknown (never zero)"
      (define out (tmp-path "emit-local.json"))
      (script-succeeds
       (emit-command
        out
        "--fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '())
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome) "unknown")
      (check-equal? (hash-ref rec 'restore-ms) "unknown")
      (check-equal? (hash-ref rec 'fallback-ms) "unknown")
      (check-equal? (hash-ref rec 'wall-clock-seconds) "unknown")
      (check-equal? (hash-ref rec 'cache-key) "prepared-env-fast:deadbeef"))

    (test-case "emit: producer skipped -> fallback with named cause producer-skipped"
      (define out (tmp-path "emit-skipped.json"))
      (script-succeeds
       (emit-command
        out
        "--fast-env-producer-result skipped --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "unavailable")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome) "fallback")
      (check-equal? (hash-ref rec 'fallback-cause) "producer-skipped"))

    (test-case "emit: state rebuilt -> rebuilt with named cause and both durations"
      (define out (tmp-path "emit-rebuilt.json"))
      (script-succeeds
       (emit-command
        out
        "--wall-clock-seconds 280.0 --fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "rebuilt") ("Q_PREPARED_ENV_RESTORE_MS" . "8000")
                                              ("Q_PREPARED_ENV_FALLBACK_MS" . "340000")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome) "rebuilt")
      (check-equal? (hash-ref rec 'fallback-cause) "restore-mismatch-or-failure")
      (check-equal? (hash-ref rec 'restore-ms) 8000)
      (check-equal? (hash-ref rec 'fallback-ms) 340000))

    (test-case "aggregate: counts, rate, gate verdict, unknown handling, round-trip check"
      (define r-verified
        (write-jsexpr (tmp-path "agg-a.json")
                      (hasheq 'schema
                              "prepared-env-restore-record"
                              'run-id
                              1001
                              'head-sha
                              "aaaa"
                              'created-at-utc
                              "2026-09-05T10:00:00Z"
                              'shard
                              0
                              'prepared-env-mode
                              "auto"
                              'fast-env-producer-result
                              "success"
                              'state
                              "restored"
                              'result
                              "success"
                              'restore-ms
                              12000
                              'fallback-ms
                              "unknown"
                              'wall-clock-seconds
                              210.5
                              'prepared-artifact-name
                              "prepared-env-fast"
                              'installer-sha256
                              "d1")))
      (write-jsexpr (tmp-path "agg-b.json")
                    (hasheq 'schema
                            "prepared-env-restore-record"
                            'run-id
                            1002
                            'head-sha
                            "bbbb"
                            'created-at-utc
                            "2026-09-05T11:00:00Z"
                            'shard
                            1
                            'prepared-env-mode
                            "auto"
                            'fast-env-producer-result
                            "success"
                            'state
                            "rebuilt"
                            'result
                            "failure"
                            'restore-ms
                            9000
                            'fallback-ms
                            350000
                            'wall-clock-seconds
                            "unknown"
                            'prepared-artifact-name
                            "prepared-env-fast"
                            'installer-sha256
                            "d1"))
      (write-jsexpr (tmp-path "agg-c.json")
                    (hasheq 'schema
                            "prepared-env-restore-record"
                            'run-id
                            1003
                            'head-sha
                            "cccc"
                            'created-at-utc
                            "2026-09-05T12:00:00Z"
                            'shard
                            2
                            'prepared-env-mode
                            "off"
                            'fast-env-producer-result
                            "skipped"
                            'state
                            "unavailable"
                            'result
                            "success"
                            'restore-ms
                            "unknown"
                            'fallback-ms
                            "unknown"
                            'wall-clock-seconds
                            "unknown"
                            'prepared-artifact-name
                            "prepared-env-fast"
                            'installer-sha256
                            "d1"))
      (define report (tmp-path "agg-report.json"))
      (script-succeeds (string-append "--aggregate "
                                      (path->string tmp-root)
                                      " --filter-prefix agg- --out "
                                      (path->string report)))
      (define rep (read-jsexpr report))
      (define obs (hash-ref rep 'observation))
      (define counts (hash-ref obs 'counts))
      (check-equal? (hash-ref counts 'verified) 1)
      (check-equal? (hash-ref counts 'rebuilt) 1)
      (check-equal? (hash-ref counts 'fallback) 1)
      (check-equal? (hash-ref counts 'unknown) 0)
      (check-equal? (hash-ref obs 'rate-denominator) 3)
      (check-= (hash-ref obs 'verified-restore-rate) (/ 1 3) 1e-9)
      (check-equal? (hash-ref (hash-ref obs 'gate) 'verdict) "fallback-causes-named")
      (define causes (hash-ref (hash-ref obs 'gate) 'fallback-causes))
      (check-true (hash-has-key? causes 'restore-mismatch-or-failure))
      (check-true (hash-has-key? causes 'producer-skipped))
      ;; Missing wall clock stays "unknown" in the rebuilt record.
      (define recs (hash-ref rep 'restores))
      (define rebuilt-rec
        (for/first ([r (in-list recs)]
                    #:when (equal? (hash-ref r 'outcome) "rebuilt"))
          r))
      (check-equal? (hash-ref rebuilt-rec 'wall-clock-seconds) "unknown")
      ;; Fresh-measurement section: labeled fresh, one sample per run,
      ;; historical numbers referenced but never substituted.
      (define fresh (hash-ref rep 'fresh-measurements))
      (check-equal? (hash-ref fresh 'label) "fresh measurements")
      (check-equal? (length (hash-ref fresh 'samples)) 3)
      (check-true (hash-has-key? fresh 'historical-note))
      ;; Round-trip: the aggregate output itself passes --check.
      (script-succeeds (string-append "--manifest " (path->string report) " --check")))

    (test-case "round-trip: emit output feeds aggregate without semantic loss"
      ;; The ci.yml wiring emits records with `raw-state` + composed
      ;; `cache-key`; the aggregator must read those emitted records back
      ;; with identical semantics (outcome, durations, cache key).
      (define out-v (tmp-path "rt-verified.json"))
      (script-succeeds
       (emit-command
        out-v
        "--wall-clock-seconds 159.0 --fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 497f")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_RESTORE_MS" . "22000")))
      (define out-r (tmp-path "rt-rebuilt.json"))
      (script-succeeds
       (emit-command
        out-r
        "--fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 497f")
       '(("Q_PREPARED_ENV_STATE" . "rebuilt") ("Q_PREPARED_ENV_RESTORE_MS" . "8000")
                                              ("Q_PREPARED_ENV_FALLBACK_MS" . "340000")))
      (define report (tmp-path "rt-report.json"))
      (script-succeeds (string-append "--aggregate "
                                      (path->string tmp-root)
                                      " --filter-prefix rt- --out "
                                      (path->string report)))
      (define rep (read-jsexpr report))
      (define recs (hash-ref rep 'restores))
      (check-equal? (length recs) 2)
      (define verified-rec
        (for/first ([r (in-list recs)]
                    #:when (equal? (hash-ref r 'raw-state) "restored"))
          r))
      (define rebuilt-rec
        (for/first ([r (in-list recs)]
                    #:when (equal? (hash-ref r 'raw-state) "rebuilt"))
          r))
      (check-equal? (hash-ref verified-rec 'outcome) "verified")
      (check-equal? (hash-ref verified-rec 'restore-ms) 22000)
      (check-equal? (hash-ref verified-rec 'cache-key) "prepared-env-fast:497f")
      (check-equal? (hash-ref rebuilt-rec 'outcome) "rebuilt")
      (check-equal? (hash-ref rebuilt-rec 'fallback-cause) "restore-mismatch-or-failure")
      (define obs (hash-ref rep 'observation))
      (check-equal? (hash-ref obs 'rate-denominator) 2)
      (check-= (hash-ref obs 'verified-restore-rate) 0.5 1e-9))

    (test-case "aggregate: --basis override lands in window.basis (honest provenance)"
      (write-jsexpr (tmp-path "basis-a.json")
                    (hasheq 'schema
                            "prepared-env-restore-record"
                            'run-id
                            2001
                            'head-sha
                            "ffff"
                            'created-at-utc
                            "2026-09-06T00:35:00Z"
                            'shard
                            0
                            'prepared-env-mode
                            "auto"
                            'fast-env-producer-result
                            "success"
                            'state
                            "restored"
                            'restore-ms
                            16000
                            'wall-clock-seconds
                            249.0
                            'prepared-artifact-name
                            "prepared-env-fast"
                            'installer-sha256
                            "497f"))
      (define report (tmp-path "basis-report.json"))
      (script-succeeds
       (string-append "--aggregate "
                      (path->string tmp-root)
                      " --filter-prefix basis- --out "
                      (path->string report)
                      " --basis reconstructed from real CI run logs via the jobs API"))
      (define rep (read-jsexpr report))
      (check-equal? (hash-ref (hash-ref rep 'window) 'basis)
                    "reconstructed from real CI run logs via the jobs API"))

    (test-case "emit: --record-source override is preserved verbatim"
      (define out (tmp-path "emit-source.json"))
      (script-succeeds
       (emit-command
        out
        "--record-source backfilled-from-run-logs --fast-env-producer-result success \
         --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'record-source) "backfilled-from-run-logs"))

    (test-case "check: rejects raw CI vocabulary leaking into outcome"
      (define report (tmp-path "bad-vocab.json"))
      (write-jsexpr
       report
       (hasheq 'schema-version
               1
               'kind
               "prepared-env-restore-report"
               'restores
               (list (hasheq 'outcome "restored" 'restore-ms 1 'sha-context "x"))
               'observation
               (hasheq 'counts
                       (hasheq 'verified 1 'rebuilt 0 'fallback 0 'unknown 0)
                       'rate-denominator
                       1
                       'verified-restore-rate
                       1.0
                       'gate
                       (hasheq 'threshold-percent 95 'verdict "pass" 'fallback-causes (hasheq)))
               'fresh-measurements
               (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "check: rejects non-verified outcome without named fallback cause"
      (define report (tmp-path "bad-cause.json"))
      (write-jsexpr
       report
       (hasheq
        'schema-version
        1
        'kind
        "prepared-env-restore-report"
        'restores
        (list (hasheq 'outcome "rebuilt" 'fallback-cause "" 'restore-ms 5 'sha-context "x"))
        'observation
        (hasheq
         'counts
         (hasheq 'verified 0 'rebuilt 1 'fallback 0 'unknown 0)
         'rate-denominator
         1
         'verified-restore-rate
         0.0
         'gate
         (hasheq 'threshold-percent 95 'verdict "fallback-causes-named" 'fallback-causes (hasheq)))
        'fresh-measurements
        (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "check: rejects fabricated zero in place of unknown data"
      (define report (tmp-path "bad-zero.json"))
      (write-jsexpr
       report
       (hasheq
        'schema-version
        1
        'kind
        "prepared-env-restore-report"
        'restores
        (list (hasheq 'outcome "unknown" 'restore-ms 0 'sha-context "unknown"))
        'observation
        (hasheq 'counts
                (hasheq 'verified 0 'rebuilt 0 'fallback 0 'unknown 1)
                'rate-denominator
                0
                'verified-restore-rate
                "unknown"
                'gate
                (hasheq 'threshold-percent 95 'verdict "insufficient-data" 'fallback-causes (hasheq)))
        'fresh-measurements
        (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "check: rejects inconsistent verified-restore-rate arithmetic"
      (define report (tmp-path "bad-rate.json"))
      (write-jsexpr
       report
       (hasheq 'schema-version
               1
               'kind
               "prepared-env-restore-report"
               'restores
               (list (hasheq 'outcome "verified" 'restore-ms 5 'sha-context "x")
                     (hasheq 'outcome
                             "fallback"
                             'fallback-cause
                             "producer-skipped"
                             'restore-ms
                             "unknown"
                             'sha-context
                             "x"))
               'observation
               (hasheq 'counts
                       (hasheq 'verified 1 'rebuilt 0 'fallback 1 'unknown 0)
                       'rate-denominator
                       2
                       'verified-restore-rate
                       0.9
                       'gate
                       (hasheq 'threshold-percent
                               95
                               'verdict
                               "fallback-causes-named"
                               'fallback-causes
                               (hasheq 'producer-skipped 1)))
               'fresh-measurements
               (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "check: rejects verdict inconsistent with the gate arithmetic"
      (define report (tmp-path "bad-verdict.json"))
      (write-jsexpr
       report
       (hasheq
        'schema-version
        1
        'kind
        "prepared-env-restore-report"
        'restores
        (list (hasheq 'outcome "verified" 'restore-ms 5 'sha-context "x"))
        'observation
        (hasheq
         'counts
         (hasheq 'verified 1 'rebuilt 0 'fallback 0 'unknown 0)
         'rate-denominator
         1
         'verified-restore-rate
         1.0
         'gate
         (hasheq 'threshold-percent 95 'verdict "fallback-causes-named" 'fallback-causes (hasheq)))
        'fresh-measurements
        (hasheq 'label "fresh measurements" 'samples (list) 'historical-note "n")))
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "check: validates the SHA256SUMS binding"
      (define dir (build-path tmp-root "sums-binding"))
      (make-directory* dir)
      (define report (build-path dir "report.json"))
      (write-jsexpr report
                    (valid-report (list (hasheq 'outcome
                                                "verified"
                                                'restore-ms
                                                5
                                                'sha-context
                                                "x"
                                                'fallback-ms
                                                "unknown"
                                                'wall-clock-seconds
                                                "unknown"
                                                'cache-key
                                                "prepared-env-fast:x"))))
      (script-succeeds
       (string-append "--manifest " (path->string report) " --check --write-checksums"))
      (check-true (file-exists? (build-path dir "SHA256SUMS")))
      (with-output-to-file (build-path dir "SHA256SUMS")
                           (lambda ()
                             (display (string-append (make-string 64 #\0) "  report.json\n")))
                           #:exists 'replace)
      (script-fails (string-append "--manifest " (path->string report) " --check")))

    (test-case "committed window report passes --check with checksums intact"
      (script-succeeds (string-append "--manifest " (path->string committed-manifest) " --check")))))

(module+ main
  (define failed (run-tests (suite)))
  (exit (if (zero? failed) 0 1)))

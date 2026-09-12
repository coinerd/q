#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration

;; W5 (current milestone): prepared-environment restore evidence report tooling.
;;
;; The tool `scripts/ci/prepared-env-report.rkt` is built tests-first by
;; this file. It must support these modes:
;;
;;   --emit-restore-record  (ci.yml wiring: one machine-readable restore
;;                           outcome per test shard, consumed by the report)
;;   --aggregate            (observation window -> committed report.json)
;;   --manifest <f> --check (durable, machine-checked gate; the wave verify
;;                           command plus [--write-checksums] for binding)
;;
;; W6 (v1.00.29): the prepared-environment IDENTITY MANIFEST modes
;; (spec §6 W4):
;;
;;   --identity-emit / --identity-compare / --identity-fallback-record
;;       the immutable identity (OS image, arch, Racket version +
;;       executable digest, lock/resolved set, precompile recipe revision,
;;       policy knobs, artifact digest) is compared at restore; ANY
;;       mismatch is a loud, counted cold fallback — never silent
;;       acceptance — and distinct environments always derive distinct
;;       artifact identities (no cross-reuse)
;;   --consumers-check / --savings-check
;;       fail-closed honesty gates over the W6 consumer matrix and the
;;       setup-savings derivation (projected rows must carry formulas;
;;       measured rows must cite retained evidence)
;;   --rollback-drill
;;       the exercisable §11.6 one-command rollback decision per consumer
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
         rackunit/text-ui)

(define-runtime-path project-root "..")
(define script-path (build-path project-root "scripts" "ci" "prepared-env-report.rkt"))
(define committed-dir
  (build-path project-root
              "artifacts"
              "ci-baseline"
              "v1.00.26-prepared-env")) ;; frozen: prepared-env evidence is a v1.00.26-series artifact
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
;; W6 identity-manifest helpers (module level: test-case bodies are
;; expression contexts, so shared defines live here)
;; ---------------------------------------------------------------------------

(define digest-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define digest-b "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(define digest-c "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")
(define digest-d "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd")

(define (identity-args out
                       #:os [os "Linux"]
                       #:os-image [os-image "ubuntu-24.04:20260901.1"]
                       #:arch [arch "x64"]
                       #:racket-version [racket-version "8.10"]
                       #:exec-digest [exec digest-a]
                       #:lock-digest [lock digest-b]
                       #:resolved [resolved digest-c]
                       #:recipe [recipe "setup-racket-full-path-r1"]
                       #:policy [policy "addon-store-v2-no-workspace-bytecode"]
                       #:artifact-digest [artifact digest-d]
                       #:base [base "prepared-env-fast"])
  (format
   "--identity-emit --out ~a --os ~a --os-image ~a --arch ~a --racket-version ~a --racket-executable-digest ~a --lock-digest ~a --resolved-set-digest ~a --precompile-recipe-revision ~a --policy-fingerprint ~a --artifact-digest ~a --artifact-name-base ~a"
   (path->string out)
   os
   os-image
   arch
   racket-version
   exec
   lock
   resolved
   recipe
   policy
   artifact
   base))

(define (compare-and-verdict expected observed consumer)
  ;; Run the compare with a verdict file; return (values code verdict stdout).
  (define verdict (tmp-path (format "verdict-~a.json" (symbol->string (gensym 'v)))))
  (define args
    (format "--identity-compare --expected ~a --observed ~a --out ~a --consumer ~a"
            (path->string expected)
            (path->string observed)
            (path->string verdict)
            consumer))
  (define-values (code out) (run-script args))
  (values code (and (file-exists? verdict) (read-jsexpr verdict)) out))

;; ---------------------------------------------------------------------------
;; Suite
;; ---------------------------------------------------------------------------

(define (suite)
  (test-suite "test-prepared-env-report"

    (test-case "tool and committed evidence files exist"
      (check-true (file-exists? script-path) "scripts/ci/prepared-env-report.rkt must exist")
      (check-true (file-exists? committed-manifest)
                  "artifacts/ci-baseline/v1.00.26-prepared-env/report.json must exist")
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
      (script-succeeds (string-append "--manifest " (path->string committed-manifest) " --check")))

    ;; ---------------------------------------------------------------
    ;; W6: prepared-environment identity manifest (spec §6 W4)
    ;; (helpers digest-a..digest-d, identity-args and
    ;; compare-and-verdict are defined at module level, below)
    ;; ---------------------------------------------------------------

    (test-case "identity: emit is deterministic and nine-dimension bound"
      (define out (tmp-path "id-base.json"))
      (script-succeeds (identity-args out))
      (define manifest (read-jsexpr out))
      (check-equal? (hash-ref manifest 'schema) "prepared-env-identity@1")
      (define fields (hash-ref manifest 'fields))
      (for ([dim (in-list (list "os"
                                "os-image"
                                "arch"
                                "racket-version"
                                "racket-executable-digest"
                                "lock-digest"
                                "resolved-set-digest"
                                "precompile-recipe-revision"
                                "policy-fingerprint"))])
        (check-true (hash-has-key? fields (string->symbol dim))
                    (format "identity manifest must record ~a" dim)))
      (check-true (hash-has-key? fields 'artifact-digest)
                  "the artifact digest is a recorded identity dimension")
      ;; Byte-identical regeneration for identical inputs (ordered JSON).
      (define out-2 (tmp-path "id-base-2.json"))
      (script-succeeds (identity-args out-2))
      (check-equal? (file->string out)
                    (file->string out-2)
                    "identity emit must be byte-deterministic for identical inputs")
      ;; Well-formedness: a malformed digest is a hard usage failure —
      ;; never an invented identity.
      (define out-bad (tmp-path "id-bad.json"))
      (script-fails (identity-args out-bad #:exec-digest "deadbeef")))

    (test-case "identity: ANY dimension change yields a distinct artifact identity (no cross-reuse)"
      (define out-base (tmp-path "id-nx-base.json"))
      (script-succeeds (identity-args out-base))
      (define base-identity (hash-ref (read-jsexpr out-base) 'artifact-identity))
      ;; One mutation per profile dimension: nine distinct environments,
      ;; nine distinct artifact identities — cross-environment reuse of an
      ;; artifact name is structurally impossible.
      (define mutations
        (list
         (cons "os" (lambda (out) (identity-args out #:os "Darwin")))
         (cons "os-image" (lambda (out) (identity-args out #:os-image "ubuntu-24.04:20261001.1")))
         (cons "arch" (lambda (out) (identity-args out #:arch "arm64")))
         (cons "racket-version" (lambda (out) (identity-args out #:racket-version "8.11")))
         (cons "racket-executable-digest" (lambda (out) (identity-args out #:exec-digest digest-b)))
         (cons "lock-digest" (lambda (out) (identity-args out #:lock-digest digest-c)))
         (cons "resolved-set-digest" (lambda (out) (identity-args out #:resolved digest-d)))
         (cons "precompile-recipe-revision"
               (lambda (out) (identity-args out #:recipe "setup-racket-full-path-r2")))
         (cons "policy-fingerprint" (lambda (out) (identity-args out #:policy "strict-queue")))))
      (for ([mutation (in-list mutations)])
        (define name (car mutation))
        (define emit (cdr mutation))
        (define out-m (tmp-path (format "id-mut-~a.json" name)))
        (script-succeeds (emit out-m))
        (define identity-m (hash-ref (read-jsexpr out-m) 'artifact-identity))
        (check-not-equal? identity-m
                          base-identity
                          (format "mutating ~a must yield a distinct artifact identity" name))))

    (test-case "identity: identical manifests verify (no fallback required)"
      (define expected (tmp-path "id-ver-exp.json"))
      (define observed (tmp-path "id-ver-obs.json"))
      (script-succeeds (identity-args expected))
      (script-succeeds (identity-args observed))
      (define-values (code verdict stdout) (compare-and-verdict expected observed "ci:smoke"))
      (check-equal? code 0 "an identical identity must verify")
      (check-equal? (hash-ref verdict 'verdict) "verified")
      (check-equal? (hash-ref verdict 'mismatch-count) 0)
      (check-equal? (hash-ref (hash-ref verdict 'fallback) 'required) #f)
      (check-false (string-contains? stdout "::warning::")))

    (test-case "identity: wrong racket executable digest = loud counted cold fallback"
      (define expected (tmp-path "id-rk-exp.json"))
      (define observed (tmp-path "id-rk-obs.json"))
      (script-succeeds (identity-args expected))
      ;; The consumer's job-local runtime digest diverges from the required
      ;; tuple (e.g. a different runtime build under the same version).
      (script-succeeds (identity-args observed #:exec-digest digest-b))
      (define-values (code verdict stdout) (compare-and-verdict expected observed "ci:smoke"))
      (check-true (positive? code) "a racket-executable-digest mismatch must fail closed")
      (check-equal? (hash-ref verdict 'verdict) "mismatch")
      (check-equal? (hash-ref verdict 'mismatch-count) 1)
      (define mismatches (hash-ref verdict 'mismatches))
      (check-equal? (length mismatches) 1)
      (check-equal? (hash-ref (car mismatches) 'field) "racket-executable-digest")
      (check-equal? (hash-ref (car mismatches) 'expected) digest-a)
      (check-equal? (hash-ref (car mismatches) 'observed) digest-b)
      (define fallback (hash-ref verdict 'fallback))
      (check-equal? (hash-ref fallback 'required) #t)
      (check-equal? (hash-ref fallback 'loud) #t "the fallback must be loud")
      (check-equal? (hash-ref fallback 'counted) #t "the fallback must be counted")
      (check-true (string-contains? stdout "::warning::prepared-environment identity mismatch")
                  "the fallback must surface a GitHub ::warning")
      (check-true (string-contains? stdout "racket-executable-digest")
                  "the warning must name the mismatching dimension")
      (check-equal? (hash-ref fallback 'stamp) "prepared-env-identity-fallback-stamp.json"))

    (test-case "identity: wrong OS profile (os and os-image) = loud counted cold fallback"
      ;; (a) different OS entirely (cross-platform reuse attempt)
      (define expected-a (tmp-path "id-os-exp.json"))
      (define observed-a (tmp-path "id-os-obs.json"))
      (script-succeeds (identity-args expected-a))
      (script-succeeds (identity-args observed-a #:os "Darwin"))
      (define-values (code-a verdict-a stdout-a)
        (compare-and-verdict expected-a observed-a "ci:smoke"))
      (check-true (positive? code-a) "a cross-OS artifact must never be accepted")
      (check-equal? (hash-ref verdict-a 'verdict) "mismatch")
      (check-equal? (hash-ref (car (hash-ref verdict-a 'mismatches)) 'field) "os")
      (check-true (string-contains? stdout-a "::warning::prepared-environment identity mismatch"))
      (check-equal? (hash-ref (hash-ref verdict-a 'fallback) 'counted) #t)
      ;; (b) same OS family but a different runner image build
      (define expected-b (tmp-path "id-img-exp.json"))
      (define observed-b (tmp-path "id-img-obs.json"))
      (script-succeeds (identity-args expected-b))
      (script-succeeds (identity-args observed-b #:os-image "ubuntu-24.04:20261001.1"))
      (define-values (code-b verdict-b stdout-b)
        (compare-and-verdict expected-b observed-b "ci:smoke"))
      (check-true (positive? code-b) "an os-image change must never be silently accepted")
      (check-equal? (hash-ref (car (hash-ref verdict-b 'mismatches)) 'field) "os-image")
      (check-true (string-contains? stdout-b "::warning::prepared-environment identity mismatch")))

    (test-case "identity: wrong lock digest = loud counted cold fallback"
      (define expected (tmp-path "id-lock-exp.json"))
      (define observed (tmp-path "id-lock-obs.json"))
      (script-succeeds (identity-args expected))
      (script-succeeds (identity-args observed #:lock-digest digest-c))
      (define-values (code verdict stdout) (compare-and-verdict expected observed "ci:test"))
      (check-true (positive? code) "a lock mismatch must fail closed")
      (check-equal? (hash-ref (car (hash-ref verdict 'mismatches)) 'field) "lock-digest")
      (check-true (string-contains? stdout "::warning::prepared-environment identity mismatch"))
      (check-equal? (hash-ref (hash-ref verdict 'fallback) 'loud) #t))

    (test-case "identity: multi-dimension mismatch names every field"
      (define expected (tmp-path "id-multi-exp.json"))
      (define observed (tmp-path "id-multi-obs.json"))
      (script-succeeds (identity-args expected))
      (script-succeeds (identity-args observed #:exec-digest digest-b #:lock-digest digest-c))
      (define-values (code verdict stdout) (compare-and-verdict expected observed "ci:test"))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'mismatch-count) 2)
      (check-equal? (sort (for/list ([m (in-list (hash-ref verdict 'mismatches))])
                            (hash-ref m 'field))
                          string<?)
                    (list "lock-digest" "racket-executable-digest")))

    (test-case "identity: an incomplete observed manifest fails closed (never silently accepted)"
      (define expected (tmp-path "id-inc-exp.json"))
      (define observed (tmp-path "id-inc-obs.json"))
      (script-succeeds (identity-args expected))
      (script-succeeds (identity-args observed))
      ;; An observed manifest that drops a dimension is not comparable —
      ;; the gate must hard-fail, not skip the check.
      (define incomplete-doc (read-jsexpr observed))
      (define incomplete-fields (hash-ref incomplete-doc 'fields))
      (write-jsexpr observed
                    (hash-set incomplete-doc
                              'fields
                              (hash-remove incomplete-fields
                                           (string->symbol "precompile-recipe-revision"))))
      (define-values (code verdict stdout) (compare-and-verdict expected observed "ci:smoke"))
      (check-true (positive? code) "a manifest missing a dimension must fail closed")
      (check-false verdict "no verdict is written on a structural failure"))

    (test-case "identity: the counted fallback record names consumer and reason"
      (define rec (tmp-path "fallback-record.json"))
      (script-succeeds
       (format
        "--identity-fallback-record --out ~a --consumer ci:smoke --reason identity-mismatch-cold-fallback --created-at-utc 2026-09-14T00:00:00Z"
        (path->string rec)))
      (define data (read-jsexpr rec))
      (check-equal? (hash-ref data 'schema) "prepared-env-fallback-record@1")
      (check-equal? (hash-ref data 'consumer) "ci:smoke")
      (check-equal? (hash-ref data 'reason) "identity-mismatch-cold-fallback")
      (check-equal? (hash-ref data 'fallback) #t)
      (check-equal? (hash-ref data 'counted) #t)
      (check-equal? (hash-ref data 'loud) #t)
      ;; An unnamed fallback is rejected — honesty requires a named cause.
      (script-fails (format "--identity-fallback-record --out ~a --consumer ci:smoke"
                            (path->string rec))))

    (test-case "consumers-check: the committed W6 matrix passes its fail-closed gate"
      (define consumers-path
        (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "consumers.json"))
      (check-true (file-exists? consumers-path) "the W6 consumers.json must exist")
      (script-succeeds (string-append "--consumers-check " (path->string consumers-path))))

    (test-case "consumers-check: cross-environment activation is rejected (no cross-reuse)"
      ;; An activated consumer whose racket-version differs from the
      ;; producer's is a cross-Racket reuse attempt — the gate must reject it.
      (define doc
        (call-with-input-file
         (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "consumers.json")
         read-json))
      (define producer-identity (hash-ref (hash-ref doc 'producer) 'artifact-identity))
      (define producer-profile (hash-ref (hash-ref doc 'producer) 'env-profile))
      (define (fixture-row profile identity activation)
        (hasheq 'consumer
                "fixture-consumer"
                'workflow
                ".github/workflows/ci.yml"
                'env-profile
                profile
                'artifact-identity
                identity
                'activation
                activation
                'reason
                "fixture"
                'rollback
                "gh variable set X --body off"))
      (define (fixture row)
        (hasheq 'schema
                "prepared-env-consumers@1"
                'wave
                "v1.00.29-w6"
                'identity-dimensions
                (hash-ref doc 'identity-dimensions)
                'producer
                (hash-ref doc 'producer)
                'consumers
                (list row)
                'counts
                (hasheq 'activated 1 'deferred 0)))
      (define bad-path (tmp-path "consumers-cross.json"))
      (write-jsexpr
       bad-path
       (fixture (fixture-row (hasheq 'os "Linux" 'arch "x64" 'racket-version "8.11" 'policy "default")
                             producer-identity
                             "activated")))
      (script-fails (string-append "--consumers-check " (path->string bad-path)))
      ;; A consumer with a distinct profile that claims the producer's
      ;; artifact identity (even deferred) is rejected.
      (define bad-path-2 (tmp-path "consumers-stolen.json"))
      (write-jsexpr
       bad-path-2
       (fixture (fixture-row
                 (hasheq 'os "Darwin" 'arch "arm64" 'racket-version "8.10" 'policy "default")
                 producer-identity
                 "deferred")))
      (script-fails (string-append "--consumers-check " (path->string bad-path-2)))
      ;; Missing rollback command: the §11.6 contract requires one command.
      (define bad-row-3
        (hash-remove (fixture-row producer-profile producer-identity "activated") 'rollback))
      (define bad-path-3 (tmp-path "consumers-norollback.json"))
      (write-jsexpr bad-path-3 (fixture bad-row-3))
      (script-fails (string-append "--consumers-check " (path->string bad-path-3)))
      ;; A same-profile deferred consumer with the producer identity is fine.
      (define ok-path (tmp-path "consumers-ok.json"))
      (write-jsexpr ok-path
                    (hash-set (fixture (fixture-row producer-profile producer-identity "deferred"))
                              'counts
                              (hasheq 'activated 0 'deferred 1)))
      (script-succeeds (string-append "--consumers-check " (path->string ok-path))))

    (test-case "savings-check: the committed W6 savings file passes its honesty gate"
      (define savings-path
        (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "setup-savings.json"))
      (check-true (file-exists? savings-path) "the W6 setup-savings.json must exist")
      (script-succeeds (string-append "--savings-check " (path->string savings-path))))

    (test-case "savings-check: fabricated or formula-free rows are rejected"
      (define doc
        (call-with-input-file
         (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "setup-savings.json")
         read-json))
      (define consumers-path
        (path->string
         (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "consumers.json")))
      (define row-projected
        (for/first ([r (in-list (hash-ref doc 'consumers))]
                    #:when (equal? (hash-ref r 'measurement) "projected-from-w0-baseline"))
          r))
      (check-true (hash? row-projected) "the committed file must contain a projected row")
      ;; A projected row without a formula is rejected.
      (define bad-1 (tmp-path "savings-noformula.json"))
      (write-jsexpr bad-1
                    (hasheq 'schema
                            "prepared-env-setup-savings@1"
                            'baseline-source
                            "x"
                            'consumers-source
                            consumers-path
                            'consumers
                            (list (hash-remove row-projected 'formula))))
      (script-fails (string-append "--savings-check " (path->string bad-1)))
      ;; A measured row without evidence is rejected.
      (define row-measured
        (for/first ([r (in-list (hash-ref doc 'consumers))]
                    #:when (equal? (hash-ref r 'measurement) "measured"))
          r))
      (check-true (hash? row-measured) "the committed file must contain a measured row")
      (define bad-2 (tmp-path "savings-noevidence.json"))
      (write-jsexpr bad-2
                    (hasheq 'schema
                            "prepared-env-setup-savings@1"
                            'baseline-source
                            "x"
                            'consumers-source
                            consumers-path
                            'consumers
                            (list (hash-remove row-measured 'evidence))))
      (script-fails (string-append "--savings-check " (path->string bad-2)))
      ;; a measured claim without retained evidence is fabrication
      ;; A row naming a consumer that is not activated is drift.
      (define bad-3 (tmp-path "savings-wrongconsumer.json"))
      (write-jsexpr bad-3
                    (hasheq 'schema
                            "prepared-env-setup-savings@1"
                            'baseline-source
                            "x"
                            'consumers-source
                            consumers-path
                            'consumers
                            (list (hash-set row-measured 'consumer "ci:lint"))))
      (script-fails (string-append "--savings-check " (path->string bad-3))))

    (test-case "rollback drill: the §11.6 one-command switch flips exactly its consumer"
      (define consumers-path
        (path->string
         (build-path project-root "artifacts" "proof-graph" "v1.00.29-w6" "consumers.json")))
      (define (drill extra)
        (define-values (code out)
          (run-script
           (format "--rollback-drill --consumers ~a --consumer ci:smoke~a" consumers-path extra)))
        (values code (with-input-from-string out read-json)))
      ;; Default: the prepared path is in play for ci:smoke.
      (define-values (code-default decision-default) (drill ""))
      (check-equal? code-default 0)
      (check-equal? (hash-ref decision-default 'prepared-path-in-play) #t)
      (check-equal? (hash-ref decision-default 'effective-setup-path) "verified prepared-env restore")
      (check-equal? (hash-ref decision-default 'rollback-variable) "RACKET_PREPARED_SMOKE")
      ;; The one-command consumer rollback: its own variable = off pins the
      ;; legacy full path for this consumer only.
      (define-values (code-off decision-off) (drill " --vars RACKET_PREPARED_SMOKE=off"))
      (check-equal? code-off 0)
      (check-equal? (hash-ref decision-off 'prepared-path-in-play) #f)
      (check-equal? (hash-ref decision-off 'effective-setup-path) "legacy full cold setup")
      (check-equal? (hash-ref decision-off 'rollback-command)
                    "gh variable set RACKET_PREPARED_SMOKE --body off")
      ;; Other consumers stay on the prepared path while ci:smoke is off.
      (define-values (code-wf out-wf)
        (run-script
         (format
          "--rollback-drill --consumers ~a --consumer ci:workflows --vars RACKET_PREPARED_SMOKE=off"
          consumers-path)))
      (check-equal? code-wf 0)
      (check-equal? (hash-ref (with-input-from-string out-wf read-json) 'prepared-path-in-play) #t)
      ;; The global switch, a dispatch event and a skipped producer each
      ;; pin the legacy full path too.
      (for ([extra (in-list (list " --vars RACKET_PREPARED_ARTIFACT=off"
                                  " --event workflow_dispatch"
                                  " --producer-result skipped"))])
        (define-values (code-g decision-g) (drill extra))
        (check-equal? code-g 0)
        (check-equal? (hash-ref decision-g 'prepared-path-in-play)
                      #f
                      (format "~a must pin the legacy full path" extra))))))

(module+ main
  (define failed (run-tests (suite)))
  (exit (if (zero? failed) 0 1)))

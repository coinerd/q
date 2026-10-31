#lang racket

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration
;; @timeout 300 — measured 126s locally (2026-09-30); the suite spawns a
;; subprocess per fixture and outgrew the 120s default budget, producing
;; false TIMEOUT verdicts in the fast suite.

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
;; W6 (the current campaign): the prepared-environment IDENTITY MANIFEST modes
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
;;   --containment-check
;;       W2 eager-compilation containment gate over the committed
;;       containment document: the rollback switch pins exactly one
;;       eager setup boundary (no per-file compile fallback), the
;;       runtime/store restore outcome stays separate from the usable
;;       compiled-code outcome, all opting consumers honor the global
;;       switch, and containment is never advertised as recovery.
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

(require "../scripts/ci/prepared-env-report.rkt" ; W1 strict trial assessor: pure exported API under test
         json
         (only-in "../util/version.rkt" q-version)
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

(define ambient-scrub
  ;; the W9 CI incident: CI jobs export Q_PREPARED_ENV_* via
  ;; GITHUB_ENV for their own downstream steps; a test that means to
  ;; simulate ABSENT telemetry must scrub those ambient variables,
  ;; otherwise the child process inherits them and classifies from real CI
  ;; telemetry (the failure was masked-green in PR CI by the missing
  ;; pipefail in the shard steps — BUG-0073 — and invisible locally).
  (string-append "env -u Q_PREPARED_ENV_STATE -u Q_PREPARED_ENV_RESTORE_MS"
                 " -u Q_PREPARED_ENV_FALLBACK_MS -u Q_PREPARED_ENV_IDENTITY_RESULT "))

(define (run-script args [envs '()])
  ;; Run the report tool from the repo root with optional environment
  ;; assignments; return (values exit-code combined-output). The child
  ;; process environment starts from the ambient environment SCRUBBED of
  ;; the Q_PREPARED_ENV_* telemetry variables and then applies the
  ;; explicit per-case assignments — each test case sees exactly the
  ;; telemetry it declares, never ambient CI state.
  (define out (open-output-string))
  (define code
    (parameterize ([current-directory project-root]
                   [current-output-port out]
                   [current-error-port out])
      (system/exit-code (string-append ambient-scrub
                                       (env-prefix envs)
                                       "racket "
                                       (path->string script-path)
                                       " "
                                       args))))
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

;; A complete, honest W2 containment document (module level: test-case
;; bodies are expression contexts, so shared defines live here). The
;; negative tests below mutate one field at a time and expect the gate
;; to reject the result.
(define good-containment-doc
  (hasheq 'schema
          "prepared-env-containment@1"
          'rollback-switch
          "RACKET_PREPARED_ARTIFACT"
          'rollback-value
          "off"
          'switch-selects-full-path
          #t
          'eager-boundary
          "raco setup --no-docs --jobs 4 --pkgs q fmt"
          'eager-boundary-count
          1
          'per-file-compile-fallback
          #f
          'purge-invariant
          "BUG-0065 purge runs fail-closed on every path"
          'compiled-code-outcome-separation
          #t
          'containment-not-recovery
          #t
          'residual-gap
          "R1 is containment only; the pre-W4 233-292s job walls remain"
          'evidence
          "docs/reports/EAGER-COMPILE-CONTAINMENT-v1.00.30.md"
          'consumers
          (list (hasheq 'consumer "ci:test" 'honors-global-switch #t)
                (hasheq 'consumer "ci:smoke" 'honors-global-switch #t))))

;; ---------------------------------------------------------------------------
;; W1 (v1.00.32) strict trial assessment helpers — ci-recovery-trial-evidence@1
;;
;; The shared schema/CLI/API contract built tests-first here is documented
;; for the workflow and report owners in docs/reports/CI-RECOVERY-EVIDENCE.md.
;; Every fixture below is SYNTHETIC: it proves the assessor's refusal rules,
;; never a real CI outcome. Actual baseline collection remains a separately
;; authorized coordinator gate.
;; ---------------------------------------------------------------------------

(define sha40-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define sha40-b "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

(define (w1-identity-fields)
  ;; Field values deliberately match the `identity-args` defaults below so
  ;; the flag-supplied expected-identity test can reuse one coherent
  ;; synthetic environment across both fixture families.
  (hasheq 'os
          "Linux"
          'os-image
          "ubuntu-24.04:20260901.1"
          'arch
          "x64"
          'racket-version
          "8.10"
          'racket-executable-digest
          digest-a
          'lock-digest
          digest-b
          'resolved-set-digest
          digest-c
          'precompile-recipe-revision
          "setup-racket-full-path-r1"
          'policy-fingerprint
          "addon-store-v2-no-workspace-bytecode"
          'artifact-digest
          digest-d))

(define (w1-restore-record shard
                           #:head [head sha40-a]
                           #:run [run 7100]
                           #:outcome [outcome "verified"]
                           #:trial-id [trial-id "trial-2026-10-30-c1"]
                           #:attempt [attempt 1]
                           #:treatment [treatment "C"]
                           #:identity [identity (w1-identity-fields)])
  ;; The four strict binding fields (trial-id/attempt/treatment/identity)
  ;; are NOT emitted by --emit-restore-record: the workflow adapter
  ;; attaches them when it builds the envelope. `identity` is the shard's
  ;; ACTUAL observed environment identity (the ten fields of a
  ;; prepared-env-identity@1 manifest as measured on this shard).
  (hasheq 'schema
          "prepared-env-restore-record"
          'trial-id
          trial-id
          'attempt
          attempt
          'treatment
          treatment
          'identity
          identity
          'run-id
          run
          'head-sha
          head
          'created-at-utc
          "2026-10-30T10:00:00Z"
          'shard
          shard
          'prepared-env-mode
          "auto"
          'fast-env-producer-result
          "success"
          'raw-state
          "restored"
          'outcome
          outcome
          'restore-ms
          12000
          'fallback-ms
          "unknown"
          'wall-clock-seconds
          210.5
          'compiled-code-outcome
          "unknown"
          'eager-setup-seconds
          "unknown"
          'cache-state
          "unknown"
          'purge-zo-count
          "unknown"
          'compiled-root-outcome
          "unknown"
          'compiled-root-producer-seconds
          "unknown"
          'compiled-root-fallback-seconds
          "unknown"
          'cache-key
          "prepared-env-fast:deadbeef"
          'sha-context
          head))

(define (w1-launcher #:mode [mode "verified-root-hit"]
                     #:hits [hits 3]
                     #:fallbacks [fallbacks 0]
                     #:digest [digest digest-d]
                     #:exit-code [exit-code 0])
  ;; exit-code mirrors the actual launcher CLI, which ALWAYS emits it.
  (hasheq 'producer-manifest-digest
          digest
          'mode
          mode
          'root-hits
          hits
          'fallbacks
          fallbacks
          'exit-code
          exit-code
          'wall-ms
          90000
          'loads
          12
          'bytes-served
          1048576
          'mechanism
          "external-compiled-root(map-dir symlinks)"
          'raw-datum
          (string-append
           "(compiled-root-launch-telemetry (mode . \"verified-root-hit\") (exit-code . 0)"
           " (wall-ms . 90000) (root-hits . 3) (fallbacks . 0) (loads . 12)"
           " (bytes-served . 1048576) (mechanism . \"external-compiled-root(map-dir symlinks)\"))")))

(define (w1-launcher-off [digest digest-d] #:exit-code [exit-code 0])
  (w1-launcher #:mode "global-off" #:hits 0 #:fallbacks 0 #:digest digest #:exit-code exit-code))

(define (w1-shard shard
                  #:record [record #f]
                  #:launcher [launcher (w1-launcher)]
                  #:trial-id [trial-id "trial-2026-10-30-c1"]
                  #:attempt [attempt 1]
                  #:treatment [treatment "C"]
                  #:identity [identity (w1-identity-fields)])
  (define base
    (hasheq 'shard
            shard
            'record
            (or record
                (w1-restore-record shard
                                   #:trial-id trial-id
                                   #:attempt attempt
                                   #:treatment treatment
                                   #:identity identity))))
  (if launcher
      (hash-set base 'launcher launcher)
      base))

(define (w1-shards-off [treatment "A"])
  (list (w1-shard 0 #:treatment treatment #:launcher (w1-launcher-off))
        (w1-shard 1 #:treatment treatment #:launcher (w1-launcher-off))))

(define (w1-envelope #:treatment [treatment "C"]
                     #:head [head sha40-a]
                     #:run [run 7100]
                     #:inventory [inventory (list 0 1)]
                     #:shards [shards (list (w1-shard 0) (w1-shard 1))]
                     #:identity [identity (w1-identity-fields)]
                     #:manifest-digest [manifest-digest digest-d])
  (define expected
    (hash-set (if identity
                  (hasheq 'shard-inventory inventory 'identity identity)
                  (hasheq 'shard-inventory inventory))
              'producer-manifest-digest
              manifest-digest))
  (hasheq 'schema
          trial-evidence-schema
          'trial
          (hasheq 'trial-id
                  "trial-2026-10-30-c1"
                  'attempt
                  1
                  'treatment
                  treatment
                  'run-id
                  run
                  'head-sha
                  head
                  'created-at-utc
                  "2026-10-30T10:00:00Z")
          'expected
          expected
          'shards
          shards))

(define (write-envelope path env)
  (with-output-to-file path (lambda () (write-json env)) #:exists 'replace))

(define (trial-assess-verdict envelope-path [extra ""])
  ;; Run --trial-assess with a verdict file; return (values code verdict stdout).
  (define verdict (tmp-path (format "trial-verdict-~a.json" (symbol->string (gensym 't)))))
  (define-values (code out)
    (run-script (format "--trial-assess --envelope ~a --out ~a~a"
                        (path->string envelope-path)
                        (path->string verdict)
                        extra)))
  (values code (and (file-exists? verdict) (read-jsexpr verdict)) out))

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
   (string-append "--identity-emit --out ~a --os ~a --os-image ~a --arch ~a --racket-version ~a"
                  " --racket-executable-digest ~a --lock-digest ~a --resolved-set-digest ~a"
                  " --precompile-recipe-revision ~a --policy-fingerprint ~a"
                  " --artifact-digest ~a --artifact-name-base ~a")
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

    (test-case "emit: identity mismatch NEVER classifies as verified (W6 review finding)"
      (define out (tmp-path "emit-identity-mismatch.json"))
      (script-succeeds
       (emit-command
        out
        "--wall-clock-seconds 210.5 --fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_RESTORE_MS" . "12000")
                                               ("Q_PREPARED_ENV_IDENTITY_RESULT" . "mismatch")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome)
                    "rebuilt"
                    "an identity-mismatch cold fallback is rebuilt, never verified")
      (check-equal? (hash-ref rec 'fallback-cause) "identity-mismatch")
      (check-not-equal? (hash-ref rec 'outcome) "verified"))

    (test-case "emit: identity verified keeps verified classification"
      (define out (tmp-path "emit-identity-verified.json"))
      (script-succeeds
       (emit-command
        out
        "--wall-clock-seconds 210.5 --fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_IDENTITY_RESULT" . "verified")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'outcome) "verified")
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

    ;; The prepared-environment consumer matrix and setup-savings file are FROZEN
    ;; v1.00.29-campaign artifacts (that campaign's W6 produced them). Their
    ;; directory was derived from q-version, which only held while the canonical
    ;; version was 1.00.29; it now names the current milestone's own W6
    ;; directory, which has neither file. Pinned literally, reason recorded.
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
                      (format "~a must pin the legacy full path" extra))))

    ;; -----------------------------------------------------------------
    ;; W2 eager-compilation containment: the runtime/store restore
    ;; outcome is separate from the usable compiled-code outcome, every
    ;; W2 field is verbatim observed telemetry (zero is a real
    ;; observation, missing is "unknown"), and the committed containment
    ;; document passes a fail-closed honesty gate.
    ;; -----------------------------------------------------------------

    (test-case "emit: W2 eager telemetry — zero purge count is a real observation, never unknown"
      (define out (tmp-path "emit-w2-eager.json"))
      (script-succeeds
       (emit-command out
                     (string-append
                      "--fast-env-producer-result skipped --prepared-artifact-name prepared-env-fast"
                      " --installer-sha256 deadbeef --compiled-code-outcome eager-full-path"
                      " --eager-setup-seconds 273.5 --cache-state cold-build --purge-zo-count 0"))
       '(("Q_PREPARED_ENV_STATE" . "unavailable")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'compiled-code-outcome) "eager-full-path")
      (check-equal? (hash-ref rec 'eager-setup-seconds) 273.5)
      (check-equal? (hash-ref rec 'cache-state) "cold-build")
      (check-equal?
       (hash-ref rec 'purge-zo-count)
       0
       "purging zero stale .zo files is a real observation; only missing data is unknown"))

    (test-case "emit: absent W2 telemetry -> unknown, never fabricated"
      (define out (tmp-path "emit-w2-absent.json"))
      (script-succeeds
       (emit-command
        out
        "--fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_RESTORE_MS" . "12000")))
      (define rec (read-jsexpr out))
      (check-equal? (hash-ref rec 'compiled-code-outcome) "unknown")
      (check-equal? (hash-ref rec 'eager-setup-seconds) "unknown")
      (check-equal? (hash-ref rec 'cache-state) "unknown")
      (check-equal? (hash-ref rec 'purge-zo-count) "unknown"))

    (test-case "aggregate: W2 eager telemetry carried through the window (absent stays unknown)"
      (write-jsexpr (tmp-path "w2a.json")
                    (hasheq 'schema
                            "prepared-env-restore-record"
                            'run-id
                            2001
                            'head-sha
                            "a2a2"
                            'created-at-utc
                            "2026-09-06T10:00:00Z"
                            'shard
                            0
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
                            300.0
                            'prepared-artifact-name
                            "prepared-env-fast"
                            'installer-sha256
                            "d2"
                            'compiled-code-outcome
                            "eager-full-path"
                            'eager-setup-seconds
                            273.5
                            'cache-state
                            "cold-build"
                            'purge-zo-count
                            12))
      (write-jsexpr (tmp-path "w2b.json")
                    (hasheq 'schema
                            "prepared-env-restore-record"
                            'run-id
                            2002
                            'head-sha
                            "b2b2"
                            'created-at-utc
                            "2026-09-06T11:00:00Z"
                            'shard
                            1
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
                            "d2"))
      (define report (tmp-path "w2-report.json"))
      (script-succeeds (string-append "--aggregate "
                                      (path->string tmp-root)
                                      " --filter-prefix w2 --out "
                                      (path->string report)))
      (define rep (read-jsexpr report))
      (define recs (hash-ref rep 'restores))
      (define eager-rec
        (for/first ([r (in-list recs)]
                    #:when (equal? (hash-ref r 'run-id) 2001))
          r))
      (check-equal? (hash-ref eager-rec 'compiled-code-outcome) "eager-full-path")
      (check-equal? (hash-ref eager-rec 'eager-setup-seconds) 273.5)
      (check-equal? (hash-ref eager-rec 'cache-state) "cold-build")
      (check-equal? (hash-ref eager-rec 'purge-zo-count) 12)
      (define bare-rec
        (for/first ([r (in-list recs)]
                    #:when (equal? (hash-ref r 'run-id) 2002))
          r))
      (check-equal? (hash-ref bare-rec 'compiled-code-outcome) "unknown")
      (check-equal? (hash-ref bare-rec 'eager-setup-seconds) "unknown")
      (check-equal? (hash-ref bare-rec 'purge-zo-count) "unknown"))

    (test-case "containment-check: the committed W2 containment evidence passes its honesty gate"
      (script-succeeds "--containment-check artifacts/ci-recovery/v1.00.30-w2/containment.json"))

    (test-case "containment-check: a complete containment document passes"
      (define p (tmp-path "good-containment.json"))
      (write-jsexpr p good-containment-doc)
      (script-succeeds (string-append "--containment-check " (path->string p))))

    (test-case "containment-check: violations are rejected fail-closed"
      (define (rejects doc name)
        (define p (tmp-path name))
        (write-jsexpr p doc)
        (script-fails (string-append "--containment-check " (path->string p))))
      ;; Containment must never be advertised as recovery.
      (rejects (hash-set good-containment-doc 'containment-not-recovery #f) "bad-not-recovery.json")
      ;; No per-file compile fallback may exist next to the boundary.
      (rejects (hash-set good-containment-doc 'per-file-compile-fallback #t)
               "bad-per-file-fallback.json")
      ;; Exactly one eager setup boundary per job.
      (rejects (hash-set good-containment-doc 'eager-boundary-count 2) "bad-two-boundaries.json")
      ;; The switch must be named and must select the full path.
      (rejects (hash-set good-containment-doc 'rollback-switch "SOMETHING_ELSE") "bad-switch.json")
      (rejects (hash-set good-containment-doc 'switch-selects-full-path #f)
               "bad-switch-full-path.json")
      ;; The runtime/store vs usable-compiled-code separation is mandatory.
      (rejects (hash-set good-containment-doc 'compiled-code-outcome-separation #f)
               "bad-separation.json")
      ;; Every opting consumer must be audited against the switch.
      (rejects (hash-set good-containment-doc 'consumers (list)) "bad-no-consumers.json")
      (rejects (hash-set good-containment-doc
                         'consumers
                         (list (hasheq 'consumer "ci:test" 'honors-global-switch #f)))
               "bad-consumer-switch.json")
      ;; The residual gap and retained evidence must be named honestly.
      (rejects (hash-set good-containment-doc 'residual-gap "") "bad-empty-gap.json")
      (rejects (hash-set good-containment-doc 'evidence "") "bad-empty-evidence.json")
      ;; Not a JSON object at all.
      (rejects (list 1 2 3) "bad-not-object.json"))

    ;; ---------------------------------------------------------------
    ;; W1 (v1.00.32): strict trial assessment (--trial-assess and the
    ;; exported pure function assess-trial-envelope). Shared contract
    ;; for the workflow and report owners; see
    ;; docs/reports/CI-RECOVERY-EVIDENCE.md. Fixtures are synthetic.
    ;; ---------------------------------------------------------------

    (test-case "trial-assess: complete C trial with ACTUAL launcher telemetry is ready-root-on"
      (define p (tmp-path "trial-c.json"))
      (write-envelope p (w1-envelope))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-equal? code 0)
      (check-true (hash? verdict) "a ready verdict must be written")
      (check-equal? (hash-ref verdict 'schema) "ci-recovery-trial-assessment@1")
      (check-equal? (hash-ref verdict 'ready?) #t)
      (check-equal? (hash-ref verdict 'classification) "ready-root-on")
      (check-equal? (hash-ref verdict 'reasons) (list))
      (check-equal? (hash-ref verdict 'treatment) "C")
      (check-equal? (hash-ref verdict 'shards-assessed) 2)
      (check-equal? (hash-ref verdict 'expected-shard-count) 2)
      ;; Raw data is retained in the verdict for the report owners.
      (define raw (hash-ref verdict 'raw))
      (check-equal? (length (hash-ref raw 'shards)) 2)
      (check-equal? (hash-ref (hash-ref raw 'trial) 'treatment) "C"))

    (test-case "trial-assess: A/B root-off trials classify honestly as ready-root-off (global-off is legitimate)"
      (for ([treatment (in-list (list "A" "B"))])
        (define p (tmp-path (format "trial-~a.json" treatment)))
        (write-envelope p (w1-envelope #:treatment treatment #:shards (w1-shards-off treatment)))
        (define-values (code verdict out) (trial-assess-verdict p))
        (check-equal? code
                      0
                      (format "treatment ~a with all-global-off telemetry must be ready" treatment))
        (check-equal? (hash-ref verdict 'ready?) #t)
        (check-equal? (hash-ref verdict 'classification)
                      "ready-root-off"
                      "root-off is a distinct honest classification, never a failed root-on trial")
        (check-equal? (hash-ref verdict 'reasons) (list))))

    (test-case "trial-assess: missing shard record is a refusal, never ready"
      (define p (tmp-path "trial-missing.json"))
      (write-envelope p (w1-envelope #:shards (list (w1-shard 0))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'ready?) #f)
      (check-equal? (hash-ref verdict 'classification) "refused")
      (check-equal? (hash-ref verdict 'reasons) (list "missing-shard-record")))

    (test-case "trial-assess: duplicate shard record is refused"
      (define p (tmp-path "trial-dup.json"))
      (write-envelope p (w1-envelope #:shards (list (w1-shard 0) (w1-shard 0) (w1-shard 1))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "duplicate-shard-record")))

    (test-case "trial-assess: shard outside the expected inventory is refused"
      (define p (tmp-path "trial-extra.json"))
      (write-envelope p (w1-envelope #:shards (list (w1-shard 0) (w1-shard 1) (w1-shard 2))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "unexpected-shard-record")))

    (test-case "trial-assess: mixed head/run/shard records are refused (exact identity binding)"
      ;; mixed head
      (define p-head (tmp-path "trial-mixed-head.json"))
      (write-envelope p-head
                      (w1-envelope #:shards
                                   (list (w1-shard 0)
                                         (w1-shard 1 #:record (w1-restore-record 1 #:head sha40-b)))))
      (define-values (code-head verdict-head out-head) (trial-assess-verdict p-head))
      (check-true (positive? code-head))
      (check-equal? (hash-ref verdict-head 'reasons) (list "record-head-sha-mismatch"))
      ;; mixed run
      (define p-run (tmp-path "trial-mixed-run.json"))
      (write-envelope p-run
                      (w1-envelope #:shards
                                   (list (w1-shard 0)
                                         (w1-shard 1 #:record (w1-restore-record 1 #:run 7199)))))
      (define-values (code-run verdict-run out-run) (trial-assess-verdict p-run))
      (check-true (positive? code-run))
      (check-equal? (hash-ref verdict-run 'reasons) (list "record-run-id-mismatch"))
      ;; record/shard key mismatch
      (define p-shard (tmp-path "trial-mixed-shard.json"))
      (write-envelope p-shard
                      (w1-envelope #:shards (list (w1-shard 0 #:record (w1-restore-record 5))
                                                  (w1-shard 1))))
      (define-values (code-shard verdict-shard out-shard) (trial-assess-verdict p-shard))
      (check-true (positive? code-shard))
      (check-equal? (hash-ref verdict-shard 'reasons) (list "record-shard-mismatch")))

    (test-case "trial-assess: records must carry strict attempt/trial-id/treatment/identity binding"
      ;; mixed attempt: a record from another attempt must not certify this one
      (define p-attempt (tmp-path "trial-mixed-attempt.json"))
      (write-envelope p-attempt
                      (w1-envelope #:shards
                                   (list (w1-shard 0)
                                         (w1-shard 1 #:record (w1-restore-record 1 #:attempt 2)))))
      (define-values (code-attempt verdict-attempt out-attempt) (trial-assess-verdict p-attempt))
      (check-true (positive? code-attempt))
      (check-equal? (hash-ref verdict-attempt 'reasons) (list "record-attempt-mismatch"))
      ;; a record MISSING the adapter-attached key refuses too (required key)
      (define p-noattempt (tmp-path "trial-missing-attempt.json"))
      (write-envelope
       p-noattempt
       (w1-envelope #:shards (list (w1-shard 0 #:record (hash-remove (w1-restore-record 0) 'attempt))
                                   (w1-shard 1))))
      (define-values (code-noattempt verdict-noattempt out-noattempt)
        (trial-assess-verdict p-noattempt))
      (check-true (positive? code-noattempt))
      (check-equal? (hash-ref verdict-noattempt 'reasons) (list "record-attempt-mismatch"))
      ;; mixed trial-id
      (define p-trialid (tmp-path "trial-mixed-trialid.json"))
      (write-envelope
       p-trialid
       (w1-envelope
        #:shards (list (w1-shard 0)
                       (w1-shard 1 #:record (w1-restore-record 1 #:trial-id "trial-2026-10-30-c2")))))
      (define-values (code-trialid verdict-trialid out-trialid) (trial-assess-verdict p-trialid))
      (check-true (positive? code-trialid))
      (check-equal? (hash-ref verdict-trialid 'reasons) (list "record-trial-id-mismatch"))
      ;; a record MISSING the trial-id key refuses too
      (define p-notrialid (tmp-path "trial-missing-trialid.json"))
      (write-envelope
       p-notrialid
       (w1-envelope #:shards (list (w1-shard 0 #:record (hash-remove (w1-restore-record 0) 'trial-id))
                                   (w1-shard 1))))
      (define-values (code-notrialid verdict-notrialid out-notrialid)
        (trial-assess-verdict p-notrialid))
      (check-true (positive? code-notrialid))
      (check-equal? (hash-ref verdict-notrialid 'reasons) (list "record-trial-id-mismatch"))
      ;; mixed treatment: a record from another treatment must not certify C
      (define p-treatment (tmp-path "trial-mixed-treatment.json"))
      (write-envelope
       p-treatment
       (w1-envelope #:shards (list (w1-shard 0)
                                   (w1-shard 1 #:record (w1-restore-record 1 #:treatment "A")))))
      (define-values (code-treatment verdict-treatment out-treatment)
        (trial-assess-verdict p-treatment))
      (check-true (positive? code-treatment))
      (check-equal? (hash-ref verdict-treatment 'reasons) (list "record-treatment-mismatch"))
      ;; a record MISSING the treatment key refuses too
      (define p-notreatment (tmp-path "trial-missing-treatment.json"))
      (write-envelope
       p-notreatment
       (w1-envelope #:shards
                    (list (w1-shard 0 #:record (hash-remove (w1-restore-record 0) 'treatment))
                          (w1-shard 1))))
      (define-values (code-notreatment verdict-notreatment out-notreatment)
        (trial-assess-verdict p-notreatment))
      (check-true (positive? code-notreatment))
      (check-equal? (hash-ref verdict-notreatment 'reasons) (list "record-treatment-mismatch")))

    (test-case "trial-assess: observed record identity must equal the expected identity field-by-field"
      ;; Valid-looking mismatches: well-formed 64-hex digests and real
      ;; platform words that simply belong to ANOTHER environment.
      (for ([case (in-list (list (cons "os" "Darwin")
                                 (cons "os-image" "ubuntu-24.04:20260101.1")
                                 (cons "arch" "arm64")
                                 (cons "racket-version" "8.11")
                                 (cons "racket-executable-digest" digest-b)
                                 (cons "lock-digest" digest-c)
                                 (cons "resolved-set-digest" digest-d)
                                 (cons "precompile-recipe-revision" "setup-racket-full-path-r2")
                                 (cons "policy-fingerprint" "strict-queue")
                                 (cons "artifact-digest" digest-a)))])
        (define dim (car case))
        (define p (tmp-path (format "trial-ident-~a.json" dim)))
        (write-envelope p
                        (w1-envelope #:shards (list (w1-shard 0)
                                                    (w1-shard 1
                                                              #:record (w1-restore-record
                                                                        1
                                                                        #:identity
                                                                        (hash-set (w1-identity-fields)
                                                                                  (string->symbol dim)
                                                                                  (cdr case)))))))
        (define-values (code verdict out) (trial-assess-verdict p))
        (check-true (positive? code) (format "an observed ~a mismatch must refuse" dim))
        (check-equal? (hash-ref verdict 'reasons)
                      (list (format "record-identity-mismatch-~a" dim))
                      (format "the refusal must name the mismatching identity field: ~a" dim)))
      ;; the identity key is REQUIRED: a record without it refuses
      (define p-missing (tmp-path "trial-ident-missing.json"))
      (write-envelope
       p-missing
       (w1-envelope #:shards (list (w1-shard 0 #:record (hash-remove (w1-restore-record 0) 'identity))
                                   (w1-shard 1))))
      (define-values (code-missing verdict-missing out-missing) (trial-assess-verdict p-missing))
      (check-true (positive? code-missing))
      (check-equal? (hash-ref verdict-missing 'reasons) (list "record-identity-missing"))
      ;; a malformed observed identity (garbage where a 64-hex digest
      ;; belongs) fails closed instead of comparing garbage
      (define p-malformed (tmp-path "trial-ident-malformed.json"))
      (write-envelope p-malformed
                      (w1-envelope #:shards
                                   (list (w1-shard 0
                                                   #:record (w1-restore-record
                                                             0
                                                             #:identity (hash-set (w1-identity-fields)
                                                                                  'lock-digest
                                                                                  "deadbeef")))
                                         (w1-shard 1))))
      (define-values (code-malformed verdict-malformed out-malformed)
        (trial-assess-verdict p-malformed))
      (check-true (positive? code-malformed))
      (check-equal? (hash-ref verdict-malformed 'reasons) (list "record-identity-malformed")))

    (test-case "trial-assess: ANY outcome outside the valid enum refuses, not only the literal unknown"
      (for ([bad (in-list (list "restored" "success" "failure" "unknown"))])
        (define expected-reason
          (if (equal? bad "unknown") "record-outcome-unknown" "record-outcome-invalid"))
        (define p (tmp-path (format "trial-outcome-~a.json" bad)))
        (write-envelope p
                        (w1-envelope #:shards
                                     (list (w1-shard 0 #:record (w1-restore-record 0 #:outcome bad))
                                           (w1-shard 1))))
        (define-values (code verdict out) (trial-assess-verdict p))
        (check-true (positive? code) (format "raw-CI outcome ~s must never certify" bad))
        (check-equal? (hash-ref verdict 'reasons)
                      (list expected-reason)
                      (format "outcome ~s must name its refusal reason" bad))))

    (test-case "trial-assess: launcher exit-code is required and must be zero (failed launcher never certifies)"
      ;; a FAILED launcher refuses even in verified-root-hit mode
      (define p-failed-hit (tmp-path "trial-exitcode-failed-hit.json"))
      (write-envelope
       p-failed-hit
       (w1-envelope #:shards (list (w1-shard 0) (w1-shard 1 #:launcher (w1-launcher #:exit-code 3)))))
      (define-values (code-failed verdict-failed out-failed) (trial-assess-verdict p-failed-hit))
      (check-true (positive? code-failed))
      (check-equal? (hash-ref verdict-failed 'reasons) (list "launcher-exit-code-nonzero"))
      ;; the actual launcher CLI ALWAYS emits exit-code: telemetry without
      ;; it is malformed and fails closed
      (define p-absent (tmp-path "trial-exitcode-absent.json"))
      (write-envelope
       p-absent
       (w1-envelope #:shards (list (w1-shard 0)
                                   (w1-shard 1 #:launcher (hash-remove (w1-launcher) 'exit-code)))))
      (define-values (code-absent verdict-absent out-absent) (trial-assess-verdict p-absent))
      (check-true (positive? code-absent))
      (check-equal? (hash-ref verdict-absent 'reasons) (list "launcher-exit-code-invalid"))
      ;; a failed launcher refuses under root-off treatments too
      (define p-failed-off (tmp-path "trial-exitcode-failed-off.json"))
      (write-envelope
       p-failed-off
       (w1-envelope #:treatment "A"
                    #:shards (list (w1-shard 0
                                             #:treatment "A"
                                             #:launcher (w1-launcher-off digest-d #:exit-code 1))
                                   (w1-shard 1 #:treatment "A" #:launcher (w1-launcher-off)))))
      (define-values (code-off verdict-off out-off) (trial-assess-verdict p-failed-off))
      (check-true (positive? code-off))
      (check-equal? (hash-ref verdict-off 'reasons) (list "launcher-exit-code-nonzero")))

    (test-case "trial-assess: bad identities are refused fail-closed"
      ;; malformed head sha
      (define p-head (tmp-path "trial-bad-head.json"))
      (write-envelope p-head (w1-envelope #:head "zz-not-a-sha"))
      (define-values (code-head verdict-head out-head) (trial-assess-verdict p-head))
      (check-true (positive? code-head))
      (check-equal? (hash-ref verdict-head 'reasons) (list "trial-head-sha-malformed"))
      ;; malformed producer manifest digest
      (define p-digest (tmp-path "trial-bad-digest.json"))
      (write-envelope p-digest (w1-envelope #:manifest-digest "deadbeef"))
      (define-values (code-digest verdict-digest out-digest) (trial-assess-verdict p-digest))
      (check-true (positive? code-digest))
      (check-equal? (hash-ref verdict-digest 'reasons)
                    (list "expected-producer-manifest-digest-malformed"))
      ;; expected identity missing a dimension (lock digest)
      (define p-identity (tmp-path "trial-bad-identity.json"))
      (write-envelope p-identity
                      (w1-envelope #:identity (hash-remove (w1-identity-fields) 'lock-digest)))
      (define-values (code-identity verdict-identity out-identity) (trial-assess-verdict p-identity))
      (check-true (positive? code-identity))
      (check-equal? (hash-ref verdict-identity 'reasons)
                    (list "expected-identity-invalid-lock-digest"))
      ;; expected identity with a malformed digest dimension
      (define p-identity-2 (tmp-path "trial-bad-identity-2.json"))
      (write-envelope
       p-identity-2
       (w1-envelope #:identity (hash-set (w1-identity-fields) 'racket-executable-digest "deadbeef")))
      (define-values (code-identity-2 verdict-identity-2 out-identity-2)
        (trial-assess-verdict p-identity-2))
      (check-true (positive? code-identity-2))
      (check-equal? (hash-ref verdict-identity-2 'reasons)
                    (list "expected-identity-invalid-racket-executable-digest"))
      ;; expected identity entirely absent
      (define p-identity-3 (tmp-path "trial-no-identity.json"))
      (write-envelope p-identity-3 (w1-envelope #:identity #f))
      (define-values (code-identity-3 verdict-identity-3 out-identity-3)
        (trial-assess-verdict p-identity-3))
      (check-true (positive? code-identity-3))
      (check-equal? (hash-ref verdict-identity-3 'reasons) (list "expected-identity-missing")))

    (test-case "trial-assess: C needs ACTUAL launcher telemetry — resolution-only claims never certify"
      ;; no launcher telemetry at all: a resolution/resolve-step success
      ;; without observed root use is not activation evidence
      (define p (tmp-path "trial-resolution-only.json"))
      (write-envelope p (w1-envelope #:shards (list (w1-shard 0 #:launcher #f) (w1-shard 1))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'classification) "refused")
      (check-equal? (hash-ref verdict 'reasons) (list "missing-launcher-telemetry"))
      ;; self-attested expected context inside a shard is refused: the
      ;; expected root identity is caller/producer-bound, never shard-declared
      (define p-self (tmp-path "trial-self-attested.json"))
      (write-envelope p-self
                      (w1-envelope #:shards (list (w1-shard 0
                                                            #:record (hash-set (w1-restore-record 0)
                                                                               'expected-identity
                                                                               (w1-identity-fields)))
                                                  (w1-shard 1))))
      (define-values (code-self verdict-self out-self) (trial-assess-verdict p-self))
      (check-true (positive? code-self))
      (check-equal? (hash-ref verdict-self 'reasons)
                    (list "self-attested-expected-context-in-record")))

    (test-case "trial-assess: C with fallback / zero-hit / non-hit launcher mode is refused"
      ;; fallback contamination: a bounded eager fallback ran
      (define p-fb (tmp-path "trial-c-fallback.json"))
      (write-envelope
       p-fb
       (w1-envelope #:shards (list (w1-shard 0) (w1-shard 1 #:launcher (w1-launcher #:fallbacks 1)))))
      (define-values (code-fb verdict-fb out-fb) (trial-assess-verdict p-fb))
      (check-true (positive? code-fb))
      (check-equal? (hash-ref verdict-fb 'reasons) (list "launcher-fallbacks-nonzero"))
      ;; resolution ran but zero loads were served from the root
      (define p-hits (tmp-path "trial-c-zero-hits.json"))
      (write-envelope p-hits
                      (w1-envelope #:shards (list (w1-shard 0)
                                                  (w1-shard 1 #:launcher (w1-launcher #:hits 0)))))
      (define-values (code-hits verdict-hits out-hits) (trial-assess-verdict p-hits))
      (check-true (positive? code-hits))
      (check-equal? (hash-ref verdict-hits 'reasons) (list "zero-root-hits"))
      ;; launcher ran with the root globally off: not a verified root hit
      (define p-mode (tmp-path "trial-c-global-off.json"))
      (write-envelope
       p-mode
       (w1-envelope #:shards
                    (list (w1-shard 0)
                          (w1-shard 1 #:launcher (w1-launcher #:mode "global-off" #:hits 0)))))
      (define-values (code-mode verdict-mode out-mode) (trial-assess-verdict p-mode))
      (check-true (positive? code-mode))
      (check-equal? (hash-ref verdict-mode 'reasons) (list "launcher-mode-global-off")))

    (test-case "trial-assess: root activity inside a root-off treatment is refused"
      (define p (tmp-path "trial-a-root-activity.json"))
      (write-envelope p
                      (w1-envelope #:treatment "A"
                                   #:shards
                                   (list (w1-shard 0 #:treatment "A" #:launcher (w1-launcher-off))
                                         (w1-shard 1 #:treatment "A"))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "launcher-mode-not-global-off")))

    (test-case "trial-assess: launcher manifest digest must equal the producer-bound expected digest"
      (define p (tmp-path "trial-digest-mismatch.json"))
      (write-envelope p
                      (w1-envelope #:shards
                                   (list (w1-shard 0)
                                         (w1-shard 1 #:launcher (w1-launcher #:digest digest-c)))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "launcher-manifest-digest-mismatch")))

    (test-case "trial-assess: a record with an unknown outcome cannot certify the attempt"
      (define p (tmp-path "trial-unknown-outcome.json"))
      (write-envelope
       p
       (w1-envelope #:shards (list (w1-shard 0 #:record (w1-restore-record 0 #:outcome "unknown"))
                                   (w1-shard 1))))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "record-outcome-unknown")))

    (test-case "emit: strict trial binding keys are attached by the workflow adapter, never fabricated by the emitter"
      ;; Old --emit-restore-record behavior stays compatible: the emitter
      ;; knows nothing about trials. The workflow adapter attaches
      ;; attempt/trial-id/treatment/identity when it builds the envelope.
      (define out (tmp-path "emit-adapter-keys.json"))
      (script-succeeds
       (emit-command
        out
        "--fast-env-producer-result success --prepared-artifact-name prepared-env-fast --installer-sha256 deadbeef")
       '(("Q_PREPARED_ENV_STATE" . "restored") ("Q_PREPARED_ENV_RESTORE_MS" . "12000")))
      (define rec (read-jsexpr out))
      (for ([key (in-list (list 'attempt 'trial-id 'treatment 'identity))])
        (check-false (hash-has-key? rec key)
                     (format "the emitter must not fabricate ~a; the workflow adapter attaches it"
                             key))))

    (test-case "trial-assess: wrong envelope schema is refused"
      (define p (tmp-path "trial-bad-schema.json"))
      (write-envelope p (hash-set (w1-envelope) 'schema "something-else@1"))
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-true (positive? code))
      (check-equal? (hash-ref verdict 'reasons) (list "envelope-schema-invalid")))

    (test-case "trial-assess: --expected-identity reuses the real identity-manifest validator"
      ;; A producer-bound expected identity manifest (emitted by
      ;; --identity-emit) can be supplied by flag instead of inline.
      (define manifest (tmp-path "trial-expected-identity.json"))
      (script-succeeds (identity-args manifest))
      (define p (tmp-path "trial-flag-identity.json"))
      (write-envelope p (w1-envelope #:identity #f))
      (define-values (code verdict out)
        (trial-assess-verdict p (format " --expected-identity ~a" (path->string manifest))))
      (check-equal? code 0 "the flag-supplied identity satisfies the expected-identity requirement")
      (check-equal? (hash-ref verdict 'ready?) #t)
      ;; A file that is not a valid prepared-env-identity@1 manifest fails closed.
      (define not-manifest (tmp-path "trial-not-manifest.json"))
      (write-jsexpr not-manifest (hasheq 'schema "nope"))
      (define p-2 (tmp-path "trial-flag-identity-2.json"))
      (write-envelope p-2 (w1-envelope))
      (define-values (code-2 verdict-2 out-2)
        (trial-assess-verdict p-2 (format " --expected-identity ~a" (path->string not-manifest))))
      (check-equal? code-2 2 "structural misuse exits 2 without writing a verdict")
      (check-false verdict-2))

    (test-case "trial-assess: --producer-manifest binds the expected digest to real manifest bytes"
      (define not-the-manifest (tmp-path "trial-producer-manifest.bin"))
      (with-output-to-file not-the-manifest
                           (lambda () (display "definitely not the manifest"))
                           #:exists 'replace)
      (define p (tmp-path "trial-flag-manifest.json"))
      (write-envelope p (w1-envelope))
      (define-values (code verdict out)
        (trial-assess-verdict p (format " --producer-manifest ~a" (path->string not-the-manifest))))
      (check-true (positive? code)
                  "a manifest file whose bytes do not hash to the expected digest is refused")
      (check-equal? (hash-ref verdict 'ready?) #f)
      (check-true (pair? (member "producer-manifest-digest-mismatch-with-file"
                                 (hash-ref verdict 'reasons)))
                  "the mismatch names its reason"))

    (test-case "trial-assess: an unparseable envelope fails closed with exit 2 and no verdict"
      (define p (tmp-path "trial-not-json.json"))
      (with-output-to-file p (lambda () (display "this is not json")) #:exists 'replace)
      (define-values (code verdict out) (trial-assess-verdict p))
      (check-equal? code 2)
      (check-false verdict))

    (test-case "assess-trial-envelope: pure, deterministic, retains raw data (exported API)"
      (define env (w1-envelope))
      (define a (assess-trial-envelope env))
      (define b (assess-trial-envelope env))
      (check-equal? a b "the assessor is deterministic for identical input")
      (check-equal? (hash-ref a 'ready?) #t)
      (check-equal? (hash-ref a 'classification) "ready-root-on")
      (check-equal? (hash-ref a 'reasons) (list))
      (check-true (hash-has-key? a 'raw) "the result retains the raw envelope data")
      (check-equal? (length (hash-ref (hash-ref a 'raw) 'shards)) 2)
      ;; And the refused side is deterministic too.
      (define refused (assess-trial-envelope (hash-set env 'schema "nope")))
      (check-equal? refused (assess-trial-envelope (hash-set env 'schema "nope")))
      (check-equal? (hash-ref refused 'classification) "refused"))

    (test-case "trial-assess: verdict JSON is byte-deterministic for identical envelopes"
      (define p-1 (tmp-path "trial-det-1.json"))
      (define p-2 (tmp-path "trial-det-2.json"))
      (write-envelope p-1 (w1-envelope))
      (write-envelope p-2 (w1-envelope))
      (define v-1 (tmp-path "trial-det-verdict-1.json"))
      (define v-2 (tmp-path "trial-det-verdict-2.json"))
      (script-succeeds
       (format "--trial-assess --envelope ~a --out ~a" (path->string p-1) (path->string v-1)))
      (script-succeeds
       (format "--trial-assess --envelope ~a --out ~a" (path->string p-2) (path->string v-2)))
      (check-equal? (file->string v-1)
                    (file->string v-2)
                    "identical envelopes must produce byte-identical verdicts"))

    (test-case "trial-assess: existing aggregate and check modes are untouched by the new mode"
      ;; The strict trial mode is additive: report-only behavior must not move.
      (script-succeeds (string-append "--manifest " (path->string committed-manifest) " --check")))))

(module+ main
  (define failed (run-tests (suite)))
  (unless (zero? failed)
    (raise-user-error 'test-prepared-env-report "~a test failure(s)" failed)))

;; raco test instantiates the `test` submodule, not `main`; run the same
;; suite here and raise so `raco test` marks the file failed on any failure.
(module+ test
  (require rackunit/text-ui)
  (define failed (run-tests (suite)))
  (unless (zero? failed)
    (raise-user-error 'test-prepared-env-report "~a test failure(s)" failed)))

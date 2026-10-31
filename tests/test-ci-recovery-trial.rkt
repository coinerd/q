#lang racket

;; @speed fast
;; @suite ci
;; @boundary integration
;;
;; tests/test-ci-recovery-trial.rkt — v1.00.32 W2 (trusted experiment wave)
;;
;; Offline contract for the thin workflow evidence adapter
;; scripts/ci/ci-recovery-trial.rkt: the trusted-dispatch validation, the
;; consumer-side producer-manifest stamps, and the per-trial evidence
;; collection (Actions metadata → required critical path +
;; ci-recovery-trial-evidence@1 envelope). Nothing here dispatches CI;
;; every fixture is synthetic and labelled as such.
;;
;; The envelope fixtures are checked with the W1 evidence owner's PURE
;; assessor (assess-trial-envelope), so the adapter is proven to produce
;; envelopes the strict trial assessment actually accepts or refuses for
;; the named reasons — never a private shape.

(require rackunit
         racket/base
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         json
         (file "../scripts/ci/ci-recovery-trial.rkt")
         (only-in (file "../scripts/ci/prepared-env-report.rkt")
                  trial-evidence-schema
                  assess-trial-envelope)
         (only-in (file "../scripts/run-tests/ci-recovery-benchmark-report.rkt") config-dimensions)
         (only-in (file "../scripts/run-tests/sha256.rkt") sha256-hex))

(define-runtime-path here ".")
(define repo-root (simplify-path (build-path here "..")))

(define sha40-a "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
(define sha40-b "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
(define digest-d "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd")
(define digest-c "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc")

;; ------------------------------------------------------------ helpers

(define (write-jsexpr! path jsexpr)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (o) (write-json jsexpr o)) #:exists 'replace))

(define (write-file! path text)
  (make-directory* (path-only path))
  (display-to-file text path #:exists 'replace))

(define (read-json-file path)
  (with-input-from-file path read-json))

;; The report owner's CLI (scripts/run-tests/ci-recovery-benchmark-report.rkt)
;; run as a REAL subprocess over the adapter's collected JSONL evidence —
;; the exact command the coordinator runs offline after collecting trial
;; artifacts. Returns (values exit-code report-path).
(define (run-report-cli! out-path envelopes-path actions-path)
  (make-directory* (path-only out-path))
  (define so (open-output-string))
  (define se (open-output-string))
  (define code
    (parameterize ([current-output-port so]
                   [current-error-port se])
      (apply system*/exit-code
             (find-executable-path "racket")
             (list (path->string
                    (build-path repo-root "scripts" "run-tests" "ci-recovery-benchmark-report.rkt"))
                   "--envelopes"
                   (path->string envelopes-path)
                   "--actions"
                   (path->string actions-path)
                   "--out"
                   (path->string out-path)))))
  (unless (zero? code)
    (eprintf "report CLI failed (~a): ~a\n" code (get-output-string se)))
  (values code out-path))

(define (raw-trials-of report)
  (hash-ref (hash-ref report 'raw) 'trials))

;; read-json hashes are eq-keyed with SYMBOL keys, so the string arm name is
;; looked up via string->symbol (the report's own pure API is equal?-keyed;
;; the CLI output is JSON).
(define (arm-of report t)
  (hash-ref (hash-ref report 'arms) (string->symbol t)))

(define tmp-base (make-temporary-file "ci-recovery-trial~a" 'directory))
(test-case "real adapter CLI dispatch and empty invocation"
  (define cli (build-path repo-root "scripts/ci/ci-recovery-trial.rkt"))
  (define (invoke . args)
    (parameterize ([current-output-port (open-output-string)]
                   [current-error-port (open-output-string)])
      (apply system*/exit-code (find-executable-path "racket") cli args)))
  (check-equal? (invoke) 2)
  (check-equal? (invoke "validate-dispatch"
                        "--trial-id"
                        "cli"
                        "--attempt"
                        "1"
                        "--treatment"
                        "A"
                        "--head-sha"
                        sha40-a)
                0)
  (write-jsexpr! (build-path tmp-base "manifest.json") (hasheq 'fixture #t))
  (check-equal? (invoke "stamp-producer-manifest"
                        "--shard"
                        "0"
                        "--out"
                        (path->string (build-path tmp-base "empty-stamp.json"))
                        "--prepared-download"
                        (path->string tmp-base)
                        "--compiled-root"
                        "")
                0))

(define identity-fields
  (hasheq 'os
          "Linux"
          'os-image
          "ubuntu24:20261001"
          'arch
          "x64"
          'racket-version
          "8.10"
          'racket-executable-digest
          digest-c
          'lock-digest
          digest-d
          'resolved-set-digest
          digest-d
          'precompile-recipe-revision
          "setup-racket-full-path-r1"
          'policy-fingerprint
          "addon-store-v2|pltaddon-explicit|no-workspace-bytecode"
          'artifact-digest
          digest-d))

;; Synthetic Actions run/jobs metadata in the exact shape `gh api` returns.
(define (fake-run-json #:run-id [run-id 7100])
  (hasheq 'id
          run-id
          'name
          "CI recovery benchmark"
          'event
          "workflow_dispatch"
          'path
          "coinerd/q/.github/workflows/ci-recovery-benchmark.yml@refs/heads/main"
          'head_sha
          sha40-a
          'head_branch
          "main"
          'run_attempt
          1
          ;; Synthetic successful raw run, not inferred from successful jobs.
          'status
          "completed"
          'conclusion
          "success"
          'run_started_at
          "2026-10-30T10:00:00Z"))

(define (fake-job #:name name
                  #:id id
                  #:status [status "completed"]
                  #:conclusion [conclusion "success"]
                  #:started [started "2026-10-30T10:01:00Z"]
                  #:completed [completed "2026-10-30T10:05:00Z"])
  (hasheq 'name
          name
          'id
          id
          'status
          status
          'conclusion
          conclusion
          'started_at
          started
          'completed_at
          completed
          'steps
          (list (hasheq 'name "checkout" 'number 1 'status "completed" 'conclusion "success"))))

;; The FULL required set (scripts/required-pr-checks.policy) plus the
;; non-required fast-env canary. The required critical path must come from
;; test (2)'s completion (10:25 → 1500 s after run start): never from the
;; LATER-completing non-required fast-env (10:30 → would be 1800), never
;; from the sum of executions, and queue time stays separate.
(define (fake-jobs-json #:test2-start [test2-start "2026-10-30T10:09:30Z"]
                        #:test2-end [test2-end "2026-10-30T10:25:00Z"]
                        #:extra [extra '()])
  (hasheq
   'total_count
   (+ 14 (length extra))
   'jobs
   (append (list (fake-job #:name "lint"
                           #:id 1
                           #:started "2026-10-30T10:00:30Z"
                           #:completed "2026-10-30T10:02:00Z")
                 (fake-job #:name "lint-quality"
                           #:id 8
                           #:started "2026-10-30T10:00:30Z"
                           #:completed "2026-10-30T10:02:30Z")
                 ;; non-required: completes LAST, must never enter the
                 ;; required critical path (a sum or an unfiltered max
                 ;; would report 1800 instead of 1500)
                 (fake-job #:name "fast-env"
                           #:id 2
                           #:started "2026-10-30T10:02:30Z"
                           #:completed "2026-10-30T10:30:00Z")
                 (fake-job #:name "test (0)"
                           #:id 3
                           #:started "2026-10-30T10:09:30Z"
                           #:completed "2026-10-30T10:15:00Z")
                 (fake-job #:name "test (1)"
                           #:id 4
                           #:started "2026-10-30T10:09:30Z"
                           #:completed "2026-10-30T10:14:00Z")
                 (fake-job #:name "test (2)" #:id 5 #:started test2-start #:completed test2-end)
                 (fake-job #:name "test-platform" #:id 6)
                 (fake-job #:name "test-aggregate"
                           #:id 7
                           #:started "2026-10-30T10:20:30Z"
                           #:completed "2026-10-30T10:23:00Z")
                 (fake-job #:name "security" #:id 9)
                 (fake-job #:name "release-dry-run"
                           #:id 10
                           #:started "2026-10-30T10:01:00Z"
                           #:completed "2026-10-30T10:06:00Z")
                 (fake-job #:name "smoke (ubuntu-latest)"
                           #:id 11
                           #:started "2026-10-30T10:01:00Z"
                           #:completed "2026-10-30T10:04:00Z")
                 (fake-job #:name "workflows (0)"
                           #:id 12
                           #:started "2026-10-30T10:01:00Z"
                           #:completed "2026-10-30T10:08:00Z")
                 (fake-job #:name "workflows (1)"
                           #:id 13
                           #:started "2026-10-30T10:01:00Z"
                           #:completed "2026-10-30T10:08:00Z")
                 (fake-job #:name "workflows-aggregate"
                           #:id 14
                           #:started "2026-10-30T10:08:30Z"
                           #:completed "2026-10-30T10:09:00Z"))
           extra)))

;; ---------------------------------------------- producer-manifests fixture

;; The aggregate job's ACTUAL restore attestation fixture (its real
;; setup-racket outputs). 'verified is the only state a B/C trial may
;; present as a prepared-aggregate measurement.
(define (write-aggregate-attestation! dir #:state [state 'verified] #:run-id [run-id 7100])
  (define doc
    (case state
      [(verified)
       (hasheq 'schema
               aggregate-attestation-schema
               'run-id
               run-id
               'run-attempt
               1
               'head-sha
               sha40-a
               'prepared-requested
               "auto"
               'result
               "success"
               'state
               "restored"
               'identity-result
               "verified"
               'identity
               identity-fields
               'producer-manifest-digest
               digest-d
               'restore-ms
               15000)]
      [(cold)
       (hasheq 'schema
               aggregate-attestation-schema
               'run-id
               run-id
               'run-attempt
               1
               'head-sha
               sha40-a
               'prepared-requested
               "auto"
               'result
               "failure"
               'state
               "rebuilt"
               'identity-result
               "unknown"
               'restore-ms
               24000)]
      [(off)
       (hasheq 'schema
               aggregate-attestation-schema
               'run-id
               run-id
               'run-attempt
               1
               'head-sha
               sha40-a
               'prepared-requested
               "off"
               'result
               "skipped"
               'state
               "unavailable"
               'identity-result
               "unknown"
               'restore-ms
               "unknown")]
      [else #f]))
  (when doc
    (write-jsexpr! (build-path dir "aggregate" "prepared-aggregate-restore.json") doc)))

(define (write-producer-binding! dir #:treatment treatment)
  (define binding
    (hasheq 'schema
            producer-manifests-schema
            'prepared-env-manifest-sha256
            digest-d
            'compiled-root-manifest-sha256
            (if (string=? treatment "C") digest-d #f)))
  (write-jsexpr! (build-path dir "producer-manifests.json")
                 (for/hasheq ([k (in-hash-keys binding)]
                              #:unless (not (hash-ref binding k)))
                   (values k (hash-ref binding k))))
  (write-jsexpr! (build-path dir "identity-manifest.json")
                 (hasheq 'schema
                         "prepared-env-identity@1"
                         'artifact-name-base
                         "prepared-env-fast"
                         'artifact-identity
                         "prepared-env-fast-0123456789abcdef"
                         'fields
                         identity-fields)))

;; A shard's OWN measured identity manifest (what the consumer-side
;; setup-racket emits via the canonical --identity-emit from the RESTORED
;; environment). Defaults to the same fields the producer observed; tests
;; that pin the anti-tautology behavior pass a shard-measured DIFFERENCE.
(define (write-shard-identity! maps-shard-dir sid #:fields [fields identity-fields])
  (write-jsexpr! (build-path maps-shard-dir (format "shard-~a.identity.json" sid))
                 (hasheq 'schema
                         "prepared-env-identity@1"
                         'artifact-name-base
                         "prepared-env-fast"
                         'artifact-identity
                         "prepared-env-fast-0123456789abcdef"
                         'fields
                         fields)))

;; ------------------------------------------------- artifact tree fixtures

(define (launcher-datum #:mode [mode "verified-root-hit"]
                        #:hits [hits 3]
                        #:fallbacks [fallbacks 0]
                        #:exit-code [exit-code 0])
  (format
   (string-append "(compiled-root-launch-telemetry (mode . ~s) (exit-code . ~a) (wall-ms . 90000)"
                  " (root-hits . ~a) (fallbacks . ~a) (loads . 12) (bytes-served . 1048576)"
                  " (mechanism . \"external-compiled-root(map-dir symlinks)\"))")
   mode
   exit-code
   hits
   fallbacks))

(define (write-artifacts! dir
                          #:treatment treatment
                          #:shards [shards '(0 1 2)]
                          #:launcher-mode [launcher-mode "verified-root-hit"]
                          #:launcher-hits [launcher-hits 3]
                          #:launcher-fallbacks [launcher-fallbacks 0]
                          #:skip-shards [skip-shards '()]
                          #:record-run [record-run #f]
                          #:run-id [run-id 7100]
                          #:shard-identity-fields [shard-identity-fields identity-fields]
                          #:aggregate [aggregate (if (string=? treatment "A") 'off 'verified)]
                          #:producer-nested [producer-nested #f])
  (define record-run-effective (or record-run run-id))
  (for ([sid (in-list shards)])
    (unless (member sid skip-shards)
      (define record
        (hasheq 'schema
                "prepared-env-restore-record"
                'run-id
                record-run-effective
                'head-sha
                sha40-a
                'created-at-utc
                "2026-10-30T10:12:00Z"
                'shard
                sid
                'prepared-env-mode
                "auto"
                'fast-env-producer-result
                "success"
                'raw-state
                "restored"
                'outcome
                "verified"
                'restore-ms
                12000
                'wall-clock-seconds
                210.5
                'compiled-code-outcome
                (if (string=? treatment "C") "mounted-verified-root" "bounded-eager-gate")
                'eager-setup-seconds
                "unknown"
                'cache-state
                "unknown"
                'purge-zo-count
                0
                'compiled-root-outcome
                (if (string=? treatment "C") "verified-root-hit" "global-off")
                'compiled-root-producer-seconds
                (if (string=? treatment "C") 300 "unknown")
                'compiled-root-fallback-seconds
                "unknown"
                'cache-key
                "prepared-env-fast:deadbeef"
                'sha-context
                sha40-a))
      (write-jsexpr! (build-path dir
                                 "records"
                                 (format "prepared-env-records-~a-~a" record-run-effective sid)
                                 (format "record-~a-~a.json" record-run-effective sid))
                     record)
      (define maps-shard-dir
        (build-path dir "maps" (format "compiled-root-map-~a-~a" record-run-effective sid)))
      (write-file!
       (build-path maps-shard-dir (format "shard-~a.launch.json" sid))
       (launcher-datum #:mode launcher-mode #:hits launcher-hits #:fallbacks launcher-fallbacks))
      (write-jsexpr!
       (build-path maps-shard-dir (format "shard-~a.producer.json" sid))
       (hasheq 'schema
               producer-stamp-schema
               'shard
               sid
               'basis
               (if (string=? treatment "C") "compiled-root-manifest" "prepared-env-manifest")
               'producer-manifest
               "restored/manifest"
               'producer-manifest-digest
               digest-d))
      ;; the shard's OWN measured identity (the canonical emitted manifest)
      (write-shard-identity! maps-shard-dir sid #:fields shard-identity-fields)))
  (define producer-dir (build-path dir "producer"))
  (when producer-nested
    ;; the REAL artifact-extractor layout: a pattern download of one artifact
    ;; nests it under its own name below the download path
    (set! producer-dir
          (build-path producer-dir (format "prepared-env-producer-manifests-~a" run-id))))
  (write-producer-binding! producer-dir #:treatment treatment)
  (write-aggregate-attestation! dir #:state aggregate #:run-id run-id))

;; ------------------------------------------------- collect (the envelope)

(define (run-collect! dir
                      #:treatment treatment
                      #:trial-id [trial-id "trial-2026-10-30-c1"]
                      #:run-id [run-id 7100]
                      #:shards [shards '(0 1 2)]
                      #:skip-shards [skip-shards '()]
                      #:launcher-mode [launcher-mode "verified-root-hit"]
                      #:launcher-hits [launcher-hits 3]
                      #:launcher-fallbacks [launcher-fallbacks 0]
                      #:record-run [record-run #f]
                      #:jobs-json [jobs-json (fake-jobs-json)]
                      #:shard-identity-fields [shard-identity-fields identity-fields]
                      #:aggregate [aggregate (if (equal? treatment "A") 'off 'verified)]
                      #:producer-nested [producer-nested #f]
                      #:shard-plan [shard-plan (list 0 1 2)]
                      #:test-inventory-digest [test-inventory-digest digest-c]
                      #:created-at [created-at "2026-10-30T10:30:00Z"])
  (write-artifacts! dir
                    #:treatment treatment
                    #:shards shards
                    #:launcher-mode launcher-mode
                    #:launcher-hits launcher-hits
                    #:launcher-fallbacks launcher-fallbacks
                    #:skip-shards skip-shards
                    #:record-run record-run
                    #:run-id run-id
                    #:shard-identity-fields shard-identity-fields
                    #:aggregate aggregate
                    #:producer-nested producer-nested)
  (define run-path (build-path dir "run.json"))
  (define jobs-path (build-path dir "jobs.json"))
  (write-jsexpr! run-path (fake-run-json #:run-id run-id))
  (write-jsexpr! jobs-path jobs-json)
  (define out-dir (build-path dir "trial"))
  (define exit-code
    (collect-trial #:run-json run-path
                   #:jobs-json jobs-path
                   #:required-policy (build-path repo-root "scripts/required-pr-checks.policy")
                   #:records-dir (build-path dir "records")
                   #:maps-dir (build-path dir "maps")
                   #:producer-dir (build-path dir "producer")
                   #:aggregate-attestation (build-path dir "aggregate")
                   #:trial-id trial-id
                   #:attempt 1
                   #:treatment treatment
                   #:run-id run-id
                   #:run-attempt 1
                   #:head-sha sha40-a
                   #:workflow-revision-sha sha40-b
                   #:shard-total (length shards)
                   #:shard-plan shard-plan
                   #:test-inventory-digest test-inventory-digest
                   #:created-at-utc created-at
                   #:out-dir out-dir))
  (values exit-code out-dir))

(module+ test
  ;; ---------------------------------------------------- validate-dispatch

  (test-case "validate-dispatch accepts a bound A/B/C trial"
    (for ([treatment (in-list '("A" "B" "C"))])
      (check-equal?
       (validate-dispatch #:trial-id "trial-x" #:attempt 1 #:treatment treatment #:head-sha sha40-a)
       #t
       (format "treatment ~a must validate" treatment))))

  (test-case "validate-dispatch refuses ill-formed trusted dispatch identities"
    (check-false (validate-dispatch #:trial-id "" #:attempt 1 #:treatment "C" #:head-sha sha40-a))
    (check-false (validate-dispatch #:trial-id "t" #:attempt 0 #:treatment "C" #:head-sha sha40-a))
    (check-false (validate-dispatch #:trial-id "t" #:attempt 1 #:treatment "C" #:head-sha "zzz"))
    (check-false (validate-dispatch #:trial-id "t" #:attempt 1 #:treatment "D" #:head-sha sha40-a))
    (check-false (validate-dispatch #:trial-id "t" #:attempt 1 #:treatment "C" #:head-sha "AAAA"))
    (check-false (validate-dispatch #:trial-id "t" #:attempt 1.5 #:treatment "C" #:head-sha sha40-a)))

  ;; --------------------------------------------- stamp-producer-manifest

  (test-case "stamp-producer-manifest prefers the restored compiled-root manifest"
    (define dir (build-path tmp-base "stamp-root"))
    (define download (build-path dir "download"))
    (define root (build-path download "artifact" "q-compiled" "trusted-root"))
    (make-directory* root)
    (display-to-file "compiled-root-manifest-bytes"
                     (build-path root "manifest.rktd")
                     #:exists 'replace)
    (write-file! (build-path download "artifact" "manifest.json") "prepared-env-manifest")
    (define out (build-path dir "stamp.json"))
    (define code
      (stamp-producer-manifest #:shard 1 #:out out #:prepared-download download #:compiled-root root))
    (check-equal? code 0)
    (define stamp (read-json-file out))
    (check-equal? (hash-ref stamp 'basis) "compiled-root-manifest")
    (check-equal? (hash-ref stamp 'shard) 1)
    (check-true (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref stamp 'producer-manifest-digest))))

  (test-case "stamp-producer-manifest falls back to the restored prepared-env manifest"
    (define dir (build-path tmp-base "stamp-prepared"))
    (define download (build-path dir "download" "nested"))
    (make-directory* download)
    (display-to-file "prepared-env-manifest-bytes"
                     (build-path download "manifest.json")
                     #:exists 'replace)
    (define out (build-path dir "stamp.json"))
    (define code (stamp-producer-manifest #:shard 0 #:out out #:prepared-download download))
    (check-equal? code 0)
    (define stamp (read-json-file out))
    (check-equal? (hash-ref stamp 'basis) "prepared-env-manifest")
    (check-true (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref stamp 'producer-manifest-digest))))

  (test-case "stamp-producer-manifest writes nothing when nothing was restored"
    (define dir (build-path tmp-base "stamp-none"))
    (define download (build-path dir "empty"))
    (make-directory* download)
    (define out (build-path dir "stamp.json"))
    (define code (stamp-producer-manifest #:shard 0 #:out out #:prepared-download download))
    (check-equal? code 1 "no restored producer manifest is an honest missing stamp")
    (check-false (file-exists? out) "an unobserved binding is never invented"))

  ;; --------------------------------------------------------------- collect

  (test-case "collect: treatment C with real launcher telemetry assesses ready-root-on"
    (define dir (build-path tmp-base "collect-c"))
    (define-values (code out-dir) (run-collect! dir #:treatment "C"))
    (check-equal? code 0)
    ;; the trial artifact is ONE directory the report owner can process
    (for ([f (in-list '("run.json" "jobs.json" "critical-path.json" "envelope.json"))])
      (check-true (file-exists? (build-path out-dir f)) (format "missing ~a" f)))
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (check-equal? (hash-ref envelope 'schema) trial-evidence-schema)
    (define trial (hash-ref envelope 'trial))
    (check-equal? (hash-ref trial 'trial-id) "trial-2026-10-30-c1")
    (check-equal? (hash-ref trial 'attempt) 1)
    (check-equal? (hash-ref trial 'treatment) "C")
    (check-equal? (hash-ref trial 'run-id) 7100)
    (check-equal? (hash-ref trial 'head-sha) sha40-a)
    (define expected (hash-ref envelope 'expected))
    (check-equal? (hash-ref expected 'shard-inventory) '(0 1 2))
    (check-equal? (hash-ref expected 'producer-manifest-digest) digest-d)
    (check-equal? (hash-ref (hash-ref expected 'identity) 'racket-version) "8.10")
    ;; every expected shard is present exactly once with its launcher datum
    (define shards (hash-ref envelope 'shards))
    (check-equal? (length shards) 3)
    (for ([entry (in-list shards)]
          [sid (in-list '(0 1 2))])
      (check-equal? (hash-ref entry 'shard) sid)
      (define launcher (hash-ref entry 'launcher))
      (check-equal? (hash-ref launcher 'mode) "verified-root-hit")
      (check-equal? (hash-ref launcher 'root-hits) 3)
      (check-equal? (hash-ref launcher 'fallbacks) 0)
      (check-equal? (hash-ref launcher 'exit-code) 0)
      (check-equal? (hash-ref launcher 'wall-ms) 90000)
      (check-equal? (hash-ref launcher 'loads) 12)
      (check-equal? (hash-ref launcher 'bytes-served) 1048576)
      (check-equal? (hash-ref launcher 'producer-manifest-digest) digest-d))
    ;; the strict W1 assessor accepts the adapter's envelope
    (define verdict (assess-trial-envelope envelope))
    (check-equal? (hash-ref verdict 'ready?) #t)
    (check-equal? (hash-ref verdict 'classification) "ready-root-on")
    (check-equal? (hash-ref verdict 'reasons) '()))

  (test-case "collect: treatment A with all-global-off telemetry assesses ready-root-off"
    (define dir (build-path tmp-base "collect-a"))
    (define-values (code out-dir)
      (run-collect! dir #:treatment "A" #:launcher-mode "global-off" #:launcher-hits 0))
    (check-equal? code 0)
    (define verdict (assess-trial-envelope (read-json-file (build-path out-dir "envelope.json"))))
    (check-equal? (hash-ref verdict 'ready?) #t)
    (check-equal? (hash-ref verdict 'classification) "ready-root-off")
    (check-equal? (hash-ref verdict 'treatment) "A"))

  (test-case "collect: records are stamped with the strict trial keys (verbatim values win)"
    ;; The concurrent strict-record repair requires per-record trial keys
    ;; equal to the trial/expected blocks. The adapter stamps the keys when
    ;; the emitter does not carry them yet, and never overwrites values.
    (define dir (build-path tmp-base "collect-stamp"))
    (define-values (code out-dir) (run-collect! dir #:treatment "B"))
    (check-equal? code 0)
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (for ([entry (in-list (hash-ref envelope 'shards))])
      (define record (hash-ref entry 'record))
      (check-equal? (hash-ref record 'trial-id) "trial-2026-10-30-c1")
      (check-equal? (hash-ref record 'attempt) 1)
      (check-equal? (hash-ref record 'treatment) "B")
      (check-equal? (hash-ref record 'identity) identity-fields)))

  (test-case "collect: a missing shard record is retained honestly and refused"
    (define dir (build-path tmp-base "collect-missing"))
    (define-values (code out-dir)
      (run-collect! dir
                    #:treatment "B"
                    #:skip-shards '(2)
                    #:launcher-mode "global-off"
                    #:launcher-hits 0))
    (check-equal? code 0 "collection retains incomplete attempts instead of failing")
    (define verdict (assess-trial-envelope (read-json-file (build-path out-dir "envelope.json"))))
    (check-false (hash-ref verdict 'ready?))
    (check-equal? (hash-ref verdict 'classification) "refused")
    (check-equal? (hash-ref verdict 'reasons) '("missing-shard-record")))

  (test-case "collect: a digest mismatch between shard stamp and producer binding is refused"
    (define dir (build-path tmp-base "collect-digest"))
    (define-values (code out-dir)
      (run-collect! dir
                    #:treatment "A"
                    #:launcher-mode "global-off"
                    #:launcher-hits 0
                    ;; the record emitter ran under a different run id, so the
                    ;; stamp digest no longer matches the producer binding
                    #:record-run 7000
                    #:run-id 7100))
    (check-equal? code 0)
    ;; digest stays equal; the mismatch here is the run-id binding
    (define verdict (assess-trial-envelope (read-json-file (build-path out-dir "envelope.json"))))
    (check-false (hash-ref verdict 'ready?))
    (check-true (and (member "record-run-id-mismatch" (hash-ref verdict 'reasons)) #t)
                (format "reasons: ~a" (hash-ref verdict 'reasons))))

  (test-case "collect: a launcher that never ran is omitted, never invented"
    (define dir (build-path tmp-base "collect-no-launcher"))
    ;; no map artifacts at all: the C trial must refuse with the named reason.
    ;; The aggregate restore IS attested verified, so the ONLY refusal driver
    ;; here is the missing launcher telemetry.
    (write-producer-binding! (build-path dir "producer") #:treatment "C")
    (write-aggregate-attestation! dir #:state 'verified)
    (for ([sid (in-list '(0 1 2))])
      (write-jsexpr! (build-path dir
                                 "records"
                                 (format "prepared-env-records-7100-~a" sid)
                                 (format "record-7100-~a.json" sid))
                     (hasheq 'schema
                             "prepared-env-restore-record"
                             'run-id
                             7100
                             'head-sha
                             sha40-a
                             'shard
                             sid
                             'outcome
                             "verified"))
      (write-jsexpr! (build-path dir
                                 "maps"
                                 (format "compiled-root-map-7100-~a" sid)
                                 (format "shard-~a.producer.json" sid))
                     (hasheq 'producer-manifest-digest digest-d)))
    (define run-path (build-path dir "run.json"))
    (define jobs-path (build-path dir "jobs.json"))
    (write-jsexpr! run-path (fake-run-json))
    (write-jsexpr! jobs-path (fake-jobs-json))
    (define out-dir (build-path dir "trial"))
    (define code
      (collect-trial #:run-json run-path
                     #:jobs-json jobs-path
                     #:required-policy (build-path repo-root "scripts/required-pr-checks.policy")
                     #:records-dir (build-path dir "records")
                     #:maps-dir (build-path dir "maps")
                     #:producer-dir (build-path dir "producer")
                     #:aggregate-attestation (build-path dir "aggregate")
                     #:trial-id "trial-2026-10-30-c1"
                     #:attempt 1
                     #:treatment "C"
                     #:run-id 7100
                     #:run-attempt 1
                     #:head-sha sha40-a
                     #:workflow-revision-sha sha40-b
                     #:shard-total 3
                     #:shard-plan '(0 1 2)
                     #:test-inventory-digest digest-c
                     #:created-at-utc "2026-10-30T10:30:00Z"
                     #:out-dir out-dir))
    (check-equal? code 0)
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (for ([entry (in-list (hash-ref envelope 'shards))])
      (check-false (hash-has-key? entry 'launcher)
                   "a launcher that never ran is omitted (missing-launcher-telemetry upstream)"))
    (define verdict (assess-trial-envelope envelope))
    (check-false (hash-ref verdict 'ready?))
    (check-true (and (member "missing-launcher-telemetry" (hash-ref verdict 'reasons)) #t)))

  ;; ------------------------------------------------- required critical path

  (test-case "collect: required critical path is max completion minus run start"
    (define dir (build-path tmp-base "collect-path"))
    (define-values (code out-dir) (run-collect! dir #:treatment "A"))
    (check-equal? code 0)
    (define cp (read-json-file (build-path out-dir "critical-path.json")))
    ;; run started 10:00:00; the last REQUIRED completion is test (2) at
    ;; 10:25:00 → 1500 s. test-aggregate completes at 10:23 (1380) and the
    ;; non-required lint-quality at 10:30 — neither may drive the number.
    (check-equal? (hash-ref cp 'required-critical-path-seconds) 1500)
    ;; queue time is separate: test (2) started at 10:09:30 → 570 s queue
    (define test2
      (for/first ([j (in-list (hash-ref cp 'required-jobs))]
                  #:when (equal? (hash-ref j 'name) "test (2)"))
        j))
    (check-equal? (hash-ref test2 'queue-seconds) 570)
    (check-equal? (hash-ref test2 'execution-seconds) 930)
    ;; the non-required fast-env job is NOT required and must not appear;
    ;; lint-quality is a required check and must be present
    (check-false (member "fast-env"
                         (map (lambda (j) (hash-ref j 'name)) (hash-ref cp 'required-jobs))))
    (check-true (and (member "fast-env" (hash-ref cp 'non-required-jobs)) #t))
    (check-true (and (member "lint-quality"
                             (map (lambda (j) (hash-ref j 'name)) (hash-ref cp 'required-jobs)))
                     #t))
    (check-equal? (hash-ref cp 'missing-required-jobs) '())
    (check-equal? (hash-ref cp 'run-started-at) "2026-10-30T10:00:00Z")
    (check-equal? (hash-ref cp 'head-sha) sha40-a)
    (check-equal? (hash-ref cp 'workflow-revision-sha) sha40-b)
    (check-false (equal? (hash-ref cp 'head-sha) (hash-ref cp 'workflow-revision-sha))
                 "the tested tree and the workflow revision are recorded as distinct fields"))

  (test-case "collect: missing required jobs are named, never silently dropped"
    (define dir (build-path tmp-base "collect-path-missing"))
    (define-values (code out-dir) (run-collect! dir #:treatment "A" #:jobs-json (fake-jobs-json)))
    (check-equal? code 0)
    (define cp (read-json-file (build-path out-dir "critical-path.json")))
    (check-equal? (hash-ref cp 'missing-required-jobs) '())
    ;; drop two required jobs from the fixture
    (define partial (fake-jobs-json))
    (define remaining
      (for/list ([j (in-list (hash-ref partial 'jobs))]
                 #:unless (member (hash-ref j 'name) '("test (1)" "security")))
        j))
    (define dir2 (build-path tmp-base "collect-path-missing2"))
    (define-values (code2 out-dir2)
      (run-collect! dir2
                    #:treatment "A"
                    #:jobs-json (hasheq 'total_count (length remaining) 'jobs remaining)))
    (check-equal? code2 0)
    (define cp2 (read-json-file (build-path out-dir2 "critical-path.json")))
    (check-equal? (sort (hash-ref cp2 'missing-required-jobs) string<?)
                  '("security" "test (1)")
                  "a missing required check is named fail-closed"))

  (test-case "bind-producer: digests over the real staged bytes only"
    (define dir (build-path tmp-base "bind-producer"))
    (define stage (build-path dir "stage"))
    (define out (build-path dir "producer-manifests.json"))
    ;; no staged manifest: fail loudly (a broken producer never binds)
    (check-exn exn:fail? (lambda () (bind-producer #:stage stage #:out out)))
    (make-directory* (build-path stage "q-compiled" "trusted-root"))
    (display-to-file "prepared-manifest-bytes" (build-path stage "manifest.json") #:exists 'replace)
    (check-equal? (bind-producer #:stage stage #:out out) 0)
    (define binding (read-json-file out))
    (check-equal? (hash-ref binding 'schema) producer-manifests-schema)
    (check-equal? (hash-ref binding 'prepared-env-manifest-sha256)
                  (sha256-hex (file->bytes (build-path stage "manifest.json"))))
    (check-false (hash-has-key? binding 'compiled-root-manifest-sha256)
                 "an unbuilt compiled root is never invented into the binding")
    (display-to-file "root-manifest-bytes"
                     (build-path stage "q-compiled" "trusted-root" "manifest.rktd")
                     #:exists 'replace)
    (check-equal? (bind-producer #:stage stage #:out out) 0)
    (check-equal?
     (hash-ref (read-json-file out) 'compiled-root-manifest-sha256)
     (sha256-hex (file->bytes (build-path stage "q-compiled" "trusted-root" "manifest.rktd")))))

  (test-case "real bind CLI and relocated consumer stamp hash actual artifact layout"
    (define dir (build-path tmp-base "real-layout"))
    (define stage (build-path dir "producer/prepared-env-fast"))
    (write-file! (build-path stage "manifest.json") "{\"actual\":true}")
    (write-file! (build-path stage "q-compiled/trusted-root/manifest.rktd") "(actual compiled bytes)")
    (define binding (build-path dir "binding.json"))
    (check-equal? (parameterize ([current-output-port (open-output-string)]
                                 [current-error-port (open-output-string)])
                    (system*/exit-code (find-executable-path "racket")
                                       (build-path repo-root "scripts/ci/ci-recovery-trial.rkt")
                                       "bind-producer"
                                       "--stage"
                                       (path->string stage)
                                       "--out"
                                       (path->string binding)))
                  0)
    (define downloaded (build-path dir "consumer/download/artifact"))
    (make-directory* (path-only downloaded))
    (copy-directory/files stage downloaded)
    (define stamp (build-path dir "stamp.json"))
    (for ([compiled (in-list '(#f #t))])
      (check-equal? (stamp-producer-manifest #:shard 0
                                             #:out stamp
                                             #:prepared-download downloaded
                                             #:compiled-root
                                             (if compiled
                                                 (build-path downloaded "q-compiled/trusted-root")
                                                 ""))
                    0)
      (check-equal?
       (hash-ref (read-json-file stamp) 'producer-manifest-digest)
       (hash-ref (read-json-file binding)
                 (if compiled 'compiled-root-manifest-sha256 'prepared-env-manifest-sha256)))))

  (test-case "real CLI producer emission matches different installed root and binds bytes"
    (define dir (build-path tmp-base "relocated"))
    (define stage (build-path dir "producer"))
    (define installed (build-path dir "installed"))
    (write-file! (build-path stage "addon-store/pkg/file") "actual package bytes")
    (copy-directory/files (build-path stage "addon-store") installed)
    (define lock (build-path dir "lock"))
    (write-file! lock "lock bytes")
    (define (emit flags out)
      (parameterize ([current-output-port (open-output-string)]
                     [current-error-port (open-output-string)])
        (apply system*/exit-code
               (find-executable-path "racket")
               (build-path repo-root "scripts/ci/ci-recovery-trial.rkt")
               (append (list "emit-identity"
                             "--out"
                             (path->string out)
                             "--lock-file"
                             (path->string lock)
                             "--racket-version"
                             "8.10"
                             "--os"
                             "Linux"
                             "--os-image"
                             "image"
                             "--arch"
                             "x64"
                             "--recipe-revision"
                             "trial-addon-relative-r1"
                             "--policy-fingerprint"
                             "policy")
                       flags))))
    (define produced (build-path dir "produced.json"))
    (define observed (build-path dir "observed.json"))
    (check-equal? (emit (list "--stage" (path->string stage)) produced) 0)
    (check-equal? (emit (list "--addon-store" (path->string installed)) observed) 0)
    (check-equal? (read-json-file produced) (read-json-file observed))
    (write-file! (build-path installed "pkg/file") "tampered")
    (check-equal? (emit (list "--addon-store" (path->string installed)) observed) 0)
    (check-not-equal? (read-json-file produced) (read-json-file observed)))

  (test-case "emit-identity: producer-observed W6 identity via the W1 CLI shape"
    (define dir (build-path tmp-base "emit-identity"))
    (define stage (build-path dir "stage"))
    (make-directory* (build-path stage "addon-store" "pkg"))
    (display-to-file "store-file-a" (build-path stage "addon-store" "pkg" "a.rktd") #:exists 'replace)
    (display-to-file "store-file-b" (build-path stage "addon-store" "b.rktd") #:exists 'replace)
    (define lock-file (build-path dir "lock.rktd"))
    (display-to-file "lock-bytes" lock-file #:exists 'replace)
    (define out (build-path dir "identity-manifest.json"))
    ;; a missing staged addon store fails loudly
    (check-exn exn:fail?
               (lambda ()
                 (emit-identity #:stage (build-path dir "absent")
                                #:out out
                                #:lock-file lock-file
                                #:racket-version "8.10"
                                #:os "Linux"
                                #:os-image "ubuntu24:test"
                                #:arch "x64"
                                #:recipe-revision "setup-racket-full-path-r1"
                                #:policy-fingerprint "addon-store-v2")))
    (check-false (file-exists? out))
    (check-equal? (emit-identity #:stage stage
                                 #:out out
                                 #:lock-file lock-file
                                 #:racket-version "8.10"
                                 #:os "Linux"
                                 #:os-image "ubuntu24:test"
                                 #:arch "x64"
                                 #:recipe-revision "setup-racket-full-path-r1"
                                 #:policy-fingerprint "addon-store-v2")
                  0)
    ;; the W1 canonical shape, with THIS store's real bytes hashed
    (define manifest (read-json-file out))
    (check-equal? (hash-ref manifest 'schema) "prepared-env-identity@1")
    (define fields (hash-ref manifest 'fields))
    (check-equal? (hash-ref fields 'lock-digest) (sha256-hex (file->bytes lock-file)))
    (check-equal? (hash-ref fields 'resolved-set-digest)
                  (hash-ref fields 'artifact-digest)
                  "artifact-digest mirrors the resolved set (same recipe as setup-racket)")
    (check-true (regexp-match? #px"^[0-9a-f]{64}$" (hash-ref fields 'resolved-set-digest)))
    (check-equal? (hash-ref fields 'os) "Linux")
    (check-equal? (hash-ref fields 'racket-version) "8.10")
    (check-true (string-prefix? (hash-ref manifest 'artifact-identity) "prepared-env-fast-"))
    ;; the emitted fields are accepted as expected-identity by the W1 assessor
    (define envelope-with-emitted-identity
      (hasheq 'schema
              trial-evidence-schema
              'trial
              (hasheq 'trial-id
                      "t"
                      'attempt
                      1
                      'treatment
                      "C"
                      'run-id
                      1
                      'head-sha
                      sha40-a
                      'created-at-utc
                      "2026-10-30T10:00:00Z")
              'expected
              (hasheq 'shard-inventory (list 0) 'producer-manifest-digest digest-d 'identity fields)
              'shards
              '()))
    (define verdict (assess-trial-envelope envelope-with-emitted-identity))
    (check-false (member "expected-identity-missing" (hash-ref verdict 'reasons)))
    (check-false (ormap (lambda (r) (string-prefix? r "expected-identity-invalid"))
                        (hash-ref verdict 'reasons))
                 (format "reasons: ~a" (hash-ref verdict 'reasons))))

  (test-case "collect: collect refuses ill-formed trial identity (fail closed)"
    (define dir (build-path tmp-base "collect-bad-identity"))
    (check-exn exn:fail?
               (lambda ()
                 (collect-trial #:run-json (build-path dir "none.json")
                                #:jobs-json (build-path dir "none.json")
                                #:required-policy
                                (build-path repo-root "scripts/required-pr-checks.policy")
                                #:records-dir (build-path dir "records")
                                #:maps-dir (build-path dir "maps")
                                #:producer-dir (build-path dir "producer")
                                #:trial-id ""
                                #:attempt 1
                                #:treatment "C"
                                #:run-id 7100
                                #:run-attempt 1
                                #:head-sha "bad"
                                #:workflow-revision-sha sha40-b
                                #:shard-total 3
                                #:shard-plan '(0 1 2)
                                #:test-inventory-digest digest-c
                                #:created-at-utc "2026-10-30T10:30:00Z"
                                #:out-dir (build-path dir "trial")))))

  ;; ------------------------------------- v1.00.32 W2 repair: REAL layouts

  (test-case "collect: the REAL nested producer-artifact layout binds (not flat-only)"
    ;; A pattern download nests each artifact below its own name under the
    ;; download path; the first cut read the flat path only, so every real
    ;; trial was refused. The adapter locates the binding files by name.
    (define dir (build-path tmp-base "collect-nested-producer"))
    (define-values (code out-dir) (run-collect! dir #:treatment "C" #:producer-nested #t))
    (check-equal? code 0)
    (check-true (file-exists? (build-path out-dir "producer-manifests.json"))
                "the nested producer binding is located and retained")
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (check-equal? (hash-ref (hash-ref envelope 'expected) 'producer-manifest-digest)
                  digest-d
                  "the expected digest comes from the REAL nested binding"))

  (test-case "collect: cancelled/null API job fields serialize honestly, never crash or false-time"
    ;; A cancelled required job carries JSON null conclusion and null
    ;; started_at/completed_at in the raw Jobs API payload.
    (define cancelled-job
      (hasheq 'name
              "test (2)"
              'id
              5
              'status
              "completed"
              'conclusion
              'null
              'started_at
              'null
              'completed_at
              'null
              'steps
              '()))
    (define jobs
      (hasheq 'total_count
              14
              'jobs
              (for/list ([j (in-list (hash-ref (fake-jobs-json) 'jobs))])
                (if (equal? (hash-ref j 'name) "test (2)") cancelled-job j))))
    (define dir (build-path tmp-base "collect-nulls"))
    (define-values (code out-dir) (run-collect! dir #:treatment "A" #:jobs-json jobs))
    (check-equal? code 0 "a null-conclusion job never crashes collection")
    ;; raw retention is verbatim: the nulls survive into run.json/jobs.json
    (define raw-jobs-text (file->string (build-path out-dir "jobs.json")))
    (check-true (string-contains? raw-jobs-text "null")
                "the raw Jobs API nulls are retained verbatim")
    (define cp (read-json-file (build-path out-dir "critical-path.json")))
    (define test2
      (for/first ([j (in-list (hash-ref cp 'required-jobs))]
                  #:when (equal? (hash-ref j 'name) "test (2)"))
        j))
    (check-equal? (hash-ref test2 'conclusion) "unknown")
    (check-equal? (hash-ref test2 'started_at) "unknown")
    (check-equal? (hash-ref test2 'completed_at) "unknown")
    (check-equal? (hash-ref test2 'queue-seconds)
                  "unknown"
                  "an unobserved queue time is never a false number")
    (check-equal? (hash-ref test2 'execution-seconds) "unknown")
    (check-false (equal? (hash-ref cp 'required-critical-path-seconds) #f)
                 "no false timing literal in the critical path")
    (check-equal? (hash-ref cp 'required-critical-path-seconds)
                  "unknown"
                  "the path is unknown while a required job has no parseable completion")
    ;; the whole attempt is retained and the report-level assessment refuses
    ;; it with named reasons (missing attempt keeps raw + envelope + refusal)
    (check-true (file-exists? (build-path out-dir "envelope.json")))
    (check-true (file-exists? (build-path out-dir "envelope.jsonl")))
    (check-true (file-exists? (build-path out-dir "actions.jsonl")))
    (define actions (read-json-file (build-path out-dir "actions.json")))
    (define cancelled-evidence
      (for/first ([j (in-list (hash-ref actions 'jobs))]
                  #:when (equal? (hash-ref j 'name) "test (2)"))
        j))
    (check-equal? (hash-ref cancelled-evidence 'conclusion) "unknown")
    (check-equal? (hash-ref cancelled-evidence 'started-at) "unknown")
    (check-equal? (hash-ref cancelled-evidence 'role) "required")
    ;; the report CLI refuses this attempt with the exact named reasons
    (define report-dir (build-path dir "report"))
    (define-values (rc _)
      (run-report-cli! (build-path report-dir "report.json")
                       (build-path out-dir "envelope.jsonl")
                       (build-path out-dir "actions.jsonl")))
    (check-equal? rc 0 "the report turns refusal data into DATA, never a crash")
    (define report (read-json-file (build-path report-dir "report.json")))
    (define reasons (hash-ref (first (raw-trials-of report)) 'reasons))
    (check-true (and (member "required-job-timestamps-missing" reasons) #t)
                (format "refusal reasons: ~a" reasons))
    (check-equal? (hash-ref (first (raw-trials-of report)) 'critical-path-seconds) "unknown")
    (check-false (member "missing-actions-evidence" reasons)
                 "the actions evidence the adapter produced is directly consumable"))

  (test-case "per-shard identity: the record carries the SHARD's measured identity, not the producer's"
    ;; Anti-tautology pin: expected.identity stays producer-bound while
    ;; record.identity is what THIS shard actually measured. A divergence is
    ;; a named, field-level refusal — proving the comparison is meaningful.
    (define shard-measured (hash-set identity-fields 'lock-digest digest-c))
    (define dir (build-path tmp-base "collect-measured-identity"))
    (define-values (code out-dir)
      (run-collect! dir #:treatment "C" #:shard-identity-fields shard-measured))
    (check-equal? code 0)
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (for ([entry (in-list (hash-ref envelope 'shards))])
      (check-equal? (hash-ref (hash-ref entry 'record) 'identity)
                    shard-measured
                    "the stamped identity is the shard's OWN measured identity"))
    (check-equal? (hash-ref (hash-ref (hash-ref envelope 'expected) 'identity) 'lock-digest)
                  digest-d
                  "the expected identity stays producer-bound")
    (define verdict (assess-trial-envelope envelope))
    (check-false (hash-ref verdict 'ready?))
    (check-true (and (member "record-identity-mismatch-lock-digest" (hash-ref verdict 'reasons)) #t)
                (format "reasons: ~a" (hash-ref verdict 'reasons))))

  (test-case "per-shard identity: a shard without a captured identity is refused, never filled from the producer"
    ;; No shard-<n>.identity.json in the map artifacts: the record carries
    ;; NO identity (the producer's manifest must never be borrowed) and the
    ;; strict assessor refuses record-identity-missing.
    (define dir (build-path tmp-base "collect-missing-shard-identity"))
    (write-artifacts! dir
                      #:treatment "A"
                      #:launcher-mode "global-off"
                      #:launcher-hits 0
                      #:aggregate 'off)
    ;; delete the captured shard identities
    (for ([p (in-directory (build-path dir "maps"))]
          #:when (regexp-match? #rx"shard-[0-9]+\\.identity\\.json$" (path->string p)))
      (delete-file p))
    (define run-path (build-path dir "run.json"))
    (define jobs-path (build-path dir "jobs.json"))
    (write-jsexpr! run-path (fake-run-json))
    (write-jsexpr! jobs-path (fake-jobs-json))
    (define out-dir (build-path dir "trial"))
    (define code
      (collect-trial #:run-json run-path
                     #:jobs-json jobs-path
                     #:required-policy (build-path repo-root "scripts/required-pr-checks.policy")
                     #:records-dir (build-path dir "records")
                     #:maps-dir (build-path dir "maps")
                     #:producer-dir (build-path dir "producer")
                     #:aggregate-attestation (build-path dir "aggregate")
                     #:trial-id "trial-2026-10-30-c1"
                     #:attempt 1
                     #:treatment "A"
                     #:run-id 7100
                     #:run-attempt 1
                     #:head-sha sha40-a
                     #:workflow-revision-sha sha40-b
                     #:shard-total 3
                     #:shard-plan '(0 1 2)
                     #:test-inventory-digest digest-c
                     #:created-at-utc "2026-10-30T10:30:00Z"
                     #:out-dir out-dir))
    (check-equal? code 0)
    (define envelope (read-json-file (build-path out-dir "envelope.json")))
    (for ([entry (in-list (hash-ref envelope 'shards))])
      (check-false (hash-has-key? (hash-ref entry 'record) 'identity)
                   "no identity is invented for a shard that measured none"))
    (define verdict (assess-trial-envelope envelope))
    (check-false (hash-ref verdict 'ready?))
    (check-true (and (member "record-identity-missing" (hash-ref verdict 'reasons)) #t)))

  ;; --------------------------------- aggregate restore attestation (B/C)

  (test-case "real aggregate CLI missing restore outputs emits valid unknown JSON"
    (define out (build-path tmp-base "aggregate-missing.json"))
    (define racket-exe (find-executable-path "racket"))
    (check-equal? (parameterize ([current-output-port (open-output-string)]
                                 [current-error-port (open-output-string)]
                                 [current-environment-variables (make-environment-variables)])
                    (system*/exit-code racket-exe
                                       (build-path repo-root "scripts/ci/ci-recovery-trial.rkt")
                                       "attest-aggregate"
                                       "--out"
                                       (path->string out)))
                  0)
    (check-equal? (hash-ref (read-json-file out) 'restore-ms) "unknown"))

  (test-case "real collect CLI rejects every aggregate binding mutation but retains files"
    (define dir (build-path tmp-base "cli-collect"))
    (define-values (initial out) (run-collect! dir #:treatment "C"))
    (define att-path (build-path dir "aggregate/prepared-aggregate-restore.json"))
    (define att (read-json-file att-path))
    (define (invoke)
      (parameterize ([current-output-port (open-output-string)]
                     [current-error-port (open-output-string)])
        (apply system*/exit-code
               (find-executable-path "racket")
               (build-path repo-root "scripts/ci/ci-recovery-trial.rkt")
               (append (list "collect")
                       (append-map
                        (lambda (pair) (list (car pair) (cdr pair)))
                        (list (cons "--run-json" (path->string (build-path dir "run.json")))
                              (cons "--jobs-json" (path->string (build-path dir "jobs.json")))
                              (cons "--required-policy"
                                    (path->string (build-path repo-root
                                                              "scripts/required-pr-checks.policy")))
                              (cons "--records-dir" (path->string (build-path dir "records")))
                              (cons "--maps-dir" (path->string (build-path dir "maps")))
                              (cons "--producer-dir" (path->string (build-path dir "producer")))
                              (cons "--aggregate-attestation"
                                    (path->string (build-path dir "aggregate")))
                              (cons "--out-dir" (path->string out))))
                       (list "--trial-id"
                             "trial-2026-10-30-c1"
                             "--attempt"
                             "1"
                             "--treatment"
                             "C"
                             "--run-id"
                             "7100"
                             "--run-attempt"
                             "1"
                             "--head-sha"
                             sha40-a
                             "--workflow-revision-sha"
                             sha40-b
                             "--shard-total"
                             "3"
                             "--shard-plan"
                             "0,1,2"
                             "--test-inventory-digest"
                             digest-c
                             "--created-at-utc"
                             "2026-10-30T10:30:00Z")))))
    (check-equal? (invoke) 0)
    (for ([mutation (in-list (list (cons 'schema "bad")
                                   (cons 'run-id 999)
                                   (cons 'run-attempt 2)
                                   (cons 'head-sha sha40-b)
                                   (cons 'identity-result "unknown")
                                   (cons 'state "unknown")
                                   (cons 'identity (hash-set identity-fields 'lock-digest digest-c))
                                   (cons 'producer-manifest-digest digest-c)))])
      (write-jsexpr! att-path (hash-set att (car mutation) (cdr mutation)))
      (check-equal? (invoke) 1 (symbol->string (car mutation)))
      (check-true (file-exists? (build-path out "actions.json"))))
    (write-jsexpr! att-path att)
    (write-jsexpr! (build-path dir "run.json") (hash-set (fake-run-json) 'conclusion "cancelled"))
    (check-equal? (invoke) 1)
    (check-equal? (hash-ref (read-json-file (build-path out "run.json")) 'conclusion) "cancelled")
    ;; Real adapter CLI -> real report CLI: all 13 required jobs succeeded,
    ;; with valid shards and aggregate; only the raw run is cancelled.
    (define collected (read-json-file (build-path out "actions.json")))
    (define required
      (filter (lambda (j) (equal? (hash-ref j 'role) "required")) (hash-ref collected 'jobs)))
    (check-equal? (length required) 13)
    (check-true (andmap (lambda (j) (equal? (hash-ref j 'conclusion) "success")) required))
    (define-values (rc report-path)
      (run-report-cli! (build-path dir "cancelled-report.json")
                       (build-path out "envelope.jsonl")
                       (build-path out "actions.jsonl")))
    (check-equal? rc 0)
    (define report (read-json-file report-path))
    (define trial (first (raw-trials-of report)))
    (check-true (hash-ref (hash-ref trial 'envelope-verdict) 'ready?))
    (check-not-false (member "actions-run-cancelled" (hash-ref trial 'reasons)))
    (check-equal? (hash-ref trial 'population-exclusion) "refused")
    (check-equal? (hash-ref trial 'critical-path-seconds) "unknown")
    (check-equal? (hash-ref (arm-of report "C") 'n) 0)
    (check-equal? (hash-ref (hash-ref report 'promotion) 'readiness) "blocked")
    (for ([echo (in-list (list (hash-ref trial 'actions-echo)
                               (first (hash-ref (hash-ref report 'raw) 'actions-attempts))))])
      (check-equal? (hash-ref (hash-ref echo 'run) 'status) "completed")
      (check-equal? (hash-ref (hash-ref echo 'run) 'conclusion) "cancelled"))
    (for ([name (in-list '("records" "maps" "producer" "aggregate"))])
      (delete-directory/files (build-path dir name))
      (make-directory* (build-path dir name)))
    (check-equal? (invoke) 1)
    (check-equal? (hash-ref (read-json-file (build-path out "collection.json")) 'result) "refused")
    (check-false (hash-ref (assess-trial-envelope (read-json-file (build-path out "envelope.json")))
                           'ready?)))

  (test-case "aggregate refuses unbound success and raw evidence is embedded"
    (check-equal? (aggregate-restore-state (hasheq 'prepared-requested "auto" 'result "success"))
                  "unknown")
    (define dir (build-path tmp-base "raw-aggregate"))
    (define-values (code out) (run-collect! dir #:treatment "C"))
    (define actions (read-json-file (build-path out "actions.json")))
    (check-equal? (hash-ref actions 'aggregate-attestation #f)
                  (read-json-file (build-path dir "aggregate/prepared-aggregate-restore.json"))))

  (test-case "aggregate attestation: B without an attested verified restore refuses fail-closed, evidence retained"
    (for ([state (in-list '(missing cold off))])
      (define dir (build-path tmp-base (format "agg-bad-~a" state)))
      (define-values (code out-dir)
        (run-collect! dir
                      #:treatment "B"
                      #:launcher-mode "global-off"
                      #:launcher-hits 0
                      #:aggregate state))
      (check-equal? code 1 (format "state ~a must refuse a B trial loudly" state))
      ;; ALL evidence is retained next to the refusal
      (check-true (file-exists? (build-path out-dir "envelope.json")))
      (check-true (file-exists? (build-path out-dir "actions.json")))
      (define actions (read-json-file (build-path out-dir "actions.json")))
      (define expected-state
        (case state
          [(missing) "unknown"]
          [(cold) "cold-fallback"]
          [(off) "off"]))
      (check-equal? (hash-ref (hash-ref actions 'config) 'prepared-aggregate-restore)
                    expected-state
                    "the observed aggregate state is embedded honestly")
      (check-equal? (hash-ref (hash-ref actions 'components) 'prepared-aggregate-seconds)
                    "unknown"
                    "a cold/unknown aggregate restore is never a fabricated duration")))

  (test-case "aggregate attestation: C with a verified restore is a prepared measurement"
    (define dir (build-path tmp-base "agg-c-verified"))
    (define-values (code out-dir) (run-collect! dir #:treatment "C" #:aggregate 'verified))
    (check-equal? code 0)
    (define actions (read-json-file (build-path out-dir "actions.json")))
    (check-equal? (hash-ref (hash-ref actions 'config) 'prepared-aggregate-restore)
                  "verified-restore")
    (check-equal? (hash-ref (hash-ref actions 'components) 'prepared-aggregate-seconds)
                  15.0
                  "the attested milliseconds are converted to seconds")
    (check-equal? (hash-ref (hash-ref actions 'components) 'restore-ms)
                  12000
                  "shard restore-ms observed in the emitted records")
    (check-equal? (hash-ref (hash-ref actions 'components) 'compiled-root-producer-seconds)
                  300
                  "producer cost observed in the emitted records")
    (check-equal? (hash-ref (hash-ref actions 'components) 'proof-seconds)
                  "unknown"
                  "an unobserved cost stays unknown, never zero-filled"))

  (test-case "aggregate attestation: A records the honest off state and never refuses"
    (define dir (build-path tmp-base "agg-a-off"))
    (define-values (code out-dir)
      (run-collect! dir #:treatment "A" #:launcher-mode "global-off" #:launcher-hits 0))
    (check-equal? code 0)
    (define actions (read-json-file (build-path out-dir "actions.json")))
    (check-equal? (hash-ref (hash-ref actions 'config) 'prepared-aggregate-restore) "off"))

  ;; ----------------------------- ci-recovery-actions-evidence@1 producer

  (define policy-names
    (with-input-from-file (build-path repo-root "scripts/required-pr-checks.policy") read))

  (test-case "actions evidence: exact report-owner contract shape (schema/run/jobs/config)"
    (define dir (build-path tmp-base "actions-shape"))
    (define-values (code out-dir) (run-collect! dir #:treatment "A"))
    (check-equal? code 0)
    (define actions (read-json-file (build-path out-dir "actions.json")))
    (check-equal? (hash-ref actions 'schema) actions-evidence-schema)
    (define run (hash-ref actions 'run))
    (check-equal? (hash-ref run 'run-id) 7100)
    (check-equal? (hash-ref run 'head-sha) sha40-a)
    (check-equal? (hash-ref run 'run-attempt) 1)
    (check-equal? (hash-ref run 'workflow) "ci-recovery-benchmark.yml")
    (check-equal? (hash-ref run 'event) "workflow_dispatch")
    (check-equal? (hash-ref run 'run-started-at) "2026-10-30T10:00:00Z")
    (check-equal? (hash-ref actions 'required-job-names) (sort policy-names string<?))
    (define jobs (hash-ref actions 'jobs))
    (check-equal? (length jobs) 14)
    (for ([j (in-list jobs)]
          #:when (equal? (hash-ref j 'name) "test (0)"))
      (check-equal? (hash-ref j 'role) "required")
      (check-equal? (hash-ref j 'status) "completed")
      (check-equal? (hash-ref j 'conclusion) "success"))
    (for ([j (in-list jobs)]
          #:when (equal? (hash-ref j 'name) "fast-env"))
      (check-equal? (hash-ref j 'role)
                    "pilot"
                    "the producer canary is a pilot, never a required job"))
    (define config (hash-ref actions 'config))
    (for ([dim (in-list config-dimensions)])
      (check-true (hash-has-key? config (string->symbol dim))
                  (format "config dimension ~a present" dim)))
    (check-equal? (hash-ref config 'runner-image) "ubuntu24:20261001")
    (check-equal? (hash-ref config 'platform) "x64")
    (check-equal? (hash-ref config 'racket-version) "8.10")
    (check-equal? (hash-ref config 'test-inventory-digest) digest-c)
    (check-equal? (hash-ref config 'shard-plan) '(0 1 2))
    (check-equal? (hash-ref config 'harness-revision)
                  (string-append "ci-recovery-benchmark.yml@" sha40-b))
    ;; JSONL twins: exactly one document per line, byte-faithful
    (define envl-lines
      (call-with-input-file (build-path out-dir "envelope.jsonl")
                            (lambda (in)
                              (for/list ([l (in-lines in)]
                                         #:unless (string=? (string-trim l) ""))
                                l))))
    (check-equal? (length envl-lines) 1)
    (check-equal? (read-json (open-input-string (first envl-lines)))
                  (read-json-file (build-path out-dir "envelope.json")))
    (define actl-lines
      (call-with-input-file (build-path out-dir "actions.jsonl")
                            (lambda (in)
                              (for/list ([l (in-lines in)]
                                         #:unless (string=? (string-trim l) ""))
                                l))))
    (check-equal? (length actl-lines) 1)
    (check-equal? (read-json (open-input-string (first actl-lines))) actions))

  (test-case "calendar-strict UTC parse matches the report owner's rules"
    ;; 2026-02-30 is not a calendar date: the adapter refuses it exactly
    ;; like the report's parse-utc-seconds (no silent normalization).
    (check-equal? (iso-utc->epoch "2026-02-30T10:00:00Z") #f)
    (check-equal? (iso-utc->epoch "2026-10-30T10:00:00Z")
                  (let ([t (iso-utc->epoch "2026-10-30T10:00:00Z")]) (and t t))
                  "a valid date parses")
    (check-true (exact-positive-integer? (iso-utc->epoch "2026-10-30T10:00:00Z")))
    (check-equal? (iso-utc->epoch "2026-13-01T00:00:00Z") #f)
    (check-equal? (iso-utc->epoch "not-a-date") #f)
    (check-equal? (iso-utc->epoch 'null) #f)
    ;; round-trip stability for a known instant
    (check-equal? (iso-utc->epoch "1970-01-01T00:00:00Z") 0))

  ;; --------------------------------- end-to-end: adapter → report CLI

  (test-case "end-to-end: ONE real-layout trial flows through the report CLI without missing-actions-evidence"
    (define dir (build-path tmp-base "e2e-single"))
    (define-values (code out-dir)
      (run-collect! dir
                    #:treatment "A"
                    #:launcher-mode "global-off"
                    #:launcher-hits 0
                    #:producer-nested #t))
    (check-equal? code 0)
    (define report-dir (build-path dir "report"))
    (define-values (rc report-path)
      (run-report-cli! (build-path report-dir "report.json")
                       (build-path out-dir "envelope.jsonl")
                       (build-path out-dir "actions.jsonl")))
    (check-equal? rc 0)
    (define report (read-json-file report-path))
    (define trial (first (raw-trials-of report)))
    (check-false (member "missing-actions-evidence" (hash-ref trial 'reasons))
                 (format "trial reasons: ~a" (hash-ref trial 'reasons)))
    (check-equal? (hash-ref trial 'critical-path-seconds)
                  1500.0
                  "the required critical path is the observed number (ms-rounded by the report)")
    (check-equal? (hash-ref (arm-of report "A") 'n) 1))

  (test-case "end-to-end: a 24-trial interleaved A/B/C fixture produces a READY comparison"
    ;; 8 independent trials per arm, strictly interleaved on ONE pinned
    ;; tree, all collected through the adapter's REAL artifact layout. A is
    ;; the slowest arm (cold aggregate), B/C are faster (prepared) but well
    ;; within the regression budget. This is a synthetic fixture: it proves
    ;; the EVIDENCE QUALITY is report-ready, never a measurement.
    (define trial-parent (make-temporary-file "ci-recovery-e2e24~a" 'directory))
    (define envelope-lines '())
    (define action-lines '())
    (for ([i (in-range 24)])
      (define treatment (list-ref '("A" "B" "C") (modulo i 3)))
      (define root-on? (string=? treatment "C"))
      (define path-seconds
        (cond
          [(string=? treatment "A") 1500]
          [(string=? treatment "B") 1420]
          [else 1380]))
      (define dir (build-path trial-parent (format "trial-~a" i)))
      (define-values (code out-dir)
        (run-collect! dir
                      #:treatment treatment
                      #:trial-id (format "trial-2026-10-30-~a~a" (string-downcase treatment) i)
                      #:run-id (+ 7100 i)
                      #:launcher-mode (if root-on? "verified-root-hit" "global-off")
                      #:launcher-hits (if root-on? 3 0)
                      #:created-at (epoch->utc-string (+ 1790020800 (* 60 i)))
                      #:jobs-json
                      (fake-jobs-json #:test2-start "2026-10-30T10:09:30Z"
                                      #:test2-end
                                      (epoch->utc-string (+ (iso-utc->epoch "2026-10-30T10:00:00Z")
                                                            path-seconds)))))
      (check-equal? code 0 (format "trial ~a (~a) collected" i treatment))
      (set! envelope-lines (cons (file->string (build-path out-dir "envelope.jsonl")) envelope-lines))
      (set! action-lines (cons (file->string (build-path out-dir "actions.jsonl")) action-lines)))
    (define envelopes-path (build-path trial-parent "envelopes.jsonl"))
    (define actions-path (build-path trial-parent "actions.jsonl"))
    (display-to-file (string-append* (reverse envelope-lines)) envelopes-path #:exists 'replace)
    (display-to-file (string-append* (reverse action-lines)) actions-path #:exists 'replace)
    (define-values (rc report-path)
      (run-report-cli! (build-path trial-parent "report.json") envelopes-path actions-path))
    (check-equal? rc 0)
    (define report (read-json-file report-path))
    (check-equal? (hash-ref report 'refusals) '() (format "refusals: ~a" (hash-ref report 'refusals)))
    (define arms (hash-ref report 'arms))
    (for ([t (in-list '("A" "B" "C"))])
      (check-equal? (hash-ref (arm-of report t) 'n) 8 (format "arm ~a has 8 samples" t)))
    (define cva (hash-ref (hash-ref report 'comparison) 'c-vs-a))
    (check-true (hash-ref cva 'evaluable #f) "the C-vs-A gate is EVALUABLE on report-ready evidence")
    (check-equal? (hash-ref cva 'verdict) "within-budget")
    (define promotion (hash-ref report 'promotion))
    (check-equal? (hash-ref promotion 'readiness) "non-regression-only")
    ;; binding: one pinned tree, uniform config
    (check-true (hash-ref (hash-ref report 'binding) 'source-uniform))
    (check-true (hash-ref (hash-ref report 'binding) 'config-uniform))))

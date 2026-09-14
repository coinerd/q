#lang racket/base

;; test-ci-exit-truth.rkt — BUG-0073 exit-truth canaries (wave W0, v1.00.30).
;;
;; @suite ci
;; @speed fast
;; @boundary integration
;;
;; Eight negative canaries plus the genuine-success control, each exercising
;; scripts/ci/verify-result-truth.rkt at the required aggregate boundary.
;; A canary represents a distinct way CI can lie about a green run:
;;
;;   1. failed runner / successful tee   -> aggregate claims clean, runner failed
;;   2. tee failure                      -> per-shard tee_exit failure
;;   3. timeout                          -> timed-out shards never verify clean
;;   4. partial JSON                     -> truncated aggregate must not verify
;;   5. missing shard                    -> declared shard count not present
;;   6. invalid totals                   -> pass+fail+timeout+skip != file_count
;;   7. mismatched run SHA               -> aggregate bound to a different commit
;;   8. successful runner / failing artifact upload -> truth-mismatch verdict
;;   9. genuine success                  -> must verify (control)

(module+ test
  (require rackunit
           racket/base
           racket/file
           racket/list
           racket/port
           racket/system
           racket/string
           json)

  (define repo-root
    (let loop ([d (simplify-path (current-directory))])
      (cond
        [(file-exists? (build-path d "scripts" "ci" "verify-result-truth.rkt")) d]
        [(equal? d (simplify-path (build-path d 'up)))
         (error 'test-ci-exit-truth "repo root not found from ~a" d)]
        [else (loop (simplify-path (build-path d 'up)))])))

  (define verifier (build-path repo-root "scripts" "ci" "verify-result-truth.rkt"))
  (define tmp-root (make-temporary-file "ci-exit-truth-~a" 'directory))

  ;; -- aggregate fixture builder ------------------------------------------------
  (define (shard-json #:artifact [artifact "shard-1"]
                      #:shard [shard 1]
                      #:file-count [file-count 3]
                      #:pass [pass 3]
                      #:fail [fail 0]
                      #:timeout [timeout 0]
                      #:skip [skip 0]
                      #:tee-exit [tee-exit #f])
    (define base
      (hasheq 'artifact
              artifact
              'shard
              shard
              'file_count
              file-count
              'pass
              pass
              'fail
              fail
              'timeout
              timeout
              'skip
              skip))
    (if tee-exit
        (hash-set base 'tee_exit tee-exit)
        base))

  (define (aggregate-json shards
                          #:run-sha [run-sha "0123456789abcdef0123456789abcdef01234567"]
                          #:suite [suite "fast"])
    (hasheq 'schema
            "q.ci.aggregate/1"
            'run_sha
            run-sha
            'suite
            suite
            'shards
            shards
            'file_count
            (for/sum ([s shards]) (hash-ref s 'file_count))
            'pass
            (for/sum ([s shards]) (hash-ref s 'pass))
            'fail
            (for/sum ([s shards]) (hash-ref s 'fail))
            'timeout
            (for/sum ([s shards]) (hash-ref s 'timeout))
            'skip
            (for/sum ([s shards]) (hash-ref s 'skip))
            'wall_clock_seconds
            12.5))

  (define (write-aggregate! name jsexpr)
    (define p (build-path tmp-root (string-append name ".json")))
    (with-output-to-file p #:exists 'replace (lambda () (write-json jsexpr)))
    p)

  (define clean-shard (shard-json))

  ;; run the verifier; returns (values exit-code stdout stderr)
  (define (run-verifier! agg-path
                         #:expect-shards [expect-shards #f]
                         #:expect-sha [expect-sha #f]
                         #:runner-conclusion [runner-conclusion "success"]
                         #:artifact-verdict [artifact-verdict #f]
                         #:artifact-exit [artifact-exit #f])
    (define args
      (append* (list (list "--aggregate" (path->string agg-path))
                     (if expect-shards
                         (list "--expect-shards" (number->string expect-shards))
                         '())
                     (if expect-sha
                         (list "--expect-sha" expect-sha)
                         '())
                     (list "--runner-conclusion" runner-conclusion)
                     (if artifact-verdict
                         (list "--artifact-verdict" artifact-verdict)
                         '())
                     (if artifact-exit
                         (list "--artifact-exit" (number->string artifact-exit))
                         '()))))
    (define so (open-output-string))
    (define se (open-output-string))
    (define ec
      (parameterize ([current-output-port so]
                     [current-error-port se]
                     [current-directory tmp-root])
        (apply system*/exit-code (find-system-path 'exec-file) verifier args)))
    (values ec (get-output-string so) (get-output-string se)))

  ;; -- canary 1: failed runner, successful tee ----------------------------------
  (test-case "canary-1: failed runner with successful tee is rejected"
    (define p (write-aggregate! "canary1" (aggregate-json (list clean-shard))))
    (define-values (ec _so se) (run-verifier! p #:runner-conclusion "failure"))
    (check-not-equal? ec 0 "aggregate claiming clean success must NOT verify when runner failed")
    (check-true (string-contains? se "truth-mismatch") "rejection must be a truth-mismatch verdict"))

  ;; -- canary 2: tee failure ----------------------------------------------------
  ;; Exercises the per-shard tee_exit branch (the tee-truth check), not
  ;; the runner-conclusion branch — a duplicate of canary 1 would leave
  ;; that branch untested.
  (test-case "canary-2: per-shard tee failure makes a clean claim unverifiable"
    (define tee-failed (shard-json #:tee-exit 1))
    (define p (write-aggregate! "canary2" (aggregate-json (list tee-failed))))
    (define-values (ec _so se) (run-verifier! p))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "tee") "rejection must name the tee-failure branch"))

  ;; -- canary 3: timeout --------------------------------------------------------
  (test-case "canary-3: timeout is never a clean success"
    (define timed-out (shard-json #:pass 2 #:timeout 1))
    (define p (write-aggregate! "canary3" (aggregate-json (list timed-out))))
    (define-values (ec _so _se) (run-verifier! p))
    (check-not-equal? ec 0 "aggregate with timeout>0 must not verify as genuine success"))

  ;; -- canary 4: partial (truncated) JSON ---------------------------------------
  (test-case "canary-4: partial JSON is rejected"
    (define p (build-path tmp-root "canary4.json"))
    (define partial
      (string-append "{\"schema\": \"q.ci.aggregate/1\", \"run_sha\": \"abc\", "
                     "\"shards\": [{\"artifact\": \"shard-1\""))
    (with-output-to-file p #:exists 'replace (lambda () (display partial)))
    (define-values (ec _so se) (run-verifier! p))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "structural")))

  ;; -- canary 5: missing shard --------------------------------------------------
  (test-case "canary-5: missing shard is rejected"
    (define p (write-aggregate! "canary5" (aggregate-json (list clean-shard))))
    (define-values (ec _so se) (run-verifier! p #:expect-shards 2))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "missing shard")))

  ;; -- canary 6: invalid totals -------------------------------------------------
  (test-case "canary-6: invalid totals are rejected"
    (define bad (shard-json #:file-count 10 #:pass 3))
    (define p (write-aggregate! "canary6" (aggregate-json (list bad))))
    (define-values (ec _so se) (run-verifier! p))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "totals")))

  ;; -- canary 7: mismatched run SHA ---------------------------------------------
  (test-case "canary-7: mismatched run SHA is rejected"
    (define p (write-aggregate! "canary7" (aggregate-json (list clean-shard))))
    (define-values (ec _so se)
      (run-verifier! p #:expect-sha "ffffffffffffffffffffffffffffffffffffffff"))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "run SHA")))

  ;; -- canary 8: successful runner, failing artifact upload ---------------------
  ;; Exercises the artifact-truth branch via the verifier's
  ;; --artifact-verdict/--artifact-exit flags (a runner-conclusion
  ;; surrogate would test the wrong branch).
  (test-case "canary-8: successful runner with failing artifact verdict is rejected"
    (define p (write-aggregate! "canary8" (aggregate-json (list clean-shard))))
    (define-values (ec _so se) (run-verifier! p #:artifact-verdict "failure" #:artifact-exit 1))
    (check-not-equal? ec 0)
    (check-true (string-contains? se "artifact") "rejection must name the artifact-truth branch"))

  ;; -- canary 9: genuine success -------------------------------------------------
  (test-case "canary-9: genuine success verifies"
    (define p (write-aggregate! "canary9" (aggregate-json (list clean-shard))))
    (define-values (ec so _se) (run-verifier! p))
    (check-equal? ec 0 "a truthful clean aggregate must verify")
    (check-true (string-contains? so "genuine clean success")))

  (test-case "SHA binding: absent aggregate SHA fails with correctly bound shard"
    (define sha "ffffffffffffffffffffffffffffffffffffffff")
    (define agg (aggregate-json (list (hash-set clean-shard 'head_sha sha)) #:run-sha sha))
    (define p (write-aggregate! "missing-aggregate-sha" (hash-remove agg 'run_sha)))
    (define-values (ec _so se) (run-verifier! p #:expect-sha sha))
    (check-equal? ec 3)
    (check-true (string-contains? se "aggregate has no run SHA binding")))

  (test-case "SHA binding: missing and stale shard SHA fail independently"
    (define sha "ffffffffffffffffffffffffffffffffffffffff")
    (for ([shard (in-list (list clean-shard (hash-set clean-shard 'head_sha "stale")))])
      (define p (write-aggregate! "bad-shard-sha" (aggregate-json (list shard) #:run-sha sha)))
      (define-values (ec _so se) (run-verifier! p #:expect-sha sha))
      (check-equal? ec 3)
      (check-true (string-contains? se "shard"))))

  (test-case "SHA binding: fully bound clean aggregate succeeds"
    (define sha "ffffffffffffffffffffffffffffffffffffffff")
    (define p
      (write-aggregate! "bound-success"
                        (aggregate-json (list (hash-set clean-shard 'head_sha sha)) #:run-sha sha)))
    (define-values (ec _so _se) (run-verifier! p #:expect-shards 1 #:expect-sha sha))
    (check-equal? ec 0))

  ;; -- aggregate-boundary wiring (BUG-0073) --------------------------------------
  ;; The verifier exists to gate the ONE place a green claim is minted:
  ;; ci.yml test-aggregate "Produce fast-suite proof bundle".  A verifier
  ;; that nothing calls would be a regex test in disguise.
  (test-case "wiring: ci.yml test-aggregate invokes the result-truth verifier"
    (define ci-yml (file->string (build-path repo-root ".github" "workflows" "ci.yml")))
    (check-true (string-contains? ci-yml "scripts/ci/verify-result-truth.rkt")
                "test-aggregate must invoke scripts/ci/verify-result-truth.rkt")
    (check-true (string-contains? ci-yml "--expect-sha \"$GITHUB_SHA\"")
                "aggregate verification must bind the aggregate to the run SHA")
    (check-true (string-contains? ci-yml "--expect-shards \"$SHARD_EXPECT\"")
                "aggregate verification must assert the matrix-derived shard count")
    (check-true
     (string-contains? ci-yml "SHARD_EXPECT=\"${{ vars.FAST_SHARD_COUNT")
     "the expected shard count must come from the matrix variable, not the downloaded artifacts")
    (check-true
     (string-contains? ci-yml "head_sha: .run_summary.run_sha")
     "every recorded shard must carry its shard-side run SHA (not stamped by the aggregate)")
    (check-true (string-contains? ci-yml "lacks its shard-side run-SHA attestation")
                "the aggregate must refuse a bundle over an unattested shard")
    (define producer-start
      (car (car (regexp-match-positions #rx"Produce fast-suite proof bundle" ci-yml))))
    (define verifier-start
      (car (car (regexp-match-positions #rx"scripts/ci/verify-result-truth[.]rkt" ci-yml))))
    (check-true (< producer-start verifier-start)
                "verifier must run at/after the bundle-producer boundary")
    (check-true (string-contains? ci-yml "Stamp shard run SHA (shard-side attestation)")
                "the fast suite must stamp the run SHA shard-side before upload")))

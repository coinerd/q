#lang racket/base

;; test-ci-shard-verifier.rkt — Phase D shard-aggregate verifier canaries.
;;
;; @suite ci
;; @speed fast
;; @boundary integration
;;
;; Exercises scripts/ci/verify-shard-results.rkt, the verifier that replaced
;; the shell/jq aggregation inside ci.yml#test-aggregate. Fixture shard
;; directories cover the green case plus every violation class the verifier
;; must fail closed on:
;;
;;   1. green aggregate                   -> exit 0, aggregate written
;;   2. missing shard                     -> exit 2
;;   3. extra shard                       -> exit 2
;;   4. duplicate artifact name           -> exit 2
;;   5. duplicate shard index             -> exit 2
;;   6. shard index out of range          -> exit 2
;;   7. invalid document (schema)         -> exit 2
;;   8. unparseable document              -> exit 2
;;   9. files/file_count mismatch         -> exit 2
;;  10. duplicate path inside a shard     -> exit 2
;;  11. duplicate path across shards      -> exit 2
;;  12. per-file status disagreement      -> exit 2
;;  13. invalid per-shard totals          -> exit 2
;;  14. missing run-SHA attestation       -> exit 3
;;  15. stale run-SHA                     -> exit 3
;;  16. zero total tests                  -> exit 1
;;  17. non-clean aggregate (failure)     -> exit 1
;;  18. unaccounted required check        -> exit 4
;;  19. unexpected shard check            -> exit 4
;;  20. workflow wiring: Racket verifier in, jq aggregation out
;;
;; The workflow case reads the live .github/workflows/ci.yml — a verifier
;; that nothing calls would be a unit test in disguise.

(module+ test
  (require json
           rackunit
           racket/base
           racket/file
           racket/list
           racket/path
           racket/port
           racket/string
           racket/system)

  (define repo-root
    (let loop ([d (simplify-path (current-directory))])
      (cond
        [(file-exists? (build-path d "scripts" "ci" "verify-shard-results.rkt")) d]
        [(equal? d (simplify-path (build-path d 'up)))
         (error 'test-ci-shard-verifier "repo root not found from ~a" d)]
        [else (loop (simplify-path (build-path d 'up)))])))

  (define verifier (build-path repo-root "scripts" "ci" "verify-shard-results.rkt"))
  (define ci-yml-path (build-path repo-root ".github" "workflows" "ci.yml"))
  (define policy-path (build-path repo-root "scripts" "required-pr-checks.policy"))

  (define sha "0123456789abcdef0123456789abcdef01234567")
  (define other-sha "ffffffffffffffffffffffffffffffffffffffff")

  ;; ---------------------------------------------------------------------
  ;; Fixture builders
  ;; ---------------------------------------------------------------------

  (define (file-record path status)
    (define exit-code (cond [(equal? status "fail") 1] [(equal? status "timeout") 2] [else 0]))
    (define category (cond
                       [(equal? status "fail") "ASSERTION_FAILURE"]
                       [(equal? status "timeout") "TIMEOUT"]
                       [(equal? status "skip") "SKIPPED_BY_PROFILE"]
                       [else "PASS"]))
    (hasheq 'path
            path
            'category
            category
            'exit_code
            exit-code
            'passed
            (if (equal? status "pass") 2 0)
            'failed
            (if (equal? status "fail") 2 0)
            'total
            (if (equal? status "skip") 0 2)
            'status
            status
            'output
            ""))

  (define (status-counts files)
    (for/fold ([counts (hasheq 'pass 0 'fail 0 'timeout 0 'skip 0)]) ([f (in-list files)])
      (hash-update counts (string->symbol (hash-ref f 'status)) add1)))

  ;; A runner-shaped document: the producer envelope plus the run_summary
  ;; projection the aggregate consumes.
  (define (document index total files
                    #:run-sha [run-sha sha]
                    #:shard-index [shard-index index]
                    #:shard-total [shard-total total]
                    #:file-count [file-count #f]
                    #:pass [pass #f]
                    #:fail [fail #f]
                    #:timeout [timeout #f]
                    #:skip [skip #f]
                    #:wall-clock [wall-clock 10.0])
    (define counts (status-counts files))
    (define run-summary
      (hasheq 'runner_version
              "test-runner"
              'suite
              "fast"
              'profile
              "fast"
              'shard
              (hasheq 'index shard-index 'total shard-total)
              'execution_mode
              "subprocess"
              'file_count
              (or file-count (length files))
              'pass
              (or pass (hash-ref counts 'pass))
              'fail
              (or fail (hash-ref counts 'fail))
              'timeout
              (or timeout (hash-ref counts 'timeout))
              'skip
              (or skip (hash-ref counts 'skip))
              'wall_clock_seconds
              wall-clock
              'run_sha
              run-sha
              'metadata_completeness
              (hasheq 'explicit (length files) 'heuristic 0 'missing 0)))
    (hasheq 'suite
            "fast"
            'mode
            "subprocess"
            'profile
            "fast"
            'verdict
            "pass"
            'elapsed_ms
            1000
            'summary
            (hasheq 'files_total (length files))
            'run_summary
            run-summary
            'files
            files))

  ;; Writes <root>/shard-results/<artifact>/test-results.json for each entry.
  (define (make-fixture! label entries)
    (define root (make-temporary-file (string-append "ci-shard-" label "-~a") 'directory))
    (for ([entry (in-list entries)])
      (define dir (build-path root "shard-results" (car entry)))
      (make-directory* dir)
      (call-with-output-file (build-path dir "test-results.json")
        #:exists
        'replace
        (lambda (out) (write-json (cdr entry) out))))
    root)

  (define (green-entries)
    (list (cons "test-results-fast-0"
                (document 0 2 (list (file-record "tests/a.rkt" "pass")
                                    (file-record "tests/b.rkt" "pass"))
                          #:wall-clock 10.5))
          (cons "test-results-fast-1"
                (document 1 2 (list (file-record "tests/c.rkt" "pass"))
                          #:wall-clock 11.25))))

  ;; ---------------------------------------------------------------------
  ;; Verifier runner
  ;; ---------------------------------------------------------------------

  ;; Runs the verifier with --shard-root shard-results relative to the
  ;; fixture root (exactly how the workflow invokes it) and returns
  ;; (values exit-code stdout stderr).
  (define (run-verifier! root
                         #:expect-shards [expect-shards 2]
                         #:expect-sha [expect-sha-arg sha]
                         #:require-shard-checks [prefix "test"]
                         #:required-checks [required '()]
                         #:aggregate-out [aggregate-out #f]
                         #:summary-out [summary-out #f]
                         #:counts-out [counts-out #f]
                         #:selected-paths-out [selected-paths-out #f])
    (define args
      (append
       (list "--shard-root"
             "shard-results"
             "--expect-shards"
             (number->string expect-shards)
             "--expect-sha"
             expect-sha-arg)
       (if prefix (list "--require-shard-checks" prefix) '())
       (append* (for/list ([name (in-list required)]) (list "--required-check" name)))
       (if aggregate-out (list "--aggregate-out" aggregate-out) '())
       (if summary-out (list "--summary-out" summary-out) '())
       (if counts-out (list "--counts-out" counts-out) '())
       (if selected-paths-out (list "--selected-paths-out" selected-paths-out) '())))
    (define out (open-output-string))
    (define err (open-output-string))
    (define exit-code
      (parameterize ([current-output-port out]
                     [current-error-port err]
                     [current-directory root])
        (apply system*/exit-code (find-system-path 'exec-file) verifier args)))
    (values exit-code (get-output-string out) (get-output-string err)))

  (define (read-json-file path)
    (call-with-input-file path read-json))

  ;; ---------------------------------------------------------------------
  ;; 1. green aggregate (control)
  ;; ---------------------------------------------------------------------

  (test-case "green aggregate verifies and writes byte-stable evidence"
    (define root (make-fixture! "green" (green-entries)))
    (define aggregate-path (build-path root "agg.json"))
    (define summary-path (build-path root "summary.md"))
    (define counts-path (build-path root "counts.env"))
    (define selected-path (build-path root "selected.txt"))
    (define-values (exit-code stdout stderr)
      (run-verifier! root
                     #:aggregate-out (path->string aggregate-path)
                     #:summary-out (path->string summary-path)
                     #:counts-out (path->string counts-path)
                     #:selected-paths-out (path->string selected-path)))
    (check-equal? exit-code 0 stderr)
    (check-true (string-contains? stderr "verdict verified"))

    ;; stdout carries the machine-readable aggregate (same bytes as the file).
    (define stdout-json (read-json (open-input-string stdout)))
    (check-true (hash? stdout-json))
    (check-equal? (file->string aggregate-path) stdout)

    (define aggregate (read-json-file aggregate-path))
    ;; The field names the job's jq reduce produced — no more, no less.
    (check-equal?
     (sort (map symbol->string (hash-keys aggregate)) string<?)
     (list "elapsed_ms_basis"
           "fail"
           "file_count"
           "pass"
           "run_sha"
           "sharded"
           "shards"
           "skip"
           "suite"
           "timeout"
           "wall_clock_seconds"))
    (check-equal? (sort (map symbol->string (hash-keys (first (hash-ref aggregate 'shards))))
                        string<?)
                  (list "artifact"
                        "fail"
                        "file_count"
                        "head_sha"
                        "pass"
                        "shard"
                        "skip"
                        "timeout"
                        "wall_clock_seconds"))
    (check-equal? (hash-ref aggregate 'run_sha) sha)
    (check-equal? (hash-ref aggregate 'suite) "fast")
    (check-equal? (hash-ref aggregate 'sharded) #t)
    (check-equal? (hash-ref aggregate 'file_count) 3)
    (check-equal? (hash-ref aggregate 'pass) 3)
    (check-equal? (hash-ref aggregate 'fail) 0)
    (check-equal? (hash-ref aggregate 'timeout) 0)
    (check-equal? (hash-ref aggregate 'skip) 0)
    (check-equal? (hash-ref aggregate 'wall_clock_seconds) 11.25)
    (check-equal?
     (hash-ref aggregate 'elapsed_ms_basis)
     "max shard wall_clock_seconds; shards execute in parallel so the suite completed when the slowest shard completed")
    (check-equal? (map (lambda (s) (hash-ref s 'artifact)) (hash-ref aggregate 'shards))
                  (list "test-results-fast-0" "test-results-fast-1"))
    (check-equal? (map (lambda (s) (hash-ref s 'shard)) (hash-ref aggregate 'shards))
                  (list (hasheq 'index 0 'total 2) (hasheq 'index 1 'total 2)))
    (check-equal? (map (lambda (s) (hash-ref s 'head_sha)) (hash-ref aggregate 'shards))
                  (list sha sha))

    ;; claim counts: the exact variable names the workflow template consumes.
    (check-equal?
     (file->string counts-path)
     (string-append "FILE_TOTAL=3\n"
                    "PASS_TOTAL=3\n"
                    "FAIL_TOTAL=0\n"
                    "TIMEOUT_TOTAL=0\n"
                    "SKIP_TOTAL=0\n"
                    "WALL_CLOCK_SECONDS=11.25\n"
                    "ELAPSED_MS=11250\n"
                    "SHARD_COUNT=2\n"))

    ;; the selection manifest stays byte-compatible with the jq projection
    ;; it replaced (`jq -s '[.[].files[].path] | unique | .[]'`).
    (check-equal? (file->string selected-path)
                  "\"tests/a.rkt\"\n\"tests/b.rkt\"\n\"tests/c.rkt\"\n")

    ;; the markdown summary is the aggregate step summary.
    (define markdown (file->string summary-path))
    (check-true (string-contains? markdown "test-aggregate"))
    (check-true (string-contains? markdown "Files: 3  pass=3 fail=0 timeout=0 skip=0"))
    (check-true (string-contains? markdown "Tests: 6 total, 6 passed, 0 failed"))
    (check-true (string-contains? markdown "Required checks accounted for: test (0), test (1)")))

  ;; ---------------------------------------------------------------------
  ;; 2-6. shard presence
  ;; ---------------------------------------------------------------------

  (test-case "missing shard is rejected"
    (define root (make-fixture! "missing" (list (first (green-entries)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "missing shard results: expected 2 shards, found 1"))
    (check-true (string-contains? stderr "missing shard index 1")))

  (test-case "extra shard is rejected"
    (define root
      (make-fixture! "extra"
                     (append (green-entries)
                             (list (cons "test-results-fast-2"
                                         (document 2 2
                                                   (list (file-record "tests/d.rkt" "pass"))))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "unexpected shard results: expected 2 shards, found 3")))

  (test-case "duplicate artifact name is rejected"
    (define duplicate
      (document 1 2 (list (file-record "tests/c.rkt" "pass"))))
    (define root
      (make-fixture! "dupe-artifact"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1" (cdr (second (green-entries))))
                           ;; a second download of the same artifact name
                           (cons ".duplicate/test-results-fast-1" duplicate))))
    (define-values (exit-code _stdout stderr) (run-verifier! root #:expect-shards 3))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "duplicate shard result: artifact \"test-results-fast-1\"")))

  (test-case "duplicate shard index is rejected"
    (define root
      (make-fixture! "dupe-index"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 0 2 (list (file-record "tests/c.rkt" "pass")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "duplicate shard index 0")))

  (test-case "shard index outside the expected range is rejected"
    (define root
      (make-fixture! "out-of-range"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 5 6 (list (file-record "tests/c.rkt" "pass")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "shard index 5 reported by test-results-fast-1 is outside the expected range 0..1")))

  (test-case "declared shard total must match the matrix-derived count"
    (define root
      (make-fixture! "shard-total"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 4 (list (file-record "tests/c.rkt" "pass")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "declares shard total 4 but the expected shard count is 2")))

  ;; ---------------------------------------------------------------------
  ;; 7-8. document validity
  ;; ---------------------------------------------------------------------

  (test-case "invalid run-summary document is rejected"
    (define broken (cdr (second (green-entries))))
    (define bad-summary (hash-remove (hash-ref broken 'run_summary) 'runner_version))
    (define root
      (make-fixture! "invalid"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1" (hash-set broken 'run_summary bad-summary)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr
                                  "test-results-fast-1/test-results.json: invalid run-summary document: missing required field runner_version")))

  (test-case "unparseable document is rejected"
    (define root (make-fixture! "unparseable" (green-entries)))
    (define path (build-path root "shard-results" "test-results-fast-1" "test-results.json"))
    (call-with-output-file path #:exists 'replace (lambda (out) (display "{\"suite\": \"fast\"" out)))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "test-results-fast-1/test-results.json: not readable JSON")))

  ;; ---------------------------------------------------------------------
  ;; 9-13. per-file completeness and count consistency
  ;; ---------------------------------------------------------------------

  (test-case "files array must match run_summary.file_count"
    (define root
      (make-fixture! "file-count"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/c.rkt" "pass"))
                                           #:file-count 4)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "files array has 1 entries but run_summary.file_count=4")))

  (test-case "duplicate file path inside a shard is rejected"
    (define root
      (make-fixture! "dupe-path"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/c.rkt" "pass")
                                                     (file-record "tests/c.rkt" "pass")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "shard test-results-fast-1 reports duplicate file path \"tests/c.rkt\" (2 entries)") stderr))

  (test-case "duplicate file path across shards is rejected"
    (define root
      (make-fixture! "dupe-cross"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/a.rkt" "pass")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "file \"tests/a.rkt\" reported by more than one shard")))

  (test-case "per-file statuses must reconcile with the declared counts"
    (define files (list (file-record "tests/c.rkt" "pass")))
    (define root
      (make-fixture! "status-counts"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 files #:pass 0 #:fail 1)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains?
                 stderr
                 "per-file status counts (pass=1 fail=0 timeout=0 skip=0) disagree with run_summary (pass=0 fail=1 timeout=0 skip=0)")))

  (test-case "per-shard totals must add up"
    (define root
      (make-fixture! "bad-totals"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/c.rkt" "pass"))
                                           #:file-count 10)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 2)
    (check-true (string-contains? stderr "shard test-results-fast-1 totals are invalid: pass+fail+timeout+skip=1 but file_count=10") stderr))

  ;; ---------------------------------------------------------------------
  ;; 14-15. run-SHA binding
  ;; ---------------------------------------------------------------------

  (test-case "missing shard run-SHA attestation is rejected"
    (define attested (cdr (second (green-entries))))
    (define root
      (make-fixture! "no-sha"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (hash-set attested
                                           'run_summary
                                           (hash-remove (hash-ref attested 'run_summary) 'run_sha))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 3)
    (check-true (string-contains?
                 stderr
                 "shard test-results-fast-1 lacks its shard-side run-SHA attestation (run_summary.run_sha)")
                stderr))

  (test-case "stale shard run-SHA is rejected"
    (define root
      (make-fixture! "stale-sha"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/c.rkt" "pass"))
                                           #:run-sha other-sha)))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 3)
    (check-true (string-contains? stderr
                                  (format "shard test-results-fast-1 ran at ~a but this run is at ~a"
                                          other-sha
                                          sha))))

  ;; ---------------------------------------------------------------------
  ;; 16-17. non-clean aggregates
  ;; ---------------------------------------------------------------------

  (test-case "zero total tests is a failure"
    ;; Files that ran but parsed no tests: every record reports total=0, so the
    ;; aggregate is clean (no fail/timeout/skip) yet executed nothing.
    (define (zero-test-record path)
      (hash-set (file-record path "pass") 'total 0))
    (define root
      (make-fixture! "zero-tests"
                     (list (cons "test-results-fast-0"
                                 (document 0 2 (list (zero-test-record "tests/a.rkt")))
                                 )
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (zero-test-record "tests/c.rkt")))))))
    (define-values (exit-code _stdout stderr) (run-verifier! root))
    (check-equal? exit-code 1 stderr)
    (check-true (string-contains? stderr "aggregate reports zero total tests") stderr))

  (test-case "a non-clean aggregate refuses to produce evidence"
    (define root
      (make-fixture! "non-clean"
                     (list (cons "test-results-fast-0" (cdr (first (green-entries))))
                           (cons "test-results-fast-1"
                                 (document 1 2 (list (file-record "tests/c.rkt" "fail")))))))
    (define-values (exit-code stdout stderr)
      (run-verifier! root
                     #:aggregate-out (path->string (build-path root "agg.json"))))
    (check-equal? exit-code 1)
    (check-true (string-contains?
                 stderr
                 "refusing to write a proof bundle over a non-clean aggregate (files=3 fail=1 timeout=0 skip=0)"))
    ;; a rejected run writes no evidence at all
    (check-false (file-exists? (build-path root "agg.json")))
    (check-equal? stdout ""))

  ;; ---------------------------------------------------------------------
  ;; 18-19. required-check accounting
  ;; ---------------------------------------------------------------------

  (test-case "an unaccounted required check is rejected"
    (define root (make-fixture! "missing-check" (green-entries)))
    (define-values (exit-code _stdout stderr)
      (run-verifier! root
                     #:require-shard-checks #f
                     #:required-checks (list "test (0)" "test (7)")))
    (check-equal? exit-code 4 stderr)
    (check-true (string-contains?
                 stderr
                 "required check \"test (7)\" is not accounted for by any shard result (observed: test (0), test (1))")
                stderr))

  (test-case "a shard that maps to no required check is rejected"
    (define root (make-fixture! "unexpected-check" (green-entries)))
    (define-values (exit-code _stdout stderr)
      (run-verifier! root #:require-shard-checks #f #:required-checks (list "test (0)")))
    (check-equal? exit-code 4 stderr)
    (check-true (string-contains?
                 stderr
                 "shard test-results-fast-1 maps to check \"test (1)\" which is not in the expected required-check list (test (0))")
                stderr))

  ;; ---------------------------------------------------------------------
  ;; 20. workflow wiring: the Racket verifier is called, the jq reduce is gone
  ;; ---------------------------------------------------------------------

  (define (job-section text job-name)
    (define lines (string-split text "\n"))
    (define header (string-append "  " job-name ":"))
    (define start (index-of lines header))
    (and start
         (let loop ([rest (list-tail lines (add1 start))] [acc '()])
           (cond
             [(null? rest) (string-join (reverse acc) "\n")]
             [(regexp-match? #rx"^  [a-zA-Z][a-zA-Z0-9_-]*:\\s*$" (car rest))
              (string-join (reverse acc) "\n")]
             [else (loop (cdr rest) (cons (car rest) acc))]))))

  (test-case "ci.yml test-aggregate invokes the Racket shard verifier"
    (define section (job-section (file->string ci-yml-path) "test-aggregate"))
    (check-true (string-contains? section "racket scripts/ci/verify-shard-results.rkt")
                "test-aggregate must invoke scripts/ci/verify-shard-results.rkt")
    (check-true (string-contains? section "--shard-root shard-results"))
    (check-true (string-contains? section "--expect-shards \"$SHARD_EXPECT\"")
                "the verifier must bind the matrix-derived shard count")
    (check-true (string-contains? section "--expect-sha \"$GITHUB_SHA\"")
                "the verifier must bind the run SHA")
    (check-true (string-contains? section "--require-shard-checks test")
                "the required check names must be derived from the matrix identity")
    (check-true (string-contains? section "--aggregate-out artifacts-run/fast-suite-results-summary.json")
                "the verifier must write the aggregate document the claim digests")
    (check-true (string-contains? section "source artifacts-run/aggregate-counts.env")
                "claim counts come from the verifier, not a shell reduce")
    ;; The aggregated evidence still flows into the truth verifier and the
    ;; claim exactly as before.
    (check-true (string-contains? section "scripts/ci/verify-result-truth.rkt"))
    (check-true (string-contains? section "--aggregate \"$SUMMARY\""))
    (check-true (string-contains? section "SHARD_EXPECT=\"${{ vars.FAST_SHARD_COUNT"))
    (check-true (string-contains? section "test-results-fast-*"))
    (check-true (string-contains? section "proof-bundle-fast"))

    ;; no jq aggregation survives in the job.
    (check-false (string-contains? section "shards.jsonl")
                 "the per-shard jq projection must be gone")
    (check-false (string-contains? section "jq -s")
                 "the jq reduce must be gone")
    (check-false (string-contains? section "jq -c --arg artifact")
                 "the per-shard jq projection must be gone")
    (check-false (string-contains? section "head_sha: .run_summary.run_sha")
                 "run-SHA binding now lives in the verifier")
    (check-false (string-contains? section "elapsed_ms_basis: \"max shard")
                 "the aggregate document is produced by the verifier")

    ;; the claim/argv binding is preserved, repointed to the qt entry point
    ;; CI actually invokes (§11.2).
    (check-true
     (string-contains? section "RUNNER_REV=\"$(git rev-parse \"$GITHUB_SHA\":qt)\""))
    (check-true
     (string-contains?
      section
      (string-append "\"argv\": [\"./qt\", \"--lane\", \"fast\", "
                     "\"--jobs\", \"4\", \"--shard-index\", \"<shard>\", \"--shard-total\", "
                     "\"<total>\", \"--json-out\", \"test-results.json\"]")))

    ;; required-check identity unchanged.
    (define policy (call-with-input-file policy-path read))
    (check-true (and (member "test-aggregate" policy) #t)
                "the required check name for this job is unchanged"))

  (test-case "the shard verifier keeps the aggregate field names and refusal wording"
    (define source (file->string verifier))
    ;; Field names are emitted from ordered pairs, so assert the symbols.
    (define expected-field-forms
      (list "(cons 'run_sha"
            "(cons 'suite"
            "(cons 'sharded"
            "(cons 'shards"
            "(cons 'file_count"
            "(cons 'pass"
            "(cons 'fail"
            "(cons 'timeout"
            "(cons 'skip"
            "(cons 'wall_clock_seconds"
            "(cons 'elapsed_ms_basis"))
    (for ([field (in-list expected-field-forms)])
      (check-true (string-contains? source field) field))
    (check-true (string-contains? source
                                  "refusing to write a proof bundle over a non-clean aggregate")
                "the producer's non-clean refusal wording is preserved")))

#lang racket/base

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration  ;; @mutates fs

(require rackunit
         rackunit/text-ui
         json
         racket/string
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         "../scripts/run-tests.rkt"
         (only-in "../scripts/run-tests/reporting.rkt" validate-run-summary-json))

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define (result #:path [path "tests/example.rkt"]
                #:exit [exit-code 0]
                #:out [out ""]
                #:err [err ""]
                #:passed [passed 0]
                #:failed [failed 0]
                #:total [total 0])
  (make-test-file-result path
                         exit-code
                         (string->bytes/utf-8 out)
                         (string->bytes/utf-8 err)
                         17
                         passed
                         failed
                         total))

(define (write-temp-test content)
  (define dir (make-temporary-file "q-json-out-test-~a" 'directory))
  (define file (build-path dir "test-json-out.rkt"))
  (call-with-output-file file #:exists 'replace (lambda (out) (display content out)))
  (values file dir))

(define (delete-dir/safe dir)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (delete-directory/files dir)))

(define (run/capture cmd)
  (parameterize ([current-directory project-root])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system/exit-code cmd)))
    (values code (get-output-string out) (get-output-string err))))

(define runner-module `(file ,(path->string (build-path project-root "scripts/run-tests.rkt"))))

(define classify-test-result*
  (dynamic-require runner-module
                   'classify-test-result
                   (lambda () (lambda (_) (error 'classify-test-result "missing export")))))

(define test-result->jsexpr*
  (dynamic-require runner-module
                   'test-result->jsexpr
                   (lambda () (lambda (_) (error 'test-result->jsexpr "missing export")))))

(define write-json-results!*
  (dynamic-require runner-module
                   'write-json-results!
                   (lambda () (lambda args (error 'write-json-results! "missing export")))))

(define suite
  (test-suite "run-tests structured classification + json"

    (test-case "parse-args accepts --json-out"
      (define-values (_jobs
                      _seq?
                      _timeout
                      _strict?
                      _suite
                      _extra
                      _repeat
                      _record?
                      _inventory?
                      _diagnose?
                      _mode
                      _scheduler
                      json-out
                      _ledger
                      _profile
                      _lint-metadata?
                      _changed-base
                      _changed-head
                      _explain?
                      _impact-dry-run?
                      _prioritize
                      _failure-history
                      _generate-covers-manifest?
                      _shard-plan
                      _durations
                      _ordering)
        (parse-args '("--json-out" "/tmp/q-results.json")))
      (check-equal? json-out "/tmp/q-results.json"))

    (test-case "classify-test-result distinguishes core categories"
      (check-equal? (classify-test-result* (result #:exit 0 #:passed 1 #:total 1)) 'PASS)
      (check-equal? (classify-test-result* (result #:exit 0 #:total 0)) 'ZERO_PARSED)
      (check-equal? (classify-test-result* (result #:exit 2 #:err "timeout after 1s")) 'TIMEOUT)
      (check-equal? (classify-test-result* (result #:exit 1 #:failed 1 #:total 1 #:out "FAILURE"))
                    'ASSERTION_FAILURE)
      (check-equal? (classify-test-result* (result #:exit 1 #:err "read-syntax: expected a )"))
                    'COMPILE_FAILURE)
      (check-equal? (classify-test-result*
                     (result #:exit 1 #:err "standard-module-name-resolver: collection not found"))
                    'MODULE_LOAD_FAILURE)
      (check-equal? (classify-test-result* (result #:exit 1
                                                   #:err "missing environment variable API_KEY"))
                    'ENVIRONMENT_MISSING)
      (check-equal? (classify-test-result* (result #:exit 1 #:err "user break")) 'USER_BREAK))

    (test-case "test-result->jsexpr includes category and counts"
      (define js
        (test-result->jsexpr*
         (result #:path "tests/failing.rkt" #:exit 1 #:failed 1 #:total 1 #:out "FAILURE")))
      (check-equal? (hash-ref js 'path) "tests/failing.rkt")
      (check-equal? (hash-ref js 'category) "ASSERTION_FAILURE")
      (check-equal? (hash-ref js 'exit_code) 1)
      (check-equal? (hash-ref js 'total) 1))

    (test-case "write-json-results! writes per-file categories"
      (define out (make-temporary-file "q-results-~a.json"))
      (dynamic-wind
       void
       (lambda ()
         (write-json-results!* out
                               (list (result #:path "tests/pass.rkt" #:passed 1 #:total 1)
                                     (result #:path "tests/zero.rkt" #:total 0))
                               #:suite 'unit-fast
                               #:mode 'in-process
                               #:elapsed-ms 25)
         (define js (call-with-input-file out read-json))
         (check-equal? (hash-ref js 'suite) "unit-fast")
         (check-equal? (hash-ref js 'mode) "in-process")
         (define files (hash-ref js 'files))
         (check-equal? (length files) 2)
         (check-equal? (hash-ref (car files) 'category) "PASS")
         (check-equal? (hash-ref (cadr files) 'category) "ZERO_PARSED"))
       (lambda ()
         (when (file-exists? out)
           (delete-file out)))))

    (test-case "CLI writes --json-out for explicit passing file"
      (define-values (file dir)
        (write-temp-test
         (string-append "#lang racket/base\n"
                        "(require rackunit rackunit/text-ui)\n"
                        "(define s (test-suite \"json\" (test-case \"ok\" (check-true #t))))\n"
                        "(run-tests s)\n")))
      (define out (build-path dir "results.json"))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture
            (format "racket scripts/run-tests.rkt --mode subprocess --json-out ~a ~a" out file)))
         (check-equal? code 0 stderr)
         (check-true (regexp-match? #rx"Category" stdout))
         (define js (call-with-input-file out read-json))
         (check-equal? (hash-ref js 'verdict) "pass")
         (define file-js (car (hash-ref js 'files)))
         (check-equal? (hash-ref file-js 'category) "PASS")
         (check-equal? (hash-ref file-js 'total) 1))
       (lambda () (delete-dir/safe dir))))))

(define w1-fixture-suite
  (test-suite "run-summary JSON W1 fixtures"
    (test-case "W1 fixture: old-schema artifact still validates"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.22\",\"suite\":\"fast\",\"profile\":\"local\","
                     "\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}]}"))))
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: new-schema artifact with full telemetry validates"
      (define artifact
        (read-json
         (open-input-string
          (string-append
           "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\",\"profile\":\"local\","
           "\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,"
           "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,"
           "\"shard\":{\"index\":0,\"total\":1,\"duration_ms\":12},"
           "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
           "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
           "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
           "\"extra\":{\"scheduler\":{\"schema_version\":1,\"scheduler_mode\":\"batch\","
           "\"worker_count\":4,\"queue_wait_ms\":3,\"worker_busy_ms\":400,"
           "\"worker_idle_ms\":50,\"serial_partition_ms\":200,\"parallel_partition_ms\":600,"
           "\"process_start_count\":5,\"grouped_subprocess_count\":2,\"gc_count\":3,"
           "\"gc_pause_ms\":12,\"selection_ms\":5,\"execution_ms\":610,\"first_batch_ms\":300},"
           "\"prepared_environment\":{\"result\":\"restored\",\"restore_ms\":120,"
           "\"fallback_ms\":null}}}"))))
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: missing optional telemetry never a problem"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\",\"profile\":\"local\","
                     "\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],\"extra\":{}}"))))
      (define bare artifact)
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: missing optional telemetry never a problem (partial)"
      (define artifact
        (read-json
         (open-input-string
          (string-append
           "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\",\"profile\":\"local\","
           "\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,"
           "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
           "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
           "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
           "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
           "\"extra\":{\"scheduler\":{\"schema_version\":1,\"scheduler_mode\":\"batch\"}}}"))))
      (define partial-artifact artifact)
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: empty prepared_environment object validates (raw envelope)"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{}}}"))))
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: partial prepared_environment with only restore_ms validates (raw envelope)"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{\"restore_ms\":120}}}"))))
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: partial prepared_environment with only result validates (raw envelope)"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{\"result\":\"rebuilt\"}}}"))))
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: empty prepared_environment object validates (flattened)"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{}}}"))))
      ;; Flattened: no run_summary key -- fields at top level
      (check-equal? (validate-run-summary-json artifact) '()))

    (test-case "W1 fixture: wrong type in prepared_environment still rejected"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{\"restore_ms\":\"slow\"}}}"))))
      (define problems (validate-run-summary-json artifact))
      (check-pred pair? problems)
      (check-not-false (ormap (lambda (p) (regexp-match "restore_ms: expected number or null" p))
                              problems)))

    (test-case "W1 fixture: wrong type for result in prepared_environment still rejected"
      (define artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\","
                     "\"profile\":\"local\",\"execution_mode\":\"subprocess\","
                     "\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":\"PASS\","
                     "\"exit_code\":0,\"passed\":1,\"failed\":0,\"total\":1}],"
                     "\"extra\":{\"prepared_environment\":{\"result\":42}}}"))))
      (define problems (validate-run-summary-json artifact))
      (check-pred pair? problems)
      (check-not-false (ormap (lambda (p) (regexp-match "prepared_environment.result: expected" p))
                              problems)))

    (test-case "W1 fixture: malformed required result fields produce problems"
      (define malformed-artifact
        (read-json (open-input-string
                    (string-append
                     "{\"runner_version\":\"1.0.23\",\"suite\":\"fast\",\"profile\":\"local\","
                     "\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,"
                     "\"timeout\":0,\"skip\":0,\"wall_clock_seconds\":0.5,\"shard\":null,"
                     "\"metadata_completeness\":{\"explicit\":0,\"heuristic\":0,\"missing\":0},"
                     "\"files\":[{\"path\":\"tests/example.rkt\",\"category\":42,\"exit_code\":0,"
                     "\"passed\":1,\"failed\":0,\"total\":1}]}"))))
      (define problems (validate-run-summary-json malformed-artifact))
      (check-pred pair? problems)
      (check-not-false (ormap (lambda (p) (regexp-match "files\\[0\\].category: expected a string" p))
                              problems)))))

(define queue-suite
  (test-suite "queue JSON validation"
    (test-case "queue producer round trip and scheduler nullability"
      (define-values (file dir)
        (write-temp-test
         (string-append "#lang racket/base\n"
                        "(require rackunit rackunit/text-ui)\n"
                        "(run-tests (test-suite \"ok\" (test-case \"ok\" (check-true #t))))\n")))
      (define out (build-path dir "results.json"))
      (dynamic-wind
       void
       (lambda ()
         (define-values (code stdout stderr)
           (run/capture
            (format "racket scripts/run-tests.rkt --scheduler queue --json-out ~a ~a" out file)))
         (check-equal? code 0 stderr)
         (define payload (call-with-input-file out read-json))
         (define sched (hash-ref (hash-ref payload 'extra) 'scheduler))
         (define artifact
           (hash-set* (hash-ref payload 'run_summary)
                      'files
                      (hash-ref payload 'files)
                      'extra
                      (hash-ref payload 'extra)))
         (define nullable
           '(queue_wait_ms worker_busy_ms worker_idle_ms serial_partition_ms parallel_partition_ms))
         (check-equal? (validate-run-summary-json payload) '())
         (for ([bad (list 'null #f 42 "bad" '())])
           (check-pred pair? (validate-run-summary-json bad))
           (check-pred pair? (validate-run-summary-json (hash-set payload 'extra bad)))
           (check-pred pair? (validate-run-summary-json (hash-set artifact 'extra bad)))
           (check-pred pair? (validate-run-summary-json (hash-set payload 'run_summary bad))))
         (check-pred pair? (validate-run-summary-json (hash-remove payload 'files)))
         (check-pred pair? (validate-run-summary-json (hash-set payload 'files 'null)))
         (check-pred pair? (validate-run-summary-json (hash-set payload 'run_summary (hasheq))))
         (check-equal? (hash-ref sched 'scheduler_mode) "queue")
         (for ([key (in-list nullable)])
           (check-equal? (hash-ref sched key) (json-null)))
         (check-equal? (validate-run-summary-json artifact) '())
         (define (with-scheduler s)
           (hash-set payload 'extra (hash-set (hash-ref payload 'extra) 'scheduler s)))
         (define numeric-sched
           (for/fold ([s sched]) ([key (in-list nullable)])
             (hash-set s key 42)))
         (for* ([base (in-list (list payload artifact))]
                [mode (in-list (list "unknown" "" "QUEUE"))])
           (define bad-sched (hash-set numeric-sched (quote scheduler_mode) mode))
           (define bad-extra (hash-set (hash-ref base (quote extra)) (quote scheduler) bad-sched))
           (check-not-false (ormap (lambda (p) (regexp-match? #rx"extra.scheduler.scheduler_mode" p))
                                   (validate-run-summary-json
                                    (hash-set base (quote extra) bad-extra)))))
         (check-equal? (validate-run-summary-json
                        (with-scheduler (hash-set numeric-sched 'scheduler_mode "batch")))
                       '())
         (check-equal? (validate-run-summary-json (with-scheduler (hash-remove numeric-sched
                                                                               'scheduler_mode)))
                       '())
         (for ([key (in-list nullable)])
           (check-equal? (validate-run-summary-json (with-scheduler (hash-set sched key 42))) '())
           (check-pred pair? (validate-run-summary-json (with-scheduler (hash-set sched key "bad"))))
           (check-pred pair?
                       (validate-run-summary-json
                        (with-scheduler (hash-set (hash-set numeric-sched 'scheduler_mode "batch")
                                                  key
                                                  (json-null))))))
         (for ([key '(worker_count process_start_count gc_count first_batch_ms)])
           (check-pred pair?
                       (validate-run-summary-json (with-scheduler (hash-set sched key (json-null))))))
         (check-pred pair?
                     (validate-run-summary-json
                      (with-scheduler (hash-set sched 'scheduler_mode (json-null)))))
         (check-pred pair?
                     (validate-run-summary-json (with-scheduler (hash-remove sched
                                                                             'scheduler_mode)))))
       (lambda () (delete-dir/safe dir))))))

(define failures (+ (run-tests suite) (run-tests w1-fixture-suite) (run-tests queue-suite)))
(exit failures)

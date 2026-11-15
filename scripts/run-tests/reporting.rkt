#lang racket/base

;; q/scripts/run-tests/reporting.rkt — Result ownership: parsing, statuses,
;; summaries, JSON evidence, known-failure ledger, gate evidence.
;;
;; Print-summary, save-failure-logs, format-duration, summary-exit-code.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; W8: per-file distinct statuses (pass/fail/timeout/skip), profile+mode on
;; every file/failure record, timeout listing, quarantine-expiry escalation.
;; Phase C fold (§11.3): absorbs the former parse.rkt (result struct, raco
;; output parsing, classification, JSON projection), ledger.rkt (known-failure
;; ledger), gate-evidence.rkt (gate evidence recording), and the
;; profile-skip result constructor from profiles.rkt. Those modules were small
;; result-shaped owners; one results module keeps every `require` cheap and
;; leaves no re-export shims behind.
;; STABILITY: internal

(require racket/string
         racket/file
         racket/path
         racket/list
         racket/match
         racket/date
         racket/port
         racket/system
         json
         (only-in "classify.rkt"
                  base-dir
                  get-file-metadata
                  skipped-requirements
                  normalize-requirement))

(provide print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         make-unique-log-name
         compute-verdict
         classify-test-result
         test-result->jsexpr
         write-json-results!
         print-ledger-summary
         result-status
         format-verdict-line
         metadata-completeness
         run-summary->jsexpr
         format-run-summary-record
         print-run-summary-record
         test-result->ledger-jsexpr
         validate-run-summary-json
         ;; ---- folded parse.rkt ----
         profile-skipped-result
         profile-skipped-result?
         test-file-result
         test-file-result?
         test-file-result-path
         test-file-result-exit-code
         test-file-result-stdout-bytes
         test-file-result-stderr-bytes
         test-file-result-elapsed-ms
         test-file-result-passed
         test-file-result-failed
         test-file-result-total
         make-test-file-result
         bytes->string*
         parse-raco-output
         normalize-counts
         effective-exit-code
         extract-failure-lines
         truncate-test-output
         DEFAULT-OUTPUT-CAP
         ;; ---- folded profiles.rkt (result-shaped half) ----
         skipped-result-exit-code
         make-skipped-result
         ;; ---- folded ledger.rkt ----
         load-known-failure-ledger
         normalize-ledger-entry
         summarize-ledger-results
         ledger-summary-counts
         ledger-entry-matches-result?
         ledger-entry-expired?
         valid-expires-on?
         today-iso8601
         ;; ---- folded gate-evidence.rkt ----
         record-gate-evidence!
         validate-gate-evidence!
         gate-evidence-refusal
         strict-sha-valid?)

(define (format-duration ms)
  (define total-secs (/ ms 1000.0))
  (cond
    [(>= ms 3600000)
     (define hours (quotient ms 3600000))
     (define remaining (- ms (* hours 3600000)))
     (define mins (quotient remaining 60000))
     (define secs (/ (- remaining (* mins 60000)) 1000.0))
     (format "~ah ~am ~as" hours mins secs)]
    [(>= ms 60000)
     (define mins (quotient ms 60000))
     (define secs (/ (- ms (* mins 60000)) 1000.0))
     (format "~am ~as" mins secs)]
    [else (format "~as" total-secs)]))

(define (summary-exit-code failure-count timeout-count)
  (cond
    [(and (> failure-count 0) (> timeout-count 0)) 3]
    [(> failure-count 0) 1]
    [(> timeout-count 0) 2]
    [else 0]))

(define (skipped-by-profile-result? r)
  (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))

(define (passed-result? r)
  (and (= (test-file-result-exit-code r) 0) (not (skipped-by-profile-result? r))))

(define (failed-result? r)
  (and (not (skipped-by-profile-result? r))
       (not (= (test-file-result-exit-code r) 0))
       (not (= (test-file-result-exit-code r) 2))))

(define (timeout-result? r)
  (and (not (skipped-by-profile-result? r)) (= (test-file-result-exit-code r) 2)))

;; W8: distinct per-file statuses for JSON evidence. Timeout and profile-skip
;; must never fold into "pass" — the full-regression workflow aggregates
;; exactly these four values when it computes run status.
(define (result-status r)
  (cond
    [(skipped-by-profile-result? r) "skip"]
    [(timeout-result? r) "timeout"]
    [(failed-result? r) "fail"]
    [else "pass"]))

;; Compute a verdict symbol from results: 'pass, 'fail, 'incomplete, 'inconclusive.
;; - 'fail:          at least one file exited non-zero (not timeout)
;; - 'incomplete:    at least one file timed out (exit-code 2)
;; - 'inconclusive:  all files passed but zero tests were parsed
;; - 'pass:          all files passed and at least one test was parsed
(define (compute-verdict results)
  (define failed-files (count failed-result? results))
  (define timeout-files (count timeout-result? results))
  (define skipped-files (count skipped-by-profile-result? results))
  (define runnable-results (filter (lambda (r) (not (skipped-by-profile-result? r))) results))
  (define total-tests (for/sum ([r (in-list runnable-results)]) (test-file-result-total r)))
  (cond
    [(> failed-files 0) 'fail]
    [(> timeout-files 0) 'incomplete]
    [(and (null? runnable-results) (= skipped-files 0)) 'inconclusive]
    [(and (pair? runnable-results) (= total-tests 0)) 'inconclusive]
    [(and (null? runnable-results) (> skipped-files 0)) 'pass]
    [else 'pass]))

(define (format-verdict-line verdict timeout-count)
  (case verdict
    [(fail) "  VERDICT:   ❌ FAIL"]
    [(incomplete)
     (format "  VERDICT:   ⏱ INCOMPLETE (~a timeout~a)"
             timeout-count
             (if (= timeout-count 1) "" "s"))]
    [(inconclusive) "  VERDICT:   ⚠ INCONCLUSIVE (0 tests parsed)"]
    [(pass) "  VERDICT:   ✅ PASS"]
    [else "  VERDICT:   ❓ UNKNOWN"]))

(define known-result-categories
  '(PASS SKIPPED_BY_PROFILE
         ASSERTION_FAILURE
         MODULE_LOAD_FAILURE
         COMPILE_FAILURE
         TIMEOUT
         ENVIRONMENT_MISSING
         ZERO_PARSED
         USER_BREAK
         UNKNOWN_FAILURE))

(define (category-counts results)
  (define counts (make-hasheq))
  (for ([category (in-list known-result-categories)])
    (hash-set! counts category 0))
  (for ([r (in-list results)])
    (define category (classify-test-result r))
    (hash-set! counts category (add1 (hash-ref counts category 0))))
  counts)

(define (print-category-summary results)
  (define counts (category-counts results))
  (define non-zero
    (filter (lambda (category) (> (hash-ref counts category 0) 0)) known-result-categories))
  (when (pair? non-zero)
    (printf "  Category:  ~a~n"
            (string-join (for/list ([category (in-list non-zero)])
                           (format "~a=~a" category (hash-ref counts category)))
                         ", "))))

(define (category-counts->jsexpr counts)
  (for/hasheq ([category (in-list known-result-categories)]
               #:when (> (hash-ref counts category 0) 0))
    (values (string->symbol (symbol->string category)) (hash-ref counts category))))

(define (ledger-match ledger r)
  (and ledger
       (for/first ([entry (in-list ledger)]
                   #:when (ledger-entry-matches-result? entry r))
         entry)))

(define (test-result->ledger-jsexpr r ledger)
  (define base (test-result->jsexpr r))
  (define entry (ledger-match ledger r))
  (cond
    [(not entry) (hash-set base 'known_failure #f)]
    ;; W8: an expired quarantine is a failure that must escalate — flag it
    ;; per-file so per-shard JSON evidence cannot read as a tolerated known.
    [(ledger-entry-expired? entry)
     (hash-set* base
                'known_failure
                #t
                'issue
                (hash-ref entry 'issue)
                'owner
                (hash-ref entry 'owner)
                'release_blocking
                (hash-ref entry 'release_blocking)
                'expires_on
                (hash-ref entry 'expires_on #f)
                'quarantine_expired
                #t
                'escalate
                #t)]
    [else
     (hash-set* base
                'known_failure
                #t
                'issue
                (hash-ref entry 'issue)
                'owner
                (hash-ref entry 'owner)
                'release_blocking
                (hash-ref entry 'release_blocking))]))

;; ============================================================
;; Run-summary record (W0 baseline measurement)
;; ============================================================
;; Stable schema for CI-retained baselines. Field names use the file's
;; snake_case convention: runner-version -> runner_version, etc.

(define known-area-prefixes
  '("tests/tui/" "tests/workflows/"
                 "tests/security/"
                 "tests/arch/"
                 "tests/runtime/"
                 "tests/extensions/"
                 "tests/interfaces/"
                 "tests/provider/"
                 "tests/skills/"))

(define (path-area-heuristic? path)
  (define p
    (if (path? path)
        (path->string path)
        path))
  (ormap (lambda (prefix) (string-prefix? p prefix)) known-area-prefixes))

;; 'explicit  — file declares @suite (or at least @speed) metadata
;; 'heuristic — no suite metadata, but a path/area classifier still applies
;; 'missing   — no metadata and no area heuristic (filename pattern only)
(define (metadata-completeness path)
  (define m (get-file-metadata path))
  (cond
    [(or (hash-ref m 'suite #f) (hash-ref m 'speed #f)) 'explicit]
    [(path-area-heuristic? path) 'heuristic]
    [else 'missing]))

(define (metadata-completeness-counts results)
  (define explicit 0)
  (define heuristic 0)
  (define missing 0)
  (for ([r (in-list results)])
    (case (metadata-completeness (test-file-result-path r))
      [(explicit) (set! explicit (add1 explicit))]
      [(heuristic) (set! heuristic (add1 heuristic))]
      [else (set! missing (add1 missing))]))
  (hasheq 'explicit explicit 'heuristic heuristic 'missing missing))

(define (optional-id-string v)
  (cond
    [(not v) #f]
    [(symbol? v) (symbol->string v)]
    [(string? v) v]
    [else (format "~a" v)]))

;; W8: every file record (and therefore every failure record) carries a
;; distinct `status` plus the execution profile and runner mode, so shard JSON
;; artifacts are interpretable without the workflow context that produced them.
(define (annotate-file-jsexpr js
                              r
                              #:profile [profile #f]
                              #:mode [mode #f]
                              #:execution-mode [execution-mode #f])
  (define base
    (hash-set* js
               'status
               (result-status r)
               'duration_seconds
               (/ (test-file-result-elapsed-ms r) 1000.0)
               'metadata_completeness
               (symbol->string (metadata-completeness (test-file-result-path r)))))
  (define with-profile
    (let ([s (optional-id-string profile)])
      (if s
          (hash-set base 'execution_profile s)
          base)))
  (define with-mode
    (let ([s (optional-id-string mode)])
      (if s
          (hash-set with-profile 'runner_mode s)
          with-profile)))
  ;; Actual per-file execution mode (schema-additive). Every surviving mode
  ;; runs one subprocess per file, so the value is `subprocess`.
  (if execution-mode
      (hash-set with-mode 'execution_mode (symbol->string execution-mode))
      with-mode))

(define (shard->jsexpr shard)
  (if (pair? shard)
      (hasheq 'index (car shard) 'total (cdr shard))
      'null))

(define (run-summary->jsexpr results
                             #:suite suite
                             #:profile [profile 'local]
                             #:shard [shard #f]
                             #:mode [mode 'subprocess]
                             #:elapsed-ms [elapsed-ms 0]
                             #:runner-version [runner-version "unknown"])
  (hasheq 'runner_version
          runner-version
          'suite
          (if (symbol? suite)
              (symbol->string suite)
              suite)
          'profile
          (if (symbol? profile)
              (symbol->string profile)
              profile)
          'shard
          (shard->jsexpr shard)
          'execution_mode
          (if (symbol? mode)
              (symbol->string mode)
              mode)
          'file_count
          (length results)
          'pass
          (count passed-result? results)
          'fail
          (count failed-result? results)
          'timeout
          (count timeout-result? results)
          'skip
          (count skipped-by-profile-result? results)
          'wall_clock_seconds
          (/ elapsed-ms 1000.0)
          'metadata_completeness
          (metadata-completeness-counts results)))

(define (format-run-summary-record results suite profile shard mode elapsed-ms runner-version)
  (define mc (metadata-completeness-counts results))
  (format (string-append "RUN-SUMMARY runner-version=~a suite=~a profile=~a shard=~a "
                         "execution-mode=~a file-count=~a pass=~a fail=~a timeout=~a "
                         "skip=~a wall-clock-seconds=~a "
                         "metadata-completeness=explicit:~a/heuristic:~a/missing:~a")
          runner-version
          suite
          profile
          (if shard
              (format "~a/~a" (car shard) (cdr shard))
              "none")
          mode
          (length results)
          (count passed-result? results)
          (count failed-result? results)
          (count timeout-result? results)
          (count skipped-by-profile-result? results)
          (/ elapsed-ms 1000.0)
          (hash-ref mc 'explicit)
          (hash-ref mc 'heuristic)
          (hash-ref mc 'missing)))

(define (print-run-summary-record results
                                  #:suite suite
                                  #:profile [profile 'local]
                                  #:shard [shard #f]
                                  #:mode [mode 'subprocess]
                                  #:elapsed-ms [elapsed-ms 0]
                                  #:runner-version [runner-version "unknown"])
  (printf "~a~n"
          (format-run-summary-record results suite profile shard mode elapsed-ms runner-version)))

(define (write-json-results! path
                             results
                             #:suite [suite 'all]
                             #:mode [mode 'subprocess]
                             #:elapsed-ms [elapsed-ms 0]
                             #:ledger [ledger #f]
                             #:profile [profile 'local]
                             #:shard [shard #f]
                             #:runner-version [runner-version "unknown"]
                             #:extra [extra #f]
                             #:actual-modes [actual-modes #f])
  (define passed-files (count passed-result? results))
  (define failed-files (count failed-result? results))
  (define timeout-files (count timeout-result? results))
  (define skipped-files (count skipped-by-profile-result? results))
  (define counts (category-counts results))
  (define ledger-summary (and ledger (summarize-ledger-results ledger results)))
  (define (file-execution-mode r)
    (and actual-modes
         (let ([p (test-file-result-path r)])
           ;; W2 FIX: result paths for metadata-missing files are stored as
           ;; strings (not path? objects); path->string on them raises a
           ;; contract violation, aborting JSON output (wave W2 blocker).
           (hash-ref actual-modes
                     (if (path? p)
                         (path->string p)
                         p)
                     #f))))
  (define payload
    (hasheq 'suite
            (symbol->string suite)
            'mode
            (symbol->string mode)
            'profile
            (symbol->string profile)
            'verdict
            (symbol->string (compute-verdict results))
            'elapsed_ms
            elapsed-ms
            'summary
            (hasheq 'files_total
                    (length results)
                    'files_passed
                    passed-files
                    'files_failed
                    failed-files
                    'files_timeout
                    timeout-files
                    'files_skipped_by_profile
                    skipped-files
                    'tests_total
                    (for/sum ([r (in-list results)]) (test-file-result-total r))
                    'tests_passed
                    (for/sum ([r (in-list results)]) (test-file-result-passed r))
                    'tests_failed
                    (for/sum ([r (in-list results)]) (test-file-result-failed r)))
            'category_counts
            (category-counts->jsexpr counts)
            'ledger
            (and ledger-summary (ledger-summary-counts ledger-summary))
            'run_summary
            (run-summary->jsexpr results
                                 #:suite suite
                                 #:profile profile
                                 #:shard shard
                                 #:mode mode
                                 #:elapsed-ms elapsed-ms
                                 #:runner-version runner-version)
            'files
            (if ledger
                (map (lambda (r)
                       (annotate-file-jsexpr (test-result->ledger-jsexpr r ledger)
                                             r
                                             #:profile profile
                                             #:mode mode
                                             #:execution-mode (file-execution-mode r)))
                     results)
                (map (lambda (r)
                       (annotate-file-jsexpr (test-result->jsexpr r)
                                             r
                                             #:profile profile
                                             #:mode mode
                                             #:execution-mode (file-execution-mode r)))
                     results))))
  (define payload*
    (if extra
        (hash-set payload 'extra extra)
        payload))
  (make-parent-directory* path)
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (write-json payload* out))))

;; W1 action 4: run-summary JSON fixture validation.
;;
;; Old-schema artifacts (pre-W1) lack the scheduler / prepared-environment /
;; timing telemetry entirely; they must still validate (no problems).
;; New-schema artifacts add the W1 `extra.scheduler` object and the
;; `extra.prepared_environment` object; every scheduler key is optional
;; (missing optional telemetry is tolerated). Required top-level and
;; per-file result fields must be present and well-typed — malformed
;; required result fields produce descriptive problems.
;;
;; Returns projection/type problems, not whole-envelope or semantic validation.
;; An empty list means only the checked fields passed. Never raises.
(define (validate-run-summary-json input)
  (define js
    (if (hash? input)
        input
        (hasheq)))
  (define summary
    (if (hash-has-key? js 'run_summary)
        (hash-ref js 'run_summary)
        js))
  (define problems '())
  (define (problem msg)
    (set! problems (cons msg problems)))
  (define (string-field? v)
    (string? v))
  (define (number-field? v)
    (and (number? v) (real? v)))
  (define (check-type field v pred what)
    (unless (pred v)
      (problem (format "~a: expected ~a, got ~s" field what v))))
  (unless (hash? input)
    (problem (format "root: expected object, got ~s" input)))
  (unless (hash? summary)
    (problem (format "run_summary: expected object, got ~s" summary)))
  (unless (hash? summary)
    (set! summary (hasheq)))
  ;; Required summary fields (nested in producer envelopes, flat in legacy artifacts).
  (for ([f '(runner_version suite profile execution_mode)])
    (unless (hash-has-key? summary f)
      (problem (format "missing required field ~a" f)))
    (when (hash-has-key? summary f)
      (check-type f (hash-ref summary f) string-field? "a string")))
  (for ([f '(file_count pass fail timeout skip wall_clock_seconds)])
    (unless (hash-has-key? summary f)
      (problem (format "missing required field ~a" f)))
    (when (hash-has-key? summary f)
      (check-type f (hash-ref summary f) number-field? "a number")))
  (unless (hash-has-key? summary 'shard)
    (problem "missing required field shard"))
  (when (hash-has-key? summary 'shard)
    (define sh (hash-ref summary 'shard))
    (cond
      [(eq? sh (json-null)) (void)]
      [(hash? sh)
       (for ([f '(index total)])
         (unless (hash-has-key? sh f)
           (problem (format "shard: missing ~a" f)))
         (when (hash-has-key? sh f)
           (check-type f (hash-ref sh f) number-field? "a number")))]
      [else (problem (format "shard: expected null or object, got ~s" sh))]))
  (unless (hash-has-key? summary 'metadata_completeness)
    (problem "missing required field metadata_completeness"))
  (when (hash-has-key? summary 'metadata_completeness)
    (define mc (hash-ref summary 'metadata_completeness))
    (cond
      [(hash? mc)
       (for ([f '(explicit heuristic missing)])
         (unless (hash-has-key? mc f)
           (problem (format "metadata_completeness: missing ~a" f)))
         (when (hash-has-key? mc f)
           (check-type f (hash-ref mc f) number-field? "a number")))]
      [else (problem (format "metadata_completeness: expected object, got ~s" mc))]))
  ;; Required per-file result fields.
  (unless (hash-has-key? js 'files)
    (problem "missing required field files"))
  (when (hash-has-key? js 'files)
    (define files (hash-ref js 'files))
    (unless (list? files)
      (problem (format "files: expected list, got ~s" files)))
    (when (list? files)
      (for ([f (in-list files)]
            [i (in-naturals)])
        (cond
          [(not (hash? f)) (problem (format "files[~a]: expected object, got ~s" i f))]
          [else
           (for ([rf '(path category exit_code passed failed total)])
             (unless (hash-has-key? f rf)
               (problem (format "files[~a]: missing required result field ~a" i rf)))
             (when (hash-has-key? f rf)
               (check-type (format "files[~a].~a" i rf)
                           (hash-ref f rf)
                           (if (member rf '(path category)) string-field? number-field?)
                           (if (member rf '(path category)) "a string" "a number"))))]))))
  ;; Optional W1 telemetry: `extra` may be absent entirely; when present,
  ;; `scheduler` and `prepared_environment` may be absent or partially
  ;; populated — missing optional telemetry is never a problem. If the
  ;; scheduler object IS present, every field inside it must be well-typed.
  (when (hash-has-key? js 'extra)
    (define extra (hash-ref js 'extra))
    (unless (hash? extra)
      (problem (format "extra: expected object, got ~s" extra)))
    (when (hash? extra)
      (when (hash-has-key? extra 'scheduler)
        (define sched (hash-ref extra 'scheduler))
        (if (hash? sched)
            (for ([(k v) (in-hash sched)])
              (cond
                [(eq? k 'scheduler_mode)
                 (unless (member v '("batch" "queue"))
                   (problem (format "extra.scheduler.scheduler_mode: expected batch or queue, got ~s"
                                    v)))]
                [else
                 (unless (or (number-field? v)
                             (and (equal? (hash-ref sched 'scheduler_mode #f) "queue")
                                  (memq k
                                        '(queue_wait_ms worker_busy_ms
                                                        worker_idle_ms
                                                        serial_partition_ms
                                                        parallel_partition_ms))
                                  (eq? v (json-null))))
                   (problem
                    (format "extra.scheduler.~a: expected number or queue-only null, got ~s" k v)))]))
            (problem (format "extra.scheduler: expected object, got ~s" sched))))
      (when (hash-has-key? extra 'prepared_environment)
        (define pe (hash-ref extra 'prepared_environment))
        (cond
          [(not (hash? pe))
           (problem (format "extra.prepared_environment: expected object, got ~s" pe))]
          [else
           (when (hash-has-key? pe 'result)
             (define result (hash-ref pe 'result))
             (unless (member result '("restored" "rebuilt" "unavailable"))
               (problem
                (format
                 "extra.prepared_environment.result: expected restored/rebuilt/unavailable, got ~s"
                 result))))
           (for ([f '(restore_ms fallback_ms)])
             (when (hash-has-key? pe f)
               (define v (hash-ref pe f))
               (unless (or (eq? v (json-null)) (number-field? v))
                 (problem (format "extra.prepared_environment.~a: expected number or null, got ~s"
                                  f
                                  v)))))]))))
  (reverse problems))

(define (print-ledger-summary ledger results)
  (define summary (summarize-ledger-results ledger results))
  (define counts (ledger-summary-counts summary))
  (newline)
  (displayln "KNOWN-FAILURE LEDGER")
  (printf "  Known failures: ~a~n" (hash-ref counts 'known_failures))
  (printf "  New failures: ~a~n" (hash-ref counts 'new_failures))
  (printf "  Unclassified failures: ~a~n" (hash-ref counts 'unclassified_failures))
  (printf "  Resolved known failures: ~a~n" (hash-ref counts 'resolved_known_failures))
  (printf "  Release-blocking known failures: ~a~n"
          (hash-ref counts 'release_blocking_known_failures))
  ;; W8 quarantine expiry: an entry past `expires_on` is reported as an
  ;; escalating failure instead of being tolerated.
  (when (> (hash-ref counts 'expired_quarantine_failures 0) 0)
    (printf "  Expired quarantine failures: ~a~n" (hash-ref counts 'expired_quarantine_failures))
    (for ([e (in-list (hash-ref summary 'expired_quarantine_failures '()))])
      (printf "    ⏫ ESCALATE ~a [~a] quarantine expired ~a (owner=~a issue=~a)~n"
              (hash-ref e 'file)
              (hash-ref e 'category)
              (hash-ref e 'expires_on)
              (hash-ref e 'owner)
              (hash-ref e 'issue)))
    (displayln
     "  ⛔ Quarantine expired: failure now blocks; re-triage, fix, or renew with justification."))
  (when (> (hash-ref counts 'unclassified_failures) 0)
    (displayln "  ⛔ Ledger incomplete: unclassified failures remain."))
  (when (> (hash-ref counts 'new_failures) 0)
    (displayln "  ⛔ New failures require triage before broad gate approval."))
  summary)

(define (print-summary results total-start-ms)
  (define total-files (length results))
  (define passed-files (count passed-result? results))
  (define failed-files (count failed-result? results))
  (define timeout-files (count timeout-result? results))
  (define skipped-files (count skipped-by-profile-result? results))
  (define zero-test-files
    (count (lambda (r) (and (= (test-file-result-exit-code r) 0) (= (test-file-result-total r) 0)))
           results))
  (define total-passed (for/sum ([r (in-list results)]) (test-file-result-passed r)))
  (define total-failed (for/sum ([r (in-list results)]) (test-file-result-failed r)))
  (define total-tests (for/sum ([r (in-list results)]) (test-file-result-total r)))
  (define verdict (compute-verdict results))
  (newline)
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "                    TEST SUMMARY")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Files:     ~a total, ~a passed, ~a failed, ~a timeouts~n"
          total-files
          passed-files
          failed-files
          timeout-files)
  (when (> skipped-files 0)
    (printf "             Skipped by profile: ~a~n" skipped-files))
  (when (> zero-test-files 0)
    (printf "             ⚠ ~a file~a with zero parsed tests (exit=0 but no rackunit output)~n"
            zero-test-files
            (if (= zero-test-files 1) "" "s")))
  (printf "  Tests:     ~a total, ~a passed, ~a failed~n" total-tests total-passed total-failed)
  (print-category-summary results)
  (when (and (= total-tests 0) (= passed-files total-files))
    (displayln "  ⚠ No test results parsed — files may use non-standard output format"))
  (printf "  Elapsed:   ~a~n" (format-duration total-start-ms))
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln (format-verdict-line verdict timeout-files))
  (displayln "═══════════════════════════════════════════════════════════")
  ;; W8: timed-out files are listed explicitly and separately — a timeout is
  ;; never a pass and must remain visible as its own status.
  (define timeouts (filter timeout-result? results))
  (when (pair? timeouts)
    (newline)
    (displayln "TIMEOUTS (distinct status — never counted as passes):")
    (for ([f (in-list timeouts)])
      (printf "  ⏱ ~a [~a] (exit=~a, ~a)~n"
              (test-file-result-path f)
              (classify-test-result f)
              (test-file-result-exit-code f)
              (format-duration (test-file-result-elapsed-ms f)))))
  (define failures (filter failed-result? results))
  (when (pair? failures)
    (newline)
    (displayln "FAILURES:")
    (for ([f (in-list failures)])
      (define path (test-file-result-path f))
      (define ec (test-file-result-exit-code f))
      (printf "  ~a ~a [~a] (exit=~a, ~a passed, ~a failed, ~a)~n"
              (if (= ec 2) "⏱" "✗")
              path
              (classify-test-result f)
              ec
              (test-file-result-passed f)
              (test-file-result-failed f)
              (format-duration (test-file-result-elapsed-ms f)))
      (define flines (extract-failure-lines (test-file-result-stdout-bytes f)))
      (for ([line (in-list flines)])
        (printf "    ~a~n" line)))))

(define (make-unique-log-name test-path)
  (define base
    (let ([b (file-name-from-path test-path)])
      (if b
          (path->string b)
          "unknown")))
  (define safe-base (regexp-replace* #rx"[^a-zA-Z0-9._-]" base "_"))
  (define path-hash (number->string (equal-hash-code test-path) 16))
  (string-append "q-test-fail-" (string-replace safe-base ".rkt" "") "-" path-hash ".log"))

(define (save-failure-logs results #:profile [profile #f] #:mode [mode #f])
  (define failures (filter failed-result? results))
  (define profile-label (or (optional-id-string profile) "unknown"))
  (define mode-label (or (optional-id-string mode) "unknown"))
  (for ([f (in-list failures)])
    (define log-path (build-path "/tmp" (make-unique-log-name (test-file-result-path f))))
    (call-with-output-file log-path
                           #:exists 'truncate/replace
                           (lambda (out)
                             (fprintf out
                                      "=== ~a (exit=~a, elapsed=~a, profile=~a, mode=~a) ===~n"
                                      (test-file-result-path f)
                                      (test-file-result-exit-code f)
                                      (format-duration (test-file-result-elapsed-ms f))
                                      profile-label
                                      mode-label)
                             (fprintf out "--- stdout ---~n")
                             (display (bytes->string* (test-file-result-stdout-bytes f)) out)
                             (fprintf out "~n--- stderr ---~n")
                             (display (bytes->string* (test-file-result-stderr-bytes f)) out)))))

;; ─────────────────────────────────────────────────────────────────────
;; Phase C fold (§11.3): result parsing + classification (formerly parse.rkt)
;; ─────────────────────────────────────────────────────────────────────

(define (bytes->string* bs)
  (with-handlers ([exn:fail? (lambda (e) (bytes->string/latin-1 bs))])
    (bytes->string/utf-8 bs)))

(struct test-file-result (path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total)
  #:transparent)

(struct profile-skipped-result test-file-result () #:transparent)

(define (make-test-file-result path
                               exit-code
                               stdout-bytes
                               stderr-bytes
                               elapsed-ms
                               passed
                               failed
                               total)
  (test-file-result path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total))

(define (parse-raco-output stdout-bytes)
  (define-values (passed failed total) (parse-raco-output/raw stdout-bytes))
  ;; Independent failure evidence supplies a lower bound when summaries pass.
  (define output (bytes->string* stdout-bytes))
  (define diagnostic?
    (or (regexp-match? #px"(?m:^(?:FAILURE|ERROR)\r?\n(?:(?!-+$)[^\n]*\n)*(?:name|location|message):)"
                       output)
        (regexp-match? #rx"#<test-(failure|error)>" output)
        (for/or ([m (in-list (regexp-match* #px"(?m:^([0-9]+) ([0-9]+) .+\\.(?:rkt|rktl|scrbl)$)"
                                            output
                                            #:match-select values))])
          (positive? (string->number (cadr m))))))
  (if (and (zero? failed) diagnostic?)
      (values passed 1 (add1 total))
      (values passed failed total)))

(define (parse-raco-output/raw stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))
  (define all-matches
    (filter
     values
     (for/list ([line (in-list lines)])
       (regexp-match
        #px"([0-9]+) success\\(es\\) ([0-9]+) failure\\(s\\)(?: ([0-9]+) error\\(s\\))? ([0-9]+) test\\(s\\) run"
        line))))
  (cond
    [(pair? all-matches)
     (define passed (for/sum ([m (in-list all-matches)]) (string->number (cadr m))))
     (define failed
       (for/sum ([m (in-list all-matches)])
                (+ (string->number (caddr m))
                   (let ([errors (list-ref m 3)])
                     (if errors
                         (string->number errors)
                         0)))))
     (values passed failed (+ passed failed))]
    [else
     ;; `raco test` emits `<failed> <total> <path>` when a module has
     ;; failures but no explicit RackUnit summary. Parse these lines before
     ;; the generic legacy formats so failed assertions are never reported as
     ;; zero parsed tests.
     (define compact-matches
       (filter values
               (for/list ([line (in-list lines)])
                 (regexp-match #px"^([0-9]+) ([0-9]+) .+\\.(?:rkt|rktl|scrbl)$" line))))
     (define compact-failed (for/sum ([m (in-list compact-matches)]) (string->number (cadr m))))
     (define compact-total (for/sum ([m (in-list compact-matches)]) (string->number (caddr m))))
     (define passed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? passed" line))
         (if m
             (string->number (cadr m))
             n)))
     (define failed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? failed" line))
         (if m
             (string->number (cadr m))
             n)))
     (cond
       [(pair? compact-matches)
        (values (- compact-total compact-failed) compact-failed compact-total)]
       [(> (+ passed failed) 0) (values passed failed (+ passed failed))]
       [else
        (define result-successes (length (regexp-match* #rx"#<test-success>" output)))
        (define result-failures
          (+ (length (regexp-match* #rx"#<test-failure>" output))
             (length (regexp-match* #rx"#<test-error>" output))))
        (values result-successes result-failures (+ result-successes result-failures))])]))

(define (normalize-counts exit-code passed failed total)
  (values passed failed total))

(define (effective-exit-code exit-code failed)
  (if (and (= exit-code 0) (> failed 0)) 1 exit-code))

(define (result-output-string r)
  (string-downcase (string-append (bytes->string* (test-file-result-stdout-bytes r))
                                  "\n"
                                  (bytes->string* (test-file-result-stderr-bytes r)))))

(define (output-matches-any? output patterns)
  (for/or ([pat (in-list patterns)])
    (regexp-match? pat output)))

(define (classify-test-result r)
  (define exit-code (test-file-result-exit-code r))
  (define failed (test-file-result-failed r))
  (define total (test-file-result-total r))
  (define output (result-output-string r))
  (cond
    [(profile-skipped-result? r) 'SKIPPED_BY_PROFILE]
    [(and (= exit-code 0) (> total 0)) 'PASS]
    [(and (= exit-code 0) (= total 0)) 'ZERO_PARSED]
    [(= exit-code 2) 'TIMEOUT]
    [(output-matches-any? output (list #rx"user break" #rx"break exception")) 'USER_BREAK]
    [(output-matches-any? output
                          (list #rx"missing environment variable"
                                #rx"environment variable"
                                #rx"api[_-]?key"
                                #rx"display.*not available"
                                #rx"x11"
                                #rx"no such file or directory"))
     'ENVIRONMENT_MISSING]
    [(output-matches-any?
      output
      (list #rx"read-syntax" #rx"syntax error" #rx"module: identifier already defined"))
     'COMPILE_FAILURE]
    [(output-matches-any? output
                          (list #rx"standard-module-name-resolver"
                                #rx"collection not found"
                                #rx"cannot open module file"
                                #rx"cannot find module"
                                #rx"cannot open input file"))
     'MODULE_LOAD_FAILURE]
    [(or (> failed 0)
         (output-matches-any? output
                              (list #rx"failure"
                                    #rx"check-[a-z0-9-]+"
                                    #rx"actual:"
                                    #rx"expected:"
                                    #rx"test raised an exception")))
     'ASSERTION_FAILURE]
    [else 'UNKNOWN_FAILURE]))

(define (test-result->jsexpr r)
  (hasheq 'path
          (let ([p (test-file-result-path r)])
            (if (path? p)
                (path->string p)
                p))
          'category
          (symbol->string (classify-test-result r))
          'exit_code
          (test-file-result-exit-code r)
          'elapsed_ms
          (test-file-result-elapsed-ms r)
          'passed
          (test-file-result-passed r)
          'failed
          (test-file-result-failed r)
          'total
          (test-file-result-total r)
          'output
          (truncate-test-output (result-output-string r) DEFAULT-OUTPUT-CAP)))

(define FAILURE-START #rx"^-+ FAILURE -+$")
;; FIXED (v0.99.39): Changed from #rx to #px mode. The {20,} quantifier
;; is literal in #rx mode but a valid repetition in #px (PCRE) mode.
;; DESIGN FACT (W3 F1): {20,} quantifier is literal in #rx but valid repetition in #px (PCRE).
;; This regex never matched "----..." separator lines before the fix.
(define FAILURE-END #px"^-{20,}$")

(define (extract-failure-lines stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))
  (define in-failure? (box #f))
  (for/list ([line (in-list lines)]
             #:when (cond
                      [(regexp-match? FAILURE-START line)
                       (set-box! in-failure? #t)
                       #t]
                      [(and (unbox in-failure?)
                            (regexp-match? FAILURE-END line)
                            (not (regexp-match? FAILURE-START line)))
                       (set-box! in-failure? #f)
                       #t]
                      [(unbox in-failure?) #t]
                      [else #f]))
    line))

(define DEFAULT-OUTPUT-CAP 65536)

(define (truncate-test-output output max-bytes)
  (define bts (string->bytes/utf-8 output))
  (if (<= (bytes-length bts) max-bytes)
      output
      (let* ([half (quotient max-bytes 2)]
             [head (subbytes bts 0 half)]
             [tail (subbytes bts (- (bytes-length bts) half))]
             [marker (string->bytes/utf-8 (format "\n... truncated (~a bytes, showing ~a + ~a) ...\n"
                                                  (bytes-length bts)
                                                  half
                                                  half))])
        (bytes->string/utf-8 (bytes-append head marker tail)))))

;; ─────────────────────────────────────────────────────────────────────
;; Phase C fold (§11.3): profile-skip results (formerly profiles.rkt)
;; ─────────────────────────────────────────────────────────────────────

(define skipped-result-exit-code 5)

(define (make-skipped-result path profile requirements)
  (define skipped (skipped-requirements profile requirements))
  (define reason
    (format "SKIPPED_BY_PROFILE profile=~a requires=~a"
            profile
            (string-join (map normalize-requirement skipped) ",")))
  (profile-skipped-result path
                          skipped-result-exit-code
                          (string->bytes/utf-8 (string-append reason "\n"))
                          #""
                          0
                          0
                          0
                          0))

;; ─────────────────────────────────────────────────────────────────────
;; Phase C fold (§11.3): known-failure ledger (formerly ledger.rkt)
;; ─────────────────────────────────────────────────────────────────────

(define required-ledger-keys '(file category owner first_seen release_blocking issue notes))
(define missing-sentinel (gensym 'missing))

(define (hash-ref* h key [default #f])
  (cond
    [(hash-has-key? h key) (hash-ref h key)]
    [(hash-has-key? h (string->symbol (symbol->string key)))
     (hash-ref h (string->symbol (symbol->string key)))]
    [else default]))

(define (path->ledger-string p)
  (cond
    [(path? p) (path->string p)]
    [(string? p) p]
    [else (format "~a" p)]))

(define (normalize-path-string p)
  (define s (path->ledger-string p))
  (cond
    [(string-prefix? s "./") (substring s 2)]
    [else s]))

(define (normalize-category v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(string? v) v]
    [else (format "~a" v)]))

;; Quarantine expiry dates must be unambiguous, zero-padded ISO dates so that
;; plain string comparison matches calendar order. Shape alone is not enough:
;; `2026-13-01` passes the regex but would sort permanently after every real
;; date and thus never expire. Validate calendar ranges too.
(define (valid-expires-on? v)
  (and (string? v)
       (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" v)
       (let ([month (string->number (substring v 5 7))]
             [day (string->number (substring v 8 10))])
         (and month day (<= 1 month 12) (<= 1 day 31)))))

(define (normalize-expires-on v)
  (cond
    [(or (not v) (eq? v 'null)) #f]
    [(valid-expires-on? v) v]
    [else
     (raise-arguments-error 'normalize-ledger-entry
                            "expires_on must be an ISO YYYY-MM-DD string or null"
                            "expires_on"
                            v)]))

(define (pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (number->string n)))

;; Local-timezone calendar date as "YYYY-MM-DD" (basis for quarantine expiry).
(define (today-iso8601)
  (define d (seconds->date (current-seconds)))
  (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d))))

(define (normalize-ledger-entry raw)
  (unless (hash? raw)
    (raise-arguments-error 'normalize-ledger-entry "ledger entry must be a JSON object" "entry" raw))
  (for ([key (in-list required-ledger-keys)])
    (when (eq? (hash-ref* raw key missing-sentinel) missing-sentinel)
      (raise-arguments-error 'normalize-ledger-entry
                             "ledger entry missing required key"
                             "key"
                             key
                             "entry"
                             raw)))
  (hasheq 'file
          (normalize-path-string (hash-ref* raw 'file))
          'category
          (normalize-category (hash-ref* raw 'category))
          'owner
          (hash-ref* raw 'owner)
          'first_seen
          (hash-ref* raw 'first_seen)
          'release_blocking
          (and (hash-ref* raw 'release_blocking) #t)
          'issue
          (hash-ref* raw 'issue)
          'notes
          (hash-ref* raw 'notes)
          'expires_on
          (normalize-expires-on (hash-ref* raw 'expires_on #f))))

;; #t once `today` has reached the entry's `expires_on` (the quarantine window
;; is [first_seen, expires_on); on the expiry date itself the entry escalates).
;; Entries without `expires_on` never expire through this mechanism.
(define (ledger-entry-expired? entry #:today [today (today-iso8601)])
  (define expires (hash-ref entry 'expires_on #f))
  (and expires (string>=? today expires)))

(define (extract-ledger-entries payload)
  (cond
    [(list? payload) payload]
    [(hash? payload) (hash-ref* payload 'entries '())]
    [else '()]))

(define (load-known-failure-ledger path)
  ;; The ledger file is optional: a missing path is an empty ledger (every
  ;; failure is "new"), not an error — W8's full-regression workflow always
  ;; passes --ledger and the file only appears once quarantined failures exist.
  (cond
    [(not (or (file-exists? path) (link-exists? path))) '()]
    [else
     (map normalize-ledger-entry (extract-ledger-entries (call-with-input-file path read-json)))]))

(define (result-failure? r)
  (and (not (= (test-file-result-exit-code r) 0))
       (not (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))))

(define (result-path-string r)
  (normalize-path-string (test-file-result-path r)))

(define (result-category-string r)
  (symbol->string (classify-test-result r)))

(define (same-file? entry r)
  (string=? (hash-ref entry 'file) (result-path-string r)))

(define (ledger-entry-matches-result? entry r)
  (and (same-file? entry r) (string=? (hash-ref entry 'category) (result-category-string r))))

(define (matching-entry ledger r)
  (for/first ([entry (in-list ledger)]
              #:when (ledger-entry-matches-result? entry r))
    entry))

(define (file-entry ledger r)
  (for/first ([entry (in-list ledger)]
              #:when (same-file? entry r))
    entry))

(define (result->unclassified-entry r)
  (hasheq 'file
          (result-path-string r)
          'category
          (result-category-string r)
          'known_category
          (let ([entry (file-entry '() r)]) (and entry (hash-ref entry 'category #f)))))

(define (result->new-entry r)
  (hasheq 'file (result-path-string r) 'category (result-category-string r)))

;; An expired quarantine match is surfaced as a failure that must escalate:
;; it keeps its identifying fields and carries escalate=#t so downstream
;; consumers (JSON evidence, triage) cannot mistake it for a tolerated known.
(define (result->escalation-entry entry)
  (hasheq 'file
          (hash-ref entry 'file)
          'category
          (hash-ref entry 'category)
          'expires_on
          (hash-ref entry 'expires_on #f)
          'owner
          (hash-ref entry 'owner)
          'issue
          (hash-ref entry 'issue)
          'escalate
          #t))

(define (summarize-ledger-results ledger results #:today [today (today-iso8601)])
  (define (expired? entry)
    (ledger-entry-expired? entry #:today today))
  (define failures (filter result-failure? results))
  (define known
    (for/list ([r (in-list failures)]
               #:do [(define entry (matching-entry ledger r))]
               #:when (and entry (not (expired? entry))))
      entry))
  (define expired-quarantine
    (for/list ([r (in-list failures)]
               #:do [(define entry (matching-entry ledger r))]
               #:when (and entry (expired? entry)))
      (result->escalation-entry entry)))
  (define new
    (for/list ([r (in-list failures)]
               #:when (not (file-entry ledger r)))
      (result->new-entry r)))
  (define unclassified
    (for/list ([r (in-list failures)]
               #:when (not (matching-entry ledger r)))
      (define by-file (file-entry ledger r))
      (hasheq 'file
              (result-path-string r)
              'category
              (result-category-string r)
              'known_category
              (and by-file (hash-ref by-file 'category #f))
              'issue
              (and by-file (hash-ref by-file 'issue #f)))))
  (define resolved
    (for/list ([entry (in-list ledger)]
               #:unless (for/or ([r (in-list failures)])
                          (same-file? entry r)))
      entry))
  (define release-blocking-known
    (filter (lambda (entry) (hash-ref entry 'release_blocking #f)) known))
  (hasheq 'known_failures
          known
          'new_failures
          new
          'unclassified_failures
          unclassified
          'expired_quarantine_failures
          expired-quarantine
          'resolved_known_failures
          resolved
          'release_blocking_known_failures
          release-blocking-known))

(define (ledger-summary-counts summary)
  (hasheq 'known_failures
          (length (hash-ref summary 'known_failures '()))
          'new_failures
          (length (hash-ref summary 'new_failures '()))
          'unclassified_failures
          (length (hash-ref summary 'unclassified_failures '()))
          'expired_quarantine_failures
          (length (hash-ref summary 'expired_quarantine_failures '()))
          'resolved_known_failures
          (length (hash-ref summary 'resolved_known_failures '()))
          'release_blocking_known_failures
          (length (hash-ref summary 'release_blocking_known_failures '()))))

;; ─────────────────────────────────────────────────────────────────────
;; Phase C fold (§11.3): gate evidence (formerly gate-evidence.rkt)
;; ─────────────────────────────────────────────────────────────────────

;; F-15 strict validation: full 40-hex-char SHA required
(define (strict-sha-valid? sha)
  (and (string? sha) (= (string-length sha) 40) (regexp-match? #px"^[0-9a-f]{40}$" sha)))

;; v1.00.24 W3 truthful shard policy: gate evidence always represents a FULL
;; suite run. A sharded run (shard-total > 1) covers only part of the suite,
;; so its evidence would be partial and could be mistaken for (or overwrite)
;; a full-suite PASS. The minimum truthful behavior is to REFUSE recording
;; entirely rather than retain partial evidence. Returns #f when recording is
;; allowed for this shard configuration, otherwise a human-readable reason.
(define (gate-evidence-refusal shard-index shard-total)
  (cond
    [(not (and (integer? shard-total) (> shard-total 0)))
     (format "--shard-total must be a positive integer; got ~a" shard-total)]
    [(not (and (integer? shard-index) (>= shard-index 0) (< shard-index shard-total)))
     (format "--shard-index must be an integer in [0, ~a); got ~a" shard-total shard-index)]
    [(> shard-total 1)
     (format (string-append "--record-gate-evidence is refused for sharded runs (shard ~a of ~a): "
                            "a shard produces only partial results and must never overwrite or emit "
                            "full-suite PASS evidence")
             shard-index
             shard-total)]
    [else #f]))

;; F-15: Validate recorded evidence hash against strict requirements.
;; Rejects unknown/empty version, short/missing SHA, stale/future timestamp,
;; non-zero failed/timed-out counts, zero test count.
;; v1.00.24 W3 (fail closed): also rejects evidence without valid EXPLICIT
;; shard identity (pre-migration evidence cannot prove it covers a full
;; suite) and any partial shard evidence (shard-total > 1), which is never
;; valid full-suite gate evidence.
(define (validate-gate-evidence! evidence-hash)
  (define ver (hash-ref evidence-hash 'version #f))
  (define sha (hash-ref evidence-hash 'git-sha #f))
  (define shard-index (hash-ref evidence-hash 'shard-index #f))
  (define shard-total (hash-ref evidence-hash 'shard-total #f))
  (define pass-c (hash-ref evidence-hash 'passed 0))
  (define fail-c (hash-ref evidence-hash 'failed -1))
  (define timeout-c (hash-ref evidence-hash 'timed-out -1))
  (define parsed (hash-ref evidence-hash 'parsed-test-count 0))
  (define ts (hash-ref evidence-hash 'timestamp #f))
  (define now (current-seconds))
  (define max-age 7200) ;; 2 hours
  (define max-future 300) ;; 5 minutes clock skew tolerance
  (cond
    [(or (not ver) (equal? ver "unknown"))
     (raise-user-error 'validate-gate-evidence! "version is unknown or missing")]
    [(or (not sha) (equal? sha "unknown"))
     (raise-user-error 'validate-gate-evidence! "git SHA is unknown")]
    [(not (strict-sha-valid? sha))
     (raise-user-error 'validate-gate-evidence! "SHA must be full 40 hex chars; got ~a" sha)]
    [(not (and (integer? shard-total)
               (> shard-total 0)
               (integer? shard-index)
               (>= shard-index 0)
               (< shard-index shard-total)))
     (raise-user-error
      'validate-gate-evidence!
      (string-append
       "evidence lacks valid explicit shard identity (shard-index/shard-total; got ~a/~a); "
       "pre-migration evidence is rejected — fail closed, regenerate with the current runner")
      shard-index
      shard-total)]
    [(> shard-total 1)
     (raise-user-error 'validate-gate-evidence!
                       "evidence is a partial shard (~a of ~a), never valid full-suite gate evidence"
                       shard-index
                       shard-total)]
    [(not (and (integer? parsed) (positive? parsed)))
     (raise-user-error 'validate-gate-evidence! "parsed test count must be positive; got ~a" parsed)]
    [(and (integer? fail-c) (positive? fail-c))
     (raise-user-error 'validate-gate-evidence! "evidence has ~a failure(s)" fail-c)]
    [(and (integer? timeout-c) (positive? timeout-c))
     (raise-user-error 'validate-gate-evidence! "evidence has ~a timeout(s)" timeout-c)]
    [(and ts (integer? ts) (>= (- now ts) max-age))
     (raise-user-error 'validate-gate-evidence! "evidence is stale (~a seconds old)" (- now ts))]
    [(and ts (integer? ts) (>= (- ts now) max-future))
     (raise-user-error 'validate-gate-evidence!
                       "evidence has future timestamp (~a seconds ahead)"
                       (- ts now))]
    [else (void)]))

(define (get-git-sha)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define-values (sp out in err)
      (subprocess #f #f #f (find-executable-path "git") "rev-parse" "HEAD"))
    (close-output-port in)
    (define sha (string-trim (port->string out)))
    (close-input-port out)
    (close-input-port err)
    (subprocess-wait sp)
    (if (string=? sha "") "unknown" sha)))

;; The gate-evidence document has its own hand-written JSON shape (kept
;; verbatim from gate-evidence.rkt); the name is distinct from json's
;; write-json because this module also writes run-summary documents.
(define (write-gate-evidence-json h out)
  (display "{" out)
  (define pairs
    (list (cons "version" (format "~s" (hash-ref h 'version)))
          (cons "git_sha" (format "~s" (hash-ref h 'git-sha)))
          (cons "suite" (format "~s" (hash-ref h 'suite)))
          (cons "args"
                (format "[~a]"
                        (string-join (for/list ([a (hash-ref h 'args)])
                                       (format "~s" a))
                                     ", ")))
          (cons "jobs" (format "~a" (hash-ref h 'jobs)))
          (cons "timeout"
                (let ([t (hash-ref h 'timeout)])
                  (if (string? t)
                      (format "~s" t)
                      (format "~a" t))))
          (cons "repeat" (format "~a" (hash-ref h 'repeat)))
          (cons "shard_index" (format "~a" (hash-ref h 'shard-index)))
          (cons "shard_total" (format "~a" (hash-ref h 'shard-total)))
          (cons "selected_file_count" (format "~a" (hash-ref h 'selected-file-count)))
          (cons "parsed_test_count" (format "~a" (hash-ref h 'parsed-test-count)))
          (cons "passed" (format "~a" (hash-ref h 'passed)))
          (cons "failed" (format "~a" (hash-ref h 'failed)))
          (cons "timed_out" (format "~a" (hash-ref h 'timed-out)))
          (cons "inventory_hash" (format "~s" (hash-ref h 'inventory-hash)))
          (cons "timestamp" (format "~a" (hash-ref h 'timestamp)))))
  (for ([p (in-list pairs)]
        [i (in-naturals)])
    (when (> i 0)
      (display ", " out))
    (display (format "\"~a\": ~a" (car p) (cdr p)) out))
  (displayln "}" out))

;; v1.00.24 W3 truthful recording contract:
;;  - sharded runs (shard-total > 1) are REFUSED via gate-evidence-refusal:
;;    a shard covers only part of the suite and must never overwrite or emit
;;    evidence that claims to be a full suite;
;;  - results must account for every selected file (#:file-count), so an
;;    interrupted run (partial results / missing RUN-SUMMARY) can never be
;;    recorded as a PASS;
;;  - the effective args and the explicit shard identity (shard-index,
;;    shard-total) are recorded so unsharded and sharded configurations stay
;;    distinguishable in the evidence itself.
(define (record-gate-evidence! suite-label
                               #:results [results '()]
                               #:args [args '()]
                               #:jobs [jobs 1]
                               #:timeout [timeout #f]
                               #:repeat [repeat 1]
                               #:file-count [file-count 0]
                               #:inventory-hash [inv-hash "n/a"]
                               #:shard-index [shard-index 0]
                               #:shard-total [shard-total 1])
  (define refusal (gate-evidence-refusal shard-index shard-total))
  (when refusal
    (raise-user-error 'run-tests "cannot record gate evidence: ~a" refusal))
  (unless (pair? results)
    (raise-user-error 'run-tests "cannot record gate evidence: no results"))
  (unless (= (length results) file-count)
    (raise-user-error
     'run-tests
     (string-append "cannot record gate evidence: incomplete run — results cover ~a of ~a selected "
                    "files (interrupted or missing RUN-SUMMARY); refusing to record PASS evidence")
     (length results)
     file-count))
  (define total-parsed (for/sum ([r (in-list results)]) (test-file-result-total r)))
  (when (zero? total-parsed)
    (raise-user-error 'run-tests "cannot record gate evidence: zero parsed tests"))
  (define any-fail?
    (for/or ([r (in-list results)])
      (not (zero? (test-file-result-failed r)))))
  (when any-fail?
    (raise-user-error 'run-tests "cannot record gate evidence: failures present"))
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (unless (directory-exists? evid-dir)
    (make-directory evid-dir))
  (define ver
    (let ([m (regexp-match #rx"define q-version \"([^\"]+)\""
                           (file->string (build-path base-dir "util/version.rkt")))])
      (or (and m (cadr m)) "unknown")))
  (define sha (get-git-sha))
  (define pass-count (for/sum ([r (in-list results)]) (test-file-result-passed r)))
  (define fail-count (for/sum ([r (in-list results)]) (test-file-result-failed r)))
  (define timeout-count (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define evidence-file (build-path evid-dir (format "~a.json" suite-label)))
  (define evidence-hash
    (hash 'version
          ver
          'git-sha
          sha
          'suite
          suite-label
          'args
          (map (lambda (a)
                 (if (symbol? a)
                     (symbol->string a)
                     a))
               args)
          'jobs
          jobs
          'timeout
          (or timeout "none")
          'repeat
          repeat
          'shard-index
          shard-index
          'shard-total
          shard-total
          'selected-file-count
          file-count
          'parsed-test-count
          total-parsed
          'passed
          pass-count
          'failed
          fail-count
          'timed-out
          timeout-count
          'inventory-hash
          inv-hash
          'timestamp
          (current-seconds)))
  ;; F-15: Validate recorded evidence hash before writing
  (validate-gate-evidence! evidence-hash)
  (call-with-output-file evidence-file
                         (lambda (out)
                           (write-gate-evidence-json evidence-hash out)
                           (newline out))
                         #:exists 'truncate)
  (printf ";; gate evidence v2 recorded: ~a (v~a) ~a files, ~a tests~n"
          suite-label
          ver
          file-count
          total-parsed))

#lang racket/base

;; q/scripts/run-tests/full-regression-status.rkt — L4 full-regression evidence
;; aggregation. The summary is deliberately more conservative than individual
;; job conclusions: every required lane must have complete, parseable, passing
;; evidence before the top-level status can be "pass".
;; STABILITY: internal

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/string)

(provide (struct-out lane-record)
         json-ref
         make-lane-records
         evaluate-full-regression
         write-summary!
         main)

(struct lane-record (source payload problem) #:transparent)

(define required-shard-count 6)
(define summary-schema-version "1.00.09")

;; JSON readers normally produce symbol keys, but supporting string keys makes
;; fixture construction and future external producers unambiguous.
(define (json-ref h key [default #f])
  (cond
    [(and (hash? h) (hash-has-key? h key)) (hash-ref h key)]
    [(and (hash? h) (symbol? key) (hash-has-key? h (symbol->string key)))
     (hash-ref h (symbol->string key))]
    [(and (hash? h) (string? key) (hash-has-key? h (string->symbol key)))
     (hash-ref h (string->symbol key))]
    [else default]))

(define (nonnegative-number? v)
  (and (number? v) (>= v 0)))

(define (valid-run-record? payload)
  (define summary (json-ref payload 'run_summary #f))
  (and (hash? payload)
       (string? (json-ref payload 'verdict #f))
       (hash? summary)
       (positive? (json-ref summary 'file_count 0))
       (andmap nonnegative-number?
               (list (json-ref summary 'pass #f)
                     (json-ref summary 'fail #f)
                     (json-ref summary 'timeout #f)
                     (json-ref summary 'skip #f)
                     (json-ref summary 'wall_clock_seconds #f)))))

(define (record-verdict record)
  (cond
    [(lane-record-problem record) "malformed"]
    [(not (valid-run-record? (lane-record-payload record))) "malformed"]
    [else (json-ref (lane-record-payload record) 'verdict "malformed")]))

(define (record-status record)
  (define verdict (record-verdict record))
  (cond
    [(equal? verdict "pass") "pass"]
    ;; Malformed or unreadable evidence is operationally incomplete, not a
    ;; test failure: it must retain the stronger timeout classification.
    [(member verdict '("incomplete" "timeout" "malformed")) "timeout"]
    [else "fail"]))

(define (status-precedence statuses)
  (cond
    [(member "timeout" statuses) "timeout"]
    [(member "fail" statuses) "fail"]
    [else "pass"]))

(define (lane-summary name expected records upstream-result)
  (define collected (length records))
  (define record-statuses (map record-status records))
  (define reasons '())
  (define (add-reason! reason) (set! reasons (cons reason reasons)))
  (when (not (= collected expected))
    (add-reason! (format "expected ~a evidence record(s), collected ~a" expected collected)))
  (when (member upstream-result '("cancelled" "timed_out"))
    (add-reason! (format "upstream job was ~a" upstream-result)))
  (when (not (member upstream-result '("success" "failure" "cancelled" "timed_out" "skipped")))
    (add-reason! (format "upstream job result is unknown: ~a" upstream-result)))
  (for ([record (in-list records)] [status (in-list record-statuses)])
    (when (equal? status "timeout")
      (add-reason! (format "unreadable, malformed, or incomplete evidence: ~a"
                           (lane-record-source record))))
    (when (and (equal? status "fail")
               (not (equal? (record-verdict record) "fail")))
      (add-reason! (format "unexpected record verdict ~a: ~a"
                           (record-verdict record)
                           (lane-record-source record)))))
  (define evidence-status
    (cond
      [(or (not (= collected expected))
           (member upstream-result '("cancelled" "timed_out"))
           (member "timeout" record-statuses))
       "timeout"]
      [(or (not (equal? upstream-result "success"))
           (member "fail" record-statuses))
       "fail"]
      [else "pass"]))
  (when (and (equal? evidence-status "fail")
             (not (equal? upstream-result "success")))
    (add-reason! (format "upstream job result is ~a" upstream-result)))
  (when (and (equal? evidence-status "fail")
             (member "fail" record-statuses))
    (add-reason! "at least one evidence record has verdict fail"))
  (hasheq 'name name
          'expected_records expected
          'collected_records collected
          'upstream_job_result upstream-result
          'record_verdicts (map record-verdict records)
          'record_sources (map lane-record-source records)
          'evidence_classification evidence-status
          'status evidence-status
          'reasons (reverse reasons)))

(define (record-number record key)
  (define summary (json-ref (lane-record-payload record) 'run_summary (hasheq)))
  (define v (json-ref summary key 0))
  (if (nonnegative-number? v) v 0))

(define (unique-strings values)
  (sort (remove-duplicates (filter string? values)) string<?))

(define (matrix-summary shard-records)
  (define valid-shards
    (filter (lambda (record) (valid-run-record? (lane-record-payload record))) shard-records))
  (define (sum key) (for/sum ([record (in-list valid-shards)]) (record-number record key)))
  (define (summary-field record key)
    (json-ref (json-ref (lane-record-payload record) 'run_summary (hasheq)) key #f))
  (hasheq 'shards
          (for/list ([record (in-list valid-shards)])
            (define payload (lane-record-payload record))
            (define summary (json-ref payload 'run_summary (hasheq)))
            (hasheq 'shard (json-ref summary 'shard 'null)
                    'suite (json-ref payload 'suite "unknown")
                    'profile (json-ref payload 'profile "unknown")
                    'runner_mode (json-ref summary 'execution_mode "unknown")
                    'verdict (json-ref payload 'verdict "malformed")
                    'pass (record-number record 'pass)
                    'fail (record-number record 'fail)
                    'timeout (record-number record 'timeout)
                    'skip (record-number record 'skip)
                    'wall_clock_seconds (record-number record 'wall_clock_seconds)))
          'totals
          (hasheq 'files (sum 'file_count)
                  'pass (sum 'pass)
                  'fail (sum 'fail)
                  'timeout (sum 'timeout)
                  'skip (sum 'skip)
                  'wall_clock_seconds (sum 'wall_clock_seconds))
          'suite (string-join (unique-strings (map (lambda (r) (json-ref (lane-record-payload r) 'suite #f)) valid-shards)) ",")
          'profile (string-join (unique-strings (map (lambda (r) (json-ref (lane-record-payload r) 'profile #f)) valid-shards)) ",")
          'runner_mode (string-join (unique-strings (map (lambda (r) (summary-field r 'execution_mode)) valid-shards)) ",")))

(define (evaluate-full-regression #:shard-records shard-records
                                  #:workflows-records workflows-records
                                  #:platform-records platform-records
                                  #:test-result [test-result "unknown"]
                                  #:workflows-result [workflows-result "unknown"]
                                  #:platform-result [platform-result "unknown"]
                                  #:expected-shards [expected-shards required-shard-count]
                                  #:run-url [run-url #f])
  (define linux-lane (lane-summary "linux-shards" expected-shards shard-records test-result))
  (define workflows-lane (lane-summary "workflows" 1 workflows-records workflows-result))
  (define platform-lane (lane-summary "platform" 1 platform-records platform-result))
  (define required-lanes
    (hasheq 'linux_shards linux-lane
            'workflows workflows-lane
            'platform platform-lane))
  (define status
    (status-precedence
     (list (json-ref linux-lane 'status)
           (json-ref workflows-lane 'status)
           (json-ref platform-lane 'status))))
  (hash-set* (matrix-summary shard-records)
             'schema_version summary-schema-version
             'status status
             'generated_at_epoch (current-seconds)
             'run_url (or run-url 'null)
             'required_lanes required-lanes))

(define (read-json-record path)
  (with-handlers ([exn:fail? (lambda (e) (lane-record (path->string path) #f (exn-message e)))])
    (call-with-input-file path
      (lambda (in)
        (lane-record (path->string path) (read-json in) #f)))))

(define (json-files-in directory)
  (if (directory-exists? directory)
      (sort (filter (lambda (path) (regexp-match? #px"\\.json$" (path->string path)))
                    (find-files file-exists? directory))
            path<?)
      '()))

(define (make-lane-records directory)
  (map read-json-record (json-files-in directory)))

(define (write-json-file! path payload)
  (call-with-output-file path
    (lambda (out)
      (write-json payload out)
      (newline out))
    #:exists 'truncate/replace))

(define (write-summary! summary #:out out-path #:matrix-out matrix-path)
  (write-json-file! out-path summary)
  (write-json-file! matrix-path
                    (hasheq 'schema_version (json-ref summary 'schema_version)
                            'status (json-ref summary 'status)
                            'generated_at_epoch (json-ref summary 'generated_at_epoch)
                            'matrix (for/hash ([key '(shards totals suite profile runner_mode)])
                                      (values key (json-ref summary key)))))
  summary)

(define (main)
  (define shards-dir #f)
  (define workflows-dir #f)
  (define platform-dir #f)
  (define out-path #f)
  (define matrix-out-path #f)
  (define test-result "unknown")
  (define workflows-result "unknown")
  (define platform-result "unknown")
  (define run-url #f)
  (define expected-shards required-shard-count)
  (command-line
   #:program "full-regression-status.rkt"
   #:once-each
   [("--shards-dir") directory "Directory containing Linux-shard JSON evidence" (set! shards-dir directory)]
   [("--workflows-dir") directory "Directory containing workflows-suite JSON evidence" (set! workflows-dir directory)]
   [("--platform-dir") directory "Directory containing macOS-platform JSON evidence" (set! platform-dir directory)]
   [("--out") path "Output path for run-summary JSON" (set! out-path path)]
   [("--matrix-out") path "Output path for matrix-summary JSON" (set! matrix-out-path path)]
   [("--test-result") result "GitHub matrix job result" (set! test-result result)]
   [("--workflows-result") result "GitHub workflows-suite job result" (set! workflows-result result)]
   [("--platform-result") result "GitHub macOS platform job result" (set! platform-result result)]
   [("--expected-shards") count "Expected Linux shard record count" (set! expected-shards (string->number count))]
   [("--run-url") url "GitHub Actions run URL" (set! run-url url)])
  (unless (and shards-dir workflows-dir platform-dir out-path matrix-out-path
               (exact-positive-integer? expected-shards))
    (raise-user-error 'full-regression-status
                      "--shards-dir, --workflows-dir, --platform-dir, --out, --matrix-out, and a positive --expected-shards are required"))
  (define summary
    (evaluate-full-regression
     #:shard-records (make-lane-records shards-dir)
     #:workflows-records (make-lane-records workflows-dir)
     #:platform-records (make-lane-records platform-dir)
     #:test-result test-result
     #:workflows-result workflows-result
     #:platform-result platform-result
     #:expected-shards expected-shards
     #:run-url run-url))
  (write-summary! summary #:out out-path #:matrix-out matrix-out-path)
  (displayln (json-ref summary 'status)))

(module+ main (main))

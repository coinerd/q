#lang racket/base

;; q/scripts/run-tests/runtime-census.rkt — v1.00.28 W0 repository-wide
;; runtime census over the canonical `fast` inventory.
;;
;; Extends the hotspot-benchmark machinery (RUNTIME-AUDIT-SPEC-v1.00.28.md):
;;  - allowlist -> full fast inventory (scripts/run-tests/classify.rkt);
;;  - >= 3 successful samples per file, every attempt retained (failures and
;;    timeouts are records, never discarded);
;;  - per-test metadata from the @speed/@suite/@boundary/@covers parser;
;;  - work-type counters via the opt-in dynamic scopes
;;    (scripts/run-tests/work-counters.rkt + tests/helpers/*); categories that
;;    no sample observed serialize as JSON null — never 0;
;;  - median (exact) / p95 (hotspot linear interpolation);
;;  - aggregates: work mass, Pareto top 10/25/50/100 + remainder, six runtime
;;    buckets, boundary split, review queues Q1..Q7, hotspot attributions;
;;  - companion static wait/subprocess scan (§5, triage input);
;;  - `--check PATH` verifies schema, inventory completeness, canonical byte
;;    identity, median/p95 self-consistency, sample floor, and SHA256SUMS;
;;  - `--refresh-sums PATH` regenerates SHA256SUMS over the artifact set.
;;
;; Modes:
;;   racket runtime-census.rkt [--samples N] [--jobs N] [--timeout-s S]
;;          [--limit N] [--out PATH]
;;   racket runtime-census.rkt --check PATH
;;   racket runtime-census.rkt --refresh-sums PATH

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "classify.rkt" collect-test-files get-file-metadata)
         (only-in "hotspot-benchmark.rkt" hotspot-percentile)
         (only-in "sha256.rkt" sha256-hex))

(provide census-record
         census-record-required-fields
         census-instrumentation-unknown
         census-work-type-counters
         census-completeness-errors
         census-aggregates
         census-static-scan
         census-canonical-bytes
         census-manifest-errors
         census-collect
         main)

;; ============================================================
;; constants
;; ============================================================

(define census-schema "q.census.fast-runtime/1")
(define static-scan-schema "q.census.static-wait-scan/1")
(define default-census-out "artifacts/test-runtime/v1.00.28-census/fast-runtime-census.json")
(define default-static-out "artifacts/test-runtime/v1.00.28-census/static-wait-scan.json")

;; RUNTIME-AUDIT-SPEC §2 instrumentation categories.
(define instrumentation-fields
  '(racket_subprocesses subprocesses
                        git_commands
                        temp_dirs
                        temp_files
                        session_fixtures
                        git_fixtures
                        worktrees
                        sleep_requested_ms
                        timeouts
                        server_or_socket_lifecycles))

;; RUNTIME-AUDIT-SPEC §5 static scan term list.
(define static-scan-terms
  (list "sleep"
        "sync/timeout"
        "alarm-evt"
        "thread-wait"
        "subprocess"
        "subprocess-wait"
        "system"
        "system*"
        "system*/exit-code"
        "git"
        "make-temporary-file"
        "make-directory"
        "make-directory*"
        "delete-directory/files"
        "current-seconds"
        "current-milliseconds"
        "retry"
        "backoff"
        "poll"
        "timeout"
        "tcp-listen"
        "unix-socket"))

(define runtime-buckets
  (list (hasheq 'label "<250 ms" 'min_ms 0 'max_ms 250)
        (hasheq 'label "250 ms-1 s" 'min_ms 250 'max_ms 1000)
        (hasheq 'label "1-2 s" 'min_ms 1000 'max_ms 2000)
        (hasheq 'label "2-5 s" 'min_ms 2000 'max_ms 5000)
        (hasheq 'label "5-10 s" 'min_ms 5000 'max_ms 10000)
        (hasheq 'label ">10 s" 'min_ms 10000 'max_ms +inf.0)))

;; ============================================================
;; canonical JSON serialization (sorted keys, stable scalars)
;; ============================================================

(define (json-str v)
  (with-output-to-string (lambda () (write-json v))))

(define (key->string k)
  (if (symbol? k)
      (symbol->string k)
      k))

;; Canonical jsexpr renderer: hash keys sorted, 'unknown -> null,
;; non-integer rationals -> inexact, symbols -> strings. Pure and stable.
(define (census-canonical-json v)
  (cond
    [(hash? v)
     (define entries
       (for/list ([k (in-list (sort (hash-keys v) string<? #:key key->string))])
         (string-append (json-str (key->string k)) ":" (census-canonical-json (hash-ref v k)))))
     (string-append "{" (string-join entries ",") "}")]
    [(pair? v) (string-append "[" (string-join (map census-canonical-json v) ",") "]")]
    [(null? v) "[]"]
    [(eq? v 'unknown) "null"]
    [(eq? v 'null) "null"]
    [(boolean? v) (if v "true" "false")]
    [(symbol? v) (json-str (symbol->string v))]
    [(string? v) (json-str v)]
    [(and (rational? v) (not (integer? v))) (number->string (exact->inexact v))]
    [(real? v) (number->string v)]
    [else (error 'census-canonical-json "unserializable value: ~a" v)]))

(define (census-canonical-bytes jsexpr)
  (string->bytes/utf-8 (string-append (census-canonical-json jsexpr) "\n")))

;; ============================================================
;; records (RUNTIME-AUDIT-SPEC §2, §3)
;; ============================================================

(define census-record-required-fields
  '(path speed suite boundary covers samples_ms median_ms p95_ms status all_attempts instrumentation))

;; All-instrumentation-unknown skeleton: unknown means "no sample observed
;; this category", which serializes as JSON null — never 0.
(define (census-instrumentation-unknown)
  (for/hash ([f (in-list instrumentation-fields)])
    (values f 'unknown)))

;; Merge per-attempt counter hashes (category -> number | 'absent) into one
;; instrumentation object: summed where observed, 'unknown where no attempt
;; observed the category.
(define (census-work-type-counters attempt-counters)
  (for/hash ([f (in-list instrumentation-fields)])
    (define vals
      (for/list ([h (in-list attempt-counters)]
                 #:when (hash? h)
                 #:do [(define v (hash-ref h f 'absent))]
                 #:unless (eq? v 'absent))
        v))
    (if (null? vals)
        (values f 'unknown)
        (values f
                (apply +
                       (map (lambda (v)
                              (if (list? v)
                                  (length v)
                                  v))
                            vals))))))

(define (census-median xs)
  (cond
    [(null? xs) 0]
    [else
     (define sorted (sort xs <))
     (define n (length sorted))
     (if (odd? n)
         (list-ref sorted (quotient n 2))
         (/ (+ (list-ref sorted (sub1 (quotient n 2))) (list-ref sorted (quotient n 2))) 2))]))

;; Build one census record from raw sampling output.
;;   samples_ms    — successful sample durations
;;   all_attempts  — EVERY attempt (pass/fail/timeout), retained verbatim
;;   meta          — hash with speed/suite/boundary/covers (or null)
(define (census-record path samples_ms all_attempts meta)
  (define sorted (sort samples_ms <))
  (define instrumentation
    (census-work-type-counters (for/list ([a (in-list all_attempts)]
                                          #:when (equal? (hash-ref a 'status) "pass"))
                                 (hash-ref a 'counters (hasheq)))))
  (hasheq 'path
          path
          'speed
          (hash-ref meta 'speed 'null)
          'suite
          (hash-ref meta 'suite 'null)
          'boundary
          (hash-ref meta 'boundary 'null)
          'covers
          (hash-ref meta 'covers 'null)
          'samples_ms
          samples_ms
          'median_ms
          (census-median sorted)
          'p95_ms
          (if (null? sorted)
              'null
              (hotspot-percentile sorted 95))
          'status
          (if (>= (length sorted) 3) "pass" "collection-failure")
          'all_attempts
          (for/list ([a (in-list all_attempts)])
            (hasheq 'attempt
                    (hash-ref a 'attempt)
                    'status
                    (hash-ref a 'status)
                    'duration_ms
                    (hash-ref a 'duration_ms)))
          'instrumentation
          instrumentation))

;; Inventory completeness: silent omissions are contract violations.
;; Every fast file needs a record (pass or explicit collection failure), and
;; every record must correspond to a real fast file.
(define (census-completeness-errors inventory-paths records)
  (define recorded
    (for/hash ([r (in-list records)])
      (values (hash-ref r 'path) r)))
  (define inventory-h
    (for/hash ([p (in-list inventory-paths)])
      (values p #t)))
  (append (for/list ([p (in-list inventory-paths)]
                     #:unless (hash-has-key? recorded p))
            (format "fast file missing from census (silent omission): ~a" p))
          (for/list ([r (in-list records)]
                     #:unless (hash-has-key? inventory-h (hash-ref r 'path)))
            (format "census record without fast inventory file: ~a" (hash-ref r 'path)))))

;; ============================================================
;; aggregates (RUNTIME-AUDIT-SPEC §4, §6, §8)
;; ============================================================

(define (pass-record? r)
  (equal? (hash-ref r 'status) "pass"))

(define (census-aggregates records #:static-scan [static-scan (hasheq)])
  (define passing (filter pass-record? records))
  (define mass (apply + (map (lambda (r) (hash-ref r 'median_ms)) passing)))
  (define ranked (sort passing > #:key (lambda (r) (hash-ref r 'median_ms)) #:cache-keys? #t))
  (define (bracket n)
    (define taken (take ranked (min n (length ranked))))
    (define m (apply + (map (lambda (r) (hash-ref r 'median_ms)) taken)))
    (hasheq 'count
            (length taken)
            'mass_ms
            m
            'pct
            (if (zero? mass)
                0.0
                (* 100.0 (/ m mass)))))
  (define pareto
    (for/fold ([acc (hasheq)]) ([n (in-list '(1 10 25 50 100))])
      (hash-set acc (string->symbol (format "top~a" n)) (bracket n))))
  (define pareto-with-remainder
    (hash-set pareto
              'remainder
              (hasheq 'count
                      (- (length ranked) (min 100 (length ranked)))
                      'mass_ms
                      (- mass (hash-ref (bracket 100) 'mass_ms)))))
  (define buckets
    (for/list ([b (in-list runtime-buckets)])
      (define in-bucket
        (filter (lambda (r)
                  (define m (hash-ref r 'median_ms))
                  (and (>= m (hash-ref b 'min_ms)) (< m (hash-ref b 'max_ms))))
                passing))
      (hasheq 'label
              (hash-ref b 'label)
              'file_count
              (length in-bucket)
              'mass_ms
              (apply + (map (lambda (r) (hash-ref r 'median_ms)) in-bucket)))))
  (define (boundary-key r)
    (define speed (hash-ref r 'speed))
    (define boundary (hash-ref r 'boundary))
    (cond
      [(equal? speed "unit-fast") "unit-fast/unit"]
      [(equal? boundary "unit") "fast/unit"]
      [(equal? boundary "integration") "fast/integration"]
      [else "fast/other"]))
  (define boundary
    ;; NOTE: string keys — must be equal-based `hash`, never `hasheq`
    ;; (eq?-based lookup on non-interned strings is nondeterministic).
    (for/fold ([acc (hash "unit-fast/unit"
                          (hasheq 'file_count 0 'mass_ms 0)
                          "fast/unit"
                          (hasheq 'file_count 0 'mass_ms 0)
                          "fast/integration"
                          (hasheq 'file_count 0 'mass_ms 0)
                          "fast/other"
                          (hasheq 'file_count 0 'mass_ms 0))])
              ([r (in-list passing)])
      (define k (boundary-key r))
      (define cur (hash-ref acc k))
      (hash-set acc
                k
                (hasheq 'file_count
                        (add1 (hash-ref cur 'file_count))
                        'mass_ms
                        (+ (hash-ref cur 'mass_ms) (hash-ref r 'median_ms))))))
  ;; §4.5 work-type totals: summed over passing records; 'unknown if no
  ;; passing record observed the category.
  (define work-type-totals
    (for/hash ([f (in-list instrumentation-fields)])
      (define vals
        (for/list ([r (in-list passing)]
                   #:do [(define v (hash-ref (hash-ref r 'instrumentation) f 'unknown))]
                   #:unless (eq? v 'unknown))
          v))
      (values f
              (if (null? vals)
                  'unknown
                  (apply + vals)))))
  ;; §6 queues.
  (define (num r f)
    (define v (hash-ref (hash-ref r 'instrumentation) f 'unknown))
    (if (number? v) v 0))
  (define (process-sum r)
    (+ (num r 'racket_subprocesses) (num r 'subprocesses) (num r 'git_commands)))
  (define (fixture-sum r)
    (+ (num r 'temp_dirs)
       (num r 'temp_files)
       (num r 'session_fixtures)
       (num r 'git_fixtures)
       (num r 'worktrees)))
  (define (has-wait-signal? r)
    (or (> (num r 'sleep_requested_ms) 0)
        (> (num r 'timeouts) 0)
        (for/or ([t (in-list (list "sleep"
                                   "sync/timeout"
                                   "alarm-evt"
                                   "thread-wait"
                                   "retry"
                                   "backoff"
                                   "poll"
                                   "timeout"))])
          (member t (hash-ref static-scan (hash-ref r 'path) '())))))
  (define (has-isolation-signal? r)
    (or (> (process-sum r) 0) (> (fixture-sum r) 0) (> (num r 'timeouts) 0) (has-wait-signal? r)))
  (define queues
    (hasheq 'q1
            (map (lambda (r) (hash-ref r 'path))
                 (filter (lambda (r) (> (hash-ref r 'median_ms) 10000)) ranked))
            'q2
            (map (lambda (r) (hash-ref r 'path))
                 (filter (lambda (r)
                           (define m (hash-ref r 'median_ms))
                           (and (> m 5000) (<= m 10000)))
                         ranked))
            'q3
            (map (lambda (r) (hash-ref r 'path))
                 (filter (lambda (r)
                           (and (equal? (hash-ref r 'speed) "fast")
                                (equal? (hash-ref r 'boundary) "integration")))
                         ranked))
            'q4
            (map (lambda (r) (hash-ref r 'path))
                 (filter (lambda (r)
                           (and (has-wait-signal? r)
                                (member (hash-ref r 'speed) '("fast" "unit-fast"))))
                         ranked))
            'q5
            (map (lambda (r) (hash-ref r 'path)) (filter (lambda (r) (>= (fixture-sum r) 8)) ranked))
            'q6
            (map (lambda (r) (hash-ref r 'path)) (filter (lambda (r) (>= (process-sum r) 10)) ranked))
            'q7
            (map (lambda (r) (hash-ref r 'path))
                 (filter (lambda (r)
                           (and (pass-record? r)
                                (or (equal? (hash-ref r 'speed) "unit-fast")
                                    (equal? (hash-ref r 'boundary) "unit"))
                                (not (has-isolation-signal? r))))
                         ranked))))
  ;; §8 attribution: rule-based, deterministic; UNKNOWN is preferred over
  ;; speculation (spec §8).
  (define (attribution r)
    (cond
      [(> (num r 'sleep_requested_ms) 0) "REAL_SLEEP"]
      [(> (num r 'timeouts) 0) "TIMEOUT_WAIT"]
      [(> (fixture-sum r) 0)
       (cond
         [(> (+ (num r 'git_fixtures) (num r 'worktrees)) 0) "GIT_FIXTURE"]
         [(> (num r 'session_fixtures) 0) "SESSION_FIXTURE"]
         [else "FILESYSTEM_FIXTURE"])]
      [(> (process-sum r) 0) "PROCESS_STARTUP"]
      [(equal? (hash-ref r 'boundary) "integration") "FULL_INTEGRATION_FLOW"]
      [else "UNKNOWN"]))
  (define attributions
    (for/list ([r (in-list (take ranked (min 25 (length ranked))))])
      (hasheq
       'path
       (hash-ref r 'path)
       'median_ms
       (hash-ref r 'median_ms)
       'primary
       (attribution r)
       'secondary
       (for/list ([pair (in-list
                         (list (cons "REAL_SLEEP" (lambda (r) (> (num r 'sleep_requested_ms) 0)))
                               (cons "TIMEOUT_WAIT" (lambda (r) (> (num r 'timeouts) 0)))
                               (cons "PROCESS_STARTUP" (lambda (r) (> (process-sum r) 0)))
                               (cons "GIT_FIXTURE"
                                     (lambda (r) (> (+ (num r 'git_fixtures) (num r 'worktrees)) 0)))
                               (cons "SESSION_FIXTURE" (lambda (r) (> (num r 'session_fixtures) 0)))
                               (cons "FILESYSTEM_FIXTURE" (lambda (r) (> (num r 'temp_dirs) 0)))))]
                  #:when ((cdr pair) r))
         (car pair)))))
  (hasheq 'work_mass_ms
          mass
          'measured_file_count
          (length passing)
          'collection_failure_count
          (- (length records) (length passing))
          'pareto
          pareto-with-remainder
          'buckets
          buckets
          'boundary
          boundary
          'work_type_totals
          work-type-totals
          'queues
          queues
          'hotspot_attributions
          attributions))

;; ============================================================
;; static scan (RUNTIME-AUDIT-SPEC §5) — triage input, not proof
;; ============================================================

(define (term-regexp term)
  (define boundary "(?<![a-zA-Z0-9_!?*<>=/.-])")
  (define tail "(?![a-zA-Z0-9_!?*<>=/-])")
  (regexp (string-append boundary (regexp-quote term) tail)))

;; Scan every .rkt test file under tests-root; keys are paths relative to the
;; parent of tests-root (e.g. "tests/test-x.rkt"); values are matched terms.
(define (census-static-scan tests-root)
  (define rel-base
    (let-values ([(base _n _d?) (split-path (simple-form-path tests-root))])
      (path->string base)))
  (for/hash ([p (in-list (sort (find-files-rec tests-root) string<? #:key path->string))]
             #:do
             [(define body (file->string p))
              (define rel
                (path->string (find-relative-path (simple-form-path rel-base) (simple-form-path p))))
              (define matched
                (for/list ([t (in-list static-scan-terms)]
                           #:when (regexp-match? (term-regexp t) body))
                  t))]
             #:unless (null? matched))
    (values rel matched)))

(define (find-files-rec root)
  (let loop ([dir root])
    (apply append
           (for/list ([e (in-list (directory-list dir #:build? #t))])
             (cond
               [(directory-exists? e)
                (if (string-contains? (path->string e) "compiled")
                    '()
                    (loop e))]
               [(and (file-exists? e) (string-suffix? (path->string e) ".rkt")) (list e)]
               [else '()])))))

;; ============================================================
;; sampling runner
;; ============================================================

;; work-counters.rkt arms a test process via the marker file
;; `<cwd>/.q-census-counters` containing the counters directory path; the
;; process dumps one {kind: n} JSON object per sample into that directory at
;; plumber flush. Canonical census runs therefore use --jobs 1 so each
;; attempt owns the marker for its full child lifetime (race-free counter
;; attribution); higher job counts are smoke-only.
(define (read-attempt-counters cdir)
  (define merged (make-hasheq))
  (with-handlers ([exn:fail? void])
    (for ([f (in-list (directory-list cdir #:build? #t))]
          #:when (string-suffix? (path->string f) ".json"))
      (with-handlers ([exn:fail? void])
        (define obj (read-json (open-input-file f)))
        (when (hash? obj)
          (for ([(k v) (in-hash obj)]
                #:when (symbol? (string->symbol (format "~a" k)))
                #:when (real? v))
            (define sym (string->symbol (format "~a" k)))
            (hash-update! merged sym (lambda (old) (+ old (inexact->exact (floor v)))) 0))))))
  (for/hash ([f (in-list instrumentation-fields)])
    (if (hash-has-key? merged f)
        (values f (hash-ref merged f))
        (values f 'absent))))

(define (run-attempt file round timeout-s counters-root)
  (define racket-path (find-executable-path "racket"))
  (define target (path->string (build-path (current-directory) file)))
  (define slug (regexp-replace* #rx"[^a-zA-Z0-9._-]" file "_"))
  (define cdir (build-path counters-root (format "~a.r~a" slug round)))
  (make-directory* cdir)
  (define marker (build-path (current-directory) ".q-census-counters"))
  (with-output-to-file marker (lambda () (displayln (path->string cdir))) #:exists 'replace)
  (define started (current-inexact-milliseconds))
  (define stdout-p (open-output-file "/dev/null" #:exists 'append))
  (define-values (proc _i _o _e) (subprocess stdout-p #f (current-error-port) racket-path target))
  (define status
    (let ([ready (sync/timeout timeout-s proc)])
      (cond
        [ready (if (zero? (subprocess-status proc)) "pass" "fail")]
        [else
         (with-handlers ([exn:fail? void])
           (subprocess-kill proc #t))
         "timeout"])))
  (sync/timeout 5 proc)
  (define duration (inexact->exact (floor (- (current-inexact-milliseconds) started))))
  (close-output-port stdout-p)
  (with-handlers ([exn:fail? void])
    (delete-file marker))
  (define counters (read-attempt-counters cdir))
  (with-handlers ([exn:fail? void])
    (delete-directory/files cdir))
  (hasheq 'attempt round 'status status 'duration_ms duration 'counters counters))

;; Per-file rounds: sample i of every family starts no earlier than sample
;; i-1 of the same family completes (same invariant as the hotspot tool).
;; BUG-0066 recovery: emit one flushed progress line per attempt so a
;; multi-hour serial run is never indistinguishable from a hang — the
;; 2026-09-08 crash began with a silent census log misread as a zombie.
(define (census-collect files #:samples [samples 3] #:jobs [jobs 4] #:timeout-s [timeout-s 300])
  (define counters-root (make-temporary-file "q-census-counters-~a" 'directory))
  (define records (make-hash)) ; file -> attempts, newest first
  (define census-started (current-inexact-milliseconds))
  (define attempts-done (box 0))
  (define total-attempts (* samples (length files)))
  (for ([round (in-range 1 (add1 samples))])
    (define sema (make-semaphore (max 1 jobs)))
    (define results (make-hash))
    (define threads
      (for/list ([f (in-list files)])
        (thread (lambda ()
                  (semaphore-wait sema)
                  (define res
                    (with-handlers ([exn:fail? (lambda (e)
                                                 (hasheq 'attempt
                                                         round
                                                         'status
                                                         "collection-failure"
                                                         'duration_ms
                                                         0
                                                         'counters
                                                         (hasheq)
                                                         'error
                                                         (exn-message e)))])
                      (run-attempt f round timeout-s counters-root)))
                  (hash-set! results f res)
                  (set-box! attempts-done (add1 (unbox attempts-done)))
                  (define elapsed-s (/ (- (current-inexact-milliseconds) census-started) 1000.0))
                  (fprintf (current-output-port)
                           "census: [~a/~a round ~a/~a] ~a — ~a (~ams, ~as elapsed)\n"
                           (unbox attempts-done)
                           total-attempts
                           round
                           samples
                           f
                           (hash-ref res 'status "?")
                           (hash-ref res 'duration_ms 0)
                           (~r elapsed-s #:precision 1))
                  (flush-output)
                  (semaphore-post sema)))))
    (for ([t (in-list threads)])
      (thread-wait t))
    (for ([f (in-list files)])
      (hash-set! records f (cons (hash-ref results f) (hash-ref records f '())))))
  (with-handlers ([exn:fail? void])
    (delete-directory/files counters-root))
  (for/list ([f (in-list (sort files string<?))])
    (define attempts (reverse (hash-ref records f '())))
    (define samples-ms
      (for/list ([a (in-list attempts)]
                 #:when (equal? (hash-ref a 'status) "pass"))
        (hash-ref a 'duration_ms)))
    (define meta
      (with-handlers ([exn:fail? (lambda (_) (hasheq))])
        (get-file-metadata f)))
    (census-record f samples-ms attempts meta)))

;; ============================================================
;; manifest schema (--check contract)
;; ============================================================

(define (census-manifest-errors m)
  (define (err s)
    s)
  (append
   (if (equal? (hash-ref m 'schema #f) census-schema)
       '()
       (list (err (format "schema mismatch: expected ~a" census-schema))))
   (if (list? (hash-ref m 'records #f))
       '()
       (list (err "records must be a list")))
   (if (hash? (hash-ref m 'inventory #f))
       '()
       (list (err "inventory must be an object")))
   (if (hash? (hash-ref m 'aggregates #f))
       '()
       (list (err "aggregates must be an object")))
   (if (hash? (hash-ref m 'static_scan #f))
       '()
       (list (err "static_scan must be an object")))
   (if (list? (hash-ref m 'collection_failures #f))
       '()
       (list (err "collection_failures must be a list")))
   (append
    (for/list ([r (in-list (hash-ref m 'records '()))]
               #:unless (hash? r))
      (err "record is not an object"))
    (for/list ([r (in-list (filter hash? (hash-ref m 'records '())))]
               [f (in-list
                   (for/list ([r (in-list (filter hash? (hash-ref m 'records '())))])
                     (append (for/list ([k (in-list census-record-required-fields)]
                                        #:unless (hash-has-key? r k))
                               (format "~a: record missing field ~a" (hash-ref r 'path "?") k))
                             (if (hash? (hash-ref r 'instrumentation #f))
                                 (for/list ([k (in-list instrumentation-fields)]
                                            #:unless (hash-has-key? (hash-ref r 'instrumentation) k))
                                   (format "~a: instrumentation missing ~a" (hash-ref r 'path "?") k))
                                 (list (format "~a: instrumentation must be an object"
                                               (hash-ref r 'path "?")))))))]
               #:unless (null? f))
      (first f)))))

;; ============================================================
;; SHA256SUMS
;; ============================================================

(define (sums-candidates)
  (list default-census-out
        default-static-out
        "docs/reports/TEST-WORK-MASS-v1.00.28.md"
        "docs/reports/gsd-wave-evidence/v1.00.28-w0.rktd"
        "docs/reports/gsd-wave-reviews/v1.00.28-w0.rktd"
        "docs/reports/gsd-wave-validation/v1.00.28-w0.rktd"))

;; Write SHA256SUMS (paths relative to the repo root = cwd) for all
;; artifact-set members that currently exist. Sums files that already exist
;; are always regenerated so the set is internally consistent.
(define (write-sha256sums! sums-path)
  (define lines
    (for/list ([rel (in-list (sums-candidates))]
               #:when (file-exists? rel))
      (format "~a  ~a\n" (sha256-hex (file->bytes rel)) rel)))
  (unless (null? lines)
    (make-directory* (path-only sums-path))
    (with-output-to-file sums-path
                         (lambda ()
                           (for ([l (in-list lines)])
                             (display l)))
                         #:exists 'replace)))

;; Verify every entry of SHA256SUMS; returns list of errors.
(define (verify-sha256sums sums-path)
  (for/list ([line (in-list (file->lines sums-path))]
             #:unless (string-prefix? line "#"))
    (match (string-split line "  ")
      [(list hex rel)
       (cond
         [(not (file-exists? rel)) (format "SHA256SUMS: missing artifact ~a" rel)]
         [(not (equal? hex (sha256-hex (file->bytes rel))))
          (format "SHA256SUMS: checksum mismatch for ~a" rel)]
         [else #f])]
      [else (format "SHA256SUMS: malformed line: ~a" line)])))

;; ============================================================
;; --check
;; ============================================================

(define (census-check census-path)
  (define errors
    (append
     (let ([raw (file->bytes census-path)])
       (define m
         (with-handlers ([exn:fail? (lambda (_) #f)])
           (read-json (open-input-bytes raw))))
       (cond
         [(not (hash? m)) (list "census: file is not a JSON object")]
         [else
          (append
           (census-manifest-errors m)
           ;; canonical byte identity: stored file must equal its canonical
           ;; re-serialization (no hand-edits, no drift).
           (let ([expected (census-canonical-bytes m)])
             (if (equal? expected raw)
                 '()
                 (list "census: stored bytes are not canonical JSON")))
           ;; inventory completeness against the live fast inventory.
           (let ([live (sort (collect-test-files 'fast) string<?)]
                 [recorded (append (hash-ref m 'records '()) (hash-ref m 'collection_failures '()))])
             (census-completeness-errors live recorded))
           ;; sample floor + median/p95 self-consistency.
           (append (for/list ([r (in-list (hash-ref m 'records '()))]
                              #:when (equal? (hash-ref r 'status) "pass")
                              #:unless (>= (length (hash-ref r 'samples_ms '()))
                                           (hash-ref m 'samples_per_file 3)))
                     (format "~a: fewer successful samples than the floor" (hash-ref r 'path)))
                   (for/list ([r (in-list (hash-ref m 'records '()))]
                              #:unless (equal? (hash-ref r 'median_ms)
                                               (census-median (hash-ref r 'samples_ms '()))))
                     (format "~a: stored median does not match samples" (hash-ref r 'path)))
                   (for/list ([r (in-list (hash-ref m 'records '()))]
                              #:when (and (pass-record? r) (not (null? (hash-ref r 'samples_ms '()))))
                              #:unless (equal? (hash-ref r 'p95_ms)
                                               (hotspot-percentile (hash-ref r 'samples_ms) 95)))
                     (format "~a: stored p95 does not match samples" (hash-ref r 'path)))))]))
     (let ([sums (path-replace-extension census-path #".json")])
       ;; SHA256SUMS sits next to the census JSON.
       (define sums-path (build-path (path-only (simple-form-path census-path)) "SHA256SUMS"))
       (if (file-exists? sums-path)
           (filter values (verify-sha256sums sums-path))
           (list "census: SHA256SUMS missing")))))
  (if (null? errors)
      (begin
        (printf "census-check: PASS ~a\n" census-path)
        0)
      (begin
        (for ([e (in-list errors)])
          (eprintf "census-check: ~a\n" e))
        1)))

;; ============================================================
;; main
;; ============================================================

(define (q-sha-short)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define p (open-input-string (with-output-to-string (lambda () (system "git rev-parse HEAD")))))
    (string-trim (read-line p))))

(define (main)
  (define check-path (make-parameter #f))
  (define refresh-sums (make-parameter #f))
  (define samples-n (make-parameter 3))
  (define jobs-n (make-parameter 1))
  (define timeout-s (make-parameter 300))
  (define limit-n (make-parameter #f))
  (define out-p (make-parameter default-census-out))
  (command-line
   #:program "runtime-census"
   #:once-each ["--check" p "verify an existing census artifact" (check-path p)]
   ["--refresh-sums" p "regenerate SHA256SUMS for the artifact set" (refresh-sums p)]
   ["--samples" n "successful samples per file (default 3)" (samples-n (string->number n))]
   ["--jobs"
    n
    "parallel test subprocesses (default 1 = race-free counters)"
    (jobs-n (string->number n))]
   ["--timeout-s" n "per-attempt timeout seconds (default 300)" (timeout-s (string->number n))]
   ["--limit" n "collect only the first N files (smoke runs only)" (limit-n (string->number n))]
   ["--out" p "census output path" (out-p p)]
   #:args ()
   (void))
  (cond
    [(check-path) (exit (census-check (check-path)))]
    [(refresh-sums)
     (write-sha256sums! "artifacts/test-runtime/v1.00.28-census/SHA256SUMS")
     (printf "refreshed SHA256SUMS\n")]
    [else
     (define files (collect-test-files 'fast))
     (when (limit-n)
       (set! files (take files (min (limit-n) (length files)))))
     (printf "census: fast inventory = ~a files, ~a samples, jobs ~a\n"
             (length files)
             (samples-n)
             (jobs-n))
     (define records
       (census-collect files #:samples (samples-n) #:jobs (jobs-n) #:timeout-s (timeout-s)))
     (define failures
       (for/list ([r (in-list records)]
                  #:when (equal? (hash-ref r 'status) "collection-failure"))
         (hasheq 'path (hash-ref r 'path) 'status "collection-failure")))
     (define completeness-errors (census-completeness-errors files records))
     (for ([e (in-list completeness-errors)])
       (eprintf "census: COMPLETENESS ~a\n" e))
     ;; static companion scan (§5).
     (printf "census: static wait/subprocess scan over tests/ ...\n")
     (define scan (census-static-scan (build-path (current-directory) "tests")))
     (define total-matches (for/sum ([(_ terms) (in-hash scan)]) (length terms)))
     (define static-manifest
       (hasheq 'schema
               static-scan-schema
               'q_sha
               (q-sha-short)
               'terms
               static-scan-terms
               'note
               "triage input, not proof of a runtime defect (RUNTIME-AUDIT-SPEC §5)"
               'total_matches
               total-matches
               'files
               scan))
     (define manifest
       (hasheq 'schema
               census-schema
               'q_sha
               (q-sha-short)
               'command
               (list "racket"
                     "scripts/run-tests/runtime-census.rkt"
                     "--samples"
                     (~a (samples-n))
                     "--jobs"
                     (~a (jobs-n)))
               'suite
               "fast"
               'samples_per_file
               (samples-n)
               'jobs
               (jobs-n)
               'inventory
               (hasheq 'source "collect-test-files 'fast" 'file_count (length files))
               'records
               records
               'collection_failures
               failures
               'completeness_errors
               completeness-errors
               'aggregates
               (census-aggregates records #:static-scan scan)
               'static_scan
               (hasheq 'schema
                       static-scan-schema
                       'total_matches
                       total-matches
                       'matched_file_count
                       (hash-count scan)
                       'ref
                       "static-wait-scan.json")))
     (define out (out-p))
     (define out-dir (path-only (simplify-path (build-path (current-directory) out))))
     (when out-dir
       (make-directory* out-dir))
     (with-output-to-file out
                          (lambda () (write-bytes (census-canonical-bytes manifest)))
                          #:exists 'replace)
     (define static-out default-static-out)
     (define static-dir (path-only (simplify-path (build-path (current-directory) static-out))))
     (when static-dir
       (make-directory* static-dir))
     (with-output-to-file static-out
                          (lambda () (write-bytes (census-canonical-bytes static-manifest)))
                          #:exists 'replace)
     (write-sha256sums! "artifacts/test-runtime/v1.00.28-census/SHA256SUMS")
     ;; summary
     (define ag (hash-ref manifest 'aggregates))
     (printf "census: wrote ~a (~a bytes)\n" out (file-size (build-path (current-directory) out)))
     (printf "census: wrote ~a (matched files: ~a, matches: ~a)\n"
             static-out
             (hash-count scan)
             total-matches)
     (printf "census: fast work mass = ~a ms over ~a measured files (~a collection failures)\n"
             (hash-ref ag 'work_mass_ms)
             (hash-ref ag 'measured_file_count)
             (hash-ref ag 'collection_failure_count))
     (define top10 (hash-ref (hash-ref ag 'pareto) 'top10))
     (printf "census: top10 = ~a ms (~a%)\n"
             (hash-ref top10 'mass_ms)
             (~r (hash-ref top10 'pct) #:precision 2))
     (define queues (hash-ref ag 'queues))
     (for ([q (in-list '(q1 q2 q3 q4 q5 q6 q7))])
       (printf "census: ~a = ~a files\n" q (length (hash-ref queues q))))
     (unless (null? completeness-errors)
       (eprintf "census: FAILED inventory completeness\n")
       (exit 1))]))

(module+ main
  (main))

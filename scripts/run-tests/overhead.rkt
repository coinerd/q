#lang racket/base

;; q/scripts/run-tests/overhead.rkt — test-runner overhead diagnostics
;;
;; W0 of v0.99.30: measure fixed-cost process/test startup overhead so
;; local/VPS/CI broad-suite behavior can be reported with evidence instead
;; of conjecture.

(require racket/file
         racket/format
         racket/future
         json
         racket/list
         racket/match
         racket/math
         racket/path
         racket/port
         racket/string
         racket/system
         (only-in "profiles.rkt"
                  test-file-area
                  grouped-area-config
                  grouped-area-config->jsexpr
                  area-grouped-decision))

(provide (struct-out overhead-result)
         make-overhead-result
         run-overhead-command
         collect-overhead-diagnostics
         format-overhead-result
         print-overhead-diagnostics
         ;; v1.00.27 W4 (#9592): L0/L1/L2 local feedback telemetry
         local-p90-min-samples
         local-loop-p90
         slo-verdict
         machine-context
         local-feedback-loops
         area-mode-rows
         collect-local-p90
         check-local-p90-record
         check-local-p90-file
         implementation-sha)

(struct overhead-result (label command exit-code elapsed-ms stdout stderr) #:transparent)

(define (make-overhead-result label command exit-code elapsed-ms stdout stderr)
  (overhead-result label command exit-code elapsed-ms stdout stderr))

(define (now-ms)
  (current-inexact-milliseconds))

(define (elapsed-since t0)
  (exact-round (- (now-ms) t0)))

(define (run-overhead-command label executable args #:cwd [cwd (current-directory)])
  (define exe-path (find-executable-path executable))
  (unless exe-path
    (error 'run-overhead-command "executable not found: ~a" executable))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  (define command-text (string-join (cons executable args) " "))
  (define t0 (now-ms))
  (define-values (_proc stdin _pid _stderr ctrl)
    (parameterize ([current-directory cwd])
      (apply values (apply process*/ports stdout-out #f stderr-out exe-path args))))
  (when stdin
    (close-output-port stdin))
  (ctrl 'wait)
  (define exit-code (ctrl 'exit-code))
  (make-overhead-result label
                        command-text
                        exit-code
                        (elapsed-since t0)
                        (get-output-string stdout-out)
                        (get-output-string stderr-out)))

(define (write-file path content)
  (call-with-output-file path #:exists 'replace (lambda (out) (display content out))))

(define (collect-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (define temp-dir (make-temporary-file "q-run-tests-overhead-~a" 'directory))
  (define empty-file (build-path temp-dir "empty.rkt"))
  (define rackunit-file (build-path temp-dir "rackunit-empty.rkt"))
  (write-file empty-file "#lang racket/base\n(void)\n")
  (write-file rackunit-file
              (string-append
               "#lang racket\n"
               "(require rackunit rackunit/text-ui)\n"
               "(run-tests (test-suite \"empty\" (test-case \"ok\" (check-true #t))))\n"))
  (define representative (build-path base-dir "tests" "test-version.rkt"))
  (define commands
    (append
     (list (list "racket-noop" "racket" (list "-e" "(void)"))
           (list "racket-empty" "racket" (list (path->string empty-file)))
           (list "raco-empty" "raco" (list "test" (path->string empty-file)))
           (list "raco-rackunit-empty" "raco" (list "test" (path->string rackunit-file))))
     (if (file-exists? representative)
         (list (list "raco-representative-test" "raco" (list "test" (path->string representative))))
         '())))
  (dynamic-wind void
                (lambda ()
                  (for/list ([cmd (in-list commands)])
                    (match cmd
                      [(list label exe args) (run-overhead-command label exe args #:cwd base-dir)])))
                (lambda ()
                  (when (directory-exists? temp-dir)
                    (delete-directory/files temp-dir)))))

(define (format-overhead-result r)
  (format "  ~a: ~ams exit=~a cmd=~a"
          (overhead-result-label r)
          (overhead-result-elapsed-ms r)
          (overhead-result-exit-code r)
          (overhead-result-command r)))

(define (print-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "              TEST RUNNER OVERHEAD DIAGNOSTIC")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Base dir:   ~a~n" (path->string (simplify-path base-dir)))
  (printf "  Racket:     ~a~n" (or (find-executable-path "racket") "not found"))
  (printf "  raco:       ~a~n" (or (find-executable-path "raco") "not found"))
  (newline)
  (define results (collect-overhead-diagnostics #:base-dir base-dir))
  (for ([r (in-list results)])
    (displayln (format-overhead-result r)))
  (newline)
  (displayln "Interpretation:")
  (displayln "  - racket-noop approximates raw Racket process startup cost.")
  (displayln "  - raco-empty approximates per-file raco test harness overhead.")
  (displayln "  - raco-rackunit-empty approximates parser-visible rackunit overhead.")
  (displayln "  - raco-representative-test samples one normal project test when available.")
  (displayln "═══════════════════════════════════════════════════════════")
  results)

;; ── v1.00.27 W4 (#9592): L0/L1/L2 local feedback telemetry ─────────────
;; Real p90 evidence for the three local feedback loops of
;; docs/TEST_CONVENTIONS.md: L0 = single test file (≤5s), L1 = direct unit
;; impact (≤30s), L2 = transitive impact = the unit-fast tier (≤120s).
;; The collector measures representative invocations ≥20 times per loop
;; (first sample cold, remaining samples warm, every sample a fresh
;; run-tests process), stamps machine context and the W3-governed
;; grouped/subprocess mode per area, and computes every SLO verdict from
;; the samples. Verdicts are never hand-written: --check re-derives p90
;; (linear interpolation) and the verdict from the recorded samples and
;; rejects hand-editing, missing fields, failed samples, sample counts
;; below the minimum, and any area mode contradicting the W3
;; grouped-expansion configuration.

(define local-p90-min-samples 20)

;; v1.00.27-w4: L2 local p90 SLO adjusted 120s -> 240s through the governed
;; evidence record artifacts/tier-ownership/v1.00.27-w4/slo-evidence-record.md
;; (measured p90 233.2s over 20 samples of the 943-file unit-fast tier).
(define local-feedback-slo-table (list (cons "L0" 5000) (cons "L1" 30000) (cons "L2" 240000)))

(define (slo-budget-for label)
  (cond
    [(assoc label local-feedback-slo-table)
     =>
     cdr]
    [else #f]))

(define (percentile-rank xs p)
  ;; Linear interpolation on the sorted sample list: rank = p*(n-1).
  (define sorted (sort (map exact->inexact xs) <))
  (define n (length sorted))
  (when (zero? n)
    (error 'percentile-rank "empty sample list"))
  (define rank (* p (sub1 n)))
  (define lo (floor rank))
  (define hi (ceiling rank))
  (define vlo (list-ref sorted (inexact->exact lo)))
  (if (= lo hi)
      vlo
      (+ vlo (* (- rank lo) (- (list-ref sorted (inexact->exact hi)) vlo)))))

(define (local-loop-p90 elapsed-ms-list)
  (exact-round (percentile-rank elapsed-ms-list 0.9)))

(define (slo-verdict p90-ms budget-ms)
  (if (<= p90-ms budget-ms) "meet" "miss"))

(define (machine-context)
  (define loadavg-path "/proc/loadavg")
  (define-values (l1 l5 l15)
    (if (file-exists? loadavg-path)
        (let* ([parts (string-split (string-trim (file->string loadavg-path)) " ")])
          (values (string->number (first parts))
                  (string->number (second parts))
                  (string->number (third parts))))
        (values #f #f #f)))
  (hasheq 'cpu-count
          (processor-count)
          'load-1
          (or l1 0.0)
          'load-5
          (or l5 0.0)
          'load-15
          (or l15 0.0)
          'platform
          (symbol->string (system-type))
          'racket
          (version)))

(define (local-feedback-loops base-dir)
  (define (loop-def label description argv paths)
    (hasheq 'loop
            label
            'description
            description
            'argv
            argv
            'paths
            paths
            'slo-ms
            (slo-budget-for label)))
  (list
   (loop-def "L0"
             "current behavior: the single test file being edited (explicit-file mode)"
             (list "racket" "scripts/run-tests.rkt" "tests/ci/metadata-discovery-test.rkt")
             (list "tests/ci/metadata-discovery-test.rkt"))
   (loop-def "L1"
             "direct unit impact: direct tests of changed modules + required local contract tests"
             (list "racket"
                   "scripts/run-tests.rkt"
                   "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"
                   "tests/test-version.rkt")
             (list "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"
                   "tests/test-version.rkt"))
   (loop-def "L2"
             "transitive impact: the full unit-fast developer iteration tier"
             (list "racket" "scripts/run-tests.rkt" "--suite" "unit-fast")
             (list "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"))))

(define (expected-area-mode area-string)
  ;; grouped-area-config is string-keyed (test-file-area convention).
  (define policy (hash-ref (grouped-area-config) area-string 'subprocess))
  (if (eq? policy 'grouped) "grouped" "subprocess"))

(define (area-mode-rows paths #:base-dir [base-dir (current-directory)])
  (parameterize ([current-directory base-dir])
    (for/list ([p (in-list paths)])
      (define-values (mode fallback-reason) (area-grouped-decision p))
      (define area (test-file-area p))
      (hasheq 'path
              (if (string? p)
                  p
                  (path->string p))
              'area
              (if (symbol? area)
                  (symbol->string area)
                  area)
              'mode
              (if (eq? mode 'grouped) "grouped" "subprocess")
              'fallback-reason
              (and fallback-reason (symbol->string fallback-reason))))))

(define (grouped-config-jsexpr)
  (define cfg (grouped-area-config->jsexpr))
  (hasheq 'source
          "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json"
          'default-mode
          "subprocess"
          'areas
          (for/list ([(area policy) (in-hash cfg)])
            (hasheq 'area
                    (if (symbol? area)
                        (symbol->string area)
                        area)
                    'mode
                    (if (member policy '(grouped "grouped")) "grouped" "subprocess")))))

(define (implementation-sha #:base-dir [base-dir (current-directory)])
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "warning: implementation-sha: git rev-parse failed: ~a\n"
                                        (exn-message e))
                               "unknown")])
    (define r (run-overhead-command "git rev-parse" "git" (list "rev-parse" "HEAD") #:cwd base-dir))
    (define line (string-trim (overhead-result-stdout r)))
    (if (regexp-match? #px"^[0-9a-f]{40,64}$" line)
        line
        (begin
          (eprintf "warning: implementation-sha: unexpected rev-parse output ~s\n" line)
          "unknown"))))

(define (iso-utc-now)
  (define d (seconds->date (current-seconds) #t))
  (define (pad2 x)
    (~r x #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (collect-local-p90 #:base-dir [base-dir (current-directory)]
                           #:loops [loops #f]
                           #:samples-per-loop [samples-per-loop local-p90-min-samples]
                           #:cold-samples [cold-samples 1])
  (define loop-defs (or loops (local-feedback-loops base-dir)))
  (define mc (machine-context))
  (define loop-records
    (for/list ([l (in-list loop-defs)])
      (define label (hash-ref l 'loop))
      (define argv (hash-ref l 'argv))
      (define samples
        (for/list ([i (in-range samples-per-loop)])
          (define start (if (< i cold-samples) "cold" "warm"))
          (define r (run-overhead-command label (first argv) (rest argv) #:cwd base-dir))
          (unless (zero? (overhead-result-exit-code r))
            (eprintf "warning: ~a sample ~a exited ~a\n"
                     label
                     (add1 i)
                     (overhead-result-exit-code r)))
          (hasheq 'i
                  (add1 i)
                  'start
                  start
                  'elapsed-ms
                  (overhead-result-elapsed-ms r)
                  'exit-code
                  (overhead-result-exit-code r))))
      (define p90 (local-loop-p90 (map (lambda (s) (hash-ref s 'elapsed-ms)) samples)))
      (define slo (hash-ref l 'slo-ms))
      (hasheq 'loop
              label
              'description
              (hash-ref l 'description)
              'command
              (string-join argv " ")
              'paths
              (hash-ref l 'paths)
              'area-modes
              (area-mode-rows (hash-ref l 'paths) #:base-dir base-dir)
              'sample-count
              (length samples)
              'samples
              samples
              'p90-ms
              p90
              'slo-ms
              slo
              'verdict
              (slo-verdict p90 slo))))
  (hasheq
   'version
   1
   'wave
   "v1.00.27-w4"
   'ticket
   "#9592"
   'generated-at
   (iso-utc-now)
   'implementation-sha
   (implementation-sha #:base-dir base-dir)
   'method
   (hasheq
    'p90
    "linear interpolation"
    'rank
    "0.9*(n-1)"
    'cold-samples
    cold-samples
    'warm-samples
    (max 0 (- samples-per-loop cold-samples))
    'note
    "each sample is a fresh run-tests process; cold = first invocation of the loop in this collection, warm = subsequent invocations")
   'machine
   mc
   'grouped-config
   (grouped-config-jsexpr)
   'loops
   loop-records))

(define (check-local-p90-record rec)
  (define violations '())
  (define (bad fmt . args)
    (set! violations (cons (apply format fmt args) violations)))
  (define (need h field ctx)
    (unless (and (hash? h) (hash-has-key? h field))
      (bad "~a: missing required field: ~a" ctx field)))
  (if (not (hash? rec))
      (bad "record is not a JSON object")
      (let ()
        (for ([f (in-list '(version wave
                                    ticket
                                    generated-at
                                    implementation-sha
                                    method
                                    machine
                                    grouped-config
                                    loops))])
          (need rec f "record"))
        (define mc (hash-ref rec 'machine #f))
        (when (hash? mc)
          (for ([f (in-list '(cpu-count load-1 load-5 load-15 platform racket))])
            (need mc f "machine")))
        (define sha (hash-ref rec 'implementation-sha #f))
        (when (string? sha)
          (unless (regexp-match? #px"^[0-9a-f]{40,64}$" sha)
            (bad "implementation-sha ~a is not a 40/64-hex commit sha" sha)))
        (define method (hash-ref rec 'method #f))
        (when (hash? method)
          (for ([f (in-list '(p90 rank))])
            (need method f "method")))
        (define gcfg (hash-ref rec 'grouped-config #f))
        (when (hash? gcfg)
          (for ([f (in-list '(source default-mode areas))])
            (need gcfg f "grouped-config"))
          (when (hash-has-key? gcfg 'source)
            (unless (equal? (hash-ref gcfg 'source)
                            "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json")
              (bad "grouped-config source must reference the W3 governed expansion artifact")))
          (define areas (hash-ref gcfg 'areas #f))
          (when (list? areas)
            (unless (pair? areas)
              (bad "grouped-config: areas must not be empty"))
            (for ([row (in-list areas)])
              (need row 'area "grouped-config area row")
              (need row 'mode "grouped-config area row"))))
        (define loops (hash-ref rec 'loops #f))
        (unless (and (list? loops) (= (length loops) 3))
          (bad "record: loops must contain exactly L0, L1, L2"))
        (when (and (list? loops) (= (length loops) 3))
          (define labels (map (lambda (l) (hash-ref l 'loop #f)) loops))
          (unless (equal? labels '("L0" "L1" "L2"))
            (bad "record: loops must be labeled L0, L1, L2 (got ~a)" labels)))
        (when (list? loops)
          (for ([l (in-list loops)])
            (define label (hash-ref l 'loop "?"))
            (for ([f (in-list '(loop description
                                     command
                                     paths
                                     area-modes
                                     sample-count
                                     samples
                                     p90-ms
                                     slo-ms
                                     verdict))])
              (need l f label))
            (define count (hash-ref l 'sample-count #f))
            (when (and (hash-has-key? l 'sample-count) (not (integer? count)))
              (bad "~a: sample-count must be an integer" label))
            (when (and (integer? count) (< count local-p90-min-samples))
              (bad "~a: sample-count ~a is below the minimum of ~a real telemetry samples"
                   label
                   count
                   local-p90-min-samples))
            (define samples (hash-ref l 'samples #f))
            (when (list? samples)
              (when (and (integer? count) (not (= (length samples) count)))
                (bad "~a: sample-count ~a does not match ~a recorded samples"
                     label
                     count
                     (length samples)))
              (for ([s (in-list samples)])
                (for ([f (in-list '(i start elapsed-ms exit-code))])
                  (need s f (format "~a sample" label)))
                (unless (member (hash-ref s 'start #f) '("cold" "warm"))
                  (bad "~a sample ~a: start must be cold or warm" label (hash-ref s 'i "?")))
                (define ms (hash-ref s 'elapsed-ms #f))
                (unless (and (integer? ms) (>= ms 0))
                  (bad "~a sample ~a: elapsed-ms must be a non-negative integer"
                       label
                       (hash-ref s 'i "?")))
                (unless (equal? (hash-ref s 'exit-code #f) 0)
                  (bad "~a sample ~a: exit-code ~a is not 0 (failed runs are not latency evidence)"
                       label
                       (hash-ref s 'i "?")
                       (hash-ref s 'exit-code "?")))))
            (define elapsed-list
              (and (list? samples)
                   (not (null? samples))
                   (andmap (lambda (s) (and (hash? s) (hash-has-key? s 'elapsed-ms))) samples)
                   (map (lambda (s) (hash-ref s 'elapsed-ms)) samples)))
            (define slo (hash-ref l 'slo-ms #f))
            (when (hash-has-key? l 'slo-ms)
              (unless (equal? slo (slo-budget-for label))
                (bad "~a: slo-ms ~a contradicts the declared budget ~a"
                     label
                     slo
                     (slo-budget-for label))))
            (when (list? elapsed-list)
              (define recomputed (local-loop-p90 elapsed-list))
              (define recorded-p90 (hash-ref l 'p90-ms #f))
              (when (integer? recorded-p90)
                (unless (= recorded-p90 recomputed)
                  (bad
                   "~a: p90-ms ~a does not match recomputed p90 ~a (linear interpolation over samples)"
                   label
                   recorded-p90
                   recomputed)))
              (when (integer? slo)
                (define expected-verdict (slo-verdict recomputed slo))
                (define recorded-verdict (hash-ref l 'verdict #f))
                (unless (equal? recorded-verdict expected-verdict)
                  (bad
                   "~a: verdict '~a' is not the computed verdict '~a' (SLO verdicts are computed, never hand-written)"
                   label
                   recorded-verdict
                   expected-verdict))))
            (define rows (hash-ref l 'area-modes #f))
            (unless (and (list? rows) (pair? rows))
              (bad "~a: area-modes must list the grouped/subprocess mode per area" label))
            (when (list? rows)
              (for ([row (in-list rows)])
                (define area (hash-ref row 'area #f))
                (define mode (hash-ref row 'mode #f))
                (unless (string? area)
                  (bad "~a: area-mode row is missing area" label))
                (unless (member mode '("grouped" "subprocess"))
                  (bad "~a: area-mode row for ~a has invalid mode ~a" label area mode))
                (when (and (string? area) (member mode '("grouped" "subprocess")))
                  (define expected (expected-area-mode area))
                  (unless (equal? mode expected)
                    (bad
                     "~a: area mode for ~a contradicts the W3 grouped configuration (recorded ~a, governed ~a)"
                     label
                     area
                     mode
                     expected)))))))))
  (reverse violations))

(define (check-local-p90-file path)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (list (format "local-p90 check could not read ~a: ~a" path (exn-message e))))])
    (define rec (with-input-from-file path read-json))
    (check-local-p90-record rec)))

(module+ main
  (define argv (current-command-line-arguments))
  (define (usage)
    (displayln "usage: racket scripts/run-tests/overhead.rkt --check <local-p90.json>")
    (displayln
     "       racket scripts/run-tests/overhead.rkt --collect-local [--samples N] [--out PATH] [--loop L0|L1|L2]"))
  (cond
    [(and (>= (vector-length argv) 2) (equal? (vector-ref argv 0) "--check"))
     (define path (vector-ref argv 1))
     (define violations (check-local-p90-file path))
     (if (null? violations)
         (begin
           (printf "local-p90 OK: ~a (loops L0/L1/L2 verified, verdicts recomputed)\n" path)
           (exit 0))
         (begin
           (printf "local-p90 check FAILED for ~a:\n" path)
           (for ([v (in-list violations)])
             (printf "  - ~a\n" v))
           (exit 1)))]
    [(and (>= (vector-length argv) 1) (equal? (vector-ref argv 0) "--collect-local"))
     (define samples local-p90-min-samples)
     (define out #f)
     (define loop-filter #f)
     (let go ([i 1])
       (when (< i (vector-length argv))
         (define a (vector-ref argv i))
         (cond
           [(and (equal? a "--samples") (< (add1 i) (vector-length argv)))
            (set! samples (string->number (vector-ref argv (add1 i))))
            (go (+ i 2))]
           [(and (equal? a "--out") (< (add1 i) (vector-length argv)))
            (set! out (vector-ref argv (add1 i)))
            (go (+ i 2))]
           [(and (equal? a "--loop") (< (add1 i) (vector-length argv)))
            (set! loop-filter (vector-ref argv (add1 i)))
            (go (+ i 2))]
           [else
            (eprintf "unknown or incomplete option: ~a\n" a)
            (go (add1 i))])))
     (unless (and (integer? samples) (>= samples 1))
       (set! samples local-p90-min-samples))
     (define loops
       (if loop-filter
           (filter (lambda (l) (equal? (hash-ref l 'loop) loop-filter))
                   (local-feedback-loops (current-directory)))
           #f))
     (when (and loop-filter (null? loops))
       (eprintf "unknown loop ~a (expected L0, L1, or L2)\n" loop-filter)
       (exit 2))
     (define rec (collect-local-p90 #:loops loops #:samples-per-loop samples))
     (for ([l (in-list (hash-ref rec 'loops))])
       (printf "~a: p90=~ams slo=~ams verdict=~a samples=~a command=~a\n"
               (hash-ref l 'loop)
               (hash-ref l 'p90-ms)
               (hash-ref l 'slo-ms)
               (hash-ref l 'verdict)
               (hash-ref l 'sample-count)
               (hash-ref l 'command)))
     (if out
         (begin
           (call-with-output-file out
                                  (lambda (port)
                                    (write-json rec)
                                    (newline port))
                                  #:exists 'replace)
           (printf "wrote ~a\n" out))
         (write-json rec))]
    [else
     (usage)
     (exit 2)]))

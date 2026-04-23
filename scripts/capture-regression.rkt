#lang racket

;; scripts/capture-regression.rkt — Capture session logs as regression baselines
;;
;; Takes a session trace (JSONL) and creates a regression baseline file.
;; The baseline captures key metrics (tool calls, iteration count, outcome)
;; for later comparison against new runs.
;;
;; Usage:
;;   racket scripts/capture-regression.rkt <trace.jsonl> -o baseline.json
;;   racket scripts/capture-regression.rkt --compare <trace.jsonl> <baseline.json>
;;   racket scripts/capture-regression.rkt --check <trace.jsonl> <baseline.json>

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         json
         (only-in "../util/jsonl.rkt" jsonl-read-all-valid))

(provide capture-baseline
         compare-to-baseline
         regression-passes?
         load-baseline
         baseline-metrics)

;; ── Baseline struct ──

(struct baseline-metrics (tool-call-count iteration-count unique-tools outcome-length) #:transparent)

;; ── Extract metrics from trace ──

(define (extract-trace-metrics entries)
  "Extract key metrics from a list of JSONL entries."
  (define tool-calls
    (filter-map (lambda (e)
                  (and (hash? e)
                       (let ([role (hash-ref e 'role #f)])
                         (and (equal? role "assistant")
                              (let ([tcs (hash-ref e 'tool_calls '())]) (and (pair? tcs) tcs))))))
                entries))
  (define all-tools
    (apply append
           (for/list ([tc-group (in-list tool-calls)])
             (for/list ([tc (in-list tc-group)])
               (hash-ref (hash-ref tc 'function (hasheq)) 'name "unknown")))))
  (define iterations
    (length (filter (lambda (e) (and (hash? e) (equal? (hash-ref e 'role #f) "assistant"))) entries)))
  (define last-entry (last entries))
  (define outcome-text
    (cond
      [(hash-ref last-entry 'content #f)
       =>
       (lambda (c)
         (if (string? c)
             c
             (format "~a" c)))]
      [else ""]))
  (baseline-metrics (length all-tools)
                    iterations
                    (remove-duplicates all-tools)
                    (string-length outcome-text)))

;; ── Capture ──

(define (capture-baseline trace-path [output-path #f])
  "Read a trace file and write baseline metrics."
  (define entries (jsonl-read-all-valid trace-path))
  (define metrics (extract-trace-metrics entries))
  (define baseline
    (hasheq 'tool_call_count
            (baseline-metrics-tool-call-count metrics)
            'iteration_count
            (baseline-metrics-iteration-count metrics)
            'unique_tools
            (baseline-metrics-unique-tools metrics)
            'outcome_length
            (baseline-metrics-outcome-length metrics)
            'source
            (path->string trace-path)
            'captured_at
            (current-seconds)))
  (define out-path (or output-path (path-replace-extension trace-path ".baseline.json")))
  (call-with-output-file out-path (lambda (out) (write-json baseline out)) #:exists 'replace)
  out-path)

;; ── Load & Compare ──

(define (load-baseline path)
  "Load a baseline JSON file."
  (call-with-input-file path read-json))

(define (compare-to-baseline trace-path baseline-path)
  "Compare a trace against a baseline. Returns comparison hash."
  (define entries (jsonl-read-all-valid trace-path))
  (define metrics (extract-trace-metrics entries))
  (define baseline (load-baseline baseline-path))
  (hasheq 'tool_call_count
          (hasheq 'actual
                  (baseline-metrics-tool-call-count metrics)
                  'baseline
                  (hash-ref baseline 'tool_call_count 0))
          'iteration_count
          (hasheq 'actual
                  (baseline-metrics-iteration-count metrics)
                  'baseline
                  (hash-ref baseline 'iteration_count 0))
          'unique_tools
          (hasheq 'actual
                  (baseline-metrics-unique-tools metrics)
                  'baseline
                  (hash-ref baseline 'unique_tools '()))
          'outcome_length
          (hasheq 'actual
                  (baseline-metrics-outcome-length metrics)
                  'baseline
                  (hash-ref baseline 'outcome_length 0))))

(define (regression-passes? trace-path baseline-path [tolerance 0.25])
  "Check if a trace passes regression comparison within tolerance.
   Tolerance is the allowed fractional deviation (default 25%)."
  (define cmp (compare-to-baseline trace-path baseline-path))
  (define (within-tolerance? actual expected)
    (if (zero? expected)
        (zero? actual)
        (< (abs (- (/ actual expected 1.0) 1.0)) tolerance)))
  (define tool-count-ok
    (within-tolerance? (hash-ref (hash-ref cmp 'tool_call_count) 'actual)
                       (hash-ref (hash-ref cmp 'tool_call_count) 'baseline)))
  (define iter-count-ok
    (within-tolerance? (hash-ref (hash-ref cmp 'iteration_count) 'actual)
                       (hash-ref (hash-ref cmp 'iteration_count) 'baseline)))
  (and tool-count-ok iter-count-ok))

;; ── CLI ──

(module+ main
  (define output-file (make-parameter #f))
  (define compare-mode (make-parameter #f))
  (define check-mode (make-parameter #f))

  (define files
    (command-line #:program "capture-regression"
                  #:once-each [("-o" "--output") path "Output baseline file" (output-file path)]
                  [("--compare") "Compare trace to baseline" (compare-mode #t)]
                  [("--check") "Check regression (exit code)" (check-mode #t)]
                  #:args args
                  args))

  (cond
    [(and (check-mode) (= (length files) 2))
     (define passes? (regression-passes? (car files) (cadr files)))
     (printf "Regression check: ~a~n" (if passes? "PASS" "FAIL"))
     (exit (if passes? 0 1))]
    [(and (compare-mode) (= (length files) 2))
     (define cmp (compare-to-baseline (car files) (cadr files)))
     (displayln (jsexpr->string cmp))]
    [(= (length files) 1)
     (define out (capture-baseline (car files) (output-file)))
     (printf "Baseline written to: ~a~n" out)]
    [else
     (printf "Usage: capture-regression <trace.jsonl> [-o baseline.json]~n")
     (printf "       capture-regression --compare <trace.jsonl> <baseline.json>~n")
     (printf "       capture-regression --check <trace.jsonl> <baseline.json>~n")]))

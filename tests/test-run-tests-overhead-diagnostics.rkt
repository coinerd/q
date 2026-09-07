#lang racket

;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary unit  ;; @mutates fs

;; tests/test-run-tests-overhead-diagnostics.rkt
;; W0: verifies the run-tests overhead diagnostics entry points.

(require rackunit
         rackunit/text-ui
         racket/runtime-path
         racket/system
         racket/string
         "../scripts/run-tests/overhead.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

;; v1.00.27 W4 (#9592): builds a schema-valid local-p90 telemetry record with
;; synthetic samples (110..300 ms). Defaults compute p90 and verdicts the same
;; way the collector must; keyword overrides simulate hand-editing so the
;; --check rejection paths are testable.
(define (synthetic-local-p90-record #:samples [n 20]
                                    #:l0-p90-ms [l0-p90 #f]
                                    #:l0-verdict [l0-verdict #f])
  (define (mk-samples)
    (for/list ([i (in-range n)])
      (hasheq 'i
              (add1 i)
              'start
              (if (zero? i) "cold" "warm")
              'elapsed-ms
              (+ 110 (* 10 i))
              'exit-code
              0)))
  (define l0-samples (mk-samples))
  (define l0-recomputed-p90 (local-loop-p90 (map (lambda (s) (hash-ref s 'elapsed-ms)) l0-samples)))
  (define (mk-loop label slo samples p90 verdict)
    (hasheq 'loop
            label
            'description
            (string-append label " representative loop")
            'command
            "racket scripts/run-tests.rkt (synthetic)"
            'paths
            (list "tests/ci/metadata-discovery-test.rkt" "tests/test-version.rkt")
            'area-modes
            (list (hasheq 'area "ci" 'mode "grouped") (hasheq 'area "*" 'mode "subprocess"))
            'sample-count
            (length samples)
            'samples
            samples
            'p90-ms
            p90
            'slo-ms
            slo
            'verdict
            verdict))
  (hasheq 'version
          1
          'wave
          "v1.00.27-w4"
          'ticket
          "#9592"
          'generated-at
          "2026-02-19T00:00:00Z"
          'implementation-sha
          (make-string 40 #\a)
          'method
          (hasheq 'p90 "linear interpolation" 'rank "0.9*(n-1)")
          'machine
          (hasheq 'cpu-count 12 'load-1 0.5 'load-5 0.4 'load-15 0.3 'platform "unix" 'racket "8.x")
          'grouped-config
          (hasheq 'source
                  "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json"
                  'default-mode
                  "subprocess"
                  'areas
                  (list (hasheq 'area "ci" 'mode "grouped")))
          'loops
          (list (mk-loop "L0"
                         5000
                         l0-samples
                         (or l0-p90 l0-recomputed-p90)
                         (or l0-verdict (slo-verdict l0-recomputed-p90 5000)))
                (mk-loop "L1" 30000 l0-samples 281 (slo-verdict 281 30000))
                (mk-loop "L2" 120000 l0-samples 281 (slo-verdict 281 120000)))))

(define (run/capture cmd)
  (parameterize ([current-directory project-root])
    (define out (open-output-string))
    (define err (open-output-string))
    (define code
      (parameterize ([current-output-port out]
                     [current-error-port err])
        (system/exit-code cmd)))
    (values code (get-output-string out) (get-output-string err))))

(define suite
  (test-suite "run-tests overhead diagnostics"

    (test-case "make-overhead-result records label command exit and elapsed"
      (define r (make-overhead-result "noop" "racket -e '(void)'" 0 12 "" ""))
      (check-equal? (overhead-result-label r) "noop")
      (check-equal? (overhead-result-command r) "racket -e '(void)'")
      (check-equal? (overhead-result-exit-code r) 0)
      (check-equal? (overhead-result-elapsed-ms r) 12))

    (test-case "format-overhead-result includes useful fields"
      (define line (format-overhead-result (make-overhead-result "simple" "cmd" 0 25 "" "")))
      (check-true (string-contains? line "simple"))
      (check-true (string-contains? line "25ms"))
      (check-true (string-contains? line "exit=0")))

    (test-case "diagnose command exits successfully and prints report header"
      (define-values (code out err) (run/capture "racket scripts/run-tests.rkt --diagnose-overhead"))
      (check-equal? code 0 err)
      (check-true (string-contains? out "TEST RUNNER OVERHEAD DIAGNOSTIC"))
      (check-true (string-contains? out "racket-noop"))
      (check-true (string-contains? out "raco-empty")))

    ;; ── v1.00.27 W4 (#9592): L0/L1/L2 local feedback telemetry ──

    (test-case "p90 uses linear interpolation over sorted samples"
      ;; n=10: rank = 0.9*(10-1) = 8.1 → 900 + 0.1*(1000-900) = 910
      (check-equal? (local-loop-p90 '(100 200 300 400 500 600 700 800 900 1000)) 910)
      (check-equal? (local-loop-p90 '(42)) 42)
      ;; unsorted input is sorted first: (100 500 1000) → rank 1.8 → 500 + 0.8*500 = 900
      (check-equal? (local-loop-p90 '(1000 100 500)) 900))

    (test-case "slo verdicts are computed from p90 against the budget"
      (check-equal? (slo-verdict 4000 5000) "meet")
      (check-equal? (slo-verdict 5000 5000) "meet")
      (check-equal? (slo-verdict 5001 5000) "miss"))

    (test-case "machine context carries cpu count load and platform"
      (define mc (machine-context))
      (check-pred (lambda (n) (and (integer? n) (>= n 1))) (hash-ref mc 'cpu-count))
      (check-pred (lambda (x) (and (real? x) (>= x 0))) (hash-ref mc 'load-1))
      (check-pred (lambda (x) (and (real? x) (>= x 0))) (hash-ref mc 'load-5))
      (check-pred (lambda (x) (and (real? x) (>= x 0))) (hash-ref mc 'load-15))
      (check-pred string? (hash-ref mc 'platform))
      (check-pred string? (hash-ref mc 'racket)))

    (test-case "local feedback loops declare L0 L1 L2 with slo budgets"
      (define loops (local-feedback-loops project-root))
      (check-equal? (map (lambda (l) (hash-ref l 'loop)) loops) '("L0" "L1" "L2"))
      (check-equal? (map (lambda (l) (hash-ref l 'slo-ms)) loops) '(5000 30000 120000))
      (for ([l (in-list loops)])
        (check-pred (lambda (a) (and (list? a) (pair? a))) (hash-ref l 'argv))
        (check-pred (lambda (p) (and (list? p) (pair? p))) (hash-ref l 'paths))
        (check-pred string? (hash-ref l 'description))))

    (test-case "area mode rows carry grouped/subprocess per W3 configuration"
      (define rows
        (area-mode-rows (list "tests/ci/metadata-discovery-test.rkt" "tests/test-version.rkt")))
      (check-equal? (length rows) 2)
      (define by-path
        (for/hash ([r (in-list rows)])
          (values (hash-ref r 'path) (hash-ref r 'mode))))
      (check-equal? (hash-ref by-path "tests/ci/metadata-discovery-test.rkt") "grouped")
      (check-equal? (hash-ref by-path "tests/test-version.rkt") "subprocess")
      (for ([r (in-list rows)])
        (check-pred string? (hash-ref r 'area))))

    (test-case "check accepts a well-formed synthetic telemetry record"
      (check-equal? (check-local-p90-record (synthetic-local-p90-record)) '()))

    (test-case "check rejects a record missing machine context"
      (define violations (check-local-p90-record (hash-remove (synthetic-local-p90-record) 'machine)))
      (check-true (pair? violations))
      (check-true (string-contains? (string-join violations " ") "machine")))

    (test-case "check rejects a record missing per-loop p90 and samples"
      (define loop0 (car (hash-ref (synthetic-local-p90-record) 'loops)))
      (define missing-p90
        (check-local-p90-record
         (hash-set (synthetic-local-p90-record) 'loops (list (hash-remove loop0 'p90-ms)))))
      (check-true (pair? missing-p90))
      (check-true (string-contains? (string-join missing-p90 " ") "p90"))
      (define missing-samples
        (check-local-p90-record
         (hash-set (synthetic-local-p90-record) 'loops (list (hash-remove loop0 'samples)))))
      (check-true (pair? missing-samples)))

    (test-case "check rejects hand-written slo verdicts and wrong p90"
      ;; verdict recorded ≠ verdict computed from recomputed p90
      (define bad-verdict (check-local-p90-record (synthetic-local-p90-record #:l0-verdict "miss")))
      (check-true (pair? bad-verdict))
      (check-true (string-contains? (string-join bad-verdict " ") "verdict"))
      ;; p90 recorded ≠ p90 recomputed from samples
      (define bad-p90 (check-local-p90-record (synthetic-local-p90-record #:l0-p90-ms 9000)))
      (check-true (pair? bad-p90))
      (check-true (string-contains? (string-join bad-p90 " ") "p90")))

    (test-case "check rejects sample counts below the minimum of 20"
      (define violations (check-local-p90-record (synthetic-local-p90-record #:samples 3)))
      (check-true (pair? violations))
      (check-true (string-contains? (string-join violations " ") "sample")))

    (test-case "check rejects area modes that contradict W3 configuration"
      (define rec (synthetic-local-p90-record))
      (define loops
        (for/list ([l (in-list (hash-ref rec 'loops))])
          (hash-set l
                    'area-modes
                    (list (hasheq 'area "ci" 'mode "subprocess")
                          (hasheq 'area "*" 'mode "grouped")))))
      (define violations (check-local-p90-record (hash-set rec 'loops loops)))
      (check-true (pair? violations))
      (check-true (string-contains? (string-join violations " ") "area")))

    (test-case "collect-local-p90 samples a real loop cold then warm"
      (define rec
        (collect-local-p90
         #:base-dir project-root
         #:loops (list (hasheq 'loop
                               "L0"
                               'description
                               "synthetic single-file loop"
                               'slo-ms
                               5000
                               'argv
                               (list "racket" "scripts/run-tests.rkt" "tests/test-version.rkt")
                               'paths
                               (list "tests/test-version.rkt")))
         #:samples-per-loop 3))
      (define loop0 (car (hash-ref rec 'loops)))
      (check-equal? (hash-ref loop0 'sample-count) 3)
      (check-equal? (map (lambda (s) (hash-ref s 'start)) (hash-ref loop0 'samples))
                    '("cold" "warm" "warm"))
      (check-true (andmap (lambda (s) (equal? (hash-ref s 'exit-code) 0)) (hash-ref loop0 'samples)))
      (check-equal? (hash-ref loop0 'p90-ms)
                    (local-loop-p90 (map (lambda (s) (hash-ref s 'elapsed-ms))
                                         (hash-ref loop0 'samples))))
      (check-equal? (hash-ref loop0 'verdict) (slo-verdict (hash-ref loop0 'p90-ms) 5000))
      (check-true (pair? (hash-ref loop0 'area-modes)))
      (check-pred integer? (hash-ref (hash-ref rec 'machine) 'cpu-count))
      (check-pred string? (hash-ref rec 'implementation-sha)))))

(run-tests suite)

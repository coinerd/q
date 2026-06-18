#lang racket

;; @speed fast
;; @suite testing
;; @isolation subprocess

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
      (check-true (string-contains? out "raco-empty")))))

(run-tests suite)

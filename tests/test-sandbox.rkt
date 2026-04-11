#lang racket

;; test-sandbox.rkt — Tests for sandbox/ modules: limits, subprocess, evaluator

(require rackunit
         rackunit/text-ui
         racket/port
         "../../q/sandbox/limits.rkt"
         "../../q/sandbox/subprocess.rkt"
         "../../q/sandbox/evaluator.rkt")

;; ============================================================
;; 1. limits.rkt tests
;; ============================================================

(define-test-suite limits-tests

  ;; --- struct & constructors ---

  (test-case "default-exec-limits has expected values"
    (define lim (default-exec-limits))
    (check-equal? (exec-limits-timeout-seconds lim) 120)
    (check-equal? (exec-limits-max-output-bytes lim) 1048576) ; 1 MB
    (check-equal? (exec-limits-max-memory-bytes lim) 536870912) ; 512 MB
    (check-equal? (exec-limits-max-processes lim) 10))

  (test-case "strict-exec-limits is more restrictive than default"
    (define s (strict-exec-limits))
    (define d (default-exec-limits))
    (check < (exec-limits-timeout-seconds s) (exec-limits-timeout-seconds d))
    (check < (exec-limits-max-output-bytes s) (exec-limits-max-output-bytes d))
    (check < (exec-limits-max-memory-bytes s) (exec-limits-max-memory-bytes d))
    (check < (exec-limits-max-processes s) (exec-limits-max-processes d)))

  (test-case "permissive-exec-limits is more lenient than default"
    (define p (permissive-exec-limits))
    (define d (default-exec-limits))
    (check > (exec-limits-timeout-seconds p) (exec-limits-timeout-seconds d))
    (check > (exec-limits-max-output-bytes p) (exec-limits-max-output-bytes d))
    (check > (exec-limits-max-memory-bytes p) (exec-limits-max-memory-bytes d))
    (check > (exec-limits-max-processes p) (exec-limits-max-processes d)))

  ;; --- merge-limits ---

  (test-case "merge-limits: stricter timeout wins"
    (define base (exec-limits 120 1048576 536870912 10))
    (define override (exec-limits 30 1048576 536870912 10))
    (define merged (merge-limits base override))
    (check-equal? (exec-limits-timeout-seconds merged) 30))

  (test-case "merge-limits: stricter output limit wins"
    (define base (exec-limits 120 1048576 536870912 10))
    (define override (exec-limits 120 65536 536870912 10))
    (define merged (merge-limits base override))
    (check-equal? (exec-limits-max-output-bytes merged) 65536))

  (test-case "merge-limits: stricter memory wins"
    (define base (exec-limits 120 1048576 536870912 10))
    (define override (exec-limits 120 1048576 134217728 10))
    (define merged (merge-limits base override))
    (check-equal? (exec-limits-max-memory-bytes merged) 134217728))

  (test-case "merge-limits: stricter process count wins"
    (define base (exec-limits 120 1048576 536870912 10))
    (define override (exec-limits 120 1048576 536870912 3))
    (define merged (merge-limits base override))
    (check-equal? (exec-limits-max-processes merged) 3))

  (test-case "merge-limits: override more lenient in some fields"
    (define base (exec-limits 30 65536 134217728 3))
    (define override (exec-limits 120 1048576 536870912 10))
    (define merged (merge-limits base override))
    ;; base is stricter in all fields, so base wins
    (check-equal? (exec-limits-timeout-seconds merged) 30)
    (check-equal? (exec-limits-max-output-bytes merged) 65536)
    (check-equal? (exec-limits-max-memory-bytes merged) 134217728)
    (check-equal? (exec-limits-max-processes merged) 3))

  ;; --- within-limits? ---

  (test-case "within-limits?: everything within bounds"
    (define lim (exec-limits 10 1000 1000 5))
    (check-true (within-limits? lim #:elapsed 5 #:output-size 500 #:memory 500)))

  (test-case "within-limits?: elapsed exceeds timeout"
    (define lim (exec-limits 10 1000 1000 5))
    (check-false (within-limits? lim #:elapsed 11 #:output-size 500 #:memory 500)))

  (test-case "within-limits?: output exceeds max"
    (define lim (exec-limits 10 1000 1000 5))
    (check-false (within-limits? lim #:elapsed 5 #:output-size 1001 #:memory 500)))

  (test-case "within-limits?: memory exceeds max"
    (define lim (exec-limits 10 1000 1000 5))
    (check-false (within-limits? lim #:elapsed 5 #:output-size 500 #:memory 1001)))

  (test-case "within-limits?: omitted fields are not checked"
    (define lim (exec-limits 10 1000 1000 5))
    (check-true (within-limits? lim)))

  (test-case "within-limits?: exact boundary is within limits"
    (define lim (exec-limits 10 1000 1000 5))
    (check-true (within-limits? lim #:elapsed 10 #:output-size 1000 #:memory 1000)))
)

;; ============================================================
;; 2. subprocess.rkt tests
;; ============================================================

(define-test-suite subprocess-tests

  (test-case "run-subprocess: simple echo"
    (define result (run-subprocess "echo" #:args '("hello world")))
    (check-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result))
    (check regexp-match? #rx"hello world" (subprocess-result-stdout result)))

  (test-case "run-subprocess: captures stderr"
    (define result (run-subprocess "/bin/sh" #:args '("-c" "echo err >&2"))
    )
    (check-equal? (subprocess-result-exit-code result) 0)
    (check regexp-match? #rx"err" (subprocess-result-stderr result)))

  (test-case "run-subprocess: non-zero exit code"
    (define result (run-subprocess "false"))
    (check-not-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result)))

  (test-case "run-subprocess: elapsed-ms is positive"
    (define result (run-subprocess "true"))
    (check-true (>= (subprocess-result-elapsed-ms result) 0)))

  (test-case "run-subprocess: timeout kills process"
    (define strict (exec-limits 1 1048576 536870912 10))
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 30")
                                    #:limits strict))
    (check-pred subprocess-result-timed-out? result)
    ;; Elapsed should be roughly the timeout (1s = 1000ms), allow some slack
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "run-subprocess: output is captured as string by default"
    (define result (run-subprocess "echo" #:args '("test123")))
    (check-true (string? (subprocess-result-stdout result))))

  (test-case "run-subprocess: working directory"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "pwd")
                                    #:directory "/tmp"))
    (check regexp-match? #rx"/tmp" (subprocess-result-stdout result)))

  (test-case "run-subprocess: stdout is bounded by max-output-bytes"
    ;; Generate 100KB of output but limit to 1KB
    (define strict (exec-limits 10 1024 536870912 10))
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "dd if=/dev/zero bs=1024 count=100 2>/dev/null | tr '\\0' 'x'")
                                    #:limits strict))
    (check-true (<= (string-length (subprocess-result-stdout result))
                    1024)))

  (test-case "kill-subprocess! does not crash on valid custodian"
    (define cust (make-custodian))
    ;; Just test that it doesn't raise an error on a plain custodian
    (check-not-exn (lambda () (kill-subprocess! cust))))
)

;; ============================================================
;; 3. evaluator.rkt tests
;; ============================================================

(define-test-suite evaluator-tests

  (test-case "eval-in-sandbox: simple arithmetic"
    (define result (eval-in-sandbox "(+ 1 2)"))
    (check-false (eval-result-error result))
    (check-equal? (eval-result-value result) 3))

  (test-case "eval-in-sandbox: string result"
    (define result (eval-in-sandbox "\"hello\""))
    (check-false (eval-result-error result))
    (check-equal? (eval-result-value result) "hello"))

  (test-case "eval-in-sandbox: captures stdout"
    (define result (eval-in-sandbox "(display \"out\")"))
    (check-false (eval-result-error result))
    (check regexp-match? #rx"out" (eval-result-output result)))

  (test-case "eval-in-sandbox: error on bad syntax"
    (define result (eval-in-sandbox "(+ undefined-var 1)"))
    (check-not-false (eval-result-error result)))

  (test-case "eval-in-sandbox: elapsed-ms is non-negative"
    (define result (eval-in-sandbox "42"))
    (check-true (>= (eval-result-elapsed-ms result) 0)))

  (test-case "eval-in-sandbox: timeout returns error"
    ;; Use a very short timeout and a long-running computation
    (define result (eval-in-sandbox "(let loop () (loop))"
                                     #:timeout 1))
    (check-not-false (eval-result-error result)))
)

;; ============================================================
;; Run all
;; ============================================================

(run-tests limits-tests)
(run-tests subprocess-tests)
(run-tests evaluator-tests)

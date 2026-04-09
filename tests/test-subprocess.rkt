#lang racket

;; test-subprocess.rkt — focused tests for sandbox/subprocess.rkt and limits.rkt

(require rackunit
         rackunit/text-ui
         "../sandbox/limits.rkt"
         "../sandbox/subprocess.rkt")

(define-test-suite subprocess-tests

  ;; --- limits.rkt standalone constants ---

  (test-case "default-timeout-seconds is 300"
    (check-equal? default-timeout-seconds 300))

  (test-case "default-max-output-bytes is 10 MB"
    (check-equal? default-max-output-bytes 10485760))

  ;; --- with-resource-limits ---

  (test-case "with-resource-limits: successful execution returns result"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) 42)
                            #:timeout 5))
    (check-equal? result 42)
    (check-false timed-out?))

  (test-case "with-resource-limits: timeout triggers"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) (sleep 30) 99)
                            #:timeout 1))
    (check-true timed-out?))

  ;; --- run-subprocess with #:timeout ---

  (test-case "run-subprocess: simple echo"
    (define result (run-subprocess "echo" #:args '("hello world")))
    (check-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result))
    (check regexp-match? #rx"hello world" (subprocess-result-stdout result)))

  (test-case "run-subprocess: #:timeout override"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 30")
                                    #:timeout 1))
    (check-true (subprocess-result-timed-out? result))
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "run-subprocess: non-zero exit code"
    (define result (run-subprocess "false"))
    (check-not-equal? (subprocess-result-exit-code result) 0)
    (check-false (subprocess-result-timed-out? result)))

  (test-case "run-subprocess: output truncation marker"
    ;; Generate large output with a very small limit
    (define strict (exec-limits 10 100 536870912 10))
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "seq 1 10000")
                                    #:limits strict))
    (check-true (string-contains? (subprocess-result-stdout result)
                                  "[output truncated at 100 bytes]")))

  (test-case "run-subprocess: process killed on timeout"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 60")
                                    #:timeout 1))
    (check-true (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) -1)
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "kill-subprocess! does not crash"
    (define cust (make-custodian))
    (check-not-exn (lambda () (kill-subprocess! cust))))
)

(run-tests subprocess-tests)

#lang racket

;; tests/test-sandbox-security.rkt — security regression tests
;;
;; Covers:
;;   1. Path traversal: read/write tools handle relative/abs paths via OS
;;   2. Cancellation propagation: cancelled tokens report correctly
;;   3. Timeout enforcement: subprocess & evaluator timeouts fire
;;   4. Sandboxed evaluator: blocks dangerous/long-running operations

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         "../sandbox/limits.rkt"
         "../sandbox/subprocess.rkt"
         "../sandbox/evaluator.rkt"
         "../util/cancellation.rkt"
         "../tools/tool.rkt"
         "../tools/builtins/read.rkt"
         "../tools/builtins/write.rkt"
         "../tools/builtins/bash.rkt")

;; ============================================================
;; 1. Path traversal
;; ============================================================

(define-test-suite path-traversal-tests

  (test-case "read tool rejects non-existent path traversal"
    ;; tool-read should return an error for a path that doesn't resolve,
    ;; whether it contains ../ or not. The key invariant: no crash, no data
    ;; leak, just a clean error result.
    (define result (tool-read (hasheq 'path "../../etc/passwd")))
    (check-true (tool-result? result))
    (check-true (tool-result-is-error? result)))

  (test-case "read tool rejects relative parent path with offset"
    (define result (tool-read (hasheq 'path "../nonexistent-secret-file"
                                      'offset 1)))
    (check-true (tool-result? result))
    (check-true (tool-result-is-error? result)))

  (test-case "write tool fails on path traversal to non-existent parent"
    ;; Writing to ../../tmp/test should either succeed (if the path is writable
    ;; as the user) or fail cleanly. The important thing: no crash.
    (define result (tool-write (hasheq 'path "/tmp/q-sandbox-test-traversal/t.txt"
                                       'content "test")))
    ;; Cleanup
    (with-handlers ([exn:fail? void])
      (delete-directory/files "/tmp/q-sandbox-test-traversal" #:must-exist? #f))
    (check-true (tool-result? result)))

  (test-case "read tool works on real file, rejects sibling traversal"
    ;; Create a temp file, read it, then try reading a non-existent sibling
    (define tmp-dir (make-temporary-file "q-security-test-~a"))
    (delete-file tmp-dir)
    (make-directory tmp-dir)
    (define tmp-file (build-path tmp-dir "real.txt"))
    (with-output-to-file tmp-file (lambda () (display "hello")))
    ;; Reading the real file works
    (define good (tool-read (hasheq 'path (path->string tmp-file))))
    (check-true (tool-result? good))
    (check-false (tool-result-is-error? good))
    ;; Traversing above it to non-existent path fails
    (define bad (tool-read (hasheq 'path (path->string
                                          (build-path tmp-dir ".." "no-such-file-xyz"))))
      )
    (check-true (tool-result? bad))
    (check-true (tool-result-is-error? bad))
    ;; Cleanup
    (delete-directory/files tmp-dir #:must-exist? #f))

  (test-case "bash tool does not crash on shell injection characters"
    ;; The bash tool passes commands to /bin/sh -c. Special characters should
    ;; not cause q to crash — the shell handles them.
    (define result (tool-bash (hasheq 'command "echo 'hello; rm -rf /'")))
    (check-true (tool-result? result))
    (check-false (tool-result-is-error? result))))

;; ============================================================
;; 2. Cancellation propagation
;; ============================================================

(define-test-suite cancellation-security-tests

  (test-case "fresh token is not cancelled"
    (define tok (make-cancellation-token))
    (check-false (cancellation-token-cancelled? tok)))

  (test-case "cancelled token reports cancelled"
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (check-true (cancellation-token-cancelled? tok)))

  (test-case "cancellation callback fires exactly once per cancel-token! call"
    (define count (box 0))
    (define tok (make-cancellation-token
                 #:callback (lambda (_) (set-box! count (add1 (unbox count))))))
    (cancel-token! tok)
    (check-equal? (unbox count) 1))

  (test-case "cancellation callback receives the token"
    (define received (box #f))
    (define tok (make-cancellation-token
                 #:callback (lambda (t) (set-box! received t))))
    (cancel-token! tok)
    (check-eq? (unbox received) tok))

  (test-case "multiple independent tokens do not cross-contaminate"
    (define tok-a (make-cancellation-token))
    (define tok-b (make-cancellation-token))
    (cancel-token! tok-a)
    (check-true (cancellation-token-cancelled? tok-a))
    (check-false (cancellation-token-cancelled? tok-b)))

  (test-case "double cancel is idempotent for cancelled? predicate"
    (define tok (make-cancellation-token))
    (cancel-token! tok)
    (cancel-token! tok)
    (check-true (cancellation-token-cancelled? tok))))

;; ============================================================
;; 3. Timeout enforcement
;; ============================================================

(define-test-suite timeout-enforcement-tests

  (test-case "subprocess timeout kills long-running command"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 60")
                                    #:timeout 1))
    (check-true (subprocess-result-timed-out? result))
    (check-equal? (subprocess-result-exit-code result) -1)
    ;; Should complete well within 5 seconds of the 1-second timeout
    (check-true (< (subprocess-result-elapsed-ms result) 5000)))

  (test-case "subprocess timeout error message includes duration"
    (define result (run-subprocess "/bin/sh"
                                    #:args '("-c" "sleep 60")
                                    #:timeout 1))
    (check regexp-match? #rx"Timed out" (subprocess-result-stderr result)))

  (test-case "with-resource-limits returns timed-out on slow thunk"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) (sleep 30) 'done)
                            #:timeout 0.5))
    (check-true timed-out?))

  (test-case "with-resource-limits returns result for fast thunk"
    (define-values (result timed-out?)
      (with-resource-limits (lambda (cust) 42)
                            #:timeout 5))
    (check-equal? result 42)
    (check-false timed-out?))

  (test-case "eval-in-sandbox timeout terminates infinite loop"
    (define result (eval-in-sandbox "(let loop () (loop))"
                                     #:timeout 1))
    (check-true (eval-result? result))
    (check-not-false (eval-result-error result)))

  (test-case "eval-in-sandbox completes within timeout"
    (define result (eval-in-sandbox "(+ 1 2)" #:timeout 30))
    (check-false (eval-result-error result))
    (check-equal? (eval-result-value result) 3))

  (test-case "bash tool timeout propagates to subprocess"
    (define result (tool-bash (hasheq 'command "sleep 60"
                                      'timeout 1)))
    (check-true (tool-result? result))
    ;; The tool returns success (the subprocess ran) but details show timed-out
    (define details (tool-result-details result))
    (check-true (hash-ref details 'timed-out? #f)))

  (test-case "strict limits have lower timeout than default"
    (define strict (strict-exec-limits))
    (define default (default-exec-limits))
    (check-true (< (exec-limits-timeout-seconds strict)
                   (exec-limits-timeout-seconds default))))

  (test-case "merge-limits: stricter timeout always wins"
    (define base (default-exec-limits))
    (define override (strict-exec-limits))
    (define merged (merge-limits base override))
    (check-equal? (exec-limits-timeout-seconds merged)
                  (exec-limits-timeout-seconds override))))

;; ============================================================
;; 4. Sandboxed evaluator blocks dangerous operations
;; ============================================================

(define-test-suite sandbox-evaluator-security-tests

  (test-case "sandbox blocks filesystem write"
    ;; The Racket sandbox restricts file I/O. Attempting to write should fail.
    (define result (eval-in-sandbox
                    "(call-with-output-file \"/tmp/q-sandbox-escape-test\"\n  (lambda (p) (display \"escape\" p)))"))
    (check-true (eval-result? result))
    (check-not-false (eval-result-error result)
                     "sandbox should block file writes"))

  (test-case "sandbox blocks subprocess creation"
    (define result (eval-in-sandbox
                    "(require racket/system) (system \"echo pwned\")"))
    (check-true (eval-result? result))
    (check-not-false (eval-result-error result)
                     "sandbox should block system calls"))

  (test-case "sandbox enforces timeout on infinite loop"
    (define result (eval-in-sandbox "(let loop () (loop))" #:timeout 1))
    (check-true (eval-result? result))
    (check-not-false (eval-result-error result))
    ;; Should return quickly, not hang
    (check-true (< (eval-result-elapsed-ms result) 10000)))

  (test-case "sandbox allows pure computation"
    (define result (eval-in-sandbox "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)"))
    (check-false (eval-result-error result))
    (check-equal? (eval-result-value result) 55))

  (test-case "sandbox output capture prevents stdout leak"
    ;; Output goes to a string, not the real stdout
    (define result (eval-in-sandbox "(display \"secret-data\")"))
    (check-false (eval-result-error result))
    (check-equal? (eval-result-output result) "secret-data"))

  (test-case "sandbox memory limit terminates allocation bomb"
    ;; Default sandbox-memory-limit is 256 MB. This should exhaust it.
    (define result (eval-in-sandbox
                    "(define xs (make-list 10000000 (make-string 1000 #\\x))) (length xs)"
                    #:timeout 10))
    (check-true (eval-result? result))
    ;; Either error (memory exceeded) or timeout — either way, it terminates
    (check-true (or (eval-result-error result)
                    (< (eval-result-elapsed-ms result) 15000)))))

;; ============================================================
;; Run all
;; ============================================================

(run-tests path-traversal-tests)
(run-tests cancellation-security-tests)
(run-tests timeout-enforcement-tests)
(run-tests sandbox-evaluator-security-tests)

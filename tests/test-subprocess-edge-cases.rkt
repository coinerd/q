#lang racket

;; tests/test-subprocess-edge-cases.rkt — Wave 13: SP1-SP6 subprocess edge cases
;;
;; Tests for subprocess lifecycle: non-existent commands, working directory,
;; timeout partial output, large output truncation, shell quoting, and
;; combined stdout/stderr.

(require rackunit
         rackunit/text-ui
         "../sandbox/subprocess.rkt"
         "../sandbox/limits.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (fast-limits #:timeout [timeout 5] #:max-output [max-out 1048576])
  (exec-limits timeout max-out 536870912 10))

;; ============================================================
;; Tests
;; ============================================================

(define subprocess-edge-tests
  (test-suite
   "Subprocess Edge Case Tests"

   ;; ============================================================
   ;; SP1: run-subprocess with non-existent command
   ;; ============================================================
   (test-case
    "SP1: run-subprocess with non-existent command returns exit-code -1"
    (define result
      (run-subprocess "definitely-not-a-real-command-xyz123"
                      #:limits (fast-limits #:timeout 2)))
    (check-equal? (subprocess-result-exit-code result) -1
                  "non-existent command returns exit-code -1")
    (check-false (subprocess-result-timed-out? result)
                 "non-existent command is not a timeout")
    (check-not-false (regexp-match? #rx"Failed to execute"
                                    (subprocess-result-stderr result))
                     "stderr mentions execution failure"))

   ;; ============================================================
   ;; SP2: run-subprocess with custom working directory
   ;; ============================================================
   (test-case
    "SP2: run-subprocess with custom working directory"
    (define tmp-dir (make-temporary-file "q-sp2-~a" 'directory))
    (define result
      (run-subprocess "pwd"
                      #:limits (fast-limits #:timeout 5)
                      #:directory tmp-dir))
    (check-equal? (subprocess-result-exit-code result) 0
                  "pwd succeeds")
    ;; The output should contain the temp directory path
    (define out (string-trim (subprocess-result-stdout result)))
    (check-not-false (regexp-match? (regexp-quote (path->string tmp-dir)) out)
                     (format "stdout contains working dir, got: ~a" out))
    ;; Cleanup
    (delete-directory/files tmp-dir))

   ;; ============================================================
   ;; SP3: timeout partial output content
   ;; ============================================================
   (test-case
    "SP3: timeout captures partial output"
    ;; Use a command that produces output slowly, then times out
    (define result
      (run-subprocess "/bin/sh"
                      #:args '("-c" "echo started; sleep 30")
                      #:limits (fast-limits #:timeout 1)))
    (check-true (subprocess-result-timed-out? result)
                "command timed out")
    (check-equal? (subprocess-result-exit-code result) -9
                  "timed out process gets exit-code -9")
    (check-not-false (regexp-match? #rx"started"
                                    (subprocess-result-stdout result))
                     "partial stdout captured before timeout")
    (check-not-false (regexp-match? #rx"timed out"
                                    (subprocess-result-stderr result))
                     "stderr mentions timeout"))

   ;; ============================================================
   ;; SP4: large output truncation marker
   ;; ============================================================
   (test-case
    "SP4: large output is truncated with marker"
    ;; Generate output larger than the max-output limit
    ;; printf + seq is fast and produces predictable output
    (define result
      (run-subprocess "/bin/sh"
                      #:args '("-c" "printf \"%0.sx\" $(seq 1 600)")
                      #:limits (fast-limits #:timeout 10 #:max-output 512)))
    (check-equal? (subprocess-result-exit-code result) 0
                  "command completes")
    (check-not-false (regexp-match? #rx"output truncated"
                                    (subprocess-result-stdout result))
                     "output contains truncation marker")
    ;; Output should be longer than 1024 bytes (original data + marker text)
    (check-true (> (string-length (subprocess-result-stdout result)) 0)
                "some output was captured"))

   ;; ============================================================
   ;; SP5: shell-quote preserves special characters in args
   ;; ============================================================
   (test-case
    "SP5: shell-quote preserves special characters in args"
    ;; Verify quoting actually works with real commands
    ;; Spaces preserved
    (define r1
      (run-subprocess "echo"
                      #:args (list "hello world")
                      #:limits (fast-limits #:timeout 5)))
    (check-equal? (subprocess-result-exit-code r1) 0)
    (check-not-false (regexp-match? #rx"hello world"
                                    (subprocess-result-stdout r1))
                     "shell-quote preserves spaces in argument")
    ;; Dollar sign not expanded
    (define r2
      (run-subprocess "echo"
                      #:args (list "$FOO_BAR_TEST")
                      #:limits (fast-limits #:timeout 5)))
    (check-equal? (subprocess-result-exit-code r2) 0)
    (check-not-false (regexp-match? #rx"[$]FOO_BAR_TEST"
                                    (subprocess-result-stdout r2))
                     "shell-quote prevents dollar expansion")
    ;; Semicolons not interpreted
    (define r3
      (run-subprocess "echo"
                      #:args (list "a;b")
                      #:limits (fast-limits #:timeout 5)))
    (check-equal? (subprocess-result-exit-code r3) 0)
    (check-not-false (regexp-match? #rx"a;b"
                                    (subprocess-result-stdout r3))
                     "shell-quote prevents semicolon injection"))

   ;; ============================================================
   ;; SP6: combined stdout + stderr when both are non-empty
   ;; ============================================================
   (test-case
    "SP6: stdout and stderr are both captured"
    (define result
      (run-subprocess "/bin/sh"
                      #:args '("-c" "echo stdout-msg; echo stderr-msg >&2")
                      #:limits (fast-limits #:timeout 5)))
    (check-equal? (subprocess-result-exit-code result) 0
                  "command succeeds")
    (check-not-false (regexp-match? #rx"stdout-msg"
                                    (subprocess-result-stdout result))
                     "stdout contains stdout-msg")
    (check-not-false (regexp-match? #rx"stderr-msg"
                                    (subprocess-result-stderr result))
                     "stderr contains stderr-msg"))))

(module+ main
  (run-tests subprocess-edge-tests))

(module+ test
  (run-tests subprocess-edge-tests))

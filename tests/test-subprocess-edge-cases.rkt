#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: integration

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

;; W1 v0.99.77: process-group kill is only possible where `setsid` exists
;; (Linux util-linux). On macOS (no setsid) the timeout path still sends
;; @boundary integration
;; SIGTERM then SIGKILL to the direct child, but cannot signal the whole
;; group — so survivor assertions are conditional on setsid availability.
(define setsid-available? (not (false? (find-executable-path "setsid"))))

;; v1.00.07 W2: macOS /bin/sh is bash (PIPESTATUS evaluates; no dash); SP12's
;; dash-specific assertions are conditional, mirroring setsid-available?.
(define sh-is-dash?
  (let ([probe (run-subprocess "/bin/sh"
                               #:args '("-c" "false | true; echo PS=${PIPESTATUS[0]}")
                               #:limits (fast-limits #:timeout 5))])
    (and (equal? (subprocess-result-exit-code probe) 2)
         (regexp-match? #rx"Bad substitution" (subprocess-result-stderr probe)))))

;; Find PIDs whose cmdline contains `marker` (any state).
(define (matching-pids marker)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [code (parameterize ([current-output-port out]
                              [current-error-port err])
                 (system (format "ps -eo pid,stat,cmd | grep '~a' | grep -v grep || true" marker)))])
    (for/list ([line (in-list (string-split (get-output-string out) "\n"))]
               #:when (regexp-match? #rx"qw1sp" line))
      (car (string-split line)))))

;; Clean up any leaked marker processes (SIGKILL — works on stopped processes).
(define (cleanup-markers! marker)
  (for ([pid (in-list (matching-pids marker))])
    (system (format "kill -KILL ~a 2>/dev/null || true" pid)))
  (sleep 0.2))

;; ============================================================
;; Tests
;; ============================================================

(define subprocess-edge-tests
  (test-suite "Subprocess Edge Case Tests"

    ;; ============================================================
    ;; SP1: run-subprocess with non-existent command
    ;; ============================================================
    (test-case "SP1: run-subprocess with non-existent command returns exit-code -1"
      (define result
        (run-subprocess "definitely-not-a-real-command-xyz123" #:limits (fast-limits #:timeout 2)))
      (check-equal? (subprocess-result-exit-code result)
                    -1
                    "non-existent command returns exit-code -1")
      (check-false (subprocess-result-timed-out? result) "non-existent command is not a timeout")
      (check-not-false (regexp-match? #rx"Failed to execute" (subprocess-result-stderr result))
                       "stderr mentions execution failure"))

    ;; ============================================================
    ;; SP2: run-subprocess with custom working directory
    ;; ============================================================
    (test-case "SP2: run-subprocess with custom working directory"
      (define tmp-dir (make-temporary-file "q-sp2-~a" 'directory))
      (define result (run-subprocess "pwd" #:limits (fast-limits #:timeout 5) #:directory tmp-dir))
      (check-equal? (subprocess-result-exit-code result) 0 "pwd succeeds")
      ;; The output should contain the temp directory path
      (define out (string-trim (subprocess-result-stdout result)))
      (check-not-false (regexp-match? (regexp-quote (path->string tmp-dir)) out)
                       (format "stdout contains working dir, got: ~a" out))
      ;; Cleanup
      (delete-directory/files tmp-dir))

    ;; ============================================================
    ;; SP3: timeout partial output content
    ;; ============================================================
    (test-case "SP3: timeout captures partial output"
      ;; Use a command that produces output slowly, then times out
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "echo started; sleep 30")
                        #:limits (fast-limits #:timeout 1)))
      (check-true (subprocess-result-timed-out? result) "command timed out")
      (check-equal? (subprocess-result-exit-code result) -9 "timed out process gets exit-code -9")
      (check-not-false (regexp-match? #rx"started" (subprocess-result-stdout result))
                       "partial stdout captured before timeout")
      (check-not-false (regexp-match? #rx"timed out" (subprocess-result-stderr result))
                       "stderr mentions timeout"))

    ;; ============================================================
    ;; SP4: large output truncation marker
    ;; ============================================================
    (test-case "SP4: large output is truncated"
      ;; Generate output larger than the max-output limit
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "printf \"%0.sx\" $(seq 1 600)")
                        #:limits (fast-limits #:timeout 10 #:max-output 512)))
      (check-equal? (subprocess-result-exit-code result) 0 "command completes")
      ;; Non-blocking read caps output at max-output-bytes
      (check-true (<= (string-length (subprocess-result-stdout result)) 512)
                  "output is capped at max-output limit")
      (check-true (> (string-length (subprocess-result-stdout result)) 0) "some output was captured"))

    ;; ============================================================
    ;; SP5: shell-quote preserves special characters in args
    ;; ============================================================
    (test-case "SP5: shell-quote preserves special characters in args"
      ;; Verify quoting actually works with real commands
      ;; Spaces preserved
      (define r1
        (run-subprocess "echo" #:args (list "hello world") #:limits (fast-limits #:timeout 5)))
      (check-equal? (subprocess-result-exit-code r1) 0)
      (check-not-false (regexp-match? #rx"hello world" (subprocess-result-stdout r1))
                       "shell-quote preserves spaces in argument")
      ;; Dollar sign not expanded
      (define r2
        (run-subprocess "echo" #:args (list "$FOO_BAR_TEST") #:limits (fast-limits #:timeout 5)))
      (check-equal? (subprocess-result-exit-code r2) 0)
      (check-not-false (regexp-match? #rx"[$]FOO_BAR_TEST" (subprocess-result-stdout r2))
                       "shell-quote prevents dollar expansion")
      ;; Semicolons not interpreted
      (define r3 (run-subprocess "echo" #:args (list "a;b") #:limits (fast-limits #:timeout 5)))
      (check-equal? (subprocess-result-exit-code r3) 0)
      (check-not-false (regexp-match? #rx"a;b" (subprocess-result-stdout r3))
                       "shell-quote prevents semicolon injection"))

    ;; ============================================================
    ;; SP6: combined stdout + stderr when both are non-empty
    ;; ============================================================
    (test-case "SP6: stdout and stderr are both captured"
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "echo stdout-msg; echo stderr-msg >&2")
                        #:limits (fast-limits #:timeout 5)))
      (check-equal? (subprocess-result-exit-code result) 0 "command succeeds")
      (check-not-false (regexp-match? #rx"stdout-msg" (subprocess-result-stdout result))
                       "stdout contains stdout-msg")
      (check-not-false (regexp-match? #rx"stderr-msg" (subprocess-result-stderr result))
                       "stderr contains stderr-msg"))

    ;; ============================================================
    ;; SP7: child output larger than pipe buffer must not deadlock
    ;; ============================================================
    (test-case "SP7: large stdout larger than pipe buffer completes without timeout"
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "yes x | head -c 200000")
                        #:limits (fast-limits #:timeout 5 #:max-output 4096)))
      (check-equal? (subprocess-result-exit-code result) 0 "large stdout command succeeds")
      (check-false (subprocess-result-timed-out? result) "large stdout does not deadlock")
      (check-true (subprocess-result-truncated? result) "large stdout is marked truncated")
      (check-true (<= (string-length (subprocess-result-stdout result)) 4096)
                  "large stdout is capped at the byte budget"))

    (test-case "SP7: large stderr larger than pipe buffer completes without timeout"
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "yes e | head -c 200000 >&2")
                        #:limits (fast-limits #:timeout 5 #:max-output 4096)))
      (check-equal? (subprocess-result-exit-code result) 0 "large stderr command succeeds")
      (check-false (subprocess-result-timed-out? result) "large stderr does not deadlock")
      (check-true (subprocess-result-truncated? result) "large stderr is marked truncated")
      (check-true (<= (string-length (subprocess-result-stderr result)) 4096)
                  "large stderr is capped at the byte budget"))

    ;; ============================================================
    ;; SP8: exact byte-budget output is not truncation
    ;; ============================================================
    (test-case "SP8: exact max-output stdout is not marked truncated"
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "printf '%*s' 4096 '' | tr ' ' x")
                        #:limits (fast-limits #:timeout 5 #:max-output 4096)))
      (check-equal? (subprocess-result-exit-code result) 0 "exact-budget command succeeds")
      (check-false (subprocess-result-timed-out? result) "exact-budget command does not time out")
      (check-equal? (string-length (subprocess-result-stdout result)) 4096)
      (check-false (subprocess-result-truncated? result)
                   "exactly max-output bytes is not truncation"))

    ;; ============================================================
    ;; W1 (v0.99.77): F-18b timeout kill-after — SIGTERM then SIGKILL
    ;; ============================================================

    (test-case "SP9: SIGTERM-ignoring subprocess is SIGKILL'd on timeout (no survivor)"
      ;; A child that traps SIGTERM cannot be stopped by the first signal; the
      ;; timeout path must escalate to SIGKILL (to the whole process group
      ;; when setsid is available) so nothing survives the tool call.
      (define marker (format "qw1sp-sigterm-~a" (random 1000000)))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (cleanup-markers! marker)
                      (define result
                        (run-subprocess "/bin/bash"
                                        #:args
                                        (list "-c" (format "trap '' TERM; sleep 60 # ~a" marker))
                                        #:process-group? #t
                                        #:limits (fast-limits #:timeout 1)))
                      (check-true (subprocess-result-timed-out? result)
                                  "SIGTERM-ignoring child still reports timed-out?")
                      (check-equal? (subprocess-result-exit-code result) -9 "timeout exit code is -9")
                      (sleep 0.3)
                      (when setsid-available?
                        (check-equal? (matching-pids marker)
                                      '()
                                      (format "no ~a process survives SIGKILL escalation" marker))))
                    (lambda () (cleanup-markers! marker))))

    (test-case "SP10: SIGSTOP'd subprocess is SIGKILL'd on timeout (no T-state survivor)"
      ;; SIGTERM is NOT delivered to a SIGSTOP'd (T-state) process. Only
      ;; SIGKILL works. The timeout path must escalate so no stopped process
      ;; group survives — the W0 reproducer's exact scenario, as a contract
      ;; test in the shared edge-case suite.
      (define marker (format "qw1sp-stop-~a" (random 1000000)))
      (dynamic-wind
       (lambda () (void))
       (lambda ()
         (cleanup-markers! marker)
         (define result
           (run-subprocess "/bin/bash"
                           #:args (list "-c" (format "kill -STOP $$; sleep 60 # ~a" marker))
                           #:process-group? #t
                           #:limits (fast-limits #:timeout 1)))
         (check-true (subprocess-result-timed-out? result) "SIGSTOP'd child reports timed-out?")
         (check-equal? (subprocess-result-exit-code result) -9 "timeout exit code is -9")
         (sleep 0.3)
         (when setsid-available?
           (check-equal? (matching-pids marker)
                         '()
                         (format "no T-state ~a process survives" marker))))
       (lambda () (cleanup-markers! marker))))

    (test-case "SP11: bash-isms evaluate under /bin/bash (no Bad substitution)"
      ;; D3 shell compat: the bash tool executes under /bin/bash, so
      ;; ${PIPESTATUS[0]} must evaluate — never a bare "Bad substitution"
      ;; silent exit-2 abort (F-20).
      (define result
        (run-subprocess "/bin/bash"
                        #:args '("-c" "false | true; echo PS=${PIPESTATUS[0]}")
                        #:limits (fast-limits #:timeout 5)))
      (check-equal? (subprocess-result-exit-code result) 0 "PIPESTATUS command succeeds")
      (check-not-false (regexp-match? #rx"PS=1" (subprocess-result-stdout result))
                       (format "PIPESTATUS[0] is 1, got: ~a" (subprocess-result-stdout result)))
      (check-false (regexp-match? #rx"Bad substitution"
                                  (string-append (subprocess-result-stdout result)
                                                 (subprocess-result-stderr result)))
                   "no 'Bad substitution' error under bash"))

    (test-case "SP12: sh PIPESTATUS outcome is recorded per implementation (baseline for D3)"
      ;; Documents the F-20 misattribution: dash aborts with a silent exit-2
      ;; "Bad substitution"; bash-as-sh (macOS /bin/sh) instead evaluates
      ;; PIPESTATUS. The bash tool no longer uses /bin/sh, so neither surface
      ;; is exercised in production — the contract is that /bin/sh produces a
      ;; predictable, implementation-recorded outcome on both platforms.
      (define result
        (run-subprocess "/bin/sh"
                        #:args '("-c" "false | true; echo PS=${PIPESTATUS[0]}")
                        #:limits (fast-limits #:timeout 5)))
      (cond
        [sh-is-dash?
         (check-equal? (subprocess-result-exit-code result) 2 "dash exits 2 on Bad substitution")
         (check-not-false (regexp-match? #rx"Bad substitution" (subprocess-result-stderr result))
                          "stderr explains the failure")]
        [else
         (check-equal? (subprocess-result-exit-code result) 0 "bash-as-sh evaluates PIPESTATUS")
         (check-not-false (regexp-match? #rx"PS=1" (subprocess-result-stdout result))
                          "PIPESTATUS[0] is 1 under bash-as-sh")]))))

(module+ main
  (run-tests subprocess-edge-tests))

(module+ test
  (run-tests subprocess-edge-tests))

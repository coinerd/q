#lang racket/base

;; @suite harness
;; @speed fast
;; @boundary integration
;;
;; NOTE: This reproducer was tagged slow during W0 because it was
;; intentionally red (documenting the F-18b leak). Since the W1 fix landed
;; (SIGTERM then SIGKILL to the process group on timeout), it passes and is
;; a permanent fast-gate regression test.
;; IMPORTANT: the metadata parser takes the LAST speed tag in the first
;; 50 lines, so no other line in this header may mention a speed tag.

;; tests/test-tool-call-freeze.rkt — W0 reproducer for F-6/F-18 + F-18b
;;
;; Agent-harness reliability: a long-running foreground tool call must either
;; complete and return its result, or return a timeout/error result. It must
;; NEVER leave a live but stopped (T-state) subprocess group behind — a leaked
;; stopped process holds inherited pipes open and can deadlock the agent turn.
;;
;; Defects reproduced (fail BEFORE the W1 fix):
;;   F-18b: The tool timeout path calls (subprocess-kill sp) with one argument,
;;          which is an arity error in this Racket (subprocess-kill takes 2
;;          args: subprocess and kill?). The error is swallowed by with-handlers,
;;          so no signal is delivered. custodian-shutdown-all sends SIGTERM,
;;          which is NOT delivered to stopped (SIGSTOP'd) processes. Result:
;;          the tool call returns a timeout result, but the stopped process
;;          group LEAKS and stays in T state.
;;
;; This test asserts:
;;   1. The tool call returns within a bounded deadline (does not hang forever).
;;   2. No T-state process matching the unique marker survives after the call.
;; Assertion 2 FAILS today (leaked T-state process). After the W1 fix
;; (SIGKILL to the process group on timeout), assertion 2 passes.

(require rackunit
         rackunit/text-ui
         racket/system
         racket/string
         racket/port
         "../sandbox/subprocess.rkt"
         "../sandbox/limits.rkt")

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

(define (fast-limits [timeout 5])
  (exec-limits timeout 1048576 536870912 10))

;; A unique marker embedded in the child command so `ps` detection only finds
;; THIS test's leaked processes (safe under the parallel gate).
(define marker
  (format "qfreeze-~a-~a" (inexact->exact (round (current-inexact-milliseconds))) (random 1000000)))

;; Build a command that SIGSTOPs the shell immediately, then would sleep 60s.
;; The marker appears in the process argv so we can detect leaks.
(define (stopped-command)
  (format "kill -STOP $$; sleep 60 # ~a" marker))

;; Run the real run-subprocess path (same as tool-bash: bash -c ...) with a
;; short timeout, in process-group mode so the W1 SIGTERM→SIGKILL escalation
;; exercises the exact tool path. Returns the subprocess-result.
(define (run-stopped-child #:timeout [timeout 3])
  (run-subprocess "/bin/bash"
                  #:args (list "-c" (stopped-command))
                  #:process-group? #t
                  #:limits (fast-limits timeout)))

;; Return the list of PIDs still running (any state) that match our marker.
(define (matching-pids)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [code (parameterize ([current-output-port out]
                              [current-error-port err])
                 (system (format "ps -eo pid,stat,cmd | grep '~a' | grep -v grep || true" marker)))])
    (for/list ([line (in-list (string-split (get-output-string out) "\n"))]
               #:when (regexp-match? #rx"qfreeze-" line))
      (car (string-split line)))))

;; Return list of (pid . state) for our marker processes still in stopped state.
(define (stopped-survivors)
  (let* ([out (open-output-string)]
         [err (open-output-string)]
         [code
          (parameterize ([current-output-port out]
                         [current-error-port err])
            (system
             (format
              "ps -eo pid,stat,cmd | grep '~a' | grep -v grep | awk '$2 ~~ /T/ {print $1 \" \" $2}' || true"
              marker)))])
    (for/list ([line (in-list (string-split (get-output-string out) "\n"))]
               #:when (not (string=? line "")))
      (string-split line))))

;; Clean up any leaked marker processes (SIGKILL — works on stopped processes).
;; Called in the test teardown so a failing assertion does not leak zombies.
(define (cleanup-leaks!)
  (for ([pid (in-list (matching-pids))])
    (system (format "kill -KILL ~a 2>/dev/null || true" pid)))
  (sleep 0.2))

;; ------------------------------------------------------------
;; Tests
;; ------------------------------------------------------------

(define tool-call-freeze-tests
  (test-suite "tool-call-freeze (F-6/F-18/F-18b)"
    (test-case "F-18b: tool timeout returns bounded result but must not leak a stopped process group"
      ;; dynamic-wind guarantees teardown even when a check fails (a failed
      ;; check aborts the test-case body, which would otherwise leak the
      ;; stopped process into the gate environment).
      (dynamic-wind
       (lambda () (void))
       (lambda ()
         ;; Baseline: no leftover marker processes before we start.
         (cleanup-leaks!)
         (define start-ms (current-inexact-milliseconds))
         (define result (run-stopped-child #:timeout 3))
         (define elapsed-ms (- (current-inexact-milliseconds) start-ms))

         ;; Assertion 1: the call returns within a bounded deadline.
         ;; 3s timeout + up to 2s reader-join grace + slack = < 15s.
         (check-true (< elapsed-ms 15000) (format "tool call returned in ~ams (bounded)" elapsed-ms))
         (check-true (subprocess-result-timed-out? result) "subprocess reports timed-out?")
         (check-equal? (subprocess-result-exit-code result) -9 "timeout exit code is -9")

         ;; Assertion 2: NO stopped (T-state) process survives the timeout.
         ;; THIS FAILS TODAY (F-18b): the SIGSTOP'd shell leaks in T state
         ;; because the arity-broken subprocess-kill sends no signal and
         ;; SIGTERM does not reach stopped processes.
         (sleep 0.2)
         (define survivors (stopped-survivors))
         (check-equal? survivors
                       '()
                       (format "no T-state survivors after tool timeout (leaked: ~a)"
                               (if (pair? survivors) survivors "(none)"))))
       (lambda ()
         ;; Teardown: ensure no marker processes remain even on failure.
         (cleanup-leaks!))))))

(module+ main
  (run-tests tool-call-freeze-tests))

(module+ test
  (run-tests tool-call-freeze-tests))

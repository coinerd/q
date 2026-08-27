#lang racket/base

;; tests/test-gsd-infra-retry-policy.rkt — BUG-0042 (v1.00.21 W7)
;;
;; CHARACTERIZATION tests for the infra-retry policy seam (infra failure
;; classification, outcome routing, retry backoff sequence, prior-attempt
;; context building/joining). Written BEFORE the extraction from
;; extensions/gsd/go-orchestrator.rkt into extensions/gsd/infra-retry-policy.rkt
;; and run unchanged AFTER it: the extracted module must reproduce every
;; pinned decision byte-for-byte (same expectations file, no edits between
;; the two runs). Imports go through go-orchestrator.rkt, which re-exports
;; the extracted seam after the move (compat shim — no mass test edits).

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../util/loop-result.rkt"
                  make-loop-result
                  loop-result?
                  loop-result-termination-reason)
         (only-in "../extensions/gsd/wave-runner-port.rkt"
                  wave-execution-outcome-kind
                  wave-execution-outcome-message
                  wave-execution-outcome)
         (only-in "../extensions/gsd/policy.rkt"
                  current-gsd-campaign-infra-retry-delay
                  current-gsd-campaign-infra-retries)
         (only-in "../extensions/gsd/go-orchestrator.rkt"
                  infra-failure?
                  prompt-run-result->outcome
                  build-wave-attempt-context
                  append-dirty-capture-to-context
                  take-up-to))

(define suite
  (test-suite "infra-retry-policy characterization (BUG-0042)"

    ;; ----------------------------------------------------------------
    ;; Backoff sequence (default): 30s → 60s → 120s → flat 120s
    ;; ----------------------------------------------------------------
    (test-case "default infra-retry backoff: 30/60/120 then flat 120"
      (define delay (current-gsd-campaign-infra-retry-delay))
      (check-equal? (delay
                      0)
                    30)
      (check-equal? (delay
                      1)
                    30)
      (check-equal? (delay
                      2)
                    60)
      (check-equal? (delay
                      3)
                    120)
      (check-equal? (delay
                      4)
                    120)
      (check-equal? (delay
                      9)
                    120)
      ;; Default budget: 3 automatic retries (BUG-0024 W3).
      (check-equal? (current-gsd-campaign-infra-retries) 3))

    ;; ----------------------------------------------------------------
    ;; infra-failure? classification (D8 #9357)
    ;; ----------------------------------------------------------------
    (test-case "infra-failure?: provider/network domains are infra"
      ;; NOTE (characterization): infra-failure? may return a truthy memq
      ;; tail (e.g. '(provider)) instead of #t; callers only test truthiness.
      (check-not-false
       (infra-failure? (make-loop-result '() 'error (hasheq 'errorType '(provider . 500)))))
      (check-not-false
       (infra-failure? (make-loop-result '() 'error (hasheq 'errorType '(network . "conn")))))
      (check-false (infra-failure?
                    (make-loop-result '() 'error (hasheq 'errorType '(client . 400))))))

    (test-case "infra-failure?: retry-exhausted marker is infra"
      (check-true (infra-failure?
                   (make-loop-result '() 'error (hasheq 'retries-attempted 3 'error "boom"))))
      (check-false (infra-failure? (make-loop-result '() 'error (hasheq 'retries-attempted 0))))
      (check-false (infra-failure? (make-loop-result '() 'error (hasheq)))))

    (test-case "infra-failure?: transient stream messages are infra"
      (check-true (infra-failure?
                   (make-loop-result '() 'error (hasheq 'error "SSE stream read timeout"))))
      (check-true (infra-failure?
                   (make-loop-result '() 'error (hasheq 'error "connection reset by peer"))))
      (check-true (infra-failure?
                   (make-loop-result '() 'error (hasheq 'error "provider circuit open")))))

    (test-case "infra-failure?: logic failures and non-error terminations are NOT infra"
      (check-false (infra-failure? (make-loop-result '() 'error (hasheq 'error "assertion failed"))))
      (check-false (infra-failure? (make-loop-result '() 'completed (hasheq))))
      (check-false (infra-failure? (make-loop-result '() 'cancelled (hasheq))))
      (check-false (infra-failure? "not a loop result")))

    ;; ----------------------------------------------------------------
    ;; prompt-run-result->outcome routing (drives the retry decisions)
    ;; ----------------------------------------------------------------
    (test-case "outcome routing: completed without reason → done"
      (define r (prompt-run-result->outcome (make-loop-result '() 'completed (hasheq))))
      (check-eq? (wave-execution-outcome-kind r) 'done)
      (check-equal? (wave-execution-outcome-message r) ""))

    (test-case "outcome routing: graceful shutdown / cancelled → cancelled"
      (check-eq? (wave-execution-outcome-kind
                  (prompt-run-result->outcome
                   (make-loop-result '() 'completed (hasheq 'reason "graceful-shutdown"))))
                 'cancelled)
      (check-eq? (wave-execution-outcome-kind
                  (prompt-run-result->outcome (make-loop-result '() 'cancelled (hasheq))))
                 'cancelled))

    (test-case "outcome routing: completion blocked → failed with reason"
      (define r
        (prompt-run-result->outcome (make-loop-result '() 'completed (hasheq 'reason "tool-lint"))))
      (check-eq? (wave-execution-outcome-kind r) 'failed)
      (check-equal? (wave-execution-outcome-message r) "completion blocked: tool-lint"))

    (test-case "outcome routing: named terminations"
      (check-equal? (wave-execution-outcome-message
                     (prompt-run-result->outcome (make-loop-result '() 'tool-calls-pending (hasheq))))
                    "tool calls remain pending")
      (check-equal? (wave-execution-outcome-message
                     (prompt-run-result->outcome (make-loop-result '() 'empty-response (hasheq))))
                    "model returned an empty response")
      (check-equal?
       (wave-execution-outcome-message
        (prompt-run-result->outcome (make-loop-result '() 'error (hasheq 'error "logic boom"))))
       "termination reason: error"))

    (test-case "outcome routing: tool loop limit wins over everything"
      (define r
        (prompt-run-result->outcome (make-loop-result '() 'completed (hasheq 'toolLoopLimit #t))))
      (check-eq? (wave-execution-outcome-kind r) 'failed)
      (check-equal? (wave-execution-outcome-message r) "tool loop limit reached"))

    (test-case "outcome routing: infra failure → infra-failed, attempt preserved"
      (define r
        (prompt-run-result->outcome
         (make-loop-result '() 'error (hasheq 'errorType '(provider . 503)))))
      (check-eq? (wave-execution-outcome-kind r) 'infra-failed)
      (check-equal? (wave-execution-outcome-message r)
                    "provider/network failure — wave preserved (attempt not consumed)"))

    (test-case "outcome routing: stall-cause error → infra-failed with attempt-preserved suffix"
      (define stall-msg
        (string-append
         "mutation-stall watchdog: attempt terminated after 5 mutation-free "
         "calls (limit 3). Target files: (none recorded). Recent tools: (none recorded). "
         "The attempt will be re-attempted automatically with its prior "
         "context preserved — resume implementation from recorded state."))
      (define r (prompt-run-result->outcome (make-loop-result '() 'error (hasheq 'error stall-msg))))
      (check-eq? (wave-execution-outcome-kind r) 'infra-failed)
      (check-equal? (wave-execution-outcome-message r)
                    (string-append stall-msg " attempt preserved for automatic re-attempt")))

    (test-case "outcome routing: legacy symbol runner results"
      (check-eq? (wave-execution-outcome-kind (prompt-run-result->outcome 'ok)) 'done)
      (check-eq? (wave-execution-outcome-kind (prompt-run-result->outcome 'cancelled)) 'cancelled)
      (define r (prompt-run-result->outcome 'weird))
      (check-eq? (wave-execution-outcome-kind r) 'failed)
      (check-equal? (wave-execution-outcome-message r) "unknown runner result: weird"))

    ;; ----------------------------------------------------------------
    ;; Prior-attempt context (BUG-0024 W3): build + join + 2 KB cap
    ;; ----------------------------------------------------------------
    (test-case "build-wave-attempt-context: exact canonical text"
      (check-equal? (build-wave-attempt-context 5 2 "boom")
                    (string-append
                     "Prior attempt 2 of wave W5 ended in an INFRASTRUCTURE failure "
                     "(provider/network), not a logic failure. Work already done lives on "
                     "the attempt branch — check git status / git diff there before writing "
                     "anything.\nLast executor error: boom\n"
                     "Resume from that state; do NOT restart exploration from zero.")))

    (test-case "build-wave-attempt-context: hard-capped at 2048 chars"
      (define s (build-wave-attempt-context 0 9 (make-string 5000 #\x)))
      (check-equal? (string-length s) 2048)
      (check-true (string-suffix? s (make-string 256 #\x))))

    (test-case "append-dirty-capture-to-context: clean capture appends nothing"
      (define base "base context")
      (check-equal?
       (append-dirty-capture-to-context base (hasheq 'dirty-sha #f 'diff-stat "" 'edited-files '()))
       base)
      (check-equal? (append-dirty-capture-to-context base #f) base))

    (test-case "append-dirty-capture-to-context: dirty capture joins with canonical block"
      (check-equal?
       (append-dirty-capture-to-context
        "CTX"
        (hasheq 'dirty-sha "abc123" 'diff-stat " 1 file changed" 'edited-files '("a.rkt" "b.rkt")))
       (string-append "CTX\n"
                      "Dirty state captured at infra-stop (BUG-0030):\n"
                      "- dirty-sha-if-committed: abc123\n"
                      "- diff-summary-stat:  1 file changed\n"
                      "- edited-files: a.rkt, b.rkt\n")))

    (test-case "append-dirty-capture-to-context: untracked-only residue names no dirty-sha"
      (define s
        (append-dirty-capture-to-context
         "CTX"
         (hasheq 'dirty-sha #f 'diff-stat "" 'edited-files '("new.rkt"))))
      (check-true
       (string-contains? s "- dirty-sha-if-committed: none (clean or untracked-only residue)\n"))
      (check-true (string-contains? s "- edited-files: new.rkt\n")))

    (test-case "append-dirty-capture-to-context: joined context hard-capped at 2048 chars"
      (define s
        (append-dirty-capture-to-context
         "CTX"
         (hasheq 'dirty-sha "sha" 'diff-stat (make-string 5000 #\d) 'edited-files '("a.rkt"))))
      (check-equal? (string-length s) 2048)
      (check-true (string-prefix? s "CTX\nDirty state captured at infra-stop (BUG-0030):")))

    (test-case "take-up-to: bounded prefix"
      (check-equal? (take-up-to '(1 2 3) 2) '(1 2))
      (check-equal? (take-up-to '(1) 5) '(1))
      (check-equal? (take-up-to '() 3) '())
      (check-equal? (take-up-to '(1 2 3) 0) '()))))

(module+ main
  (exit (run-tests suite)))
(module+ test
  (void (run-tests suite)))

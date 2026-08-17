#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-policy.rkt — GSD Policy Engine tests
;;
;; Tests all mode × tool × action combinations for the unified
;; policy module introduced in v0.24.1 W0.

(require rackunit
         "../extensions/gsd/policy.rkt"
         "../extensions/gsd/state-machine.rkt"
         (only-in "../runtime/session/session-config.rkt" resolve-max-iterations-hard)
         (only-in "../runtime/session/session-config.rkt" hash->session-config))

;; ============================================================
;; Explicit campaign budgets
;; ============================================================

(test-case "GSD campaign budgets are finite and positive"
  (check-true (positive? (current-gsd-wave-timeout-seconds)))
  (check-true (exact-positive-integer? (current-gsd-wave-max-iterations)))
  (check-true (exact-positive-integer? (current-gsd-max-consecutive-tool-calls))))

(test-case "executor tool-loop limit clears implementation workloads (D3, issue #9351)"
  ;; Incident 81f9be4b W2: the executor died at the old default of 30
  ;; consecutive tool-only turns (attempt-3, tool-loop.limit-reached) while
  ;; the identical wave completed in the main session with 24-consecutive
  ;; bursts. Implementation waves must not be policy-killed; 60 is the
  ;; minimum credible floor.
  (check-true (>= (current-gsd-max-consecutive-tool-calls) 60)
              "current-gsd-max-consecutive-tool-calls default is too small for implementation waves"))

(test-case "GSD session iteration budget caps a larger configured budget"
  (parameterize ([current-gsd-wave-max-iterations 12])
    (check-equal? (gsd-session-iteration-budget 100) 12)
    (check-equal? (gsd-session-iteration-budget 8) 8)))

(test-case "wave iteration budget is high enough for implementation waves"
  ;; The user observed a live /go wave policy-cancelled at iteration 80
  ;; ("[SYS] [executing... iteration 79, 1 remaining before hard stop]").
  ;; That hard stop is derived from the wave session's max-iterations budget;
  ;; with the old default of 50 the hard ceiling was only 80
  ;; (resolve-max-iterations-hard = max(iter*8/5, 80)). Implementation waves
  ;; legitimately run many tool turns; the timeout + consecutive-tool breaker
  ;; are the real bounds, not a tiny iteration ceiling.
  (check-true (>= (current-gsd-wave-max-iterations) 1000)
              "current-gsd-wave-max-iterations default is too small; wave is iteration-killed")
  (define cfg (hash->session-config (hasheq 'max-iterations (current-gsd-wave-max-iterations))))
  (define derived-hard (resolve-max-iterations-hard cfg (current-gsd-wave-max-iterations)))
  ;; resolve-max-iterations-hard = max(iter*8/5, 80). With the raised budget the
  ;; hard ceiling is comfortably in the thousands (e.g. 3200 at 2000), so a wave
  ;; is bounded by the 1800s timeout and the consecutive-tool breaker, not by
  ;; an iteration kill at 80.
  (check-true (>= derived-hard (quotient (* 8 (current-gsd-wave-max-iterations)) 5))
              "derived hard limit should scale with the soft budget"))

;; ============================================================
;; blocked-tools-for
;; ============================================================

(test-case "idle mode blocks no tools"
  (check-equal? (blocked-tools-for 'idle) '()))

(test-case "exploring mode blocks no tools"
  (check-equal? (blocked-tools-for 'exploring) '()))

(test-case "plan-written blocks edit/write/bash"
  (check-equal? (sort (blocked-tools-for 'plan-written) string<?) '("bash" "edit" "write")))

(test-case "executing blocks planning-write"
  (check-equal? (blocked-tools-for 'executing) '("planning-write")))

(test-case "verifying blocks edit/write/bash/planning-write"
  (check-equal? (sort (blocked-tools-for 'verifying) string<?)
                '("bash" "edit" "planning-write" "write")))

;; ============================================================
;; gsd-decide-action: tool-call
;; ============================================================

(test-case "policy fails closed for unknown actions and malformed tool calls"
  (check-true (policy-blocked? (gsd-decide-action (hasheq 'mode 'executing) 'unknown-action)))
  (check-true (policy-blocked? (gsd-decide-action (hasheq 'mode 'executing 'tool "") 'tool-call))))

(test-case "tool-call: read allowed in all modes"
  (for ([mode '(idle exploring plan-written executing verifying)])
    (define d (gsd-decide-action (hasheq 'mode mode 'tool "read") 'tool-call))
    (check-true (policy-allowed? d) (format "read blocked in ~a" mode))))

(test-case "tool-call: edit blocked in plan-written and verifying"
  (for ([mode '(plan-written verifying)])
    (define d (gsd-decide-action (hasheq 'mode mode 'tool "edit") 'tool-call))
    (check-true (policy-blocked? d) (format "edit should be blocked in ~a" mode))))

(test-case "tool-call: edit allowed in idle, exploring, executing"
  (for ([mode '(idle exploring executing)])
    (define d (gsd-decide-action (hasheq 'mode mode 'tool "edit") 'tool-call))
    (check-true (policy-allowed? d) (format "edit should be allowed in ~a" mode))))

(test-case "tool-call: planning-write blocked in executing and verifying"
  (for ([mode '(executing verifying)])
    (define d (gsd-decide-action (hasheq 'mode mode 'tool "planning-write") 'tool-call))
    (check-true (policy-blocked? d) (format "planning-write should be blocked in ~a" mode))))

(test-case "tool-call: determinism — same inputs produce same result"
  (for* ([mode '(idle exploring plan-written executing verifying)]
         [tool '("read" "edit" "write" "bash" "planning-write")])
    (define d1 (gsd-decide-action (hasheq 'mode mode 'tool tool) 'tool-call))
    (define d2 (gsd-decide-action (hasheq 'mode mode 'tool tool) 'tool-call))
    (check-equal? d1 d2 (format "~a/~a not deterministic" mode tool))))

;; ============================================================
;; gsd-decide-action: write-file
;; ============================================================

(test-case "write-file: allowed in idle"
  (define d
    (gsd-decide-action (hasheq 'mode 'idle 'target-path "/tmp/x" 'pinned-dir "/tmp") 'write-file))
  (check-true (policy-allowed? d)))

(test-case "write-file: blocked when executing + in planning dir"
  (define d
    (gsd-decide-action (hasheq 'mode
                               'executing
                               'target-path
                               "/home/user/.planning/PLAN.md"
                               'pinned-dir
                               "/home/user/.planning")
                       'write-file))
  (check-true (policy-blocked? d))
  (check-not-false (regexp-match? #rx"write-blocked" (format "~a" (policy-tags d)))))

(test-case "write-file: allowed when executing + outside planning dir"
  (define d
    (gsd-decide-action
     (hasheq 'mode 'executing 'target-path "/tmp/out.txt" 'pinned-dir "/home/user/.planning")
     'write-file))
  (check-true (policy-allowed? d)))

;; ============================================================
;; gsd-decide-action: edit-plan
;; ============================================================

(test-case "edit-plan: blocked in executing"
  (define d (gsd-decide-action (hasheq 'mode 'executing) 'edit-plan))
  (check-true (policy-blocked? d)))

(test-case "edit-plan: allowed in idle"
  (define d (gsd-decide-action (hasheq 'mode 'idle) 'edit-plan))
  (check-true (policy-allowed? d)))

;; ============================================================
;; Integration: gsm-tool-allowed? routes through policy
;; ============================================================

(test-case "gsm-tool-allowed? matches policy for all modes"
  (reset-gsm!)
  ;; idle — all allowed
  (for ([tool '("read" "edit" "write" "bash" "planning-write")])
    (check-true (gsm-tool-allowed? tool) (format "~a should be allowed in idle" tool)))
  ;; plan-written — edit/write/bash blocked
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (check-false (gsm-tool-allowed? "edit"))
  (check-false (gsm-tool-allowed? "write"))
  (check-false (gsm-tool-allowed? "bash"))
  (check-true (gsm-tool-allowed? "read"))
  (check-true (gsm-tool-allowed? "planning-read"))
  ;; verifying — most blocked
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (check-false (gsm-tool-allowed? "edit"))
  (check-false (gsm-tool-allowed? "write"))
  (check-false (gsm-tool-allowed? "bash"))
  (check-false (gsm-tool-allowed? "planning-write"))
  (check-true (gsm-tool-allowed? "read")))

;; ============================================================
;; policy-decision struct
;; ============================================================

(test-case "policy-decision accessors"
  (define d (policy-decision #f "test reason" '(tag1 tag2)))
  (check-false (policy-allowed? d))
  (check-true (policy-blocked? d))
  (check-equal? (policy-reason d) "test reason")
  (check-equal? (policy-tags d) '(tag1 tag2)))

(test-case "allowed decision"
  (define d (policy-decision #t #f '(ok)))
  (check-true (policy-allowed? d))
  (check-false (policy-blocked? d))
  (check-false (policy-reason d)))

(test-case "write-file policy: blocks writing to planning dir during execution"
  (define d
    (gsd-decide-action (hasheq 'mode
                               'executing
                               'target-path
                               "/project/.planning/PLAN.md"
                               'pinned-dir
                               "/project/.planning")
                       'write-file))
  (check-true (policy-blocked? d))
  (check-not-false (policy-reason d)))

(test-case "write-file policy: allows writing outside planning dir during execution"
  (define d
    (gsd-decide-action
     (hasheq 'mode 'executing 'target-path "/project/src/foo.rkt" 'pinned-dir "/project/.planning")
     'write-file))
  (check-true (policy-allowed? d)))

(test-case "write-file policy: allows writing in non-executing mode"
  (for ([mode '(idle exploring plan-written verifying)])
    (define d
      (gsd-decide-action
       (hasheq 'mode mode 'target-path "/project/.planning/PLAN.md" 'pinned-dir "/project/.planning")
       'write-file))
    (check-true (policy-allowed? d) (format "write should be allowed in ~a" mode))))

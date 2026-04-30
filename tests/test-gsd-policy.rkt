#lang racket

;; tests/test-gsd-policy.rkt — GSD Policy Engine tests
;;
;; Tests all mode × tool × action combinations for the unified
;; policy module introduced in v0.24.1 W0.

(require rackunit
         "../extensions/gsd/policy.rkt"
         "../extensions/gsd/state-machine.rkt")

;; ============================================================
;; blocked-tools-for
;; ============================================================

(test-case "idle mode blocks no tools"
  (check-equal? (blocked-tools-for 'idle) '()))

(test-case "exploring mode blocks no tools"
  (check-equal? (blocked-tools-for 'exploring) '()))

(test-case "plan-written blocks edit/write/bash"
  (check-equal? (sort (blocked-tools-for 'plan-written) string<?)
                '("bash" "edit" "write")))

(test-case "executing blocks planning-write"
  (check-equal? (blocked-tools-for 'executing) '("planning-write")))

(test-case "verifying blocks edit/write/bash/planning-write"
  (check-equal? (sort (blocked-tools-for 'verifying) string<?)
                '("bash" "edit" "planning-write" "write")))

;; ============================================================
;; gsd-decide-action: tool-call
;; ============================================================

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
  (define d (gsd-decide-action (hasheq 'mode 'idle 'target-path "/tmp/x" 'pinned-dir "/tmp") 'write-file))
  (check-true (policy-allowed? d)))

(test-case "write-file: blocked when executing + in planning dir"
  (define d (gsd-decide-action
             (hasheq 'mode 'executing 'target-path "/home/user/.planning/PLAN.md" 'pinned-dir "/home/user/.planning")
             'write-file))
  (check-true (policy-blocked? d))
  (check-not-false (regexp-match? #rx"write-blocked" (format "~a" (policy-tags d)))))

(test-case "write-file: allowed when executing + outside planning dir"
  (define d (gsd-decide-action
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

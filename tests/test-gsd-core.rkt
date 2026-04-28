#lang racket

;; tests/test-gsd-core.rkt — Core GSD extension tests
;;
;; Wave 1 of v0.21.0: Tests for command dispatch, tool guard,
;; write guard, and status display.

(require rackunit
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/state-machine.rkt"
         (only-in "../extensions/gsd-planning.rkt" gsd-tool-guard)
         (only-in "../util/hook-types.rkt" hook-result-action))

;; ============================================================
;; Command dispatch: /plan
;; ============================================================

(test-case "/plan: transitions to exploring"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'plan "Fix the bug"))
  (check-equal? (hash-ref r 'success) #t)
  (check-eq? (hash-ref r 'mode) 'exploring))

(test-case "/plan: includes user text in message"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'plan "Investigate crash"))
  (check-true (string-contains? (hash-ref r 'message) "Investigate crash")))
(test-case "/plan: succeeds from plan-written (same as replan)"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'plan "re-plan"))
  (check-equal? (hash-ref r 'success) #t)
  (check-eq? (hash-ref r 'mode) 'exploring))

;; ============================================================
;; Command dispatch: /go
;; ============================================================

(test-case "/go: transitions to executing from plan-written"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'go ""))
  (check-equal? (hash-ref r 'success) #t)
  (check-eq? (hash-ref r 'mode) 'executing))

(test-case "/go: fails from idle"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'go ""))
  (check-equal? (hash-ref r 'success) #f))

(test-case "/go: fails from exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsd-command-dispatch 'go ""))
  (check-equal? (hash-ref r 'success) #f))

(test-case "/go: accepts wave argument"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'go "2"))
  (check-equal? (hash-ref r 'success) #t)
  (check-true (string-contains? (hash-ref r 'message) "wave 2")))

;; ============================================================
;; Command dispatch: /replan
;; ============================================================

(test-case "/replan: from plan-written to exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'replan #f))
  (check-equal? (hash-ref r 'success) #t)
  (check-eq? (hash-ref r 'mode) 'exploring))

(test-case "/replan: from executing to exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'replan #f))
  (check-equal? (hash-ref r 'success) #t)
  (check-eq? (hash-ref r 'mode) 'exploring))

(test-case "/replan: fails from idle"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'replan #f))
  (check-equal? (hash-ref r 'success) #f))

(test-case "/replan: fails from exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsd-command-dispatch 'replan #f))
  (check-equal? (hash-ref r 'success) #f))

;; ============================================================
;; Command dispatch: /skip
;; ============================================================

(test-case "/skip: works during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'skip "3"))
  (check-equal? (hash-ref r 'success) #t)
  (check-true (string-contains? (hash-ref r 'message) "3")))

(test-case "/skip: fails outside executing"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'skip "1"))
  (check-equal? (hash-ref r 'success) #f))

(test-case "/skip: fails with no wave number"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'skip "abc"))
  (check-equal? (hash-ref r 'success) #f))

;; ============================================================
;; Command dispatch: /reset
;; ============================================================

(test-case "/reset: goes to idle from any state"
  (for ([states (list '(exploring plan-written executing verifying))])
    (reset-gsm!)
    (for ([s states])
      (gsm-transition! s))
    (define r (gsd-command-dispatch 'reset #f))
    (check-equal? (hash-ref r 'success) #t)
    (check-eq? (hash-ref r 'mode) 'idle)))

;; ============================================================
;; Command dispatch: unknown command
;; ============================================================

(test-case "unknown command returns #f"
  (reset-gsm!)
  (check-false (gsd-command-dispatch 'foobar "")))

;; ============================================================
;; Tool guard
;; ============================================================

(test-case "tool guard: allows all tools in idle"
  (reset-gsm!)
  (for ([tool '("read" "edit" "write" "bash" "planning-read" "planning-write")])
    (check-eq? (hook-result-action (gsd-tool-guard (hasheq 'tool-name tool)))
               'pass
               (format "~a in idle" tool))))

(test-case "tool guard: allows all tools in exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (for ([tool '("read" "edit" "write" "bash" "planning-read" "planning-write")])
    (check-eq? (hook-result-action (gsd-tool-guard (hasheq 'tool-name tool)))
               'pass
               (format "~a in exploring" tool))))

(test-case "tool guard: blocks edit/write/bash in plan-written"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (for ([tool '("edit" "write" "bash")])
    (define r (gsd-tool-guard (hasheq 'tool-name tool)))
    (check-eq? (hook-result-action r) 'block (format "~a should be blocked" tool))))

(test-case "tool guard: allows read/planning-read in plan-written"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (check-eq? (hook-result-action (gsd-tool-guard (hasheq 'tool-name "read"))) 'pass)
  (check-eq? (hook-result-action (gsd-tool-guard (hasheq 'tool-name "planning-read"))) 'pass))

(test-case "tool guard: blocks planning-write in executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-tool-guard (hasheq 'tool-name "planning-write")))
  (check-eq? (hook-result-action r) 'block))

(test-case "tool guard: allows edit/write/bash in executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (for ([tool '("edit" "write" "bash" "read")])
    (check-eq? (hook-result-action (gsd-tool-guard (hasheq 'tool-name tool)))
               'pass
               (format "~a in executing" tool))))

(test-case "tool guard: blocks edit/write/bash in verifying"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (for ([tool '("edit" "write" "bash")])
    (define r (gsd-tool-guard (hasheq 'tool-name tool)))
    (check-eq? (hook-result-action r) 'block (format "~a should be blocked" tool))))

;; ============================================================
;; Write guard
;; ============================================================

(test-case "write guard: blocks PLAN.md during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard ".planning/PLAN.md" ".planning"))
  (check-true (hash? r))
  (check-equal? (hash-ref r 'blocked #f) #t))

(test-case "write guard: allows non-PLAN.md during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (gsd-write-guard "src/fix.rkt" ".planning")))

(test-case "write guard: allows PLAN.md during idle"
  (reset-gsm!)
  (check-true (gsd-write-guard ".planning/PLAN.md" ".planning")))

(test-case "write guard: allows PLAN.md during exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-true (gsd-write-guard ".planning/PLAN.md" ".planning")))

;; ============================================================
;; Status display
;; ============================================================

(test-case "/gsd shows current mode and valid transitions"
  (reset-gsm!)
  (define r (gsd-show-status))
  (check-eq? (hash-ref r 'mode) 'idle)
  (check-true (list? (hash-ref r 'valid-next-states))))

;; ============================================================
;; Write guard hardening (DD-6)
;; ============================================================

(test-case "write guard: blocks .. traversal to PLAN.md"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard "src/../.planning/PLAN.md" ".planning"))
  (check-true (hash? r))
  (check-equal? (hash-ref r 'blocked #f) #t))

(test-case "write guard: blocks multiple .. traversal"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard "a/b/../../.planning/PLAN.md" ".planning"))
  (check-true (hash? r))
  (check-equal? (hash-ref r 'blocked #f) #t))

(test-case "write guard: blocks other .planning files during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard ".planning/STATE.md" ".planning"))
  (check-true (hash-ref r 'blocked #f)))

(test-case "write guard: allows files outside .planning"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (gsd-write-guard "src/fix.rkt" ".planning")))

;; ============================================================
;; W4: Hardened path guard tests (F6)
;; ============================================================

(test-case "W4: write guard blocks .planning/waves/W0-slug.md during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard ".planning/waves/W0-slug.md" ".planning"))
  (check-true (hash-ref r 'blocked #f)))

(test-case "W4: write guard allows q/foo.rkt during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (gsd-write-guard "q/foo.rkt" ".planning")))

(test-case "W4: write guard allows during exploring mode"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-true (gsd-write-guard ".planning/PLAN.md" ".planning")))

(test-case "W4: write guard allows during idle mode"
  (reset-gsm!)
  (check-true (gsd-write-guard ".planning/PLAN.md" ".planning")))

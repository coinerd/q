#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-core.rkt — Core GSD extension tests
;;
;; Wave 1 of v0.21.0: Tests for command dispatch, tool guard,
;; write guard, and status display.
;; v0.24.0: Updated to use gsd-command-result struct accessors.

(require rackunit
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/state-machine.rkt"
         (only-in "../extensions/gsd-planning.rkt" gsd-tool-guard)
         (only-in "../util/hook-types.rkt" hook-result-action)
         (only-in "../extensions/gsd/policy.rkt" policy-decision? policy-allowed? policy-blocked?))

;; ============================================================
;; Command dispatch: /plan
;; ============================================================

(test-case "/plan: transitions to exploring"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'plan "Fix the bug"))
  (check-true (gsd-command-result-success r))
  (check-eq? (gsd-command-result-mode r) 'exploring))

(test-case "/plan: includes user text in message"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'plan "Investigate crash"))
  (check-true (string-contains? (gsd-command-result-message r) "Investigate crash")))

(test-case "/plan: succeeds from plan-written (same as replan)"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'plan "re-plan"))
  (check-true (gsd-command-result-success r))
  (check-eq? (gsd-command-result-mode r) 'exploring))

;; ============================================================
;; Command dispatch: /go
;; ============================================================

(test-case "/go: transitions to executing from plan-written"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'go ""))
  (check-true (gsd-command-result-success r))
  (check-eq? (gsd-command-result-mode r) 'executing))

(test-case "/go: fails from idle"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'go ""))
  (check-false (gsd-command-result-success r)))

(test-case "/go: fails from exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsd-command-dispatch 'go ""))
  (check-false (gsd-command-result-success r)))

(test-case "/go: accepts wave argument"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'go "2"))
  (check-true (gsd-command-result-success r))
  (check-true (string-contains? (gsd-command-result-message r) "wave 2")))

;; ============================================================
;; Command dispatch: /replan
;; ============================================================

(test-case "/replan: from plan-written to exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define r (gsd-command-dispatch 'replan #f))
  (check-true (gsd-command-result-success r))
  (check-eq? (gsd-command-result-mode r) 'exploring))

(test-case "/replan: from executing to exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'replan #f))
  (check-true (gsd-command-result-success r))
  (check-eq? (gsd-command-result-mode r) 'exploring))

(test-case "/replan: fails from idle"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'replan #f))
  (check-false (gsd-command-result-success r)))

(test-case "/replan: fails from exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (define r (gsd-command-dispatch 'replan #f))
  (check-false (gsd-command-result-success r)))

;; ============================================================
;; Command dispatch: /skip
;; ============================================================

(test-case "/skip: works during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'skip "3"))
  (check-true (gsd-command-result-success r))
  (check-true (string-contains? (gsd-command-result-message r) "3")))

(test-case "/skip: fails outside executing"
  (reset-gsm!)
  (define r (gsd-command-dispatch 'skip "1"))
  (check-false (gsd-command-result-success r)))

(test-case "/skip: fails with no wave number"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-command-dispatch 'skip "abc"))
  (check-false (gsd-command-result-success r)))

;; ============================================================
;; Command dispatch: /reset
;; ============================================================

(test-case "/reset: goes to idle from any state"
  (for ([states (list '(exploring plan-written executing verifying))])
    (reset-gsm!)
    (for ([s states])
      (gsm-transition! s))
    (define r (gsd-command-dispatch 'reset #f))
    (check-true (gsd-command-result-success r))
    (check-eq? (gsd-command-result-mode r) 'idle)))

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
  (check-true (policy-decision? r))
  (check-true (policy-blocked? r)))

(test-case "write guard: allows non-PLAN.md during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (policy-allowed? (gsd-write-guard "src/fix.rkt" ".planning"))))

(test-case "write guard: allows PLAN.md during idle"
  (reset-gsm!)
  (check-true (policy-allowed? (gsd-write-guard ".planning/PLAN.md" ".planning"))))

(test-case "write guard: allows PLAN.md during exploring"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-true (policy-allowed? (gsd-write-guard ".planning/PLAN.md" ".planning"))))

;; ============================================================
;; Status display
;; ============================================================

(test-case "/gsd shows current mode"
  (reset-gsm!)
  (define r (gsd-show-status))
  (check-true (gsd-result? r))
  (check-eq? (gsd-command-result-mode r) 'idle))

;; ============================================================
;; Write guard hardening (DD-6)
;; ============================================================

(test-case "write guard: blocks .. traversal to PLAN.md"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard "src/../.planning/PLAN.md" ".planning"))
  (check-true (policy-decision? r))
  (check-true (policy-blocked? r)))

(test-case "write guard: blocks multiple .. traversal"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard "a/b/../../.planning/PLAN.md" ".planning"))
  (check-true (policy-decision? r))
  (check-true (policy-blocked? r)))

(test-case "write guard: blocks other .planning files during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard ".planning/STATE.md" ".planning"))
  (check-true (policy-blocked? r)))

(test-case "write guard: allows files outside .planning"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (policy-allowed? (gsd-write-guard "src/fix.rkt" ".planning"))))

;; ============================================================
;; W4: Hardened path guard tests (F6)
;; ============================================================

(test-case "W4: write guard blocks .planning/waves/W0-slug.md during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define r (gsd-write-guard ".planning/waves/W0-slug.md" ".planning"))
  (check-true (policy-blocked? r)))

(test-case "W4: write guard allows q/foo.rkt during executing"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (check-true (policy-allowed? (gsd-write-guard "q/foo.rkt" ".planning"))))

(test-case "W4: write guard allows during exploring mode"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (check-true (policy-allowed? (gsd-write-guard ".planning/PLAN.md" ".planning"))))

(test-case "W4: write guard allows during idle mode"
  (reset-gsm!)
  (check-true (policy-allowed? (gsd-write-guard ".planning/PLAN.md" ".planning"))))

;; ============================================================
;; v0.24.1: Transaction wrapper tests
;; ============================================================

(test-case "transaction: successful wave-done updates state"
  (reset-gsm!)
  (gsm-set-total-waves! 3)
  (define result (gsd-command-dispatch 'wave-done "0"))
  (check-true (gsd-command-result-success result))
  (check-true (gsm-wave-complete? 0)))

(test-case "transaction: invalid wave number returns error"
  (reset-gsm!)
  (define result (gsd-command-dispatch 'wave-done "abc"))
  (check-false (gsd-command-result-success result))
  (check-not-false (string-contains? (gsd-command-result-message result) "Invalid")))

(test-case "transaction: negative wave number returns error"
  (reset-gsm!)
  (define result (gsd-command-dispatch 'wave-done "-1"))
  (check-false (gsd-command-result-success result))
  (check-not-false (string-contains? (gsd-command-result-message result) "non-negative")))

;; ============================================================
;; W2: Archive result migration + transaction tests
;; ============================================================

(test-case "cmd-done returns gsd-command-result on missing plan"
  (reset-gsm!)
  (define result (cmd-done "/nonexistent/path"))
  (check-false (gsd-command-result-success result))
  (check-not-false (string-contains? (gsd-command-result-message result) "No PLAN.md")))

(test-case "cmd-done returns gsd-command-result on incomplete waves"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  ;; Create a temp dir with an incomplete plan
  (define tmp-dir (make-temporary-file "gsd-archive-test-~a" 'directory))
  (define planning-dir (build-path tmp-dir ".planning"))
  (make-directory* planning-dir)
  (call-with-output-file
   (build-path planning-dir "PLAN.md")
   (lambda (out)
     (display "# Plan: Test\n## Wave 0: Fix\n- File: foo.rkt\n<!-- status: [Inbox] -->\n" out))
   #:exists 'truncate)
  (define result (cmd-done tmp-dir))
  (check-false (gsd-command-result-success result))
  (check-not-false (string-contains? (gsd-command-result-message result) "Not all waves"))
  (delete-directory/files tmp-dir))

(test-case "write guard returns policy-decision type"
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (define blocked (gsd-write-guard ".planning/PLAN.md" ".planning"))
  (check-true (policy-decision? blocked))
  (check-true (policy-blocked? blocked))
  (define allowed (gsd-write-guard "src/foo.rkt" ".planning"))
  (check-true (policy-decision? allowed))
  (check-true (policy-allowed? allowed)))

(test-case "cmd-go is deprecated — bare state transition only"
  ;; cmd-go was removed from provide in v0.29.13. Test via dispatch.
  (reset-gsm!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (define result (gsd-command-dispatch 'go ""))
  (check-true (gsd-command-result-success result))
  (check-not-false (string-contains? (gsd-command-result-message result) "Execution started")))

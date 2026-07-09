#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w3-gsd.rkt — GSD subsystem real-world audit
;;
;; Audit of the GSD (Get Stuff Done) subsystem covering:
;;   1. Pure transition logic (state table, valid-targets, path finder)
;;   2. State machine lifecycle (idle → exploring → plan-written → executing → verifying)
;;   3. Context-aware state machine API (isolated sessions)
;;   4. Wave state tracking (completion, pending, counters)
;;   5. Policy engine (tool blocking, write guard, edit-plan guard)
;;   6. Rework-loop protection (max iterations enforcement)
;;   7. State invariants (structural correctness checks)
;;   8. Command parser (pure AST: /plan, /go, /skip, /reset, etc.)
;;   9. Plan types (creation, validation, normalization, status conversion)
;;  10. Wave status constants and helpers
;;
;; FINDING: GSD state machine is robust with explicit transition table,
;; BFS path-finding for multi-step transitions, and per-session isolation.
;;
;; All tests use synthetic data — no real API keys or filesystem needed.

(require rackunit
         racket/set
         racket/list
         racket/string
         ;; Pure transition logic
         "../extensions/gsd/transition-logic.rkt"
         ;; Session state (per-session contexts)
         "../extensions/gsd/session-state.rkt"
         ;; State machine (uses session-state internally, re-exports runtime-state-types)
         "../extensions/gsd/state-machine.rkt"
         ;; Policy engine
         "../extensions/gsd/policy.rkt"
         ;; Wave status constants
         "../extensions/gsd/wave-status.rkt"
         ;; Command parser (pure AST)
         "../extensions/gsd/command-parser.rkt"
         ;; Plan types (typed/racket boundary)
         "../extensions/gsd/plan-types.rkt")

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (with-fresh-gsd-state thunk)
  (reset-gsm!)
  (thunk)
  (reset-gsm!))

;; ---------------------------------------------------------------------------
;; 1. Pure Transition Logic
;; ---------------------------------------------------------------------------

(test-case "audit-states-defined"
  (check-equal? GSD-STATES '(idle exploring plan-written executing verifying))
  (check-true (gsm-state? 'idle))
  (check-true (gsm-state? 'exploring))
  (check-true (gsm-state? 'plan-written))
  (check-true (gsm-state? 'executing))
  (check-true (gsm-state? 'verifying))
  (check-false (gsm-state? 'unknown))
  (check-false (gsm-state? "idle")))

(test-case "audit-valid-transitions"
  (check-true (valid-transition? 'idle 'exploring))
  (check-true (valid-transition? 'exploring 'plan-written))
  (check-true (valid-transition? 'plan-written 'executing))
  (check-true (valid-transition? 'executing 'verifying))
  (check-true (valid-transition? 'verifying 'idle))
  ;; Cancel transitions
  (check-true (valid-transition? 'exploring 'idle))
  (check-true (valid-transition? 'plan-written 'idle))
  (check-true (valid-transition? 'executing 'idle)))

(test-case "audit-invalid-transitions"
  (check-false (valid-transition? 'idle 'executing) "Cannot jump idle→executing")
  (check-false (valid-transition? 'idle 'verifying) "Cannot jump idle→verifying")
  (check-false (valid-transition? 'exploring 'executing) "Cannot jump exploring→executing")
  (check-false (valid-transition? 'plan-written 'verifying) "Cannot jump plan-written→verifying"))

(test-case "audit-valid-targets-from-idle"
  (define targets (valid-targets 'idle))
  (check-not-false (member 'exploring targets) "idle→exploring should be valid"))

(test-case "audit-valid-targets-from-executing"
  (define targets (valid-targets 'executing))
  (check-not-false (member 'verifying targets) "executing→verifying should be valid")
  (check-not-false (member 'idle targets) "executing→idle (cancel) should be valid"))

(test-case "audit-find-transition-path-direct"
  ;; Direct transition: idle → exploring
  (check-equal? (find-transition-path 'idle 'exploring) '(exploring)))

(test-case "audit-find-transition-path-multi-step"
  ;; Multi-step: idle → exploring → plan-written
  (define path (find-transition-path 'idle 'plan-written))
  (check-true (and (list? path) (> (length path) 1)) "Should find multi-step path")
  (check-equal? (last path) 'plan-written "Path should end at target"))

(test-case "audit-find-transition-path-same-state"
  ;; FINDING: Returns empty list '() when from==to, not #f
  (check-equal? (find-transition-path 'idle 'idle) '() "Same state should return empty path"))

(test-case "audit-find-transition-path-rework"
  ;; Rework path: verifying → executing
  (check-equal? (find-transition-path 'verifying 'executing) '(executing)))

;; ---------------------------------------------------------------------------
;; 2. State Machine Lifecycle
;; ---------------------------------------------------------------------------

(test-case "audit-sm-initial-state"
  (with-fresh-gsd-state (lambda () (check-equal? (gsm-current) 'idle))))

(test-case "audit-sm-transition-plan-flow"
  (with-fresh-gsd-state (lambda ()
                          (check-equal? (gsm-current) 'idle)
                          (define r1 (gsm-transition! 'exploring))
                          (check-true (ok? r1) "idle→exploring should succeed")
                          (check-equal? (gsm-current) 'exploring)
                          (define r2 (gsm-transition! 'plan-written))
                          (check-true (ok? r2) "exploring→plan-written should succeed")
                          (check-equal? (gsm-current) 'plan-written)
                          (define r3 (gsm-transition! 'executing))
                          (check-true (ok? r3))
                          (check-equal? (gsm-current) 'executing)
                          (define r4 (gsm-transition! 'verifying))
                          (check-true (ok? r4))
                          (check-equal? (gsm-current) 'verifying)
                          (define r5 (gsm-transition! 'idle))
                          (check-true (ok? r5))
                          (check-equal? (gsm-current) 'idle))))

(test-case "audit-sm-transition-invalid-blocked"
  (with-fresh-gsd-state (lambda ()
                          (check-equal? (gsm-current) 'idle)
                          (define result (gsm-transition! 'executing))
                          (check-true (err? result) "idle→executing should fail")
                          (check-equal? (gsm-current) 'idle "State should remain idle"))))

(test-case "audit-sm-reset"
  (with-fresh-gsd-state (lambda ()
                          (gsm-transition! 'exploring)
                          (check-equal? (gsm-current) 'exploring)
                          (gsm-reset!)
                          (check-equal? (gsm-current) 'idle))))

(test-case "audit-sm-transition-to-auto-routing"
  (with-fresh-gsd-state (lambda ()
                          ;; Auto-routing: idle → plan-written (multi-step)
                          (define result (gsm-transition-to! 'plan-written))
                          (check-true (ok? result) "Auto-routing should find path")
                          (check-equal? (gsm-current) 'plan-written))))

(test-case "audit-sm-transition-to-same-state"
  (with-fresh-gsd-state (lambda ()
                          (define result (gsm-transition-to! 'idle))
                          (check-true (ok? result) "Already at idle should succeed")
                          (check-equal? (gsm-current) 'idle))))

(test-case "audit-sm-history-recorded"
  (with-fresh-gsd-state (lambda ()
                          (gsm-transition! 'exploring)
                          (define h (gsm-history))
                          (check-true (> (length h) 0) "History should record transitions")
                          (check-equal? (caar h) 'idle "First entry: from idle")
                          (check-equal? (cadar h) 'exploring "First entry: to exploring"))))

;; ---------------------------------------------------------------------------
;; 3. Context-Aware API (Per-Session Isolation)
;; ---------------------------------------------------------------------------

(test-case "audit-ctx-isolation"
  (define ctx1 (make-gsd-context))
  (define ctx2 (make-gsd-context))
  ;; Both start at idle
  (check-equal? (gsm-ctx-current ctx1) 'idle)
  (check-equal? (gsm-ctx-current ctx2) 'idle)
  ;; Transition ctx1 to exploring
  (gsm-ctx-transition! ctx1 'exploring)
  (check-equal? (gsm-ctx-current ctx1) 'exploring)
  ;; ctx2 should remain at idle
  (check-equal? (gsm-ctx-current ctx2) 'idle "Context 2 should be unaffected"))

(test-case "audit-ctx-full-lifecycle"
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  (gsm-ctx-transition! ctx 'idle)
  (check-equal? (gsm-ctx-current ctx) 'idle))

(test-case "audit-ctx-reset"
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-reset! ctx)
  (check-equal? (gsm-ctx-current ctx) 'idle))

(test-case "audit-ctx-transition-to-auto"
  (define ctx (make-gsd-context))
  (gsm-ctx-transition-to! ctx 'executing)
  (check-equal? (gsm-ctx-current ctx) 'executing))

;; ---------------------------------------------------------------------------
;; 4. Wave State Tracking
;; ---------------------------------------------------------------------------

(test-case "audit-wave-tracking-defaults"
  (with-fresh-gsd-state (lambda ()
                          (check-equal? (gsm-total-waves) 0)
                          (check-equal? (gsm-current-wave) 0)
                          (check-equal? (set-count (gsm-completed-waves)) 0))))

(test-case "audit-wave-set-total"
  (with-fresh-gsd-state (lambda ()
                          (gsm-set-total-waves! 5)
                          (check-equal? (gsm-total-waves) 5))))

(test-case "audit-wave-mark-complete"
  (with-fresh-gsd-state (lambda ()
                          (gsm-set-total-waves! 5)
                          (gsm-mark-wave-complete! 0)
                          (gsm-mark-wave-complete! 2)
                          (check-true (gsm-wave-complete? 0) "Wave 0 should be complete")
                          (check-true (gsm-wave-complete? 2) "Wave 2 should be complete")
                          (check-false (gsm-wave-complete? 1) "Wave 1 should not be complete"))))

(test-case "audit-wave-next-pending"
  (with-fresh-gsd-state (lambda ()
                          (gsm-set-total-waves! 5)
                          (check-equal? (gsm-next-pending-wave) 0 "First pending should be 0")
                          (gsm-mark-wave-complete! 0)
                          (check-equal? (gsm-next-pending-wave) 1 "Next pending after 0 should be 1")
                          (gsm-mark-wave-complete! 1)
                          (gsm-mark-wave-complete! 2)
                          (gsm-mark-wave-complete! 3)
                          (gsm-mark-wave-complete! 4)
                          (check-false (gsm-next-pending-wave) "All complete → #f"))))

(test-case "audit-ctx-wave-tracking"
  (define ctx (make-gsd-context))
  (gsm-ctx-set-total-waves! ctx 3)
  (gsm-ctx-mark-wave-complete! ctx 1)
  (check-true (gsm-ctx-wave-complete? ctx 1))
  (check-false (gsm-ctx-wave-complete? ctx 0))
  (check-equal? (gsm-ctx-next-pending-wave ctx) 0))

(test-case "audit-compute-next-pending-wave-pure"
  ;; Pure function — no state needed
  (check-equal? (compute-next-pending-wave 5 (set)) 0)
  (check-equal? (compute-next-pending-wave 5 (set 0 1)) 2)
  (check-equal? (compute-next-pending-wave 0 (set)) #f)
  (check-equal? (compute-next-pending-wave 3 (set 0 1 2)) #f))

;; ---------------------------------------------------------------------------
;; 5. Policy Engine
;; ---------------------------------------------------------------------------

(test-case "audit-policy-allowed-by-default"
  (define d (gsd-decide-action (hasheq 'mode 'idle 'tool "read") 'tool-call))
  (check-true (policy-allowed? d) "Read tool in idle mode should be allowed"))

(test-case "audit-policy-blocked-tools-plan-written"
  (define d (gsd-decide-action (hasheq 'mode 'plan-written 'tool "edit") 'tool-call))
  (check-true (policy-blocked? d) "Edit in plan-written mode should be blocked")
  (check-true (string? (policy-reason d)) "Should have reason"))

(test-case "audit-policy-blocked-tools-executing"
  (define d (gsd-decide-action (hasheq 'mode 'executing 'tool "planning-write") 'tool-call))
  (check-true (policy-blocked? d) "Planning-write in executing mode should be blocked"))

(test-case "audit-policy-blocked-tools-verifying"
  (for ([tool '("edit" "write" "bash" "planning-write")])
    (define d (gsd-decide-action (hasheq 'mode 'verifying 'tool tool) 'tool-call))
    (check-true (policy-blocked? d) (format "~a in verifying mode should be blocked" tool))))

(test-case "audit-policy-write-guard-executing"
  ;; During execution, writes to .planning/ blocked
  (define d
    (gsd-decide-action (hasheq 'mode
                               'executing
                               'target-path
                               "project/.planning/PLAN.md"
                               'pinned-dir
                               "project/.planning")
                       'write-file))
  (check-true (policy-blocked? d) "Write to .planning during execution should be blocked"))

(test-case "audit-policy-write-guard-allowed"
  ;; Writes to source files during execution are allowed
  (define d
    (gsd-decide-action
     (hasheq 'mode 'executing 'target-path "project/src/main.rkt" 'pinned-dir "project/.planning")
     'write-file))
  (check-true (policy-allowed? d) "Write to source files during execution should be allowed"))

(test-case "audit-policy-edit-plan-blocked-in-executing"
  (define d (gsd-decide-action (hasheq 'mode 'executing) 'edit-plan))
  (check-true (policy-blocked? d) "Edit plan in executing mode should be blocked"))

(test-case "audit-policy-edit-plan-allowed-in-exploring"
  (define d (gsd-decide-action (hasheq 'mode 'exploring) 'edit-plan))
  (check-true (policy-allowed? d) "Edit plan in exploring mode should be allowed"))

(test-case "audit-policy-blocked-tools-for-mode"
  (check-equal? (blocked-tools-for 'plan-written) '("edit" "write" "bash"))
  (check-equal? (blocked-tools-for 'executing) '("planning-write"))
  (check-equal? (blocked-tools-for 'verifying) '("edit" "write" "bash" "planning-write"))
  (check-equal? (blocked-tools-for 'idle) '()))

;; ---------------------------------------------------------------------------
;; 6. Rework-Loop Protection
;; ---------------------------------------------------------------------------

(test-case "audit-rework-limit-default"
  (check-equal? (gsd-max-rework-iterations) 3))

(test-case "audit-rework-limit-blocked"
  (define ctx (make-gsd-context))
  ;; Manually set rework count to limit
  (set-box! (gsd-session-ctx-rework-count-box ctx) 3)
  (check-true (gsd-rework-limit-reached? ctx) "Should be blocked at limit"))

(test-case "audit-rework-limit-not-reached"
  (define ctx (make-gsd-context))
  (check-false (gsd-rework-limit-reached? ctx) "Fresh context should not be at limit"))

;; ---------------------------------------------------------------------------
;; 7. State Invariants
;; ---------------------------------------------------------------------------

(test-case "audit-invariants-valid-initial"
  (define state (make-initial-gsd-state))
  (define-values (ok? err) (check-state-invariants state))
  (check-true ok? "Initial state should satisfy invariants")
  (check-false err))

(test-case "audit-invariants-invalid-mode"
  (define state (gsd-runtime-state 'invalid-mode 0 0 (set) #f #f #f 500 '()))
  (define-values (ok? err) (check-state-invariants state))
  (check-false ok? "Invalid mode should fail invariant")
  (check-true (string? err)))

(test-case "audit-invariants-wave-overflow"
  ;; current-wave > total-waves is invalid
  (define state (gsd-runtime-state 'executing 3 5 (set) 'executor #f #f 500 '()))
  (define-values (ok? err) (check-state-invariants state))
  (check-false ok? "current-wave > total-waves should fail"))

(test-case "audit-invariants-executing-needs-executor"
  (define state (gsd-runtime-state 'executing 3 0 (set) #f #f #f 500 '()))
  (define-values (ok? err) (check-state-invariants state))
  (check-false ok? "Executing with waves but no executor should fail"))

;; ---------------------------------------------------------------------------
;; 8. Command Parser (Pure AST)
;; ---------------------------------------------------------------------------

(test-case "audit-cmd-parse-plan"
  (define cmd (parse-gsd-command "/plan" "/plan fix the bug"))
  (check-true (gsd-cmd-plan? cmd))
  (check-true (string? (gsd-cmd-plan-plan-text cmd))))

(test-case "audit-cmd-parse-go"
  (define cmd (parse-gsd-command "/go" "/go 3"))
  (check-true (gsd-cmd-go? cmd)))

(test-case "audit-cmd-parse-go-alias"
  (define cmd (parse-gsd-command "/i" "/i"))
  (check-true (gsd-cmd-go? cmd) "/i should be alias for /go"))

(test-case "audit-cmd-parse-skip"
  (define cmd (parse-gsd-command "/skip" "/skip 2"))
  (check-true (gsd-cmd-skip? cmd)))

(test-case "audit-cmd-parse-reset"
  (define cmd (parse-gsd-command "/reset" "/reset"))
  (check-true (gsd-cmd-reset? cmd)))

(test-case "audit-cmd-parse-wave-done"
  (define cmd (parse-gsd-command "/wave-done" "/wave-done 0"))
  (check-true (gsd-cmd-wave-done? cmd)))

(test-case "audit-cmd-parse-wave-done-alias"
  (define cmd (parse-gsd-command "/wd" "/wd 1"))
  (check-true (gsd-cmd-wave-done? cmd) "/wd should be alias for /wave-done"))

(test-case "audit-cmd-parse-done"
  (define cmd (parse-gsd-command "/done" "/done"))
  (check-true (gsd-cmd-done? cmd))
  (check-false (gsd-cmd-done-force? cmd)))

(test-case "audit-cmd-parse-done-force"
  (define cmd (parse-gsd-command "/done" "/done --force"))
  (check-true (gsd-cmd-done? cmd))
  (check-true (gsd-cmd-done-force? cmd)))

(test-case "audit-cmd-parse-replan"
  (define cmd (parse-gsd-command "/replan" "/replan"))
  (check-true (gsd-cmd-replan? cmd)))

(test-case "audit-cmd-parse-status"
  (define cmd (parse-gsd-command "/gsd" "/gsd"))
  (check-true (gsd-cmd-status? cmd)))

(test-case "audit-cmd-parse-unknown"
  (define cmd (parse-gsd-command "/unknown" "/unknown"))
  (check-false cmd "Unknown command should return #f"))

(test-case "audit-cmd-parse-state-artifact"
  (define cmd (parse-gsd-command "/state" "/state"))
  (check-true (gsd-cmd-artifact? cmd))
  (check-equal? (gsd-cmd-artifact-artifact-name cmd) "STATE"))

(test-case "audit-cmd-parse-handoff-artifact"
  (define cmd (parse-gsd-command "/handoff" "/handoff"))
  (check-true (gsd-cmd-artifact? cmd))
  (check-equal? (gsd-cmd-artifact-artifact-name cmd) "HANDOFF"))

;; ---------------------------------------------------------------------------
;; 9. Plan Types
;; ---------------------------------------------------------------------------

(test-case "audit-plan-create-wave"
  (define t (make-gsd-task "Task 1" '("file.rkt") "action" "verify" "done"))
  (check-true (gsd-task? t))
  (check-equal? (gsd-task-name t) "Task 1")
  (check-equal? (gsd-task-status t) 'pending)
  (define w
    (make-gsd-wave 0 "First Wave" "root cause" '("file.rkt") (list t) "verify cmd" '("criterion")))
  (check-true (gsd-wave? w))
  (check-equal? (gsd-wave-index w) 0)
  (check-equal? (gsd-wave-title w) "First Wave")
  (check-equal? (gsd-wave-status w) 'pending))

(test-case "audit-plan-validation-valid"
  (define t (make-gsd-task "Task 1" '("file.rkt") "action" "verify" "done"))
  (define w (make-gsd-wave 0 "First Wave" "root cause" '("file.rkt") (list t) "verify cmd" '("done")))
  (define p (gsd-plan (list w) #f '("constraint1") '("must-have1")))
  (define vr (validate-plan p))
  (check-true (validation-valid? vr)))

(test-case "audit-plan-validation-no-waves"
  (define p (gsd-plan '() #f '() '()))
  (define vr (validate-plan p))
  (check-false (validation-valid? vr))
  (check-true (> (length (validation-result-errors vr)) 0) "Should have errors"))

(test-case "audit-plan-validation-no-files-warning"
  (define w (make-gsd-wave 0 "Wave" "cause" '() '() "verify" '()))
  (define p (gsd-plan (list w) #f '() '()))
  (define vr (validate-plan p))
  ;; No files in ANY wave is an error
  (check-false (validation-valid? vr)))

(test-case "audit-plan-normalize"
  (define t (make-gsd-task "Task 1" '("file.rkt") "action" "verify" "done"))
  (define w1 (make-gsd-wave 0 "Wave A" "cause" '("a.rkt") (list t) "verify" '()))
  (define w2 (make-gsd-wave 1 "Wave B" "cause" '("b.rkt") (list t) "verify" '()))
  (define p (gsd-plan (list w1 w2) #f '() '()))
  (define norm (normalize-plan p))
  (check-true (gsd-normalized-plan? norm))
  (check-equal? (length (gsd-normalized-plan-waves norm)) 2))

(test-case "audit-plan-normalize-duplicate-titles"
  (define t (make-gsd-task "Task" '("f.rkt") "a" "v" "d"))
  (define w1 (make-gsd-wave 0 "Same" "c" '("a.rkt") (list t) "v" '()))
  (define w2 (make-gsd-wave 1 "Same" "c" '("b.rkt") (list t) "v" '()))
  (define p (gsd-plan (list w1 w2) #f '() '()))
  (define result (normalize-plan p))
  ;; Duplicate titles → returns error string
  (check-true (string? result) "Duplicate titles should return error string"))

(test-case "audit-plan-wave-set-status"
  (define w (make-gsd-wave 0 "Wave" "cause" '("f.rkt") '() "v" '()))
  (define w2 (gsd-wave-set-status w 'completed))
  (check-equal? (gsd-wave-status w2) 'completed)
  (check-equal? (gsd-wave-status w) 'pending "Original should be unchanged (immutable)"))

(test-case "audit-plan-pending-waves"
  (define t (make-gsd-task "T" '("f.rkt") "a" "v" "d"))
  (define w0 (make-gsd-wave 0 "W0" "c" '("a.rkt") (list t) "v" '()))
  (define w1 (gsd-wave-set-status (make-gsd-wave 1 "W1" "c" '("b.rkt") (list t) "v" '()) 'completed))
  (define w2 (make-gsd-wave 2 "W2" "c" '("c.rkt") (list t) "v" '()))
  (define p (gsd-plan (list w0 w1 w2) #f '() '()))
  (define pending (plan-pending-waves p))
  (check-equal? (length pending) 2 "Should have 2 pending waves")
  (define next (plan-next-pending-wave p))
  (check-true (gsd-wave? next))
  (check-equal? (gsd-wave-index next) 0))

(test-case "audit-status-conversion"
  (check-equal? (wave-status->string 'pending) "Inbox")
  (check-equal? (wave-status->string 'completed) "DONE")
  (check-equal? (wave-status->string 'skipped) "DEFERRED")
  (check-equal? (string->wave-status "Inbox") 'pending)
  (check-equal? (string->wave-status "DONE") 'completed)
  (check-equal? (string->wave-status "DEFERRED") 'skipped))

(test-case "audit-wave-slug"
  (define w (make-gsd-wave 0 "Fix the Authentication Bug" "c" '("f.rkt") '() "v" '()))
  (define slug (gsd-wave-slug w))
  (check-true (string? slug))
  (check-false (string-contains? slug " ")))

;; ---------------------------------------------------------------------------
;; 10. Wave Status Constants
;; ---------------------------------------------------------------------------

(test-case "audit-wave-status-constants"
  (check-equal? STATUS-INBOX "Inbox")
  (check-equal? STATUS-IN-PROGRESS "In-Progress")
  (check-equal? STATUS-DONE "DONE")
  (check-equal? STATUS-DEFERRED "DEFERRED")
  (check-equal? STATUS-FAILED "FAILED"))

(test-case "audit-wave-status-string-predicate"
  (check-true (wave-status-string? "Inbox"))
  (check-true (wave-status-string? "DONE"))
  (check-false (wave-status-string? "Random")))

(test-case "audit-wave-status-terminal"
  (check-true (terminal-status? "DONE"))
  (check-true (terminal-status? "DEFERRED"))
  (check-true (terminal-status? "done") "Case-insensitive")
  (check-false (terminal-status? "Inbox"))
  (check-false (terminal-status? "In-Progress")))

(test-case "audit-wave-status-normalize"
  (check-equal? (normalize-status! "done") STATUS-DONE)
  (check-equal? (normalize-status! "Done") STATUS-DONE)
  (check-equal? (normalize-status! "DONE") STATUS-DONE)
  (check-equal? (normalize-status! "inbox") STATUS-INBOX)
  (check-false (normalize-status! "bogus") "Unknown → #f"))

(test-case "audit-wave-status-active"
  (check-true (active-status? "Inbox"))
  (check-true (active-status? "In-Progress"))
  (check-false (active-status? "DONE")))

;; ---------------------------------------------------------------------------
;; 11. Wave Gate (Budget Enforcement)
;; ---------------------------------------------------------------------------

(test-case "audit-wave-gate-defaults"
  (check-equal? (gsd-wave-gate-interval) 5)
  (gsd-wave-gate-counter 0)
  (check-false (gsd-wave-gate-blocked?) "Fresh counter should not be blocked"))

(test-case "audit-wave-gate-blocked-at-limit"
  (parameterize ([gsd-wave-gate-interval 3])
    (gsd-wave-gate-counter 3)
    (check-true (gsd-wave-gate-blocked?))
    (gsm-clear-wave-gate!)
    (check-false (gsd-wave-gate-blocked?) "After clear should not be blocked")))

(test-case "audit-wave-gate-increment"
  (gsd-wave-gate-counter 0)
  (gsd-wave-gate-increment!)
  (check-equal? (gsd-wave-gate-counter) 1))

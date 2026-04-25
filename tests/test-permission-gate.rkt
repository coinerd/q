#lang racket

;;; tests/test-permission-gate.rkt — Tests for tools/permission-gate.rkt (G3.4)
;;;
;;; Core permission gate tests without scheduler integration:
;;;   - Auto-approved tools skip permission check
;;;   - Needs-approval tools call the callback
;;;   - Callback returning #f blocks execution
;;;   - Callback returning #t allows execution
;;;   - Default config classifies tools correctly
;;;   - make-default-permission-config has safe defaults
;;;   - Unknown tools require approval by default
;;;   - Custom overrides work correctly

(require rackunit
         rackunit/text-ui
         racket/set
         "../tools/permission-gate.rkt")

;; ============================================================
;; Suite
;; ============================================================

(define permission-gate-suite
  (test-suite "permission-gate tests (G3.4)"

    ;; ── Auto-approved tools skip permission check ──
    (test-case "auto-approved tools return #f from tool-needs-approval?"
      (define cfg (make-default-permission-config))
      (for ([name '("read" "glob" "ls" "find" "grep" "context-files")])
        (check-false (tool-needs-approval? cfg name) (format "~a should be auto-approved" name))))

    ;; ── Needs-approval tools call the callback ──
    (test-case "needs-approval tools return #t from tool-needs-approval?"
      (define cfg (make-default-permission-config))
      (for ([name '("edit" "write" "bash" "delete" "move" "spawn_subagent")])
        (check-true (tool-needs-approval? cfg name) (format "~a should need approval" name))))

    ;; ── Unknown tools require approval by default ──
    (test-case "unknown tools require approval (safe default)"
      (define cfg (make-default-permission-config))
      (check-true (tool-needs-approval? cfg "unknown_tool"))
      (check-true (tool-needs-approval? cfg "rm-rf"))
      (check-true (tool-needs-approval? cfg "execute")))

    ;; ── Callback returning #f blocks execution ──
    (test-case "callback returning #f denies approval"
      (define cfg (make-default-permission-config #:callback (lambda (tool-name args) #f)))
      (check-false (request-approval cfg "bash" (hasheq 'cmd "rm -rf /"))))

    ;; ── Callback returning #t allows execution ──
    (test-case "callback returning #t grants approval"
      (define cfg (make-default-permission-config #:callback (lambda (tool-name args) #t)))
      (check-true (request-approval cfg "bash" (hasheq 'cmd "ls"))))

    ;; ── Callback receives correct tool name and args ──
    (test-case "callback receives tool-name and args"
      (define received (box #f))
      (define cfg
        (make-default-permission-config #:callback (lambda (tool-name args)
                                                     (set-box! received (cons tool-name args))
                                                     #t)))
      (request-approval cfg "edit" (hasheq 'path "/tmp/test.rkt"))
      (define r (unbox received))
      (check-equal? (car r) "edit")
      (check-equal? (hash-ref (cdr r) 'path) "/tmp/test.rkt"))

    ;; ── Default config has safe defaults ──
    (test-case "make-default-permission-config auto-approved set"
      (define cfg (make-default-permission-config))
      (define auto (permission-config-auto-approved-tools cfg))
      (check-true (set? auto))
      (for ([name '("read" "glob" "ls" "find" "grep" "context-files")])
        (check-true (set-member? auto name) (format "~a should be in auto-approved set" name))))

    (test-case "make-default-permission-config needs-approval set"
      (define cfg (make-default-permission-config))
      (define needs (permission-config-needs-approval-tools cfg))
      (check-true (set? needs))
      (for ([name '("edit" "write" "bash" "delete" "move" "spawn_subagent")])
        (check-true (set-member? needs name) (format "~a should be in needs-approval set" name))))

    (test-case "make-default-permission-config default callback returns #t"
      (define cfg (make-default-permission-config))
      (check-true (request-approval cfg "bash" (hasheq))))

    ;; ── Custom overrides work ──
    (test-case "custom auto-approved set overrides defaults"
      (define cfg (make-default-permission-config #:auto-approved (set "bash" "rm")))
      (check-false (tool-needs-approval? cfg "bash"))
      (check-false (tool-needs-approval? cfg "rm"))
      ;; read is no longer in auto-approved — falls through to unknown → needs approval
      (check-true (tool-needs-approval? cfg "read")))

    (test-case "custom needs-approval set overrides defaults"
      (define cfg
        (make-default-permission-config #:auto-approved (set) #:needs-approval (set "read")))
      (check-true (tool-needs-approval? cfg "read"))
      ;; bash is unknown (not in either set) → needs approval
      (check-true (tool-needs-approval? cfg "bash")))))

;; ============================================================
;; Run
;; ============================================================

(run-tests permission-gate-suite)

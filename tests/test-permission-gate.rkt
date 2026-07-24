#lang racket

;; @speed fast  ;; @suite security

;; BOUNDARY: integration

;;; tests/test-permission-gate.rkt — Tests for tools/permission-gate.rkt
;;;
;;; v0.99.66 (Security Enforcement Hardening) updates:
;;;   - Default approval callback now DENIES by default (W2, finding #2).
;;;   - make-strict-permission-config is the hard-deny config (W1, finding #1).
;;;   - make-permissive-permission-config is the ONLY auto-approve path (W2).
;;;   - Tool-name lists come from tool-classification.rkt (single source).
;;;
;;; Coverage:
;;;   - Auto-approved tools skip permission check
;;;   - Needs-approval tools require approval
;;;   - Unknown tools require approval by default (fail closed)
;;;   - Default callback DENIES (deny-by-default, W2)
;;;   - Explicit callback returning #f denies
;;;   - Explicit callback returning #t grants
;;;   - Callback receives correct tool name and args
;;;   - make-strict-permission-config always denies dangerous tools
;;;   - make-permissive-permission-config auto-approves (only opt-in path)
;;;   - Custom overrides work correctly

(require rackunit
         rackunit/text-ui
         racket/set
         "../tools/permission-gate.rkt")

;; ============================================================
;; Suite
;; ============================================================

(define permission-gate-suite
  (test-suite "permission-gate tests (v0.99.66)"

    ;; ── Auto-approved tools skip permission check ──
    (test-case "auto-approved tools return #f from tool-needs-approval?"
      (define cfg (make-default-permission-config))
      ;; Real auto-approved tool names from tool-classification.rkt
      (for ([name '("read" "ls" "find" "grep" "date"
                    "session_recall" "skill-route"
                    "save-conclusion" "record_conclusion" "set-task-state"
                    "browser_observe" "browser_extract" "browser_screenshot"
                    "browser_scroll" "browser_close"
                    "list-memory" "search-memory")])
        (check-false (tool-needs-approval? cfg name)
                     (format "~a should be auto-approved" name))))

    ;; ── Needs-approval tools require approval ──
    (test-case "needs-approval tools return #t from tool-needs-approval?"
      (define cfg (make-default-permission-config))
      ;; Real needs-approval tool names from tool-classification.rkt
      (for ([name '("edit" "write" "bash" "delete-lines"
                    "spawn-subagent" "spawn-subagents" "firecrawl"
                    "browser_open" "browser_click" "browser_type" "browser_press"
                    "delete-memory" "clear-memory")])
        (check-true (tool-needs-approval? cfg name)
                    (format "~a should need approval" name))))

    ;; ── Unknown tools require approval by default (fail closed) ──
    (test-case "unknown tools require approval (safe default / fail closed)"
      (define cfg (make-default-permission-config))
      (check-true (tool-needs-approval? cfg "unknown_tool"))
      (check-true (tool-needs-approval? cfg "rm-rf"))
      (check-true (tool-needs-approval? cfg "execute"))
      ;; Tools removed in v0.99.66 classification (must fail closed now)
      (check-true (tool-needs-approval? cfg "glob"))
      (check-true (tool-needs-approval? cfg "context-files")))

    ;; ── W2: Default callback DENIES (deny-by-default) ──
    (test-case "default callback DENIES dangerous tools (W2 deny-by-default)"
      (define cfg (make-default-permission-config))
      ;; A dangerous tool reaching the approval branch is blocked by default.
      (check-false (request-approval cfg "bash" (hasheq 'cmd "rm -rf /")))
      (check-false (request-approval cfg "edit" (hasheq 'path "/etc/passwd")))
      ;; Even a benign-looking dangerous call is denied without explicit callback.
      (check-false (request-approval cfg "bash" (hasheq 'cmd "ls"))))

    ;; ── Callback returning #f denies approval ──
    (test-case "explicit callback returning #f denies approval"
      (define cfg (make-default-permission-config #:callback (lambda (tool-name args) #f)))
      (check-false (request-approval cfg "bash" (hasheq 'cmd "rm -rf /"))))

    ;; ── Callback returning #t grants approval ──
    (test-case "explicit callback returning #t grants approval"
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

    ;; ── Default config populated from classification source ──
    (test-case "make-default-permission-config auto-approved set (from classification)"
      (define cfg (make-default-permission-config))
      (define auto (permission-config-auto-approved-tools cfg))
      (check-true (set? auto))
      (for ([name '("read" "ls" "find" "grep" "date" "session_recall" "skill-route")])
        (check-true (set-member? auto name)
                    (format "~a should be in auto-approved set" name))))

    (test-case "make-default-permission-config needs-approval set (from classification)"
      (define cfg (make-default-permission-config))
      (define needs (permission-config-needs-approval-tools cfg))
      (check-true (set? needs))
      (for ([name '("edit" "write" "bash" "delete-lines"
                    "spawn-subagent" "spawn-subagents" "firecrawl")])
        (check-true (set-member? needs name)
                    (format "~a should be in needs-approval set" name))))

    ;; ── W1: make-strict-permission-config always denies dangerous tools ──
    (test-case "make-strict-permission-config denies all dangerous tools"
      (define cfg (make-strict-permission-config))
      ;; Dangerous tools are classified correctly (still need approval)
      (check-true (tool-needs-approval? cfg "bash"))
      (check-true (tool-needs-approval? cfg "edit"))
      (check-true (tool-needs-approval? cfg "write"))
      ;; But the callback denies them (hard-deny)
      (check-false (request-approval cfg "bash" (hasheq 'cmd "ls")))
      (check-false (request-approval cfg "edit" (hasheq 'path "/tmp/x")))
      ;; Safe tools still bypass the gate entirely
      (check-false (tool-needs-approval? cfg "read"))
      (check-false (tool-needs-approval? cfg "ls")))

    ;; ── W2: make-permissive-permission-config is the only auto-approve path ──
    (test-case "make-permissive-permission-config auto-approves dangerous tools"
      (define cfg (make-permissive-permission-config))
      ;; Dangerous tools still classified as needing approval...
      (check-true (tool-needs-approval? cfg "bash"))
      ;; ...but the callback grants approval (only opt-in path)
      (check-true (request-approval cfg "bash" (hasheq 'cmd "rm -rf /")))
      (check-true (request-approval cfg "edit" (hasheq 'path "/etc/passwd")))
      ;; Policy mode is 'permissive
      (check-equal? (permission-config-policy-mode cfg) 'permissive))

    (test-case "make-strict-permission-config uses strict policy mode"
      (define cfg (make-strict-permission-config))
      (check-equal? (permission-config-policy-mode cfg) 'strict))

    ;; ── Permissive mode: unknown tools do NOT require approval ──
    (test-case "permissive mode auto-approves unknown tools"
      (define cfg (make-permissive-permission-config))
      ;; Unknown tools in permissive mode skip approval
      (check-false (tool-needs-approval? cfg "unknown_tool"))
      (check-false (tool-needs-approval? cfg "rm-rf")))

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
      ;; bash is unknown (not in either set) → needs approval (strict default)
      (check-true (tool-needs-approval? cfg "bash")))))

;; ============================================================
;; Run
;; ============================================================

(run-tests permission-gate-suite)

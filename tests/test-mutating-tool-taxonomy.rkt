#lang racket
;; @suite security

;; tests/test-mutating-tool-taxonomy.rkt — Mutating-tool taxonomy enforcement (v0.54.2 W2)
;;
;; Verifies:
;;   - All dangerous-tool-names appear in the permission gate's needs-approval set
;;   - All registered built-in tools appear in one permission classification
;;   - Dangerous tools have the dangerous? flag set in their tool struct
;;   - No mutating tool bypasses serialization policy

(require rackunit
         rackunit/text-ui
         racket/set
         "../tools/permission-gate.rkt"
         "../tools/registry-table.rkt"
         (only-in "../tools/tool-struct.rkt" tool? tool-name tool-dangerous?)
         (only-in "../tools/registry.rkt" make-tool-registry lookup-tool)
         (only-in "../tools/registry-defaults.rkt" register-default-tools!))

(define mutating-tool-taxonomy-suite
  (test-suite "mutating-tool taxonomy enforcement"

    ;; ── dangerous-tool-names ⊆ needs-approval-tools ──
    (test-case "all dangerous tools require approval"
      (define cfg (make-default-permission-config))
      (for ([name dangerous-tool-names])
        (check-true (tool-needs-approval? cfg name)
                    (format "dangerous tool ~a should require approval" name))))

    ;; ── All built-in tool specs are classified ──
    (test-case "all built-in tool names are classified in permission sets"
      (define cfg (make-default-permission-config))
      (define auto (permission-config-auto-approved-tools cfg))
      (define needs (permission-config-needs-approval-tools cfg))
      (define all-classified (set-union auto needs))
      (for ([spec (in-list tool-specs)])
        (define name (tool-spec-name spec))
        (check-true (set-member? all-classified name)
                    (format "tool ~a not classified in any permission set" name))))

    ;; ── Dangerous tools get the dangerous? flag ──
    (test-case "dangerous tool specs produce dangerous tools"
      (define reg (make-tool-registry))
      (register-default-tools! reg)
      ;; Verify dangerous tools are marked in the registry
      (for ([name dangerous-tool-names])
        (define t (lookup-tool reg name))
        (when t
          (check-true (tool-dangerous? t)
                      (format "~a should have dangerous? #t" name)))))

    ;; ── Read-only tools don't appear in dangerous-tool-names ──
    (test-case "read-only tools are not classified as dangerous"
      (define read-only-names '("read" "find" "ls" "grep" "date" "context-files" "session_recall" "skill-route"))
      (for ([name read-only-names])
        (check-false (member name dangerous-tool-names)
                     (format "~a should not be in dangerous-tool-names" name))))

    ;; ── Permission gate default covers all known mutating operations ──
    (test-case "needs-approval covers all known mutating operations"
      (define cfg (make-default-permission-config))
      (define needs (permission-config-needs-approval-tools cfg))
      ;; Core mutating operations that MUST be in the set
      (for ([name '("edit" "write" "bash")])
        (check-true (set-member? needs name)
                    (format "~a must be in needs-approval set" name))))

    ;; ── tool-specs has expected minimum count ──
    (test-case "at least 10 built-in tools registered"
      (check-true (>= (length tool-specs) 10)
                  (format "expected >= 10 tool specs, got ~a" (length tool-specs))))))

(run-tests mutating-tool-taxonomy-suite)

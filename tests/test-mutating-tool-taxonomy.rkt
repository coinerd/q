#lang racket

;; @speed fast
;; @suite security

;; tests/test-mutating-tool-taxonomy.rkt — Mutating-tool taxonomy enforcement (v0.54.2 W2)
;;
;; Verifies:
;;   - All registered built-in tools appear in one permission classification
;;   - Registry dangerous? metadata exactly follows authoritative classification
;;   - No approval-requiring tool bypasses dangerous execution policy

(require rackunit
         rackunit/text-ui
         racket/set
         "../tools/permission-gate.rkt"
         "../tools/registry-table.rkt"
         "../tools/tool-classification.rkt"
         (only-in "../tools/tool-struct.rkt" tool? tool-name tool-dangerous?)
         (only-in "../tools/registry.rkt" make-tool-registry lookup-tool)
         (only-in "../tools/registry-defaults.rkt" register-default-tools!))

(define mutating-tool-taxonomy-suite
  (test-suite "mutating-tool taxonomy enforcement"

    ;; ── All built-in tool specs are classified ──
    (test-case "all built-in tool names are classified in permission sets"
      (define all-classified (list->set (all-classified-tool-names)))
      (for ([spec (in-list tool-specs)])
        (define name (tool-spec-name spec))
        (check-true (set-member? all-classified name)
                    (format "tool ~a not classified in any permission set" name))))

    ;; ── Registry metadata is wholly derived from classification ──
    (test-case "registered tool dangerous metadata matches approval classification"
      (define reg (make-tool-registry))
      (register-default-tools! reg)
      (for ([spec (in-list tool-specs)])
        (define name (tool-spec-name spec))
        (define t (lookup-tool reg name))
        (check-not-false t (format "~a should be registered" name))
        (when t
          (check-equal? (tool-dangerous? t)
                        (not (tool-name-auto-approved? name))
                        (format "~a dangerous metadata must follow classification" name)))))

    (test-case "known read-only tools are not classified as dangerous"
      (for ([name '("read" "find" "ls" "grep" "date" "session_recall")])
        (check-false (tool-name-needs-approval? name) (format "~a should be auto-approved" name))))

    (test-case "skill-route is dangerous because workflow action spawns work"
      (check-true (tool-name-needs-approval? "skill-route")))

    ;; ── Permission gate default covers all known mutating operations ──
    (test-case "needs-approval covers all known mutating operations"
      (define cfg (make-default-permission-config))
      (define needs (permission-config-needs-approval-tools cfg))
      ;; Core mutating operations that MUST be in the set
      (for ([name '("edit" "write" "bash")])
        (check-true (set-member? needs name) (format "~a must be in needs-approval set" name))))

    ;; ── tool-specs has expected minimum count ──
    (test-case "at least 10 built-in tools registered"
      (check-true (>= (length tool-specs) 10)
                  (format "expected >= 10 tool specs, got ~a" (length tool-specs))))))

(run-tests mutating-tool-taxonomy-suite)

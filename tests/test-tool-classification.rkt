#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary unit
;;; tests/test-tool-classification.rkt
;;;
;;; Tests for tools/tool-classification.rkt — the single authoritative
;;; source for tool classification (v0.99.66 W0, finding #6).
;;;
;;; Verifies:
;;;   1. Parity between classification table and registry-table tool-specs.
;;;   2. Unknown tool names fail closed → 'needs-approval.
;;;   3. Query functions behave consistently.
;;;   4. Derived sets match classification predicates.

(require racket/set
         rackunit
         rackunit/text-ui
         "../tools/tool.rkt"
         "../tools/registry-table.rkt"
         "../tools/tool-classification.rkt")

(define-test-suite
 classification-suite
 ;; ── Completeness parity ────────────────────────────────────────
 ;; Every tool registered via tool-specs MUST be classified. This
 ;; prevents silent regressions where a new tool is added to the
 ;; registry but not classified (which would fail-closed anyway, but
 ;; we want explicit classification for auditability).
 (test-case "classification parity: every registered tool is classified"
   (define reg (make-tool-registry))
   (register-tools-from-specs! reg tool-specs)
   (define registered-names (map tool-name (list-tools reg)))
   (define classified-set (list->set (all-classified-tool-names)))
   (for ([name (in-list registered-names)])
     (check-true (set-member? classified-set name)
                 (format "tool ~a is registered but NOT in classification table" name)))
   ;; Also check no extra entries in classification table
   (define registered-set (list->set registered-names))
   (for ([name (in-list (all-classified-tool-names))])
     (check-true (set-member? registered-set name)
                 (format "tool ~a is classified but NOT in registry" name))))
 ;; ── Unknown tool names fail closed ─────────────────────────────
 (test-case "unknown tool: classify → needs-approval (fail closed)"
   (check-eq? (classify-tool-by-name "nonexistent-tool-xyz") 'needs-approval))
 (test-case "unknown tool: needs-approval? → #t (fail closed)"
   (check-true (tool-name-needs-approval? "nonexistent-tool-xyz")))
 (test-case "unknown tool: auto-approved? → #f"
   (check-false (tool-name-auto-approved? "nonexistent-tool-xyz")))
 (test-case "empty string: needs-approval? → #t (fail closed)"
   (check-true (tool-name-needs-approval? "")))
 ;; ── Known auto-approved tools ─────────────────────────────────
 (test-case "read-only tools are auto-approved"
   (for ([name '("read" "ls" "find" "grep" "date")])
     (check-eq? (classify-tool-by-name name)
                'auto-approved
                (format "expected ~a to be auto-approved" name))
     (check-true (tool-name-auto-approved? name))
     (check-false (tool-name-needs-approval? name))))
 ;; ── Known needs-approval tools ────────────────────────────────
 (test-case "side-effecting tools need generic approval"
   (for ([name '("write" "edit"
                         "bash"
                         "delete-lines"
                         "firecrawl"
                         "skill-route"
                         "browser_open"
                         "browser_click"
                         "browser_type"
                         "delete-memory"
                         "clear-memory")])
     (check-eq? (classify-tool-by-name name)
                'needs-approval
                (format "expected ~a to need approval" name))
     (check-true (tool-name-needs-approval? name))
     (check-false (tool-name-auto-approved? name))))
 (test-case "spawn tools own their approval instead of using the generic gate"
   (for ([name '("spawn-subagent" "spawn-subagents")])
     (check-eq? (classify-tool-by-name name) 'tool-owned-approval)
     (check-true (tool-name-tool-owned-approval? name))
     ;; They remain dangerous for registry metadata and sandbox routing.
     (check-true (tool-name-needs-approval? name))))
 ;; ── Derived sets consistency ──────────────────────────────────
 (test-case "auto-approved-tool-names set matches predicate"
   (for ([name (in-set auto-approved-tool-names)])
     (check-true (tool-name-auto-approved? name))))
 (test-case "needs-approval-tool-names set matches predicate"
   (for ([name (in-set needs-approval-tool-names)])
     (check-true (tool-name-needs-approval? name))))
 (test-case "registered dangerous metadata is derived from authoritative classification"
   (define reg (make-tool-registry))
   (register-tools-from-specs! reg tool-specs)
   (for ([t (in-list (list-tools reg))])
     (check-equal? (tool-dangerous? t)
                   (tool-name-needs-approval? (tool-name t))
                   (format "registry metadata drift for ~a" (tool-name t)))))
 (test-case "auto-approved and needs-approval sets are disjoint"
   (check-equal? (set-intersect auto-approved-tool-names needs-approval-tool-names) (set)))
 (test-case "union of all three sets equals all classified names"
   (define all-classified (list->set (all-classified-tool-names)))
   (define union
     (set-union auto-approved-tool-names needs-approval-tool-names tool-owned-approval-tool-names))
   (check-equal? all-classified union)))

(run-tests classification-suite)

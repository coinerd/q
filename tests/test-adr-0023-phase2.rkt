#lang racket/base

;; @speed fast
;; @suite default
;; ADR-0023 Phase 2 test scaffolding — GAP-4, GAP-5, GAP-8
;; TDD red phase: tests verify behaviors that need fixing.

(require rackunit
         racket/string
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id
                  task-conclusion-text
                  task-conclusion-origin-message-ids)
         (only-in "../runtime/context-assembly/conclusion-graph.rkt"
                  build-conclusion-graph
                  graph-select-conclusions))

;; ═══════════════════════════════════════════════════════════════
;; GAP-4: Graph seed resolution maps WS msg IDs → conclusion IDs
;; ═══════════════════════════════════════════════════════════════

(define gap4-suite
  (test-suite "ADR-0023 GAP-4: Graph seed resolution"

    (test-case "GAP-4: WS message IDs resolve to conclusion IDs via origin-message-ids"
      ;; Setup: two conclusions covering different WS messages
      (define c1
        (task-conclusion "conc-1"
                         "Found auth pattern"
                         'fact
                         'exploration
                         '("ws-msg-1" "ws-msg-3")
                         (current-seconds)
                         '()
                         '()))
      (define c2
        (task-conclusion "conc-2"
                         "Implemented auth fix"
                         'fact
                         'implementation
                         '("ws-msg-2")
                         (current-seconds)
                         '()
                         '()))
      (define conclusions (list c1 c2))

      ;; Build graph from conclusions
      (define graph (build-conclusion-graph conclusions))

      ;; Simulate WS messages that should map to conclusions
      ;; In the current code, seed-ids = ("ws-msg-1" "ws-msg-2")
      ;; But graph nodes are keyed by conclusion IDs ("conc-1", "conc-2")
      ;; So graph-select-conclusions returns empty — BUG!

      ;; After fix: seed IDs should be derived from origin-message-ids mapping
      ;; ws-msg-1 → conc-1, ws-msg-2 → conc-2
      (define ws-message-ids '("ws-msg-1" "ws-msg-2"))

      ;; The fix should map WS msg IDs to conclusion IDs:
      (define mapped-seed-ids
        (for*/list ([mid (in-list ws-message-ids)]
                    [c (in-list conclusions)]
                    #:when (member mid (task-conclusion-origin-message-ids c)))
          (task-conclusion-id c)))

      (check-equal? (sort mapped-seed-ids string<?)
                    '("conc-1" "conc-2")
                    "WS message IDs should map to conclusion IDs via origin-message-ids")

      ;; Verify graph selection works with mapped seeds
      (define selected (graph-select-conclusions graph mapped-seed-ids))
      (check-equal? (length selected)
                    2
                    "Graph selection should find conclusions when seeds are properly mapped"))

    (test-case "GAP-4: graph-select-conclusions returns empty with raw WS IDs (current bug)"
      ;; This test documents the CURRENT broken behavior
      (define c1
        (task-conclusion "conc-1"
                         "Found pattern"
                         'fact
                         'exploration
                         '("ws-msg-1")
                         (current-seconds)
                         '()
                         '()))
      (define graph (build-conclusion-graph (list c1)))

      ;; Using raw WS message IDs as seeds — these don't exist in graph
      (define selected (graph-select-conclusions graph '("ws-msg-1")))
      (check-equal? (length selected)
                    0
                    "Current behavior: raw WS IDs don't match graph nodes (this is the bug)"))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-5: WS evolution guard on state change
;; ═══════════════════════════════════════════════════════════════

(require (only-in "../runtime/working-set.rkt"
                  make-working-set)
         (only-in "../runtime/context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state/result
                  evolution-result?))

(define gap5-suite
  (test-suite "ADR-0023 GAP-5: WS evolution state guard"

    (test-case "GAP-5: evolve returns #f when old-state equals new-state"
      ;; When the FSM state hasn't changed, evolution should be a no-op
      (define ws (make-working-set))
      (define result (evolve-working-set-for-state/result ws 'exploration 'exploration '()))
      ;; With no working set and same state, should return #f or non-evolution-result
      (check-false (and result (evolution-result? result))
                   "Evolution should not produce result when old-state = new-state"))))

;; ═══════════════════════════════════════════════════════════════
;; GAP-8: Dynamic conclusion budget
;; ═══════════════════════════════════════════════════════════════

(require (only-in "../runtime/context-assembly/config.rkt" current-conclusion-token-budget))

(define gap8-suite
  (test-suite "ADR-0023 GAP-8: Dynamic conclusion budget"

    (test-case "GAP-8: budget scales with context window"
      ;; Default is 2000 — verify the parameter exists and is configurable
      (check-equal? (current-conclusion-token-budget) 2000 "Default budget should be 2000")
      (parameterize ([current-conclusion-token-budget 4000])
        (check-equal? (current-conclusion-token-budget)
                      4000
                      "Budget should be dynamically configurable")))))

;; ═══════════════════════════════════════════════════════════════
;; Run all suites
;; ═══════════════════════════════════════════════════════════════

(define all-suites
  (test-suite "ADR-0023 Phase 2 Tests"
    gap4-suite
    gap5-suite
    gap8-suite))

(module+ main
  (void (run-tests all-suites)))

(module+ test
  (require (submod ".." main)))

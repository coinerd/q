#lang racket/base

;; runtime/context-assembly/state-relevance.rkt — static mapping of task-state → context relevance
;; v0.75.3 W0: State-aware context assembly support
;;
;; Maps each task state to a set of context-category → relevance-level bindings.
;; Relevance levels: 'full (include unchanged), 'summary (compress), 'filtered (subset), 'excluded (omit).

(require (only-in "task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging
                  task-state?)
         (only-in "../../util/fsm.rkt" fsm-state-name))

(provide state-relevance-table
         context-level-for-state
         relevance-levels
         context-categories)

;; Context categories that can be filtered by state.
(define context-categories
  '(system-prompt conclusions working-set recent-messages tool-results plan-notes))

(define relevance-levels '(full summary filtered excluded))

;; ── State × Category relevance matrix ──
;;
;; Rows = task states. Columns = context categories.
;; 'full = include unchanged
;; 'summary = include but compress/summarize
;; 'filtered = include subset (e.g. only high-relevance)
;; 'excluded = omit from context

(define state-relevance-table
  ;; idle: default state — moderate relevance, mostly summary
  (hasheq 'idle
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'summary
                  'working-set
                  'excluded
                  'recent-messages
                  'summary
                  'tool-results
                  'filtered
                  'plan-notes
                  'filtered)
          ;; exploration: reading/gathering — full working-set retention
          'exploration
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'full
                  'working-set
                  'full
                  'recent-messages
                  'full
                  'tool-results
                  'full
                  'plan-notes
                  'filtered)
          ;; planning: plan-focused — conclusions + plan, skip working-set
          'planning
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'full
                  'working-set
                  'excluded
                  'recent-messages
                  'summary
                  'tool-results
                  'filtered
                  'plan-notes
                  'full)
          ;; implementation: focused — conclusions replace working-set
          'implementation
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'full
                  'working-set
                  'excluded
                  'recent-messages
                  'summary
                  'tool-results
                  'filtered
                  'plan-notes
                  'summary)
          ;; verification: test-focused — conclusions + tool-results
          'verification
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'full
                  'working-set
                  'filtered
                  'recent-messages
                  'summary
                  'tool-results
                  'full
                  'plan-notes
                  'summary)
          ;; debugging: error-focused — filtered working-set + full tool-results
          ;; v0.76.7 W5: Changed from 'full to 'filtered per decision A7
          'debugging
          (hasheq 'system-prompt
                  'full
                  'conclusions
                  'full
                  'working-set
                  'filtered
                  'recent-messages
                  'full
                  'tool-results
                  'full
                  'plan-notes
                  'summary)))

;; context-level-for-state : task-state? symbol? → (or/c 'full 'summary 'filtered 'excluded)
;;
;; Returns the relevance level for a given context category under the current task state.
;; Defaults to 'full if state or category is unrecognized.
(define (context-level-for-state state category)
  (define state-name
    (if (task-state? state)
        (fsm-state-name state)
        state))
  (define state-map (hash-ref state-relevance-table state-name #f))
  (if state-map
      (hash-ref state-map category 'full)
      'full))

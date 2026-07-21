#lang racket/base

;; runtime/context-assembly/state-aware-builder.rkt — State-aware context assembly
;; v0.76.0 W2: Extracted from serialization.rkt
;;
;; v0.95.14: Observe-only memory telemetry is wired behind session-config gates;
;; prompt injection remains explicit in memory-builder.rkt.
;;
;; W6 v0.99.35: Pure helper functions (state coercion, text extraction,
;; rollback trigger computation, conclusion-first replacement, state guidance)
;; extracted to state-aware-helpers.rkt. This module focuses on the stateful
;; orchestrator: memory observation/injection, trace callbacks, rollback action
;; execution, and preamble generation with parameter side-effects.

(require racket/list
         racket/runtime-path
         racket/string
         (only-in "../../util/content/content-parts.rkt" make-text-part)
         (only-in "../../util/message/message.rkt"
                  message
                  message?
                  message-id
                  message-content
                  message-role
                  message-meta-safe
                  make-message)
         (only-in "task-conclusion.rkt"
                  task-conclusion?
                  task-conclusion-text
                  task-conclusion-origin-message-ids
                  task-conclusion-id
                  task-conclusion-fsm-state-origin)
         (only-in "state-relevance.rkt" context-level-for-state)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "state-aware-helpers.rkt"
                  coerce-task-state
                  extract-recent-text
                  check-rollback-triggers
                  check-rollback-triggers-with-actions
                  ws-entry->conclusion-or-self
                  state-guidance-table)
         "context-floor.rkt"
         (only-in "conclusion-graph.rkt"
                  build-conclusion-graph
                  graph-select-conclusions
                  graph-detect-cycles)
         (only-in "conclusion-ranker.rkt" rank-and-budget)
         (only-in "rollback-actions.rkt"
                  maybe-execute-action
                  current-force-distill-fn
                  current-expand-context-fn
                  current-revert-state-fn
                  current-rollback-action-execution?
                  current-rollback-action-log
                  rollback-action-reason)
         (only-in "auto-distillation.rkt" current-auto-distillation-enabled?)
         (only-in "../../runtime/session/session-config.rkt"
                  context-assembly-options?
                  context-assembly-options-graph-conclusion-selection?
                  context-assembly-options-conclusion-token-budget)
         (only-in "config.rkt"
                  current-task-state-aware-assembly?
                  current-graph-conclusion-selection?
                  current-conclusion-token-budget
                  current-ws-evolution-enabled?)
         (only-in "memory-builder.rkt"
                  observe-memory-for-context
                  memory-telemetry->jsexpr
                  current-memory-injection-budget
                  inject-memory-for-context)
         (only-in "blackboard-context.rkt" build-blackboard-context-snippet))

(provide current-task-state-aware-assembly?
         build-tiered-context/state-aware
         build-state-awareness-preamble
         check-rollback-triggers
         ws-entry->conclusion-or-self
         current-graph-conclusion-selection?
         current-conclusion-token-budget
         current-ws-evolution-enabled?
         check-rollback-triggers-with-actions
         extract-recent-text
         current-reflection-event
         current-blackboard-injection-enabled)

;; v0.96.13 W3: Latest reflection event (set by step-interpreter, checked in preamble)
(define current-reflection-event (make-parameter #f))

;; v0.99.7 W5: Blackboard context injection into system prompt preamble
(define current-blackboard-injection-enabled (make-parameter #f))

;; build-tiered-context/state-aware :
;;   Same signature as build-tiered-context, plus optional task-state and conclusions.
;;   When a task-state is provided, uses state-relevance-table to decide which
;;   context categories to include/summarize/filter/exclude.
(define (build-tiered-context/state-aware messages
                                          #:tier-b-count [tier-b-count #f]
                                          #:tier-c-count [tier-c-count 4]
                                          #:working-set-messages [ws-messages '()]
                                          #:task-state [task-state #f]
                                          #:conclusions [conclusions '()]
                                          #:trace [trace-cb #f]
                                          #:recent-tool-calls [recent-tool-calls '()]
                                          #:ca-options [ca-options #f]
                                          #:session-config [session-config #f])
  ;; Accept both fsm-state structs and bare symbols
  (define state-name (coerce-task-state task-state))
  (define ws-level (and state-name (context-level-for-state state-name 'working-set)))
  (define conclusions-level (and state-name (context-level-for-state state-name 'conclusions)))

  ;; Adjust working-set based on state relevance
  ;; v0.76.4: conclusion-first replacement when filtered/excluded
  (define effective-ws
    (cond
      [(eq? ws-level 'excluded)
       ;; v0.76.4: Instead of dropping all, keep entries that have matching conclusions
       ;; (as compact replacement). Unmatched entries are still dropped (backward compat).
       (for/list ([m (in-list ws-messages)]
                  #:when (for/or ([c (in-list conclusions)]
                                  #:when (task-conclusion? c))
                           (member (message-id m) (task-conclusion-origin-message-ids c))))
         (ws-entry->conclusion-or-self m conclusions))]
      [(eq? ws-level 'filtered)
       (define filtered (take ws-messages (min 3 (length ws-messages))))
       (map (λ (m) (ws-entry->conclusion-or-self m conclusions)) filtered)]
      [else ws-messages]))

  ;; v0.99.54 W2 R-8: Use ca-options when provided, fall back to global parameter
  (define graph-selection-enabled?
    (if ca-options
        (context-assembly-options-graph-conclusion-selection? ca-options)
        (current-graph-conclusion-selection?)))
  ;; v0.77.3 W3.3 + v0.77.9 T1.1: Graph-based conclusion filtering when flag is enabled
  (define effective-conclusions
    (cond
      [(not graph-selection-enabled?) conclusions]
      [(null? conclusions) '()]
      [else
       ;; GAP-4: Map WS message IDs → conclusion IDs via origin-message-ids
       (define seed-ids
         (let ([mapped (for*/list ([m (in-list ws-messages)]
                                   [c (in-list conclusions)]
                                   #:when (task-conclusion? c)
                                   #:when (member (message-id m)
                                                  (task-conclusion-origin-message-ids c)))
                         (task-conclusion-id c))])
           ;; Use mapped IDs if any; fall back to raw WS IDs for backward compat
           (if (pair? mapped)
               mapped
               (map (lambda (m) (message-id m)) ws-messages))))
       (define current-states
         (if state-name
             (list state-name)
             '()))
       ;; Build the dependency graph and use seed-based selection
       (define graph (build-conclusion-graph conclusions))
       (define cycles (graph-detect-cycles graph))
       (cond
         [(pair? cycles)
          ;; GAP-D v0.97.9: Cycles detected — downgrade to info, use semantic ranking
          (log-info "context-assembly: conclusion graph has ~a cycle(s), using rank-and-budget"
                    (length cycles))
          (rank-and-budget conclusions
                           #:current-state (and (pair? current-states) (car current-states))
                           #:max-conclusion-tokens (or (current-conclusion-token-budget) 2000))]
         [else
          ;; Seed-based subgraph selection (v0.77.10 M4: uses convenience wrapper)
          (define selected-conclusions (graph-select-conclusions graph seed-ids))
          (if (null? selected-conclusions)
              ;; GAP-D v0.97.9: No seeds matched — use semantic ranking instead of recency
              (rank-and-budget conclusions
                               #:current-state (and (pair? current-states) (car current-states))
                               #:max-conclusion-tokens (or (current-conclusion-token-budget) 2000))
              selected-conclusions)])]))

  ;; GAP-4: Extract active tags from working-set messages for tag-based ranking
  (define active-tags
    (let* ([path-symbols (for*/list ([m (in-list effective-ws)]
                                     #:when (hash? (message-meta-safe m))
                                     [k (in-list '(path file filepath))]
                                     [v (in-value (hash-ref (message-meta-safe m) k #f))]
                                     #:when (string? v))
                           ;; Extract filename without extension and directory path components as tags
                           (define basename
                             (let ([parts (string-split v "/")])
                               (if (pair? parts)
                                   (last parts)
                                   v)))
                           (define name-no-ext
                             (path->string (path-replace-suffix (string->path basename) "")))
                           (list basename name-no-ext v))]
           [flat-tags (filter-map (lambda (p) (and (string? p) (> (string-length p) 0) p))
                                  (apply append path-symbols))]
           [deduped (remove-duplicates flat-tags)])
      (let ([syms (map string->symbol deduped)])
        (if (> (length syms) 20)
            (take syms 20)
            syms))))

  ;; v0.77.9 T1.2: Apply rank-and-budget when budget is configured (>0)
  (define budgeted-conclusions
    (let ([budget (current-conclusion-token-budget)])
      (if (and budget (> budget 0) (pair? effective-conclusions))
          (rank-and-budget effective-conclusions
                           #:current-state state-name
                           #:active-tags active-tags
                           #:max-conclusion-tokens budget)
          effective-conclusions)))

  ;; Add conclusions as context entries when relevant
  (define conclusion-entries
    (cond
      [(or (not state-name) (eq? conclusions-level 'excluded)) '()]
      [(eq? conclusions-level 'full)
       (for/list ([c (in-list budgeted-conclusions)]
                  #:when (task-conclusion? c))
         (make-message (generate-id)
                       #f
                       'system
                       'system-instruction
                       (list (make-text-part (format "[Conclusion] ~a" (task-conclusion-text c))))
                       (current-seconds)
                       (hasheq)))]
      [(eq? conclusions-level 'summary)
       (if (<= (length budgeted-conclusions) 3)
           (for/list ([c (in-list budgeted-conclusions)]
                      #:when (task-conclusion? c))
             (make-message (generate-id)
                           #f
                           'system
                           'system-instruction
                           (list (make-text-part (format "[Conclusion] ~a" (task-conclusion-text c))))
                           (current-seconds)
                           (hasheq)))
           ;; v0.78.3 G8: Use rank-and-budget for summary (not first+last)
           (let ([ranked (rank-and-budget budgeted-conclusions
                                          #:current-state state-name
                                          #:active-tags active-tags
                                          #:max-conclusion-tokens 500)])
             (for/list ([c (in-list ranked)]
                        #:when (task-conclusion? c))
               (make-message (generate-id)
                             #f
                             'system
                             'system-instruction
                             (list (make-text-part (format "[Conclusion] ~a"
                                                           (task-conclusion-text c))))
                             (current-seconds)
                             (hasheq)))))]
      [else '()]))

  ;; v0.96.13 WP-1: Construct memory query text from task state + recent messages
  (define memory-query-text
    (and task-state
         (let* ([state-str (let ([s (coerce-task-state task-state)]) (and s (symbol->string s)))]
                [recent-text (extract-recent-text messages 3)])
           (and state-str
                (if recent-text
                    (string-append "Task: " state-str ". " recent-text)
                    (format "Task: ~a" state-str))))))

  ;; v0.97.5 GAP-G: Enrich memory query with state, active tags, and recent conclusions
  (define enriched-query-text
    (and
     task-state
     (let* ([state-str (let ([s (coerce-task-state task-state)]) (and s (symbol->string s)))]
            [recent-text (extract-recent-text messages 3)]
            [tag-str (if (and (pair? active-tags) (> (length active-tags) 0))
                         (format "Active Tags: ~a. "
                                 (string-join (map symbol->string
                                                   (take active-tags (min 5 (length active-tags))))
                                              ", "))
                         "")]
            [conclusion-str (if (and (pair? budgeted-conclusions) (> (length budgeted-conclusions) 0))
                                (format "Recent Conclusions: ~a. "
                                        (string-join (map task-conclusion-text
                                                          (take budgeted-conclusions
                                                                (min 2
                                                                     (length budgeted-conclusions))))
                                                     "; "))
                                "")])
       (and state-str
            (string-append "State: " state-str ". " tag-str conclusion-str (or recent-text ""))))))

  ;; Observe-only memory integration. This deliberately emits telemetry only;
  ;; prompt injection remains owned by memory-builder and explicit config gates.
  (define memory-section-text #f)
  (when session-config
    (define observed
      (observe-memory-for-context session-config
                                  #:scope #f
                                  #:query-text (or enriched-query-text memory-query-text)
                                  #:tags active-tags))
    (when trace-cb
      (trace-cb 'memory-observe (memory-telemetry->jsexpr (cdr observed))))
    ;; v0.95.15 W3: Inject memory context when injection budget is configured
    (when (current-memory-injection-budget)
      (define injected
        (inject-memory-for-context session-config
                                   #:scope #f
                                   #:query-text (or enriched-query-text memory-query-text)
                                   #:tags active-tags))
      (define section (car injected))
      (when (and section (positive? (string-length section)))
        (set! memory-section-text section)
        (when trace-cb
          (define tel (cdr injected))
          (trace-cb 'memory-inject (memory-telemetry->jsexpr tel))))))

  ;; Build base context with adjusted working-set
  (define base-tc
    (build-tiered-context messages
                          #:tier-b-count tier-b-count
                          #:tier-c-count tier-c-count
                          #:working-set-messages effective-ws
                          #:trace trace-cb))

  ;; v0.75.6: Prepend state-awareness preamble to tier-a
  ;; v0.78.3 G7: Use budgeted conclusions (not raw) for preamble
  (define preamble (build-state-awareness-preamble task-state budgeted-conclusions))
  (define preamble-entries
    (if preamble
        (list preamble)
        '()))
  ;; Prepend preamble + memory + conclusion entries to tier-a
  (define memory-entries
    (if memory-section-text
        (list (make-message (generate-id)
                            #f
                            'system
                            'system-instruction
                            (list (make-text-part memory-section-text))
                            (current-seconds)
                            (hasheq)))
        '()))
  (define new-tier-a
    (append preamble-entries memory-entries conclusion-entries (tiered-context-tier-a base-tc)))
  ;; v0.76.7 W6 + v0.77.9 T2.2: Run rollback trigger checks with action execution
  (when (current-task-state-aware-assembly?)
    (define n-conclusions (length (filter task-conclusion? conclusions)))
    (define coverage
      (if (> (length ws-messages) 0)
          (/ n-conclusions (length ws-messages))
          0.0))
    ;; v0.78.0 G9: Compute repeat-tool-count from recent tool calls
    ;; Count the max frequency of any single tool name in recent history
    (define repeat-count
      (if (null? recent-tool-calls)
          0
          (let ()
            (define freq (make-hash))
            (for ([tc (in-list recent-tool-calls)])
              (hash-set! freq tc (add1 (hash-ref freq tc 0))))
            (apply max (hash-values freq)))))
    (define-values (warnings recommended-action)
      (check-rollback-triggers-with-actions #:before-messages (length ws-messages)
                                            #:after-messages (length effective-ws)
                                            #:conclusion-coverage coverage
                                            #:repeat-tool-count repeat-count))
    (when (pair? warnings)
      (log-warning "context-assembly: rollback triggers fired: ~a" warnings))
    (when recommended-action
      ;; v0.77.10 M2: Wire real execution callbacks
      (parameterize
          ([current-rollback-action-execution? #t]
           [current-force-distill-fn
            (lambda (a)
              (log-warning "context-assembly: force-distill enabling auto-distillation")
              (current-auto-distillation-enabled? #t))]
           [current-expand-context-fn
            (lambda (a)
              (define current-budget (current-conclusion-token-budget))
              (define expanded (* current-budget 2))
              (log-warning "context-assembly: expanding budget ~a → ~a" current-budget expanded)
              (current-conclusion-token-budget expanded))]
           [current-revert-state-fn
            (lambda (a)
              (log-warning "context-assembly: revert-state action triggered: ~a"
                           (rollback-action-reason a)))])
        (define executed (maybe-execute-action recommended-action))
        (when executed
          (log-warning "context-assembly: executed rollback action: ~a" executed)))))
  (if (and (null? preamble-entries) (null? memory-entries) (null? conclusion-entries))
      base-tc
      (tiered-context new-tier-a (tiered-context-tier-b base-tc) (tiered-context-tier-c base-tc))))

;; build-state-awareness-preamble : task-state? (listof task-conclusion?) -> (or/c #f message?)
;; Generates a system prompt section describing the current task state.
;; Returns #f if no meaningful preamble (idle state with no conclusions).
(define (build-state-awareness-preamble task-state conclusions)
  ;; Accept both fsm-state structs and bare symbols
  (define state-name (coerce-task-state task-state))
  (cond
    [(not state-name) #f]
    [else
     (define label
       (case state-name
         [(idle) "IDLE"]
         [(exploration) "EXPLORATION"]
         [(planning) "PLANNING"]
         [(implementation) "IMPLEMENTATION"]
         [(verification) "VERIFICATION"]
         [(debugging) "DEBUGGING"]
         [else "UNKNOWN"]))
     (define guidance (hash-ref state-guidance-table state-name "Focus on the current task."))
     ;; v0.95.18 F9: Filter bare [Auto] conclusions before count/render
     (define visible-conclusions
       (filter (lambda (c)
                 (and (task-conclusion? c)
                      (not (equal? (string-trim (task-conclusion-text c)) "[Auto]"))))
               conclusions))
     (define conclusion-count (length visible-conclusions))
     ;; GAP-A v0.97.7: Cap at 20 (not 10) — budget already controls count
     (define conclusion-section
       (if (> conclusion-count 0)
           (let* ([top (take visible-conclusions (min 20 (length visible-conclusions)))]
                  [texts (for/list ([c (in-list top)])
                           (format "  - ~a" (task-conclusion-text c)))])
             (format "\n\nTop conclusions (~a total, ~a shown):\n~a"
                     conclusion-count
                     (length top)
                     (string-join texts "\n")))
           "\n\nNo conclusions in memory yet. Use record_conclusion to save findings."))
     (define reflection-reminder
       (if (current-reflection-event)
           "\n\nReminder: You recently received large tool results. Use record_conclusion to save key findings before taking more actions."
           ""))
     (define preamble-text
       (format (string-append "You are currently in the ~a phase.~a~a~a~a\n\n"
                              "Workflow: Use record_conclusion to persist findings. "
                              "Use set_task_state to transition: "
                              "exploration→planning→implementation→verification→debugging.")
               label
               guidance
               conclusion-section
               reflection-reminder
               (if (current-blackboard-injection-enabled)
                   (let ([snippet (build-blackboard-context-snippet)])
                     (if snippet
                         (string-append "\n\n" snippet)
                         ""))
                   "")))
     ;; Clear reflection event after consuming it
     (current-reflection-event #f)
     (make-message "state-awareness-preamble"
                   #f
                   'system
                   'system-instruction
                   (list (make-text-part preamble-text))
                   (current-seconds)
                   (hasheq))]))

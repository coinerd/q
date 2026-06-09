#lang racket/base

;; runtime/context-assembly/state-aware-builder.rkt — State-aware context assembly
;; v0.76.0 W2: Extracted from serialization.rkt
;;
;; v0.95.14: Observe-only memory telemetry is wired behind session-config gates;
;; prompt injection remains explicit in memory-builder.rkt.

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
         (only-in "../../util/fsm/fsm.rkt" fsm-state? fsm-state-name)
         (only-in "../../util/content/content-parts.rkt" text-part-text text-part? text-part)
         "context-floor.rkt"
         (only-in "conclusion-graph.rkt"
                  build-conclusion-graph
                  graph-select-conclusions
                  graph-detect-cycles
                  fallback-select-conclusions)
         (only-in "conclusion-ranker.rkt" rank-and-budget)
         (only-in "rollback-actions.rkt"
                  warnings->actions
                  select-highest-priority-action
                  maybe-execute-action
                  current-force-distill-fn
                  current-expand-context-fn
                  current-revert-state-fn
                  current-rollback-action-log
                  rollback-action-reason)
         (only-in "auto-distillation.rkt" current-auto-distillation-enabled?)
         (only-in "config.rkt"
                  current-task-state-aware-assembly?
                  current-graph-conclusion-selection?
                  current-conclusion-token-budget
                  current-ws-evolution-enabled?)
         (only-in "memory-builder.rkt"
                  observe-memory-for-context
                  memory-telemetry->jsexpr
                  current-memory-injection-budget
                  inject-memory-for-context))

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
         current-reflection-event)

;; v0.96.13 W3: Latest reflection event (set by step-interpreter, checked in preamble)
(define current-reflection-event (make-parameter #f))

;; v0.96.13 WP-1: Extract recent assistant message text for memory query context.
;; Takes a list of messages and count N, returns concatenated text of the last N
;; assistant/user messages truncated to 200 chars. Returns #f if no text found.
(define (extract-recent-text messages n)
  (define assistant-msgs
    (for/list ([m (in-list messages)]
               #:when (and (message? m) (memq (message-role m) '(assistant user))))
      (define content-parts (message-content m))
      (define text
        (for/fold ([acc ""])
                  ([part (in-list (if (list? content-parts)
                                      content-parts
                                      '()))])
          (if (text-part? part)
              (string-append acc (if (string=? acc "") "" " ") (text-part-text part))
              acc)))
      text))
  (define recent (take (reverse assistant-msgs) (min n (length assistant-msgs))))
  (define joined (string-join (filter (lambda (s) (> (string-length s) 0)) recent) ". "))
  (if (> (string-length joined) 0)
      (substring joined 0 (min (string-length joined) 200))
      #f))

;; State-specific guidance strings (action-oriented instructions)
(define state-guidance-table
  (hasheq 'idle
          "Awaiting instructions."
          'exploration
          (string-append "Focus on reading and understanding the codebase. "
                         "Record key findings with record_conclusion before moving on.")
          'planning
          (string-append "Break down the task into steps. "
                         "Save your plan as conclusions with record_conclusion.")
          'implementation
          (string-append "Focus on editing files. "
                         "Record key decisions with record_conclusion. "
                         "Prefer existing conclusions over re-reading files.")
          'verification
          (string-append "Run tests and verify correctness. "
                         "Record test results as conclusions with record_conclusion.")
          'debugging
          (string-append "Focus on error-related files. "
                         "Save error analysis as conclusions with record_conclusion.")))

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
                                          #:session-config [session-config #f])
  ;; Accept both fsm-state structs and bare symbols
  (define state-name
    (cond
      [(not task-state) #f]
      [(symbol? task-state) task-state]
      [(fsm-state? task-state) (fsm-state-name task-state)]
      [else #f]))
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

  ;; v0.77.3 W3.3 + v0.77.9 T1.1: Graph-based conclusion filtering when flag is enabled
  (define effective-conclusions
    (cond
      [(not (current-graph-conclusion-selection?)) conclusions]
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
          ;; Cycles detected — log and fall back to recency selection
          (log-warning "context-assembly: conclusion graph has ~a cycle(s), falling back"
                       (length cycles))
          (fallback-select-conclusions conclusions 20 current-states)]
         [else
          ;; Seed-based subgraph selection (v0.77.10 M4: uses convenience wrapper)
          (define selected-conclusions (graph-select-conclusions graph seed-ids))
          (if (null? selected-conclusions)
              ;; No seeds matched — fall back to recency selection
              (fallback-select-conclusions conclusions 20 current-states)
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
         (let* ([state-str (if (symbol? task-state)
                               (symbol->string task-state)
                               (if (fsm-state? task-state)
                                   (symbol->string (fsm-state-name task-state))
                                   #f))]
                [recent-text (extract-recent-text messages 3)])
           (and state-str
                (if recent-text
                    (string-append "Task: " state-str ". " recent-text)
                    (format "Task: ~a" state-str))))))

  ;; v0.97.5 GAP-G: Enrich memory query with state, active tags, and recent conclusions
  (define enriched-query-text
    (and
     task-state
     (let* ([state-str (if (symbol? task-state)
                           (symbol->string task-state)
                           (if (fsm-state? task-state)
                               (symbol->string (fsm-state-name task-state))
                               #f))]
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
      (parameterize ([current-force-distill-fn
                      (lambda (a)
                        (log-warning "context-assembly: force-distill enabling auto-distillation")
                        (current-auto-distillation-enabled? #t))]
                     [current-expand-context-fn
                      (lambda (a)
                        (define current-budget (current-conclusion-token-budget))
                        (define expanded (* current-budget 2))
                        (log-warning "context-assembly: expanding budget ~a \xe2\x86\x92 ~a"
                                     current-budget
                                     expanded)
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
  (define state-name
    (cond
      [(not task-state) #f]
      [(symbol? task-state) task-state]
      [(fsm-state? task-state) (fsm-state-name task-state)]
      [else #f]))
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
     (define conclusion-section
       (if (> conclusion-count 0)
           (let* ([top (take visible-conclusions (min 10 (length visible-conclusions)))]
                  [texts (for/list ([c (in-list top)])
                           (format "  - ~a" (task-conclusion-text c)))])
             (format "\n\nKey conclusions (~a in memory):\n~a"
                     conclusion-count
                     (string-join texts "\n")))
           "\n\nNo conclusions in memory yet. Use record_conclusion to save findings."))
     (define reflection-reminder
       (if (current-reflection-event)
           "\n\nReminder: You recently received large tool results. Use record_conclusion to save key findings before taking more actions."
           ""))
     (define preamble-text
       (format
        "You are currently in the ~a phase.~a~a~a\n\nWorkflow: Use record_conclusion to persist findings. Use set_task_state to transition: exploration→planning→implementation→verification→debugging."
        label
        guidance
        conclusion-section
        reflection-reminder))
     ;; Clear reflection event after consuming it
     (current-reflection-event #f)
     (make-message "state-awareness-preamble"
                   #f
                   'system
                   'system-instruction
                   (list (make-text-part preamble-text))
                   (current-seconds)
                   (hasheq))]))

;; ════════════════════════════════════════════════════════════════
;; v0.76.3 W2: Rollback trigger checks (observational only)
;; ════════════════════════════════════════════════════════════════

;; check-rollback-triggers :
;;   #:before-messages number? #:after-messages number?
;;   #:conclusion-coverage number? #:repeat-tool-count exact-nonnegative-integer?
;;   -> (listof (list/c symbol? string?))
;;
;; Returns a list of warning tuples: (trigger-type message).
;; Triggers are observational — callers decide what to do.
(define (check-rollback-triggers #:before-messages before-messages
                                 #:after-messages after-messages
                                 #:conclusion-coverage conclusion-coverage
                                 #:repeat-tool-count repeat-tool-count)
  (define warnings '())

  ;; Trigger 1: Excessive savings (>50% messages cut)
  ;; R2 risk: context may be too small to work effectively
  (when (and (> before-messages 0) (> after-messages 0) (< after-messages (* before-messages 0.50)))
    (set!
     warnings
     (cons (list 'excessive-savings
                 (format "Context reduced by >50%: ~a → ~a messages" before-messages after-messages))
           warnings)))

  ;; Trigger 2: Low conclusion coverage (amnesia risk)
  ;; R0 risk: agent forgetting key findings
  (when (< conclusion-coverage 0.20)
    (set! warnings
          (cons (list 'amnesia-risk (format "Conclusion coverage too low: ~a" conclusion-coverage))
                warnings)))

  ;; Trigger 3: Repeated tool calls (same file re-read > 2x)
  ;; Indicates agent is re-reading files it should have conclusions for.
  ;; NOTE: When repeat-tool-count >= 6 AND conclusion-coverage = 0,
  ;; this trigger AND Trigger 4 (stuck-detected) both fire. This is
  ;; intentional — Trigger 3 produces task-amnesia warnings that feed
  ;; the repeat escalation path; Trigger 4 produces stuck warnings that
  ;; trigger expand-context. The actions from both paths are collected
  ;; and the highest-priority action wins (see check-rollback-triggers-with-actions).
  (when (> repeat-tool-count 2)
    (set! warnings
          (cons (list 'task-amnesia-detected
                      (format "Repeated tool calls detected: ~a re-reads" repeat-tool-count))
                warnings)))

  ;; v0.96.14 F1: Trigger 4 — Stuck detection (≥6 tool calls, 0 conclusions)
  ;; Agent has done many tool calls but hasn't recorded any conclusions
  (when (and (>= repeat-tool-count 6) (= conclusion-coverage 0))
    (set! warnings
          (cons (list 'stuck-detected
                      (format "stuck: ~a tool calls without recording conclusions" repeat-tool-count))
                warnings)))

  (reverse warnings))

;; v0.77.6 W6.2: Extended trigger output with recommended actions.
;; Returns (values warnings recommended-action) where recommended-action
;; is the highest-priority rollback-action or #f.
(define (check-rollback-triggers-with-actions #:before-messages before-messages
                                              #:after-messages after-messages
                                              #:conclusion-coverage conclusion-coverage
                                              #:repeat-tool-count repeat-tool-count)
  (define warnings
    (check-rollback-triggers #:before-messages before-messages
                             #:after-messages after-messages
                             #:conclusion-coverage conclusion-coverage
                             #:repeat-tool-count repeat-tool-count))
  (define warning-strs (map (lambda (w) (cadr w)) warnings))
  (define actions (warnings->actions warning-strs))
  (define recommended (select-highest-priority-action actions))
  (values warnings recommended))

;; ════════════════════════════════════════════════════════════════
;; v0.76.4 M5 W0: Conclusion-first working-set replacement
;; ════════════════════════════════════════════════════════════════

;; ws-entry->conclusion-or-self :
;;   message? (listof task-conclusion?) -> message?
;; If a conclusion references this message's ID via origin-message-ids,
;; returns a compact replacement message with conclusion text.
;; Otherwise returns the original message unchanged.
(define (ws-entry->conclusion-or-self ws-msg conclusions)
  (define msg-id-val (message-id ws-msg))
  ;; v0.76.7 C1+C2: Use ormap+equal? for robust matching across types
  (define (origin-matches? c)
    (define oids (task-conclusion-origin-message-ids c))
    (and (pair? oids) (ormap (lambda (oid) (equal? msg-id-val oid)) oids)))
  (define matching-conclusion
    (for/first ([c (in-list conclusions)]
                #:when (and (task-conclusion? c) (origin-matches? c)))
      c))
  (cond
    [matching-conclusion
     (make-message (message-id ws-msg)
                   #f
                   'system
                   'system-instruction
                   (list (make-text-part (format "[Conclusion replaces context] ~a"
                                                 (task-conclusion-text matching-conclusion))))
                   (current-seconds)
                   (hasheq))]
    [else ws-msg]))

#lang racket/base

;; runtime/context-assembly/state-aware-builder.rkt — State-aware context assembly
;; v0.76.0 W2: Extracted from serialization.rkt

(require racket/list
         racket/string
         (only-in "../../util/protocol-types.rkt" message make-message make-text-part)
         (only-in "task-conclusion.rkt" task-conclusion? task-conclusion-text)
         (only-in "state-relevance.rkt" context-level-for-state)
         (only-in "../../util/ids.rkt" generate-id)
         (only-in "../../util/fsm.rkt" fsm-state? fsm-state-name)
         "context-floor.rkt")

(provide current-task-state-aware-assembly?
         build-tiered-context/state-aware
         build-state-awareness-preamble)

;; Feature flag: state-aware context assembly (v0.75.3)
(define current-task-state-aware-assembly? (make-parameter #f))

;; State-specific guidance strings
(define state-guidance-table
  (hasheq
   'idle
   "Awaiting instructions."
   'exploration
   "Focus on reading and understanding the codebase. Use save_conclusion to record key findings."
   'planning
   "Break down the task into steps. Use save_conclusion to record the plan."
   'implementation
   "Focus on editing files. Use save_conclusion to record key decisions."
   'verification
   "Run tests and verify correctness. Use save_conclusion to record test results."
   'debugging
   "Focus on error-related files. Use save_conclusion to record debugging insights."))

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
                                          #:trace [trace-cb #f])
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
  (define effective-ws
    (cond
      [(eq? ws-level 'excluded) '()]
      ;; Keep only the most recent working-set entries
      [(eq? ws-level 'filtered) (take ws-messages (min 3 (length ws-messages)))]
      [else ws-messages]))

  ;; Add conclusions as context entries when relevant
  (define conclusion-entries
    (cond
      [(or (not state-name) (eq? conclusions-level 'excluded)) '()]
      [(eq? conclusions-level 'full)
       (for/list ([c (in-list conclusions)]
                  #:when (task-conclusion? c))
         (make-message (generate-id)
                       #f
                       'system-instruction
                       'text
                       (list (make-text-part (format "[Conclusion] ~a" (task-conclusion-text c))))
                       (current-seconds)
                       (hasheq)))]
      [(eq? conclusions-level 'summary)
       (if (<= (length conclusions) 3)
           (for/list ([c (in-list conclusions)]
                      #:when (task-conclusion? c))
             (make-message (generate-id)
                           #f
                           'system-instruction
                           'text
                           (list (make-text-part (format "[Conclusion] ~a" (task-conclusion-text c))))
                           (current-seconds)
                           (hasheq)))
           ;; Summarize: take first + last
           (let ([first-c (car conclusions)]
                 [last-c (car (reverse conclusions))])
             (for/list ([c (list first-c last-c)]
                        #:when (task-conclusion? c))
               (make-message (generate-id)
                             #f
                             'system-instruction
                             'text
                             (list (make-text-part (format "[Conclusion] ~a"
                                                           (task-conclusion-text c))))
                             (current-seconds)
                             (hasheq)))))]
      [else '()]))

  ;; Build base context with adjusted working-set
  (define base-tc
    (build-tiered-context messages
                          #:tier-b-count tier-b-count
                          #:tier-c-count tier-c-count
                          #:working-set-messages effective-ws
                          #:trace trace-cb))

  ;; v0.75.6: Prepend state-awareness preamble to tier-a
  (define preamble (build-state-awareness-preamble task-state conclusions))
  (define preamble-entries
    (if preamble
        (list preamble)
        '()))
  ;; Prepend preamble + conclusion entries to tier-a
  (define new-tier-a (append preamble-entries conclusion-entries (tiered-context-tier-a base-tc)))
  (if (and (null? preamble-entries) (null? conclusion-entries))
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
    [(eq? state-name 'idle) #f]
    [else
     (define label
       (case state-name
         [(exploration) "EXPLORATION"]
         [(planning) "PLANNING"]
         [(implementation) "IMPLEMENTATION"]
         [(verification) "VERIFICATION"]
         [(debugging) "DEBUGGING"]
         [else "UNKNOWN"]))
     (define guidance (hash-ref state-guidance-table state-name "Focus on the current task."))
     (define conclusion-section
       (if (and (pair? conclusions) (task-conclusion? (car conclusions)))
           (let* ([top (take conclusions (min 10 (length conclusions)))]
                  [texts (for/list ([c (in-list top)])
                           (format "  - ~a" (task-conclusion-text c)))])
             (format "\n\nKey conclusions:\n~a" (string-join texts "\n")))
           ""))
     (define preamble-text
       (format "You are currently in the ~a phase.~a~a" label guidance conclusion-section))
     (make-message "state-awareness-preamble"
                   #f
                   'system-instruction
                   'text
                   (list (make-text-part preamble-text))
                   (current-seconds)
                   (hasheq))]))

#lang racket/base

;; tools/builtins/save-conclusion.rkt — Save conclusion tool
;;
;; Layer: tools (built-in)
;; Purpose: Allows the agent to save distilled insights (conclusions) during
;; a task, tagged with the current FSM state and relevance tags. These
;; conclusions replace raw file contents in the prompt once exploration
;; is complete, enabling context optimization.

(require racket/contract
         racket/string
         "../tool.rkt"
         "../define-tool.rkt"
         (only-in "../../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-category?
                  valid-categories)
         (only-in "../../runtime/context-assembly/task-memory.rkt"
                  task-memory?
                  task-memory-conclusions
                  add-conclusion
                  make-task-memory)
         (only-in "../exec-context.rkt" exec-context-event-publisher exec-context?))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (save-conclusion-handler args [exec-ctx #f])
  (define content (hash-ref args 'content #f))
  (define category (hash-ref args 'category 'fact))
  (define tags (hash-ref args 'tags '()))

  (cond
    [(not content) (make-error-result "Missing required argument: content")]
    [(not (string? content)) (make-error-result "content must be a string")]
    [(not (task-conclusion-category? category))
     (make-error-result (format "Invalid category: ~a. Valid: ~a" category valid-categories))]
    [(not (and (list? tags) (andmap symbol? tags)))
     (make-error-result "tags must be a list of symbols")]
    [else
     ;; v0.76.7 W1: Emit deprecation event
     (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
     (when ev-pub
       (ev-pub "tool.deprecated" (hasheq 'tool "save-conclusion" 'replacement "record_conclusion")))
     ;; Generate a unique ID and create the conclusion
     (define id (format "c~a" (current-inexact-milliseconds)))
     (define c
       (task-conclusion id
                        content
                        category
                        'idle ; fsm-state-origin: will be set by session
                        '() ; origin-message-ids: set by session
                        (current-seconds)
                        tags
                        '())) ; dependencies — v0.76.5
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Conclusion saved: [~a] ~a" category content)))
      (hasheq '_conclusion c))]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 save-conclusion
 #:description
 "[DEPRECATED - use record_conclusion instead] Save a distilled insight or conclusion about the current task. Use after discovering important facts, making decisions, identifying patterns, finding error causes, or getting test results. Conclusions replace raw file contents in the prompt when the agent moves past exploration, enabling context optimization."
 #:required ("content")
 #:properties
 [(content "string" "The conclusion text")
  (category "string" "Category: fact, decision, pattern, error-cause, test-result (default: fact)")
  (tags "array" "List of relevance tags (symbols) for state-aware filtering")]
 save-conclusion-handler)

(provide (contract-out [save-conclusion any/c]))

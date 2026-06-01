#lang racket/base

;; tools/builtins/record-conclusion.rkt — Record conclusion tool
;;
;; Layer: tools (built-in)
;; Purpose: Allows the agent to record distilled insights (conclusions) during
;; a task. Emits a tool.record_conclusion.completed event that the session
;; layer subscribes to for persistence.

(require racket/contract
         racket/string
         "../tool.rkt"
         "../define-tool.rkt"
         (only-in "../../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-category?
                  valid-categories)
         (only-in "../../tools/exec-context.rkt"
                  exec-context-event-publisher
                  exec-context?
                  exec-context-call-id))

;; --------------------------------------------------
;; Helpers
;; --------------------------------------------------

;; Coerce argument to symbol category, validating against known categories.
(define (parse-category v)
  (define sym
    (if (string? v)
        (string->symbol v)
        v))
  (and (task-conclusion-category? sym) sym))

;; Coerce tags argument to list of symbols.
(define (parse-tags v)
  (cond
    [(null? v) '()]
    [(list? v)
     (for/list ([item (in-list v)])
       (if (string? item)
           (string->symbol item)
           item))]
    [else '()]))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (record-conclusion-handler args [exec-ctx #f])
  (define text (hash-ref args 'text #f))
  (define category-raw (hash-ref args 'category "fact"))
  (define tags-raw (hash-ref args 'tags '()))

  (cond
    [(not text) (make-error-result "Missing required argument: text")]
    [(not (string? text)) (make-error-result "text must be a string")]
    [else
     (define category (parse-category category-raw))
     (define tags (parse-tags tags-raw))
     (if (not category)
         (make-error-result (format "Invalid category: ~a. Valid: ~a"
                                    category-raw
                                    (string-join (map symbol->string valid-categories) ", ")))
         (let ([id (format "c~a" (current-inexact-milliseconds))])
           ;; Create conclusion (fsm-state-origin is 'idle placeholder — session layer fills in)
           (define c
             (task-conclusion id
                              text
                              category
                              'idle ; placeholder — session layer overrides
                              '() ; origin-message-ids — set by session layer
                              (current-seconds)
                              tags
                              '())) ; dependencies — v0.76.5

           ;; Emit event for session layer to persist
           (define ev-pub (and exec-ctx (exec-context-event-publisher exec-ctx)))
           (when ev-pub
             (define origin-id (or (and exec-ctx (exec-context-call-id exec-ctx)) ""))
             (ev-pub "tool.record_conclusion.completed"
                     (hasheq 'conclusion-id
                             id
                             'text
                             text
                             'category
                             category
                             'tags
                             tags
                             'origin-message-id
                             origin-id)))

           (make-success-result
            (list (hasheq 'type "text" 'text (format "Conclusion recorded: [~a] ~a" category text))
                  (hasheq 'type "json" 'json (hasheq 'success #t 'conclusion_id id))))))]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 record_conclusion
 #:description
 "Record a distilled insight or conclusion about the current task. Use after discovering important facts, making decisions, identifying patterns, finding error causes, or getting test results. Conclusions replace raw file contents in the prompt when the agent moves past exploration, enabling context optimization."
 #:required ("text")
 #:properties
 [(text "string" "The conclusion text")
  (category "string" "Category: fact, decision, pattern, error-cause, test-result (default: fact)")
  (tags "array" "List of relevance tags for state-aware filtering")]
 record-conclusion-handler)

(provide (contract-out [record_conclusion any/c]))

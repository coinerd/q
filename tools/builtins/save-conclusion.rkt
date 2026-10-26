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
;; R94 (BUG-0094): JSON wire -> internal symbol normalization
;; --------------------------------------------------
;; The wire schema advertises category as a JSON string and tags as a JSON
;; array of strings, so JSON-decoded LLM arguments arrive as strings while
;; internal callers may pass symbols directly. Normalization happens ONLY
;; here at the handler boundary — after scheduler approval/hook final
;; admission — and never before it. Defaulting applies ONLY to a fully
;; omitted category; every explicit malformed value is rejected with a
;; structured error and zero side effects.

;; Exact string->symbol map for the five known wire categories.
;; No fuzzy matching: unknown, empty, or wrong-case strings are rejected.
(define (wire-category->symbol v)
  (case v
    [("fact") 'fact]
    [("decision") 'decision]
    [("pattern") 'pattern]
    [("error-cause") 'error-cause]
    [("test-result") 'test-result]
    [else #f]))

;; Resolve a category argument:
;;  - symbol (internal caller): preserved iff one of the five valid categories
;;  - string (JSON wire): mapped iff it exactly names a known category
;;  - anything else (null/boolean/number/object): rejected
(define (resolve-category v)
  (cond
    [(symbol? v) (and (task-conclusion-category? v) v)]
    [(string? v) (wire-category->symbol v)]
    [else #f]))

;; Normalize one tag element: JSON strings become symbols, valid symbols are
;; preserved, and null/number/boolean/object elements are rejected (#f).
;; JSON null decodes to the 'null symbol, so that symbol is rejected too.
(define (normalize-tag v)
  (cond
    [(string? v) (string->symbol v)]
    [(and (symbol? v) (not (eq? v 'null))) v]
    [else #f]))

;; Resolve a tags argument: must be a list and every element must normalize.
;; Returns the normalized symbol list, or #f on any non-list/malformed input.
(define (resolve-tags v)
  (and (list? v)
       (let loop ([xs v])
         (cond
           [(null? xs) '()]
           [else
            (define tag (normalize-tag (car xs)))
            (and tag
                 (let ([rest (loop (cdr xs))])
                   (and rest (cons tag rest))))]))))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (save-conclusion-handler args [exec-ctx #f])
  (define content (hash-ref args 'content #f))
  (cond
    [(not content) (make-error-result "Missing required argument: content")]
    [(not (string? content)) (make-error-result "content must be a string")]
    [else
     ;; R94: default ONLY a fully omitted category; explicit values are
     ;; normalized or rejected below, never defaulted.
     (define category-raw (hash-ref args 'category 'fact))
     (define tags-raw (hash-ref args 'tags '()))
     (define category (resolve-category category-raw))
     (define tags (resolve-tags tags-raw))
     (cond
       [(not category)
        (make-error-result
         (format "Invalid category: ~a. Valid: ~a" category-raw valid-categories))]
       [(not tags)
        (make-error-result
         (format "tags must be a list of strings or symbols; got ~v" tags-raw))]
       [else
        ;; v0.76.7 W1: Emit deprecation event — only after full validation,
        ;; so every rejection path is free of side effects.
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
         (hasheq '_conclusion c))])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 save-conclusion
 #:description
 "[DEPRECATED - use record_conclusion instead] Save a distilled insight or conclusion about the current task."
 #:required ("content")
 #:properties
 [(content "string" "The conclusion text")
  (category "string" "Category: fact, decision, pattern, error-cause, test-result (default: fact)")
  (tags "array" "List of relevance tags for state-aware filtering (JSON strings, normalized internally)")]
 save-conclusion-handler)

(provide (contract-out [save-conclusion tool?]))

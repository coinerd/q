#lang racket/base

;; skills/mas-workflow.rkt — MAS workflow skill type
;; STABILITY: evolving
;;
;; v0.99.26 §5.2: Defines the data structures and parser for
;; mas-workflow skills. A mas-workflow skill packages a multi-agent
;; pipeline as a declarative spec in SKILL.md frontmatter.
;;
;; The frontmatter format is:
;;
;;   ---
;;   type: mas-workflow
;;   agents:
;;     - role: analyst
;;       task: "Read and summarize {{file}}"
;;       capabilities: [read-only]
;;     - role: reviewer
;;       task: "Review the summary: {{result}}"
;;       capabilities: [read-only]
;;   ---
;;
;; This module handles parsing + validation only. Execution is in
;; skills/workflow-executor.rkt (W3).

(require racket/string
         racket/list)

;; ============================================================
;; Structs
;; ============================================================

;; A single step in a multi-agent workflow pipeline.
;; role:         string? — agent role (used as system prompt or skill name)
;; task:         string? — task description (supports {{var}} templates)
;; capabilities: (or/c (listof symbol?) #f) — capability filter for child tools
;; parallel?:    boolean? — if #t, run this step in parallel with siblings
(struct workflow-step (role task capabilities parallel?) #:transparent)

;; A complete parsed mas-workflow.
;; name:        string?
;; description: string?
;; steps:       (listof workflow-step?)
;; variables:   (listof string?) — template vars detected in tasks (e.g., "file")
(struct mas-workflow (name description steps variables) #:transparent)

;; ============================================================
;; Parser
;; ============================================================

;; parse-mas-workflow : string? string? hash? -> (values (or/c mas-workflow? #f) (or/c string? #f))
;; Parse frontmatter hash into a mas-workflow.
;; Returns (values workflow error) where error is #f on success,
;; or an error message string on failure.
(define (parse-mas-workflow name description frontmatter)
  (define agents-raw (hash-ref frontmatter 'agents #f))
  (cond
    [(not agents-raw) (values #f "mas-workflow requires 'agents' field")]
    [(not (list? agents-raw)) (values #f "'agents' must be a list")]
    [(null? agents-raw) (values #f "'agents' list must not be empty")]
    [else
     (define steps
       (for/list ([agent (in-list agents-raw)])
         (workflow-step (hash-ref agent 'role "assistant")
                        (hash-ref agent 'task "")
                        (parse-capabilities (hash-ref agent 'capabilities #f))
                        (hash-ref agent 'parallel #f))))
     ;; Validate: every step must have a non-empty task
     (define empty-tasks (filter (lambda (s) (string=? (workflow-step-task s) "")) steps))
     (cond
       [(not (null? empty-tasks)) (values #f "every workflow step must have a 'task'")]
       [else
        (define vars (extract-template-variables steps))
        (values (mas-workflow name description steps vars) #f)])]))

;; ============================================================
;; Template variable extraction
;; ============================================================

;; extract-template-variables : (listof workflow-step?) -> (listof string?)
;; Extract {{var}} references from all step tasks.
;; Returns a deduplicated list of variable names (without braces).
;; Note: {{result}} is a built-in variable, but it IS included in the
;; list so the executor can track all referenced variables.
(define (extract-template-variables steps)
  (remove-duplicates (for*/list ([step (in-list steps)]
                                 [m (in-list (regexp-match* #rx"\\{\\{([^}]+)\\}\\}"
                                                            (workflow-step-task step)
                                                            #:match-select cadr))])
                       (string-trim m))))

;; ============================================================
;; Capability parsing
;; ============================================================

;; parse-capabilities : (or/c list? #f) -> (or/c (listof symbol?) #f)
;; Parse capabilities field: ("read-only" "file-write") → '(read-only file-write)
;; Returns #f when input is #f, empty, or contains no valid strings.
(define (parse-capabilities caps-raw)
  (cond
    [(not caps-raw) #f]
    [(list? caps-raw)
     (define parsed (filter-map (lambda (c) (and (string? c) (string->symbol c))) caps-raw))
     (if (null? parsed) #f parsed)]
    [else #f]))

;; ============================================================
;; Utility predicates
;; ============================================================

;; workflow-has-result-chaining? : mas-workflow? -> boolean?
;; Returns #t if any step references {{result}} (indicating sequential chaining).
(define (workflow-has-result-chaining? wf)
  (ormap (lambda (v) (string=? v "result")) (mas-workflow-variables wf)))

;; ============================================================
;; Exports
;; ============================================================

(provide workflow-step
         workflow-step?
         workflow-step-role
         workflow-step-task
         workflow-step-capabilities
         workflow-step-parallel?
         mas-workflow
         mas-workflow?
         mas-workflow-name
         mas-workflow-description
         mas-workflow-steps
         mas-workflow-variables
         parse-mas-workflow
         extract-template-variables
         parse-capabilities
         workflow-has-result-chaining?)

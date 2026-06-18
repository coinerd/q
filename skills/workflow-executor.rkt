#lang racket/base

;; skills/workflow-executor.rkt — MAS workflow execution engine
;; STABILITY: evolving
;;
;; v0.99.26 §5.2: Executes a parsed mas-workflow by spawning subagents
;; in sequence, passing each step's result as context to the next step.
;;
;; Reuses run-subagent-with-config from spawn-subagent.rkt.
;; Respects HITL approval for dangerous capabilities (v0.99.25).
;;
;; Execution model:
;;   step1 → result1
;;   step2(task: "Review: {{result}}", context: result1) → result2
;;   step3(task: "Fix: {{result}}", context: result2) → result3
;;   return aggregated result list
;;
;; Error stops the pipeline: remaining steps are skipped, partial
;; results are returned.

(require racket/string
         racket/list
         "mas-workflow.rkt"
         "template.rkt"
         (only-in "../tools/builtins/spawn-subagent.rkt" subagent-config run-subagent-with-config)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-is-error?
                  tool-result-content
                  make-error-result
                  make-success-result))

;; ============================================================
;; Workflow execution result
;; ============================================================

;; A single step's execution result.
;; step:     exact-nonnegative-integer? — 0-indexed step number
;; role:     string? — agent role used
;; task:     string? — the rendered task that was executed
;; success?: boolean? — whether the step completed without error
;; result:   string? — extracted text result (empty on error)
(struct workflow-step-result (step role task success? result) #:transparent)

;; ============================================================
;; Execution
;; ============================================================

;; execute-workflow : mas-workflow? hash? (or/c exec-context? #f) -> tool-result?
;; Execute a mas-workflow sequentially.
;; workflow:  mas-workflow?
;; variables: hash? — user-provided variable substitutions (e.g., #hash((file . "test.rkt")))
;; exec-ctx:  exec-context? (or #f for testing — provider resolution may fail)
;; Returns: tool-result? — success result with step results as content,
;;           or error result if any step fails.
(define (execute-workflow workflow variables exec-ctx)
  (define steps (mas-workflow-steps workflow))
  (define step-results
    (let loop ([remaining steps]
               [idx 0]
               [prev-result #f]
               [acc '()])
      (cond
        [(null? remaining) (reverse acc)]
        [else
         (define step (car remaining))
         (define rendered-task (render-step-task step variables prev-result))
         (define cfg
           (subagent-config rendered-task
                            (workflow-step-role step)
                            10 ; max-turns (F-1b default)
                            #f ; tools (inherit defaults)
                            #f ; model (inherit)
                            (workflow-step-capabilities step)))
         (define result (run-subagent-with-config cfg exec-ctx))
         (define result-text (extract-result-text result))
         (define step-result
           (workflow-step-result idx
                                 (workflow-step-role step)
                                 rendered-task
                                 (not (tool-result-is-error? result))
                                 result-text))
         (cond
           ;; On error, stop pipeline and return partial results
           [(tool-result-is-error? result) (reverse (cons step-result acc))]
           [else (loop (cdr remaining) (add1 idx) result-text (cons step-result acc))])])))
  ;; Build final result
  (define all-success? (andmap workflow-step-result-success? step-results))
  (if all-success?
      (make-success-result
       (hasheq 'workflow (mas-workflow-name workflow) 'steps (step-results->jsexpr step-results)))
      (make-error-result (format "workflow '~a' failed at step ~a"
                                 (mas-workflow-name workflow)
                                 (find-failed-step-index step-results)))))

;; ============================================================
;; Helpers
;; ============================================================

;; render-step-task : workflow-step? hash? (or/c string? #f) -> string?
;; Render a step's task with template variables and previous result.
(define (render-step-task step variables prev-result)
  (define base-task (workflow-step-task step))
  (define vars-with-result (hash-set variables 'result (or prev-result "")))
  (render-template base-task vars-with-result))

;; extract-result-text : (or/c tool-result? #f) -> string?
;; Extract text from a tool-result's content.
(define (extract-result-text result)
  (cond
    [(not result) ""]
    [(not (tool-result? result)) ""]
    [else
     (define content (tool-result-content result))
     (cond
       [(list? content)
        (string-join (for/list ([c (in-list content)])
                       (if (hash? c)
                           (hash-ref c 'text "")
                           (format "~a" c)))
                     " ")]
       [(string? content) content]
       [else (format "~a" content)])]))

;; step-results->jsexpr : (listof workflow-step-result?) -> (listof hash?)
(define (step-results->jsexpr results)
  (for/list ([r (in-list results)])
    (hasheq 'step
            (workflow-step-result-step r)
            'role
            (workflow-step-result-role r)
            'success
            (workflow-step-result-success? r)
            'result
            (workflow-step-result-result r))))

;; find-failed-step-index : (listof workflow-step-result?) -> exact-nonnegative-integer?
(define (find-failed-step-index results)
  (define failed (findf (lambda (r) (not (workflow-step-result-success? r))) results))
  (if failed
      (workflow-step-result-step failed)
      0))

;; ============================================================
;; Exports
;; ============================================================

(provide workflow-step-result
         workflow-step-result?
         workflow-step-result-step
         workflow-step-result-role
         workflow-step-result-task
         workflow-step-result-success?
         workflow-step-result-result
         execute-workflow
         render-step-task
         extract-result-text)

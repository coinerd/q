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
         (only-in "../tools/builtins/spawn-subagent.rkt"
                  subagent-config
                  run-subagent-with-config
                  tool-spawn-subagents
                  requires-hitl-approval?
                  request-spawn-approval)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-is-error?
                  tool-result-content
                  tool-result-details
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
;; Execute a mas-workflow.
;; Sequential steps run one-by-one with {{result}} chaining.
;; Consecutive steps marked parallel?: #t run together via spawn_subagents.
;; workflow:  mas-workflow?
;; variables: hash? — user-provided variable substitutions (e.g., #hash((file . "test.rkt")))
;; exec-ctx:  exec-context? (or #f for testing — provider resolution may fail)
;; Returns: tool-result? — success result with step results as content,
;;           or error result if any step fails.
(define (execute-workflow workflow variables exec-ctx)
  (define steps (mas-workflow-steps workflow))
  (define groups (group-steps steps))
  (define step-results
    (let loop ([remaining-groups groups]
               [idx 0]
               [prev-result #f]
               [acc '()])
      (cond
        [(null? remaining-groups) (reverse acc)]
        [else
         (define group (car remaining-groups))
         (define-values (group-results next-idx group-success? next-prev-result)
           (execute-step-group group idx variables prev-result exec-ctx))
         (define new-acc (append group-results acc))
         (cond
           ;; On error, stop pipeline and return partial results
           [(not group-success?) (reverse new-acc)]
           [else (loop (cdr remaining-groups) next-idx next-prev-result new-acc)])])))
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

;; group-steps : (listof workflow-step?) -> (listof (listof workflow-step?))
;; Group consecutive parallel? steps together. Each sequential step is its own group.
;; This preserves {{result}} chaining for sequential steps while allowing parallel
;; siblings to run concurrently.
(define (group-steps steps)
  (let loop ([remaining steps]
             [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(workflow-step-parallel? (car remaining))
       (define-values (par rest) (span workflow-step-parallel? remaining))
       (loop rest (cons par acc))]
      [else (loop (cdr remaining) (cons (list (car remaining)) acc))])))

;; span : (X -> boolean?) (listof X) -> (values (listof X) (listof X))
;; Split list into prefix satisfying pred and remainder.
(define (span pred lst)
  (let loop ([lst lst]
             [taken '()])
    (cond
      [(null? lst) (values (reverse taken) '())]
      [(pred (car lst)) (loop (cdr lst) (cons (car lst) taken))]
      [else (values (reverse taken) lst)])))

;; step-group-capabilities : (listof workflow-step?) -> (listof symbol?)
;; Aggregate all capabilities requested by steps in a group.
(define (step-group-capabilities group)
  (remove-duplicates (append* (for/list ([step (in-list group)])
                                (or (workflow-step-capabilities step) '())))))

;; step-group-task-preview : (listof workflow-step?) hash? (or/c string? #f) -> string?
;; Build a short task preview for approval requests.
(define (step-group-task-preview group variables prev-result)
  (string-join
   (for/list ([step (in-list group)])
     (format "[~a] ~a" (workflow-step-role step) (render-step-task step variables prev-result)))
   "\n"))

;; execute-step-group : (listof workflow-step?) exact-nonnegative-integer? hash? string? (or/c exec-context? #f)
;;                      -> (values (listof workflow-step-result?) exact-nonnegative-integer? boolean? string?)
;; Execute a group of steps (sequential singleton or parallel batch).
;; Returns the step results, next index, whether the group succeeded, and the
;; result text to pass as {{result}} to the next group (last step's result).
(define (execute-step-group group idx variables prev-result exec-ctx)
  ;; v0.99.27 W3: HITL approval gate for dangerous capabilities.
  ;; Covers both sequential steps (run-subagent-with-config also checks) and
  ;; parallel batches (which previously bypassed the gate via run-subagent).
  (define group-caps (step-group-capabilities group))
  (define approval-denied?
    (and (requires-hitl-approval? group-caps)
         (not (request-spawn-approval group-caps
                                      (step-group-task-preview group variables prev-result)
                                      exec-ctx))))
  (cond
    [approval-denied?
     (define denied-msg "subagent spawn blocked — HITL approval denied")
     (define denied-results
       (for/list ([step (in-list group)]
                  [step-idx (in-naturals idx)])
         (workflow-step-result step-idx
                               (workflow-step-role step)
                               (render-step-task step variables prev-result)
                               #f
                               denied-msg)))
     (values denied-results (+ idx (length group)) #f "")]
    [(= (length group) 1)
     (define step (car group))
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
     (values (list step-result) (add1 idx) (not (tool-result-is-error? result)) result-text)]
    [else
     ;; Parallel batch: run via spawn_subagents
     (define jobs
       (for/list ([step (in-list group)]
                  [job-idx (in-naturals idx)])
         (define rendered-task (render-step-task step variables prev-result))
         (hasheq 'jobId
                 (format "step-~a" job-idx)
                 'task
                 rendered-task
                 'role
                 (workflow-step-role step)
                 'capabilities
                 (or (workflow-step-capabilities step) '()))))
     (define batch-result
       (tool-spawn-subagents (hasheq 'jobs jobs 'maxParallel 3 'aggregate #f) exec-ctx))
     (cond
       [(tool-result-is-error? batch-result)
        ;; Entire batch failed at orchestration level; mark all steps failed.
        (define results
          (for/list ([step (in-list group)]
                     [step-idx (in-naturals idx)])
            (define rendered-task (render-step-task step variables prev-result))
            (workflow-step-result step-idx
                                  (workflow-step-role step)
                                  rendered-task
                                  #f
                                  (extract-result-text batch-result))))
        (values results (+ idx (length group)) #f "")]
       [else
        (define details (or (tool-result-details batch-result) (hasheq)))
        (define job-results (hash-ref details 'jobs '()))
        ;; Map results by jobId because parallel completion order may differ
        ;; from submission order.
        (define job-result-by-id
          (for/hash ([job-result (in-list job-results)])
            (values (hash-ref job-result 'jobId (format "step-~a" idx)) job-result)))
        (define results
          (for/list ([step (in-list group)]
                     [step-idx (in-naturals idx)])
            (define rendered-task (render-step-task step variables prev-result))
            (define job-id (format "step-~a" step-idx))
            (define job-result (hash-ref job-result-by-id job-id #f))
            (define success (and job-result (hash-ref job-result 'success #f)))
            (define result-text
              (if job-result
                  (extract-text-from-content (hash-ref job-result 'content '()))
                  ""))
            (workflow-step-result step-idx
                                  (workflow-step-role step)
                                  rendered-task
                                  success
                                  result-text)))
        (define group-success? (andmap workflow-step-result-success? results))
        ;; The next sequential group sees the result of the last step in this
        ;; group (by original step order), not the last-completed job.
        (define next-prev-result
          (if (null? results)
              ""
              (workflow-step-result-result (last results))))
        (values results (+ idx (length group)) group-success? next-prev-result)])]))

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
    [else (extract-text-from-content (tool-result-content result))]))

;; extract-text-from-content : (or/c list? string? any/c) -> string?
;; Extract text from a tool result content value.
(define (extract-text-from-content content)
  (cond
    [(list? content)
     (string-join (for/list ([c (in-list content)])
                    (cond
                      [(hash? c) (hash-ref c 'text "")]
                      [(string? c) c]
                      [else (format "~a" c)]))
                  " ")]
    [(string? content) content]
    [else (format "~a" content)]))

;; step-results->jsexpr : (listof workflow-step-result?) -> (listof hash?)
(define (step-results->jsexpr results)
  (for/list ([r (in-list results)])
    (hasheq 'step
            (workflow-step-result-step r)
            'role
            (workflow-step-result-role r)
            'task
            (workflow-step-result-task r)
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

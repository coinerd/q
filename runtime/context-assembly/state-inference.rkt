#lang racket/base

;; runtime/context-assembly/state-inference.rkt — infer task-state from tool-call patterns
;; v0.75.2 W0: Static heuristics for automatic task-state inference
;;
;; Design: Pure functions that analyze a sequence of recent tool-call names
;; and return (values inferred-state confidence). No side effects.
;; The caller (session layer) decides whether to apply the transition.

(require racket/list
         (only-in "task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging
                  task-state?))

(provide infer-task-state-from-tools
         infer-from-recent-turns
         current-state-inference-threshold
         tool-name->category)

;; ── Confidence threshold ──

(define current-state-inference-threshold (make-parameter 0.7))

;; ── Tool classification ──

;; Categorize tool names into groups for heuristic matching.
(define (tool-name->category name)
  (define s
    (if (symbol? name)
        (symbol->string name)
        name))
  (cond
    [(member s '("read" "find" "grep" "ls")) 'read]
    [(member s '("edit" "write" "edit-normalize")) 'write]
    [(member s '("bash")) 'bash]
    [(member s '("save-conclusion" "set-task-state" "record_conclusion")) 'meta]
    [else 'other]))

;; ── Pattern counters ──

(define (count-category calls cat)
  (length (filter (lambda (c) (eq? (tool-name->category c) cat)) calls)))

;; Count bash calls that look like test execution
(define (count-test-bash calls args-list)
  (for/sum ([c (in-list calls)] [a
                                 (in-list (if (>= (length args-list) (length calls))
                                              args-list
                                              (make-list (length calls) #f)))])
           (if (and (eq? (tool-name->category c) 'bash)
                    a
                    (let ([cmd (if (hash-has-key? a 'command)
                                   (hash-ref a 'command)
                                   "")])
                      (or (string-contains? cmd "test")
                          (string-contains? cmd "raco test")
                          (string-contains? cmd "run-tests"))))
               1
               0)))

(define (string-contains? s substr)
  (regexp-match? (regexp-quote substr) s))

;; ── Core inference ──

;; infer-task-state-from-tools :
;;   ((listof (or/c string? symbol?))
;;    (listof (or/c hash? #f))
;;    -> (values (or/c task-state? #f) real?))
;;
;; Analyzes recent tool calls and returns (values inferred-state confidence).
;; Returns (values #f 0.0) if no inference possible.
(define (infer-task-state-from-tools recent-calls [args-list '()])
  (cond
    ;; No tools → idle (high confidence if empty)
    [(null? recent-calls) (values task-idle 0.9)]

    ;; Count categories
    [else
     (define n-read (count-category recent-calls 'read))
     (define n-write (count-category recent-calls 'write))
     (define n-test (count-test-bash recent-calls args-list))
     (define n-total (length recent-calls))

     ;; Heuristic rules (ordered by priority):

     ;; Test failure debugging: test bash + recent read of error-like content
     (cond
       ;; verification: test runner invoked
       [(and (>= n-test 1) (< n-write 1)) (values task-verification 0.8)]

       ;; debugging: test failures + edit/write (fixing)
       [(and (>= n-test 1) (>= n-write 1)) (values task-debugging 0.75)]

       ;; GAP-C v0.97.8: planning: meta tools + reads, allow writes when meta ≥ writes
       ;; Must come before implementation rule so plan-file writes aren't misclassified.
       ;; (agent may write PLAN.md while planning)
       [(let ([n-meta (count-category recent-calls 'meta)]) (and (>= n-meta 1) (>= n-meta n-write)))
        (values task-planning 0.75)]

       ;; implementation: edit/write tools called
       [(>= n-write 1) (values task-implementation 0.8)]

       ;; exploration: ≥3 reads, 0 writes
       [(and (>= n-read 3) (= n-write 0)) (values task-exploration 0.85)]

       ;; Light exploration: some reads, no writes
       [(and (>= n-read 1) (= n-write 0)) (values task-exploration 0.5)]

       ;; Default: no confident inference
       [else (values #f 0.0)])]))

;; ── Batch inference (sliding window) ──

;; infer-from-recent-turns :
;;   ((listof (listof (or/c string? symbol?)))
;;    -> (values (or/c task-state? #f) real?))
;;
;; Takes a list of per-turn tool call lists (most recent first)
;; and returns the best inference across all windows.
(define (infer-from-recent-turns turn-tool-lists)
  (define flat-calls (apply append turn-tool-lists))
  (if (null? flat-calls)
      (values task-idle 0.9)
      (infer-task-state-from-tools (take flat-calls (min (length flat-calls) 10)))))

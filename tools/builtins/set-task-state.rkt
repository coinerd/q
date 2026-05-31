#lang racket/base

;; tools/builtins/set-task-state.rkt — Set task state tool
;;
;; Layer: tools (built-in)
;; Purpose: Allows the agent to transition between task states (idle,
;; exploration, planning, implementation, verification, debugging).
;; The current state drives context assembly optimization.

(require racket/contract
         racket/string
         "../tool.rkt"
         "../define-tool.rkt"
         (only-in "../../runtime/context-assembly/task-state.rkt"
                  task-state?
                  task-valid-transition?
                  task-next-state
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging
                  task-begin-explore
                  task-begin-plan
                  task-begin-implement
                  task-begin-verify
                  task-begin-debug
                  task-task-complete
                  task-revisit
                  task-force-transition
                  task-states-list)
         (only-in "../../util/fsm.rkt" fsm-state-name fsm-state?))

;; State name → singleton mapping
(define state-lookup
  (hasheq 'idle
          task-idle
          'exploration
          task-exploration
          'planning
          task-planning
          'implementation
          task-implementation
          'verification
          task-verification
          'debugging
          task-debugging))

;; Event name → singleton mapping
(define event-lookup
  (hasheq 'begin-explore
          task-begin-explore
          'begin-plan
          task-begin-plan
          'begin-implement
          task-begin-implement
          'begin-verify
          task-begin-verify
          'begin-debug
          task-begin-debug
          'task-complete
          task-task-complete
          'revisit
          task-revisit
          'force-transition
          task-force-transition))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (set-task-state-handler args [exec-ctx #f])
  (define state-name (hash-ref args 'state #f))
  (define event-name (hash-ref args 'event #f))
  ;; Convert strings to symbols for hasheq lookup (LLM sends strings)
  (define state-sym (if (string? state-name) (string->symbol state-name) state-name))
  (define event-sym (if (string? event-name) (string->symbol event-name) event-name))
  (cond
    [(not state-name) (make-error-result "Missing required argument: state")]
    [(not event-name) (make-error-result "Missing required argument: event")]
    [else
     (define target-state (hash-ref state-lookup state-sym #f))
     (define event (hash-ref event-lookup event-sym #f))
     (cond
       [(not target-state)
        (make-error-result (format "Unknown state: ~a. Valid: ~a" state-name (task-states-list)))]
       [(not event)
        (make-error-result
         (format
          "Unknown event: ~a. Valid: begin-explore, begin-plan, begin-implement, begin-verify, begin-debug, task-complete, revisit, force-transition"
          event-name))]
       [else
        ;; The actual transition validation happens in session mutation
        ;; Here we just package the request
        (make-success-result
         (list (hasheq 'type
                       "text"
                       'text
                       (format "Task state transition requested: ~a via ~a" state-name event-name)))
         (hasheq '_task-state-target state-name '_task-state-event event-name))])]))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 set-task-state
 #:description
 "Transition the current task state. Valid states: idle, exploration, planning, implementation, verification, debugging. Valid events: begin-explore, begin-plan, begin-implement, begin-verify, begin-debug, task-complete, revisit, force-transition. The task state drives context assembly optimization."
 #:required ("state" "event")
 #:properties
 [(state "string"
         "Target state: idle, exploration, planning, implementation, verification, debugging")
  (event
   "string"
   "Transition event: begin-explore, begin-plan, begin-implement, begin-verify, begin-debug, task-complete, revisit, force-transition")]
 set-task-state-handler)

(provide (contract-out [set-task-state any/c]))

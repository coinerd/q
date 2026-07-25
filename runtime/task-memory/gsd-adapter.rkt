#lang racket/base

;; runtime/task-memory/gsd-adapter.rkt
;; STABILITY: internal
;;
;; W3C (#8940): GSD adapter — translates GSD state-machine transitions,
;; task-state FSM transitions, conclusions, and checkpoints into
;; task-ledger-events.
;;
;; This is the "state-aware" capture layer: it observes GSD lifecycle
;; signals (not tool outcomes) and emits durable ledger events that
;; record the agent's workflow progression.
;;
;; Layering: this is a RUNTIME module. It imports outcome-capture (for
;; the capture-context and digest helper) and types (the ledger).
;;
;; GSD state machine (extensions/gsd/transition-logic.rkt):
;;   idle → exploring → plan-written → executing → verifying → idle
;;
;; Task-state FSM (runtime/context-assembly/task-state.rkt):
;;   idle → exploration → planning → implementation → verification/debugging

(require racket/contract
         racket/list
         "../../util/ids.rkt"
         "types.rkt"
         "outcome-capture.rkt")

(provide gsd-transition->event-kind
         task-state-transition->event-kind
         conclusion->event-kind
         capture-gsd-transition
         capture-task-state-transition
         capture-conclusion
         capture-checkpoint
         capture-gsd-transitions)

;; ============================================================
;; GSD transition → event-kind mapping
;; ============================================================

;; Map a GSD (from, to, trigger-event) triple to a canonical event-kind.
;; The GSD state machine has 9 transitions (see transition-logic.rkt).
(define (gsd-transition->event-kind from to trigger)
  (cond
    ;; Wave lifecycle
    [(and (eq? from 'plan-written) (eq? to 'executing)) 'wave-started]
    [(and (eq? from 'verifying) (eq? to 'idle) (eq? trigger 'done)) 'wave-completed]
    ;; Cancellations
    [(eq? to 'idle) 'cancelled]
    ;; Plan written = objective set
    [(and (eq? from 'exploring) (eq? to 'plan-written)) 'objective-set]
    ;; All other transitions are phase changes
    [else 'phase-changed]))

;; ============================================================
;; Task-state transition → event-kind mapping
;; ============================================================

;; Map a task-state (from, to) pair to a canonical event-kind.
;; Leaving 'idle means a new task has started.
(define (task-state-transition->event-kind from to)
  (cond
    [(and (eq? from 'idle) (not (eq? to 'idle))) 'task-started]
    [else 'phase-changed]))

;; ============================================================
;; Conclusion → event-kind mapping
;; ============================================================

;; A recorded conclusion is a distilled insight/objective.
(define (conclusion->event-kind category)
  'objective-set)

;; ============================================================
;; Event construction helpers
;; ============================================================

;; Build a task-ledger-event from structured capture fields.
;; Reuses the capture-context for provenance and the digest helper
;; for content integrity.
(define (build-ledger-event ctx
                            #:event-kind event-kind
                            #:correlation-id correlation-id
                            #:payload payload
                            #:evidence-refs [evidence-refs '()]
                            #:timestamp [timestamp #f])
  (make-task-ledger-event 1 ; schema-version
                          (capture-context-session-seq ctx)
                          (generate-id) ; event-id
                          (capture-context-session-id ctx)
                          (capture-context-project-id ctx)
                          (capture-context-task-id ctx)
                          (capture-context-parent-task-id ctx)
                          (capture-context-branch-id ctx)
                          (capture-context-turn-id ctx)
                          (capture-context-request-id ctx)
                          (capture-context-assembly-id ctx)
                          (or correlation-id (generate-id)) ; correlation-id
                          #f ; causation-id
                          'runtime-observed ; source-class
                          event-kind
                          payload
                          (or timestamp (current-seconds))
                          evidence-refs
                          (compute-payload-digest payload)))

;; ============================================================
;; Capture: GSD transition
;; ============================================================

(define (capture-gsd-transition ctx
                                #:from-state from-state
                                #:to-state to-state
                                #:trigger-event trigger-event
                                #:correlation-id correlation-id
                                #:evidence-refs [evidence-refs '()]
                                #:timestamp [timestamp #f])
  (define kind (gsd-transition->event-kind from-state to-state trigger-event))
  (define payload
    (hash 'from-state
          (symbol->string from-state)
          'to-state
          (symbol->string to-state)
          'trigger
          (symbol->string trigger-event)))
  (build-ledger-event ctx
                      #:event-kind kind
                      #:correlation-id correlation-id
                      #:payload payload
                      #:evidence-refs evidence-refs
                      #:timestamp timestamp))

;; ============================================================
;; Capture: task-state transition
;; ============================================================

(define (capture-task-state-transition ctx
                                       #:from-state from-state
                                       #:to-state to-state
                                       #:correlation-id correlation-id
                                       #:evidence-refs [evidence-refs '()]
                                       #:timestamp [timestamp #f])
  (define kind (task-state-transition->event-kind from-state to-state))
  (define payload (hash 'from-state (symbol->string from-state) 'to-state (symbol->string to-state)))
  (build-ledger-event ctx
                      #:event-kind kind
                      #:correlation-id correlation-id
                      #:payload payload
                      #:evidence-refs evidence-refs
                      #:timestamp timestamp))

;; ============================================================
;; Capture: conclusion
;; ============================================================

(define (capture-conclusion ctx
                            #:category category
                            #:summary summary
                            #:correlation-id correlation-id
                            #:tags [tags '()]
                            #:evidence-refs [evidence-refs '()]
                            #:timestamp [timestamp #f])
  (define kind (conclusion->event-kind category))
  (define tag-strings
    (for/list ([t (in-list tags)])
      (symbol->string t)))
  (define payload (hash 'category (symbol->string category) 'summary summary 'tags tag-strings))
  (build-ledger-event ctx
                      #:event-kind kind
                      #:correlation-id correlation-id
                      #:payload payload
                      #:evidence-refs evidence-refs
                      #:timestamp timestamp))

;; ============================================================
;; Capture: checkpoint
;; ============================================================

(define (capture-checkpoint ctx
                            #:checkpoint-id checkpoint-id
                            #:reason reason
                            #:correlation-id correlation-id
                            #:evidence-refs [evidence-refs '()]
                            #:timestamp [timestamp #f])
  (define payload (hash 'checkpoint-id checkpoint-id 'reason reason))
  (build-ledger-event ctx
                      #:event-kind 'checkpoint-created
                      #:correlation-id correlation-id
                      #:payload payload
                      #:evidence-refs evidence-refs
                      #:timestamp timestamp))

;; ============================================================
;; Batch: GSD transition history
;; ============================================================

;; Translate a list of (from to trigger) triples into events.
;; Each triple is a 3-element list: (list from-state to-state trigger).
(define (capture-gsd-transitions ctx transitions #:correlation-id correlation-id)
  (for/list ([t (in-list transitions)])
    (capture-gsd-transition ctx
                            #:from-state (first t)
                            #:to-state (second t)
                            #:trigger-event (third t)
                            #:correlation-id correlation-id)))

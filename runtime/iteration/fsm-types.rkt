#lang racket/base

;; runtime/iteration/fsm-types.rkt — Iteration FSM state/event types (R-06, R-07)
;; STABILITY: evolving
;;
;; Defines an explicit FSM for the iteration loop. States and events are
;; enumerated as data, and the TRANSITIONS table maps (state, event) pairs
;; to next states. Unknown transitions raise explicit errors.

(require racket/match
         (only-in "../../util/fsm.rkt" make-fsm fsm-lookup fsm-valid-transition?))

;; States
(provide iteration-state?
         iteration-state
         state-idle
         state-provider-turn
         state-tool-exec
         state-decision
         state-complete
         state-retrying
         state-aborted
         state->symbol

         ;; Events
         iteration-event?
         iteration-event
         event-start-loop
         event-model-response
         event-tool-result
         event-tool-calls-present
         event-termination-reason
         event-hook-block
         event-error
         event-retry-requested
         event-cancel
         iteration-event->symbol

         ;; Transition table + lookup
         TRANSITIONS
         next-iteration-state
         valid-transition?)

;; ── States ──

(struct iteration-state (name) #:transparent)

(define state-idle (iteration-state 'idle))
(define state-provider-turn (iteration-state 'provider-turn))
(define state-tool-exec (iteration-state 'tool-exec))
(define state-decision (iteration-state 'decision))
(define state-complete (iteration-state 'complete))
(define state-retrying (iteration-state 'retrying))
(define state-aborted (iteration-state 'aborted))

(define (state->symbol s)
  (iteration-state-name s))

;; ── Events ──

(struct iteration-event (name) #:transparent)

(define event-start-loop (iteration-event 'start-loop))
(define event-model-response (iteration-event 'model-response))
(define event-tool-result (iteration-event 'tool-result))
(define event-tool-calls-present (iteration-event 'tool-calls-present))
(define event-termination-reason (iteration-event 'termination-reason))
(define event-hook-block (iteration-event 'hook-block))
(define event-error (iteration-event 'error))
(define event-retry-requested (iteration-event 'retry-requested))
(define event-cancel (iteration-event 'cancel))

(define (iteration-event->symbol e)
  (iteration-event-name e))

;; ── Transition table ──
;; Format: ((state . event) . next-state)

(define TRANSITIONS
  ;; Idle -> Provider-Turn (loop starts)
  ;; Provider-Turn -> Decision (model responds)
  '([(idle . start-loop) . provider-turn] [(provider-turn . model-response) . decision]
                                          ;; Provider-Turn -> Aborted (hook blocks)
                                          [(provider-turn . hook-block) . aborted]
                                          ;; Provider-Turn -> Aborted (cancel)
                                          [(provider-turn . cancel) . aborted]
                                          ;; Provider-Turn -> Retrying (error)
                                          [(provider-turn . error) . retrying]
                                          ;; Decision -> Tool-Exec (tool calls present)
                                          [(decision . tool-calls-present) . tool-exec]
                                          ;; Decision -> Complete (termination reason)
                                          [(decision . termination-reason) . complete]
                                          ;; Decision -> Aborted (hook blocks)
                                          [(decision . hook-block) . aborted]
                                          ;; Tool-Exec -> Decision (tool results back)
                                          [(tool-exec . tool-result) . decision]
                                          ;; Tool-Exec -> Retrying (error)
                                          [(tool-exec . error) . retrying]
                                          ;; Retrying -> Provider-Turn (retry)
                                          [(retrying . retry-requested) . provider-turn]
                                          ;; Retrying -> Aborted (too many retries)
                                          [(retrying . error) . aborted]
                                          ;; Complete and Aborted are terminal states
                                          [(complete . cancel) . complete]
                                          [(aborted . cancel) . aborted]))

;; ── Transition lookup ──

;; fsm machine instance for lookup
(define iteration-machine
  (make-fsm '(idle provider-turn tool-exec decision complete retrying aborted)
            '(start-loop model-response
                         tool-result
                         tool-calls-present
                         termination-reason
                         hook-block
                         error
                         retry-requested
                         cancel)
            TRANSITIONS))

(define (find-transition state-sym event-sym)
  (fsm-lookup iteration-machine state-sym event-sym))

(define (next-iteration-state state event)
  (define state-sym (state->symbol state))
  (define event-sym (iteration-event->symbol event))
  (define next-sym (find-transition state-sym event-sym))
  (unless next-sym
    (error 'next-iteration-state "invalid FSM transition: (~a, ~a)" state-sym event-sym))
  (case next-sym
    [(idle) state-idle]
    [(provider-turn) state-provider-turn]
    [(tool-exec) state-tool-exec]
    [(decision) state-decision]
    [(complete) state-complete]
    [(retrying) state-retrying]
    [(aborted) state-aborted]
    [else (error 'next-iteration-state "unknown state: ~a" next-sym)]))

(define (valid-transition? state event)
  (fsm-valid-transition? iteration-machine (state->symbol state) (iteration-event->symbol event)))

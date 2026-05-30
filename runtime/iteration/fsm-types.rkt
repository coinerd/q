#lang racket/base

;; runtime/iteration/fsm-types.rkt — Iteration FSM state/event types (R-06, R-07)
;; STABILITY: evolving
;;
;; Defines an explicit FSM for the iteration loop using define-fsm-machine
;; from util/fsm.rkt. Backward-compatible exports maintained.

(require racket/contract
         (only-in "../../util/fsm.rkt"
                  define-fsm-machine
                  fsm-state
                  fsm-state?
                  fsm-state-name
                  fsm-event
                  fsm-event?
                  fsm-event-name
                  fsm?
                  fsm-transitions
                  fsm-valid-transition?))

;; ── Machine definition ──

(define-fsm-machine iteration
                    #:states (idle provider-turn tool-exec decision complete retrying aborted)
                    #:events (start-loop model-response
                                         tool-result
                                         tool-calls-present
                                         termination-reason
                                         hook-block
                                         error
                                         retry-requested
                                         cancel)
                    #:transitions [(idle -> provider-turn) start-loop]
                    [(provider-turn -> decision) model-response]
                    [(provider-turn -> aborted) hook-block]
                    [(provider-turn -> aborted) cancel]
                    [(provider-turn -> retrying) error]
                    [(decision -> tool-exec) tool-calls-present]
                    [(decision -> complete) termination-reason]
                    [(decision -> aborted) hook-block]
                    [(tool-exec -> decision) tool-result]
                    [(tool-exec -> retrying) error]
                    [(retrying -> provider-turn) retry-requested]
                    [(retrying -> aborted) error]
                    [(complete -> complete) cancel]
                    [(aborted -> aborted) cancel])

;; ── Backward-compatible exports ──

;; State singletons (old names: state-<name>)
(define state-idle iteration-idle)
(define state-provider-turn iteration-provider-turn)
(define state-tool-exec iteration-tool-exec)
(define state-decision iteration-decision)
(define state-complete iteration-complete)
(define state-retrying iteration-retrying)
(define state-aborted iteration-aborted)

;; Event singletons (old names: event-<name>)
(define event-start-loop iteration-start-loop)
(define event-model-response iteration-model-response)
(define event-tool-result iteration-tool-result)
(define event-tool-calls-present iteration-tool-calls-present)
(define event-termination-reason iteration-termination-reason)
(define event-hook-block iteration-hook-block)
(define event-error iteration-error)
(define event-retry-requested iteration-retry-requested)
(define event-cancel iteration-cancel)

;; Converters
(define (state->symbol s)
  (fsm-state-name s))
(define (iteration-event->symbol e)
  (fsm-event-name e))

;; Transition table (backward compat)
(define TRANSITIONS (fsm-transitions iteration-machine))

;; Next-state with error (backward compat)
(define (next-iteration-state state event)
  (define result (iteration-next-state state event))
  (unless result
    (error 'next-iteration-state
           "invalid FSM transition: (~a, ~a)"
           (fsm-state-name state)
           (fsm-event-name event)))
  result)

;; Valid transition check (backward compat)
(define (valid-transition? state event)
  (iteration-valid-transition? state event))

;; ── Provides ──
(provide iteration-state?
         state-idle
         state-provider-turn
         state-tool-exec
         state-decision
         state-complete
         state-retrying
         state-aborted

         iteration-event?
         event-start-loop
         event-model-response
         event-tool-result
         event-tool-calls-present
         event-termination-reason
         event-hook-block
         event-error
         event-retry-requested
         event-cancel

         TRANSITIONS
         (contract-out [state->symbol (-> fsm-state? symbol?)]
                       [iteration-event->symbol (-> fsm-event? symbol?)]
                       [next-iteration-state (-> fsm-state? fsm-event? any)]
                       [valid-transition? (-> fsm-state? fsm-event? boolean?)]))

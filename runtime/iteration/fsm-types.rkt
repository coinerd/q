#lang racket/base

;; runtime/iteration/fsm-types.rkt — COMPATIBILITY RE-EXPORT
;;
;; v0.99.86: FSM types moved to util/iteration/fsm-types.rkt.
;; This file re-exports them for backward compatibility.
;; TODO: Remove once all consumers import from util/iteration/fsm-types.rkt.

(require (only-in "../../util/iteration/fsm-types.rkt"
                  iteration-state?
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
                  state->symbol
                  iteration-event->symbol
                  next-iteration-state
                  valid-transition?))

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
         state->symbol
         iteration-event->symbol
         next-iteration-state
         valid-transition?)

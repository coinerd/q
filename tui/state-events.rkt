#lang racket/base

;; tui/state-events.rkt -- Event->state reduction for UI state (thin facade)
;; STABILITY: internal
;;
;; v0.96.6 (F1): Decomposed into registry + helpers + core-handlers + goal-handlers.
;; This module requires all sub-modules to trigger handler registration,
;; then re-exports the public API.

(require racket/contract
         (only-in "../runtime/gsd-query.rkt" current-gsd-mode-query)
         (only-in "../util/event/event.rkt" event?)
         "state-types.rkt"
         "state-events/registry.rkt"
         "state-events/helpers.rkt"
         "state-events/core-handlers.rkt"
         "state-events/goal-handlers.rkt")

(provide current-gsd-mode-query
         (contract-out [apply-event-to-state (-> ui-state? event? ui-state?)]
                       [register-event-reducer! (-> string? procedure? void?)]
                       [call-with-test-registry (-> procedure? any)]
                       [event-reducer-registered? (-> string? boolean?)]
                       [classify-error-type (-> any/c hash? symbol?)]
                       [format-error-hint
                        (-> symbol? exact-nonnegative-integer? (listof symbol?) string?)]))

;; Re-export ui-state? and event? for the contract-out signatures.
;; They come from state-types and event, pulled in by sub-modules.
;; The requires above already trigger handler registration side effects.
;; No additional code needed — facade is pure re-export.

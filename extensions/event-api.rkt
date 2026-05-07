#lang racket/base

;; extensions/event-api.rkt — Event emission API for extensions
;;
;; Provides a stable API for extensions to emit typed events
;; without importing from agent/ internals.

(require racket/contract
         "api.rkt"
         "../util/event.rkt"
         "../agent/event-emitter.rkt"
         "../agent/event-structs/iteration-events.rkt")

(provide emit-typed-event!
         make-injection-event
         publish!
         make-event)

;; This facade exists to avoid extensions importing directly from agent/.
;; In a future refactor, the event emitter will move to util/
;; and this facade will re-export from there.

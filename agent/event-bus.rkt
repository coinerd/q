#lang racket/base
;; STABILITY: public

;; agent/event-bus.rkt — RE-EXPORT SHIM
;; v0.98.15 (AXIS1-F12): canonical location is now util/event/event-bus.rkt
;; This shim re-exports everything for backward compatibility.

(require "../util/event/event-bus.rkt")
(provide (rename-out [make-event-bus make-event-bus]
                     [subscribe! subscribe!]
                     [subscribe-map! subscribe-map!]
                     [subscribe-filter! subscribe-filter!]
                     [unsubscribe! unsubscribe!]
                     [publish! publish!]
                     [typed-event->event typed-event->event]
                     [bus-emit-typed! bus-emit-typed!])
         event-bus?
         current-event-bus-error-handler
         current-circuit-breaker-threshold
         current-circuit-breaker-cooldown-secs
         circuit-broken?
         record-failure!
         record-success!)

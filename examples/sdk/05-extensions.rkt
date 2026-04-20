#lang racket/base

;; examples/sdk/05-extensions.rkt — Subscribe to events
;;
;; Demonstrates subscribing to agent events via the SDK.
;; Event payloads carry typed data (event-ev extracts the raw event struct).

(require "../../interfaces/sdk.rkt"
         (only-in "../../util/protocol-types.rkt" event-ev))

(define rt (create-agent-session #:provider (hasheq 'type 'test)))

;; Subscribe to all events — the handler receives raw event structs.
;; subscribe-events! returns a subscription ID.
(define sub-id (subscribe-events! rt (lambda (event) (printf "Event: ~a~n" (event-ev event)))))

(printf "Subscribed with ID: ~a~n" sub-id)
;; To unsubscribe, use the internal event bus:
;;   (unsubscribe! (runtime-config-event-bus (rt-cfg rt)) sub-id)
;; This is an advanced pattern; most consumers just let subscriptions
;; live for the session lifetime.
(printf "Session active with event subscription.~n")

#lang racket/base

;; examples/sdk/05-extensions.rkt — Subscribe to events

(require "../../interfaces/sdk.rkt")

(define rt (create-agent-session #:provider (hasheq 'type 'test)))

;; Subscribe to all events
(define sub-id (subscribe-events! rt (lambda (event) (printf "Event: ~a~n" (event-ev event)))))

(printf "Subscribed with ID: ~a~n" sub-id)
(unsubscribe! (runtime-config-event-bus (rt-cfg rt)) sub-id)
(printf "Unsubscribed~n")

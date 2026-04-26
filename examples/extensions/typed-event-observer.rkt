#lang racket/base

;; examples/extensions/typed-event-observer.rkt — Typed event observer
;;
;; Demonstrates the typed event bridge (EVT-01):
;;   - Publishing typed events via bus-emit-typed!
;;   - Subscribing and inspecting both typed and raw events
;;   - Using typed-event->jsexpr for structured JSON logging

(require "../../agent/event-bus.rkt"
         "../../agent/event-types.rkt"
         "../../util/protocol-types.rkt")

;; Start an observer that logs all events on the bus.
;; Returns subscription ID for cleanup.
(define (start-observer! bus)
  (subscribe!
   bus
   (lambda (evt)
     (define ev-name (event-ev evt))
     (define payload (event-payload evt))
     (printf "[event] ~a | payload keys: ~a\n"
             ev-name
             (if (hash? payload) (hash-keys payload) '())))
   #:filter (lambda (evt) #t)))

;; Demonstrate publishing a typed event through the bus bridge.
;; The typed event is converted to a raw event internally.
(define (demo-typed-emit! bus session-id turn-id)
  ;; Create a typed turn-start event
  (define te (make-turn-start-event #:session-id session-id
                                     #:turn-id turn-id
                                     #:model "gpt-4"
                                     #:provider "openai"))
  ;; Emit via the typed bridge
  (define raw-evt (bus-emit-typed! bus te))
  ;; The raw event now has the typed event's type as event name
  ;; and extra fields as payload
  (printf "[demo] emitted typed event as raw: ~a\n" (event-ev raw-evt))
  raw-evt)

(provide start-observer!
         demo-typed-emit!)

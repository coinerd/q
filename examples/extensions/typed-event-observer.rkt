#lang racket/base

;; examples/extensions/typed-event-observer.rkt — Per-tool typed event observer (#1326)
;;
;; Demonstrates how to subscribe to events and inspect typed event payloads.
;; This extension subscribes to the event bus and logs typed tool-call events.

(require "../../agent/event-bus.rkt"
         "../../agent/event-types.rkt"
         "../../util/protocol-types.rkt")

;; Example: observe all events on the bus and log typed tool-call events.
;; Usage: Call (start-observer! bus) to begin, returns subscription ID.

(define (start-observer! bus)
  (subscribe!
   bus
   (lambda (evt)
     (define payload (event-payload evt))
     (define ev-name (event-ev evt))
     (cond
       ;; Check for typed tool-call events
       [(bash-tool-call-event? payload)
        (printf "[typed-event] bash: ~a in ~a (~ams)\n"
                (bash-tool-call-event-command payload)
                (bash-tool-call-event-cwd payload)
                (bash-tool-call-event-timeout payload))]
       [(edit-tool-call-event? payload)
        (printf "[typed-event] edit: ~a\n"
                (edit-tool-call-event-path payload))]
       [(write-tool-call-event? payload)
        (printf "[typed-event] write: ~a (~a bytes)\n"
                (write-tool-call-event-path payload)
                (string-length (write-tool-call-event-content payload)))]
       [(read-tool-call-event? payload)
        (printf "[typed-event] read: ~a\n"
                (read-tool-call-event-path payload))]
       [(custom-tool-call-event? payload)
        (printf "[typed-event] custom: ~a\n"
                (tool-call-event-tool-name payload))]
       ;; Non-typed events: just log name
       [else
        (printf "[event] ~a\n" ev-name)]))
   #:filter (lambda (evt) #t)))

(provide start-observer!)

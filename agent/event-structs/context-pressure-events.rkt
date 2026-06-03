#lang racket/base

;; agent/event-structs/context-pressure-events.rkt — context pressure signaling events
;; STABILITY: internal

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; Emitted after context assembly to communicate current pressure level.
;; level: 'green | 'yellow | 'red
;; usage-percent: 0.0–100.0+
(define-typed-event context-pressure-event
                    "context.pressure"
                    (level usage-percent)
                    #:schema-version 1
                    #:no-serialize)

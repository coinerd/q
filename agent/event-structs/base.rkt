#lang racket/base

;; agent/event-structs/base.rkt — base typed-event struct
;;
;; All event structs inherit from typed-event.
;; Category-specific sub-modules require this module.

(provide (struct-out typed-event)
         typed-event?)

;; STABILITY: public — stable for extensions
;; Base struct: all events carry type, timestamp, session-id, turn-id
(struct typed-event (type timestamp session-id turn-id) #:transparent)

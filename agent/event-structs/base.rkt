#lang racket/base
;; STABILITY: public

;; agent/event-structs/base.rkt — base typed-event struct
;;
;; All event structs inherit from typed-event.
;; Category-specific sub-modules require this module.

(provide typed-event
         typed-event?
         typed-event-type
         typed-event-timestamp
         typed-event-session-id
         typed-event-turn-id)

;; STABILITY: public — stable for extensions
;; Base struct: all events carry type, timestamp, session-id, turn-id
(struct typed-event (type timestamp session-id turn-id) #:transparent)

#lang racket/base
;; STABILITY: public

;; agent/event-bus.rkt — RE-EXPORT SHIM
;; v0.98.15 (AXIS1-F12): canonical location is now util/event/event-bus.rkt
;; This shim re-exports everything for backward compatibility.

(require "../util/event/event-bus.rkt")
(provide (all-from-out "../util/event/event-bus.rkt"))

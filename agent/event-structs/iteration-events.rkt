#lang racket/base

;; agent/event-structs/iteration-events.rkt — iteration lifecycle events
;; (auto-retry, compaction, injection)

(require "base.rkt"
         "../../util/event-macro.rkt")

;; NOTE (v0.29.16): auto-retry-event has 1 production emission site
;; (runtime/turn-orchestrator.rkt). It is wired — not deferred.

(define-typed-event auto-retry-event "auto-retry"
  (attempt max-attempts error-type))

(define-typed-event compaction-event "compaction"
  (reason tokens-before tokens-after))

(define-typed-event injection-event "injection"
  (source content-type content-length)
  #:optional ([message #f]))

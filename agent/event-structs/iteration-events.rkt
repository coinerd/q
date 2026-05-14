#lang racket/base

;; agent/event-structs/iteration-events.rkt — iteration lifecycle events
;; (auto-retry, compaction, injection)

(require "base.rkt"
         "../../util/event-macro.rkt")

;; NOTE (v0.29.16): auto-retry-event has 1 production emission site
;; (runtime/turn-orchestrator.rkt). It is wired — not deferred.

(define-typed-event auto-retry-event "auto-retry" (attempt max-attempts error-type) #:no-serialize)

;; v0.44.3 (R2): Detailed auto-retry start event — replaces 5-key hasheq payload
(define-typed-event auto-retry-start-event
                    "auto-retry.start"
                    (attempt max-retries delay-ms error error-type))

(define-typed-event compaction-event "compaction" (reason tokens-before tokens-after) #:no-serialize)

(define-typed-event injection-event
                    "injection"
                    (source content-type content-length)
                    #:optional ([message #f])
                    #:no-serialize)

;; v0.44.3 (R2): Iteration decision event — replaces 5-key hasheq payload
(define-typed-event iteration-decision-event
                    "iteration.decision"
                    (iteration termination consecutive-tools max-iterations max-iterations-hard))

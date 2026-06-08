#lang racket/base

;; agent/event-structs/iteration-events.rkt — iteration lifecycle events
;; (auto-retry, compaction, injection)

(require "base.rkt"
         "../../util/event/event-macro.rkt")

;; NOTE (v0.29.16): auto-retry-event has 1 production emission site
;; (runtime/turn-orchestrator.rkt). It is wired — not deferred.

(define-typed-event auto-retry-event "auto-retry" (attempt max-attempts error-type) #:schema-version 1 #:no-serialize)

;; v0.44.3 (R2): Detailed auto-retry start event — replaces 5-key hasheq payload
(define-typed-event auto-retry-start-event
                    "auto-retry.start"
                    (attempt max-retries delay-ms error error-type) #:schema-version 1)

(define-typed-event compaction-event "compaction" (reason tokens-before tokens-after) #:schema-version 1 #:no-serialize)

;; STABILITY: public — stable for extensions
(define-typed-event injection-event
                    "injection"
                    (source content-type content-length)
                    #:optional ([message #f])
                    #:schema-version 1 #:no-serialize)

;; v0.44.3 (R2): Iteration decision event — replaces 5-key hasheq payload
;; Wire-format note (v0.44.4): Old hasheq used underscore keys (consecutive_tools,
;; max_iterations, max_iterations_hard). Struct serialization uses camelCase
;; (consecutiveTools, maxIterations, maxIterationsHard). Session trace files
;; written before v0.44.3 will have different key formats.
(define-typed-event iteration-decision-event
                    "iteration.decision"
                    (iteration termination consecutive-tools max-iterations max-iterations-hard) #:schema-version 1)


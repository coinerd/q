#lang racket/base

;; agent/event-structs/iteration-events.rkt — iteration lifecycle events
;; (auto-retry, compaction, injection)

(require "base.rkt")

;; Auto-retry events
(provide (struct-out auto-retry-event)
         make-auto-retry-event
         auto-retry-event?

         ;; Compaction events
         (struct-out compaction-event)
         make-compaction-event
         compaction-event?

         ;; Injection events
         (struct-out injection-event)
         make-injection-event
         injection-event?)

;; ============================================================
;; Auto-retry events
;; ============================================================
;;
;; NOTE (v0.29.16): auto-retry-event has 1 production emission site
;; (runtime/turn-orchestrator.rkt). It is wired — not deferred.

(struct auto-retry-event typed-event (attempt max-attempts error-type) #:transparent)

(define (make-auto-retry-event #:session-id session-id
                               #:turn-id turn-id
                               #:timestamp timestamp
                               #:attempt attempt
                               #:max-attempts max-attempts
                               #:error-type error-type)
  (auto-retry-event "auto-retry" timestamp session-id turn-id attempt max-attempts error-type))

;; ============================================================
;; Compaction events
;; ============================================================

(struct compaction-event typed-event (reason tokens-before tokens-after) #:transparent)

(define (make-compaction-event #:session-id session-id
                               #:turn-id turn-id
                               #:timestamp timestamp
                               #:reason reason
                               #:tokens-before tokens-before
                               #:tokens-after tokens-after)
  (compaction-event "compaction" timestamp session-id turn-id reason tokens-before tokens-after))

;; ============================================================
;; Injection events
;; ============================================================

(struct injection-event typed-event (source content-type content-length message) #:transparent)

(define (make-injection-event #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp timestamp
                              #:source source
                              #:content-type content-type
                              #:content-length content-length
                              #:message [message #f])
  (injection-event "injection"
                   timestamp
                   session-id
                   turn-id
                   source
                   content-type
                   content-length
                   message))

#lang racket/base

;; BOUNDARY: integration

;; tests/test-event-structs-facade.rkt — Verify event-structs facade completeness
;;
;; v0.32.9 M-4: Ensures the event-structs.rkt facade re-exports all sub-modules,
;; including stream-events.rkt which was previously missing.

(require rackunit
         "../agent/event-structs.rkt")

;; Verify stream events are accessible through the facade
(test-case "stream-events accessible through facade"
  (check-pred
   stream-completed-event?
   (make-stream-completed-event #:session-id #f #:turn-id #f #:usage (hasheq) #:finish_reason "stop"))
  (check-pred stream-delta-event?
              (make-stream-delta-event #:session-id #f #:turn-id #f #:delta "hello"))
  (check-pred stream-thinking-event?
              (make-stream-thinking-event #:session-id #f #:turn-id #f #:delta "thinking..."))
  (check-pred stream-message-start-event?
              (make-stream-message-start-event #:session-id #f #:turn-id #f #:message-id "m1"))
  (check-pred
   stream-message-delta-event?
   (make-stream-message-delta-event #:session-id #f #:turn-id #f #:text "delta" #:message-id "m1")))

(test-case "existing events still accessible through facade"
  ;; turn-events
  (check-pred turn-start-event?
              (make-turn-start-event #:session-id #f #:turn-id #f #:model "m" #:provider "p"))
  ;; message-events
  (check-pred message-start-event?
              (make-message-start-event #:session-id #f #:turn-id #f #:role "user" #:model "m"))
  ;; tool-events
  (check-pred tool-execution-start-event?
              (make-tool-execution-start-event #:session-id #f
                                               #:turn-id #f
                                               #:tool-name "read"
                                               #:tool-call-id "tc1"))
  ;; provider-events
  (check-pred provider-request-event?
              (make-provider-request-event #:session-id #f #:turn-id #f #:model "m" #:provider "p"))
  ;; session-events
  (check-pred session-start-event?
              (make-session-start-event #:session-id "s1" #:turn-id #f #:model "m"))
  ;; iteration-events
  (check-pred auto-retry-event?
              (make-auto-retry-event #:session-id #f
                                     #:turn-id #f
                                     #:attempt 1
                                     #:error-type "rate"
                                     #:max-attempts 3))
  ;; hook-events
  (check-pred model-request-blocked-event?
              (make-model-request-blocked-event #:session-id #f #:turn-id #f #:reason "blocked")))

(test-case "stream-events fields are correct"
  (define evt (make-stream-delta-event #:session-id "s1" #:turn-id "t1" #:delta "hello world"))
  (check-equal? (typed-event-type evt) "model.stream.delta")
  (check-equal? (stream-delta-event-delta evt) "hello world")
  (check-equal? (typed-event-session-id evt) "s1"))

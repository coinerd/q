#lang racket/base

;; tests/test-event-structs-v2.rkt — Round-trip tests for v0.30.6 new event structs
;;
;; Tests that all new typed-event structs (streaming + blocked + cancelled)
;; round-trip correctly through JSON serialization.

(require rackunit
         racket/match
         "../agent/event-structs.rkt"
         "../agent/event-json.rkt")

;; ============================================================
;; Helper
;; ============================================================

(define (round-trip evt)
  (define jsexpr (typed-event->jsexpr evt))
  (jsexpr->typed-event jsexpr))

(define ts 1700000000.0)
(define sid "test-session")
(define tid "test-turn")

;; ============================================================
;; Streaming events
;; ============================================================

(test-case "model-stream-delta-event round-trip"
  (define evt
    (make-model-stream-delta-event #:session-id sid
                                   #:turn-id tid
                                   #:timestamp ts
                                   #:delta "hello"
                                   #:model "gpt-4"))
  (define rt (round-trip evt))
  (check-pred model-stream-delta-event? rt)
  (check-equal? (model-stream-delta-event-delta rt) "hello")
  (check-equal? (model-stream-delta-event-model rt) "gpt-4"))

(test-case "model-stream-thinking-event round-trip"
  (define evt
    (make-model-stream-thinking-event #:session-id sid
                                      #:turn-id tid
                                      #:timestamp ts
                                      #:thinking "pondering"
                                      #:model "claude"))
  (define rt (round-trip evt))
  (check-pred model-stream-thinking-event? rt)
  (check-equal? (model-stream-thinking-event-thinking rt) "pondering")
  (check-equal? (model-stream-thinking-event-model rt) "claude"))

(test-case "model-stream-completed-event round-trip"
  (define evt
    (make-model-stream-completed-event #:session-id sid
                                       #:turn-id tid
                                       #:timestamp ts
                                       #:model "gpt-4"
                                       #:provider "openai"))
  (define rt (round-trip evt))
  (check-pred model-stream-completed-event? rt)
  (check-equal? (model-stream-completed-event-model rt) "gpt-4")
  (check-equal? (model-stream-completed-event-provider rt) "openai"))

;; ============================================================
;; Blocked events
;; ============================================================

(test-case "model-request-blocked-event round-trip"
  (define evt
    (make-model-request-blocked-event #:session-id sid #:turn-id tid #:timestamp ts #:reason "hook"))
  (define rt (round-trip evt))
  (check-pred model-request-blocked-event? rt)
  (check-equal? (model-request-blocked-event-reason rt) "hook"))

(test-case "message-blocked-event round-trip"
  (define evt
    (make-message-blocked-event #:session-id sid
                                #:turn-id tid
                                #:timestamp ts
                                #:hook "message-start"
                                #:reason "blocked"))
  (define rt (round-trip evt))
  (check-pred message-blocked-event? rt)
  (check-equal? (message-blocked-event-hook rt) "message-start")
  (check-equal? (message-blocked-event-reason rt) "blocked"))

(test-case "turn-cancelled-event round-trip"
  (define evt (make-turn-cancelled-event #:session-id sid #:turn-id tid #:reason "user-cancel"))
  (define rt (round-trip evt))
  (check-pred turn-cancelled-event? rt)
  (check-equal? (turn-cancelled-event-reason rt) "user-cancel"))

(test-case "assistant-message-completed-event round-trip"
  (define evt
    (make-assistant-message-completed-event #:session-id sid
                                            #:turn-id tid
                                            #:timestamp ts
                                            #:content-length 42))
  (define rt (round-trip evt))
  (check-pred assistant-message-completed-event? rt)
  (check-equal? (assistant-message-completed-event-content-length rt) 42))

;; ============================================================
;; W-01: Provider-stream vs iteration-stream type string disambiguation
;; ============================================================

(test-case "model-stream-delta-event has distinct type from stream-delta-event"
  (check-not-equal?
   (typed-event-type
    (make-model-stream-delta-event #:session-id #f #:turn-id #f #:delta "" #:model ""))
   (typed-event-type (make-stream-delta-event #:session-id #f #:turn-id #f #:delta ""))))

(test-case "model-stream-thinking-event has distinct type from stream-thinking-event"
  (check-not-equal?
   (typed-event-type
    (make-model-stream-thinking-event #:session-id #f #:turn-id #f #:thinking "" #:model ""))
   (typed-event-type (make-stream-thinking-event #:session-id #f #:turn-id #f #:delta ""))))

(test-case "model-stream-completed-event has distinct type from stream-completed-event"
  (check-not-equal?
   (typed-event-type
    (make-model-stream-completed-event #:session-id #f #:turn-id #f #:model "" #:provider ""))
   (typed-event-type
    (make-stream-completed-event #:session-id #f #:turn-id #f #:usage (hasheq) #:finish_reason ""))))

(test-case "provider-stream events use provider.stream prefix"
  (check-equal? (typed-event-type
                 (make-model-stream-delta-event #:session-id #f #:turn-id #f #:delta "" #:model ""))
                "provider.stream.delta")
  (check-equal?
   (typed-event-type
    (make-model-stream-thinking-event #:session-id #f #:turn-id #f #:thinking "" #:model ""))
   "provider.stream.thinking")
  (check-equal?
   (typed-event-type
    (make-model-stream-completed-event #:session-id #f #:turn-id #f #:model "" #:provider ""))
   "provider.stream.completed"))

(test-case "iteration-stream events keep model.stream prefix"
  (check-equal? (typed-event-type (make-stream-delta-event #:session-id #f #:turn-id #f #:delta ""))
                "model.stream.delta")
  (check-equal?
   (typed-event-type (make-stream-thinking-event #:session-id #f #:turn-id #f #:delta ""))
   "model.stream.thinking")
  (check-equal?
   (typed-event-type
    (make-stream-completed-event #:session-id #f #:turn-id #f #:usage (hasheq) #:finish_reason ""))
   "model.stream.completed"))

;; ============================================================
;; Registry check
;; ============================================================

(test-case "all-known-event-types includes new types"
  (define types (all-known-event-types))
  (for ([t '("provider.stream.delta" "provider.stream.thinking"
                                     "provider.stream.completed"
                                     "model.request.blocked"
                                     "message.blocked"
                                     "turn.cancelled"
                                     "assistant.message.completed")])
    (check-not-false (member t types) (format "~a not in registry" t))))

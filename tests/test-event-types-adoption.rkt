#lang racket/base

;; tests/test-event-types-adoption.rkt
;;
;; Verify typed event bridge (EVT-01) and adoption (EVT-08):
;;   1. bus-emit-typed! converts typed events to raw events
;;   2. typed-event->event preserves all fields
;;   3. JSON round-trip works for typed events
;;   4. Backward compatibility — raw events still work

(require rackunit
         "../agent/event-bus.rkt"
         "../agent/event-types.rkt"
         "../util/protocol-types.rkt")

(define TS 1000)

;; ============================================================
;; Test: typed-event->event conversion
;; ============================================================

(test-case "typed-event->event preserves type as event name"
  (define te (make-turn-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp TS
                                     #:model "gpt-4"
                                     #:provider "openai"))
  (define evt (typed-event->event te))
  (check-equal? (event-ev evt) "turn-start")
  (check-equal? (event-session-id evt) "s1")
  (check-equal? (event-turn-id evt) "t1"))

(test-case "typed-event->event packs extra fields into payload"
  (define te (make-turn-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp TS
                                     #:model "glm-5"
                                     #:provider "anthropic"))
  (define evt (typed-event->event te))
  (define payload (event-payload evt))
  (check-equal? (hash-ref payload 'model #f) "glm-5")
  (check-equal? (hash-ref payload 'provider #f) "anthropic"))

(test-case "typed-event->event strips base fields from payload"
  (define te (make-turn-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:timestamp TS
                                     #:model "test"
                                     #:provider "test"))
  (define evt (typed-event->event te))
  (define payload (event-payload evt))
  (check-false (hash-has-key? payload 'type))
  (check-false (hash-has-key? payload 'timestamp))
  (check-false (hash-has-key? payload 'sessionId))
  (check-false (hash-has-key? payload 'turnId)))

;; ============================================================
;; Test: bus-emit-typed! publishes on bus
;; ============================================================

(test-case "bus-emit-typed! delivers to subscribers"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (define te (make-session-start-event #:session-id "s1"
                                        #:turn-id #f
                                        #:timestamp TS
                                        #:model "test"))
  (bus-emit-typed! bus te)
  (check-pred event? (unbox received))
  (check-equal? (event-ev (unbox received)) "session-start"))

;; ============================================================
;; Test: JSON round-trip for typed events
;; ============================================================

(test-case "typed-event JSON round-trip preserves type"
  (define te (make-message-start-event #:session-id "s1"
                                        #:turn-id "t1"
                                        #:timestamp TS
                                        #:role "assistant"
                                        #:model "gpt-4"))
  (define h (typed-event->jsexpr te))
  (check-equal? (hash-ref h 'type) "message-start")
  (check-equal? (hash-ref h 'role) "assistant")
  ;; Round-trip
  (define te2 (jsexpr->typed-event h))
  (check-pred message-start-event? te2)
  (check-equal? (message-start-event-role te2) "assistant"))

(test-case "all known event types deserialize"
  (for ([type (in-list (all-known-event-types))])
    (define h (hasheq 'type type
                      'timestamp TS
                      'sessionId "s1"
                      'turnId "t1"))
    (define te (jsexpr->typed-event h))
    (check-pred typed-event? te (format "type ~a should deserialize" type))))

;; ============================================================
;; Test: backward compatibility — raw events still work
;; ============================================================

(test-case "raw event JSON round-trip unchanged"
  (define evt (make-event "turn.started" 1000 "s1" "t1" (hasheq 'turnId "t1")))
  (define h (event->jsexpr evt))
  (define evt2 (jsexpr->event h))
  (check-equal? (event-ev evt2) "turn.started")
  (check-equal? (event-session-id evt2) "s1"))

(test-case "bus handles both raw and typed events"
  (define bus (make-event-bus))
  (define raw-count (box 0))
  (subscribe! bus
              (lambda (evt)
                (set-box! raw-count (add1 (unbox raw-count)))))
  ;; Raw emit
  (publish! bus (make-event "test.raw" 1000 "s1" #f (hasheq)))
  ;; Typed emit
  (define te (make-turn-end-event #:session-id "s1"
                                   #:turn-id "t1"
                                   #:timestamp TS
                                   #:reason "done"
                                   #:duration-ms 100))
  (bus-emit-typed! bus te)
  (check-equal? (unbox raw-count) 2))

;; ============================================================
;; Test: various typed event constructors produce valid structs
;; ============================================================

(test-case "tool-execution typed events"
  (define te (make-tool-execution-start-event #:session-id "s1"
                                               #:turn-id "t1"
                                               #:timestamp TS
                                               #:tool-name "bash"
                                               #:tool-call-id "tc1"))
  (check-pred tool-execution-start-event? te)
  (check-equal? (tool-execution-start-event-tool-name te) "bash")
  (define evt (typed-event->event te))
  (check-equal? (hash-ref (event-payload evt) 'toolName) "bash"))

(test-case "session lifecycle typed events"
  (define te (make-session-shutdown-event #:session-id "s1"
                                           #:turn-id #f
                                           #:timestamp TS
                                           #:reason "user"))
  (check-pred session-shutdown-event? te)
  (define h (typed-event->jsexpr te))
  (check-equal? (hash-ref h 'type) "session-shutdown")
  (check-equal? (hash-ref h 'reason) "user"))

(test-case "provider typed events"
  (define te (make-provider-request-event #:session-id "s1"
                                           #:turn-id "t1"
                                           #:timestamp TS
                                           #:model "gpt-4"
                                           #:provider "openai"))
  (check-pred provider-request-event? te)
  (define h (typed-event->jsexpr te))
  (define te2 (jsexpr->typed-event h))
  (check-pred provider-request-event? te2)
  (check-equal? (provider-request-event-model te2) "gpt-4"))

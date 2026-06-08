#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: serialization

;; BOUNDARY: unit

;; tests/test-typed-event-codec-roundtrip.rkt -- B10-04: Codec round-trip tests

(require rackunit
         rackunit/text-ui
         (only-in "../util/event/event-macro.rkt"
                  lookup-event-serializer
                  lookup-event-deserializer
                  current-event-serializer-registry
                  current-event-deserializer-registry)
         (only-in "../agent/event-structs/base.rkt"
                  typed-event-type typed-event-timestamp
                  typed-event-session-id typed-event-turn-id)
         ;; Import all event modules so serializers register
         "../agent/event-structs/hook-events.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "../agent/event-structs/message-events.rkt"
         "../agent/event-structs/provider-events.rkt"
         "../agent/event-structs/session-events.rkt"
         "../agent/event-structs/stream-events.rkt"
         "../agent/event-structs/tool-events.rkt"
         "../agent/event-structs/turn-events.rkt"
         (only-in "../agent/event-structs/tool-events.rkt"
                  tool-execution-start-event-tool-name
                  tool-execution-start-event-tool-call-id)
         (only-in "../agent/event-structs/provider-events.rkt"
                  provider-request-event-model
                  provider-request-event-provider)
         (only-in "../agent/event-structs/stream-events.rkt"
                  stream-delta-event-delta)
         (only-in "../agent/event-structs/turn-events.rkt"
                  turn-end-event-reason)
         (only-in "../agent/event-structs/hook-events.rkt"
                  message-blocked-event-hook
                  message-blocked-event-reason))

(define codec-suite
  (test-suite "typed event codec round-trip"

    (test-case "serializer registry has entries"
      (define reg (current-event-serializer-registry))
      (check > (hash-count reg) 30 "should have 30+ serializer entries"))

    (test-case "deserializer registry has entries"
      (define reg (current-event-deserializer-registry))
      (check > (hash-count reg) 30 "should have 30+ deserializer entries"))

    (test-case "turn-start-event round-trips"
      (define evt (make-turn-start-event #:session-id "s1" #:turn-id "t1"
                                          #:timestamp 1000.0 #:model "gpt-4"
                                          #:provider "openai"))
      (define ser (lookup-event-serializer "turn.started"))
      (define des (lookup-event-deserializer "turn.started"))
      (check-not-false ser "serializer should exist")
      (check-not-false des "deserializer should exist")
      (define h (ser evt))
      (define decoded (des "turn.started" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "turn.started")
      (check-equal? (typed-event-session-id decoded) "s1")
      (check-equal? (typed-event-turn-id decoded) "t1"))

    (test-case "session-start-event round-trips"
      (define evt (make-session-start-event #:session-id "s1" #:turn-id "t1"
                                            #:timestamp 1000.0 #:model "gpt-4"))
      (define ser (lookup-event-serializer "session.started"))
      (define des (lookup-event-deserializer "session.started"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "session.started" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "session.started")
      (check-equal? (typed-event-session-id decoded) "s1"))

    (test-case "tool-execution-start-event round-trips"
      (define evt (make-tool-execution-start-event #:session-id "s1" #:turn-id "t1"
                                                    #:timestamp 1000.0
                                                    #:tool-name "bash"
                                                    #:tool-call-id "tc1"))
      (define ser (lookup-event-serializer "tool.execution.started"))
      (define des (lookup-event-deserializer "tool.execution.started"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "tool.execution.started" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "tool.execution.started")
      (check-equal? (tool-execution-start-event-tool-name decoded) "bash")
      (check-equal? (tool-execution-start-event-tool-call-id decoded) "tc1"))

    (test-case "provider-request-event round-trips"
      (define evt (make-provider-request-event #:session-id "s1" #:turn-id "t1"
                                               #:timestamp 1000.0
                                               #:model "gpt-4"
                                               #:provider "openai"))
      (define ser (lookup-event-serializer "model.request.started"))
      (define des (lookup-event-deserializer "model.request.started"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "model.request.started" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "model.request.started")
      (check-equal? (provider-request-event-model decoded) "gpt-4")
      (check-equal? (provider-request-event-provider decoded) "openai"))

    (test-case "stream-delta-event round-trips"
      (define evt (make-stream-delta-event #:session-id "s1" #:turn-id "t1"
                                           #:timestamp 1000.0
                                           #:delta "hello"))
      (define ser (lookup-event-serializer "model.stream.delta"))
      (define des (lookup-event-deserializer "model.stream.delta"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "model.stream.delta" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "model.stream.delta")
      (check-equal? (stream-delta-event-delta decoded) "hello"))

    (test-case "turn-end-event round-trips"
      (define evt (make-turn-end-event #:session-id "s1" #:turn-id "t1"
                                       #:timestamp 1000.0
                                       #:reason "completed"
                                       #:duration-ms 500.0))
      (define ser (lookup-event-serializer "turn.completed"))
      (define des (lookup-event-deserializer "turn.completed"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "turn.completed" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "turn.completed")
      (check-equal? (turn-end-event-reason decoded) "completed"))

    (test-case "message-blocked-event round-trips"
      (define evt (make-message-blocked-event #:session-id "s1" #:turn-id "t1"
                                              #:timestamp 1000.0
                                              #:hook "message-start"
                                              #:reason "blocked"))
      (define ser (lookup-event-serializer "message.blocked"))
      (define des (lookup-event-deserializer "message.blocked"))
      (check-not-false ser)
      (check-not-false des)
      (define h (ser evt))
      (define decoded (des "message.blocked" 1000.0 "s1" "t1" h))
      (check-equal? (typed-event-type decoded) "message.blocked")
      (check-equal? (message-blocked-event-hook decoded) "message-start")
      (check-equal? (message-blocked-event-reason decoded) "blocked"))))

(run-tests codec-suite 'verbose)

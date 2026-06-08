#lang racket/base

;; @speed fast
;; @suite default

;; tests/helpers/test-events.rkt — Shared event builder functions
;;
;; Provides convenient constructors for building events in tests.
;; Replaces duplicated ad-hoc event construction across test files.

(require "../../util/event/event.rkt"
         "../../agent/event-structs/stream-events.rkt"
         "../../agent/event-structs/session-events.rkt"
         "../../agent/event-structs/turn-events.rkt"
         "../../agent/event-structs/tool-events.rkt"
         "../../agent/event-structs/message-events.rkt"
         "../../agent/event-structs/iteration-events.rkt"
         "../../agent/event-structs/provider-events.rkt")

(provide make-test-event
         make-test-stream-chunk-event
         make-test-session-start-event
         make-test-turn-start-event
         make-test-tool-start-event
         make-test-message-start-event
         make-test-provider-request-event
         make-test-iteration-start-event)

;; make-test-event : string? any? [#:time real?] [#:session-id string?] [#:turn-id string?] -> event?
;;  General-purpose event builder. Wraps util/event.rkt make-event.
(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [session-id "test-session"]
                         #:turn-id [turn-id "turn-1"])
  (make-event ev-type time session-id turn-id payload))

;; make-test-stream-chunk-event : produces a stream.chunk event
(define (make-test-stream-chunk-event #:content [content "hello"]
                                      #:session-id [sid "test-session"]
                                      #:turn-id [tid "turn-1"])
  (make-event "stream.chunk" 1000 sid tid (hasheq 'content content)))

;; make-test-session-start-event : produces a session.started event
(define (make-test-session-start-event #:session-id [sid "test-session"])
  (make-event "session.started" 1000 sid #f (hasheq 'session-id sid)))

;; make-test-turn-start-event : produces a turn.started event
(define (make-test-turn-start-event #:session-id [sid "test-session"]
                                    #:turn-id [tid "turn-1"]
                                    #:role [role "user"])
  (make-event "turn.started" 1000 sid tid (hasheq 'role role)))

;; make-test-tool-start-event : produces a tool.execution.started event
(define (make-test-tool-start-event #:tool-name [tool-name "bash"]
                                   #:session-id [sid "test-session"]
                                   #:turn-id [tid "turn-1"])
  (make-event "tool.execution.started" 1000 sid tid (hasheq 'tool-name tool-name)))

;; make-test-message-start-event : produces a message.started event
(define (make-test-message-start-event #:session-id [sid "test-session"]
                                       #:turn-id [tid "turn-1"]
                                       #:role [role "assistant"])
  (make-event "message.started" 1000 sid tid (hasheq 'role role)))

;; make-test-provider-request-event : produces a provider.request event
(define (make-test-provider-request-event #:session-id [sid "test-session"]
                                          #:turn-id [tid "turn-1"]
                                          #:model [model "test-model"])
  (make-event "provider.request" 1000 sid tid (hasheq 'model model)))

;; make-test-iteration-start-event : produces an iteration.started event
(define (make-test-iteration-start-event #:session-id [sid "test-session"]
                                         #:turn-id [tid "turn-1"])
  (make-event "iteration.started" 1000 sid tid (hasheq)))

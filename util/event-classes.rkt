#lang racket/base

;; util/event-classes.rkt — FEAT-77: Typed event classes for SDK/RPC
;;
;; Provides typed constructors and predicates for common event types.
;; Each constructor wraps make-event with the correct topic and payload shape.
;; Each predicate checks the event topic.

(require racket/contract
         "protocol-types.rkt")

(provide
 ;; Event class predicates
 stream-text-event?
 stream-tool-call-event?
 stream-thinking-event?
 tool-start-event?
 tool-end-event?
 iteration-start-event?
 iteration-end-event?
 interrupt-event?
 error-event?
 compaction-started-event?
 compaction-completed-event?

 ;; Event class constructors
 make-stream-text-event
 make-stream-tool-call-event
 make-stream-thinking-event
 make-tool-start-event
 make-tool-end-event
 make-iteration-start-event
 make-iteration-end-event
 make-interrupt-event
 make-error-event
 make-compaction-started-event
 make-compaction-completed-event

 ;; Event topic constants
 TOPIC-STREAM-TEXT
 TOPIC-STREAM-TOOL-CALL
 TOPIC-STREAM-THINKING
 TOPIC-TOOL-START
 TOPIC-TOOL-END
 TOPIC-ITERATION-START
 TOPIC-ITERATION-END
 TOPIC-INTERRUPT
 TOPIC-ERROR
 TOPIC-COMPACTION-STARTED
 TOPIC-COMPACTION-COMPLETED)

;; ============================================================
;; Topic constants
;; ============================================================

(define TOPIC-STREAM-TEXT "model.stream.text")
(define TOPIC-STREAM-TOOL-CALL "model.stream.tool_call")
(define TOPIC-STREAM-THINKING "model.stream.thinking")
(define TOPIC-TOOL-START "tool.execution.start")
(define TOPIC-TOOL-END "tool.execution.end")
(define TOPIC-ITERATION-START "iteration.started")
(define TOPIC-ITERATION-END "iteration.completed")
(define TOPIC-INTERRUPT "interrupt.requested")
(define TOPIC-ERROR "error")
(define TOPIC-COMPACTION-STARTED "compaction.started")
(define TOPIC-COMPACTION-COMPLETED "compaction.completed")

;; ============================================================
;; Helpers
;; ============================================================

(define (current-time-seconds)
  (inexact->exact (truncate (/ (current-inexact-milliseconds) 1000))))

(define (event-topic=? evt topic)
  (equal? (event-ev evt) topic))

;; ============================================================
;; Predicates
;; ============================================================

(define (stream-text-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-STREAM-TEXT)))

(define (stream-tool-call-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-STREAM-TOOL-CALL)))

(define (stream-thinking-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-STREAM-THINKING)))

(define (tool-start-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-TOOL-START)))

(define (tool-end-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-TOOL-END)))

(define (iteration-start-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-ITERATION-START)))

(define (iteration-end-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-ITERATION-END)))

(define (interrupt-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-INTERRUPT)))

(define (error-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-ERROR)))

(define (compaction-started-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-COMPACTION-STARTED)))

(define (compaction-completed-event? evt)
  (and (event? evt) (event-topic=? evt TOPIC-COMPACTION-COMPLETED)))

;; ============================================================
;; Constructors
;; ============================================================

(define (make-stream-text-event session-id payload #:turn-id [turn-id #f])
  (make-event TOPIC-STREAM-TEXT (current-time-seconds) session-id turn-id payload))

(define (make-stream-tool-call-event session-id payload #:turn-id [turn-id #f])
  (make-event TOPIC-STREAM-TOOL-CALL (current-time-seconds) session-id turn-id payload))

(define (make-stream-thinking-event session-id payload #:turn-id [turn-id #f])
  (make-event TOPIC-STREAM-THINKING (current-time-seconds) session-id turn-id payload))

(define (make-tool-start-event session-id tool-name tool-call-id)
  (make-event TOPIC-TOOL-START (current-time-seconds) session-id #f
              (hasheq 'tool-name tool-name 'tool-call-id tool-call-id)))

(define (make-tool-end-event session-id tool-name tool-call-id)
  (make-event TOPIC-TOOL-END (current-time-seconds) session-id #f
              (hasheq 'tool-name tool-name 'tool-call-id tool-call-id)))

(define (make-iteration-start-event session-id turn-id)
  (make-event TOPIC-ITERATION-START (current-time-seconds) session-id turn-id
              (hasheq 'sessionId session-id)))

(define (make-iteration-end-event session-id turn-id reason)
  (make-event TOPIC-ITERATION-END (current-time-seconds) session-id turn-id
              (hasheq 'sessionId session-id 'reason reason)))

(define (make-interrupt-event session-id)
  (make-event TOPIC-INTERRUPT (current-time-seconds) session-id #f
              (hasheq 'sessionId session-id)))

(define (make-error-event session-id message #:turn-id [turn-id #f])
  (make-event TOPIC-ERROR (current-time-seconds) session-id turn-id
              (hasheq 'message message)))

(define (make-compaction-started-event session-id #:persist? [persist? #f])
  (make-event TOPIC-COMPACTION-STARTED (current-time-seconds) session-id #f
              (hasheq 'sessionId session-id 'persist? persist?)))

(define (make-compaction-completed-event session-id removed-count #:persist? [persist? #f])
  (make-event TOPIC-COMPACTION-COMPLETED (current-time-seconds) session-id #f
              (hasheq 'sessionId session-id 'persist? persist? 'removedCount removed-count)))

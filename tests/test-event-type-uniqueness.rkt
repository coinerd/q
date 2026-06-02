#lang racket/base

;; tests/test-event-type-uniqueness.rkt — T1-3: Verify ALL event type strings are unique
;; STABILITY: evolving
;; v0.80.xx audit: Expanded from 10-sample to ALL event types after
;; "assistant.message.completed" collision was missed.

(require rackunit
         rackunit/text-ui
         "../agent/event-structs.rkt")

;; ── Collect ALL event type string constants ──
;; *-event-type are exported as string constants by define-typed-event.

(define all-type-strings
  (list
   ;; turn-events
   turn-start-event-type
   turn-end-event-type
   turn-cancelled-event-type
   ;; message-events
   message-start-event-type
   message-update-event-type
   message-end-event-type
   ;; tool-events
   tool-execution-start-event-type
   tool-execution-update-event-type
   tool-execution-end-event-type
   tool-call-event-type
   tool-result-event-type
   ;; provider-events
   provider-request-event-type
   provider-response-event-type
   model-stream-delta-event-type
   model-stream-thinking-event-type
   model-stream-completed-event-type
   ;; session-events
   session-start-event-type
   session-shutdown-event-type
   input-event-type
   model-select-event-type
   agent-start-event-type
   agent-end-event-type
   ;; context-events
   context-event-type
   context-assembled-event-type
   context-blocked-event-type
   working-set-injected-event-type
   context-assembly-detail-event-type
   ;; goal-events
   goal-start-event-type
   goal-turn-start-event-type
   goal-evaluated-event-type
   goal-check-event-type
   goal-achieved-event-type
   goal-failed-event-type
   ;; retry-events
   auto-retry-event-type
   auto-retry-start-event-type
   ;; compaction-events
   compaction-event-type
   injection-event-type
   ;; iteration-events
   iteration-decision-event-type
   model-request-blocked-event-type
   message-blocked-event-type
   ;; hook-events
   assistant-message-completed-event-type
   ;; stream-events
   stream-completed-event-type
   stream-delta-event-type
   stream-tool-call-delta-event-type
   stream-thinking-event-type
   stream-message-start-event-type
   stream-message-delta-event-type
   stream-message-end-event-type
   stream-turn-completed-event-type
   stream-turn-cancelled-event-type
   stream-tool-call-started-event-type
   stream-assistant-msg-completed-event-type))

;; ── Test Suite ──

(define suite
  (test-suite "Event Type Uniqueness (T1-3)"

    (test-case "all event type constants are strings"
      (for ([t (in-list all-type-strings)])
        (check-true (string? t) (format "event type should be a string, got: ~a" t))))

    (test-case "no duplicate event type strings among ALL events"
      (define seen (make-hash))
      (define duplicates '())
      (for ([t (in-list all-type-strings)])
        (when (hash-has-key? seen t)
          (set! duplicates (cons t duplicates)))
        (hash-set! seen t #t))
      (check-equal? duplicates '() (format "Duplicate event type strings: ~a" duplicates)))

    (test-case "stream-turn-completed uses stream.turn.completed"
      (check-equal? stream-turn-completed-event-type "stream.turn.completed"))

    (test-case "turn-end keeps canonical turn.completed"
      (check-equal? turn-end-event-type "turn.completed"))

    (test-case "stream-turn-completed differs from turn-end"
      (check-not-equal? stream-turn-completed-event-type turn-end-event-type))

    (test-case "stream-turn-cancelled uses stream.turn.cancelled"
      (check-equal? stream-turn-cancelled-event-type "stream.turn.cancelled"))

    (test-case "stream-turn-cancelled differs from turn-cancelled"
      (check-not-equal? stream-turn-cancelled-event-type turn-cancelled-event-type))

    (test-case "hook assistant-message differs from stream assistant-msg"
      (check-not-equal? assistant-message-completed-event-type
                         stream-assistant-msg-completed-event-type
                         (format "hook=~a stream=~a"
                                 assistant-message-completed-event-type
                                 stream-assistant-msg-completed-event-type)))))

(run-tests suite)

#lang racket

;; tests/test-event-classes.rkt — FEAT-77: Typed event classes

(require rackunit
         rackunit/text-ui
         "../util/event-classes.rkt"
         "../util/protocol-types.rkt")

(define event-class-tests
  (test-suite "typed event classes"

    ;; ============================================================
    ;; Stream events
    ;; ============================================================
    (test-case "stream-text-event predicate and constructor"
      (define evt (make-stream-text-event "s1" (hasheq 'text "hello")))
      (check-true (stream-text-event? evt))
      (check-false (tool-start-event? evt))
      (check-equal? (event-ev evt) "model.stream.text")
      (check-equal? (hash-ref (event-payload evt) 'text) "hello"))

    (test-case "stream-tool-call-event predicate"
      (define evt (make-stream-tool-call-event "s1" (hasheq 'name "bash")))
      (check-true (stream-tool-call-event? evt))
      (check-equal? (event-ev evt) "model.stream.tool_call"))

    (test-case "stream-thinking-event predicate"
      (define evt (make-stream-thinking-event "s1" (hasheq 'thinking "hmm")))
      (check-true (stream-thinking-event? evt))
      (check-equal? (event-ev evt) "model.stream.thinking"))

    ;; ============================================================
    ;; Tool lifecycle events
    ;; ============================================================
    (test-case "tool-start-event and tool-end-event"
      (define start (make-tool-start-event "s1" "bash" "tc-1"))
      (define end (make-tool-end-event "s1" "bash" "tc-1"))
      (check-true (tool-start-event? start))
      (check-true (tool-end-event? end))
      (check-false (tool-end-event? start))
      (check-equal? (hash-ref (event-payload start) 'tool-name) "bash")
      (check-equal? (hash-ref (event-payload end) 'tool-call-id) "tc-1"))

    ;; ============================================================
    ;; Iteration events
    ;; ============================================================
    (test-case "iteration-start-event and iteration-end-event"
      (define start (make-iteration-start-event "s1" "turn-1"))
      (define end (make-iteration-end-event "s1" "turn-1" "completed"))
      (check-true (iteration-start-event? start))
      (check-true (iteration-end-event? end))
      (check-equal? (hash-ref (event-payload end) 'reason) "completed"))

    ;; ============================================================
    ;; Interrupt and error events
    ;; ============================================================
    (test-case "interrupt-event"
      (define evt (make-interrupt-event "s1"))
      (check-true (interrupt-event? evt))
      (check-false (error-event? evt))
      (check-equal? (event-ev evt) "interrupt.requested"))

    (test-case "error-event"
      (define evt (make-error-event "s1" "something went wrong"))
      (check-true (error-event? evt))
      (check-equal? (hash-ref (event-payload evt) 'message) "something went wrong"))

    ;; ============================================================
    ;; Compaction events
    ;; ============================================================
    (test-case "compaction-started-event and compaction-completed-event"
      (define started (make-compaction-started-event "s1" #:persist? #t))
      (define completed (make-compaction-completed-event "s1" 15 #:persist? #t))
      (check-true (compaction-started-event? started))
      (check-true (compaction-completed-event? completed))
      (check-equal? (hash-ref (event-payload completed) 'removedCount) 15))

    ;; ============================================================
    ;; Topic constants
    ;; ============================================================
    (test-case "topic constants match event topics"
      (check-equal? TOPIC-STREAM-TEXT "model.stream.text")
      (check-equal? TOPIC-TOOL-START "tool.execution.start")
      (check-equal? TOPIC-TOOL-END "tool.execution.end")
      (check-equal? TOPIC-INTERRUPT "interrupt.requested")
      (check-equal? TOPIC-ERROR "error")
      (check-equal? TOPIC-COMPACTION-STARTED "compaction.started"))

    ;; ============================================================
    ;; Edge cases
    ;; ============================================================
    (test-case "stream-text-event with #:turn-id keyword"
      (define evt (make-stream-text-event "s1" (hasheq 'text "hi") #:turn-id "turn-42"))
      (check-true (stream-text-event? evt))
      (check-equal? (event-ev evt) "model.stream.text")
      (check-equal? (event-turn-id evt) "turn-42"))

    (test-case "predicates return #f for non-event values"
      (check-false (stream-text-event? "not an event"))
      (check-false (tool-start-event? 42))
      (check-false (error-event? #f)))

    (test-case "compaction-started-event defaults persist? to #f"
      (define evt (make-compaction-started-event "s1"))
      (check-true (compaction-started-event? evt))
      (check-equal? (hash-ref (event-payload evt) 'persist?) #f))

    ;; ============================================================
    ;; Serialization
    ;; ============================================================
    (test-case "event classes produce serializable events"
      (define evt (make-stream-text-event "s1" (hasheq 'text "test")))
      (define jsexpr (event->jsexpr evt))
      (check-equal? (hash-ref jsexpr 'event) "model.stream.text")
      (check-equal? (hash-ref jsexpr 'sessionId) "s1")
      (define restored (jsexpr->event jsexpr))
      (check-true (stream-text-event? restored)))))

(run-tests event-class-tests)

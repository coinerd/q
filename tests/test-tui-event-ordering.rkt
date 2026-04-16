#lang racket

;; tests/test-tui-event-ordering.rkt — Event ordering and state consistency
;; edge case tests (Wave 4, W4.2 #888).
;;
;; Tests out-of-order events, unknown events, rapid state transitions,
;; and compaction event handling.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt")

(define event-ordering-tests
  (test-suite
   "Event Ordering & State Consistency Tests"

   ;; EO1: Unknown event type is safely ignored
   (test-case "EO1: unknown event type ignored without error"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "unknown.event.type" (hash 'foo "bar")
        "assistant.message.completed" (hash 'content "Still works")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (check-equal? (entry-count state1) 1)
     (check-not-false (find-entry-by-text state1 "Still works"))
     (check-false (ui-state-busy? state1)))

   ;; EO2: Compaction events update state but don't corrupt transcript
   (test-case "EO2: compaction events update state cleanly"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "assistant.message.completed" (hash 'content "Before compaction")
        "compaction.warning" (hash 'tokenCount 80000)
        "compaction.started" (hash)
        "compaction.completed" (hash)
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     ;; Compaction warning creates a system entry; compaction start/end don't
     (check-true (>= (entry-count state1) 2))
     (check-not-false (find-entry-by-text state1 "Before compaction"))
     (check-not-false (find-entry-by-text state1 "compaction warning"))
     ;; Status message cleared after compaction.completed
     (check-false (ui-state-status-message state1))
     (check-false (ui-state-busy? state1)))

   ;; EO3: Rapid turn start/complete cycles maintain clean state
   (test-case "EO3: rapid turn cycles maintain clean state"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "assistant.message.completed" (hash 'content "Turn 1")
        "turn.completed" (hash)
        "turn.started" (hash)
        "assistant.message.completed" (hash 'content "Turn 2")
        "turn.completed" (hash)
        "turn.started" (hash)
        "assistant.message.completed" (hash 'content "Turn 3")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (check-equal? (entry-count state1) 3)
     (check-not-false (find-entry-by-text state1 "Turn 1"))
     (check-not-false (find-entry-by-text state1 "Turn 2"))
     (check-not-false (find-entry-by-text state1 "Turn 3"))
     (check-false (ui-state-busy? state1))
     (check-false (ui-state-streaming-text state1)))

   ;; EO4: Events without prior turn.started still produce entries
   (test-case "EO4: events without turn.started still produce entries"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        ;; No turn.started — events should still be processed
        "assistant.message.completed" (hash 'content "Orphan message")
        "tool.call.started" (hash 'name "bash" 'arguments "{\"cmd\":\"ls\"}")
        "tool.call.completed" (hash 'name "bash" 'result "file.txt")))
     (define state1 (apply-events state0 events))
     (check-equal? (entry-count state1) 3)
     (check-not-false (find-entry-by-text state1 "Orphan message"))
     (check-not-false (find-entry-by-text state1 "[TOOL: bash]"))
     (check-not-false (find-entry-by-text state1 "[OK: bash]")))))

(run-tests event-ordering-tests)

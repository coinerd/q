#lang racket

;; tests/test-streaming-transitions.rkt — Wave 2: Comprehensive TUI state transition tests
;;
;; Tests for all streaming -> tool state transitions in the TUI state machine.
;; Uses the event-simulator infrastructure for clean event sequence testing.

(require rackunit
         rackunit/text-ui
         "tui/event-simulator.rkt"
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

(define streaming-transition-tests
  (test-suite
   "TUI Streaming State Transitions"

   ;; Test 1: Streaming text committed when assistant.message.completed fires
   ;; before tool.call.started
   (test-case
    "streaming text committed when assistant.message.completed fires before tool.call.started"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Answer: "))
            (make-test-event "model.stream.delta" (hasheq 'delta "42"))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Answer: 42"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read"
                                     'arguments (hasheq 'path "/tmp/test.txt")))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 5))
    (check-equal? (transcript-types final) '(assistant tool-start)
                  "assistant + tool-start in transcript")
    (check-false (ui-state-streaming-text final)
                 "streaming-text cleared after assistant.message.completed")
    (check-equal? (transcript-entry-text (car (ui-state-transcript final)))
                  "Answer: 42"
                  "committed text matches streamed content"))

   ;; Test 2: Full text committed before tool calls in complete cycle
   (test-case
    "full text committed before tool calls in complete cycle"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Hello "))
            (make-test-event "model.stream.delta" (hasheq 'delta "world"))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Hello world"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read"
                                     'arguments (hasheq 'path "/tmp/x")))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "read" 'result "content"))
            (make-test-event "turn.completed"
                             (hasheq 'termination 'tool-calls-pending 'turnId "turn-1"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 7))
    (check-equal? (transcript-types final) '(assistant tool-start tool-end)
                  "full transcript: assistant, tool-start, tool-end")
    (check-false (ui-state-streaming-text final)
                 "streaming-text clear at end of tool-call turn")
    (check-false (ui-state-busy? final) "not busy after turn.completed"))

   ;; Test 3: Streaming text does NOT accumulate across turns
   (test-case
    "streaming text does NOT accumulate across turns"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Turn1"))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Turn1"))
            (make-test-event "turn.completed"
                             (hasheq 'termination 'completed 'turnId "turn-1"))
            (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")
            (make-test-event "model.stream.delta" (hasheq 'delta "Turn2")
                             #:turn-id "turn-2")))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 6))
    (check-equal? (ui-state-streaming-text final) "Turn2"
                  "no cross-turn accumulation")
    (check-equal? (transcript-length final) 1
                  "only one assistant entry from turn 1"))

   ;; Test 4: Full tool-use loop: answer + tool + follow-up answer
   (test-case
    "full tool-use loop: answer + tool + follow-up answer"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "CL answer"))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "CL answer"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read"
                                     'arguments (hasheq 'path "/tmp/x")))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "read" 'result "file data"))
            (make-test-event "turn.completed"
                             (hasheq 'termination 'tool-calls-pending 'turnId "turn-1"))
            (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")
            (make-test-event "model.stream.delta" (hasheq 'delta "Follow-up")
                             #:turn-id "turn-2")
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m2" 'content "Follow-up")
                             #:turn-id "turn-2")
            (make-test-event "turn.completed"
                             (hasheq 'termination 'completed 'turnId "turn-2"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 11))
    (check-equal? (transcript-types final)
                  '(assistant tool-start tool-end assistant)
                  "full loop: both assistant entries preserved with tool entries")
    (check-false (ui-state-streaming-text final)
                 "streaming-text clear after full loop")
    (check-false (ui-state-busy? final) "not busy after loop completes"))

   ;; Test 5: Tool calls without prior streaming (edge case)
   (test-case
    "tool calls without prior streaming"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content ""))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "bash"
                                     'arguments (hasheq 'command "ls")))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "bash" 'result "file1\nfile2"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 4))
    (check-equal? (transcript-types final) '(assistant tool-start tool-end)
                  "assistant (empty) + tool entries")
    (check-false (ui-state-streaming-text final) "no streaming text"))

   ;; Test 6: Cancellation mid-stream clears streaming text
   (test-case
    "cancellation mid-stream clears streaming text"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Partial answer..."))
            (make-test-event "turn.cancelled" (hasheq))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 3))
    (check-false (ui-state-streaming-text final)
                 "streaming-text cleared on cancel")
    (check-false (ui-state-busy? final) "not busy after cancel")
    (check-equal? (transcript-length final) 0
                  "no transcript entries for cancelled turn"))

   ;; Test 7: Multiple tool calls preserve all entries
   (test-case
    "multiple tool calls preserve all entries"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Reading files..."))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Reading files..."))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read"
                                     'arguments (hasheq 'path "/tmp/a")))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-2" 'name "bash"
                                     'arguments (hasheq 'command "ls")))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "read" 'result "content-a"))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "bash" 'result "file1"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 7))
    (check-equal? (transcript-length final) 5
                  "5 entries: 1 assistant + 2 tool-starts + 2 tool-ends")
    (check-equal? (transcript-types final)
                  '(assistant tool-start tool-start tool-end tool-end)
                  "all entry types correct"))

   ;; Test 8: Failed tool calls are recorded as tool-fail
   (test-case
    "failed tool calls are recorded as tool-fail"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Let me read that"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read"
                                     'arguments (hasheq 'path "/nonexistent")))
            (make-test-event "tool.call.failed"
                             (hasheq 'name "read" 'error "File not found"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 4))
    (check-equal? (transcript-types final) '(assistant tool-start tool-fail)
                  "last entry is tool-fail, not tool-end")
    (define fail-text (last (transcript-texts final)))
    (check-not-false (regexp-match #rx"\\[FAIL:" fail-text)
                     (format "fail entry has FAIL prefix, got: ~a" fail-text)))

   ;; ============================================================
   ;; Wave 4 edge case tests
   ;; ============================================================

   ;; Edge 1: Empty streaming text — no content deltas at all
   (test-case
    "empty streaming: no deltas, only completion"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content ""))
            (make-test-event "turn.completed"
                             (hasheq 'termination 'completed 'turnId "turn-1"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 3))
    (check-false (ui-state-streaming-text final) "no streaming text")
    (check-false (ui-state-busy? final) "not busy")
    (check-equal? (transcript-length final) 1 "one transcript entry for empty message"))

   ;; Edge 2: Concurrent tool calls — both started before either completes
   (test-case
    "concurrent tool calls: both started before either completes"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "Working..."))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read" 'arguments "/tmp/a"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-2" 'name "read" 'arguments "/tmp/b"))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "read" 'result "data-b"))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "read" 'result "data-a"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 6))
    (check-equal? (transcript-length final) 5
                  "5 entries: 1 assistant + 2 tool-starts + 2 tool-ends")
    (check-false (ui-state-busy? final) "not busy"))

   ;; Edge 3: Rapid events — delta+completed+tool-start in quick succession
   (test-case
    "rapid events: delta then completed then tool-start"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "X"))
            (make-test-event "assistant.message.completed"
                             (hasheq 'messageId "m1" 'content "X"))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "bash" 'arguments "echo hi"))
            (make-test-event "tool.call.completed"
                             (hasheq 'name "bash" 'result "hi"))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 5))
    (check-equal? (transcript-types final) '(assistant tool-start tool-end)
                  "rapid succession produces correct transcript")
    (check-false (ui-state-streaming-text final) "streaming cleared"))

   ;; Edge 4: Cancelled turn clears all state
   (test-case
    "cancelled turn clears streaming text and busy state"
    (define s0 (initial-ui-state))
    (define events
      (list (make-test-event "turn.started" (hasheq))
            (make-test-event "model.stream.delta" (hasheq 'delta "Partial..."))
            (make-test-event "tool.call.started"
                             (hasheq 'id "tc-1" 'name "read" 'arguments "/tmp/x"))
            (make-test-event "turn.cancelled" (hasheq))))
    (define states (simulate-and-record s0 events))
    (define final (state-at states 4))
    (check-false (ui-state-streaming-text final) "streaming cleared on cancel")
    (check-false (ui-state-busy? final) "not busy after cancel"))))

(module+ main
  (run-tests streaming-transition-tests))

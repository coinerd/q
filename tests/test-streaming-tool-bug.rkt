#lang racket

;; tests/test-streaming-tool-bug.rkt — Wave 0: Reproduction test for streaming content
;; disappearance when tool calls are present.
;;
;; Issue #798: When the LLM produces text + tool calls, assistant.message.completed
;; is never emitted, so streaming text stays in the temporary buffer and is not
;; committed to the permanent transcript.
;;
;; This test simulates the exact event sequence that triggers the bug at the
;; TUI state machine level using apply-event-to-state.

(require rackunit
         rackunit/text-ui
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; Helper: make a test event with defaults
(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [session-id "test-session"]
                         #:turn-id [turn-id "turn-1"])
  (event 1 ev-type time session-id turn-id payload))

;; Helper: get transcript entry types from a state
(define (transcript-types state)
  (map transcript-entry-kind (ui-state-transcript state)))

;; Helper: get transcript entry texts from a state
(define (transcript-texts state)
  (map transcript-entry-text (ui-state-transcript state)))

(define streaming-tool-bug-tests
  (test-suite
   "Streaming Tool Bug Reproduction"

   ;; ------------------------------------------------------------------
   ;; BUG REPRODUCTION: The core sequence that triggers the disappearance
   ;;
   ;; Real event sequence from agent loop when text + tool calls present:
   ;;   model.stream.delta("Champions League answer...")
   ;;   tool.call.started(read)
   ;;   tool.call.completed(read)
   ;;   turn.completed(tool-calls-pending)
   ;;
   ;; NOTE: assistant.message.completed is MISSING from this sequence —
   ;; that's Bug B1. The TUI never commits streaming text to transcript.
   ;; ------------------------------------------------------------------

   (test-case
    "BUG: streaming text NOT committed when tool.call.started follows (no assistant.message.completed)"
    ;; This test FAILS before the fix, demonstrating the bug.
    ;; After Wave 1 fix (adding assistant.message.completed to loop.rkt),
    ;; the agent loop will emit the event and this scenario won't occur.
    ;; But at the TUI state level, this test verifies the raw behavior.
    (define s0 (initial-ui-state))

    ;; Turn starts
    (define s1 (apply-event-to-state s0 (make-test-event "turn.started" (hasheq))))
    (check-true (ui-state-busy? s1) "turn.started → busy")

    ;; Model streams text
    (define s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hasheq 'delta "The answer is "))))
    (check-equal? (ui-state-streaming-text s2) "The answer is " "delta accumulated in streaming-text")

    (define s3 (apply-event-to-state s2 (make-test-event "model.stream.delta" (hasheq 'delta "42."))))
    (check-equal? (ui-state-streaming-text s3) "The answer is 42." "second delta appended")

    ;; Tool call starts — WITHOUT assistant.message.completed being emitted first
    (define s4 (apply-event-to-state s3 (make-test-event "tool.call.started"
                                                          (hasheq 'id "tc-1"
                                                                  'name "read"
                                                                  'arguments (hasheq 'path "/tmp/test.txt")))))
    (check-true (ui-state-busy? s4) "tool.call.started → busy")
    (check-equal? (transcript-types s4) '(tool-start) "only tool-start in transcript")

    ;; KEY BUG ASSERTION: streaming-text still has the answer text!
    ;; In the real system, no assistant.message.completed is emitted before
    ;; tool.call.started, so streaming-text is never committed to transcript.
    ;; This means the answer text lives only in the temporary streaming-text buffer.
    (check-equal? (ui-state-streaming-text s4) "The answer is 42."
                   "BUG PRESENT: streaming-text persists after tool.call.started (not committed)")

    ;; The answer text is NOT in the transcript — it's only in streaming-text
    (check-false (member "The answer is 42." (transcript-texts s4))
                 "BUG PRESENT: answer text is NOT in permanent transcript")

    ;; Tool completes
    (define s5 (apply-event-to-state s4 (make-test-event "tool.call.completed"
                                                          (hasheq 'name "read"
                                                                  'result "file contents"))))
    (check-equal? (transcript-types s5) '(tool-start tool-end))

    ;; Turn completes
    (define s6 (apply-event-to-state s5 (make-event "turn.completed"
                                                     1005
                                                     "test-session"
                                                     "turn-1"
                                                     (hasheq 'termination 'tool-calls-pending
                                                             'turnId "turn-1"))))
    (check-false (ui-state-busy? s6) "turn.completed → not busy")

    ;; BUG ASSERTION: streaming-text is STILL not cleared on turn.completed
    ;; (Bug B2 — turn.completed doesn't clear streaming-text)
    ;; The answer "The answer is 42." is lost from display on next turn
    (check-equal? (ui-state-streaming-text s6) "The answer is 42."
                   "BUG B2 PRESENT: streaming-text persists after turn.completed"))

   ;; ------------------------------------------------------------------
   ;; CORRECT BEHAVIOR: When assistant.message.completed IS emitted,
   ;; text is properly committed
   ;; ------------------------------------------------------------------

   (test-case
    "CORRECT: streaming text committed when assistant.message.completed fires before tool calls"
    (define s0 (initial-ui-state))

    (define s1 (apply-event-to-state s0 (make-test-event "turn.started" (hasheq))))
    (define s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hasheq 'delta "Hello "))))
    (define s3 (apply-event-to-state s2 (make-test-event "model.stream.delta" (hasheq 'delta "world."))))

    ;; The FIX: emit assistant.message.completed before tool.call.started
    (define s4 (apply-event-to-state s3 (make-test-event "assistant.message.completed"
                                                          (hasheq 'messageId "msg-1"
                                                                  'content "Hello world."))))
    ;; After assistant.message.completed: text is committed, streaming cleared
    (check-false (ui-state-streaming-text s4) "streaming-text cleared after commit")
    (check-equal? (transcript-types s4) '(assistant) "assistant entry in transcript")
    (check-equal? (transcript-entry-text (car (ui-state-transcript s4))) "Hello world."
                  "committed text matches streamed content")

    ;; Now tool calls start — answer is safe in transcript
    (define s5 (apply-event-to-state s4 (make-test-event "tool.call.started"
                                                          (hasheq 'id "tc-1"
                                                                  'name "read"
                                                                  'arguments (hasheq 'path "/tmp/x.txt")))))
    (check-equal? (transcript-types s5) '(assistant tool-start)
                  "assistant entry preserved when tool starts")
    (check-false (ui-state-streaming-text s5) "streaming-text remains clear"))

   ;; ------------------------------------------------------------------
   ;; CROSS-TURN ACCUMULATION BUG
   ;; ------------------------------------------------------------------

   (test-case
    "BUG: streaming text accumulates across turns (turn.completed doesn't clear it)"
    ;; Bug B2: turn.completed sets busy?=#f but doesn't clear streaming-text
    ;; Next turn's deltas concatenate onto leftover streaming-text
    (define s0 (initial-ui-state))

    ;; Turn 1: stream + complete normally
    (define s1 (apply-event-to-state s0 (make-test-event "turn.started" (hasheq))))
    (define s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hasheq 'delta "Turn1 "))))
    (define s3 (apply-event-to-state s2 (make-test-event "assistant.message.completed"
                                                          (hasheq 'content "Turn1 text"
                                                                  'messageId "m1"))))

    ;; Turn 1 completes — streaming-text should be cleared
    (define s4 (apply-event-to-state s3 (make-test-event "turn.completed"
                                                          (hasheq 'termination 'completed
                                                                 'turnId "turn-1"))))
    (check-false (ui-state-streaming-text s4)
                 "streaming-text cleared after turn.completed following normal completion")

    ;; Turn 2: stream new text
    (define s5 (apply-event-to-state s4 (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")))
    (define s6 (apply-event-to-state s5 (make-test-event "model.stream.delta" (hasheq 'delta "Turn2 text")
                                                          #:turn-id "turn-2")))

    ;; This SHOULD be just "Turn2 text", not "Turn1 textTurn2 text"
    (check-equal? (ui-state-streaming-text s6) "Turn2 text"
                  "no cross-turn accumulation after proper completion"))

   (test-case
    "BUG: streaming text accumulates across turns when assistant.message.completed missing"
    ;; This is the actual worst case: no assistant.message.completed AND no
    ;; streaming-text cleanup on turn.completed
    (define s0 (initial-ui-state))

    ;; Turn 1: stream text, then tool call (no assistant.message.completed)
    (define s1 (apply-event-to-state s0 (make-test-event "turn.started" (hasheq))))
    (define s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hasheq 'delta "Leftover "))))
    (define s3 (apply-event-to-state s2 (make-test-event "tool.call.started"
                                                          (hasheq 'id "tc-1" 'name "bash"
                                                                  'arguments (hasheq 'command "ls")))))
    ;; turn.completed — streaming-text NOT cleared (Bug B2)
    (define s4 (apply-event-to-state s3 (make-test-event "turn.completed"
                                                          (hasheq 'termination 'tool-calls-pending
                                                                 'turnId "turn-1"))))

    ;; BUG: streaming-text still has "Leftover "
    (check-equal? (ui-state-streaming-text s4) "Leftover "
                  "BUG B2: streaming-text persists across turn boundary")

    ;; Turn 2: stream new text
    (define s5 (apply-event-to-state s4 (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")))
    (define s6 (apply-event-to-state s5 (make-test-event "model.stream.delta" (hasheq 'delta "New text")
                                                          #:turn-id "turn-2")))

    ;; BUG: streaming-text is "Leftover New text" — contaminated!
    (check-equal? (ui-state-streaming-text s6) "Leftover New text"
                  "BUG CONFIRMED: cross-turn contamination from uncleared streaming-text"))))

(module+ main
  (run-tests streaming-tool-bug-tests))

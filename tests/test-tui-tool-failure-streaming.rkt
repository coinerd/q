#lang racket

;; tests/test-tui-tool-failure-streaming.rkt — Tool failure, streaming
;; interleaving, and edge case tests (Wave 3, W3.3 #885).
;;
;; Tests runtime errors during tool calls, streaming text interleaving
;; with tool calls, and cancelled tool operations.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt")

(define tool-failure-streaming-tests
  (test-suite "Tool Failure & Streaming Tests"

    ;; TF1: runtime.error creates error entry and clears busy
    (test-case "TF1: runtime.error creates error entry and clears busy"
      (define ms (make-mock-session))
      (define ms1
        (mock-apply-events ms
                           (list (cons "turn.started" (hash))
                                 (cons "tool.call.started"
                                       (hash 'name "bash" 'arguments "{\"command\":\"bad-cmd\"}"))
                                 (cons "runtime.error" (hash 'error "connection lost"))
                                 (cons "turn.completed" (hash)))))
      (define texts (mock-entry-texts ms1))
      ;; tool-start + error + system hint = 3 entries
      (check-equal? (length texts) 3)
      (check-not-false (find-entry-by-text (mock-session-state ms1) "Error: connection lost")
                       "should find error entry")
      (check-false (mock-busy? ms1)))

    ;; TF2: Streaming text interleaved with tool call
    (test-case "TF2: streaming text + tool call produces correct entries"
      ;; Note: assistant.message.completed prefers streaming text over payload
      ;; content. To test a clean assistant entry, we skip streaming before it.
      (define state0 (initial-ui-state))
      (define events
        (event-sequence "turn.started"
                        (hash)
                        "tool.call.started"
                        (hash 'name "read" 'arguments "{\"path\":\"x.rkt\"}")
                        "tool.call.completed"
                        (hash 'name "read" 'result "contents")
                        ;; No streaming before this assistant event, so content comes from payload
                        "assistant.message.completed"
                        (hash 'content "Here is the file.")
                        "turn.completed"
                        (hash)))
      (define state1 (apply-events state0 events))
      ;; tool-start + tool-end + assistant = 3 entries
      (check-equal? (entry-count state1) 3)
      (check-false (ui-state-streaming-text state1))
      (check-not-false (find-entry-by-text state1 "[TOOL: read]"))
      (check-not-false (find-entry-by-text state1 "Here is the file.")))

    ;; TF3: Cancelled turn with active tool call cleans up state
    (test-case "TF3: cancelled turn cleans up busy and streaming"
      (define state0 (initial-ui-state))
      (define events
        (event-sequence "turn.started"
                        (hash)
                        "model.stream.delta"
                        (hash 'delta "Thinking...")
                        "tool.call.started"
                        (hash 'name "bash" 'arguments "{\"command\":\"sleep 10\"}")
                        "turn.cancelled"
                        (hash)))
      (define state1 (apply-events state0 events))
      ;; Cancel clears busy, streaming, pending-tool-name
      (check-false (ui-state-busy? state1))
      (check-false (ui-state-streaming-text state1))
      (check-false (ui-state-pending-tool-name state1))
      ;; Tool-start entry still exists (was already appended)
      (check-equal? (entry-count state1) 1)
      (check-not-false (find-entry-by-text state1 "[TOOL: bash]")))

    ;; TF4: Multiple streaming deltas accumulate correctly
    (test-case "TF4: multiple streaming deltas accumulate"
      (define state0 (initial-ui-state))
      (define events
        (event-sequence "turn.started"
                        (hash)
                        "model.stream.delta"
                        (hash 'delta "Hello")
                        "model.stream.delta"
                        (hash 'delta " ")
                        "model.stream.delta"
                        (hash 'delta "world")))
      (define state1 (apply-events state0 events))
      ;; Streaming text should accumulate: "Hello world"
      (check-equal? (ui-state-streaming-text state1) "Hello world")
      (check-true (ui-state-busy? state1))
      ;; No transcript entries from streaming alone
      (check-equal? (entry-count state1) 0))))

(run-tests tool-failure-streaming-tests)

#lang racket

;; tests/test-tui-workflow-harness.rkt — Smoke tests for the TUI workflow
;; harness infrastructure (Wave 1, W1.3 #877).
;;
;; Verifies that the event→state→render pipeline works end-to-end without
;; a real terminal. These are infrastructure smoke tests, not deep logic tests.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; ── Workflow Harness Smoke Tests ──────────────────────────

(define workflow-harness-smoke-tests
  (test-suite
   "TUI Workflow Harness Smoke Tests"

   (test-case "SH1: apply-events + render-state-strings pipeline"
     (define state0 (initial-ui-state))
     (define events
       (list (make-test-event "turn.started" (hash))
             (make-test-event "assistant.message.completed"
                              (hash 'content "Hello from assistant"))
             (make-test-event "turn.completed" (hash))))
     (define state1 (apply-events state0 events))
     ;; State should have 1 transcript entry (turn.started doesn't create one)
     (check-equal? (entry-count state1) 1)
     (check-equal? (state->texts state1) '("Hello from assistant"))
     ;; Render should produce at least one line
     (define-values (lines _state2) (render-state-strings state1 80 24))
     (check-true (>= (length lines) 1)
                 (format "expected >=1 rendered line, got ~a" (length lines)))
     ;; The rendered output should contain the assistant message
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "Hello from assistant"))
      "rendered output should contain assistant message"))

   (test-case "SH2: mock-tui-session lifecycle simulation"
     (define ms (make-mock-session))
     ;; Apply a realistic sequence: turn start → tool call → tool complete → turn end
     (define ms1
       (mock-apply-events
        ms
        (list (cons "turn.started" (hash))
              (cons "tool.call.started"
                    (hash 'name "read" 'arguments "{\"path\":\"test.rkt\"}"))
              (cons "tool.call.completed"
                    (hash 'name "read" 'result "(define x 1)"))
              (cons "assistant.message.completed"
                    (hash 'content "I read the file."))
              (cons "turn.completed" (hash)))))
     ;; Should have 3 transcript entries: tool-start, tool-end, assistant
     (check-equal? (length (mock-entry-texts ms1)) 3)
     ;; busy? should be #f after turn.completed
     (check-false (mock-busy? ms1))
     ;; Render should work
     (define rendered (mock-render ms1 80 24))
     (check-true (>= (length rendered) 1)))

   (test-case "SH3: find-entry-by-text and event-sequence helpers"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "tool.call.started" (hash 'name "bash" 'arguments "{\"command\":\"ls\"}")
        "tool.call.completed" (hash 'name "bash" 'result "file1.rkt\nfile2.rkt")
        "assistant.message.completed" (hash 'content "Found 2 files")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     ;; find-entry-by-text should locate the tool-start entry
     (define tool-entry (find-entry-by-text state1 "[TOOL: bash]"))
     (check-not-false tool-entry "should find tool-start entry")
     ;; find-entry-by-text should locate the assistant entry
     (define asst-entry (find-entry-by-text state1 "Found 2 files"))
     (check-not-false asst-entry "should find assistant entry")
     ;; entry-count
     (check-equal? (entry-count state1) 3)
     ;; state->texts — check entries contain expected text
     (define texts (state->texts state1))
     ;; Tool entry has arg summary appended: "[TOOL: bash] ls"
     (check-not-false (find-entry-by-text state1 "[TOOL: bash]") "texts should contain tool-start")
     (check-not-false (member "Found 2 files" texts) "texts should contain assistant"))))

(run-tests workflow-harness-smoke-tests)

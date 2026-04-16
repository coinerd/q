#lang racket

;; tests/test-tui-tool-cycle.rkt — Single tool call cycle workflow tests
;; (Wave 3, W3.1 #883).
;;
;; Tests that a single tool call start→complete/failed/blocked cycle
;; correctly creates transcript entries and updates busy state.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt")

(define tool-cycle-tests
  (test-suite
   "Single Tool Call Cycle Tests"

   ;; TC1: Complete tool cycle (start→complete) creates two entries
   (test-case "TC1: start→complete creates tool-start and tool-end entries"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "read" 'arguments "{\"path\":\"main.rkt\"}"))
         (cons "tool.call.completed"
               (hash 'name "read" 'result "(define x 1)"))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     ;; Should have 2 entries: tool-start + tool-end
     (check-equal? (length texts) 2)
     ;; Tool start includes arg summary
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: read]")
      "should find tool-start entry")
     ;; Tool end includes result
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[OK: read]")
      "should find tool-end entry")
     ;; Not busy after turn completed
     (check-false (mock-busy? ms1)))

   ;; TC2: Failed tool cycle creates tool-start and tool-fail entries
   (test-case "TC2: start→failed creates tool-start and tool-fail entries"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "bash" 'arguments "{\"command\":\"rm -rf /\"}"))
         (cons "tool.call.failed"
               (hash 'name "bash" 'error "permission denied"))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     (check-equal? (length texts) 2)
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: bash]")
      "should find tool-start entry")
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[FAIL: bash]")
      "should find tool-fail entry"))

   ;; TC3: Blocked tool creates system entry with reason
   (test-case "TC3: blocked tool creates system entry"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "bash" 'arguments "{\"command\":\"ls\"}"))
         (cons "tool.call.blocked"
               (hash 'name "bash" 'reason "sandbox policy"))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     ;; tool-start + blocked system entry = 2 entries
     (check-equal? (length texts) 2)
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[tool blocked: bash")
      "should find blocked tool entry"))

   ;; TC4: Tool cycle renders correctly in terminal output
   (test-case "TC4: tool cycle renders in terminal output"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "tool.call.started" (hash 'name "read" 'arguments "{\"path\":\"test.rkt\"}")
        "tool.call.completed" (hash 'name "read" 'result "ok")
        "assistant.message.completed" (hash 'content "Done reading.")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (define-values (lines _st) (render-state-strings state1 80 24))
     ;; All 3 entries should appear in rendered output
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "[TOOL: read]"))
      "tool start should render")
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "[OK: read]"))
      "tool end should render")
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "Done reading."))
      "assistant message should render"))))

(run-tests tool-cycle-tests)

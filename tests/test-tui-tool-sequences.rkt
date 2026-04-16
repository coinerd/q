#lang racket

;; tests/test-tui-tool-sequences.rkt — Sequential and concurrent tool call
;; tests (Wave 3, W3.2 #884).
;;
;; Tests that multiple tool calls in sequence and overlapping patterns
;; produce correct transcript entries and maintain state consistency.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt")

(define tool-sequence-tests
  (test-suite
   "Sequential & Concurrent Tool Tests"

   ;; SQ1: Sequential tool calls (read→edit→bash) all recorded
   (test-case "SQ1: sequential tools read→edit→bash all recorded"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "read" 'arguments "{\"path\":\"a.rkt\"}"))
         (cons "tool.call.completed"
               (hash 'name "read" 'result "(define a 1)"))
         (cons "tool.call.started"
               (hash 'name "edit" 'arguments "{\"path\":\"a.rkt\",\"old\":\"1\",\"new\":\"2\"}"))
         (cons "tool.call.completed"
               (hash 'name "edit" 'result "ok"))
         (cons "tool.call.started"
               (hash 'name "bash" 'arguments "{\"command\":\"raco test\"}"))
         (cons "tool.call.completed"
               (hash 'name "bash" 'result "3 tests passed"))
         (cons "assistant.message.completed"
               (hash 'content "All done."))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     ;; 3 tool-starts + 3 tool-ends + 1 assistant = 7 entries
     (check-equal? (length texts) 7)
     ;; Verify each tool pair
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: read]")
      "should find read tool-start")
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: edit]")
      "should find edit tool-start")
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: bash]")
      "should find bash tool-start")
     ;; All renders correctly
     (define rendered (mock-render ms1 80 24))
     (check-true (>= (length rendered) 1))
     (check-false (mock-busy? ms1)))

   ;; SQ2: Multiple tool calls in single turn maintain ordering
   (test-case "SQ2: tools in single turn maintain ordering"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "tool.call.started" (hash 'name "tool-a" 'arguments "{\"x\":1}")
        "tool.call.completed" (hash 'name "tool-a" 'result "a-result")
        "tool.call.started" (hash 'name "tool-b" 'arguments "{\"y\":2}")
        "tool.call.completed" (hash 'name "tool-b" 'result "b-result")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (define texts (state->texts state1))
     ;; Order: tool-a start, tool-a end, tool-b start, tool-b end
     (check-equal? (length texts) 4)
     (define a-start-idx
       (for/first ([i (in-naturals)] [t (in-list texts)]
                   #:when (string-contains? t "[TOOL: tool-a]")) i))
     (define a-end-idx
       (for/first ([i (in-naturals)] [t (in-list texts)]
                   #:when (string-contains? t "[OK: tool-a]")) i))
     (define b-start-idx
       (for/first ([i (in-naturals)] [t (in-list texts)]
                   #:when (string-contains? t "[TOOL: tool-b]")) i))
     (check-true (< a-start-idx a-end-idx b-start-idx)
                 "tool-a start < tool-a end < tool-b start"))

   ;; SQ3: Two full turns with different tools each turn
   (test-case "SQ3: two turns with different tools each turn"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         ;; Turn 1: read file
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "read" 'arguments "{\"path\":\"f.rkt\"}"))
         (cons "tool.call.completed"
               (hash 'name "read" 'result "content"))
         (cons "assistant.message.completed"
               (hash 'content "File read."))
         (cons "turn.completed" (hash))
         ;; Turn 2: edit file
         (cons "turn.started" (hash))
         (cons "tool.call.started"
               (hash 'name "edit" 'arguments "{\"path\":\"f.rkt\"}"))
         (cons "tool.call.completed"
               (hash 'name "edit" 'result "ok"))
         (cons "assistant.message.completed"
               (hash 'content "File edited."))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     ;; Turn 1: tool-start + tool-end + assistant = 3
     ;; Turn 2: tool-start + tool-end + assistant = 3
     (check-equal? (length texts) 6)
     ;; Both tools present
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: read]"))
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "[TOOL: edit]"))
     ;; Both assistants present
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "File read."))
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "File edited.")))))

(run-tests tool-sequence-tests)

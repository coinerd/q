#lang racket

;; tests/test-tui-render-edge-cases.rkt — Render pipeline edge case tests
;; (Wave 4, W4.1 #887).
;;
;; Tests render pipeline behavior with wrapping, truncation, empty state,
;; and narrow terminals.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt")

(define render-edge-tests
  (test-suite
   "Render Pipeline Edge Cases"

   ;; RE1: Empty state renders without error
   (test-case "RE1: empty state renders without error"
     (define state0 (initial-ui-state))
     (check-equal? (entry-count state0) 0)
     (define-values (lines state1) (render-state-strings state0 80 24))
     ;; Empty transcript should produce no lines (or minimal output)
     (check-true (list? lines))
     (check-true (<= (length lines) 24)))

   ;; RE2: Long entry text wraps correctly at narrow width
   (test-case "RE2: long text wraps at narrow terminal width"
     (define state0 (initial-ui-state))
     (define long-text (make-string 200 #\X))
     (define events
       (event-sequence
        "assistant.message.completed" (hash 'content long-text)))
     (define state1 (apply-events state0 events))
     (check-equal? (entry-count state1) 1)
     ;; Render at narrow width (40 cols)
     (define-values (lines _st) (render-state-strings state1 40 24))
     ;; Long text should produce multiple wrapped lines
     (check-true (> (length lines) 1)
                 (format "expected wrapped lines, got ~a" (length lines))))

   ;; RE3: Render state consistency — render twice, same result
   (test-case "RE3: rendering twice produces identical output"
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "tool.call.started" (hash 'name "read" 'arguments "{\"path\":\"a.rkt\"}")
        "tool.call.completed" (hash 'name "read" 'result "ok")
        "assistant.message.completed" (hash 'content "Result.")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (define-values (lines1 state-after-1) (render-state-strings state1 80 24))
     (define-values (lines2 state-after-2) (render-state-strings state-after-1 80 24))
     (check-equal? lines1 lines2 "rendering twice should produce same output"))

   ;; RE4: Mock session render matches workflow harness render
   (test-case "RE4: mock-session and workflow-harness produce same texts"
     (define evt-data
       (list (cons "turn.started" (hash))
             (cons "assistant.message.completed" (hash 'content "Test msg"))
             (cons "turn.completed" (hash))))
     ;; Via workflow-harness
     (define state0 (initial-ui-state))
     (define events
       (event-sequence
        "turn.started" (hash)
        "assistant.message.completed" (hash 'content "Test msg")
        "turn.completed" (hash)))
     (define state1 (apply-events state0 events))
     (define harness-texts (state->texts state1))
     ;; Via mock-tui-session
     (define ms (make-mock-session))
     (define ms1 (mock-apply-events ms evt-data))
     (define mock-texts (mock-entry-texts ms1))
     ;; Both should produce the same entry texts
     (check-equal? mock-texts harness-texts
                   "mock-session and harness should agree on entry texts"))))

(run-tests render-edge-tests)

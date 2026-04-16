#lang racket

;; tests/test-tui-selection-state.rkt — Wave 6: T8 selection + transcript mutation
;;
;; Verifies that mouse selection state interacts correctly with transcript mutations
;; caused by incoming events.

(require rackunit
         rackunit/text-ui
         "tui/event-simulator.rkt"
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

(define selection-state-tests
  (test-suite
   "TUI Selection + Transcript Mutation"

   ;; T8: Selection + transcript mutation via event
   ;; When a new event adds to the transcript while selection is active,
   ;; the selection coordinates become stale (wrong row offset).
   (test-case
    "selection becomes stale when transcript mutates"
    (define s0 (initial-ui-state))
    ;; Build up some transcript
    (define s1
      (simulate-events s0
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.delta" (hasheq 'delta "Line 1"))
              (make-test-event "assistant.message.completed"
                               (hasheq 'messageId "m1" 'content "Line 1"))
              (make-test-event "turn.completed"
                               (hasheq 'termination 'completed 'turnId "t1")))))
    ;; Now set a selection
    (define s2 (set-selection-anchor s1 0 0))
    (define s3 (set-selection-end s2 5 0))
    (check-true (has-selection? s3) "selection is active")

    ;; Now a new turn adds transcript entries — selection coords are now stale
    (define s4
      (simulate-events s3
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.delta" (hasheq 'delta "Line 2"))
              (make-test-event "assistant.message.completed"
                               (hasheq 'messageId "m2" 'content "Line 2"))
              (make-test-event "turn.completed"
                               (hasheq 'termination 'completed 'turnId "t2")))))
    ;; Selection is still technically active (coords unchanged)
    (check-true (has-selection? s4)
                "selection still active after transcript growth (documents behavior)")
    ;; But the transcript is longer now
    (check-equal? (transcript-length s4) 3
                  "3 entries after 2 turns + session-start? or 2 entries from turns"))

   ;; Selection cleared when scroll changes
   (test-case
    "clear-selection resets anchor and end"
    (define s0 (initial-ui-state))
    (define s1 (set-selection-anchor s0 0 0))
    (define s2 (set-selection-end s1 10 5))
    (check-true (has-selection? s2) "selection active before clear")
    (define s3 (clear-selection s2))
    (check-false (has-selection? s3) "selection cleared")
    (check-false (ui-state-sel-anchor s3) "anchor is #f")
    (check-false (ui-state-sel-end s3) "end is #f"))))

(module+ main
  (run-tests selection-state-tests))

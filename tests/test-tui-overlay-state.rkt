#lang racket

;; tests/test-tui-overlay-state.rkt — Wave 6: T11 overlay + event interaction
;;
;; Verifies that overlay state interacts correctly with incoming events.
;; When an overlay is active and events arrive, the transcript should still
;; mutate underneath and the overlay should persist.

(require rackunit
         rackunit/text-ui
         "tui/event-simulator.rkt"
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

(define overlay-state-tests
  (test-suite
   "TUI Overlay + Event Interaction"

   ;; T11: Events arriving while overlay is active
   ;; Risk: Overlay persists, transcript mutates underneath
   (test-case
    "events during active overlay: transcript mutates underneath"
    (define s0 (initial-ui-state))
    ;; Start a turn to have some context
    (define s1
      (simulate-events s0
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.delta" (hasheq 'delta "Hello"))
              (make-test-event "assistant.message.completed"
                               (hasheq 'messageId "m1" 'content "Hello"))
              (make-test-event "turn.completed"
                               (hasheq 'termination 'completed 'turnId "t1")))))
    ;; Show overlay (e.g., command palette)
    (define s2 (show-overlay s1 'command-palette
                             '(("Option 1" normal) ("Option 2" normal))
                             "se"))
    (check-true (overlay-active? s2) "overlay is active")

    ;; Now events arrive while overlay is up
    (define s3
      (simulate-events s2
        (list (make-test-event "turn.started" (hasheq))
              (make-test-event "model.stream.delta" (hasheq 'delta "Background"))
              (make-test-event "assistant.message.completed"
                               (hasheq 'messageId "m2" 'content "Background"))
              (make-test-event "turn.completed"
                               (hasheq 'termination 'completed 'turnId "t2")))))

    ;; Overlay should still be active
    (check-true (overlay-active? s3)
                "overlay persists through background events")
    ;; Transcript should have grown
    (check-true (> (transcript-length s3) (transcript-length s2))
                "transcript grew during overlay"))

   ;; Overlay dismiss clears overlay
   (test-case
    "dismiss-overlay clears active overlay"
    (define s0 (initial-ui-state))
    (define s1 (show-overlay s0 'command-palette '(("A" normal)) ""))
    (check-true (overlay-active? s1) "overlay active")
    (define s2 (dismiss-overlay s1))
    (check-false (overlay-active? s2) "overlay dismissed")
    (check-false (ui-state-active-overlay s2) "active-overlay is #f"))))

(module+ main
  (run-tests overlay-state-tests))

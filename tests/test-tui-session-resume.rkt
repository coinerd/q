#lang racket

;; tests/test-tui-session-resume.rkt — Session resume and continuity tests
;; (Wave 2, W2.3 #881).
;;
;; Tests that session.resume events correctly update the TUI state,
;; and that a simulated session switch preserves entry continuity.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "tui/mock-tui-session.rkt"
         "../tui/state.rkt"
         "../tui/scrollback.rkt"
         "../util/protocol-types.rkt")

(define session-resume-tests
  (test-suite
   "Session Resume & Continuity Tests"

   ;; SE1: session.started creates transcript entry and sets session-id
   (test-case "SE1: session.started sets session-id and creates entry"
     (define state0 (initial-ui-state))
     (check-false (ui-state-session-id state0))
     (define evt
       (make-test-event "session.started"
                        (hash 'sessionId "sess-123")))
     (define state1 (apply-event-to-state state0 evt))
     (check-equal? (ui-state-session-id state1) "sess-123")
     (check-equal? (entry-count state1) 1)
     (check-not-false
      (find-entry-by-text state1 "sess-123")
      "should find session ID in transcript"))

   ;; SE2: session.resumed updates session-id and creates entry
   (test-case "SE2: session.resumed updates session-id"
     (define ms (make-mock-session #:session-id "old-session"))
     (define ms1
       (mock-apply-event ms "session.resumed"
                         (hash 'sessionId "resumed-session")))
     (check-equal? (mock-session-id ms1) "resumed-session")
     (check-true (>= (length (mock-entry-texts ms1)) 1))
     (check-not-false
      (find-entry-by-text (mock-session-state ms1) "resumed-session")
      "should find resumed session ID in transcript"))

   ;; SE3: Full session lifecycle preserves entry ordering
   (test-case "SE3: full lifecycle preserves entry ordering"
     (define ms (make-mock-session))
     (define ms1
       (mock-apply-events
        ms
        (list
         (cons "session.started" (hash 'sessionId "s1"))
         (cons "turn.started" (hash))
         (cons "assistant.message.completed"
               (hash 'content "First response"))
         (cons "turn.completed" (hash))
         (cons "turn.started" (hash))
         (cons "assistant.message.completed"
               (hash 'content "Second response"))
         (cons "turn.completed" (hash)))))
     (define texts (mock-entry-texts ms1))
     ;; Should have: session.started entry, first response, second response
     (check-true (>= (length texts) 3))
     ;; Ordering should be preserved
     (define first-idx
       (for/first ([i (in-naturals)]
                   [t (in-list texts)]
                   #:when (string-contains? t "First response"))
         i))
     (define second-idx
       (for/first ([i (in-naturals)]
                   [t (in-list texts)]
                   #:when (string-contains? t "Second response"))
         i))
     (check-not-false first-idx "first response should exist")
     (check-not-false second-idx "second response should exist")
     (check-true (< first-idx second-idx)
                 "first response should come before second"))

   ;; SE4: Scrollback + session resume produces continuous IDs
   (test-case "SE4: scrollback + session resume continuous IDs"
     (reset-scrollback-id-counter!)
     ;; Simulate scrollback from previous session
     (define scrollback
       (list (transcript-entry 'assistant "Previous turn" 100 (hash) 0)
             (transcript-entry 'assistant "Another prev" 200 (hash) 1)))
     (define max-id
       (for/fold ([m -1]) ([e (in-list scrollback)])
         (max m (or (transcript-entry-id e) -1))))
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript scrollback]
                    [next-entry-id (add1 max-id)]))
     ;; Resume session
     (define state1
       (apply-event-to-state
        state0
        (make-test-event "session.resumed"
                         (hash 'sessionId "resumed-s1"))))
     ;; New event after resume
     (define state2
       (apply-event-to-state
        state1
        (make-test-event "assistant.message.completed"
                         (hash 'content "Post-resume message"))))
     ;; All IDs must be unique
     (define all-ids
       (map transcript-entry-id (ui-state-transcript state2)))
     (check-equal? (length all-ids) (length (remove-duplicates all-ids))
                   "all IDs must be unique after scrollback + resume")
     ;; Post-resume entry should have ID >= 2
     (define last-entry (last (ui-state-transcript state2)))
     (check-true (>= (transcript-entry-id last-entry) 2)
                 (format "post-resume ID should be >= 2, got ~a"
                         (transcript-entry-id last-entry))))))

(run-tests session-resume-tests)

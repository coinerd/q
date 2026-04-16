#lang racket

;; tests/test-tui-scrollback-collision.rkt — Scrollback ID collision regression
;; tests (Wave 2, W2.1 #879).
;;
;; Tests that the Wave 0 fix (advancing next-entry-id past max scrollback ID)
;; prevents render cache collisions when scrollback entries and new runtime
;; entries coexist.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "../tui/state.rkt"
         "../tui/scrollback.rkt"
         "../util/protocol-types.rkt")

(define scrollback-collision-tests
  (test-suite
   "Scrollback ID Collision Regression Tests"

   ;; SC1: Scrollback entries with IDs should not collide with new runtime IDs
   (test-case "SC1: scrollback IDs do not collide with runtime IDs"
     (reset-scrollback-id-counter!)
     ;; Simulate scrollback entries with IDs 0, 1, 2
     (define scrollback-entries
       (list (transcript-entry 'assistant "old msg 1" 100 (hash) 0)
             (transcript-entry 'assistant "old msg 2" 200 (hash) 1)
             (transcript-entry 'assistant "old msg 3" 300 (hash) 2)))
     ;; Compute next-entry-id as tui-init.rkt does
     (define max-id
       (for/fold ([m -1]) ([e (in-list scrollback-entries)])
         (max m (or (transcript-entry-id e) -1))))
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript scrollback-entries]
                    [next-entry-id (add1 max-id)]))
     (check-equal? (ui-state-next-entry-id state0) 3)
     ;; Apply new event — should get ID 3 (not 0!)
     (define new-evt
       (make-test-event "assistant.message.completed"
                        (hash 'content "new msg")))
     (define state1 (apply-event-to-state state0 new-evt))
     (define new-entry (last (ui-state-transcript state1)))
     (check-equal? (transcript-entry-id new-entry) 3)
     ;; All IDs must be unique
     (define all-ids
       (map transcript-entry-id (ui-state-transcript state1)))
     (check-equal? (length all-ids) (length (remove-duplicates all-ids))
                   "all entry IDs must be unique"))

   ;; SC2: Render cache returns correct content after scrollback load + new events
   (test-case "SC2: render cache correct after scrollback + new events"
     (reset-scrollback-id-counter!)
     (define scrollback-entries
       (list (transcript-entry 'assistant "Alpha" 100 (hash) 0)
             (transcript-entry 'assistant "Beta" 200 (hash) 1)))
     (define max-id
       (for/fold ([m -1]) ([e (in-list scrollback-entries)])
         (max m (or (transcript-entry-id e) -1))))
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript scrollback-entries]
                    [next-entry-id (add1 max-id)]))
     ;; Render the scrollback entries
     (define-values (lines1 state1) (render-state-strings state0 80 24))
     (check-true (>= (length lines1) 1))
     ;; Apply new event and render again
     (define new-evt
       (make-test-event "assistant.message.completed"
                        (hash 'content "Gamma")))
     (define state2 (apply-event-to-state state1 new-evt))
     (define-values (lines2 state3) (render-state-strings state2 80 24))
     ;; All 3 entries should be visible (old cached + new)
     (check-not-false
      (for/or ([l (in-list lines2)]) (string-contains? l "Alpha"))
      "Alpha should appear in rendered output")
     (check-not-false
      (for/or ([l (in-list lines2)]) (string-contains? l "Gamma"))
      "Gamma should appear in rendered output"))

   ;; SC3: Empty scrollback produces clean initial state
   (test-case "SC3: empty scrollback produces clean initial state"
     (reset-scrollback-id-counter!)
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript '()]
                    [next-entry-id 0]))
     (check-equal? (entry-count state0) 0)
     (check-equal? (ui-state-next-entry-id state0) 0)
     ;; First event gets ID 0
     (define evt (make-test-event "assistant.message.completed"
                                  (hash 'content "first")))
     (define state1 (apply-event-to-state state0 evt))
     (check-equal? (entry-count state1) 1)
     (check-equal? (transcript-entry-id (first (ui-state-transcript state1))) 0))

   ;; SC4: Large scrollback (50 entries) all get unique IDs
   (test-case "SC4: large scrollback all unique IDs"
     (reset-scrollback-id-counter!)
     (define N 50)
     (define scrollback-entries
       (for/list ([i (in-range N)])
         (transcript-entry 'assistant (format "entry ~a" i) (* i 100) (hash) i)))
     (define max-id
       (for/fold ([m -1]) ([e (in-list scrollback-entries)])
         (max m (or (transcript-entry-id e) -1))))
     (check-equal? max-id 49)
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript scrollback-entries]
                    [next-entry-id (add1 max-id)]))
     (check-equal? (ui-state-next-entry-id state0) 50)
     ;; Apply 5 new events
     (define state1
       (for/fold ([st state0])
                 ([i (in-range 5)])
         (apply-event-to-state
          st
          (make-test-event "assistant.message.completed"
                           (hash 'content (format "new ~a" i))))))
     ;; Total entries: 50 + 5 = 55, all IDs unique
     (check-equal? (entry-count state1) 55)
     (define all-ids (map transcript-entry-id (ui-state-transcript state1)))
     (check-equal? (length all-ids) (length (remove-duplicates all-ids))
                   "all 55 IDs must be unique"))))

(run-tests scrollback-collision-tests)

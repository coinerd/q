#lang racket
;; @covers tui/state.rkt

;; @speed fast
;; @suite tui
;; @boundary unit

;; tests/test-c-tui-state-consolidated.rkt — WC Phase C consolidation (#9935, milestone #899)
;;
;; Merged three genuinely pure unit owners of the single target `tui/state.rkt`
;; (identical owner=tui, boundary=unit, speed=fast, suite=tui, gates=[tui, unit-fast],
;; side_effects=none-declared) into one per-file-isolated test module:
;;   - tests/test-tui-atomic-state.rkt   (AXIS2-F05 regression; required tui/context.rkt)
;;   - tests/test-tui-overlay-state.rkt  (Wave 6 T11; required tui/event-simulator + protocol-types)
;;   - tests/test-tui-selection-state.rkt (Wave 6 T8; same requires as overlay)
;; Every unique assertion and case label is preserved verbatim; the merged
;; require set is exactly the union of the three originals. Selection breadth
;; is unchanged: the covers mapping (tui/state.rkt -> this file) and the
;; production reverse-dependency graph are untouched. No shared mutable state:
;; each test-case builds a fresh `initial-ui-state`; the concurrency probe
;; creates and joins its own threads inside its case.

(require rackunit
         rackunit/text-ui
         "tui/event-simulator.rkt"
         "../tui/context.rkt"
         "../tui/state.rkt"
         "../util/message/protocol-types.rkt")

;; ── Group 1: tests/test-tui-atomic-state.rkt (AXIS2-F05) ──────────────────────
;; Verify that atomic-state-update! prevents data loss under concurrent writes.
(define atomic-state-tests
  (test-suite "TUI Atomic State (AXIS2-F05)"

    ;; Test 1: basic atomic update
    (test-case "atomic-state-update! basic update"
      (check-equal? (let ([b (box (initial-ui-state))])
                      (atomic-state-update! b (lambda (s) (set-busy s #t)))
                      (ui-state-busy? (unbox b)))
                    #t
                    "atomic-state-update! basic update"))

    ;; Test 2: sequential updates preserve all mutations
    (test-case "sequential atomic updates preserve busy flag through clear-streaming"
      (check-equal? (let ([b (box (initial-ui-state))])
                      (atomic-state-update! b (lambda (s) (set-busy s #t)))
                      (atomic-state-update! b (lambda (s) (clear-streaming s)))
                      (ui-state-busy? (unbox b)))
                    #t
                    "sequential atomic updates preserve busy flag through clear-streaming"))

    ;; Test 3: concurrent writes don't lose updates
    (test-case "concurrent atomic updates don't lose entries"
      (check-equal?
       (let* ([b (box (initial-ui-state))]
              [n 100]
              [sema (make-semaphore 1)]
              [barrier (make-semaphore 0)]
              [threads (for/list ([i (in-range n)])
                         (thread (lambda ()
                                   (semaphore-wait barrier) ; wait for all threads to start
                                   (atomic-state-update!
                                    b
                                    (lambda (s)
                                      (add-transcript-entry s
                                                            (make-entry 'user
                                                                        (number->string i)
                                                                        (current-inexact-milliseconds)
                                                                        (hasheq))))))))])
         ;; Release all threads at once
         (for ([_ (in-range n)])
           (semaphore-post barrier))
         ;; Wait for all threads
         (for-each thread-wait threads)
         ;; All n entries should be present
         (length (ui-state-transcript (unbox b))))
       100
       "concurrent atomic updates don't lose entries"))

    ;; Test 4: multiple boxes get independent semaphores
    (test-case "independent boxes do not interfere (per-box semaphores)"
      (check-not-exn (lambda ()
                       (let ([b1 (box (initial-ui-state))]
                             [b2 (box (initial-ui-state))])
                         (atomic-state-update! b1 (lambda (s) (set-busy s #t)))
                         (atomic-state-update! b2 (lambda (s) (set-busy s #f)))
                         (unless (and (ui-state-busy? (unbox b1)) (not (ui-state-busy? (unbox b2))))
                           (error 'test "independent boxes interfered"))))))))

;; ── Group 2: tests/test-tui-overlay-state.rkt (Wave 6 T11) ────────────────────
;; Verifies that overlay state interacts correctly with incoming events.
;; When an overlay is active and events arrive, the transcript should still
;; mutate underneath and the overlay should persist.
(define overlay-state-tests
  (test-suite "TUI Overlay + Event Interaction"

    ;; T11: Events arriving while overlay is active
    ;; Risk: Overlay persists, transcript mutates underneath
    (test-case "events during active overlay: transcript mutates underneath"
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
      (define s2 (show-overlay s1 'command-palette '(("Option 1" normal) ("Option 2" normal)) "se"))
      (check-true (overlay-active? s2) "overlay is active")

      ;; Now events arrive while overlay is up — a distinct second turn, as in
      ;; production where every turn carries its own canonical turn id.
      (define s3
        (simulate-events
         s2
         (list (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")
               (make-test-event "model.stream.delta" (hasheq 'delta "Background") #:turn-id "turn-2")
               (make-test-event "assistant.message.completed"
                                (hasheq 'messageId "m2" 'content "Background")
                                #:turn-id "turn-2")
               (make-test-event "turn.completed"
                                (hasheq 'termination 'completed 'turnId "t2")
                                #:turn-id "turn-2"))))

      ;; Overlay should still be active
      (check-true (overlay-active? s3) "overlay persists through background events")
      ;; Transcript should have grown
      (check-true (> (transcript-length s3) (transcript-length s2)) "transcript grew during overlay"))

    ;; Overlay dismiss clears overlay
    (test-case "tui-overlay-state: dismiss-overlay clears active overlay"
      (define s0 (initial-ui-state))
      (define s1 (show-overlay s0 'command-palette '(("A" normal)) ""))
      (check-true (overlay-active? s1) "overlay active")
      (define s2 (dismiss-overlay s1))
      (check-false (overlay-active? s2) "overlay dismissed")
      (check-false (ui-state-active-overlay s2) "active-overlay is #f"))))

;; ── Group 3: tests/test-tui-selection-state.rkt (Wave 6 T8) ───────────────────
;; Verifies that mouse selection state interacts correctly with transcript
;; mutations caused by incoming events.
(define selection-state-tests
  (test-suite "TUI Selection + Transcript Mutation"

    ;; T8: Selection + transcript mutation via event
    ;; When a new event adds to the transcript while selection is active,
    ;; the selection coordinates become stale (wrong row offset).
    (test-case "selection becomes stale when transcript mutates"
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

      ;; Now a new turn adds transcript entries — selection coords are now stale.
      ;; The second turn carries its own canonical turn id, as in production.
      (define s4
        (simulate-events
         s3
         (list (make-test-event "turn.started" (hasheq) #:turn-id "turn-2")
               (make-test-event "model.stream.delta" (hasheq 'delta "Line 2") #:turn-id "turn-2")
               (make-test-event "assistant.message.completed"
                                (hasheq 'messageId "m2" 'content "Line 2")
                                #:turn-id "turn-2")
               (make-test-event "turn.completed"
                                (hasheq 'termination 'completed 'turnId "t2")
                                #:turn-id "turn-2"))))
      ;; Selection is still technically active (coords unchanged)
      (check-true (has-selection? s4)
                  "selection still active after transcript growth (documents behavior)")
      ;; But the transcript is longer now
      (check-equal? (transcript-length s4) 2 "2 entries from 2 turns"))

    ;; Selection cleared when scroll changes
    (test-case "clear-selection resets anchor and end"
      (define s0 (initial-ui-state))
      (define s1 (set-selection-anchor s0 0 0))
      (define s2 (set-selection-end s1 10 5))
      (check-true (has-selection? s2) "selection active before clear")
      (define s3 (clear-selection s2))
      (check-false (has-selection? s3) "selection cleared")
      (check-false (selection-state-anchor (ui-state-selection s3)) "anchor is #f")
      (check-false (selection-state-end (ui-state-selection s3)) "end is #f"))))

(module+ main
  (run-tests atomic-state-tests)
  (run-tests overlay-state-tests)
  (run-tests selection-state-tests))

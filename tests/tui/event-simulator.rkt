#lang racket/base

;; tests/tui/event-simulator.rkt — Infrastructure for TUI state transition testing
;;
;; Provides helpers to simulate sequences of events on a ui-state and
;; inspect intermediate states. Used by test-streaming-transitions.rkt
;; and other TUI state transition test files.

(require racket/match
         "../../tui/state.rkt"
         "../../util/protocol-types.rkt")

(provide make-test-event
         simulate-events
         simulate-and-record
         state-at
         transcript-types
         transcript-texts
         transcript-length)

;; make-test-event : string? hash? [#:time nat] [#:session-id string?] [#:turn-id string?] -> event?
(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [session-id "test-session"]
                         #:turn-id [turn-id "turn-1"])
  (event 1 ev-type time session-id turn-id payload))

;; simulate-events : ui-state? (listof event?) -> ui-state?
;; Applies events sequentially, returning the final state.
(define (simulate-events state events)
  (for/fold ([st state])
            ([evt (in-list events)])
    (apply-event-to-state st evt)))

;; simulate-and-record : ui-state? (listof event?) -> (listof ui-state?)
;; Applies events sequentially, recording all intermediate states.
;; Returns list: [initial, after-evt1, after-evt2, ...]
(define (simulate-and-record state events)
  (define states (list state))
  (for ([evt (in-list events)])
    (define next (apply-event-to-state (car states) evt))
    (set! states (cons next states)))
  (reverse states))

;; state-at : (listof ui-state?) integer -> ui-state?
;; Convenience accessor for intermediate states from simulate-and-record.
;; Index 0 = initial state, 1 = after first event, etc.
(define (state-at states idx)
  (list-ref states idx))

;; transcript-types : ui-state? -> (listof symbol?)
(define (transcript-types state)
  (map transcript-entry-kind (ui-state-transcript state)))

;; transcript-texts : ui-state? -> (listof string?)
(define (transcript-texts state)
  (map transcript-entry-text (ui-state-transcript state)))

;; transcript-length : ui-state? -> nat
(define (transcript-length state)
  (length (ui-state-transcript state)))

#lang racket/base

;; tests/workflows/fixtures/event-recorder.rkt — event bus subscriber
;;
;; Records all events from an event bus for later assertion.
;; Provides filtered access by event name and sequence matching.

(require racket/list
         "../../../agent/event-bus.rkt"
         "../../../agent/types.rkt")

(provide make-event-recorder
         recorded-events
         events-of-type
         event-sequence-matches?
         event-names
         clear-events!)

;; ============================================================
;; Event recorder
;; ============================================================

;; An event-recorder wraps a mutable box of events and a subscription ID.
(struct event-recorder (events-box bus sub-id) #:transparent)

;; make-event-recorder : event-bus? -> event-recorder?
;; Subscribes to all events on the bus and records them in order.
(define (make-event-recorder bus)
  (define events-box (box '()))
  (define sub-id
    (subscribe! bus
                (lambda (evt)
                  (set-box! events-box
                            (append (unbox events-box) (list evt)))))
    ;; No filter — record everything
    )
  (event-recorder events-box bus sub-id))

;; recorded-events : event-recorder? -> (listof event?)
;; Returns all recorded events in chronological order.
(define (recorded-events recorder)
  (unbox (event-recorder-events-box recorder)))

;; events-of-type : event-recorder? string? -> (listof event?)
;; Returns only events matching the given event name.
(define (events-of-type recorder event-name)
  (filter (lambda (evt) (equal? (event-ev evt) event-name))
          (recorded-events recorder)))

;; event-names : event-recorder? -> (listof string?)
;; Returns the event names in order.
(define (event-names recorder)
  (map event-ev (recorded-events recorder)))

;; event-sequence-matches? : event-recorder? (listof string?) -> (or/c #t string?)
;; Checks that the recorded events contain the given sequence as a subsequence.
;; Returns #t if found, or a string describing the mismatch.
(define (event-sequence-matches? recorder expected-names)
  (define names (event-names recorder))
  (define names-len (length names))
  (define expected-len (length expected-names))
  (cond
    [(< names-len expected-len)
     (format "only ~a events recorded, need ~a for sequence match. Got: ~a"
             names-len expected-len names)]
    [else
     ;; Slide a window of expected-len over names
     (define found
       (for/or ([i (in-range (- names-len expected-len -1))])
         (define window (take (drop names i) expected-len))
         (for/and ([a (in-list window)]
                   [b (in-list expected-names)])
           (equal? a b))))
     (cond
       [found #t]
       [else
        (format "sequence ~a not found in ~a" expected-names names)])]))

;; clear-events! : event-recorder? -> void?
;; Clears all recorded events (useful for between-turn assertions).
(define (clear-events! recorder)
  (set-box! (event-recorder-events-box recorder) '()))

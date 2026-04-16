#lang racket

;; tests/tui/mock-tui-session.rkt — Mock session for TUI workflow testing
;;
;; Simulates a full TUI session lifecycle: create a session, apply events,
;; render state, and query results. Pure functions only — no side effects.

(require "../../tui/state.rkt"
         "../../tui/render.rkt"
         "../../util/protocol-types.rkt"
         "workflow-harness.rkt")

(provide (struct-out mock-session)
         make-mock-session
         mock-apply-event
         mock-apply-events
         mock-render
         mock-transcript
         mock-entry-texts
         mock-busy?
         mock-session-id
         mock-streaming-text)

;; A mock TUI session: holds the current ui-state and a log of applied events.
(struct mock-session (state events) #:transparent)

;; Create a fresh mock session with an initial ui-state.
;; state is the current ui-state; events is the list of applied event strings.
(define (make-mock-session #:session-id [session-id "test-session"]
                           #:model-name [model-name "test-model"])
  (mock-session (initial-ui-state #:session-id session-id
                                  #:model-name model-name)
                '()))

;; Apply a single event (by string name and payload hash) to the mock session.
;; Returns a new mock-session with updated state and event appended to log.
(define (mock-apply-event ms ev-str payload)
  (define evt (make-event ev-str 0 (ui-state-session-id (mock-session-state ms)) #f payload))
  (define new-state (apply-event-to-state (mock-session-state ms) evt))
  (mock-session new-state
                (append (mock-session-events ms) (list ev-str))))

;; Apply multiple events to the mock session.
;; ev-str-payload-pairs is a list of (cons ev-str payload).
(define (mock-apply-events ms ev-str-payload-pairs)
  (for/fold ([current ms])
            ([pair (in-list ev-str-payload-pairs)])
    (mock-apply-event current (car pair) (cdr pair))))

;; Render the current session state as a list of plain-text strings.
;; Uses render-transcript which returns 2 values: (sliced-lines state1).
(define (mock-render ms [width 80] [height 24])
  (define-values (sliced-lines _updated-state)
    (render-transcript (mock-session-state ms) height width))
  (map styled-line->text sliced-lines))

;; Return the transcript entries from the current ui-state.
(define (mock-transcript ms)
  (ui-state-transcript (mock-session-state ms)))

;; Return the text of each transcript entry as a list of strings.
(define (mock-entry-texts ms)
  (map transcript-entry-text (ui-state-transcript (mock-session-state ms))))

;; Return whether the session is currently busy.
(define (mock-busy? ms)
  (ui-state-busy? (mock-session-state ms)))

;; Return the session id from the current state.
(define (mock-session-id ms)
  (ui-state-session-id (mock-session-state ms)))

;; Return the current streaming text (or #f).
(define (mock-streaming-text ms)
  (ui-state-streaming-text (mock-session-state ms)))

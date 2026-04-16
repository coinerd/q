#lang racket

;; tests/tui/workflow-harness.rkt — Test infrastructure for simulating agent
;; events through the TUI state→render pipeline WITHOUT a real terminal.
;;
;; Pure functions only. No terminal I/O. No side effects.

(require rackunit
         "../../tui/state.rkt"
         "../../tui/render.rkt"
         "../../util/protocol-types.rkt")

(provide make-test-event
         apply-events
         render-state
         render-state-strings
         event-sequence
         find-entry-by-text
         entry-count
         state->texts)

;; ------------------------------------------------------------
;; make-test-event
;; Wrap make-event with convenient keyword defaults for tests.
;; ------------------------------------------------------------
(define (make-test-event ev payload
                         #:session-id [session-id "test-session"]
                         #:turn-id    [turn-id "t1"]
                         #:time       [time 1000.0])
  (make-event ev time session-id turn-id payload))

;; ------------------------------------------------------------
;; apply-events
;; Fold apply-event-to-state over a list of events.
;; Returns the final ui-state.
;; ------------------------------------------------------------
(define (apply-events state events)
  (foldl (lambda (evt st) (apply-event-to-state st evt))
         state
         events))

;; ------------------------------------------------------------
;; render-state
;; Call render-transcript and return (values lines state1).
;; lines is a (listof styled-line).
;; ------------------------------------------------------------
(define (render-state state [width 80] [height 24])
  (render-transcript state height width))

;; ------------------------------------------------------------
;; render-state-strings
;; Like render-state but returns plain strings instead of styled-lines.
;; Returns (values strings state1) where strings is (listof string).
;; ------------------------------------------------------------
(define (render-state-strings state [width 80] [height 24])
  (define-values (lines state1) (render-transcript state height width))
  (values (map styled-line->text lines) state1))

;; ------------------------------------------------------------
;; event-sequence
;; Convenience: takes alternating ev-string and payload-hash,
;; returns a list of event structs.
;; Example: (event-sequence "assistant_text" (hash "text" "hi")
;;                          "tool_start"     (hash "name" "read"))
;; ------------------------------------------------------------
(define (event-sequence . ev-payload-pairs)
  (let loop ([pairs ev-payload-pairs] [acc '()])
    (cond
      [(null? pairs) (reverse acc)]
      [(null? (cdr pairs))
       (error 'event-sequence "odd number of arguments — expected ev/payload pairs")]
      [else
       (define ev (car pairs))
       (define payload (cadr pairs))
       (loop (cddr pairs)
             (cons (make-test-event ev payload) acc))])))

;; ------------------------------------------------------------
;; find-entry-by-text
;; Find the first transcript-entry whose text contains the given
;; substring. Returns #f if not found.
;; ------------------------------------------------------------
(define (find-entry-by-text state text)
  (for/first ([e (in-list (ui-state-transcript state))]
              #:when (string-contains? (transcript-entry-text e) text))
    e))

;; ------------------------------------------------------------
;; entry-count
;; Return the number of entries in the transcript.
;; ------------------------------------------------------------
(define (entry-count state)
  (length (ui-state-transcript state)))

;; ------------------------------------------------------------
;; state->texts
;; Return a list of plain entry texts from the transcript.
;; ------------------------------------------------------------
(define (state->texts state)
  (map transcript-entry-text (ui-state-transcript state)))

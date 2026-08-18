#lang racket

;; @speed fast  ;; @suite tui
;; @boundary unit

;; Regression coverage for v0.99.95 W1: a canonical prompt terminal owns
;; current-session cleanup even when its turn identifier is stale. Interrupt
;; feedback remains strictly correlated.

(require rackunit
         racket/logging
         racket/string
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         (only-in "tui/event-simulator.rkt" make-test-event))

(define (busy-prompt-state turn-id)
  (set-active-model-turn-id
   (set-active-turn-id
    (set-streaming-text (set-pending-tool-name (set-busy (initial-ui-state #:session-id "session-1"
                                                                           #:model-name "model")
                                                         #t)
                                               "read")
                        "partial")
    turn-id)
   "model-turn"))

(define (prompt-terminal turn-id
                         #:session-id [session-id "session-1"]
                         #:payload [payload #hasheq((scope . "prompt") (reason . "completed"))])
  (make-test-event "turn.completed" payload #:session-id session-id #:turn-id turn-id))

(define (check-terminal-cleared state)
  (check-false (ui-state-busy? state))
  (check-false (ui-state-busy-since state))
  (check-false (ui-state-streaming-text state))
  (check-false (ui-state-pending-tool-name state))
  (check-false (ui-state-active-turn-id state))
  (check-false (ui-state-active-model-turn-id state))
  (check-false (ui-state-interrupt-request-id state)))

(test-case "current-session prompt terminal with stale turn id clears transient state"
  (define waiting (set-interrupt-request-id (busy-prompt-state "active-prompt") "interrupt-1"))
  (define messages '())
  (define completed #f)
  (with-intercepted-logging
   (lambda (log-entry) (set! messages (cons (vector-ref log-entry 1) messages)))
   (lambda () (set! completed (apply-event-to-state waiting (prompt-terminal "stale-prompt"))))
   'warning)
  (check-terminal-cleared completed)
  (check-true (for/or ([message (in-list messages)])
                (string-contains? message "prompt terminal turn-id mismatch")))
  (check-false (for/or ([entry (in-list (ui-state-transcript completed))])
                 (string-contains? (transcript-entry-text entry) "interrupt"))))

(test-case "matching turn with wrong interrupt request clears without feedback"
  (define waiting (set-interrupt-request-id (busy-prompt-state "active-prompt") "interrupt-1"))
  (define completed
    (apply-event-to-state waiting
                          (prompt-terminal "active-prompt"
                                           #:payload #hasheq((scope . "prompt")
                                                             (reason . "cancelled")
                                                             (request-id . "other-interrupt")))))
  (check-terminal-cleared completed)
  (check-false (for/or ([entry (in-list (ui-state-transcript completed))])
                 (string-contains? (transcript-entry-text entry) "interrupt"))))

(test-case "matching prompt terminal retains correlated interrupt feedback"
  (define waiting (set-interrupt-request-id (busy-prompt-state "active-prompt") "interrupt-1"))
  (define completed
    (apply-event-to-state waiting
                          (prompt-terminal "active-prompt"
                                           #:payload #hasheq((scope . "prompt")
                                                             (reason . "cancelled")
                                                             (request-id . "interrupt-1")))))
  (check-terminal-cleared completed)
  (check-true (for/or ([entry (in-list (ui-state-transcript completed))])
                (string-contains? (transcript-entry-text entry) "interrupt completed"))))

(test-case "different-session prompt terminal remains ignored"
  (define waiting (busy-prompt-state "active-prompt"))
  (define unchanged
    (apply-event-to-state waiting (prompt-terminal "stale-prompt" #:session-id "other-session")))
  (check-equal? unchanged waiting))

(test-case "current-session prompt terminal clears when active turn id is absent"
  (define waiting (busy-prompt-state #f))
  (define completed (apply-event-to-state waiting (prompt-terminal "terminal-prompt")))
  (check-terminal-cleared completed))

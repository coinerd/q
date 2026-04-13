#lang racket

;; test-wiring-run-interactive.rkt — Tests for wiring/run-interactive.rkt
;;
;; Tests module exports and make-terminal-subscriber behavior.
;; run-interactive, run-single-shot, run-resume require full runtime,
;; so we test only what can be verified without it.

(require rackunit
         rackunit/text-ui
         racket/port
         (only-in "../wiring/run-interactive.rkt"
                  make-terminal-subscriber
                  handle-sessions-interactive-command)
         (only-in "../util/protocol-types.rkt" make-event))

(define test-wiring-run-interactive
  (test-suite "wiring/run-interactive"

    ;; --------------------------------------------------
    ;; Test 1: make-terminal-subscriber returns a procedure
    ;; --------------------------------------------------
    (test-case "make-terminal-subscriber returns callable"
      (define sub (make-terminal-subscriber))
      (check-pred procedure? sub))

    ;; --------------------------------------------------
    ;; Test 2: subscriber handles model.stream.completed without error
    ;; --------------------------------------------------
    (test-case "subscriber handles model.stream.completed event"
      (define sub (make-terminal-subscriber))
      (define evt (make-event "model.stream.completed"
                              (current-seconds) "test-session" #f (hasheq)))
      (define output (with-output-to-string (lambda () (sub evt))))
      (check-true (or (string? output) (equal? output ""))))

    ;; --------------------------------------------------
    ;; Test 3: subscriber handles model.stream.delta event
    ;; --------------------------------------------------
    (test-case "subscriber handles model.stream.delta event"
      (define sub (make-terminal-subscriber))
      (define evt (make-event "model.stream.delta"
                              (current-seconds) "test-session" #f (hasheq 'delta "hello")))
      (define output (with-output-to-string (lambda () (sub evt))))
      (check-true (string? output)))

    ;; --------------------------------------------------
    ;; Test 4: subscriber handles unknown event type
    ;; --------------------------------------------------
    (test-case "subscriber handles unknown event type"
      (define sub (make-terminal-subscriber))
      (define evt (make-event "unknown.event.type"
                              (current-seconds) "test-session" #f (hasheq 'text "something")))
      (define output (with-output-to-string (lambda () (sub evt))))
      (check-true (string? output)))

    ;; --------------------------------------------------
    ;; Test 5: handle-sessions-interactive-command displays usage for bad input
    ;; --------------------------------------------------
    (test-case "handle-sessions-interactive-command shows usage for bad input"
      (define output
        (with-output-to-string
         (lambda () (handle-sessions-interactive-command '(bad-cmd) (current-output-port) #f))))
      (check-true (string-contains? output "Usage")))))

(run-tests test-wiring-run-interactive)

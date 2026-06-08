#lang racket/base

;; @speed fast
;; @suite default

(require racket/string
         rackunit
         (only-in "../tui/commands/goal-bridge.rkt"
                  make-goal-event-bridge
                  make-goal-run-prompt!)
         (only-in "../agent/event-bus.rkt"
                  make-event-bus subscribe! publish!)
         (only-in "../util/event/event.rkt" event-ev event?))

(test-case "make-goal-event-bridge maps symbols to event names"
  (define bus (make-event-bus))
  (define received '())
  (subscribe! bus (lambda (evt)
                    (when (and (event? evt)
                               (string-prefix? (or (event-ev evt) "") "goal."))
                      (set! received (cons evt received)))))
  (define bridge (make-goal-event-bridge bus "test-session"))
  (bridge 'goal-started (hasheq 'goal-text "test"))
  (bridge 'goal-achieved (hasheq 'goal-text "test"))
  (check-equal? (length received) 2))

(test-case "make-goal-event-bridge ignores unknown symbols"
  (define bus (make-event-bus))
  (define received '())
  (subscribe! bus (lambda (evt)
                    (set! received (cons evt received))))
  (define bridge (make-goal-event-bridge bus "test-session"))
  (bridge 'unknown-event (hasheq))
  (check-equal? received '()))

(test-case "make-goal-event-bridge handles #f event-bus gracefully"
  (define bridge (make-goal-event-bridge #f "test-session"))
  ;; Should not crash
  (bridge 'goal-started (hasheq 'goal-text "test")))

(test-case "make-goal-run-prompt! returns a procedure"
  (check-true (procedure? (make-goal-run-prompt! #f))))

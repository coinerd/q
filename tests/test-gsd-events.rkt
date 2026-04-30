#lang racket

;; tests/test-gsd-events.rkt — GSD Event Telemetry tests
;;
;; Tests event emission from command dispatch and state transitions.

(require rackunit
         "../extensions/gsd/events.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/core.rkt")

;; ============================================================
;; Event collector setup
;; ============================================================

(define (with-collector thunk)
  (define-values (collector query) (make-event-collector))
  (define old-bus (unbox gsd-event-bus-box))
  (set-gsd-event-bus! collector)
  (thunk collector query)
  (set-gsd-event-bus! old-bus))

;; ============================================================
;; Event emission tests
;; ============================================================

(test-case "transition events emitted in correct order"
  (with-collector (lambda (collector query)
                    (reset-gsm!)
                    (gsm-transition! 'exploring)
                    (define events (query))
                    (check-equal? (length events) 2)
                    (check-equal? (hash-ref (list-ref events 0) 'event) 'gsd.transition.attempted)
                    (check-equal? (hash-ref (list-ref events 1) 'event) 'gsd.transition.succeeded))))

(test-case "transition failed event on invalid transition"
  (with-collector (lambda (collector query)
                    (reset-gsm!)
                    ;; Can't go from idle to executing
                    (gsm-transition! 'executing)
                    (define events (query))
                    (check >= (length events) 2)
                    (check-equal? (hash-ref (list-ref events 0) 'event) 'gsd.transition.attempted)
                    (check-equal? (hash-ref (list-ref events 1) 'event) 'gsd.transition.failed))))

(test-case "command events emitted on dispatch"
  (with-collector (lambda (collector query)
                    (reset-gsm!)
                    (gsd-command-dispatch 'gsd "")
                    (define events (query))
                    ;; Should have: received + completed
                    (check >= (length events) 2)
                    (check-equal? (hash-ref (list-ref events 0) 'event) 'gsd.command.received)
                    (check-equal? (hash-ref (list-ref events (- (length events) 1)) 'event)
                                  'gsd.command.completed))))

(test-case "command events include correlation ID"
  (with-collector (lambda (collector query)
                    (reset-gsm!)
                    (gsd-command-dispatch 'gsd "")
                    (define events (query))
                    (define completed-event
                      (findf (lambda (e) (eq? (hash-ref e 'event) 'gsd.command.completed)) events))
                    ;; Correlation ID should be set (not #f)
                    (check-not-false (hash-ref completed-event 'correlation-id))))

  (test-case "event data includes command name"
    (with-collector (lambda (collector query)
                      (reset-gsm!)
                      (gsd-command-dispatch 'gsd "")
                      (define events (query))
                      (define received-event
                        (findf (lambda (e) (eq? (hash-ref e 'event) 'gsd.command.received)) events))
                      (check-equal? (hash-ref (hash-ref received-event 'data) 'command) 'gsd))))

  (test-case "no events emitted when bus is default (void)"
    ;; Default bus is void — should not error
    (reset-gsm!)
    (gsm-transition! 'exploring)
    (check-equal? (gsm-current) 'exploring))

  (test-case "event collector accumulates across multiple transitions"
    (with-collector (lambda (collector query)
                      (reset-gsm!)
                      (gsm-transition! 'exploring)
                      (gsm-transition! 'plan-written)
                      (define events (query))
                      ;; 2 transitions × 2 events each = 4
                      (check-equal? (length events) 4)
                      (check-equal? (hash-ref (list-ref events 0) 'event) 'gsd.transition.attempted)
                      (check-equal? (hash-ref (list-ref events 1) 'event) 'gsd.transition.succeeded)
                      (check-equal? (hash-ref (list-ref events 2) 'event) 'gsd.transition.attempted)
                      (check-equal? (hash-ref (list-ref events 3) 'event)
                                    'gsd.transition.succeeded))))

  (test-case "gsd-event-names includes required events"
    (for ([name '(gsd.command.received gsd.command.completed
                                       gsd.transition.attempted
                                       gsd.transition.succeeded
                                       gsd.transition.failed
                                       gsd.wave.started
                                       gsd.wave.completed)])
      (check-not-false (member name gsd-event-names) (format "~a not in gsd-event-names" name)))))

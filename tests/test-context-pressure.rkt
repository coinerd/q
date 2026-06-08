#lang racket

;;; tests/test-context-pressure.rkt — context pressure event tests

(require rackunit
         rackunit/text-ui
         (only-in "helpers/temp-fs.rkt" with-temp-dir)
         "../runtime/context/context-pressure.rkt"
         "../llm/token-budget.rkt"
         "../agent/event-bus.rkt"
         "../agent/event-structs/base.rkt"
         "../agent/event-structs/context-pressure-events.rkt"
         "../runtime/session/session-types.rkt"
         "../runtime/session/session-store.rkt"
         "../runtime/agent-session.rkt"
         "../util/event/event.rkt")

(define (make-test-session bus dir)
  (make-agent-session (hasheq 'session-dir
                              dir
                              'event-bus
                              bus
                              'provider
                              #f
                              'tool-registry
                              #f
                              'model-name
                              "test"
                              'system-instructions
                              '())))

(define-test-suite context-pressure-tests
                   (test-case "context-pressure-level green below 60%"
                     (check-eq? (context-pressure-level 0.0) 'green)
                     (check-eq? (context-pressure-level 30.0) 'green)
                     (check-eq? (context-pressure-level 59.9) 'green))
                   (test-case "context-pressure-level yellow at 60%–80%"
                     (check-eq? (context-pressure-level 60.0) 'yellow)
                     (check-eq? (context-pressure-level 70.0) 'yellow)
                     (check-eq? (context-pressure-level 80.0) 'red))
                   (test-case "context-pressure-level red above 80%"
                     (check-eq? (context-pressure-level 80.1) 'red)
                     (check-eq? (context-pressure-level 100.0) 'red))
                   (test-case "check-context-pressure emits event on bus"
                     (with-temp-dir (dir)
                                    (define bus (make-event-bus))
                                    (define events (box '()))
                                    (subscribe! bus
                                                (lambda (evt)
                                                  (when (equal? (event-ev evt) "context.pressure")
                                                    (set-box! events (cons evt (unbox events))))))
                                    (define sess (make-test-session bus dir))
                                    (check-eq? (check-context-pressure sess 50 100) 'green)
                                    (define evts (reverse (unbox events)))
                                    (check-equal? (length evts) 1)
                                    (define evt (car evts))
                                    (check-equal? (event-ev evt) "context.pressure")
                                    (define payload (event-payload evt))
                                    (check-equal? (hash-ref payload 'level) 'green)
                                    (check-true (real? (hash-ref payload 'usage-percent)))
                                    (check-= (hash-ref payload 'usage-percent) 50.0 0.01)))
                   (test-case "check-context-pressure computes correct percentage"
                     (with-temp-dir (dir)
                                    (define bus (make-event-bus))
                                    (define events (box '()))
                                    (subscribe! bus
                                                (lambda (evt)
                                                  (when (equal? (event-ev evt) "context.pressure")
                                                    (set-box! events (cons evt (unbox events))))))
                                    (define sess (make-test-session bus dir))
                                    (check-eq? (check-context-pressure sess 25 100) 'green)
                                    (define payload (event-payload (car (reverse (unbox events)))))
                                    (check-= (hash-ref payload 'usage-percent) 25.0 0.01)))
                   (test-case "check-context-pressure red at 90%"
                     (with-temp-dir (dir)
                                    (define bus (make-event-bus))
                                    (define events (box '()))
                                    (subscribe! bus
                                                (lambda (evt)
                                                  (when (equal? (event-ev evt) "context.pressure")
                                                    (set-box! events (cons evt (unbox events))))))
                                    (define sess (make-test-session bus dir))
                                    (check-eq? (check-context-pressure sess 90 100) 'red)
                                    (define payload (event-payload (car (reverse (unbox events)))))
                                    (check-equal? (hash-ref payload 'level) 'red)
                                    (check-= (hash-ref payload 'usage-percent) 90.0 0.01))))

(module+ main
  (run-tests context-pressure-tests))

#lang racket

;; q/tests/test-gui-integration.rkt — Smoke tests for GUI bridge integration

(require rackunit
         rackunit/text-ui
         "../gui/main.rkt"
         "../ui-core/observable-bridge.rkt"
         "../agent/event-bus.rkt"
         "../util/event.rkt")

(define (test-event type payload)
  (make-event type (current-inexact-milliseconds) #f #f payload))

(define-test-suite test-gui-integration
                   (test-case "gui-available? returns boolean in headless env"
                     (check-true (boolean? (gui-available?))))
                   (test-case "wire-bridge! connects to event bus"
                     (define bus (make-event-bus))
                     (define state-box (box (hash)))
                     (define bridge (make-gui-state-bridge state-box (hash)))
                     (bridge-subscribe! bridge bus)
                     (publish! bus (test-event 'test "integration"))
                     (check-true (>= (bridge-update-count bridge) 1))
                     (bridge-dispose! bridge))
                   (test-case "bridge state reflects latest event"
                     (define bus (make-event-bus))
                     (define state-box (box #f))
                     (define bridge (make-gui-state-bridge state-box #f))
                     (bridge-subscribe! bridge bus)
                     (define evt (test-event 'message "hello"))
                     (publish! bus evt)
                     (check-equal? (bridge-state-ref bridge) evt)
                     (bridge-dispose! bridge)))

(run-tests test-gui-integration)

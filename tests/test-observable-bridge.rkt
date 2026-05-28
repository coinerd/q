#lang racket

;; q/tests/test-observable-bridge.rkt — Tests for ui-core/observable-bridge

(require rackunit
         rackunit/text-ui
         "../ui-core/observable-bridge.rkt"
         "../agent/event-bus.rkt"
         "../util/event.rkt")

;; Helper: create a simple event for testing
(define (test-event type payload)
  (make-event type (current-inexact-milliseconds) #f #f payload))

(define-test-suite
 test-observable-bridge
 ;; ── Construction ──
 (test-case "make-gui-state-bridge creates bridge"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box 'initial))
   (check-true (gui-state-bridge? bridge))
   (check-equal? (bridge-state-ref bridge) 'initial)
   (check-equal? (bridge-update-count bridge) 0))
 (test-case "bridge with custom initial state"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box (hash 'count 0)))
   (check-equal? (bridge-state-ref bridge) (hash 'count 0)))
 ;; ── Subscribe and propagate ──
 (test-case "bridge-subscribe! returns subscription ID"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box #f))
   (define bus (make-event-bus))
   (define sub-id (bridge-subscribe! bridge bus))
   (check-true (exact-nonnegative-integer? sub-id))
   (bridge-dispose! bridge))
 (test-case "bridge receives events and updates state"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box #f))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   (define evt (test-event 'test "hello"))
   (publish! bus evt)
   (check-equal? (bridge-state-ref bridge) evt)
   (check-equal? (bridge-update-count bridge) 1)
   (bridge-dispose! bridge))
 (test-case "bridge with transform function"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box "" #:transform (lambda (evt) (event-payload evt))))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   (publish! bus (test-event 'test "payload-data"))
   (check-equal? (bridge-state-ref bridge) "payload-data")
   (bridge-dispose! bridge))
 (test-case "bridge with filter function"
   (define state-box (box 'no-update))
   (define bridge
     (make-gui-state-bridge state-box
                            'no-update
                            #:filter (lambda (evt) (eq? (event-ev evt) 'important))))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   ;; Filtered out event
   (publish! bus (test-event 'unimportant "skip"))
   (check-equal? (bridge-state-ref bridge) 'no-update)
   (check-equal? (bridge-update-count bridge) 0)
   ;; Passes filter
   (publish! bus (test-event 'important "pass"))
   (check-equal? (bridge-update-count bridge) 1)
   (bridge-dispose! bridge))
 ;; ── Dispose ──
 (test-case "bridge-dispose! stops event propagation"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box #f))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   (publish! bus (test-event 'test "before"))
   (check-equal? (bridge-update-count bridge) 1)
   (bridge-dispose! bridge)
   (publish! bus (test-event 'test "after"))
   ;; Should NOT increment after dispose
   (check-equal? (bridge-update-count bridge) 1))
 ;; ── Thread safety ──
 (test-case "100 rapid events processed safely"
   (define state-box (box 0))
   (define bridge
     (make-gui-state-bridge state-box 0 #:transform (lambda (evt) (add1 (bridge-state-ref bridge)))))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   (for ([i (in-range 100)])
     (publish! bus (test-event 'test i)))
   (sleep 0.2) ; allow async processing
   (check-equal? (bridge-update-count bridge) 100)
   (bridge-dispose! bridge))
 (test-case "multiple bridges on same bus"
   (define box1 (box #f))
   (define box2 (box #f))
   (define bridge1 (make-gui-state-bridge box1 #f))
   (define bridge2 (make-gui-state-bridge box2 #f))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge1 bus)
   (bridge-subscribe! bridge2 bus)
   (publish! bus (test-event 'test "shared"))
   (check-equal? (bridge-update-count bridge1) 1)
   (check-equal? (bridge-update-count bridge2) 1)
   (bridge-dispose! bridge1)
   (bridge-dispose! bridge2))
 (test-case "bridge with filter that checks event-ev (matches wire-bridge! pattern)"
   ;; This test verifies the fix for audit finding F1: the wire-bridge! filter
   ;; must handle event structs (not hashes) by using event-ev.
   (define state-box (box 'no-update))
   (define bridge
     (make-gui-state-bridge state-box
                            'no-update
                            #:filter (lambda (evt) (and (symbol? (event-ev evt)) #t))))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   ;; Event struct should pass the filter (this would fail with the old hash? check)
   (publish! bus (test-event 'user.input (hash 'text "hello")))
   (check-equal? (bridge-update-count bridge) 1)
   (bridge-dispose! bridge))
 (test-case "bridge with filter rejects non-symbol event-ev (F1 defense-in-depth)"
   ;; T4: Verify the event-ev filter pattern REJECTS events with non-symbol ev values
   ;; (e.g., if event-ev returns a string or #f, it should be filtered out)
   (define state-box (box 'no-update))
   (define bridge
     (make-gui-state-bridge state-box
                            'no-update
                            #:filter (lambda (evt) (and (symbol? (event-ev evt)) #t))))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   ;; Create event with string ev (should be rejected by symbol? check)
   (define string-ev-evt (make-event "not-a-symbol" (current-inexact-milliseconds) #f #f '()))
   (publish! bus string-ev-evt)
   (check-equal? (bridge-update-count bridge) 0)
   ;; Create event with symbol ev (should be accepted)
   (publish! bus (test-event 'user.input '()))
   (check-equal? (bridge-update-count bridge) 1)
   (bridge-dispose! bridge))
 (test-case "dispose is idempotent"
   (define state-box (box #f))
   (define bridge (make-gui-state-bridge state-box #f))
   (define bus (make-event-bus))
   (bridge-subscribe! bridge bus)
   (bridge-dispose! bridge)
   (bridge-dispose! bridge) ; should not error
   (check-equal? (bridge-update-count bridge) 0)))

(run-tests test-observable-bridge)

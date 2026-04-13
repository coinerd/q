#lang racket

;; test-tui-init.rkt — Tests for tui/tui-init.rkt
;;
;; Tests module loading, export binding, and subscribe-runtime-events!.
;; run-tui / run-tui-with-runtime require a real terminal, so we test
;; only what can be verified without one.

(require rackunit
         rackunit/text-ui
         racket/async-channel
         "../tui/tui-init.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/state.rkt"
         "../agent/event-bus.rkt")

(define test-tui-init
  (test-suite "tui/tui-init"

    ;; --------------------------------------------------
    ;; Test 1: Module exports are bound
    ;; --------------------------------------------------
    (test-case "run-tui is bound"
      (check-pred procedure? run-tui))

    (test-case "run-tui-with-runtime is bound"
      (check-pred procedure? run-tui-with-runtime))

    (test-case "subscribe-runtime-events! is bound"
      (check-pred procedure? subscribe-runtime-events!))

    ;; --------------------------------------------------
    ;; Test 2: subscribe-runtime-events! wires bus → channel
    ;; --------------------------------------------------
    (test-case "subscribe-runtime-events! posts events to async channel"
      (define bus (make-event-bus))
      (define ctx (make-tui-ctx #:event-bus bus))
      (subscribe-runtime-events! ctx)
      (define test-event (hasheq 'ev "test.event" 'payload (hasheq 'x 1)))
      (publish! bus test-event)
      ;; publish! calls subscribers synchronously, so event is already on channel
      (define ch (tui-ctx-event-ch ctx))
      (define received (async-channel-try-get ch))
      (check-not-false received "event should arrive on channel"))

    ;; --------------------------------------------------
    ;; Test 3: subscribe-runtime-events! with #f bus is safe
    ;; --------------------------------------------------
    (test-case "subscribe-runtime-events! with #f bus does not crash"
      (define ctx (make-tui-ctx #:event-bus #f))
      (check-not-exn (lambda () (subscribe-runtime-events! ctx))))

    ;; --------------------------------------------------
    ;; Test 4: make-tui-ctx defaults work
    ;; --------------------------------------------------
    (test-case "make-tui-ctx creates valid context"
      (define ctx (make-tui-ctx))
      (check-pred tui-ctx? ctx)
      (check-true (unbox (tui-ctx-running-box ctx)))
      (check-true (unbox (tui-ctx-needs-redraw-box ctx)))
      (check-false (unbox (tui-ctx-term-box ctx)))
      (check-false (unbox (tui-ctx-ubuf-box ctx))))

    ;; --------------------------------------------------
    ;; Test 5: subscribe-runtime-events! delivers multiple events
    ;; --------------------------------------------------
    (test-case "subscribe-runtime-events! delivers multiple events in order"
      (define bus (make-event-bus))
      (define ctx (make-tui-ctx #:event-bus bus))
      (subscribe-runtime-events! ctx)
      (define ch (tui-ctx-event-ch ctx))
      (publish! bus (hasheq 'ev "first" 'payload (hasheq)))
      (publish! bus (hasheq 'ev "second" 'payload (hasheq)))
      ;; Drain both
      (define e1 (sync/timeout 0.5 ch))
      (define e2 (sync/timeout 0.5 ch))
      (check-not-false e1 "first event should arrive")
      (check-not-false e2 "second event should arrive"))))

(run-tests test-tui-init)
